/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
extern crate nalgebra as na;
use crate::dachshund::graph_base::GraphBase;
use crate::dachshund::id_types::NodeId;
use crate::dachshund::node::{NodeBase, NodeEdgeBase, SimpleNode};
use counter::Counter;
use riker::actors::{
    Actor, ActorFactoryArgs, ActorRef, ActorRefFactory, ActorSystem, BasicActorRef, Context,
    Sender, Tell,
};
use std::collections::HashMap;
use std::convert::TryInto;
use std::sync::{Arc, RwLock};
use std::time::Duration;

type CommunityId = NodeId;
type LabelsArcRwLock = Arc<RwLock<HashMap<NodeId, CommunityId>>>;

#[derive(Default, Clone, Debug)]
pub struct RAKMessage {
    payload: CommunityId,
}
impl RAKMessage {
    fn new(payload: CommunityId) -> Self {
        Self { payload }
    }
    fn get_payload(&self) -> CommunityId {
        self.payload
    }
}
#[derive(Default)]
pub struct LabeledNodeActor {
    node_id: NodeId,
    label: CommunityId,
    num_neighbors: usize,
    messages: Vec<(BasicActorRef, RAKMessage)>,
    labels: LabelsArcRwLock,
}
impl LabeledNodeActor {
    fn new(
        node_id: NodeId,
        label: CommunityId,
        num_neighbors: usize,
        labels: LabelsArcRwLock,
    ) -> Self {
        Self {
            node_id: node_id,
            label: label,
            num_neighbors: num_neighbors,
            messages: Vec::with_capacity(num_neighbors),
            labels: labels,
        }
    }
    fn update_received_messages(&mut self, msg: RAKMessage, sender: BasicActorRef) {
        self.messages.push((sender, msg));
    }
    fn has_received_all_messages(&self) -> bool {
        self.messages.len() == self.num_neighbors
    }
    fn update_label(&mut self) -> bool {
        let counts = self
            .messages
            .iter()
            .map(|x| x.1.get_payload())
            .collect::<Counter<_>>()
            .most_common_ordered();
        let old_label = self.label;
        self.label = counts[0].0;
        self.label == old_label && counts[0].1 > self.num_neighbors / 2
    }
    fn send_label_to_neighbors(&self, ctx: &Context<RAKMessage>) {
        for (neighbor_ref, _msg) in self.messages.iter() {
            neighbor_ref
                .try_tell(RAKMessage::new(self.label), Some(ctx.myself().into()))
                .unwrap();
        }
    }
    fn clear_messages(&mut self) {
        self.messages.clear();
    }
}
impl Actor for LabeledNodeActor {
    type Msg = RAKMessage;
    fn recv(&mut self, ctx: &Context<RAKMessage>, msg: RAKMessage, sender: Sender) {
        self.update_received_messages(msg, sender.unwrap());
        if self.has_received_all_messages() {
            let majority = self.update_label();
            if !majority {
                self.send_label_to_neighbors(ctx);
            } else {
                self.labels
                    .write()
                    .unwrap()
                    .insert(self.node_id, self.label);
            }
            self.clear_messages();
        }
    }
}
impl ActorFactoryArgs<(NodeId, usize, LabelsArcRwLock)> for LabeledNodeActor {
    fn create_args((node_id, degree, labels): (NodeId, usize, LabelsArcRwLock)) -> Self {
        Self::new(node_id, node_id.value().try_into().unwrap(), degree, labels)
    }
}
pub trait RAKCommunities: GraphBase<NodeType = SimpleNode> {
    fn get_rak_communities(&self) -> HashMap<NodeId, CommunityId> {
        let sys = ActorSystem::new().unwrap();
        let mut actors: HashMap<NodeId, ActorRef<RAKMessage>> = HashMap::new();
        let labels: LabelsArcRwLock = Arc::new(RwLock::new(HashMap::new()));

        for node in self.get_nodes_iter() {
            let actor_name = format!("Node {}", node.get_id().value());
            let actor_ref = sys
                .actor_of_args::<LabeledNodeActor, _>(
                    &actor_name,
                    (node.get_id(), node.degree(), labels.clone()),
                )
                .unwrap();
            actors.insert(node.get_id(), actor_ref);
        }
        for node in self.get_nodes_iter() {
            let ego_ref = BasicActorRef::from(actors.get(&node.get_id()).unwrap().clone());
            let community_id: CommunityId = node.get_id();
            for ne in node.get_edges() {
                let nid = ne.get_neighbor_id();
                let neighbor_ref = actors.get(&nid).unwrap();
                neighbor_ref.tell(RAKMessage::new(community_id), Some(ego_ref.clone()));
            }
        }
        while labels.read().unwrap().len() < self.count_nodes() {
            std::thread::sleep(Duration::from_millis(5000));
        }
        let lock = Arc::try_unwrap(labels).unwrap();
        lock.into_inner().unwrap()
    }
}
