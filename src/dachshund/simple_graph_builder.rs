/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
use crate::dachshund::graph_builder_base::GraphBuilderBase;
use crate::dachshund::id_types::NodeId;
use crate::dachshund::node::SimpleNode;
use std::collections::{BTreeMap, BTreeSet, HashMap};

pub trait SimpleGraphBuilder: GraphBuilderBase {
    fn get_node_ids(data: &Vec<(i64, i64)>) -> BTreeMap<NodeId, BTreeSet<NodeId>> {
        let mut ids: BTreeMap<NodeId, BTreeSet<NodeId>> = BTreeMap::new();
        for (id1, id2) in data {
            ids.entry(NodeId::from(*id1))
                .or_insert_with(BTreeSet::new)
                .insert(NodeId::from(*id2));
            ids.entry(NodeId::from(*id2))
                .or_insert_with(BTreeSet::new)
                .insert(NodeId::from(*id1));
        }
        ids
    }
    fn get_nodes(ids: BTreeMap<NodeId, BTreeSet<NodeId>>) -> HashMap<NodeId, SimpleNode> {
        let mut nodes: HashMap<NodeId, SimpleNode> = HashMap::new();
        for (id, neighbors) in ids.into_iter() {
            nodes.insert(
                id,
                SimpleNode {
                    node_id: id,
                    neighbors: neighbors,
                },
            );
        }
        nodes
    }
}
