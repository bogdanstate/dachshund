/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
use crate::dachshund::graph_base::GraphBase;
use crate::dachshund::id_types::NodeId;
use crate::dachshund::node::{NodeBase, NodeEdgeBase};
use crate::dachshund::simple_directed_graph::DirectedGraph;
use crate::dachshund::simple_undirected_graph::UndirectedGraph;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::iter::FromIterator;

type OrderedNodeSet = BTreeSet<NodeId>;

pub trait ConnectedComponents: GraphBase {
    // returns a hashmap of the form node_id => component_id -- can be turned
    // in to a vector of node_ids inside _get_connected_components.
    fn _get_connected_components_membership(
        &self,
        ignore_nodes: Option<&HashSet<NodeId>>,
        ignore_edges: Option<&HashSet<(NodeId, NodeId)>>,
    ) -> (HashMap<NodeId, usize>, usize) {
        let mut components: HashMap<NodeId, usize> = HashMap::new();
        let mut queue: OrderedNodeSet = BTreeSet::new();
        for id in self.get_ids_iter() {
            if ignore_nodes.is_none() || !ignore_nodes.unwrap().contains(id) {
                queue.insert(*id);
            }
        }
        let mut idx = 0;
        while !queue.is_empty() {
            let id = queue.pop_first().unwrap();
            let distinct_nodes: Vec<NodeId> = self
                .get_node(id)
                .get_edges()
                .map(|x| x.get_neighbor_id())
                .filter(|x| {
                    ignore_edges.is_none()
                        || (!ignore_edges.unwrap().contains(&(id, *x))
                            && !ignore_edges.unwrap().contains(&(*x, id)))
                })
                .collect();
            let mut q2: OrderedNodeSet = BTreeSet::from_iter(distinct_nodes.into_iter());

            while !q2.is_empty() {
                let nid = q2.pop_first().unwrap();
                if ignore_nodes.is_none() || !ignore_nodes.unwrap().contains(&nid) {
                    components.insert(nid, idx);
                    if queue.contains(&nid) {
                        queue.remove(&nid);
                    }
                    for e in self.get_node(nid).get_edges() {
                        let nid2 = e.get_neighbor_id();
                        if (ignore_nodes.is_none() || !ignore_nodes.unwrap().contains(&nid2))
                            && (ignore_edges.is_none()
                                || (!ignore_edges.unwrap().contains(&(nid, nid2))
                                    && !ignore_edges.unwrap().contains(&(nid2, nid))))
                            && !components.contains_key(&nid2)
                        {
                            q2.insert(nid2);
                        }
                    }
                }
            }
            idx += 1;
        }
        (components, idx)
    }
    fn _get_connected_components(
        &self,
        ignore_nodes: Option<&HashSet<NodeId>>,
        ignore_edges: Option<&HashSet<(NodeId, NodeId)>>,
    ) -> Vec<Vec<NodeId>> {
        let (components, n) = self._get_connected_components_membership(ignore_nodes, ignore_edges);
        let mut v: Vec<Vec<NodeId>> = vec![Vec::new(); n];
        for (nid, core_idx) in components {
            v[core_idx].push(nid);
        }
        v
    }
}

pub trait ConnectedComponentsUndirected: GraphBase
where
    Self: ConnectedComponents,
    Self: UndirectedGraph,
{
    fn get_connected_components(&self) -> Vec<Vec<NodeId>> {
        self._get_connected_components(None, None)
    }
}
pub trait ConnectedComponentsDirected: GraphBase
where
    Self: ConnectedComponents,
    Self: DirectedGraph,
{
    fn get_weakly_connected_components(&self) -> Vec<Vec<NodeId>> {
        self._get_connected_components(None, None)
    }
}
