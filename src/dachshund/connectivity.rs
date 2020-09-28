/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
use crate::dachshund::graph_base::GraphBase;
use crate::dachshund::id_types::NodeId;
use crate::dachshund::node::{NodeBase, NodeEdgeBase};
use crate::dachshund::simple_undirected_graph::UndirectedGraph;
use std::collections::BTreeSet;

type OrderedNodeSet = BTreeSet<NodeId>;

pub trait Connectivity: GraphBase {
    fn visit_nodes_from_root<'a>(
        &'a self,
        root: &NodeId,
        visited: &mut OrderedNodeSet,
        edge_fn: fn(
            &'a Self::NodeType,
        ) -> Box<
            dyn Iterator<Item = &'a <<Self as GraphBase>::NodeType as NodeBase>::NodeEdgeType> + 'a,
        >,
    ) {
        let mut to_visit: Vec<NodeId> = Vec::new();
        to_visit.push(*root);
        while !to_visit.is_empty() {
            let node_id = to_visit.pop().unwrap();
            let node = &self.get_node(node_id);
            for edge in edge_fn(node) {
                let neighbor_id = edge.get_neighbor_id();
                if !visited.contains(&neighbor_id) {
                    to_visit.push(neighbor_id);
                }
            }
            visited.insert(node_id);
        }
    }
    fn _get_is_connected<'a>(
        &'a self,
        edge_fn: fn(
            &'a Self::NodeType,
        ) -> Box<
            dyn Iterator<Item = &'a <<Self as GraphBase>::NodeType as NodeBase>::NodeEdgeType> + 'a,
        >,
    ) -> Result<bool, &'static str> {
        let mut visited: OrderedNodeSet = BTreeSet::new();
        if self.count_nodes() == 0 {
            return Err("Graph is empty");
        }
        let root = self.get_ids_iter().next().unwrap();
        self.visit_nodes_from_root(&root, &mut visited, edge_fn);
        Ok(visited.len() == self.count_nodes())
    }
}
pub trait ConnectivityUndirected: GraphBase
where
    Self: Connectivity,
    Self: UndirectedGraph,
{
    fn get_is_connected(&self) -> Result<bool, &'static str> {
        self._get_is_connected(Self::NodeType::get_edges)
    }
}
