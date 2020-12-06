/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
use crate::dachshund::brokerage::Brokerage;
use crate::dachshund::connected_components::{ConnectedComponents, ConnectedComponentsDirected};
use crate::dachshund::connectivity::{Connectivity, ConnectivityDirected};
use crate::dachshund::graph_base::GraphBase;
use crate::dachshund::graph_schema::SimpleGraphSchema;
use crate::dachshund::id_types::NodeId;
use crate::dachshund::node::{DirectedNodeBase, NodeBase, SimpleDirectedNode};
use crate::dachshund::simple_graph::SimpleGraph;
use std::collections::hash_map::{Keys, Values};
use std::collections::{HashMap, HashSet};

pub trait DirectedGraph
where
    Self: GraphBase,
    <Self as GraphBase>::NodeType: DirectedNodeBase,
{
    fn is_acyclic(&self) -> bool {
        // from https://www.cs.hmc.edu/~keller/courses/cs60/s98/examples/acyclic/
        let mut leaves: HashSet<NodeId> = HashSet::new();
        let num_nodes = self.count_nodes();
        while leaves.len() < num_nodes {
            let mut leaf_was_found: bool = false;
            for node in self.get_nodes_iter() {
                let node_id = node.get_id();
                if !leaves.contains(&node_id) && node.has_no_out_neighbors_except_set(&leaves) {
                    leaves.insert(node.get_id());
                    leaf_was_found = true;
                }
            }
            if !leaf_was_found {
                return false;
            }
        }
        return true;
    }
}
pub struct SimpleDirectedGraph {
    pub nodes: HashMap<NodeId, SimpleDirectedNode>,
    pub ids: Vec<NodeId>,
}
impl DirectedGraph for SimpleDirectedGraph {}
impl SimpleGraph for SimpleDirectedGraph {}
impl GraphBase for SimpleDirectedGraph {
    type NodeType = SimpleDirectedNode;
    type SchemaType = SimpleGraphSchema;

    /// core and non-core IDs are the same for a `SimpleDirectedGraph`.
    fn get_core_ids(&self) -> &Vec<NodeId> {
        &self.ids
    }
    /// core and non-core IDs are the same for a `SimpleDirectedGraph`.
    fn get_non_core_ids(&self) -> Option<&Vec<NodeId>> {
        Some(&self.ids)
    }
    fn get_ids_iter(&self) -> Keys<NodeId, SimpleDirectedNode> {
        self.nodes.keys()
    }
    fn get_nodes_iter(&self) -> Values<NodeId, SimpleDirectedNode> {
        self.nodes.values()
    }
    fn get_mut_nodes(&mut self) -> &mut HashMap<NodeId, SimpleDirectedNode> {
        &mut self.nodes
    }
    fn has_node(&self, node_id: NodeId) -> bool {
        self.nodes.contains_key(&node_id)
    }
    fn get_node(&self, node_id: NodeId) -> &SimpleDirectedNode {
        &self.nodes[&node_id]
    }
    fn count_edges(&self) -> usize {
        let mut num_edges: usize = 0;
        for node in self.nodes.values() {
            num_edges += node.degree();
        }
        assert_eq!(num_edges % 2, 0);
        num_edges / 2
    }
    fn count_nodes(&self) -> usize {
        self.nodes.len()
    }
    fn create_empty() -> Self {
        SimpleDirectedGraph {
            nodes: HashMap::new(),
            ids: Vec::new(),
        }
    }
}

impl Brokerage for SimpleDirectedGraph {}
impl ConnectedComponents for SimpleDirectedGraph {}
impl ConnectedComponentsDirected for SimpleDirectedGraph {}
impl Connectivity for SimpleDirectedGraph {}
impl ConnectivityDirected for SimpleDirectedGraph {}
