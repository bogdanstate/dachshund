/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
extern crate nalgebra as na;
use crate::dachshund::error::CLQResult;
use crate::dachshund::graph_builder_base::{GraphBuilderBase, GraphBuilderFromVector};
use crate::dachshund::graph_schema::SimpleGraphSchema;
use crate::dachshund::id_types::NodeId;
use crate::dachshund::node::SimpleDirectedNode;
use crate::dachshund::simple_directed_graph::SimpleDirectedGraph;
use crate::dachshund::simple_graph_builder::SimpleGraphBuilder;
use std::collections::{BTreeMap, BTreeSet, HashMap};
use std::rc::Rc;

pub struct SimpleDirectedGraphBuilder {}
impl SimpleGraphBuilder for SimpleDirectedGraphBuilder {}

impl GraphBuilderBase for SimpleDirectedGraphBuilder {
    type GraphType = SimpleDirectedGraph;
    type RowType = (i64, i64);
    type SchemaType = SimpleGraphSchema;

    fn get_schema(&self) -> Rc<Self::SchemaType> {
        Rc::new(SimpleGraphSchema {})
    }
}
impl GraphBuilderFromVector for SimpleDirectedGraphBuilder {
    // builds a graph from a vector of IDs. Repeated edges are ignored.
    #[allow(clippy::ptr_arg)]
    fn from_vector(&mut self, data: Vec<(i64, i64)>) -> CLQResult<SimpleDirectedGraph> {
        let mut ids: BTreeMap<NodeId, (BTreeSet<NodeId>, BTreeSet<NodeId>)> = BTreeMap::new();
        for (id1, id2) in data {
            ids.entry(NodeId::from(id1))
                .or_insert_with(|| (BTreeSet::new(), BTreeSet::new()))
                .1
                .insert(NodeId::from(id2));
            ids.entry(NodeId::from(id2))
                .or_insert_with(|| (BTreeSet::new(), BTreeSet::new()))
                .0
                .insert(NodeId::from(id1));
        }
        let mut nodes: HashMap<NodeId, SimpleDirectedNode> = HashMap::new();
        for (id, (in_neighbors, out_neighbors)) in ids.into_iter() {
            nodes.insert(
                id,
                SimpleDirectedNode {
                    node_id: id,
                    in_neighbors: in_neighbors,
                    out_neighbors: out_neighbors,
                },
            );
        }
        Ok(SimpleDirectedGraph {
            ids: nodes.keys().cloned().collect(),
            nodes: nodes,
        })
    }
}
