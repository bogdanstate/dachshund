/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
use crate::dachshund::error::CLQResult;
use crate::dachshund::graph_builder_base::{
    GraphBuilderBase,
    GraphBuilderBaseWithGeneratedCliques,
    GraphBuilderBaseWithKnownCliques,
    GraphBuilderBaseWithPreProcessing,
    GraphBuilderFromVector,
};
use crate::dachshund::graph_schema::SimpleGraphSchema;
use crate::dachshund::id_types::NodeId;
use crate::dachshund::simple_graph_builder::SimpleGraphBuilder;
use crate::dachshund::simple_undirected_graph::SimpleUndirectedGraph;
use itertools::Itertools;
use rand::prelude::*;
use std::collections::{BTreeSet, HashSet};
use std::rc::Rc;
pub struct SimpleUndirectedGraphBuilder {}
impl SimpleGraphBuilder for SimpleUndirectedGraphBuilder {}

pub trait TSimpleUndirectedGraphBuilder:
    GraphBuilderBase<
    GraphType = SimpleUndirectedGraph,
    RowType = (i64, i64),
    SchemaType = SimpleGraphSchema,
>
where
    Self: GraphBuilderFromVector,
{
    // Build a graph with n vertices with every possible edge.
    fn get_complete_graph(&mut self, n: u64) -> CLQResult<Self::GraphType> {
        let mut v = Vec::new();
        for i in 1..n {
            for j in i + 1..=n {
                v.push((i, j));
            }
        }
        self.from_vector(v.into_iter().map(|(x, y)| (x as i64, y as i64)).collect())
    }

    // Build a graph with a sequence of n vertices with an edge between
    // each pair of successive vertices.
    fn get_path_graph(&mut self, n: u64) -> CLQResult<Self::GraphType> {
        let mut v = Vec::new();
        for i in 0..n {
            v.push((i, (i + 1)));
        }

        self.from_vector(v.into_iter().map(|(x, y)| (x as i64, y as i64)).collect())
    }

    // Build a graph with a sequence of n vertices with an edge between
    // each pair of successive vertices, plus an edge between the first and
    // last vertices.
    fn get_cycle_graph(&mut self, n: u64) -> CLQResult<Self::GraphType> {
        let mut v = Vec::new();
        for i in 0..n {
            v.push((i, (i + 1) % n));
        }

        self.from_vector(v.into_iter().map(|(x, y)| (x as i64, y as i64)).collect())
    }

    // Builds an Erdos-Renyi graph on n edges with p vertices.
    // (Each possible edge is added to the graph independently at random with
    //  probability p.)
    // [TODO] Switch to the faster implementation using geometric distributions
    // for sparse graphs.
    fn get_er_graph(&mut self, n: u64, p: f64) -> CLQResult<Self::GraphType> {
        let mut v = Vec::new();
        let mut rng = rand::thread_rng();

        for i in 1..n {
            for j in i + 1..=n {
                if rng.gen::<f64>() < p {
                    v.push((i, j));
                }
            }
        }

        self.from_vector(v.into_iter().map(|(x, y)| (x as i64, y as i64)).collect())
    }
}

impl<T: GraphBuilderBaseWithPreProcessing + TSimpleUndirectedGraphBuilder> GraphBuilderBase for T {
    type GraphType = SimpleUndirectedGraph;
    type RowType = (i64, i64);
    type SchemaType = SimpleGraphSchema;
    // builds a graph from a vector of IDs. Repeated edges are ignored.
    // Edges only need to be provided once (this being an undirected graph)
    fn get_schema(&self) -> Rc<SimpleGraphSchema> {
        Rc::new(SimpleGraphSchema {})
    }
}

impl<T: SimpleGraphBuilder + GraphBuilderBaseWithPreProcessing + TSimpleUndirectedGraphBuilder>
    GraphBuilderFromVector for T
{
    #[allow(clippy::ptr_arg)]
    fn from_vector(&mut self, data: Vec<(i64, i64)>) -> CLQResult<SimpleUndirectedGraph> {
        let data = self.pre_process_rows(data)?;
        let ids = Self::get_node_ids(&data);
        let nodes = Self::get_nodes(ids);
        Ok(SimpleUndirectedGraph {
            ids: nodes.keys().cloned().collect(),
            nodes,
        })
    }
}
impl TSimpleUndirectedGraphBuilder for SimpleUndirectedGraphBuilder {}
impl GraphBuilderBaseWithPreProcessing for SimpleUndirectedGraphBuilder {}
pub struct SimpleUndirectedGraphBuilderWithCliques {
    cliques: Vec<BTreeSet<NodeId>>,
}
impl SimpleGraphBuilder for SimpleUndirectedGraphBuilderWithCliques {}
impl SimpleUndirectedGraphBuilderWithCliques {
    pub fn new(cliques: Vec<BTreeSet<NodeId>>) -> Self {
        Self { cliques }
    }
}

impl TSimpleUndirectedGraphBuilder for SimpleUndirectedGraphBuilderWithCliques {}

impl GraphBuilderBaseWithPreProcessing for SimpleUndirectedGraphBuilderWithCliques {
    fn pre_process_rows(
        &mut self,
        data: Vec<<Self as GraphBuilderBase>::RowType>,
    ) -> CLQResult<Vec<<Self as GraphBuilderBase>::RowType>> {
        let mut row_set: HashSet<<Self as GraphBuilderBase>::RowType> = data.into_iter().collect();

        for clique in self.get_cliques() {
            for comb in clique.iter().combinations(2) {
                let id1 = comb.get(0).unwrap().clone();
                let id2 = comb.get(1).unwrap().clone();
                for clique_edge in self.get_clique_edges(*id1, *id2).unwrap().into_iter() {
                    row_set.insert(clique_edge);
                }
            }
        }
        let rows_with_cliques: Vec<_> = row_set.into_iter().collect();
        Ok(rows_with_cliques)
    }
}
impl GraphBuilderBaseWithGeneratedCliques for SimpleUndirectedGraphBuilderWithCliques {
    fn get_clique_edges(&self, id1: NodeId, id2: NodeId) -> CLQResult<Vec<(i64, i64)>> {
        Ok(vec![(id1.value(), id2.value())])
    }
}
impl GraphBuilderBaseWithKnownCliques for SimpleUndirectedGraphBuilderWithCliques {
    type CliquesType = BTreeSet<NodeId>;
    fn get_cliques(&self) -> &Vec<BTreeSet<NodeId>> {
        &self.cliques
    }
}
