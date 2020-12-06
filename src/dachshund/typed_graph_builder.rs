/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
use crate::dachshund::error::{CLQError, CLQResult};
use crate::dachshund::graph_base::GraphBase;
use crate::dachshund::graph_builder_base::{
    GraphBuilderBase, GraphBuilderBaseWithCliques, GraphBuilderBaseWithPreProcessing,
    GraphBuilderFromVector,
};
use crate::dachshund::id_types::{EdgeTypeId, GraphId, NodeId, NodeTypeId};
use crate::dachshund::node::NodeBase;
use crate::dachshund::node::{Node, NodeEdge};
use crate::dachshund::row::EdgeRow;
use crate::dachshund::typed_graph::TypedGraph;
use crate::dachshund::typed_graph_schema::TypedGraphSchema;
use std::collections::{BTreeSet, HashMap, HashSet};
use std::rc::Rc;

pub struct TypedGraphBuilder {
    pub min_degree: Option<usize>,
    pub graph_id: GraphId,
    pub schema: Rc<TypedGraphSchema>,
}
impl GraphBuilderBase for TypedGraphBuilder {
    type GraphType = TypedGraph;
    type RowType = EdgeRow;
    type SchemaType = TypedGraphSchema;

    fn get_schema(&self) -> Rc<Self::SchemaType> {
        self.schema.clone()
    }
}
impl GraphBuilderFromVector for TypedGraphBuilder {
    fn from_vector(&mut self, data: Vec<EdgeRow>) -> CLQResult<TypedGraph> {
        let mut source_ids: HashSet<NodeId> = HashSet::new();
        let mut target_ids: HashSet<NodeId> = HashSet::new();
        let mut target_type_ids: HashMap<NodeId, NodeTypeId> = HashMap::new();
        for r in data.iter() {
            assert!(self.graph_id == r.graph_id);
            source_ids.insert(r.source_id);
            target_ids.insert(r.target_id);
            target_type_ids.insert(r.target_id, r.target_type_id);
        }

        // warrant a canonical order on the id vectors
        let mut source_ids_vec: Vec<NodeId> = source_ids.into_iter().collect();
        source_ids_vec.sort();
        let mut target_ids_vec: Vec<NodeId> = target_ids.into_iter().collect();
        target_ids_vec.sort();

        let mut node_map: HashMap<NodeId, Node> =
            Self::init_nodes(&source_ids_vec, &target_ids_vec, &target_type_ids);
        Self::populate_edges(&data, &mut node_map)?;
        let mut graph = self.create_graph(node_map, source_ids_vec, target_ids_vec)?;
        if let Some(min_degree) = self.min_degree {
            graph = self.prune(graph, &data, min_degree)?;
        }
        Ok(graph)
    }
}

pub trait TypedGraphBuilderBase: GraphBuilderBase<SchemaType = TypedGraphSchema> {
    fn create_graph(
        &self,
        nodes: HashMap<NodeId, Node>,
        core_ids: Vec<NodeId>,
        non_core_ids: Vec<NodeId>,
    ) -> CLQResult<TypedGraph> {
        Ok(TypedGraph {
            nodes,
            core_ids,
            non_core_ids,
            schema: self.get_schema().clone(),
        })
    }

    fn populate_edges(rows: &[EdgeRow], node_map: &mut HashMap<NodeId, Node>) -> CLQResult<()> {
        for r in rows.iter() {
            assert!(node_map.contains_key(&r.source_id));
            assert!(node_map.contains_key(&r.target_id));

            let source_node = node_map
                .get_mut(&r.source_id)
                .ok_or_else(CLQError::err_none)?;

            source_node
                .neighbors
                .entry(r.target_id)
                .or_insert_with(Vec::new);
            source_node
                .neighbors
                .get_mut(&r.target_id)
                .unwrap()
                .push(NodeEdge::new(r.edge_type_id, r.target_id));

            // probably unnecessary.
            node_map
                .get_mut(&r.source_id)
                .ok_or_else(CLQError::err_none)?
                .edges
                .push(NodeEdge::new(r.edge_type_id, r.target_id));

            // edges with the same source and target type should not be repeated
            if r.source_type_id != r.target_type_id {
                let target_node = node_map
                    .get_mut(&r.target_id)
                    .ok_or_else(CLQError::err_none)?;

                target_node
                    .neighbors
                    .entry(r.source_id)
                    .or_insert_with(Vec::new);
                target_node
                    .neighbors
                    .get_mut(&r.source_id)
                    .unwrap()
                    .push(NodeEdge::new(r.edge_type_id, r.source_id));

                target_node
                    .edges
                    .push(NodeEdge::new(r.edge_type_id, r.source_id));
            }
        }
        Ok(())
    }

    // initializes nodes in the graph with empty neighbors fields.
    fn init_nodes(
        core_ids: &[NodeId],
        non_core_ids: &[NodeId],
        non_core_type_ids: &HashMap<NodeId, NodeTypeId>,
    ) -> HashMap<NodeId, Node> {
        let mut node_map: HashMap<NodeId, Node> = HashMap::new();
        for &id in core_ids {
            let node = Node::new(
                id,             // node_id,
                true,           // is_core,
                None,           // non_core_type,
                Vec::new(),     // edges,
                HashMap::new(), //neighbors
            );
            node_map.insert(id, node);
        }
        for &id in non_core_ids {
            let node = Node::new(
                id,                           // node_id,
                false,                        // is_core,
                Some(non_core_type_ids[&id]), // non_core_type,
                Vec::new(),                   // edges,
                HashMap::new(),               // neighbors
            );
            node_map.insert(id, node);
        }
        node_map
    }

    /// Trims edges greedily, until all edges in the graph have degree at least min_degree.
    /// Note that this function does not delete any nodes -- just finds nodes to delete. It is
    /// called by `prune`, which actually does the deletion.
    fn trim_edges(node_map: &mut HashMap<NodeId, Node>, min_degree: &usize) -> HashSet<NodeId> {
        let mut degree_map: HashMap<NodeId, usize> = HashMap::new();
        for (node_id, node) in node_map.iter() {
            let node_degree: usize = node.degree();
            degree_map.insert(*node_id, node_degree);
        }
        let mut nodes_to_delete: HashSet<NodeId> = HashSet::new();
        loop {
            let mut nodes_to_update: HashSet<NodeId> = HashSet::new();
            for (node_id, node_degree) in degree_map.iter() {
                if node_degree < min_degree && !nodes_to_delete.contains(node_id) {
                    nodes_to_update.insert(*node_id);
                    nodes_to_delete.insert(*node_id);
                }
            }
            if nodes_to_update.is_empty() {
                break;
            }
            for node_id in nodes_to_update.iter() {
                let node: &Node = &node_map[node_id];
                for n in node.edges.iter() {
                    let neighbor_node_id: NodeId = n.target_id;
                    let current_degree: usize = degree_map[&neighbor_node_id];
                    degree_map.insert(neighbor_node_id, current_degree - 1);
                }
            }
        }
        nodes_to_delete
    }
    /// Takes an already-built graph and the edge rows used to create it, returning a
    /// new graph, where all nodes are assured to have degree at least min_degree.
    /// The provision of a TypedGraph is necessary, since the notion of "degree" does
    /// not make sense outside of a graph.
    fn prune(
        &self,
        graph: TypedGraph,
        rows: &Vec<EdgeRow>,
        min_degree: usize,
    ) -> CLQResult<TypedGraph> {
        let mut target_type_ids: HashMap<NodeId, NodeTypeId> = HashMap::new();
        for r in rows.iter() {
            target_type_ids.insert(r.target_id, r.target_type_id);
        }
        let (filtered_source_ids, filtered_target_ids, filtered_rows): (
            Vec<NodeId>,
            Vec<NodeId>,
            Vec<EdgeRow>,
        ) = Self::get_filtered_sources_targets_rows(graph, min_degree, rows);
        let mut filtered_node_map: HashMap<NodeId, Node> =
            Self::init_nodes(&filtered_source_ids, &filtered_target_ids, &target_type_ids);
        Self::populate_edges(&filtered_rows, &mut filtered_node_map)?;
        self.create_graph(filtered_node_map, filtered_source_ids, filtered_target_ids)
    }
    /// called by `prune`, finds source and target nodes to exclude, as well as edges to exclude
    /// when rebuilding the graph from a filtered vector of `EdgeRows`.
    fn get_filtered_sources_targets_rows(
        mut graph: TypedGraph,
        min_degree: usize,
        rows: &Vec<EdgeRow>,
    ) -> (Vec<NodeId>, Vec<NodeId>, Vec<EdgeRow>) {
        let exclude_nodes: HashSet<NodeId> = Self::trim_edges(graph.get_mut_nodes(), &min_degree);
        let filtered_source_ids: Vec<NodeId> = graph
            .get_core_ids()
            .iter()
            .filter(|x| !exclude_nodes.contains(x))
            .cloned()
            .collect();
        let filtered_target_ids: Vec<NodeId> = graph
            .get_non_core_ids()
            .unwrap()
            .iter()
            .filter(|x| !exclude_nodes.contains(x))
            .cloned()
            .collect();
        let filtered_rows: Vec<EdgeRow> = rows
            .iter()
            .filter(|x| {
                !(exclude_nodes.contains(&x.source_id) || (exclude_nodes.contains(&x.target_id)))
            })
            .map(|x| x.clone())
            .collect();
        // todo: make member of struct
        (filtered_source_ids, filtered_target_ids, filtered_rows)
    }
}

impl TypedGraphBuilderBase for TypedGraphBuilder {}
pub struct TypedGraphBuilderWithCliquesOverExistingGraph {
    pub graph_id: GraphId,
    pub cliques: Vec<(BTreeSet<NodeId>, BTreeSet<NodeId>)>,
    pub core_type_id: NodeTypeId,
    pub non_core_type_map: HashMap<NodeId, NodeTypeId>,
    pub edge_type_map: HashMap<(NodeTypeId, NodeTypeId), Vec<EdgeTypeId>>,
    pub schema: Rc<TypedGraphSchema>,
}
impl TypedGraphBuilderWithCliquesOverExistingGraph {
    pub fn new(
        graph_id: GraphId,
        cliques: Vec<(BTreeSet<NodeId>, BTreeSet<NodeId>)>,
        schema: Rc<TypedGraphSchema>,
    ) -> Self {
        Self {
            graph_id,
            cliques,
            core_type_id: schema.get_core_type_id().unwrap().clone(),
            non_core_type_map: HashMap::new(),
            edge_type_map: HashMap::new(),
            schema: schema,
        }
    }
}
impl TypedGraphBuilderBase for TypedGraphBuilderWithCliquesOverExistingGraph {}
impl GraphBuilderBase for TypedGraphBuilderWithCliquesOverExistingGraph {
    type GraphType = TypedGraph;
    type RowType = EdgeRow;
    type SchemaType = TypedGraphSchema;

    fn get_schema(&self) -> Rc<Self::SchemaType> {
        self.schema.clone()
    }
}
impl GraphBuilderFromVector for TypedGraphBuilderWithCliquesOverExistingGraph {
    fn from_vector(&mut self, data: Vec<EdgeRow>) -> CLQResult<TypedGraph> {
        let mut source_ids: HashSet<NodeId> = HashSet::new();
        let mut target_ids: HashSet<NodeId> = HashSet::new();
        let mut target_type_ids: HashMap<NodeId, NodeTypeId> = HashMap::new();
        for r in data.iter() {
            assert!(self.graph_id == r.graph_id);
            source_ids.insert(r.source_id);
            target_ids.insert(r.target_id);
            target_type_ids.insert(r.target_id, r.target_type_id);
        }

        // warrant a canonical order on the id vectors
        let mut source_ids_vec: Vec<NodeId> = source_ids.into_iter().collect();
        source_ids_vec.sort();
        let mut target_ids_vec: Vec<NodeId> = target_ids.into_iter().collect();
        target_ids_vec.sort();

        let mut node_map: HashMap<NodeId, Node> =
            Self::init_nodes(&source_ids_vec, &target_ids_vec, &target_type_ids);
        let edges_with_cliques = self.pre_process_rows(data)?;
        Self::populate_edges(&edges_with_cliques, &mut node_map)?;
        let graph = self.create_graph(node_map, source_ids_vec, target_ids_vec)?;
        Ok(graph)
    }
}

// TODO: should be unified with SimpleUndirectedGraphBuilderWithCliques
impl GraphBuilderBaseWithPreProcessing for TypedGraphBuilderWithCliquesOverExistingGraph {
    fn pre_process_rows(
        &mut self,
        data: Vec<<Self as GraphBuilderBase>::RowType>,
    ) -> CLQResult<Vec<<Self as GraphBuilderBase>::RowType>> {
        let mut row_set: HashSet<<Self as GraphBuilderBase>::RowType> = HashSet::new();
        for el in data.into_iter() {
            let target_type = el.target_type_id.clone();
            let edge_type = el.edge_type_id.clone();
            self.non_core_type_map
                .insert(el.target_id.clone(), target_type.clone());
            self.edge_type_map
                .entry((self.core_type_id, target_type))
                .or_insert(Vec::new())
                .push(edge_type);
            row_set.insert(el);
        }

        for (core, non_core) in self.get_cliques() {
            for core_id in core {
                for non_core_id in non_core {
                    for clique_edge in self.get_clique_edges(*core_id, *non_core_id)?.into_iter() {
                        row_set.insert(clique_edge);
                    }
                }
            }
        }
        let rows_with_cliques: Vec<_> = row_set.into_iter().collect();
        self.non_core_type_map.clear();
        Ok(rows_with_cliques)
    }
}
impl GraphBuilderBaseWithCliques for TypedGraphBuilderWithCliquesOverExistingGraph {
    type CliquesType = (BTreeSet<NodeId>, BTreeSet<NodeId>);

    fn get_clique_edges(&self, id1: NodeId, id2: NodeId) -> CLQResult<Vec<EdgeRow>> {
        let source_type_id = self.core_type_id.clone();
        let target_type_id = self
            .non_core_type_map
            .get(&id2)
            .ok_or_else(|| {
                CLQError::Generic(
                    format!("Cannot find id2 ({}) in self_non_core_type_map", id2).into(),
                )
            })?
            .clone();
        Ok(self
            .edge_type_map
            .get(&(source_type_id, target_type_id))
            .ok_or_else(CLQError::err_none)?
            .iter()
            .cloned()
            .map(|edge_type_id| EdgeRow {
                graph_id: self.graph_id.clone(),
                source_id: id1,
                target_id: id2,
                source_type_id: self.core_type_id.clone(),
                target_type_id,
                edge_type_id,
            })
            .collect())
    }
    fn get_cliques(&self) -> &Vec<(BTreeSet<NodeId>, BTreeSet<NodeId>)> {
        &self.cliques
    }
}
