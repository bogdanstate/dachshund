/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this core tree.
 */
extern crate clap;
extern crate serde_json;
use crate::dachshund::error::{CLQError, CLQResult};
use crate::dachshund::graph_schema::GraphSchema;
use crate::dachshund::id_types::{EdgeTypeId, NodeTypeId};
use crate::dachshund::row::EdgeRow;
use crate::dachshund::type_ids_lookup::TypeIdsLookup;
use std::collections::{BTreeSet, HashMap};

pub struct TypedGraphSchema {
    node_type_lookup: TypeIdsLookup<NodeTypeId>,
    edge_type_lookup: TypeIdsLookup<EdgeTypeId>,
    non_core_types: Vec<String>,
    num_non_core_types: usize,
    core_type: String,
    edge_type_map: HashMap<(NodeTypeId, NodeTypeId), Vec<EdgeTypeId>>,
}
impl TypedGraphSchema {
    pub fn empty() -> Self {
        Self {
            node_type_lookup: TypeIdsLookup::<NodeTypeId>::new(),
            edge_type_lookup: TypeIdsLookup::<EdgeTypeId>::new(),
            non_core_types: Vec::new(),
            num_non_core_types: 0,
            core_type: "".to_string(),
            edge_type_map: HashMap::new(),
        }
    }

    pub fn get_human_friendly_row(&self, row: &EdgeRow) -> CLQResult<String> {
        let source_type = self
            .node_type_lookup
            .type_name(&row.source_type_id)
            .ok_or_else(CLQError::err_none)?;
        let target_type = self
            .node_type_lookup
            .type_name(&row.target_type_id)
            .ok_or_else(CLQError::err_none)?;
        let edge_type = self
            .edge_type_lookup
            .type_name(&row.edge_type_id)
            .ok_or_else(CLQError::err_none)?;
        Ok(format!(
            "{}\t{}\t{}\t{}\t{}\t{}",
            row.graph_id.value(),
            row.source_id.value(),
            row.target_id.value(),
            source_type,
            target_type,
            edge_type,
        ))
    }

    pub fn get_node_type_name(&self, id: NodeTypeId) -> CLQResult<String> {
        self.node_type_lookup.type_name(&id).ok_or_else(CLQError::err_none)
    }
    pub fn get_edge_type_name(&self, id: EdgeTypeId) -> CLQResult<String> {
        self.edge_type_lookup.type_name(&id).ok_or_else(CLQError::err_none)
    }
    /// processes a "typespec", a command-line argument, of the form:
    /// [["author", "published_in", "journal"], ["author", "co-authored", "article"]].
    /// This sets up the semantics related to the set of relations contained in the
    /// typed graph. A requirement is that all relations share a "core" type, in this
    /// case, "author". Non-core types must be listed in a vector, which is used to
    /// index the non core-types. The function creates a vector of TypeIdsLookup, which
    /// will then be used to process input rows.
    pub fn new(typespec: Vec<Vec<String>>, core_type: String) -> CLQResult<Self> {
        let mut node_type_lookup = TypeIdsLookup::<NodeTypeId>::new();
        node_type_lookup.insert(&core_type, NodeTypeId::from(0 as usize));

        let should_be_only_this_core_type = &typespec[0][0].clone();
        let non_core_types: BTreeSet<String> = typespec
            .iter()
            .map(|x| x.get(2).unwrap().to_string())
            .filter(|x| *x != core_type)
            .collect();

        for (non_core_type_ix, non_core_type) in non_core_types.iter().enumerate() {
            node_type_lookup.insert(&non_core_type, NodeTypeId::from(non_core_type_ix + 1));
        }
        let mut edge_types: BTreeSet<String> = BTreeSet::new();
        for item in typespec.clone() {
            let core_type = &item[0];
            let edge_type = &item[1];
            let target_type = &item[2];
            assert_eq!(core_type, should_be_only_this_core_type);
            let target_type_id: &mut NodeTypeId = node_type_lookup.require_mut(target_type)?;
            target_type_id.increment_possible_edge_count();
            edge_types.insert(edge_type.to_string());
        }
        let mut edge_type_lookup = TypeIdsLookup::new();
        for (i, edge_type) in edge_types.iter().enumerate() {
            edge_type_lookup.insert(&edge_type, EdgeTypeId::from(i));
        }
        let num_non_core_types = non_core_types.len();
        let mut edge_type_map: HashMap<(NodeTypeId, NodeTypeId), Vec<EdgeTypeId>> = HashMap::new();
        for item in typespec {
            let core_type = &item[0];
            let edge_type = &item[1];
            let target_type = &item[2];
            let source_type_id: &NodeTypeId = node_type_lookup.require(core_type)?;
            let target_type_id: &NodeTypeId = node_type_lookup.require(target_type)?;
            let edge_type_id: &EdgeTypeId = edge_type_lookup.require(edge_type)?;
            edge_type_map
                .entry((*source_type_id, *target_type_id))
                .or_insert(Vec::new())
                .push(*edge_type_id);
        }
        Ok(Self {
            node_type_lookup,
            edge_type_lookup,
            non_core_types: non_core_types.into_iter().collect(),
            num_non_core_types,
            core_type: core_type.to_string(),
            edge_type_map,
        })
    }

    pub fn get_core_type(&self) -> String {
        self.core_type.clone()
    }
    pub fn get_non_core_types(&self) -> Vec<String> {
        self.non_core_types.clone()
    }
    pub fn get_num_non_core_types(&self) -> usize {
        self.num_non_core_types
    }
    pub fn get_num_edge_types(&self) -> usize {
        self.edge_type_lookup.len()
    }
    pub fn get_node_type_id(&self, node_type: String) -> CLQResult<&NodeTypeId> {
        self.node_type_lookup.require(&node_type)
    }
    pub fn get_edge_type_id(&self, edge_type: String) -> CLQResult<&EdgeTypeId> {
        self.edge_type_lookup.require(&edge_type)
    }
    pub fn get_core_type_id(&self) -> CLQResult<&NodeTypeId> {
        self.get_node_type_id(self.core_type.clone())
    }
    pub fn get_non_core_type_ids(&self) -> CLQResult<Vec<NodeTypeId>> {
        let mut non_core_types: Vec<NodeTypeId> = Vec::new();
        for non_core_type in self.get_non_core_types() {
            non_core_types.push(self.get_node_type_id(non_core_type)?.clone());
        }
        Ok(non_core_types)
    }
    /*pub fn get_node_type_name(&self, non_core_type_id: &NodeTypeId) -> Option<String> {
        self.node_type_lookup.type_name(non_core_type_id)
    }*/
    pub fn get_edge_type_map(&self) -> &HashMap<(NodeTypeId, NodeTypeId), Vec<EdgeTypeId>> {
        &self.edge_type_map
    }
}

impl GraphSchema for TypedGraphSchema {}
