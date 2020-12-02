/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this core tree.
 */
extern crate clap;
extern crate serde_json;
use crate::dachshund::error::CLQResult;
use crate::dachshund::id_types::{EdgeTypeId, NodeTypeId};
use crate::dachshund::type_ids_lookup::TypeIdsLookup;
use std::collections::BTreeSet;

pub struct TypedGraphSchema {
    node_type_lookup: TypeIdsLookup<NodeTypeId>,
    edge_type_lookup: TypeIdsLookup<EdgeTypeId>,
    non_core_types: Vec<String>,
    num_non_core_types: usize,
    core_type: String,
}
impl TypedGraphSchema {
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
        for item in typespec {
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
        Ok(Self {
            node_type_lookup,
            edge_type_lookup,
            non_core_types: non_core_types.into_iter().collect(),
            num_non_core_types,
            core_type: core_type.to_string(),
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
    pub fn get_node_type_id(&self, node_type: String) -> CLQResult<&NodeTypeId> {
        self.node_type_lookup.require(&node_type)
    }
    pub fn get_edge_type_id(&self, edge_type: String) -> CLQResult<&EdgeTypeId> {
        self.edge_type_lookup.require(&edge_type)
    }
    pub fn get_core_type_id(&self) -> CLQResult<&NodeTypeId> {
        self.get_node_type_id(self.core_type.clone())
    }
}
