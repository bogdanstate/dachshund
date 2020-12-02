/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
use crate::dachshund::error::{CLQError, CLQResult};
use std::collections::HashMap;

/// A mapping from opaque strings identifying node types (e.g. "author"), to the associated integer
/// identifier used internally. Encapsulates some special/convenient accessor/mutator logic.
pub struct TypeIdsLookup<TypeId> {
    data: HashMap<String, TypeId>,
}

impl<TypeId: PartialEq> TypeIdsLookup<TypeId> {
    pub fn new() -> Self {
        Self {
            data: HashMap::new(),
        }
    }

    pub fn require(&self, type_str: &str) -> CLQResult<&TypeId> {
        let id = self
            .data
            .get(type_str)
            .ok_or_else(|| CLQError::from(format!("No mapping for non-core type: {}", type_str)))?;
        Ok(id)
    }
    pub fn require_mut(&mut self, type_str: &str) -> CLQResult<&mut TypeId> {
        let id = self
            .data
            .get_mut(type_str)
            .ok_or_else(|| CLQError::from(format!("No mapping for non-core type: {}", type_str)))?;
        Ok(id)
    }
    pub fn insert(&mut self, type_str: &str, type_id: TypeId) {
        if !self.data.contains_key(type_str) {
            self.data.insert(type_str.to_owned(), type_id);
        }
    }

    pub fn type_name(&self, non_core_type_id: &TypeId) -> Option<String> {
        self.data.iter().find_map(|(k, v)| {
            if v == non_core_type_id {
                Some(k.to_owned())
            } else {
                None
            }
        })
    }
}
impl<T: PartialEq> Default for TypeIdsLookup<T> {
    fn default() -> Self {
        Self::new()
    }
}
