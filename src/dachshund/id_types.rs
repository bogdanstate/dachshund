/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
use core::cmp::Ordering;
use std::fmt;

/// An opaque identifier for node types, with a little convenience metadata.
#[derive(Hash, Copy, Clone, Debug, PartialEq, Eq)]
pub struct NodeTypeId {
    id: usize,
    core: bool,
    max_edge_count_with_core_node: Option<usize>,
}
impl NodeTypeId {
    pub fn value(&self) -> usize {
        self.id
    }
    pub fn is_core(&self) -> bool {
        self.core
    }
    pub fn make_core(&mut self) {
        self.core = true;
    }
    pub fn max_edge_count_with_core_node(&self) -> Option<usize> {
        self.max_edge_count_with_core_node
    }
    pub fn increment_possible_edge_count(&mut self) {
        self.max_edge_count_with_core_node = Some(match self.max_edge_count_with_core_node {
            None => 1,
            Some(n) => n + 1,
        });
    }
    pub fn new(id: usize, core: bool, max_edge_count_with_core_node: Option<usize>) -> Self {
        assert!(core ^ max_edge_count_with_core_node.is_some());
        Self {
            id,
            core,
            max_edge_count_with_core_node,
        }
    }
}
impl<T> From<T> for NodeTypeId
where
    T: Into<usize>,
{
    fn from(n: T) -> Self {
        Self {
            id: n.into(),
            core: false,
            max_edge_count_with_core_node: None,
        }
    }
}
impl Ord for NodeTypeId {
    fn cmp(&self, other: &Self) -> Ordering {
        let self_tuple = (self.id, self.core, self.max_edge_count_with_core_node);
        let other_tuple = (other.id, other.core, other.max_edge_count_with_core_node);
        self_tuple.cmp(&other_tuple)
    }
}
impl PartialOrd for NodeTypeId {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
impl fmt::Display for NodeTypeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "EdgeType:{}", self.id)
    }
}

/// An opaque identifier for edge types. Not interpreted by dachshund logic in any way.
#[derive(Hash, Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct EdgeTypeId {
    id: usize,
}
impl EdgeTypeId {
    pub fn value(&self) -> usize {
        self.id
    }
}
impl<T> From<T> for EdgeTypeId
where
    T: Into<usize>,
{
    fn from(n: T) -> Self {
        Self { id: n.into() }
    }
}
impl fmt::Display for EdgeTypeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "EdgeType:{}", self.id)
    }
}

/// Uniquely identifies a `Node`, relative an existing `Graph`.
#[derive(Hash, Copy, Clone, Debug, PartialOrd, Ord, PartialEq, Eq)]
pub struct NodeId {
    id: i64,
}
impl NodeId {
    pub fn value(&self) -> i64 {
        self.id
    }
}
impl<T> From<T> for NodeId
where
    T: Into<i64>,
{
    fn from(n: T) -> Self {
        Self { id: n.into() }
    }
}
impl fmt::Display for NodeId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Node:{}", self.id)
    }
}

/// Used to refer to distinct graphs. Current use cases:
/// - as a key for input to a transformer (multiple graphs may be processed, in order).
/// - as an identifier for a (quasi-)clique, after it is output.
#[derive(Hash, Copy, Clone, Debug, PartialEq, Eq, Ord, PartialOrd)]
pub struct GraphId {
    id: i64,
}
impl GraphId {
    pub fn value(&self) -> i64 {
        self.id
    }
}
impl<T> From<T> for GraphId
where
    T: Into<i64>,
{
    fn from(n: T) -> Self {
        Self { id: n.into() }
    }
}
impl fmt::Display for GraphId {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "Node:{}", self.id)
    }
}
