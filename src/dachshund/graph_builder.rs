/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
extern crate nalgebra as na;
use crate::dachshund::graph_base::GraphBase;
use crate::dachshund::node::Node;

/// Trait encapsulting the logic required to build a graph from a set of edge
/// rows. Currently used to build typed graphs.
pub trait GraphBuilder<TGraph: GraphBase>
where
    Self: Sized,
    TGraph: Sized,
    TGraph: GraphBase<NodeType = Node>,
{
}
