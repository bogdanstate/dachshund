/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
use crate::dachshund::graph_base::GraphBase;
use crate::dachshund::graph_schema::GraphSchema;
pub trait SimpleGraph
where
    Self: GraphBase,
    <Self as GraphBase>::SchemaType: GraphSchema,
{
    fn create_empty() -> Self;
}
