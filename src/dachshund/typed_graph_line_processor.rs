/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
extern crate clap;
extern crate serde_json;

use crate::dachshund::error::CLQResult;
use crate::dachshund::id_types::{EdgeTypeId, GraphId, NodeId, NodeTypeId};
use crate::dachshund::line_processor::LineProcessorBase;
use crate::dachshund::row::Row;
use crate::dachshund::row::{CliqueRow, EdgeRow};
use crate::dachshund::typed_graph_schema::TypedGraphSchema;
use std::rc::Rc;

/// Processing lines for typed graphs
/// Can mutate ids and reverse_ids maps that keep track of
/// graph_ids seen so far.
pub struct TypedGraphLineProcessor {
    schema: Rc<TypedGraphSchema>,
}
impl LineProcessorBase for TypedGraphLineProcessor {
    /// processes a line of (tab-separated) input, of the form:
    /// graph_id\tcore_id\tnon_core_id\tcore_type\tedge_type\tnon_core_type
    ///
    /// or:
    ///
    /// graph_id\tnode_id\tnode_type
    ///
    /// Note that core_type is not used in the first row type. The second
    /// row type is used to initialize the beam search with a single existing
    /// clique, the best identified by some other search process. This existing
    /// clique may be invalidated if it no longer meets cliqueness requirements
    /// as per the current search process.
    fn process_line(&self, line: String) -> CLQResult<Box<dyn Row>> {
        let vec: Vec<&str> = line.split('\t').collect();
        // this is an edge row if we have something on column 3
        assert!(vec.len() == 6);
        let is_edge_row: bool = !vec[3].is_empty();
        if is_edge_row {
            let graph_id: GraphId = vec[0].parse::<i64>()?.into();
            let core_id: NodeId = vec[1].parse::<i64>()?.into();
            let non_core_id: NodeId = vec[2].parse::<i64>()?.into();
            let edge_type: &str = vec[4].trim_end();
            let non_core_type: &str = vec[5].trim_end();
            let non_core_type_id: NodeTypeId =
                *self.schema.get_node_type_id(non_core_type.to_string())?;
            let edge_type_id: EdgeTypeId = *self.schema.get_edge_type_id(edge_type.to_string())?;

            let core_type_id: NodeTypeId = *self.schema.get_core_type_id()?;
            return Ok(Box::new(EdgeRow {
                graph_id,
                source_id: core_id,
                target_id: non_core_id,
                source_type_id: core_type_id,
                target_type_id: non_core_type_id,
                edge_type_id,
            }));
        }
        let graph_id: GraphId = vec[0].parse::<i64>()?.into();
        let node_id: NodeId = vec[1].parse::<i64>()?.into();
        let node_type: &str = vec[2].trim_end();
        let non_core_type: Option<NodeTypeId>;
        if node_type == self.schema.get_core_type() {
            non_core_type = None;
        } else {
            let non_core_type_id: NodeTypeId =
                *self.schema.get_node_type_id(node_type.to_string())?;
            non_core_type = Some(non_core_type_id);
        }
        Ok(Box::new(CliqueRow {
            graph_id,
            node_id,
            target_type: non_core_type,
        }))
    }
}
impl TypedGraphLineProcessor {
    pub fn new(schema: Rc<TypedGraphSchema>) -> Self {
        Self { schema }
    }
}
