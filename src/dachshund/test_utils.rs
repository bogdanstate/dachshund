/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
use std::collections::HashSet;
use std::fmt::Debug;
use std::rc::Rc;
use crate::dachshund::error::{CLQError, CLQResult};
use crate::dachshund::graph_base::GraphBase;
use crate::dachshund::id_types::{GraphId, NodeId, NodeTypeId};
use crate::dachshund::line_processor::LineProcessorBase;
use crate::dachshund::row::EdgeRow;
use crate::dachshund::search_problem::SearchProblem;
use crate::dachshund::transformer::Transformer;
use crate::dachshund::typed_graph::TypedGraph;
use crate::dachshund::typed_graph_schema::TypedGraphSchema;

pub fn gen_test_transformer(
    typespec: Vec<Vec<String>>,
    core_type: String,
) -> CLQResult<Transformer> {
    
    let search_problem = Rc::new(SearchProblem::new(
        20,
        1.0,
        Some(1.0),
        Some(1.0),
        20,
        100,
        3,
        0,
    ));
    let schema = Rc::new(TypedGraphSchema::new(typespec, core_type)?);
    
    let transformer = Transformer::new(
        schema,
        search_problem,
        true,
        false,
    )?;
    Ok(transformer)
}

pub fn gen_test_typespec() -> Vec<Vec<String>> {
    return vec![
        vec![
            "author".to_string(),
            "published_at".into(),
            "conference".into(),
        ],
        vec![
            "author".to_string(),
            "published_at".into(),
            "journal".into(),
        ],
    ];
}

pub fn assert_nodes_have_ids<T>(
    graph: &TypedGraph,
    node_ids: &HashSet<NodeId>,
    test_ids: Vec<T>,
    core: bool,
) where
    T: Copy + Debug + Into<NodeId>,
{
    if node_ids.len() == test_ids.len() {
        let test_set: HashSet<NodeId> = test_ids.iter().map(|&id| id.into()).collect();
        if node_ids
            .iter()
            .all(|&id| graph.get_node(id).is_core() == core && test_set.contains(&id))
        {
            return;
        }
    }
    panic!(
        "Node set [core={}] {:?} != {:?}",
        core, &node_ids, &test_ids
    );
}

pub fn process_raw_vector(transformer: &Transformer, raw: Vec<String>) -> CLQResult<Vec<EdgeRow>> {
    let mut rows: Vec<EdgeRow> = Vec::new();
    for r in raw {
        let row: EdgeRow = transformer
            .line_processor
            .process_line(r)?
            .as_edge_row()
            .ok_or_else(CLQError::err_none)?;
        rows.push(row);
    }
    Ok(rows)
}

fn gen_clique(
    graph_id: GraphId,
    core_ids: &[NodeId],
    non_core_ids_and_types: &[(NodeId, NodeTypeId)],
    non_core_types_as_strings: &[String],
    source_type: String,
    edge_types: &[String],
) -> Vec<String> {
    let mut raw: Vec<String> = Vec::new();
    for core_id in core_ids {
        for ell in non_core_ids_and_types {
            let non_core_id: NodeId = ell.0;
            let non_core_type: NodeTypeId = ell.1;
            let non_core_type_as_string: &str = &non_core_types_as_strings[non_core_type.value()];
            for edge_type in edge_types {
                let s = format!(
                    "{}\t{}\t{}\t{}\t{}\t{}",
                    graph_id.value(),
                    core_id.value(),
                    non_core_id.value(),
                    source_type,
                    edge_type,
                    non_core_type_as_string,
                )
                .to_string();
                raw.push(s);
            }
        }
    }
    raw
}

pub fn gen_single_clique(
    graph_id: GraphId,
    num_core: usize,
    non_core_counts: Vec<usize>,
    non_core_types: Vec<String>,
    source_type: String,
    edge_types: Vec<String>,
) -> (Vec<NodeId>, Vec<(NodeId, NodeTypeId)>, Vec<String>) {
    let mut core_ids: Vec<NodeId> = Vec::new();
    let mut non_core_ids: Vec<(NodeId, NodeTypeId)> = Vec::new();

    for core_id in 0..num_core {
        core_ids.push(NodeId::from(core_id as i64));
    }
    let mut next_id: usize = core_ids.len();

    for (non_core_type, non_core_count) in non_core_counts.iter().enumerate() {
        for i in 0..*non_core_count {
            let non_core_id = next_id + i;
            non_core_ids.push((
                NodeId::from(non_core_id as i64),
                NodeTypeId::from(non_core_type),
            ));
        }
        next_id += non_core_count;
    }
    let clique_rows: Vec<String> = gen_clique(
        graph_id,
        &core_ids,
        &non_core_ids,
        &non_core_types,
        source_type,
        &edge_types,
    );
    (core_ids, non_core_ids, clique_rows)
}
