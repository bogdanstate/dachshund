/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
extern crate lib_dachshund;
use crate::lib_dachshund::dachshund::typed_graph_builder::TypedGraphBuilderBase;
use lib_dachshund::dachshund::candidate::Candidate;
use lib_dachshund::dachshund::error::{CLQError, CLQResult};
use lib_dachshund::dachshund::id_types::{GraphId, NodeId};
use lib_dachshund::dachshund::search_problem::SearchProblem;
use lib_dachshund::dachshund::test_utils::{
    assert_nodes_have_ids, gen_test_transformer, process_raw_vector,
};
use lib_dachshund::dachshund::transformer::Transformer;
use lib_dachshund::dachshund::typed_graph::TypedGraph;
use lib_dachshund::dachshund::typed_graph_builder::TypedGraphBuilder;
use lib_dachshund::dachshund::typed_graph_schema::TypedGraphSchema;
use std::collections::HashSet;
use std::rc::Rc;
use std::sync::mpsc::channel;

pub fn gen_test_typespec() -> Vec<Vec<String>> {
    return vec![
        vec!["author".into(), "published_at".into(), "conference".into()],
        vec!["author".into(), "published_at".into(), "journal".into()],
        vec!["author".into(), "reviewed_for".into(), "conference".into()],
        vec!["author".into(), "reviewed_for".into(), "journal".into()],
        vec!["author".into(), "administered".into(), "conference".into()],
        vec!["author".into(), "administered".into(), "journal".into()],
    ];
}
fn simple_test(raw: Vec<String>, min_degree: usize, expected_len: usize) -> CLQResult<()> {
    let typespec = vec![
        vec!["author".into(), "published_at".into(), "conference".into()],
        vec!["author".into(), "reviewed_for".into(), "conference".into()],
    ];
    let graph_id: GraphId = 0.into();

    let transformer = gen_test_transformer(typespec, "author".to_string())?;
    let rows = process_raw_vector(&transformer, raw)?;

    let mut graph: TypedGraph = transformer.build_pruned_graph(graph_id, rows)?;
    let exclude_nodes: HashSet<NodeId> =
        TypedGraphBuilder::trim_edges(&mut graph.nodes, &min_degree);
    assert_eq!(exclude_nodes.len(), expected_len);
    Ok(())
}

#[cfg(test)]
#[test]
fn test_no_exclude_nodes() -> CLQResult<()> {
    let raw = vec![
        "0\t1\t3\tauthor\tpublished_at\tconference".into(),
        "0\t2\t3\tauthor\tpublished_at\tconference".into(),
        "0\t1\t3\tauthor\treviewed_for\tconference".into(),
        "0\t2\t3\tauthor\treviewed_for\tconference".into(),
    ];
    simple_test(raw, 1, 0)
}

#[test]
fn test_all_exclude_nodes() -> CLQResult<()> {
    let raw = vec![
        "0\t1\t3\tauthor\tpublished_at\tconference".into(),
        "0\t2\t3\tauthor\tpublished_at\tconference".into(),
        "0\t1\t3\tauthor\treviewed_for\tconference".into(),
        "0\t2\t3\tauthor\treviewed_for\tconference".into(),
    ];
    simple_test(raw, 3, 3)
}

#[test]
fn test_partial_exclude_nodes() -> CLQResult<()> {
    let raw = vec![
        "0\t1\t3\tauthor\tpublished_at\tconference".into(),
        "0\t2\t3\tauthor\tpublished_at\tconference".into(),
        "0\t1\t3\tauthor\treviewed_for\tconference".into(),
    ];
    simple_test(raw, 2, 1)
}

#[test]
fn test_prune_small_clique() -> CLQResult<()> {
    // graph_id source_id target_id target_type
    let ts: Vec<Vec<String>> = vec![vec![
        "author".into(),
        "published_at".into(),
        "conference".into(),
    ]];
    let raw = vec![
        "0\t1\t3\tauthor\tpublished_at\tconference".into(),
        "0\t2\t3\tauthor\tpublished_at\tconference".into(),
        "0\t1\t4\tauthor\tpublished_at\tconference".into(),
        "0\t2\t4\tauthor\tpublished_at\tconference".into(),
        "0\t2\t5\tauthor\tpublished_at\tconference".into(),
    ];
    let graph_id: GraphId = 0.into();

    let transformer = gen_test_transformer(ts.clone(), "author".to_string())?;
    let rows = process_raw_vector(&transformer, raw)?;
    let mut graph: TypedGraph = transformer.build_pruned_graph(graph_id, rows.clone())?;
    assert_eq!(graph.nodes.len(), 5);
    
    let schema = Rc::new(TypedGraphSchema::new(ts.clone(), "author".into())?);
    let builder = TypedGraphBuilder{
        min_degree: Some(2),
        graph_id: GraphId::from(1),
        schema: schema,
    };
    graph = builder.prune(graph, &rows, 2)?;
    assert_eq!(graph.nodes.len(), 4);
    let v = Vec::new();
    let res: Candidate<TypedGraph> = transformer
        .process_graph(&graph, &v, graph_id, true)?
        .top_candidate;
    assert_nodes_have_ids(&graph, &res.core_ids, vec![1, 2], true);
    assert_nodes_have_ids(&graph, &res.non_core_ids, vec![3, 4], false);
    Ok(())
}

#[test]
// proves that pruning indeed shortens the amount of cycles it takes to get results
fn test_full_prune_small_clique() -> CLQResult<()> {
    // graph_id source_id target_id target_type
    let raw = vec![
        "0\t1\t3\tauthor\tpublished_at\tconference".into(),
        "0\t2\t3\tauthor\tpublished_at\tconference".into(),
        "0\t1\t3\tauthor\treviewed_for\tconference".into(),
        "0\t2\t3\tauthor\treviewed_for\tconference".into(),
        "0\t1\t3\tauthor\tadministered\tconference".into(),
        "0\t2\t3\tauthor\tadministered\tconference".into(),
        "0\t1\t4\tauthor\tpublished_at\tconference".into(),
        "0\t2\t4\tauthor\tpublished_at\tconference".into(),
        "0\t2\t5\tauthor\tpublished_at\tconference".into(),
        "0\t6\t7\tauthor\tpublished_at\tconference".into(),
        "0\t8\t9\tauthor\tpublished_at\tconference".into(),
        "0\t10\t11\tauthor\tpublished_at\tconference".into(),
    ];
    let graph_id: GraphId = 0.into();

    let ts = gen_test_typespec();

    // with pruning at degree < 3
    let (sender_prune, _receiver_prune) = channel();
    let search_problem = Rc::new(SearchProblem::new(
        20,
        1.0,
        Some(1.0),
        Some(1.0),
        20,
        10000,
        3,
        3,
    ));
    let schema = Rc::new(TypedGraphSchema::new(ts.clone(), "author".into())?);

    let transformer_prune = Transformer::new(schema, search_problem, true, false)?;
    let rows_prune = process_raw_vector(&transformer_prune, raw.clone())?;

    let graph: TypedGraph = transformer_prune.build_pruned_graph(graph_id, rows_prune)?;
    let v_prune = Vec::new();
    let result_prune = transformer_prune
        .process_clique_rows(&graph, &v_prune, graph_id, false, &sender_prune)?
        .ok_or_else(CLQError::err_none)?;
    sender_prune.send((None, true)).unwrap();
    let candidate_prune = result_prune.top_candidate;
    assert_nodes_have_ids(&graph, &candidate_prune.core_ids, vec![1, 2], true);
    assert_nodes_have_ids(&graph, &candidate_prune.non_core_ids, vec![3], false);

    // without any pruning
    let (sender, _receiver) = channel();
    let search_problem = Rc::new(SearchProblem::new(
        20,
        1.0,
        Some(1.0),
        Some(1.0),
        20,
        10000,
        3,
        0,
    ));
    let schema = Rc::new(TypedGraphSchema::new(ts, "author".into())?);

    let transformer = Transformer::new(schema, search_problem, true, false)?;
    let rows = process_raw_vector(&transformer, raw)?;

    let graph: TypedGraph = transformer.build_pruned_graph(graph_id, rows)?;
    let v = Vec::new();
    let result = transformer
        .process_clique_rows(&graph, &v, graph_id, false, &sender)?
        .ok_or_else(CLQError::err_none)?;
    sender.send((None, true)).unwrap();
    let candidate = result.top_candidate;
    assert_nodes_have_ids(&graph, &candidate.core_ids, vec![1, 2], true);
    assert_nodes_have_ids(&graph, &candidate.non_core_ids, vec![3], false);
    println!("Num steps prune: {}", result_prune.num_steps);
    println!("Num steps: {}", result.num_steps);
    assert!(result_prune.num_steps < result.num_steps);
    Ok(())
}
