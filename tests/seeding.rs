/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
extern crate lib_dachshund;

use lib_dachshund::dachshund::beam::TypedGraphCliqueSearchBeam;
use lib_dachshund::dachshund::error::{CLQError, CLQResult};
use lib_dachshund::dachshund::graph_base::GraphBase;
use lib_dachshund::dachshund::graph_builder_base::{
    GraphBuilderBase, GraphBuilderBaseWithPreProcessing,
};
use lib_dachshund::dachshund::id_types::{GraphId, NodeId};
use lib_dachshund::dachshund::line_processor::LineProcessorBase;
use lib_dachshund::dachshund::row::EdgeRow;
use lib_dachshund::dachshund::search_problem::SearchProblem;
use lib_dachshund::dachshund::typed_graph::TypedGraph;
use lib_dachshund::dachshund::typed_graph_builder::TypedGraphBuilderWithCliques;
use lib_dachshund::dachshund::typed_graph_line_processor::TypedGraphLineProcessor;
use lib_dachshund::dachshund::typed_graph_schema::TypedGraphSchema;

use std::collections::BTreeSet;
use std::rc::Rc;

fn get_builder_with_cliques(
    graph_id: GraphId,
    cliques: &Vec<(Vec<i64>, Vec<i64>)>,
    schema: Rc<TypedGraphSchema>,
) -> TypedGraphBuilderWithCliques {
    TypedGraphBuilderWithCliques::new(
        graph_id,
        cliques
            .into_iter()
            .map(|(x1, x2)| {
                (
                    x1.into_iter().map(|x| NodeId::from(x.clone())).collect(),
                    x2.into_iter().map(|x| NodeId::from(x.clone())).collect(),
                )
            })
            .collect(),
        schema,
    )
}

fn raw_to_edge_row(line_processor: &TypedGraphLineProcessor, raw: &Vec<String>) -> Vec<EdgeRow> {
    raw.iter()
        .map(|line| {
            line_processor
                .process_line(line.clone())
                .unwrap()
                .as_edge_row()
                .ok_or_else(CLQError::err_none)
                .unwrap()
        })
        .collect::<Vec<_>>()
}

fn get_seeded_rows(
    graph_id: GraphId,
    schema: Rc<TypedGraphSchema>,
    raw: &Vec<String>,
    cliques: &Vec<(Vec<i64>, Vec<i64>)>,
) -> CLQResult<BTreeSet<EdgeRow>> {
    let line_processor = TypedGraphLineProcessor::new(schema.clone());
    let num_original_rows = raw.len();
    let rows = raw_to_edge_row(&line_processor, raw);
    let mut builder_no_cliques =
        TypedGraphBuilderWithCliques::new(graph_id, Vec::new(), schema.clone());
    let mut builder_with_cliques = get_builder_with_cliques(graph_id, cliques, schema);
    let processed_rows = builder_no_cliques.pre_process_rows(rows.clone())?;
    assert_eq!(processed_rows.len(), num_original_rows);
    let processed_rows = builder_with_cliques
        .pre_process_rows(rows)?
        .into_iter()
        .collect::<BTreeSet<_>>();
    Ok(processed_rows)
}

fn get_seeded_graph(
    graph_id: GraphId,
    schema: Rc<TypedGraphSchema>,
    raw: &Vec<String>,
    cliques: &Vec<(Vec<i64>, Vec<i64>)>,
) -> CLQResult<TypedGraph> {
    let line_processor = TypedGraphLineProcessor::new(schema.clone());
    let rows = raw_to_edge_row(&line_processor, raw);
    let mut builder = get_builder_with_cliques(graph_id, cliques, schema);
    builder.from_vector(rows)
}

#[cfg(test)]
#[test]
fn test_typed_graph_seeding() -> CLQResult<()> {
    let typespec = vec![
        vec!["author".to_string(), "published".into(), "article".into()],
        vec!["author".to_string(), "cited".into(), "article".into()],
        vec!["author".to_string(), "published".into(), "book".into()],
    ];
    let raw = vec![
        "0\t1\t5\tauthor\tpublished\tarticle".to_string(),
        "0\t2\t6\tauthor\tpublished\tarticle".into(),
        "0\t3\t7\tauthor\tpublished\tarticle".into(),
        "0\t4\t8\tauthor\tcited\tarticle".into(),
    ];
    let num_original_rows = raw.len();
    let schema = Rc::new(TypedGraphSchema::new(typespec, "author".to_string())?);
    let cliques = vec![(vec![1, 2, 3], vec![5, 6, 7])];
    let processed_rows = get_seeded_rows(GraphId::from(0), schema.clone(), &raw, &cliques)?;
    assert_eq!(processed_rows.len(), num_original_rows + 9 * 2 - 3);

    let graph = get_seeded_graph(GraphId::from(0), schema, &raw, &cliques)?;
    assert_eq!(graph.count_edges() / 2, processed_rows.len());
    Ok(())
}

#[test]
fn test_typed_graph_seeding_two_cliques() -> CLQResult<()> {
    let typespec = vec![
        vec!["author".to_string(), "published".into(), "article".into()],
        vec!["author".to_string(), "cited".into(), "article".into()],
        vec!["author".to_string(), "published".into(), "book".into()],
    ];
    let raw = vec![
        "0\t1\t5\tauthor\tpublished\tarticle".to_string(),
        "0\t2\t6\tauthor\tpublished\tarticle".into(),
        "0\t3\t7\tauthor\tpublished\tarticle".into(),
        "0\t4\t8\tauthor\tcited\tarticle".into(),
        "0\t1\t9\tauthor\tpublished\tbook".into(),
        "0\t1\t10\tauthor\tpublished\tarticle".into(),
        "0\t1\t11\tauthor\tpublished\tarticle".into(),
        "0\t12\t5\tauthor\tpublished\tarticle".into(),
        "0\t13\t5\tauthor\tcited\tarticle".into(),
    ];
    let num_original_rows = raw.len();
    let schema = Rc::new(TypedGraphSchema::new(typespec, "author".to_string())?);
    let cliques = vec![
        (vec![1, 2, 3], vec![5, 6, 7]),
        (vec![12, 13], vec![8, 10, 11]),
    ];
    let processed_rows = get_seeded_rows(GraphId::from(0), schema.clone(), &raw, &cliques)?;
    assert_eq!(processed_rows.len(), num_original_rows + 9 * 2 - 3 + 6 * 2);

    let graph = get_seeded_graph(GraphId::from(0), schema, &raw, &cliques)?;
    assert_eq!(graph.count_edges() / 2, processed_rows.len());

    let search_problem = Rc::new(SearchProblem::new(
        2000,
        1.0,
        Some(1.0),
        Some(1.0),
        2000,
        10,
        10,
        0,
    ));
    let clique_rows = Vec::new();
    let mut beam = TypedGraphCliqueSearchBeam::new(
        search_problem,
        GraphId::from(0),
        &graph,
        &clique_rows,
        false,
    )?;
    let best = beam.run_search()?;
    print!("{:?}", best.top_candidate.core_ids);
    print!("{:?}", best.top_candidate.non_core_ids);
    assert_eq!(best.top_candidate.non_core_ids.len(), 3);
    assert_eq!(best.top_candidate.core_ids.len(), 3);
    let _res = vec![1, 2, 3]
        .into_iter()
        .map(|x| assert!(best.top_candidate.core_ids.contains(&NodeId::from(x))))
        .collect::<Vec<_>>();
    let _res = vec![5, 6, 7]
        .into_iter()
        .map(|x| assert!(best.top_candidate.non_core_ids.contains(&NodeId::from(x))))
        .collect::<Vec<_>>();
    Ok(())
}
