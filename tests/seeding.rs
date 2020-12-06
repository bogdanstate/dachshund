/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
extern crate lib_dachshund;

use lib_dachshund::dachshund::error::{CLQError, CLQResult};
use lib_dachshund::dachshund::graph_builder_base::GraphBuilderBaseWithPreProcessing;
use lib_dachshund::dachshund::id_types::{GraphId, NodeId};
use lib_dachshund::dachshund::line_processor::LineProcessorBase;
use lib_dachshund::dachshund::row::EdgeRow;
use lib_dachshund::dachshund::typed_graph_builder::TypedGraphBuilderWithCliques;
use lib_dachshund::dachshund::typed_graph_line_processor::TypedGraphLineProcessor;
use lib_dachshund::dachshund::typed_graph_schema::TypedGraphSchema;

use std::collections::BTreeSet;
use std::rc::Rc;

fn get_seeded_rows(
    schema: Rc<TypedGraphSchema>,
    raw: Vec<String>,
    cliques: Vec<(Vec<i64>, Vec<i64>)>,
) -> CLQResult<BTreeSet<EdgeRow>> {
    let line_processor = TypedGraphLineProcessor::new(schema.clone());
    let rows = raw
        .iter()
        .map(|line| {
            line_processor
                .process_line(line.clone())
                .unwrap()
                .as_edge_row()
                .ok_or_else(CLQError::err_none)
                .unwrap()
        })
        .collect::<Vec<_>>();
    let graph_id = GraphId::from(0);
    let mut builder_no_cliques =
        TypedGraphBuilderWithCliques::new(graph_id, Vec::new(), schema.clone());
    let processed_rows = builder_no_cliques.pre_process_rows(rows.clone())?;
    assert_eq!(processed_rows.len(), raw.len());
    let mut builder_with_cliques = TypedGraphBuilderWithCliques::new(
        graph_id,
        cliques
            .into_iter()
            .map(|(x1, x2)| {
                (
                    x1.into_iter().map(|x| NodeId::from(x)).collect(),
                    x2.into_iter().map(|x| NodeId::from(x)).collect(),
                )
            })
            .collect(),
        schema,
    );
    let processed_rows = builder_with_cliques
        .pre_process_rows(rows)?
        .into_iter()
        .collect::<BTreeSet<_>>();
    Ok(processed_rows)
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
    let processed_rows = get_seeded_rows(schema, raw, vec![(vec![1, 2, 3], vec![5, 6, 7])])?;
    assert_eq!(processed_rows.len(), num_original_rows + 9 * 2 - 3);
    Ok(())
}
