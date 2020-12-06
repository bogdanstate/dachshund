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
use lib_dachshund::dachshund::typed_graph_builder::TypedGraphBuilderWithCliques;
use lib_dachshund::dachshund::typed_graph_line_processor::TypedGraphLineProcessor;
use lib_dachshund::dachshund::typed_graph_schema::TypedGraphSchema;

use std::collections::BTreeSet;
use std::rc::Rc;

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
    let schema = Rc::new(TypedGraphSchema::new(typespec, "author".to_string())?);
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
        vec![(
            vec![1 as i64, 2 as i64, 3 as i64]
                .into_iter()
                .map(|x| NodeId::from(x))
                .collect::<BTreeSet<_>>(),
            vec![5 as i64, 6 as i64, 7 as i64]
                .into_iter()
                .map(|x| NodeId::from(x))
                .collect::<BTreeSet<_>>(),
        )],
        schema,
    );
    let processed_rows = builder_with_cliques
        .pre_process_rows(rows)?
        .into_iter()
        .collect::<BTreeSet<_>>();
    assert_eq!(processed_rows.len(), raw.len() + 9 * 2 - 3);
    Ok(())
}
