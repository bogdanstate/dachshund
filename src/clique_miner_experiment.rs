/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#![feature(map_first_last)]
extern crate clap;
extern crate lib_dachshund;

use clap::{App, ArgMatches};
use lib_dachshund::dachshund::beam::TopCandidateBeamSearchObserver;
use lib_dachshund::dachshund::error::CLQResult;
use lib_dachshund::dachshund::typed_graph_builder::TypedGraphBuilderWithCliquesOverRandomGraph;
use lib_dachshund::{
    GraphBase, GraphId, SearchProblem, TypedGraphCliqueSearchBeam, TypedGraphSchema,
};
use maplit::hashmap;
use std::collections::HashMap;
use std::rc::Rc;

fn get_command_line_args() -> ArgMatches<'static> {
    let matches: ArgMatches = App::new("Dachshund")
        .version("0.1.0")
        .author(
            "
                Alex Peysakhovich <alexpeys@fb.com>, \
                Bogdan State <bogdanstate@fb.com>, \
                Julian Mestre <julianmestre@fb.com>, \
                Michael Chen <mvc@fb.com>,
                Matthew Menard <mlmenard@fb.com>,
                Pär Winzell <zell@fb.com>",
        )
        .about("Finds (quasi-)bicliques in Erdos-Renyi Random Graphs.")
        .get_matches();
    matches
}

fn run_experiment(
    id: usize,
    search_problem: Rc<SearchProblem>,
) -> CLQResult<()> {
    let typespec = vec![
        vec!["author".to_string(), "published".into(), "article".into()],
        vec!["author".to_string(), "published".into(), "book".into()],
    ];
    let schema = Rc::new(TypedGraphSchema::new(typespec, "author".to_string())?);
    let node_type_counts: HashMap<String, usize> = hashmap! {
        "author".into() => 1000,
        "article".into() => 1000,
        "book".into() => 1000,
    };
    let clique_sizes: Vec<(usize, HashMap<(String, String), usize>)> = vec![(
        10,
        hashmap! {
            ("article".into(), "published".into()) => 90,
            ("book".into(), "published".into()) => 10,
        },
    )];
    let erdos_renyi_probabilities: HashMap<(String, String, String), f64> = hashmap! {
        ("author".into(), "article".into(), "published".into()) => 0.01,
        ("author".into(), "book".into(), "published".into()) => 0.01,
    };

    let mut builder = TypedGraphBuilderWithCliquesOverRandomGraph::new(
        GraphId::from(0),
        schema,
        node_type_counts,
        clique_sizes,
        erdos_renyi_probabilities,
    )?;
    builder.generate_cliques()?;
    let graph = builder.generate_graph()?;
    let clique_rows = Vec::new();
    let mut beam = TypedGraphCliqueSearchBeam::new(
        search_problem,
        GraphId::from(0),
        &graph,
        &clique_rows,
        false,
    )?;
    let observer = TopCandidateBeamSearchObserver::new(id);
    beam.bind_observer(observer);
    let _best = beam.run_search()?;
    print!("{}", beam.get_observer());
    Ok(())
}

fn main() -> CLQResult<()> {
    let _matches: ArgMatches = get_command_line_args();
    let search_problem = Rc::new(SearchProblem::new(
        200,
        1.0,
        Some(1.0),
        Some(1.0),
        100,
        1000,
        10,
        1,
    ));
    println!("{}", TopCandidateBeamSearchObserver::get_header());
    for id in 0..100 {
        run_experiment(id, search_problem.clone())?;
    }
    Ok(())
}
