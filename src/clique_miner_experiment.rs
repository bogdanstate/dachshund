/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#![feature(map_first_last)]
extern crate clap;
extern crate lib_dachshund;

use clap::{App, Arg, ArgMatches};
use indoc::indoc;
use lib_dachshund::dachshund::beam::TopCandidateBeamSearchObserver;
use lib_dachshund::dachshund::error::{CLQError, CLQResult};
use lib_dachshund::dachshund::typed_graph_builder::TypedGraphBuilderWithCliquesOverRandomGraph;
use lib_dachshund::{
    GraphBase, GraphId, SearchProblem, TypedGraphCliqueSearchBeam, TypedGraphSchema,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::io::Write;
use std::rc::Rc;

fn get_command_line_args() -> ArgMatches<'static> {
    let matches: ArgMatches = App::new("Dachshund")
        .version("0.1.0")
        .author(
            "
                Alex Peysakhovich <alexpeys@fb.com>, \
                Bogdan State <bogdan@scie.nz>, \
                Julian Mestre <julianmestre@fb.com>, \
                Michael Chen <mvc@fb.com>,
                Matthew Menard <mlmenard@fb.com>,
                Pär Winzell <zell@fb.com>",
        )
        .about("Generates Erdos-Renyi bipartite graph with injected cliques.")
        .arg(
            Arg::with_name("config")
                .short("c")
                .long("config")
                .takes_value(true)
                .help(indoc!(
                    "YAML config file setting up experiment. See
                     example.yaml."
                )),
        )
        .arg(
            Arg::with_name("edges")
                .short("e")
                .long("edges")
                .takes_value(true)
                .help(indoc!("File in which generated edges will be stored.")),
        )
        .arg(
            Arg::with_name("cliques")
                .short("q")
                .long("cliques")
                .takes_value(true)
                .help(indoc!("File in which generated cliques will be stored.")),
        )
        .get_matches();
    matches
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct RightShoreSize {
    edge_type: String,
    node_type: String,
    num_nodes: usize,
}

impl RightShoreSize {
    pub fn as_map_entry(&self) -> ((String, String), usize) {
        (
            (self.node_type.clone(), self.edge_type.clone()),
            self.num_nodes,
        )
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct CliqueSize {
    num_left_shore: usize,
    right_shore_sizes: Vec<RightShoreSize>,
}

impl CliqueSize {
    pub fn as_tuple(&self) -> (usize, HashMap<(String, String), usize>) {
        (
            self.num_left_shore,
            self.right_shore_sizes
                .iter()
                .map(|x| x.as_map_entry())
                .collect(),
        )
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct TypeSpec {
    edge_type: String,
    non_core_type: String,
    pub erdos_renyi_probability: f64,
}

impl TypeSpec {
    pub fn as_vector(&self, core_type: String) -> Vec<String> {
        vec![
            core_type,
            self.edge_type.clone(),
            self.non_core_type.clone(),
        ]
    }
    pub fn as_tuple(&self, core_type: String) -> (String, String, String) {
        (
            core_type,
            self.non_core_type.clone(),
            self.edge_type.clone(),
        )
    }
}

#[derive(Debug, PartialEq, Serialize, Deserialize)]
struct ExperimentSetup {
    pub core_type: String,
    typespec: Vec<TypeSpec>,
    pub node_type_counts: HashMap<String, usize>,
    clique_sizes: Vec<CliqueSize>,
}

impl ExperimentSetup {
    pub fn get_string_typespec(&self) -> Vec<Vec<String>> {
        self.typespec
            .iter()
            .map(|x| x.as_vector(self.core_type.clone()))
            .collect()
    }
    pub fn get_clique_sizes(&self) -> Vec<(usize, HashMap<(String, String), usize>)> {
        self.clique_sizes.iter().map(|x| x.as_tuple()).collect()
    }
    pub fn get_erdos_renyi_probabilities(&self) -> HashMap<(String, String, String), f64> {
        self.typespec
            .iter()
            .map(|x| {
                (
                    x.as_tuple(self.core_type.clone()),
                    x.erdos_renyi_probability,
                )
            })
            .collect()
    }
}

fn run_experiment(id: usize, search_problem: Rc<SearchProblem>) -> CLQResult<()> {
    let matches = get_command_line_args();
    let arg_value = |name: &str| -> CLQResult<&str> {
        matches
            .value_of(name)
            .ok_or_else(|| CLQError::from(format!("Missing required argument: {}", name)))
    };
    let config_file_name = arg_value("config")?;
    let edges_file_name = arg_value("edges")?;
    let cliques_file_name = arg_value("cliques")?;
    let s = fs::read_to_string(config_file_name)?;
    let setup: ExperimentSetup = serde_yaml::from_str(&s)?;
    let typespec = setup.get_string_typespec();
    let schema = Rc::new(TypedGraphSchema::new(typespec, setup.core_type.clone())?);
    let clique_sizes = setup.get_clique_sizes();

    let mut builder = TypedGraphBuilderWithCliquesOverRandomGraph::new(
        GraphId::from(0),
        schema.clone(),
        setup.node_type_counts.clone(),
        setup.get_clique_sizes(),
        setup.get_erdos_renyi_probabilities(),
    )?;
    builder.generate_cliques()?;
    let rows = builder.generate_rows();
    let mut rows_file = fs::File::create(edges_file_name)?;
    for row in &rows {
        rows_file.write(format!("{}\n", schema.get_human_friendly_row(row)?).as_bytes())?;
    }
    let raw_cliques = builder.get_serializable_cliques();
    let cliques = serde_yaml::to_string(&raw_cliques)?;
    
    let mut cliques_file = fs::File::create(cliques_file_name)?;
    cliques_file.write(cliques.as_bytes())?;

    /*let graph = builder.generate_graph()?;
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
    print!("{}", beam.get_observer());*/
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
