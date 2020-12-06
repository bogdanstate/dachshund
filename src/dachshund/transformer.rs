/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this core tree.
 */
extern crate clap;
extern crate serde_json;

use clap::ArgMatches;

use crate::dachshund::beam::{TypedGraphCliqueSearchBeam, TypedGraphCliqueSearchResult};
use crate::dachshund::error::{CLQError, CLQResult};
use crate::dachshund::graph_base::GraphBase;
use crate::dachshund::graph_builder_base::GraphBuilderBase;
use crate::dachshund::id_types::GraphId;
use crate::dachshund::line_processor::LineProcessorBase;
use crate::dachshund::row::{CliqueRow, EdgeRow, Row};
use crate::dachshund::search_problem::SearchProblem;
use crate::dachshund::transformer_base::TransformerBase;
use crate::dachshund::typed_graph::TypedGraph;
use crate::dachshund::typed_graph_builder::TypedGraphBuilder;
use crate::dachshund::typed_graph_line_processor::TypedGraphLineProcessor;
use crate::dachshund::typed_graph_schema::TypedGraphSchema;
use std::rc::Rc;
use std::sync::mpsc::Sender;
use std::sync::Arc;

/// Used to set up the typed graph clique mining algorithm.
pub struct Transformer {
    pub schema: Rc<TypedGraphSchema>,
    pub line_processor: Arc<TypedGraphLineProcessor>,
    pub search_problem: Rc<SearchProblem>,
    pub debug: bool,
    pub long_format: bool,
    edge_rows: Vec<EdgeRow>,
    clique_rows: Vec<CliqueRow>,
}
impl TransformerBase for Transformer {
    fn get_line_processor(&self) -> Arc<dyn LineProcessorBase> {
        self.line_processor.clone()
    }
    fn process_row(&mut self, row: Box<dyn Row>) -> CLQResult<()> {
        if let Some(edge_row) = row.as_edge_row() {
            self.edge_rows.push(edge_row);
        }
        if let Some(clique_row) = row.as_clique_row() {
            self.clique_rows.push(clique_row);
        }
        Ok(())
    }
    fn reset(&mut self) -> CLQResult<()> {
        self.edge_rows.clear();
        self.clique_rows.clear();
        Ok(())
    }
    fn process_batch(
        &mut self,
        graph_id: GraphId,
        output: &Sender<(Option<String>, bool)>,
    ) -> CLQResult<()> {
        let drained_rows = self.edge_rows.drain(..).collect::<Vec<_>>();
        let graph: TypedGraph = self.build_pruned_graph(graph_id, drained_rows)?;
        self.process_clique_rows(
            &graph,
            &self.clique_rows,
            graph_id,
            // verbose
            self.debug,
            output,
        )?;
        Ok(())
    }
}
impl Transformer {
    /// Called by main.rs module to set up the beam search. Parameters are as follows:
    ///     - `typespec`: a command-line argument, of the form:
    ///     [["author", "published_in", "journal"], ["author", "co-authored", "article"]].
    ///     This sets up the semantics related to the set of relations contained in the
    ///     typed graph. A requirement is that all relations share a "core" type, in this
    ///     case, "author".
    ///     - `beam_size`: TypedGraphCliqueSearchBeam construction parameter. The number of top candidates to
    ///     maintain as potential future cores for expansion in the "beam" (i.e., the list of top candidates).
    ///     - `alpha`: `Scorer` constructor parameter. Controls the contribution of density
    ///     - `global_thresh`: `Scorer` constructor parameter. If provided, candidates must be at
    ///     least this dense to be considered valid (quasi-)cliques.
    ///     - `local_thresh`: `Scorer` constructor parameter. if provided, each node in the candidate
    ///     must have at least `local_thresh` proportion of ties to other nodes in the candidate,
    ///     for the candidate to be considered valid.
    ///     - `num_to_search`: number of expansion candidates to consider for each candidate in the
    ///     beam.
    ///     - `num_epochs`: maximum number of epochs to run search for.
    ///     - `max_repeated_prior_scores`: maximum number of times for which the top score can be
    ///     repeated in consecutive epochs, before the search gets shut down early.
    ///     - `debug`: whether to produce verbose output in the search process.
    ///     - `min_degree`: minimum degree required for each node in a (quasi-)clique in order for
    ///     the subgraph to be considered interesting.
    ///     - `core_type`: the core type, as found in the typespec.
    ///     - `long_format`: whether to output results in long format, of the form:
    ///     `graph_id\tnode_id\tnode_type`, instead of the more user-friendly (but
    ///     machine-unfriendly) wide format.
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        schema: Rc<TypedGraphSchema>,
        search_problem: Rc<SearchProblem>,
        debug: bool,
        long_format: bool,
    ) -> CLQResult<Self> {
        let line_processor = Arc::new(TypedGraphLineProcessor::new(schema.clone()));
        let transformer = Self {
            schema,
            line_processor,
            search_problem,
            debug,
            long_format,
            edge_rows: Vec::new(),
            clique_rows: Vec::new(),
        };
        Ok(transformer)
    }

    /// constructs a transformer from an ArgMatches object (to help with command line arguments).
    pub fn from_argmatches(matches: ArgMatches) -> CLQResult<Self> {
        let arg_value = |name: &str| -> CLQResult<&str> {
            matches
                .value_of(name)
                .ok_or_else(|| CLQError::from(format!("Missing required argument: {}", name)))
        };
        let typespec_str: &str = arg_value("typespec")?;
        let typespec: Vec<Vec<String>> = serde_json::from_str(typespec_str)?;
        let beam_size: usize = arg_value("beam_size")?.parse::<usize>()?;
        let alpha: f32 = arg_value("alpha")?.parse::<f32>()?;
        let global_thresh: Option<f32> = Some(arg_value("global_thresh")?.parse::<f32>()?);
        let local_thresh: Option<f32> = Some(arg_value("local_thresh")?.parse::<f32>()?);
        let num_to_search: usize = arg_value("num_to_search")?.parse::<usize>()?;
        let num_epochs: usize = arg_value("epochs")?.parse::<usize>()?;
        let max_repeated_prior_scores: usize =
            arg_value("max_repeated_prior_scores")?.parse::<usize>()?;
        let debug: bool = arg_value("debug_mode")?.parse::<bool>()?;
        let min_degree: usize = arg_value("min_degree")?.parse::<usize>()?;
        let core_type: String = arg_value("core_type")?.parse::<String>()?;
        let long_format: bool = arg_value("long_format")?.parse::<bool>()?;

        let search_problem = Rc::new(SearchProblem::new(
            beam_size,
            alpha,
            global_thresh,
            local_thresh,
            num_to_search,
            num_epochs,
            max_repeated_prior_scores,
            min_degree,
        ));
        let schema = Rc::new(TypedGraphSchema::new(typespec, core_type)?);

        let transformer = Transformer::new(schema, search_problem, debug, long_format)?;
        Ok(transformer)
    }

    /// builds graph, pruned to ensure all nodes have at least self.min_degree degree
    /// with other nodes in the graph. This is done via a greedy algorithm which removes
    /// low-degree nodes iteratively.
    #[allow(clippy::ptr_arg)]
    pub fn build_pruned_graph(
        &self,
        graph_id: GraphId,
        rows: Vec<EdgeRow>,
    ) -> CLQResult<TypedGraph> {
        TypedGraphBuilder {
            graph_id,
            min_degree: Some(self.search_problem.min_degree),
            schema: self.schema.clone(),
        }
        .from_vector(rows)
    }

    /// Given a properly-built graph, runs the quasi-clique detection beam search on it.
    pub fn process_graph<'a>(
        &'a self,
        graph: &'a TypedGraph,
        clique_rows: &'a Vec<CliqueRow>,
        graph_id: GraphId,
        verbose: bool,
    ) -> CLQResult<TypedGraphCliqueSearchResult<'a>> {
        let mut beam = TypedGraphCliqueSearchBeam::new(
            graph,
            clique_rows,
            verbose,
            self.schema.get_non_core_types(),
            self.schema.get_num_non_core_types(),
            self.search_problem.clone(),
            graph_id,
        )?;
        beam.run_search()
    }
    /// Used to "seed" the beam search with an existing best (quasi-)clique (if any provided),
    /// and then run the search under the parameters specified in the constructor.
    pub fn process_clique_rows<'a>(
        &'a self,
        graph: &'a TypedGraph,
        clique_rows: &'a Vec<CliqueRow>,
        graph_id: GraphId,
        verbose: bool,
        output: &Sender<(Option<String>, bool)>,
    ) -> CLQResult<Option<TypedGraphCliqueSearchResult<'a>>> {
        if graph.get_core_ids().is_empty() || graph.get_non_core_ids().unwrap().is_empty() {
            // still have to send an acknowledgement to the output channel
            // that we have actually processed this graph, otherwise
            // we lose track of how many graphs have been processed so
            // far!
            output.send((None, false)).unwrap();
            return Ok(None);
        }
        let result = self.process_graph(graph, clique_rows, graph_id, verbose)?;
        // only print if this is a conforming clique
        if result.top_candidate.get_score()? > 0.0 {
            if !self.long_format {
                let line: String = format!(
                    "{}\t{}",
                    graph_id.value(),
                    result
                        .top_candidate
                        .to_printable_row(&self.schema.get_non_core_types())?,
                );
                output.send((Some(line), false)).unwrap();
            } else {
                result.top_candidate.print(
                    graph_id,
                    &self.schema.get_non_core_types(),
                    &self.schema.get_core_type(),
                    output,
                )?;
            }
        }
        Ok(Some(result))
    }
}
