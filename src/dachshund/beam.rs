/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
extern crate rand;

use std::collections::hash_map::DefaultHasher;
use std::collections::{HashMap, HashSet};
use std::hash::{Hash, Hasher};

use rand::prelude::*;

use crate::dachshund::candidate::Candidate;
use crate::dachshund::error::{CLQError, CLQResult};
use crate::dachshund::graph_base::GraphBase;
use crate::dachshund::id_types::{GraphId, NodeId};
use crate::dachshund::row::CliqueRow;
use crate::dachshund::scorer::Scorer;
use crate::dachshund::search_problem::SearchProblem;
use crate::dachshund::typed_graph::TypedGraph;

use std::rc::Rc;

/// The result of a beam search.
pub struct TypedGraphCliqueSearchResult<'a> {
    pub top_candidate: Candidate<'a, TypedGraph>,
    pub num_steps: usize,
}

/// Used for (quasi-clique) detection. A singleton object that keeps state across the beam search.
/// At any point this object considers a "beam" of candidates that is always kept under beam_size,
/// to avoid exponential blowup of the search space.
pub struct TypedGraphCliqueSearchBeam<'a> {
    pub candidates: Vec<Candidate<'a, TypedGraph>>,
    pub graph: &'a TypedGraph,
    pub search_problem: Rc<SearchProblem>,
    verbose: bool,
    non_core_types: Vec<String>,
    visited_candidates: HashSet<u64>,
    scorer: Scorer,
}

impl<'a> TypedGraphCliqueSearchBeam<'a> {
    /// performs a random walk of length `length` along the graph,
    /// starting at a particular node.
    fn random_walk(
        rng: &mut impl Rng,
        graph: &TypedGraph,
        node: NodeId,
        length: i16,
    ) -> CLQResult<NodeId> {
        let mut current: NodeId = node;
        for _i in 0..length {
            let next = graph
                .get_node(current)
                .edges
                .choose(rng)
                .ok_or_else(CLQError::err_none)?
                .target_id;
            current = next;
        }
        Ok(current)
    }

    /// creates new beam for mining quasi-bicliques. The following parameters are required:
    ///     - `graph`: a reference to a `TypedGraph` object (typically constructed by a transformer`.
    ///     - `clique_rows`: a Vector of `CliqueRow` entries, which are used to initialize the
    ///     search process with already-existing cliques.
    ///     - `beam_size`: the number of top candidates to maintain as potential future sources
    ///     for expansion in the "beam" (i.e., the list of top candidates).
    ///     - `verbose`: used for debugging.
    ///     - `alpha`: `Scorer` constructor parameter. Controls the contribution of density
    ///     to the ``cliqueness'' score. Higher values means denser cliques are prefered, all else
    ///     being equal.
    ///     - `global_thresh`: `Scorer` constructor parameter. If provided, candidates must be at
    ///     least this dense to be considered valid (quasi-)cliques.
    ///     - `local_thresh`: `Scorer` constructor parameter. if provided, each node in the candidate
    ///     must have at least `local_thresh` proportion of ties to other nodes in the candidate,
    ///     for the candidate to be considered valid.
    ///     - `graph_id`: uniquely identifies the graph currently being processed.
    pub fn new(
        search_problem: Rc<SearchProblem>,
        graph_id: GraphId,
        graph: &'a TypedGraph,
        clique_rows: &'a Vec<CliqueRow>,
        verbose: bool,
    ) -> CLQResult<TypedGraphCliqueSearchBeam<'a>> {
        let core_ids: &Vec<NodeId> = &graph.get_core_ids();
        let non_core_ids: &Vec<NodeId> = &graph.get_non_core_ids().unwrap();
        let schema = graph.get_schema();
        let non_core_types = schema.get_non_core_types();
        let num_non_core_types = schema.get_num_non_core_types();
        let mut candidates: Vec<Candidate<TypedGraph>> = Vec::new();
        let scorer: Scorer = Scorer::new(num_non_core_types, &search_problem);

        // To ensure deterministic behaviour between two identically configured runs,
        // seed the pseudorandom sequence with the current cluster.
        let mut seeder = DefaultHasher::new();
        graph_id.hash(&mut seeder);
        let mut rng = StdRng::seed_from_u64(seeder.finish());

        if !clique_rows.is_empty() {
            let init_clique = Candidate::from_clique_rows(clique_rows, graph, &scorer)?;
            if init_clique != None {
                candidates.push(init_clique.unwrap());
            }
        }

        while candidates.len() < search_problem.beam_size {
            assert!(!core_ids.is_empty());
            assert!(!non_core_ids.is_empty());
            let ids_vec = if rng.gen::<f32>() <= 0.5 {
                &non_core_ids
            } else {
                &core_ids
            };
            assert!(!ids_vec.is_empty());
            let root_id = ids_vec
                .choose(&mut rng)
                .ok_or_else(|| format!("Problem finding root in graph_id: {}", graph_id.value()))?;
            let candidate_node =
                TypedGraphCliqueSearchBeam::random_walk(&mut rng, graph, *root_id, 7)?;
            let candidate = Candidate::new(candidate_node, graph, &scorer)?;
            candidates.push(candidate);
        }
        let visited_candidates: HashSet<u64> = HashSet::new();
        Ok(TypedGraphCliqueSearchBeam {
            candidates,
            graph,
            search_problem,
            verbose,
            non_core_types,
            visited_candidates,
            scorer,
        })
    }

    /// Try expanding each member of the beam and keep the top candidates.
    fn one_step_search(
        &mut self,
        num_to_search: usize,
        beam_size: usize,
    ) -> CLQResult<(Candidate<'a, TypedGraph>, bool)> {
        let mut scored_expansion_candidates: HashSet<Candidate<TypedGraph>> = HashSet::new();
        let mut new_candidates: Vec<Candidate<TypedGraph>> = Vec::new();
        let mut can_continue: bool = false;
        // A map from a checksum to a reference to a candidate from the previous generation.
        // Used as a hint when materializing the neighborhood for the next generation of candidates.
        let mut previous_candidates = HashMap::new();

        for candidate in &self.candidates {
            if self.verbose {
                eprintln!(
                    "Considering the following candidate (score = {}, hash={}):\n{}",
                    match candidate.get_score() {
                        Ok(n) => n.to_string(),
                        Err(_) => "No score".to_string(),
                    },
                    candidate,
                    candidate.to_printable_row(&self.non_core_types)?,
                );
            }
            if !self
                .visited_candidates
                .contains(&candidate.checksum.unwrap())
            {
                can_continue = true;
                let v: Vec<Candidate<TypedGraph>> = candidate.one_step_search(
                    num_to_search,
                    &mut self.visited_candidates,
                    &self.scorer,
                )?;
                if self.verbose {
                    eprintln!("Have {} visited candidates:", self.visited_candidates.len());
                    eprintln!("Found the following expansion candidates:");
                }
                for ell in v {
                    if self.verbose {
                        eprintln!(
                            "(score = {}): {}",
                            ell.get_score()?,
                            ell.to_printable_row(&self.non_core_types)?,
                        );
                    }
                    scored_expansion_candidates.insert(ell);
                }
            }
            previous_candidates.insert(candidate.checksum.unwrap(), candidate);
            scored_expansion_candidates.insert(candidate.replicate(true));
        }

        // sort by score, with node_id as tie breaker for deterministic behaviour
        let mut v: Vec<Candidate<TypedGraph>> = scored_expansion_candidates.into_iter().collect();

        let mut bad_sort = false;
        v.sort_by(|a, b| {
            if let (Ok(a_score), Ok(b_score)) = (a.get_score(), b.get_score()) {
                let key_a = (a_score, a.checksum);
                let key_b = (b_score, b.checksum);
                if let Some(comparison) = key_a.partial_cmp(&key_b) {
                    return comparison.reverse();
                }
            }
            bad_sort = true;
            std::cmp::Ordering::Equal
        });
        if bad_sort {
            return Err(CLQError::new(
                "Failed to sort at least one unscored candidate.",
            ));
        }

        if self.verbose {
            eprintln!("TypedGraphCliqueSearchBeam now contains:");
        }
        for mut ell in v {
            if new_candidates.len() < beam_size {
                ell.set_neigbhorhood_with_hint(&previous_candidates);
                new_candidates.push(ell);
            }
        }
        self.candidates = new_candidates;
        Ok((self.candidates[0].replicate(true), can_continue))
    }

    /// runs one_step_search for `num_epochs` epochs, trying `num_to_search`
    /// expansion candidates for each candidate in the beam (the list of top
    /// candidates found so far). The beam is of `beam_size`. If the top
    /// score resulting from a one step search is repeated `max_repeated_prior_scores`
    /// times, the search is terminated early. (Note that the search has a stochastic
    /// component, which is why repeating the search may yield different results).
    pub fn run_search(&mut self) -> CLQResult<TypedGraphCliqueSearchResult<'a>> {
        let mut prior_score: f32 = -2.0;
        let mut num_repeated_prior_scores: usize = 0;
        let mut num_steps: usize = 0;
        if self.search_problem.num_epochs > 0 {
            for i in 0..self.search_problem.num_epochs - 1 {
                num_steps = i + 1;
                let (top, can_continue): (Candidate<TypedGraph>, bool) = self.one_step_search(
                    self.search_problem.num_to_search,
                    self.search_problem.beam_size,
                )?;
                // result of all candidates being previously visited
                if !can_continue {
                    break;
                }
                let score: f32 = top.get_score()?;
                if self.verbose {
                    eprintln!(
                        "Top candidate found: (score = {}): {}",
                        score,
                        top.to_printable_row(&self.non_core_types)?,
                    );
                }
                assert!(score >= prior_score);
                if self.verbose {
                    eprintln!("Score: {}, prior score: {}", score, prior_score);
                }
                if (score - prior_score).abs() <= f32::EPSILON {
                    num_repeated_prior_scores += 1;
                } else {
                    num_repeated_prior_scores = 0;
                }
                if num_repeated_prior_scores == self.search_problem.max_repeated_prior_scores {
                    break;
                }
                prior_score = score;
            }
            let result = self.one_step_search(
                self.search_problem.num_to_search,
                self.search_problem.beam_size,
            )?;
            return Ok(TypedGraphCliqueSearchResult {
                top_candidate: result.0,
                num_steps,
            });
        }
        // if we're just running for 0 epochs (for debug purposes, return top candidate)
        let mut best_candidate: Candidate<TypedGraph> = self.candidates[0].replicate(true);
        let mut best_score: f32 = 0.0;
        for candidate in &self.candidates {
            let score = candidate.get_score()?;
            if score > best_score {
                best_candidate = candidate.replicate(true);
                best_score = score;
            }
        }
        Ok(TypedGraphCliqueSearchResult {
            top_candidate: best_candidate,
            num_steps: 0,
        })
    }
}
