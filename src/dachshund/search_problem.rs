use std::fmt;
/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
pub struct SearchProblem {
    pub beam_size: usize,
    pub alpha: f32,
    pub global_thresh: Option<f32>,
    pub local_thresh: Option<f32>,
    pub num_to_search: usize,
    pub num_epochs: usize,
    pub max_repeated_prior_scores: usize,
    pub min_degree: usize,
}
impl SearchProblem {
    pub fn new(
        beam_size: usize,
        alpha: f32,
        global_thresh: Option<f32>,
        local_thresh: Option<f32>,
        num_to_search: usize,
        num_epochs: usize,
        max_repeated_prior_scores: usize,
        min_degree: usize,
    ) -> Self {
        Self {
            beam_size,
            alpha,
            global_thresh,
            local_thresh,
            num_to_search,
            num_epochs,
            max_repeated_prior_scores,
            min_degree,
        }
    }
    pub fn get_header() -> String {
        "beam_size\talpha\tglobal_thresh\tlocal_thresh\tnum_to_search\tnum_epochs\tmax_repeated_prior_scores\tmin_degree".to_string()
    }
}
impl fmt::Display for SearchProblem {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}\t{}\t{}\t{}\t{}\t{}\t{}\t{}",
            self.beam_size,
            self.alpha,
            match self.global_thresh {
                Some(t) => t.to_string(),
                None => "NA".to_string(),
            },
            match self.local_thresh {
                Some(t) => t.to_string(),
                None => "NA".to_string(),
            },
            self.num_to_search,
            self.num_epochs,
            self.max_repeated_prior_scores,
            self.min_degree,
        )
    }
}
