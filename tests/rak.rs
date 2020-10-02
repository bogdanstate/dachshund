/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
extern crate lib_dachshund;
use lib_dachshund::dachshund::rak_communities::RAKCommunities;
use lib_dachshund::dachshund::graph_builder_base::GraphBuilderBase;
use lib_dachshund::dachshund::simple_undirected_graph::SimpleUndirectedGraph;
use lib_dachshund::dachshund::simple_undirected_graph_builder::SimpleUndirectedGraphBuilder;

fn get_graph(idx: usize) -> Result<SimpleUndirectedGraph, String> {
    let v = match idx {
        0 => vec![(0, 1), (1, 2), (2, 0)],
        1 => vec![(0, 1), (1, 2), (2, 0), (3, 4), (4, 5), (5, 3)],
        2 => vec![(0, 1), (1, 2), (2, 0), (0, 3)],
        3 => vec![
            (0, 1),
            (1, 2),
            (2, 0),
            (0, 3),
            (1, 4),
            (2, 5),
            (4, 5),
            (1, 6),
        ],
        _ => return Err("Invalid index".to_string()),
    };
    Ok(SimpleUndirectedGraphBuilder::from_vector(
        &v.into_iter().map(|(x, y)| (x as i64, y as i64)).collect(),
    ))
}

#[test]
fn test_triad_rak_whole() {
    let g = get_graph(0).unwrap();
    let communities = g.get_rak_communities();
    assert_eq!(communities.len(), 1);
    assert_eq!(
        communities
            .values()
            .map(|x| x.len())
            .collect::<Vec<usize>>()[0],
        3
    );
}

#[test]
fn test_two_triads_rak() {
    let g = get_graph(1).unwrap();
    let communities = g.get_rak_communities();
    assert_eq!(communities.len(), 2);
    assert_eq!(
        communities
            .values()
            .map(|x| x.len())
            .collect::<Vec<usize>>()[0],
        3
    );
}

#[test]
fn test_tendril_rak() {
    let g = get_graph(2).unwrap();
    let communities = g.get_rak_communities();
    assert_eq!(communities.len(), 2);
    assert_eq!(
        communities
            .values()
            .map(|x| x.len())
            .collect::<Vec<usize>>()[0],
        3
    );
}

