/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
#![feature(test)]

extern crate lib_dachshund;
extern crate test;
use lib_dachshund::dachshund::adjacency_matrix::AdjacencyMatrix;
use lib_dachshund::dachshund::algebraic_connectivity::AlgebraicConnectivity;
use lib_dachshund::dachshund::betweenness::Betweenness;
use lib_dachshund::dachshund::brokerage::Brokerage;
use lib_dachshund::dachshund::clustering::Clustering;
use lib_dachshund::dachshund::cnm_communities::CNMCommunities;
use lib_dachshund::dachshund::connected_components::{
    ConnectedComponentsDirected, ConnectedComponentsUndirected,
};
use lib_dachshund::dachshund::connectivity::{ConnectivityDirected, ConnectivityUndirected};
use lib_dachshund::dachshund::coreness::Coreness;
use lib_dachshund::dachshund::eigenvector_centrality::EigenvectorCentrality;
use lib_dachshund::dachshund::error::CLQResult;
use lib_dachshund::dachshund::graph_base::GraphBase;
use lib_dachshund::dachshund::graph_builder_base::GraphBuilderBase;
use lib_dachshund::dachshund::id_types::NodeId;
use lib_dachshund::dachshund::laplacian::Laplacian;
use lib_dachshund::dachshund::node::DirectedNodeBase;
use lib_dachshund::dachshund::shortest_paths::ShortestPaths;
use lib_dachshund::dachshund::simple_directed_graph::{DirectedGraph, SimpleDirectedGraph};
use lib_dachshund::dachshund::simple_directed_graph_builder::SimpleDirectedGraphBuilder;
use lib_dachshund::dachshund::simple_undirected_graph::SimpleUndirectedGraph;
use lib_dachshund::dachshund::simple_undirected_graph_builder::{
    SimpleUndirectedGraphBuilder, SimpleUndirectedGraphBuilderWithCliques,
};
use lib_dachshund::dachshund::transitivity::Transitivity;
use std::collections::{BTreeSet, HashMap, HashSet};
use test::Bencher;

fn get_karate_club_edges() -> Vec<(usize, usize)> {
    vec![
        (1, 2),
        (1, 3),
        (2, 3),
        (1, 4),
        (2, 4),
        (3, 4),
        (1, 5),
        (1, 6),
        (1, 7),
        (5, 7),
        (6, 7),
        (1, 8),
        (2, 8),
        (3, 8),
        (4, 8),
        (1, 9),
        (3, 9),
        (3, 10),
        (1, 11),
        (5, 11),
        (6, 11),
        (1, 12),
        (1, 13),
        (4, 13),
        (1, 14),
        (2, 14),
        (3, 14),
        (4, 14),
        (6, 17),
        (7, 17),
        (1, 18),
        (2, 18),
        (1, 20),
        (2, 20),
        (1, 22),
        (2, 22),
        (24, 26),
        (25, 26),
        (3, 28),
        (24, 28),
        (25, 28),
        (3, 29),
        (24, 30),
        (27, 30),
        (2, 31),
        (9, 31),
        (1, 32),
        (25, 32),
        (26, 32),
        (29, 32),
        (3, 33),
        (9, 33),
        (15, 33),
        (16, 33),
        (19, 33),
        (21, 33),
        (23, 33),
        (24, 33),
        (30, 33),
        (31, 33),
        (32, 33),
        (9, 34),
        (10, 34),
        (14, 34),
        (15, 34),
        (16, 34),
        (19, 34),
        (20, 34),
        (21, 34),
        (23, 34),
        (24, 34),
        (27, 34),
        (28, 34),
        (29, 34),
        (30, 34),
        (31, 34),
        (32, 34),
        (33, 34),
    ]
}
fn _get_karate_club_graph_with_one_extra_edge<T, R>(builder: T) -> CLQResult<R>
where
    R: GraphBase,
    T: GraphBuilderBase<GraphType = R, RowType = (i64, i64)>,
{
    let mut rows = get_karate_club_edges();
    rows.push((35, 36));
    builder.from_vector(
        rows.into_iter()
            .map(|(x, y)| (x as i64, y as i64))
            .collect(),
    )
}
fn get_karate_club_graph_with_one_extra_edge() -> CLQResult<SimpleUndirectedGraph> {
    let builder = SimpleUndirectedGraphBuilder {};
    _get_karate_club_graph_with_one_extra_edge::<SimpleUndirectedGraphBuilder, _>(builder)
}
fn get_directed_karate_club_graph_with_one_extra_edge() -> CLQResult<SimpleDirectedGraph> {
    let builder = SimpleDirectedGraphBuilder {};
    _get_karate_club_graph_with_one_extra_edge::<SimpleDirectedGraphBuilder, _>(builder)
}

fn get_two_karate_clubs_edges() -> Vec<(usize, usize)> {
    let mut rows = get_karate_club_edges();
    for (i, j) in get_karate_club_edges() {
        rows.push((i + 35, j + 35));
    }
    rows
}

fn _get_two_karate_clubs<T, R>(builder: T) -> CLQResult<R>
where
    R: GraphBase,
    T: GraphBuilderBase<GraphType = R, RowType = (i64, i64)>,
{
    let rows = get_two_karate_clubs_edges();
    builder.from_vector(
        rows.into_iter()
            .map(|(x, y)| (x as i64, y as i64))
            .collect(),
    )
}
fn get_two_karate_clubs() -> CLQResult<SimpleUndirectedGraph> {
    let builder = SimpleUndirectedGraphBuilder {};
    _get_two_karate_clubs::<SimpleUndirectedGraphBuilder, _>(builder)
}
fn get_directed_karate_club_graph_both_ways() -> CLQResult<SimpleDirectedGraph> {
    let rows = get_karate_club_edges();
    let builder = SimpleDirectedGraphBuilder {};
    let graph = builder.from_vector(
        rows.iter()
            .cloned()
            .map(|(x, y)| (x as i64, y as i64))
            .chain(rows.iter().cloned().map(|(x, y)| (y as i64, x as i64)))
            .collect(),
    )?;
    for node in graph.get_nodes_iter() {
        assert_eq!(node.get_in_degree(), node.get_out_degree());
    }
    Ok(graph)
}
fn get_directed_karate_club_graph_with_core(
    core: HashSet<usize>,
) -> CLQResult<SimpleDirectedGraph> {
    let rows = get_karate_club_edges();
    let builder = SimpleDirectedGraphBuilder {};
    let graph = builder.from_vector(
        rows.iter()
            .cloned()
            .map(|(x, y)| (x as i64, y as i64))
            .chain(
                rows.iter()
                    .cloned()
                    .filter(|(x, y)| core.contains(x) && core.contains(y))
                    .map(|(x, y)| (y as i64, x as i64)),
            )
            .collect(),
    );
    graph
}

fn _get_two_karate_clubs_with_bridge<T, R>(builder: T) -> CLQResult<R>
where
    R: GraphBase,
    T: GraphBuilderBase<GraphType = R, RowType = (i64, i64)>,
{
    let mut rows = get_two_karate_clubs_edges();
    rows.push((34, 35));
    builder.from_vector(
        rows.into_iter()
            .map(|(x, y)| (x as i64, y as i64))
            .collect(),
    )
}
fn get_two_karate_clubs_with_bridge() -> CLQResult<SimpleUndirectedGraph> {
    let builder = SimpleUndirectedGraphBuilder {};
    _get_two_karate_clubs_with_bridge::<SimpleUndirectedGraphBuilder, _>(builder)
}

fn _get_karate_club_graph<T, R>(builder: T) -> CLQResult<R>
where
    R: GraphBase,
    T: GraphBuilderBase<GraphType = R, RowType = (i64, i64)>,
{
    let rows = get_karate_club_edges();
    builder.from_vector(
        rows.into_iter()
            .map(|(x, y)| (x as i64, y as i64))
            .collect(),
    )
}
fn get_karate_club_graph() -> CLQResult<SimpleUndirectedGraph> {
    let builder = SimpleUndirectedGraphBuilder {};
    _get_karate_club_graph::<SimpleUndirectedGraphBuilder, _>(builder)
}
fn get_directed_karate_club_graph() -> CLQResult<SimpleDirectedGraph> {
    let builder = SimpleDirectedGraphBuilder {};
    _get_karate_club_graph::<SimpleDirectedGraphBuilder, _>(builder)
}
fn get_karate_club_graph_with_cliques(
    cliques: Vec<BTreeSet<NodeId>>,
) -> CLQResult<SimpleUndirectedGraph> {
    let builder = SimpleUndirectedGraphBuilderWithCliques::new(cliques);
    _get_karate_club_graph::<SimpleUndirectedGraphBuilderWithCliques, _>(builder)
}

#[cfg(test)]
#[test]
fn test_karate_club() -> CLQResult<()> {
    let graph = get_karate_club_graph()?;
    assert_eq!(graph.nodes.len(), 34);
    assert_eq!(graph.count_edges(), 78);
    assert_eq!(graph.get_node_degree(NodeId::from(1 as i64)), 16);
    assert_eq!(graph.get_node_degree(NodeId::from(2 as i64)), 9);
    assert_eq!(graph.get_node_degree(NodeId::from(3 as i64)), 10);
    assert_eq!(graph.get_node_degree(NodeId::from(27 as i64)), 2);
    assert_eq!(graph.get_node_degree(NodeId::from(34 as i64)), 17);

    assert_eq!(
        graph
            .get_clustering_coefficient(NodeId::from(1 as i64))
            .unwrap(),
        0.15
    );
    assert!(
        (graph
            .get_clustering_coefficient(NodeId::from(34 as i64))
            .unwrap()
            - 0.1102941)
            <= 0.00001
    );
    assert_eq!(
        graph
            .get_clustering_coefficient(NodeId::from(22 as i64))
            .unwrap(),
        1.0
    );
    assert_eq!(
        graph.get_clustering_coefficient(NodeId::from(12 as i64)),
        None
    );
    assert_eq!(
        graph
            .get_clustering_coefficient(NodeId::from(10 as i64))
            .unwrap(),
        0.0
    );
    Ok(())
}

#[test]
fn test_shortest_paths() -> CLQResult<()> {
    let graph = get_karate_club_graph()?;
    let source = NodeId::from(1 as i64);
    let (dist, parents) = graph.get_shortest_paths(source, &None);
    assert_eq!(dist[&NodeId::from(1 as i64)], Some(0));
    assert_eq!(parents[&NodeId::from(1 as i64)].len(), 1);
    assert!(parents[&NodeId::from(1 as i64)].contains(&NodeId::from(1 as i64)));
    assert_eq!(dist[&NodeId::from(2 as i64)], Some(1));
    assert_eq!(dist[&NodeId::from(33 as i64)], Some(2));
    assert_eq!(dist[&NodeId::from(30 as i64)], Some(3));
    assert!(parents[&NodeId::from(2 as i64)].contains(&NodeId::from(1 as i64)));
    assert!(parents[&NodeId::from(10 as i64)].contains(&NodeId::from(3 as i64)));
    assert_eq!(parents[&NodeId::from(10 as i64)].len(), 1);
    assert!(parents[&NodeId::from(33 as i64)].contains(&NodeId::from(3 as i64)));
    assert!(parents[&NodeId::from(33 as i64)].contains(&NodeId::from(9 as i64)));
    assert!(parents[&NodeId::from(33 as i64)].contains(&NodeId::from(32 as i64)));
    assert_eq!(parents[&NodeId::from(33 as i64)].len(), 3);
    assert!(parents[&NodeId::from(30 as i64)].contains(&NodeId::from(33 as i64)));
    assert!(parents[&NodeId::from(30 as i64)].contains(&NodeId::from(34 as i64)));
    assert_eq!(parents[&NodeId::from(30 as i64)].len(), 2);

    let shortest_paths = graph.enumerate_shortest_paths(&dist, &parents, source);
    assert_eq!(shortest_paths.len(), 34);
    let mut unrolled_paths: HashSet<String> = HashSet::new();
    for paths in shortest_paths.values() {
        for path in paths {
            unrolled_paths.insert(
                path.iter()
                    .map(|x| format!("{}", x.value()))
                    .collect::<Vec<String>>()
                    .join("-"),
            );
        }
    }
    assert_eq!(unrolled_paths.len(), 89);
    assert_eq!(shortest_paths[&NodeId::from(2 as i64)].len(), 1);
    assert_eq!(shortest_paths[&NodeId::from(2 as i64)][0].len(), 2);
    assert_eq!(shortest_paths[&NodeId::from(30 as i64)][0].len(), 4);
    assert_eq!(shortest_paths[&NodeId::from(16 as i64)].len(), 7);
    assert!(unrolled_paths.contains("1-9-34-16"));
    assert!(unrolled_paths.contains("1-14-34-16"));
    assert!(unrolled_paths.contains("1-20-34-16"));
    assert!(unrolled_paths.contains("1-32-34-16"));
    assert!(unrolled_paths.contains("1-3-33-16"));
    assert!(unrolled_paths.contains("1-9-33-16"));
    assert!(unrolled_paths.contains("1-32-33-16"));
    Ok(())
}

#[bench]
fn bench_shortest_paths(b: &mut Bencher) -> CLQResult<()> {
    b.iter(|| {
        let graph = get_karate_club_graph().unwrap();
        let source = NodeId::from(1 as i64);
        let (_dist, _parents) = graph.get_shortest_paths(source, &None);
    });
    Ok(())
}

#[bench]
fn bench_shortest_paths_bfs(b: &mut Bencher) -> CLQResult<()> {
    b.iter(|| {
        let graph = get_karate_club_graph().unwrap();
        let source = NodeId::from(1 as i64);
        let (_ordered_students, _dist, _preds) = graph.get_shortest_paths_bfs(source);
    });
    Ok(())
}

#[test]
fn test_connectivity() -> CLQResult<()> {
    let graph = get_karate_club_graph()?;
    assert!(graph.get_is_connected().unwrap());
    let graph_unconnected = get_karate_club_graph_with_one_extra_edge()?;
    assert!(!graph_unconnected.get_is_connected()?);
    let graph_empty = SimpleUndirectedGraph::create_empty();
    assert!(graph_empty.get_is_connected().is_err(), "Graph is empty");
    let cc = graph.get_connected_components();
    assert_eq!(cc[0].len(), 34);
    assert_eq!(cc.len(), 1);
    assert!(graph.get_is_connected().unwrap());

    let cc_unconnected = graph_unconnected.get_connected_components();
    assert_eq!(cc_unconnected[0].len(), 34);
    assert_eq!(cc_unconnected[1].len(), 2);
    assert_eq!(cc_unconnected.len(), 2);
    assert!(!graph_unconnected.get_is_connected()?);
    assert_eq!(graph_empty.get_connected_components().len(), 0);
    assert!(graph_empty.get_is_connected().is_err(), "Graph is empty");
    Ok(())
}

#[test]
fn test_betweenness() -> CLQResult<()> {
    let graph = get_karate_club_graph()?;
    let bet = graph.get_node_betweenness()?;
    assert_eq!(bet[&NodeId::from(8 as i64)], 0.0);
    assert!((bet[&NodeId::from(34 as i64)] - 160.5515873).abs() <= 0.000001);
    assert!((bet[&NodeId::from(33 as i64)] - 76.6904762).abs() <= 0.000001);
    assert!((bet[&NodeId::from(32 as i64)] - 73.0095238).abs() <= 0.000001);
    Ok(())
}

#[test]
fn test_betweenness_brandes() -> CLQResult<()> {
    let graph = get_karate_club_graph()?;
    let bet = graph.get_node_betweenness_brandes().unwrap();
    assert_eq!(bet[&NodeId::from(8 as i64)], 0.0);
    assert!((bet[&NodeId::from(34 as i64)] - 160.5515873).abs() <= 0.000001);
    assert!((bet[&NodeId::from(33 as i64)] - 76.6904762).abs() <= 0.000001);
    assert!((bet[&NodeId::from(32 as i64)] - 73.0095238).abs() <= 0.000001);
    Ok(())
}

#[bench]
fn bench_betweenness(b: &mut Bencher) -> CLQResult<()> {
    b.iter(|| {
        let graph = get_karate_club_graph().unwrap();
        let _bet = graph.get_node_betweenness();
    });
    Ok(())
}

#[bench]
fn bench_betweenness_brandes(b: &mut Bencher) -> CLQResult<()> {
    b.iter(|| {
        let graph = get_karate_club_graph().unwrap();
        let _bet = graph.get_node_betweenness_brandes();
    });
    Ok(())
}

#[test]
fn test_matrices() -> CLQResult<()> {
    let graph = get_karate_club_graph()?;
    let (deg_mat, _ids) = graph.get_degree_matrix();
    assert_eq!(deg_mat.shape(), (34, 34));
    assert_eq!(deg_mat.row(0)[0], 16.0);
    assert_eq!(deg_mat.row(33)[33], 17.0);
    assert_eq!(deg_mat.row(2)[2], 10.0);
    assert_eq!(deg_mat.sum(), 156.0);
    let (adj_mat, _ids) = graph.get_adjacency_matrix();
    assert_eq!(adj_mat.shape(), (34, 34));
    assert_eq!(adj_mat.sum(), 156.0);
    assert_eq!(adj_mat.row(0).sum(), 16.0);
    assert_eq!(adj_mat.row(6)[16], 1.0);
    assert_eq!(adj_mat.row(6)[17], 0.0);
    let (laplacian, _ids) = graph.get_laplacian_matrix();
    assert_eq!(laplacian.shape(), (34, 34));
    assert_eq!(laplacian.sum(), 0.0);
    assert_eq!(laplacian + adj_mat, deg_mat);
    Ok(())
}

#[test]
fn test_eigen() -> CLQResult<()> {
    let graph = get_karate_club_graph()?;
    let fiedler = graph.get_algebraic_connectivity();
    assert!((fiedler - 0.469).abs() <= 0.001);

    let eps = 0.001;
    let ev = graph.get_eigenvector_centrality(eps, 1000);
    assert!((ev[&NodeId::from(34 as i64)] - 1.0).abs() <= eps);
    assert!((ev[&NodeId::from(1 as i64)] - 0.95213237).abs() <= eps);
    assert!((ev[&NodeId::from(19 as i64)] - 0.27159396).abs() <= eps);
    Ok(())
}

#[test]
fn test_k_cores() -> CLQResult<()> {
    let graph = get_karate_club_graph()?;
    let k_cores = graph.get_k_cores(1);
    assert_eq!(k_cores.len(), 1);
    assert_eq!(k_cores[0].len(), 34);
    let k_cores_4 = graph.get_k_cores(4);
    assert_eq!(k_cores_4.len(), 1);
    assert_eq!(k_cores_4[0].len(), 10);
    let k_cores_5 = graph.get_k_cores(5);
    assert_eq!(k_cores_5.len(), 0);

    let double_karate = get_two_karate_clubs_with_bridge()?;
    let k_cores_4_2 = double_karate.get_k_cores(4);
    assert_eq!(k_cores_4_2.len(), 2);
    assert_eq!(k_cores_4_2[0].len(), 10);
    assert_eq!(k_cores_4_2[1].len(), 10);

    let (core_assignments, coreness) = graph.get_coreness();
    assert_eq!(core_assignments[0][0].len(), 34);
    assert_eq!(core_assignments[1][0].len(), 33);
    assert_eq!(core_assignments[2][0].len(), 22);
    assert_eq!(core_assignments[3][0].len(), 10);

    assert_eq!(coreness[&NodeId::from(34 as i64)], 4);
    Ok(())
}

#[test]
fn test_connected_components() -> CLQResult<()> {
    let graph = get_karate_club_graph()?;
    let conn_comp = graph.get_connected_components();
    assert_eq!(conn_comp.len(), 1);
    assert_eq!(conn_comp[0].len(), 34);

    let double_karate = get_two_karate_clubs()?;
    let conn_comp_2 = double_karate.get_connected_components();
    assert_eq!(conn_comp_2.len(), 2);
    assert_eq!(conn_comp_2[0].len(), 34);
    assert_eq!(conn_comp_2[1].len(), 34);
    Ok(())
}

#[test]
fn test_transitivity() -> CLQResult<()> {
    let graph = get_karate_club_graph()?;
    let trans = graph.get_transitivity();
    println!("{}", trans);
    assert!((trans - 0.2556818181818182).abs() <= f64::EPSILON);

    let approx_trans = graph.get_approx_transitivity(1000);
    println!("{}", approx_trans);
    assert!((approx_trans - trans).abs() <= 0.05);
    Ok(())
}

#[test]
fn test_cnm_community() -> CLQResult<()> {
    let expected: Vec<f64> = vec![
        0.012163050624589085,
        0.023668639053254437,
        0.012491781722550954,
        0.019230769230769232,
        0.03131163708086785,
        0.012163050624589085,
        0.017258382642998026,
        0.016190006574621957,
        0.01643655489809336,
        0.012080867850098619,
        0.022682445759368834,
        0.011834319526627219,
        0.011341222879684417,
        0.011176857330703484,
        0.011176857330703484,
        0.01676528599605523,
        0.01101249178172255,
        0.010190664036817884,
        0.010190664036817882,
        0.01380670611439842,
        0.015779092702169626,
        0.0202991452991453,
        0.009861932938856014,
        0.011834319526627215,
        0.009368836291913214,
        0.009040105193951348,
        0.008711374095989481,
        0.011094674556213022,
        0.013477975016436557,
        0.01314924391847469,
        0.004684418145956606,
    ];

    let g = get_karate_club_graph()?;
    let (_, modularity_changes) = g.get_cnm_communities();
    for i in 0..expected.len() {
        println!(
            "Modularity changes: {}, {}, {}",
            i, modularity_changes[i], expected[i]
        );
        assert!((modularity_changes[i] - expected[i]).abs() <= 0.001);
    }
    Ok(())
}

#[test]
fn test_brokerage() -> CLQResult<()> {
    let expected_counts = vec![
        (0, 0, 0, 0, 0, 0),
        (0, 0, 0, 0, 0, 0),
        (0, 0, 1, 0, 0, 1),
        (3, 0, 6, 0, 0, 9),
        (2, 0, 0, 0, 0, 2),
        (0, 0, 0, 0, 0, 0),
        (1, 0, 0, 0, 0, 1),
        (2, 0, 0, 0, 0, 2),
        (0, 0, 0, 0, 0, 0),
        (0, 0, 5, 0, 0, 5),
        (0, 0, 1, 0, 0, 1),
        (0, 0, 0, 0, 0, 0),
        (0, 0, 0, 0, 0, 0),
        (0, 0, 0, 0, 0, 0),
        (0, 0, 4, 0, 0, 4),
        (0, 0, 0, 0, 0, 0),
        (0, 0, 0, 0, 0, 0),
        (0, 0, 0, 0, 0, 0),
        (0, 0, 0, 0, 0, 0),
        (0, 0, 0, 0, 0, 0),
        (0, 0, 0, 2, 0, 2),
        (0, 0, 0, 0, 0, 0),
        (0, 0, 0, 0, 0, 0),
        (0, 0, 0, 0, 0, 0),
        (0, 0, 0, 0, 0, 0),
        (0, 0, 0, 0, 0, 0),
        (1, 0, 0, 0, 0, 1),
        (0, 0, 0, 0, 0, 0),
        (1, 0, 0, 1, 0, 2),
        (0, 0, 0, 2, 0, 2),
        (1, 0, 0, 0, 0, 1),
        (0, 0, 0, 2, 0, 2),
        (5, 0, 0, 2, 0, 7),
        (0, 0, 0, 1, 0, 1),
        (0, 0, 0, 0, 0, 0),
    ];
    let g = get_directed_karate_club_graph()?;
    let mut c: HashMap<NodeId, usize> = HashMap::new();
    for node_id in g.get_ids_iter() {
        c.insert(*node_id, 1 + ((node_id.value() <= 17) as usize));
    }
    for node_id in g.get_ids_iter() {
        let scores = g.get_brokerage_scores_for_node(*node_id, &c);
        assert_eq!(
            scores.total_open_twopaths,
            expected_counts[node_id.value() as usize].5
        );
        assert_eq!(
            scores.num_coordinator_ties,
            expected_counts[node_id.value() as usize].0
        );
        assert_eq!(
            scores.num_itinerant_broker_ties,
            expected_counts[node_id.value() as usize].1
        );
        assert_eq!(
            scores.num_representative_ties,
            expected_counts[node_id.value() as usize].2
        );
        assert_eq!(
            scores.num_gatekeeper_ties,
            expected_counts[node_id.value() as usize].3
        );
        assert_eq!(
            scores.num_liaison_ties,
            expected_counts[node_id.value() as usize].4
        );
    }
    Ok(())
}
#[test]
fn test_weakly_connected_components() -> CLQResult<()> {
    let gd = get_directed_karate_club_graph()?;
    let cc = gd.get_weakly_connected_components();
    assert_eq!(cc[0].len(), 34);
    assert_eq!(cc.len(), 1);
    Ok(())
}
#[test]
fn test_connectivity_directed() -> CLQResult<()> {
    let graph = get_directed_karate_club_graph()?;
    assert!(graph.get_is_weakly_connected()?);
    let graph_unconnected = get_directed_karate_club_graph_with_one_extra_edge()?;
    assert!(!graph_unconnected.get_is_weakly_connected()?);

    let graph_empty = SimpleDirectedGraph::create_empty();
    assert!(
        graph_empty.get_is_weakly_connected().is_err(),
        "Graph is empty"
    );

    assert_eq!(
        graph.get_strongly_connected_components().len(),
        graph.count_nodes()
    );

    let graph_both_ways = get_directed_karate_club_graph_both_ways()?;
    assert_eq!(graph_both_ways.get_strongly_connected_components().len(), 1);

    let core = vec![1, 2, 3];
    let graph_with_core =
        get_directed_karate_club_graph_with_core(core.into_iter().collect::<HashSet<usize>>())?;
    let scc = graph_with_core.get_strongly_connected_components();
    assert_eq!(scc.len(), 32);
    assert_eq!(scc[0].len(), 3);
    assert!(scc[0]
        .iter()
        .collect::<HashSet<&NodeId>>()
        .contains(&NodeId::from(1)));
    assert!(scc[0]
        .iter()
        .collect::<HashSet<&NodeId>>()
        .contains(&NodeId::from(2)));
    assert!(scc[0]
        .iter()
        .collect::<HashSet<&NodeId>>()
        .contains(&NodeId::from(3)));
    Ok(())
}
#[test]
fn test_acyclic_directed() -> CLQResult<()> {
    let graph = get_directed_karate_club_graph()?;
    assert!(graph.is_acyclic());
    let graph_unconnected = get_directed_karate_club_graph_with_one_extra_edge()?;
    assert!(graph_unconnected.is_acyclic());

    let graph_empty = SimpleDirectedGraph::create_empty();
    assert!(graph_empty.is_acyclic());

    let graph_both_ways = get_directed_karate_club_graph_both_ways()?;
    assert!(!graph_both_ways.is_acyclic());

    let core = vec![1, 2, 3];
    let graph_with_core =
        get_directed_karate_club_graph_with_core(core.into_iter().collect::<HashSet<usize>>())?;
    assert!(!graph_with_core.is_acyclic());
    Ok(())
}
#[test]
fn test_clique_seeding() -> CLQResult<()> {
    let clique = vec![1, 2, 3, 4, 5];
    let cliques = vec![clique
        .into_iter()
        .map(|x| NodeId::from(x))
        .collect::<BTreeSet<_>>()];
    let g = get_karate_club_graph_with_cliques(cliques)?;
    // adding 3 edges, from 2 -> 5, 3, -> 5, 4 -> 5
    assert_eq!(g.count_edges(), 81);

    let clique1 = vec![1, 2, 3, 4, 5];
    let clique2 = vec![5, 6, 7];
    let cliques = vec![
        clique1
            .into_iter()
            .map(|x| NodeId::from(x))
            .collect::<BTreeSet<_>>(),
        clique2
            .into_iter()
            .map(|x| NodeId::from(x))
            .collect::<BTreeSet<_>>(),
    ];
    let g = get_karate_club_graph_with_cliques(cliques)?;
    // adding 5 edges, from 2 -> 5, 3, -> 5, 4 -> 5, 5 -> 6
    assert_eq!(g.count_edges(), 82);
    Ok(())
}
