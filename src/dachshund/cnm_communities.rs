/*
 * Copyright (c) Facebook, Inc. and its affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */
extern crate nalgebra as na;
use crate::dachshund::graph_base::GraphBase;
use crate::dachshund::id_types::NodeId;
use crate::dachshund::node::{SimpleNode, NodeBase, NodeEdgeBase};
use ordered_float::OrderedFloat;
use std::cmp::Ordering;
use std::collections::{BinaryHeap, HashMap, HashSet};
type Community = HashSet<NodeId>;

#[derive(Clone, Copy, Eq)]
pub struct CNMCommunityMergeInstruction {
    delta_ij: OrderedFloat<f64>,
    i: usize,
    j: usize,
}
impl CNMCommunityMergeInstruction {
    pub fn new(delta_ij: OrderedFloat<f64>, i: usize, j: usize) -> Self {
        Self { delta_ij, i, j }
    }
    pub fn tuple(self) -> (OrderedFloat<f64>, usize, usize) {
        (self.delta_ij, self.i, self.j)
    }
}
impl Ord for CNMCommunityMergeInstruction {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.delta_ij < other.delta_ij {
            Ordering::Less
        } else if self.delta_ij > other.delta_ij {
            Ordering::Greater
        } else if self.i > other.i {
            Ordering::Less
        } else if self.i < other.i {
            Ordering::Greater
        } else if self.j > other.j {
            Ordering::Less
        } else if self.j < other.j {
            Ordering::Greater
        } else {
            Ordering::Equal
        }
    }
}
impl PartialEq for CNMCommunityMergeInstruction {
    fn eq(&self, other: &Self) -> bool {
        self.delta_ij == other.delta_ij && self.i == other.i && self.j == other.j
    }
}
impl PartialOrd for CNMCommunityMergeInstruction {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}
type CNMCommunityMergeInstructionHeap = BinaryHeap<CNMCommunityMergeInstruction>;

pub trait CNMCommunities: GraphBase<NodeType = SimpleNode> {
    fn get_max_maxheap(
        &self,
        delta_q_maxheap: &HashMap<usize, CNMCommunityMergeInstructionHeap>,
    ) -> CNMCommunityMergeInstructionHeap {
        let mut maxh: CNMCommunityMergeInstructionHeap = BinaryHeap::new();
        for (_k, heap) in delta_q_maxheap.iter() {
            let maybe_top_elem = heap.peek();
            if maybe_top_elem.is_some() {
                let top_elem = maybe_top_elem.unwrap();
                maxh.push(top_elem.clone());
            }
        }
        maxh
    }
    fn init_cnm_communities(
        &self,
    ) -> (
        HashMap<usize, Community>,
        HashMap<usize, usize>,
        HashMap<usize, HashMap<usize, f64>>,
        HashMap<usize, CNMCommunityMergeInstructionHeap>,
        CNMCommunityMergeInstructionHeap,
        usize,
    ) {
        // stores current communities
        let mut communities: HashMap<usize, Community> = HashMap::new();
        let mut degree_map: HashMap<usize, usize> = HashMap::new();
        // binary map -- for finding delta_q_ik
        let mut delta_q_bmap: HashMap<usize, HashMap<usize, f64>> = HashMap::new();
        // max heaps -- for argmax_j delta_q_ij
        // using the fact that tupled are compared in lexicographic order
        // first element holds delta_q, 2nd holds index
        let mut delta_q_maxheap: HashMap<usize, CNMCommunityMergeInstructionHeap> = HashMap::new();
        let mut reverse_id_map: HashMap<NodeId, usize> = HashMap::new();

        let mut num_edges: usize = 0;
        let mut sorted_ids: Vec<NodeId> = Vec::with_capacity(self.count_nodes());
        for id in self.get_ids_iter() {
            sorted_ids.push(*id);
        }
        sorted_ids.sort();
        for (i, id) in sorted_ids.into_iter().enumerate() {
            let mut community: Community = HashSet::new();
            community.insert(id);
            communities.insert(i, community);

            let d = self.get_node(id).degree();

            degree_map.insert(i, d);
            reverse_id_map.insert(id, i);
            num_edges += d;
            delta_q_maxheap.insert(i, BinaryHeap::new());
            delta_q_bmap.insert(i, HashMap::new());
        }
        num_edges /= 2;
        let q0: f64 = 1.0 / (num_edges as f64);
        for (_i, community) in communities.iter() {
            for id in community {
                for e in self.get_node(*id).get_edges() {
                    let neighbor_id = e.get_neighbor_id();
                    let i: &usize = reverse_id_map.get(&id).unwrap();
                    let j: &usize = reverse_id_map.get(&neighbor_id).unwrap();
                    let k_i: usize = degree_map[i];
                    let k_j: usize = degree_map[j];
                    let delta_qij: f64 =
                        q0 - 2. * ((k_i * k_j) as f64) / (((2 * num_edges).pow(2)) as f64);
                    delta_q_bmap.get_mut(i).unwrap().insert(*j, delta_qij);
                    delta_q_maxheap
                        .get_mut(i)
                        .unwrap()
                        .push(CNMCommunityMergeInstruction::new(
                            OrderedFloat(delta_qij),
                            *i,
                            *j,
                        ));
                }
            }
        }
        let maxh = self.get_max_maxheap(&delta_q_maxheap);

        (
            communities,
            degree_map,
            delta_q_bmap,
            delta_q_maxheap,
            maxh,
            num_edges,
        )
    }
    fn iterate_cnm_communities(
        &self,
        mut communities: HashMap<usize, Community>,
        mut degree_map: HashMap<usize, usize>,
        mut delta_q_bmap: HashMap<usize, HashMap<usize, f64>>,
        mut delta_q_maxheap: HashMap<usize, CNMCommunityMergeInstructionHeap>,
        mut maxh: CNMCommunityMergeInstructionHeap,
        num_edges: usize,
    ) -> (
        HashMap<usize, Community>,
        HashMap<usize, usize>,
        HashMap<usize, HashMap<usize, f64>>,
        HashMap<usize, CNMCommunityMergeInstructionHeap>,
        CNMCommunityMergeInstructionHeap,
        usize,
    ) {
        // find largest delta_q_ij
        let (_largest_delta_q_ij, i, j) = maxh.pop().unwrap().tuple();

        // we will create community j from communities i and j
        let com_i: Community = communities.remove(&i).unwrap();
        let com_j: &mut Community = communities.get_mut(&j).unwrap();
        com_j.extend(com_i);

        // get communities to which com_i, com_j are connected
        let neighbors_i: HashMap<usize, f64> = delta_q_bmap.remove(&i).unwrap();
        let neighbors_j: HashMap<usize, f64> = delta_q_bmap.remove(&j).unwrap();
        let mut all_neighbors: HashSet<usize> = neighbors_i.keys().copied().collect();

        all_neighbors.extend(neighbors_j.keys().copied());
        all_neighbors.remove(&i);
        all_neighbors.remove(&j);

        let mut new_delta_qjk_map: HashMap<usize, f64> = HashMap::new();
        let mut new_community_maxheap: CNMCommunityMergeInstructionHeap = BinaryHeap::new();
        for k in all_neighbors {
            let delta_qik: Option<&f64> = neighbors_i.get(&k);
            let delta_qjk: Option<&f64> = neighbors_j.get(&k);

            /* Get new delta_qjk */
            let new_delta_qjk = match delta_qik {
                Some(x) => match delta_qjk {
                    Some(y) => x + y,
                    None => {
                        x - (degree_map[&j] as f64 / num_edges as f64)
                            * (degree_map[&k] as f64 / (2 * num_edges) as f64)
                    }
                },
                None => {
                    delta_qjk.unwrap()
                        - (degree_map[&i] as f64 / num_edges as f64)
                            * (degree_map[&k] as f64 / (2 * num_edges) as f64)
                }
            };
            new_delta_qjk_map.insert(k, new_delta_qjk);

            /* Update the binary maps for k */
            let neighbors_k: &mut HashMap<usize, f64> = delta_q_bmap.get_mut(&k).unwrap();
            if delta_qik.is_some() {
                neighbors_k.remove(&i);
            }
            neighbors_k.insert(j, new_delta_qjk);

            /* Update the binary heap for k */
            let old_maxheap: CNMCommunityMergeInstructionHeap = delta_q_maxheap.remove(&k).unwrap();
            let mut new_maxheap: CNMCommunityMergeInstructionHeap =
                BinaryHeap::with_capacity(old_maxheap.len());
            for el in old_maxheap.into_iter_sorted() {
                let ll = el.j;

                if ll != i {
                    if ll == j {
                        new_maxheap.push(CNMCommunityMergeInstruction::new(
                            OrderedFloat(new_delta_qjk),
                            k,
                            ll,
                        ));
                    } else {
                        new_maxheap.push(el);
                    }
                }
            }
            delta_q_maxheap.insert(k, new_maxheap);
            new_community_maxheap.push(CNMCommunityMergeInstruction::new(
                OrderedFloat(new_delta_qjk),
                j,
                k,
            ));
        }
        // adding the new_delta_qjk map for the newly created community
        delta_q_bmap.insert(j, new_delta_qjk_map);
        delta_q_bmap.remove(&i);
        // updating the delta_q_maxheap
        delta_q_maxheap.insert(j, new_community_maxheap);
        delta_q_maxheap.remove(&i);

        // updating the degree map
        let new_degree = degree_map[&i] + degree_map[&j];
        degree_map.insert(j, new_degree);
        degree_map.remove(&i);

        maxh = self.get_max_maxheap(&delta_q_maxheap);
        (
            communities,
            degree_map,
            delta_q_bmap,
            delta_q_maxheap,
            maxh,
            num_edges,
        )
    }
    fn get_cnm_communities(&self) -> (HashMap<usize, Community>, Vec<f64>) {
        let (
            mut communities,
            mut degree_map,
            mut delta_q_bmap,
            mut delta_q_maxheap,
            mut maxh,
            num_edges,
        ) = self.init_cnm_communities();

        let mut modularity_change = maxh.peek().unwrap().delta_ij.into_inner();
        let mut modularity_changes: Vec<f64> = vec![modularity_change];

        while maxh.len() > 0 && modularity_change > 0. {
            let res = self.iterate_cnm_communities(
                communities,
                degree_map,
                delta_q_bmap,
                delta_q_maxheap,
                maxh,
                num_edges,
            );
            communities = res.0;
            degree_map = res.1;
            delta_q_bmap = res.2;
            delta_q_maxheap = res.3;
            maxh = res.4;
            if maxh.peek().is_some() {
                modularity_change = maxh.peek().unwrap().delta_ij.into_inner();
                modularity_changes.push(modularity_change);
            }
        }
        (communities, modularity_changes)
    }
}
