//! The fewest tiles that part the core from the exits: a minimum vertex cut,
//! each tile an in and an out joined by one unit of flow. The graph is
//! Hivemind's planner's idea (MIT, David Franke, after Saruss), written anew.
use crate::grid::Weight;
use crate::tiles::{self, Tile};

const NO: u32 = u32::MAX;
const FROM_SOURCE: u32 = u32::MAX - 1;

fn node(t: Tile, out: bool) -> usize {
    2 * t.index() + usize::from(out)
}

/// `core` the tiles a raid comes for, each with the reach it is kept clear
/// to: every tile in it is inside or on the cut, as Hivemind's protected
/// rectangles keep the tiles behind a wall too. The engine allows no rampart
/// beside an exit, so the cut is never there. A `solid` tile, a planned
/// structure, is never cut while a tile off it can be.
pub fn ring(weights: &[Weight; tiles::COUNT], core: &[(Tile, i32)], solid: &[Tile]) -> Vec<Tile> {
    let open = |t: Tile| weights[t].is_some();
    let exit = |t: Tile| tiles::on_edge(t) && open(t);
    let sink: Vec<bool> = (tiles::all())
        .map(|t| open(t) && tiles::within(1, t).into_iter().any(exit))
        .collect();
    let mut uncut = vec![false; tiles::COUNT];
    for t in solid {
        uncut[*t] = !tiles::within(1, *t).into_iter().any(|n| sink[n]);
    }
    let mut kept = vec![false; tiles::COUNT];
    for (c, reach) in core {
        for t in tiles::within(*reach, *c) {
            kept[t] = open(t) && !sink[t];
        }
    }

    let mut edge_flow = vec![0i32; tiles::COUNT * 8];
    let mut vertex_used = vec![false; tiles::COUNT];
    let slot = |t: Tile, n: Tile| t.index() * 8 + (tiles::direction(t, n) - 1) as usize;

    let search = |edge_flow: &[i32], vertex_used: &[bool]| -> (Vec<u32>, Option<usize>) {
        let mut parent = vec![NO; 2 * tiles::COUNT];
        let mut queue: Vec<usize> = Vec::new();
        for t in tiles::all().filter(|t| kept[*t]) {
            parent[node(t, false)] = FROM_SOURCE;
            queue.push(node(t, false));
        }
        let mut i = 0;
        while i < queue.len() {
            let at = queue[i];
            i += 1;
            let t = Tile::of_index(at / 2);
            let mut visit = |next: usize, queue: &mut Vec<usize>| {
                if parent[next] == NO {
                    parent[next] = at as u32;
                    queue.push(next);
                }
            };
            if at % 2 == 1 {
                for &n in tiles::around(t).iter().filter(|n| open(**n)) {
                    visit(node(n, false), &mut queue);
                }
                if vertex_used[t] {
                    visit(node(t, false), &mut queue);
                }
            } else {
                if sink[t] {
                    return (parent, Some(at));
                }
                if !vertex_used[t] || uncut[t] {
                    visit(node(t, true), &mut queue);
                }
                for &m in tiles::around(t) {
                    if open(m) && edge_flow[slot(m, t)] > 0 {
                        visit(node(m, true), &mut queue);
                    }
                }
            }
        }
        (parent, None)
    };

    let reached = loop {
        let (parent, found) = search(&edge_flow, &vertex_used);
        let Some(mut at) = found else {
            break parent;
        };
        while parent[at] != FROM_SOURCE {
            let from = parent[at] as usize;
            let (a, b) = (Tile::of_index(from / 2), Tile::of_index(at / 2));
            match (from % 2 == 1, a == b) {
                (false, true) => vertex_used[a] = !uncut[a],
                (true, true) => vertex_used[a] = false,
                (true, false) => edge_flow[slot(a, b)] += 1,
                (false, false) => edge_flow[slot(b, a)] -= 1,
            }
            at = from;
        }
    };

    (tiles::all())
        .filter(|t| reached[node(*t, false)] != NO && reached[node(*t, true)] == NO)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::engine;
    use crate::layout::fixtures as fx;

    #[test]
    fn no_tile_in_ranged_reach_of_the_core_lies_outside_the_cut_nor_any_cut_beside_an_exit() {
        for colony in fx::samples(13).take(fx::MAX_TEST) {
            let Some((ground, plan)) = fx::plan_of(&colony) else {
                continue;
            };
            let core: Vec<Tile> = (plan.items.iter())
                .filter(|i| i.kind.is_spawn() || i.kind.is_storage())
                .map(|i| i.tile)
                .collect();
            let reached: Vec<(Tile, i32)> =
                core.iter().map(|c| (*c, engine::RANGED_RANGE)).collect();
            let cut = ring(&ground.weights, &reached, &[]);
            let open = |t: Tile| ground.weights[t].is_some();
            let off_exits = |t: Tile| {
                tiles::within(1, t)
                    .into_iter()
                    .all(|n| !tiles::on_edge(n) || !open(n))
            };
            let mut inside = vec![false; tiles::COUNT];
            // From behind a wall too: a pocket in reach is a source of its own.
            let mut queue: Vec<Tile> = (core.iter())
                .flat_map(|c| tiles::within(engine::RANGED_RANGE, *c))
                .filter(|t| open(*t) && off_exits(*t) && !cut.contains(t))
                .collect();
            for t in &queue {
                inside[*t] = true;
            }
            while let Some(t) = queue.pop() {
                for &n in tiles::around(t) {
                    if open(n) && !inside[n] && !cut.contains(&n) {
                        inside[n] = true;
                        queue.push(n);
                    }
                }
            }

            for t in &cut {
                assert!(off_exits(*t));
            }
            for t in tiles::all().filter(|t| inside[*t]) {
                assert!(off_exits(t), "{t} inside beside an exit");
            }
            let clear_of_exits = |c: &Tile| {
                tiles::within(engine::RANGED_RANGE + 1, *c)
                    .into_iter()
                    .all(|n| !tiles::on_edge(n))
            };
            for c in core.iter().filter(|c| clear_of_exits(c)) {
                for n in tiles::within(engine::RANGED_RANGE, *c)
                    .into_iter()
                    .filter(|n| open(*n))
                {
                    assert!(inside[n] || cut.contains(&n), "{n} in reach of {c}");
                }
            }
        }
    }
}
