//! The room graph. A node is a room's region, left by the stretches of one
//! border both rooms open into the next node; a search grows from the
//! target's room, so each node's way on is the same whichever room asks and a
//! body crossing into the next node walks that node's way on, never back.
use std::cmp::Reverse;
use std::collections::{BTreeSet, BinaryHeap};
use std::rc::Rc;

use super::grid::{UNREACHED, weight_of};
use super::tiles::{self, Table, Tile};
use super::{Atlas, Coords, Goal, Mover, RoomGrid, Target};
use crate::core::HashMap;

/// Each stretch's first and last tile on its band.
pub type Exit = Vec<(Tile, Tile)>;

/// A region settled on the way to the root, left by `exit` into `next`,
/// `value` ticks on from there.
struct Node {
    room: Coords,
    exit: Exit,
    value: i32,
    hops: i32,
    blind: i32,
    next: Option<usize>,
    dist: Option<Rc<Table<i32>>>,
}

struct Reached {
    room: Coords,
    exit: Exit,
    value: i32,
    hops: i32,
    blind: i32,
    next: usize,
}

pub struct Ways {
    nodes: Vec<Node>,
    /// Each room's regions, in the order settled.
    rooms: HashMap<Coords, Vec<usize>>,
    root: Option<Coords>,
}

/// Each room's regions' ways to the root, the first settled first.
#[derive(Clone, Debug, Default)]
pub struct Tree {
    pub root: Option<Coords>,
    next: HashMap<Coords, Vec<Way>>,
}

/// `onward` is the next room's way its landing walks on by.
#[derive(Clone, Debug)]
pub struct Way {
    pub next: Coords,
    pub onward: usize,
    pub exit: Exit,
}

impl Ways {
    pub fn tree(&self) -> Tree {
        let at = |n: usize| {
            let room = &self.rooms[&self.nodes[n].room];
            room.iter().position(|m| *m == n).unwrap_or(0)
        };
        let way = |n: &Node| {
            let next = n.next?;
            let (room, onward) = (self.nodes[next].room, at(next));
            Some(Way {
                next: room,
                onward,
                exit: n.exit.clone(),
            })
        };
        let next = (self.rooms.iter())
            .map(|(room, at)| {
                (
                    *room,
                    at.iter().filter_map(|n| way(&self.nodes[*n])).collect(),
                )
            })
            .collect();
        Tree {
            root: self.root,
            next,
        }
    }

    /// The flood out of the region of `room` that holds `at`, and the ticks
    /// on from its exit.
    pub fn out_of(
        &self,
        atlas: &Atlas,
        mover: Mover,
        room: &str,
        at: Tile,
    ) -> Option<(Rc<Table<i32>>, i32)> {
        let c = tiles::room_coords(room)?;
        let grid = atlas.rooms.get(&c)?;
        let held = |n: &&Node| n.dist.as_ref().is_some_and(|d| holds(mover, d, grid, at));
        let node = self
            .rooms
            .get(&c)?
            .iter()
            .map(|n| &self.nodes[*n])
            .find(held)?;
        Some((node.dist.clone()?, node.value))
    }
}

impl Tree {
    pub fn ways(&self, room: Coords) -> &[Way] {
        self.next.get(&room).map_or(&[], |w| w.as_slice())
    }

    pub fn contains(&self, room: &str) -> bool {
        tiles::room_coords(room).is_some_and(|c| Some(c) == self.root || !self.ways(c).is_empty())
    }

    /// The rooms from `from` onto `onto`, either of them the root: a tree
    /// grown from `from` is walked back from `onto`.
    pub fn route(&self, from: &str, onto: &str) -> Option<Vec<String>> {
        if from == onto {
            return Some(vec![from.to_string()]);
        }
        let chain = |at: Coords| -> Option<Vec<Coords>> {
            let mut chain = vec![at];
            let mut way = self.ways(at).first()?;
            while Some(way.next) != self.root {
                chain.push(way.next);
                way = self.ways(way.next).get(way.onward)?;
            }
            chain.push(way.next);
            Some(chain)
        };
        let (a, b) = (tiles::room_coords(from)?, tiles::room_coords(onto)?);
        let rooms = if Some(b) == self.root {
            chain(a)?
        } else if Some(a) == self.root {
            chain(b)?.into_iter().rev().collect()
        } else {
            return None;
        };
        Some(
            rooms
                .into_iter()
                .map(|(x, y)| tiles::room_name(x, y))
                .collect(),
        )
    }

    /// The borders `route` crosses, without naming its rooms.
    pub fn crossings(&self, from: &str, onto: &str) -> Option<i32> {
        if from == onto {
            return Some(0);
        }
        let (a, b) = (tiles::room_coords(from)?, tiles::room_coords(onto)?);
        let start = if Some(b) == self.root {
            a
        } else if Some(a) == self.root {
            b
        } else {
            return None;
        };
        let mut way = self.ways(start).first()?;
        let mut crossed = 1;
        while Some(way.next) != self.root {
            crossed += 1;
            way = self.ways(way.next).get(way.onward)?;
        }
        Some(crossed)
    }
}

/// A body on a tile no walk stands on (a post) walks off it; one in a
/// strike escapes it by any region's way.
fn holds(mover: Mover, dist: &Table<i32>, grid: &RoomGrid, at: Tile) -> bool {
    if grid.struck.as_ref().is_some_and(|s| s.contains(at)) {
        return true;
    }
    match weight_of(mover.footprint, &grid.weights, at) {
        Some(_) => dist[at] != UNREACHED,
        None => tiles::around(at).iter().any(|n| dist[*n] != UNREACHED),
    }
}

/// The stretches of `near`'s `side` both rooms open, as `near`'s tiles each
/// with its landing in `far`; a room with no grid is open.
fn stretches<'a>(
    near: Option<&'a RoomGrid>,
    far: Option<&'a RoomGrid>,
    side: i32,
) -> impl Iterator<Item = &'static [(Tile, Tile)]> + 'a {
    let open = |grid: Option<&RoomGrid>, t: Tile| grid.is_none_or(|g| g.weights[t].is_some());
    let shut = move |&(t, l): &(Tile, Tile)| !(open(near, t) && open(far, l));
    (tiles::exit_landings(side).split(shut)).filter(|run| !run.is_empty())
}

fn opposite(side: i32) -> i32 {
    (side + 3) % 8 + 1
}

fn neighbour(room: Coords, side: i32) -> Coords {
    (room.0 + tiles::dx(side), room.1 + tiles::dy(side))
}

/// A room a search stops at, and the tile whose region it stops for.
pub type Stop = (Coords, Option<Tile>);

/// Grown from `root` (its target's seats, else any tile of it) out to
/// `max_hops` crossings, until each of `stops` is settled or shown to have no
/// way out: fewest blind keeper rooms first, since a guard may stand where a
/// blind entry lands, then fewest ticks, equal ones in the order met.
pub fn search(
    atlas: &Atlas,
    mover: Mover,
    root: &str,
    target: Option<&Target>,
    avoid: &BTreeSet<String>,
    max_hops: i32,
    stops: &[Stop],
) -> Ways {
    let mut ways = Ways {
        nodes: Vec::new(),
        rooms: HashMap::default(),
        root: tiles::room_coords(root),
    };
    let Some(start) = ways.root.filter(|r| !atlas.closed.contains(r)) else {
        return ways;
    };
    let reach = |(x, y): Coords| (x - start.0).abs() + (y - start.1).abs() <= max_hops;
    let bounded = !stops.is_empty();
    let mut stops: Vec<Stop> = stops.iter().copied().filter(|(c, _)| reach(*c)).collect();
    if bounded && stops.is_empty() {
        return ways;
    }
    let ends: Vec<Coords> = stops.iter().map(|(c, _)| *c).collect();
    let avoid: Vec<Coords> = avoid.iter().filter_map(|r| tiles::room_coords(r)).collect();
    let grid_of = |c: Coords| atlas.rooms.get(&c);
    let blind = |c: Coords| match grid_of(c) {
        Some(grid) => grid.blind,
        None => atlas
            .lairs
            .get(&c)
            .copied()
            .unwrap_or(tiles::is_keeper_room(c)),
    };
    let seats =
        target.and_then(|t| Some(atlas.flood_in(grid_of(start)?, mover, Atlas::seats_of(t))));
    let on = |d: &Table<i32>, t: Tile| d[Atlas::anchor(mover.footprint, t)];
    ways.nodes.push(Node {
        room: start,
        exit: Vec::new(),
        value: 0,
        hops: 0,
        blind: 0,
        next: None,
        dist: seats,
    });
    ways.rooms.insert(start, vec![0]);

    let mut reached: Vec<Reached> = Vec::new();
    let mut queue: BinaryHeap<Reverse<(i32, i32, usize)>> = BinaryHeap::new();
    let mut settled = Some(0);
    loop {
        if let Some(i) = settled.take() {
            let node = &ways.nodes[i];
            let near = grid_of(node.room);
            for side in Atlas::EXITS {
                let there = neighbour(node.room, side);
                if there == start
                    || node.hops >= max_hops
                    || atlas.closed.contains(&there)
                    || (avoid.contains(&there) && !ends.contains(&there))
                {
                    continue;
                }
                let blind = node.blind + i32::from(blind(there));
                let entry = |&(_, l): &(Tile, Tile)| match (&node.dist, node.next) {
                    (Some(dist), _) => on(dist, l),
                    (None, None) => 0,
                    (None, Some(_)) => Atlas::crossing_ticks(),
                };
                let mut exit: Exit = Vec::new();
                let mut ticks = UNREACHED;
                for s in stretches(grid_of(there), near, opposite(side)) {
                    let d = s.iter().map(entry).min().unwrap_or(UNREACHED);
                    if d != UNREACHED {
                        exit.push((s[0].0, s[s.len() - 1].0));
                        ticks = ticks.min(d);
                    }
                }
                if exit.is_empty() {
                    continue;
                }
                queue.push(Reverse((blind, node.value + ticks, reached.len())));
                reached.push(Reached {
                    room: there,
                    exit,
                    value: node.value + ticks,
                    hops: node.hops + 1,
                    blind,
                    next: i,
                });
            }
        }
        let Some(Reverse((_, _, r))) = queue.pop() else {
            break;
        };
        let r = &mut reached[r];
        let exit_tiles = r.exit.iter().flat_map(|(a, b)| Atlas::stretch(*a, *b));
        let regions = ways.rooms.get(&r.room).map_or(&[][..], |v| v.as_slice());
        let held = regions.iter().any(|&n| {
            (ways.nodes[n].dist.as_ref())
                .is_none_or(|d| exit_tiles.clone().any(|t| on(d, *t) != UNREACHED))
        });
        if held {
            continue;
        }
        let dist = grid_of(r.room).map(|g| atlas.flood_in(g, mover, Goal::Exit(r.exit.clone())));
        ways.rooms.entry(r.room).or_default().push(ways.nodes.len());
        settled = Some(ways.nodes.len());
        ways.nodes.push(Node {
            room: r.room,
            exit: std::mem::take(&mut r.exit),
            value: r.value,
            hops: r.hops,
            blind: r.blind,
            next: Some(r.next),
            dist,
        });
        if bounded && ends.contains(&r.room) {
            stops.retain(|stop| !resolved(atlas, mover, &ways, *stop));
            if stops.is_empty() {
                break;
            }
        }
    }
    ways
}

/// A stop's room has a region holding its tile, or every stretch of its
/// borders lies in a region settled already: the tile's region has no way out.
fn resolved(atlas: &Atlas, mover: Mover, ways: &Ways, (room, at): Stop) -> bool {
    let dists: Vec<&Table<i32>> = (ways.rooms.get(&room).map_or(&[][..], |v| v.as_slice()))
        .iter()
        .filter_map(|n| ways.nodes[*n].dist.as_deref())
        .collect();
    let (Some(at), Some(grid)) = (at, atlas.rooms.get(&room)) else {
        return true;
    };
    if dists.iter().any(|d| holds(mover, d, grid, at)) {
        return true;
    }
    Atlas::EXITS.iter().all(|side| {
        let far = atlas.rooms.get(&neighbour(room, *side));
        stretches(Some(grid), far, *side).all(|s| {
            s.iter()
                .any(|(t, _)| dists.iter().any(|d| d[*t] != UNREACHED))
        })
    })
}
