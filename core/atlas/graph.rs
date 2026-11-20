//! The room graph. A node is a room's region, left by the stretches of one
//! border both rooms open into the next node; a search grows from the
//! target's room, so each node's way on is the same whichever room asks and a
//! body crossing into the next node walks that node's way on, never back.
use std::cmp::Reverse;
use std::collections::{BTreeSet, BinaryHeap};
use std::rc::Rc;

use super::{Atlas, Coords, Goal, Ground, Mover, RoomGrid, Target};
use fabot_base::HashMap;
use fabot_base::grid::{UNREACHED, weight_of};
use fabot_base::tiles::{self, Table, Tile, TileSet};
use fabot_base::world::RoomName;

/// Each stretch's first and last tile on its band.
pub type Exit = Rc<[(Tile, Tile)]>;

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
    next_in_room: Option<usize>,
}

struct Reached {
    room: Coords,
    /// Its stretches in the search's `exits`.
    exit: std::ops::Range<usize>,
    value: i32,
    hops: i32,
    blind: i32,
    next: usize,
}

pub struct Ways {
    nodes: Vec<Node>,
    /// Each room's first region settled.
    rooms: HashMap<Coords, usize>,
    root: Option<Coords>,
}

/// Each room's regions' ways to the root, the first settled first.
#[derive(Clone, Debug, Default, PartialEq)]
pub struct Tree {
    pub root: Option<Coords>,
    next: Rc<HashMap<Coords, Vec<Way>>>,
}

/// `onward` is the next room's way its landing walks on by.
#[derive(Clone, Debug, PartialEq)]
pub struct Way {
    pub next: Coords,
    pub onward: usize,
    pub exit: Exit,
}

impl Ways {
    /// The room's regions, in the order settled.
    fn regions(&self, room: Coords) -> impl Iterator<Item = usize> + '_ {
        std::iter::successors(self.rooms.get(&room).copied(), |n| {
            self.nodes[*n].next_in_room
        })
    }

    pub fn tree(&self) -> Tree {
        let at = |n: usize| {
            let room = self.nodes[n].room;
            self.regions(room).position(|m| m == n).unwrap_or(0)
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
        let mut next = HashMap::default();
        next.reserve(self.rooms.len());
        for room in self.rooms.keys() {
            let ways = self.regions(*room).filter_map(|n| way(&self.nodes[n]));
            next.insert(*room, ways.collect());
        }
        Tree {
            root: self.root,
            next: Rc::new(next),
        }
    }

    /// The flood out of the region of `room` that holds `at`, and the ticks
    /// on from its exit.
    pub fn out_of(
        &self,
        atlas: &Atlas,
        mover: Mover,
        room: RoomName,
        at: Tile,
    ) -> Option<(Rc<Table<i32>>, i32, &Exit)> {
        let c = room.coords();
        let grid = atlas.rooms.get(&c)?;
        let held = |n: &&Node| n.dist.as_ref().is_some_and(|d| holds(mover, d, grid, at));
        let node = self.regions(c).map(|n| &self.nodes[n]).find(held)?;
        Some((node.dist.clone()?, node.value, &node.exit))
    }
}

impl Ways {
    /// Out of `room` with `danger` and hostiles walled, the way on through a
    /// neighbour's region whose own way on never comes back, the fewest ticks
    /// first.
    pub fn round(
        &self,
        atlas: &Atlas,
        mover: Mover,
        room: RoomName,
        at: Tile,
        danger: Option<&TileSet>,
    ) -> Option<(Rc<Table<i32>>, i32)> {
        let c = room.coords();
        let grid = atlas.rooms.get(&c)?;
        let through = |mut n: usize| loop {
            let node = &self.nodes[n];
            match node.next {
                _ if node.room == c => return true,
                Some(next) => n = next,
                None => return false,
            }
        };
        let mut best: Option<(i32, Rc<Table<i32>>, i32)> = None;
        for side in Atlas::EXITS {
            let there = neighbour(c, side);
            let far = atlas.rooms.get(&there);
            for n in self.regions(there).filter(|n| !through(*n)) {
                let node = &self.nodes[n];
                let land = |l: Tile| match &node.dist {
                    Some(d) => d[Atlas::anchor(mover.footprint, l)],
                    None => Atlas::CROSSING_TICKS,
                };
                let mut exit = Vec::new();
                let mut landing = UNREACHED;
                for s in stretches(Some(grid), far, side) {
                    let d = s.iter().map(|(_, l)| land(*l)).min().unwrap_or(UNREACHED);
                    if d != UNREACHED {
                        exit.push((s[0].0, s[s.len() - 1].0));
                        landing = landing.min(d);
                    }
                }
                if exit.is_empty() {
                    continue;
                }
                let dist = atlas.flood_in(grid, mover, Goal::Exit(&exit), Ground::Live(danger));
                if dist[at] == UNREACHED {
                    continue;
                }
                let onward = node.value + landing;
                if best.as_ref().is_none_or(|b| dist[at] + onward < b.0) {
                    best = Some((dist[at] + onward, dist, onward));
                }
            }
        }
        best.map(|(_, dist, onward)| (dist, onward))
    }
}

impl Tree {
    pub fn ways(&self, room: Coords) -> &[Way] {
        self.next.get(&room).map_or(&[], |w| w.as_slice())
    }

    pub fn contains(&self, room: RoomName) -> bool {
        let c = room.coords();
        Some(c) == self.root || !self.ways(c).is_empty()
    }

    /// The rooms from `from` onto `onto`, either of them the root: a tree
    /// grown from `from` is walked back from `onto`.
    pub fn route(&self, from: RoomName, onto: RoomName) -> Option<Vec<RoomName>> {
        if from == onto {
            return Some(vec![from]);
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
        let (a, b) = (from.coords(), onto.coords());
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
                .map(|(x, y)| RoomName::new(x, y))
                .collect(),
        )
    }

    /// The borders `route` crosses, without naming its rooms.
    pub fn crossings(&self, from: RoomName, onto: RoomName) -> Option<i32> {
        if from == onto {
            return Some(0);
        }
        let (a, b) = (from.coords(), onto.coords());
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
/// way out: fewest blind keeper rooms and strongholds first, since a guard
/// may stand where a blind entry lands and a stronghold's towers cover its
/// room, then fewest ticks, equal ones in the order met.
pub fn search(
    atlas: &Atlas,
    mover: Mover,
    root: RoomName,
    target: Option<&Target>,
    avoid: &BTreeSet<RoomName>,
    max_hops: i32,
    stops: &[Stop],
) -> Ways {
    let mut ways = Ways {
        nodes: Vec::new(),
        rooms: HashMap::default(),
        root: Some(root.coords()),
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
    let avoid: Vec<Coords> = avoid.iter().map(|r| r.coords()).collect();
    let grid_of = |c: Coords| atlas.rooms.get(&c);
    let seats =
        target.and_then(|t| Some(atlas.flood_seats(grid_of(start)?, mover, t, Ground::Kept)));
    let on = |d: &Table<i32>, t: Tile| d[Atlas::anchor(mover.footprint, t)];
    ways.nodes.push(Node {
        room: start,
        exit: Rc::new([]),
        value: 0,
        hops: 0,
        blind: 0,
        next: None,
        dist: seats,
        next_in_room: None,
    });
    ways.rooms.insert(start, 0);

    let mut reached: Vec<Reached> = Vec::new();
    let mut exits: Vec<(Tile, Tile)> = Vec::new();
    let mut entries: Vec<i32> = Vec::new();
    let mut queue: BinaryHeap<Reverse<(i32, i32, usize)>> = BinaryHeap::default();
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
                let blind = node.blind + i32::from(atlas.crossed_last(there));
                let entry = |&(_, l): &(Tile, Tile)| match (&node.dist, node.next) {
                    (Some(dist), _) => on(dist, l),
                    (None, None) => 0,
                    (None, Some(_)) => Atlas::CROSSING_TICKS,
                };
                let first = exits.len();
                let mut ticks = UNREACHED;
                for s in stretches(grid_of(there), near, opposite(side)) {
                    let d = s.iter().map(entry).min().unwrap_or(UNREACHED);
                    if d != UNREACHED {
                        exits.push((s[0].0, s[s.len() - 1].0));
                        entries.push(d);
                        ticks = ticks.min(d);
                    }
                }
                if exits.len() == first {
                    continue;
                }
                queue.push(Reverse((blind, node.value + ticks, reached.len())));
                reached.push(Reached {
                    room: there,
                    exit: first..exits.len(),
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
        let held = |(a, b): (Tile, Tile)| {
            ways.regions(reached[r].room).any(|n| {
                (ways.nodes[n].dist.as_ref())
                    .is_none_or(|d| Atlas::stretch(a, b).iter().any(|t| on(d, *t) != UNREACHED))
            })
        };
        let free: Vec<usize> = reached[r]
            .exit
            .clone()
            .filter(|e| !held(exits[*e]))
            .collect();
        if free.is_empty() {
            continue;
        }
        // The free stretches can cost more than the popped node, so settling now breaks cheapest-first.
        if free.len() < reached[r].exit.len() {
            let first = exits.len();
            for e in free {
                exits.push(exits[e]);
                entries.push(entries[e]);
            }
            let ticks = entries[first..].iter().min().copied().unwrap_or(UNREACHED);
            let r = &reached[r];
            let value = ways.nodes[r.next].value + ticks;
            queue.push(Reverse((r.blind, value, reached.len())));
            reached.push(Reached {
                room: r.room,
                exit: first..exits.len(),
                value,
                hops: r.hops,
                blind: r.blind,
                next: r.next,
            });
            continue;
        }
        let r = &reached[r];
        let exit = &exits[r.exit.clone()];
        let dist =
            grid_of(r.room).map(|g| atlas.flood_in(g, mover, Goal::Exit(exit), Ground::Kept));
        match ways.regions(r.room).last() {
            Some(last) => ways.nodes[last].next_in_room = Some(ways.nodes.len()),
            None => {
                ways.rooms.insert(r.room, ways.nodes.len());
            }
        }
        settled = Some(ways.nodes.len());
        ways.nodes.push(Node {
            room: r.room,
            exit: Rc::from(exit),
            value: r.value,
            hops: r.hops,
            blind: r.blind,
            next: Some(r.next),
            dist,
            next_in_room: None,
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
    let dists: Vec<&Table<i32>> = (ways.regions(room))
        .filter_map(|n| ways.nodes[n].dist.as_deref())
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
