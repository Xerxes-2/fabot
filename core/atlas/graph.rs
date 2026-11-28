//! The room graph. A node is a room's region, left by the stretches of one
//! border both rooms open into the next node; a search grows from the
//! target's room, so each node's way on is the same whichever room asks and a
//! body crossing into the next node walks that node's way on, never back.
use std::{
    cmp::Reverse,
    collections::{BTreeSet, BinaryHeap},
    rc::Rc,
};

use fabot_base::{
    HashMap,
    grid::{UNREACHED, weight_of},
    tiles::{self, Table, Tile, TileSet},
    world::RoomName,
};

use super::{Atlas, Coords, Goal, Ground, Mover, RoomGrid, Target};

/// Each stretch's first and last tile on its band.
pub type Exit = Rc<[(Tile, Tile)]>;

/// Less the cheapest, so walks whose walks on differ by a constant share a flood.
pub type Seeds = Rc<[(Tile, i32)]>;

/// A flood out of a region, the ticks on from its exit, its seeds if seeded.
pub type Out<'a> = (Rc<Table<i32>>, i32, Option<Seeds>, &'a Exit);

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
        std::iter::successors(self.rooms.get(&room).copied(), |n| self.nodes[*n].next_in_room)
    }

    pub fn tree(&self) -> Tree {
        let at = |n: usize| {
            let room = self.nodes[n].room;
            self.regions(room).position(|m| m == n).unwrap_or(0)
        };
        let way = |n: &Node| {
            let next = n.next?;
            let (room, onward) = (self.nodes[next].room, at(next));
            Some(Way { next: room, onward, exit: n.exit.clone() })
        };
        let mut next = HashMap::default();
        next.reserve(self.rooms.len());
        for room in self.rooms.keys() {
            let ways = self.regions(*room).filter_map(|n| way(&self.nodes[n]));
            next.insert(*room, ways.collect());
        }
        Tree { root: self.root, next: Rc::new(next) }
    }

    /// `nearest`, but each room's exit from the root out seeded with the walk
    /// on from each of its tiles, not the nearest one.
    pub fn out_of(&self, atlas: &Atlas, mover: Mover, room: RoomName, at: Tile) -> Option<Out<'_>> {
        let n = self.region(atlas, mover, room, at)?;
        let mut seeds = Vec::new();
        let (dist, onward) = self.seeded(atlas, mover, n, &mut seeds)?;
        Some((dist?, onward, Some(seeds.as_slice().into()), &self.nodes[n].exit))
    }

    fn seeded(
        &self,
        atlas: &Atlas,
        mover: Mover,
        n: usize,
        seeds: &mut Vec<(Tile, i32)>,
    ) -> Option<(Option<Rc<Table<i32>>>, i32)> {
        let node = &self.nodes[n];
        let next = &self.nodes[node.next?];
        let (on, onward) = match next.next {
            None => (next.dist.clone(), 0),
            Some(_) => self.seeded(atlas, mover, node.next?, seeds)?,
        };
        let land = |l: Tile| match &on {
            Some(d) => match d[Atlas::anchor(mover.footprint, l)] {
                UNREACHED => UNREACHED,
                d => onward + d,
            },
            None if next.next.is_none() => 0,
            None => onward + Atlas::CROSSING_TICKS,
        };
        let band = node.exit.iter().flat_map(|(a, b)| Atlas::stretch(*a, *b));
        seeds.clear();
        seeds.extend(band.filter_map(|t| Some((*t, land(tiles::landing(*t)?)))));
        seeds.retain(|(_, d)| *d != UNREACHED);
        let least = seeds.iter().map(|(_, d)| *d).min()?;
        seeds.iter_mut().for_each(|(_, d)| *d -= least);
        let dist = (atlas.rooms.get(&node.room))
            .map(|g| atlas.flood_in(g, mover, Goal::Seeded(seeds), Ground::Kept));
        Some((dist, least))
    }

    pub fn nearest(
        &self,
        atlas: &Atlas,
        mover: Mover,
        room: RoomName,
        at: Tile,
    ) -> Option<Out<'_>> {
        let node = &self.nodes[self.region(atlas, mover, room, at)?];
        Some((node.dist.clone()?, node.value, None, &node.exit))
    }

    fn region(&self, atlas: &Atlas, mover: Mover, room: RoomName, at: Tile) -> Option<usize> {
        let c = room.coords();
        let grid = atlas.rooms.get(&c)?;
        self.regions(c)
            .find(|n| self.nodes[*n].dist.as_ref().is_some_and(|d| holds(mover, d, grid, at)))
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
        Some(rooms.into_iter().map(|(x, y)| RoomName::new(x, y)).collect())
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

/// Grown from `root` (its target's seats, else any tile) to rooms `max_hops`
/// crossings off at fewest (a cheaper way may cross more), until each stop is
/// settled or has no way out: fewest blind keeper rooms first, since a guard
/// may stand where a blind entry lands, then fewest ticks, ties as met.
pub fn search(
    atlas: &Atlas,
    mover: Mover,
    root: RoomName,
    target: Option<(&Target, Ground)>,
    avoid: &BTreeSet<RoomName>,
    max_hops: i32,
    stops: &[Stop],
) -> Ways {
    let mut ways = Ways { nodes: Vec::new(), rooms: HashMap::default(), root: Some(root.coords()) };
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
        target.and_then(|(t, ground)| Some(atlas.flood_seats(grid_of(start)?, mover, t, ground)));
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
    let mut settled = vec![0];
    loop {
        for i in std::mem::take(&mut settled) {
            let node = &ways.nodes[i];
            let near = grid_of(node.room);
            for side in Atlas::EXITS {
                let there = neighbour(node.room, side);
                if there == start
                    || node.hops >= max_hops
                    || atlas.closed.contains(&there)
                    || (avoid.contains(&there) || atlas.towered(there)) && !ends.contains(&there)
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
        let holder = |(a, b): (Tile, Tile)| {
            ways.regions(reached[r].room).find(|n| {
                (ways.nodes[*n].dist.as_ref())
                    .is_none_or(|d| Atlas::stretch(a, b).iter().any(|t| on(d, *t) != UNREACHED))
            })
        };
        let held: Vec<_> = (reached[r].exit.clone()).map(|e| (e, holder(exits[e]))).collect();
        let mut free = Vec::new();
        for (e, n) in held {
            match n {
                None => free.push(e),
                // Settled by a cheaper way of more crossings: fewer spread it on.
                Some(n) if ways.nodes[n].hops > reached[r].hops => {
                    ways.nodes[n].hops = reached[r].hops;
                    settled.push(n);
                }
                Some(_) => {}
            }
        }
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
        settled.push(ways.nodes.len());
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

/// A stop's room has a region settled (holding its tile if named), or its
/// borders all lie in settled regions: the tile's region has no way out.
fn resolved(atlas: &Atlas, mover: Mover, ways: &Ways, (room, at): Stop) -> bool {
    let dists: Vec<&Table<i32>> =
        (ways.regions(room)).filter_map(|n| ways.nodes[n].dist.as_deref()).collect();
    let (Some(at), Some(grid)) = (at, atlas.rooms.get(&room)) else {
        return ways.regions(room).next().is_some();
    };
    if dists.iter().any(|d| holds(mover, d, grid, at)) {
        return true;
    }
    Atlas::EXITS.iter().all(|side| {
        let far = atlas.rooms.get(&neighbour(room, *side));
        stretches(Some(grid), far, *side)
            .all(|s| s.iter().any(|(t, _)| dists.iter().any(|d| d[*t] != UNREACHED)))
    })
}
