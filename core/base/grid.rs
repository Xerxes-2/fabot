//! Knows nothing of the Atlas: it prices raw weight arrays, so the Atlas
//! keeps its own representation private.
use std::num::NonZeroU8;

use super::tiles::{self, Tile};
use crate::{
    engine,
    vocabulary::{Boost, Part, ResourceType, multiple},
    world::Creep,
};

pub const UNREACHED: i32 = i32::MAX;

/// A wall's distance inside a flood: no walk is shorter, so the test that
/// keeps a longer walk off a tile keeps every walk off a wall. A wall check
/// apart cost the economy 2% (bench-live).
const WALL: i32 = -1;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub enum Footprint {
    One,
    Quad,
}

/// MOVE pays off two fatigue a tick, at its boost's multiple.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct FatigueFactor {
    pub weight: i32,
    pub moves: i32,
}

/// The engine's rule: a destroyed part still weighs; only a live one carries or moves.
pub fn factor(
    parts: impl Iterator<Item = (Part, Option<ResourceType>, bool)>,
    mut load: i32,
) -> FatigueFactor {
    let capacity = |boost: Option<ResourceType>| match boost.and_then(ResourceType::boost) {
        Some(Boost::Carry(n)) => n as i32 * engine::CARRY_PART_CAPACITY,
        _ => engine::CARRY_PART_CAPACITY,
    };
    let (mut weight, mut moves) = (0, 0);
    for (kind, boost, live) in parts {
        match kind {
            Part::Move if live => moves += multiple(Part::Move, boost),
            Part::Carry if live => {
                weight += i32::from(load > 0);
                load -= capacity(boost);
            }
            Part::Move | Part::Carry => {}
            _ => weight += 1,
        }
    }
    FatigueFactor { weight, moves }
}

pub fn factor_of(creep: &Creep) -> FatigueFactor {
    factor(creep.body.iter().map(|p| (p.kind, p.boost, p.hits > 0)), creep.store.used())
}

pub fn empty_factor(body: &[Part]) -> FatigueFactor {
    factor(body.iter().map(|p| (*p, None, true)), 0)
}

#[inline]
pub fn loaded_factor(body: &[Part]) -> FatigueFactor {
    factor(body.iter().map(|p| (*p, None, true)), i32::MAX)
}

pub fn rest(factor: FatigueFactor, fatigue: i32) -> Option<i32> {
    (factor.moves > 0).then(|| (fatigue + 2 * factor.moves - 1) / (2 * factor.moves))
}

/// In half-ticks: at least one, since no step is free. None without MOVE.
fn step_units(factor: FatigueFactor, weight: i32) -> Option<i32> {
    (factor.moves > 0).then(|| 1.max((weight * factor.weight + factor.moves - 1) / factor.moves))
}

/// The detour a walk takes before it waits on a seated body; finite, so the
/// seat's own walk and the only way through still reach it.
const SEAT_SWAMPS: i32 = 5;
pub const SEAT_WEIGHT: i32 = SEAT_SWAMPS * engine::SWAMP_WEIGHT;

/// Exact per step: a body cannot step again until its fatigue is zero, so a
/// path's ticks are the sum of its steps'. A seat's price follows from swamp's,
/// so the pace class keys a flood.
#[inline]
pub fn step_ticks(factor: FatigueFactor, weight: i32) -> Option<i32> {
    let seat = weight == SEAT_WEIGHT;
    let ticks = (step_units(factor, if seat { engine::SWAMP_WEIGHT } else { weight })? + 1) / 2;
    Some(if seat { SEAT_SWAMPS * ticks } else { ticks })
}

/// The flood reads a table, not a closure.
pub fn step_table(factor: FatigueFactor) -> Vec<Option<u16>> {
    (0..=SEAT_WEIGHT).map(|w| step_ticks(factor, w).map(|t| t as u16)).collect()
}

/// A tile's step weight; None where nothing may stand.
pub type Weight = Option<NonZeroU8>;

pub fn weight(w: i32) -> Weight {
    NonZeroU8::new(w as u8)
}

pub fn value(w: Weight) -> Option<i32> {
    w.map(|w| i32::from(w.get()))
}

pub fn weight_of(footprint: Footprint, weights: &[Weight; tiles::COUNT], tile: Tile) -> Weight {
    match footprint {
        Footprint::One => weights[tile],
        Footprint::Quad => {
            let (x, y) = (tiles::x_of(tile), tiles::y_of(tile));
            if x == tiles::SIDE - 1 || y == tiles::SIDE - 1 {
                None
            } else {
                let i = tile.index();
                let side = tiles::SIDE as usize;
                let (a, b, c, d) =
                    (weights[i]?, weights[i + 1]?, weights[i + side]?, weights[i + side + 1]?);
                Some(a.max(b).max(c.max(d)))
            }
        }
    }
}

/// A step onto the border ring carries the body into the next room.
pub fn on_edge(footprint: Footprint, tile: Tile) -> bool {
    match footprint {
        Footprint::One => tiles::on_edge(tile),
        Footprint::Quad => {
            let (x, y) = (tiles::x_of(tile), tiles::y_of(tile));
            x == 0 || y == 0 || x >= tiles::SIDE - 2 || y >= tiles::SIDE - 2
        }
    }
}

const SIDE: isize = tiles::SIDE as isize;
const OFFSETS: [isize; 8] = [-SIDE, 1, SIDE, -1, -SIDE - 1, -SIDE + 1, SIDE - 1, SIDE + 1];

/// Dial's queue: no step costs more than the table's dearest, so a ring of one
/// bucket more holds every queued distance apart.
struct Dial {
    ring: usize,
    head: Vec<Option<Tile>>,
    next: [Option<Tile>; tiles::COUNT],
    prev: [Option<Tile>; tiles::COUNT],
    queued: i32,
    relaxed: i32,
}

impl Dial {
    fn new(ring: usize) -> Dial {
        Dial {
            ring,
            head: vec![None; ring],
            next: [None; tiles::COUNT],
            prev: [None; tiles::COUNT],
            queued: 0,
            relaxed: 0,
        }
    }

    fn link(&mut self, tile: Tile, d: i32) {
        let b = (d % self.ring as i32) as usize;
        let h = self.head[b];
        self.next[tile] = h;
        self.prev[tile] = None;
        if let Some(h) = h {
            self.prev[h] = Some(tile);
        }
        self.head[b] = Some(tile);
        self.queued += 1;
    }

    fn unlink(&mut self, tile: Tile, dist: &[i32]) {
        let (p, n) = (self.prev[tile].take(), self.next[tile].take());
        let b = (dist[tile] % self.ring as i32) as usize;
        match p {
            Some(p) => self.next[p] = n,
            None if self.head[b] == Some(tile) => self.head[b] = n,
            None => return,
        }
        if let Some(n) = n {
            self.prev[n] = p;
        }
        self.queued -= 1;
    }
}

/// From each seed at its own distance over `dist`: unreached, or flooded on weights none cheaper than these; `seeds` rise.
pub fn flood(
    footprint: Footprint,
    weights: &[Weight; tiles::COUNT],
    steps: &[Option<u16>],
    mut dist: tiles::Table<i32>,
    seeds: impl Iterator<Item = (Tile, i32)>,
) -> (tiles::Table<i32>, i32) {
    // The step off a tile; None where no walk leaves it.
    let mut price: [Option<u16>; tiles::COUNT] = [None; tiles::COUNT];
    let dearest = steps.iter().flatten().max().copied().unwrap_or(0);
    let mut dial = Dial::new(dearest as usize + 1);

    let step_on = |weight: NonZeroU8| steps.get(weight.get() as usize).copied().flatten();

    // A border tile no seed leaves leads nowhere: a body stepping onto it leaves.
    for t in tiles::all() {
        match weight_of(footprint, weights, t) {
            None => dist[t] = WALL,
            Some(_) if on_edge(footprint, t) => (),
            Some(w) => price[t] = step_on(w),
        }
    }

    let mut seeds = seeds.peekable();
    let mut d = 0i32;
    let mut b = 0usize;
    loop {
        while let Some((seed, at)) = seeds.next_if(|(_, at)| *at <= d) {
            let old = dist[seed];
            if let Some(w) = weight_of(footprint, weights, seed)
                && at <= old
            {
                if old != UNREACHED {
                    dial.unlink(seed, &*dist);
                }
                dist[seed] = at;
                price[seed] = step_on(w);
                dial.link(seed, at);
            }
        }
        if dial.queued == 0 {
            let Some((_, at)) = seeds.peek() else { break };
            d = *at;
            b = (d % dial.ring as i32) as usize;
            continue;
        }
        let Some(tile) = dial.head[b] else {
            d += 1;
            b = if b + 1 == dial.ring { 0 } else { b + 1 };
            continue;
        };
        dial.unlink(tile, &*dist);
        if let Some(p) = price[tile] {
            dial.relaxed += 1;
            let candidate = d + i32::from(p);
            let (x, y) = (tiles::x_of(tile), tiles::y_of(tile));
            let mut inner = [tile; 8];
            let ns: &[Tile] = if x > 0 && y > 0 && x < tiles::SIDE - 1 && y < tiles::SIDE - 1 {
                for (n, offset) in inner.iter_mut().zip(OFFSETS) {
                    *n = Tile::of_index(tile.index().wrapping_add_signed(offset));
                }
                &inner
            } else {
                tiles::around(tile)
            };
            for &n in ns {
                let old = dist[n];
                if candidate < old {
                    if old != UNREACHED {
                        dial.unlink(n, &*dist);
                    }
                    dist[n] = candidate;
                    dial.link(n, candidate);
                }
            }
        }
    }

    for d in &mut dist {
        if *d == WALL {
            *d = UNREACHED;
        }
    }
    (dist, dial.relaxed)
}
