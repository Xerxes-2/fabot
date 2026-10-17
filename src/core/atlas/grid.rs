//! Knows nothing of the Atlas: it prices raw weight arrays, so the Atlas
//! keeps its own representation private.
use std::num::NonZeroU8;

use super::tiles::{self, Tile};
use crate::core::engine;
use crate::core::vocabulary::{BodyPart, boost};
use crate::core::world::Creep;

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

impl Footprint {
    pub fn is_one(self) -> bool {
        self == Footprint::One
    }
    pub fn is_quad(self) -> bool {
        self == Footprint::Quad
    }
}

/// MOVE pays off two fatigue a tick, at its boost's multiple.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FatigueFactor {
    pub weight: i32,
    pub moves: i32,
}

/// The engine's rule: a destroyed part still weighs; only a live one carries
/// or moves.
pub fn factor_of(creep: &Creep) -> FatigueFactor {
    let mut weight = 0;
    let mut carry = 0;
    let mut moves = 0;
    for part in &creep.body {
        match part.kind {
            BodyPart::Move => {
                if part.hits > 0 {
                    moves += boost::multiple(BodyPart::Move, part.boost);
                }
            }
            BodyPart::Carry => {
                if part.hits > 0 {
                    carry += 1;
                }
            }
            _ => weight += 1,
        }
    }
    let load = creep.store.used();
    FatigueFactor {
        weight: weight
            + carry.min((load + engine::CARRY_PART_CAPACITY - 1) / engine::CARRY_PART_CAPACITY),
        moves,
    }
}

pub fn empty_factor(body: &[BodyPart]) -> FatigueFactor {
    let mut weight = 0;
    let mut moves = 0;
    for part in body {
        match part {
            BodyPart::Move => moves += 1,
            BodyPart::Carry => (),
            _ => weight += 1,
        }
    }
    FatigueFactor { weight, moves }
}

pub fn loaded_factor(body: &[BodyPart]) -> FatigueFactor {
    let moves = body.iter().filter(|p| **p == BodyPart::Move).count() as i32;
    FatigueFactor {
        weight: body.len() as i32 - moves,
        moves,
    }
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
pub fn step_ticks(factor: FatigueFactor, weight: i32) -> Option<i32> {
    let seat = weight == SEAT_WEIGHT;
    let ticks = (step_units(factor, if seat { engine::SWAMP_WEIGHT } else { weight })? + 1) / 2;
    Some(if seat { SEAT_SWAMPS * ticks } else { ticks })
}

/// The flood reads a table, not a closure.
pub fn step_table(factor: FatigueFactor) -> Vec<Option<u16>> {
    (0..=SEAT_WEIGHT)
        .map(|w| step_ticks(factor, w).map(|t| t as u16))
        .collect()
}

/// The bench's counts (`ARCHITECTURE.md` §9.3), read by `scripts/bench.mjs
/// --counts`; nothing in the bot reads them.
pub fn tally(what: &str, n: i32) {
    #[cfg(target_arch = "wasm32")]
    {
        use wasm_bindgen::JsCast;

        let counter = crate::app::js::field(&js_sys::global(), "__fabotCount");
        if counter.is_function() {
            let counter: &js_sys::Function = counter.unchecked_ref();
            let _ = counter.call2(
                &wasm_bindgen::JsValue::NULL,
                &wasm_bindgen::JsValue::from_str(what),
                &wasm_bindgen::JsValue::from_f64(n as f64),
            );
        }
    }

    #[cfg(not(target_arch = "wasm32"))]
    let _ = (what, n);
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
            let x = tiles::x_of(tile);
            let y = tiles::y_of(tile);
            if x == tiles::SIDE - 1 || y == tiles::SIDE - 1 {
                None
            } else {
                let i = tile.index();
                let side = tiles::SIDE as usize;
                let (a, b, c, d) = (
                    weights[i]?,
                    weights[i + 1]?,
                    weights[i + side]?,
                    weights[i + side + 1]?,
                );
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
            let x = tiles::x_of(tile);
            let y = tiles::y_of(tile);
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
        let p = self.prev[tile];
        let n = self.next[tile];
        match p {
            Some(p) => self.next[p] = n,
            None => {
                let b = (dist[tile] % self.ring as i32) as usize;
                self.head[b] = n;
            }
        }
        if let Some(n) = n {
            self.prev[n] = p;
        }
        self.queued -= 1;
    }
}

/// Dial's queue: no step costs more than the table's dearest, so a ring of one
/// bucket more holds every queued distance apart.
pub fn flood(
    footprint: Footprint,
    weights: &[Weight; tiles::COUNT],
    steps: &[Option<u16>],
    goals: &[Tile],
) -> tiles::Table<i32> {
    let mut dist = tiles::table(UNREACHED);
    // The step off a tile; None where no walk leaves it.
    let mut price: [Option<u16>; tiles::COUNT] = [None; tiles::COUNT];
    let dearest = steps.iter().flatten().max().copied().unwrap_or(0);
    let mut dial = Dial::new(dearest as usize + 1);

    let step_on = |weight: NonZeroU8| steps.get(weight.get() as usize).copied().flatten();

    // A non-goal border tile leads nowhere: a body stepping onto it leaves.
    for t in tiles::all() {
        match weight_of(footprint, weights, t) {
            None => dist[t] = WALL,
            Some(_) if on_edge(footprint, t) => (),
            Some(w) => price[t] = step_on(w),
        }
    }

    for &goal in goals {
        if let Some(w) = weight_of(footprint, weights, goal)
            && dist[goal] > 0
        {
            dist[goal] = 0;
            price[goal] = step_on(w);
            dial.link(goal, 0);
        }
    }

    let mut d = 0i32;
    let mut b = 0usize;
    while dial.queued > 0 {
        let Some(tile) = dial.head[b] else {
            d += 1;
            b = if b + 1 == dial.ring { 0 } else { b + 1 };
            continue;
        };
        dial.unlink(tile, &*dist);
        if let Some(p) = price[tile] {
            dial.relaxed += 1;
            let candidate = d + i32::from(p);
            let x = tiles::x_of(tile);
            let y = tiles::y_of(tile);
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

    for d in dist.iter_mut() {
        if *d == WALL {
            *d = UNREACHED;
        }
    }
    tally("flood", dial.relaxed);
    dist
}
