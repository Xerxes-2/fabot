//! Knows nothing of the Atlas: it prices raw weight arrays (-1 where nothing
//! may stand), so the Atlas keeps its own representation private.
use super::tiles;
use crate::core::engine;
use crate::core::vocabulary::{BodyPart, boost};
use crate::core::world::Creep;

pub const UNREACHED: i32 = i32::MAX;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

/// The flood reads a table, not a closure; -1 where no step is taken.
pub fn step_table(factor: FatigueFactor) -> Vec<i32> {
    (0..=SEAT_WEIGHT)
        .map(|w| step_ticks(factor, w).unwrap_or(-1))
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

pub fn weight_of(footprint: Footprint, weights: &[i32], tile: i32) -> i32 {
    match footprint {
        Footprint::One => weights[tile as usize],
        Footprint::Quad => {
            let x = tiles::x_of(tile);
            let y = tiles::y_of(tile);
            if x == tiles::SIDE - 1 || y == tiles::SIDE - 1 {
                -1
            } else {
                let a = weights[tile as usize];
                let b = weights[(tile + 1) as usize];
                let c = weights[(tile + tiles::SIDE) as usize];
                let d = weights[(tile + tiles::SIDE + 1) as usize];
                if a < 0 || b < 0 || c < 0 || d < 0 {
                    -1
                } else {
                    a.max(b).max(c.max(d))
                }
            }
        }
    }
}

/// A step onto the border ring carries the body into the next room.
pub fn on_edge(footprint: Footprint, tile: i32) -> bool {
    match footprint {
        Footprint::One => tiles::on_edge(tile),
        Footprint::Quad => {
            let x = tiles::x_of(tile);
            let y = tiles::y_of(tile);
            x == 0 || y == 0 || x >= tiles::SIDE - 2 || y >= tiles::SIDE - 2
        }
    }
}

const OFFSETS: [i32; 8] = [
    -tiles::SIDE,
    1,
    tiles::SIDE,
    -1,
    -tiles::SIDE - 1,
    -tiles::SIDE + 1,
    tiles::SIDE - 1,
    tiles::SIDE + 1,
];

/// Dial's queue: no step costs more than the table's dearest, so a ring of one
/// bucket more holds every queued distance apart.
struct Dial {
    ring: usize,
    head: Vec<i32>,
    next: Vec<i32>,
    prev: Vec<i32>,
    queued: i32,
    relaxed: i32,
}

impl Dial {
    fn new(ring: usize) -> Dial {
        Dial {
            ring,
            head: vec![-1; ring],
            next: vec![0; tiles::COUNT],
            prev: vec![0; tiles::COUNT],
            queued: 0,
            relaxed: 0,
        }
    }

    fn link(&mut self, tile: i32, d: i32) {
        let b = (d % self.ring as i32) as usize;
        let h = self.head[b];
        self.next[tile as usize] = h;
        self.prev[tile as usize] = -1;
        if h >= 0 {
            self.prev[h as usize] = tile;
        }
        self.head[b] = tile;
        self.queued += 1;
    }

    fn unlink(&mut self, tile: i32, dist: &[i32]) {
        let p = self.prev[tile as usize];
        let n = self.next[tile as usize];
        if p >= 0 {
            self.next[p as usize] = n;
        } else {
            let b = (dist[tile as usize] % self.ring as i32) as usize;
            self.head[b] = n;
        }
        if n >= 0 {
            self.prev[n as usize] = p;
        }
        self.queued -= 1;
    }
}

/// Dial's queue: no step costs more than the table's dearest, so a ring of one
/// bucket more holds every queued distance apart.
pub fn flood(footprint: Footprint, weights: &[i32], steps: &[i32], goals: &[i32]) -> Vec<i32> {
    let mut dist = vec![UNREACHED; tiles::COUNT];
    // The step onto a tile; -1 where no walk leaves it.
    let mut price = vec![0i32; tiles::COUNT];
    let mut dearest = 0;
    for s in steps {
        dearest = dearest.max(*s);
    }
    let mut dial = Dial::new((dearest + 1) as usize);

    let step_on = |weight: i32| -> i32 {
        if (weight as usize) < steps.len() {
            steps[weight as usize]
        } else {
            -1
        }
    };

    // A blocked tile reads -1 until the end, so no candidate beats it. A
    // non-goal border tile leads nowhere: a body stepping onto it leaves.
    for t in 0..tiles::COUNT {
        let w = weight_of(footprint, weights, t as i32);
        if w < 0 {
            dist[t] = -1;
        } else if on_edge(footprint, t as i32) {
            price[t] = -1;
        } else {
            price[t] = step_on(w);
        }
    }

    for &goal in goals {
        if goal >= 0 && goal < tiles::COUNT as i32 && dist[goal as usize] > 0 {
            dist[goal as usize] = 0;
            price[goal as usize] = step_on(weight_of(footprint, weights, goal));
            dial.link(goal, 0);
        }
    }

    let mut d = 0i32;
    let mut b = 0usize;
    while dial.queued > 0 {
        let tile = dial.head[b];
        if tile < 0 {
            d += 1;
            b = if b + 1 == dial.ring { 0 } else { b + 1 };
        } else {
            dial.unlink(tile, &dist);
            let p = price[tile as usize];
            if p >= 0 {
                dial.relaxed += 1;
                let candidate = d + p;
                let x = tiles::x_of(tile);
                let y = tiles::y_of(tile);
                let inner = x > 0 && y > 0 && x < tiles::SIDE - 1 && y < tiles::SIDE - 1;
                let origin = if inner { tile } else { 0 };
                let ns: &[i32] = if inner { &OFFSETS } else { tiles::around(tile) };
                for &offset in ns {
                    let n = origin + offset;
                    let old = dist[n as usize];
                    if candidate < old {
                        if old != UNREACHED {
                            dial.unlink(n, &dist);
                        }
                        dist[n as usize] = candidate;
                        dial.link(n, candidate);
                    }
                }
            }
        }
    }

    for d in dist.iter_mut() {
        if *d < 0 {
            *d = UNREACHED;
        }
    }
    tally("flood", dial.relaxed);
    dist
}
