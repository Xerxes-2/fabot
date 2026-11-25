use strum::{EnumString, IntoStaticStr};

use fabot_base::engine;
use fabot_base::grid::{self, FatigueFactor};
use fabot_base::vocabulary::{CLAIMER, Part, part_count_in};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString, IntoStaticStr)]
#[cfg_attr(test, derive(strum::EnumIter))]
#[strum(serialize_all = "lowercase")]
pub enum Pattern {
    Worker,
    Harvester,
    Hauler,
    Upgrader,
    Claimer,
    Scout,
    Dismantler,
}

pub fn block_of(pattern: Pattern) -> &'static [Part] {
    const WORKER: [Part; 3] = [Part::Work, Part::Carry, Part::Move];
    const HARVESTER: [Part; 4] = [Part::Work, Part::Work, Part::Carry, Part::Move];
    const HAULER: [Part; 3] = [Part::Carry, Part::Carry, Part::Move];
    const UPGRADER: [Part; 3] = [Part::Work, Part::Carry, Part::Move];
    match pattern {
        Pattern::Worker => &WORKER,
        Pattern::Harvester => &HARVESTER,
        Pattern::Hauler => &HAULER,
        Pattern::Upgrader => &UPGRADER,
        Pattern::Claimer => &CLAIMER,
        Pattern::Scout => &[Part::Move],
        Pattern::Dismantler => &[Part::Work, Part::Move],
    }
}

/// Unique because a spawn casts one body a tick.
pub fn cast_name(pattern: Pattern, time: i32, spawn: &str) -> String {
    let name: &str = pattern.into();
    format!("{name}-{time}-{spawn}")
}

pub fn spawn_of_name(name: &str) -> Option<&str> {
    name.rsplit_once('-').map(|(_, spawn)| spawn)
}

/// Read off the name, not the parts: damage strips parts head-first, so no
/// reading of the standing parts holds the pattern for the body's life.
pub fn pattern_of_name(name: &str) -> Option<Pattern> {
    name.split_once('-').map_or(name, |(p, _)| p).parse().ok()
}

/// At most one CARRY to four WORK, a delivery is a commute for its WORK;
/// RCL2's 550 energy affords four. The upgrader row casts only a standing body, or it is
/// a worker by another name.
pub fn standing(work: i32, carry: i32) -> bool {
    carry * 4 <= work
}

pub fn pattern_of_parts(work: i32, carry: i32, movement: i32, claim: i32) -> Pattern {
    if claim > 0 {
        Pattern::Claimer
    } else if work + carry == 0 {
        Pattern::Scout
    } else if work > 0 && (carry == 0 || work > movement) {
        Pattern::Harvester
    } else if work == 0 && carry > 0 {
        Pattern::Hauler
    } else if standing(work, carry) {
        Pattern::Upgrader
    } else {
        Pattern::Worker
    }
}

fn replicate(n: i32, part: Part) -> Vec<Part> {
    vec![part; n.max(0) as usize]
}

/// `work + carry <= 2 × move` is a tile a tick loaded on a road. Grouped so
/// damage strips Work first.
fn worker_body(most: i32, capacity: i32) -> Vec<Part> {
    let block = block_of(Pattern::Worker);
    let dearest = engine::MAX_BODY_PARTS as i32;

    let repeats = (capacity / engine::body_cost(block))
        .min(dearest / 3)
        .min(most);

    let work = repeats;
    let mut carry = repeats;
    let mut moves = repeats;

    let mut budget = if repeats >= most {
        0
    } else {
        capacity - repeats * engine::body_cost(block)
    };

    let mut slots = dearest - 3 * repeats;
    let carry_cost = engine::part_cost(Part::Carry);
    let move_cost = engine::part_cost(Part::Move);
    loop {
        if slots >= 1 && budget >= carry_cost && work + carry < 2 * moves {
            carry += 1;
            budget -= carry_cost;
            slots -= 1;
        } else if slots >= 2 && budget >= carry_cost + move_cost {
            carry += 1;
            moves += 1;
            budget -= carry_cost + move_cost;
            slots -= 2;
        } else if slots >= 1 && budget >= move_cost {
            moves += 1;
            budget -= move_cost;
            slots -= 1;
        } else {
            break;
        }
    }

    let mut body = replicate(work, Part::Work);
    body.extend(replicate(carry, Part::Carry));
    body.extend(replicate(moves, Part::Move));
    body
}

/// The dig falls into the container under it with no Carry; only a link takes
/// it from the store, and a bank that holds links has the room.
fn harvester_body(moves: impl Fn(i32) -> i32, most: i32, capacity: i32) -> Vec<Part> {
    let dig = |n: i32| -> Vec<Part> {
        let mut body = replicate(n, Part::Work);
        body.extend(replicate(moves(n), Part::Move));
        body
    };

    let mut n = most.min((engine::MAX_BODY_PARTS as i32 - 1) * 2 / 3);

    while n > 2 && engine::body_cost(&dig(n)) > capacity {
        n -= 1;
    }

    let n = n.max(2);

    let mut body = replicate(n, Part::Work);
    if engine::body_cost(&dig(n)) + engine::part_cost(Part::Carry) <= capacity {
        body.push(Part::Carry);
    }
    body.extend(replicate(moves(n), Part::Move));
    body
}

/// It walks to its seat once and empty, so its CARRY never fatigues: a MOVE
/// is bought only while the ticks it saves upgrade more than the WORK it costs.
fn upgrader_body(roads: i32, plains: i32, most: i32, capacity: i32) -> Vec<Part> {
    let dearest = engine::MAX_BODY_PARTS as i32;
    let [work_cost, carry_cost, move_cost] =
        [Part::Work, Part::Carry, Part::Move].map(engine::part_cost);
    let priced = |moves: i32| {
        let work = ((capacity - carry_cost - moves * move_cost) / work_cost)
            .min(most)
            .min(dearest - 1 - moves);
        let factor = FatigueFactor {
            weight: work,
            moves,
        };
        let step = |weight| grid::step_ticks(factor, weight);
        let (Some(road), Some(plain)) = (step(engine::ROAD_WEIGHT), step(engine::PLAIN_WEIGHT))
        else {
            return (work, 0.0);
        };
        let arrives = roads * road + plains * plain;
        let spawned = engine::SPAWN_TICKS_PER_PART * (work + 1 + moves);
        let upgrading = (engine::CREEP_LIFETIME - arrives - spawned).max(0);
        let cost = work * work_cost + carry_cost + moves * move_cost;
        (work, f64::from(work * upgrading) / f64::from(cost))
    };
    let (mut moves, (mut work, mut rate)) = (1, priced(1));
    for m in 2..dearest {
        let (w, r) = priced(m);
        if w < 1 {
            break;
        }
        if r > rate {
            (moves, work, rate) = (m, w, r);
        }
    }
    let mut body = replicate(work, Part::Work);
    body.push(Part::Carry);
    body.extend(replicate(moves, Part::Move));
    body
}

/// `roads` and `plains` are the walk to the seat, a swamp five plains.
pub fn upgrader(roads: i32, plains: i32, most: i32, capacity: i32) -> Option<Vec<Part>> {
    (capacity >= engine::body_cost(block_of(Pattern::Upgrader)))
        .then(|| engine::with_move_last(upgrader_body(roads, plains, most.max(1), capacity)))
}

fn whole_blocks(block: &[Part], most: i32, capacity: i32) -> Vec<Part> {
    let repeats = (capacity / engine::body_cost(block))
        .min(engine::MAX_BODY_PARTS as i32 / block.len() as i32)
        .min(most);

    let mut body = Vec::new();
    for (i, part) in block.iter().enumerate() {
        if block.iter().position(|p| p == part) == Some(i) {
            body.extend(replicate(repeats * part_count_in(block, *part), *part));
        }
    }
    body
}

/// v1's: a deposit's miner digs a container full in a hauler's few trips.
pub const MINER_WORK: i32 = 20;

pub fn sized(pattern: Pattern, most: i32, capacity: i32) -> Option<Vec<Part>> {
    if capacity < engine::body_cost(block_of(pattern)) {
        return None;
    }
    let most = most.max(1);
    let body = match pattern {
        Pattern::Worker => worker_body(most, capacity),
        Pattern::Harvester => harvester_body(|n| (n + 1) / 2, most.max(2), capacity),
        Pattern::Hauler => whole_blocks(block_of(pattern), (most + 1) / 2, capacity),
        Pattern::Upgrader => upgrader_body(0, 0, most, capacity),
        Pattern::Claimer | Pattern::Dismantler => whole_blocks(block_of(pattern), most, capacity),
        Pattern::Scout => block_of(pattern).to_vec(),
    };
    Some(engine::with_move_last(body))
}

/// A route's walk for a loaded body at each full-speed pace, and back empty.
#[derive(Clone, Copy, Debug)]
pub struct Leg {
    /// One CARRY a MOVE: full speed loaded on plain.
    pub even: i32,
    /// Two CARRY a MOVE: full speed loaded on road only.
    pub roaded: i32,
    pub empty: i32,
}

/// CARRY a MOVE: any ratio between 2 and 1 walks plain no faster than 2 and
/// carries less, so only these two are ever cast. Energy delivered over a
/// life per energy spent decides; a tie goes to 2, fewer parts and creeps a
/// load.
pub fn carry_per_move(leg: Leg) -> i32 {
    let delivered = |carry: i32, cost: i32, loaded: i32| {
        f64::from(carry) / f64::from(cost) / f64::from(loaded + leg.empty).max(1.0)
    };
    let even = delivered(engine::CARRY_PART_CAPACITY, 100, leg.even);
    let roaded = delivered(2 * engine::CARRY_PART_CAPACITY, 150, leg.roaded);
    if even > roaded { 1 } else { 2 }
}

/// A hauler of `carry` CARRY at most, in whole blocks of the ratio.
pub fn hauler(ratio: i32, carry: i32, capacity: i32) -> Option<Vec<Part>> {
    let mut block = replicate(ratio, Part::Carry);
    block.push(Part::Move);
    (capacity >= engine::body_cost(&block)).then(|| {
        let most = (carry + ratio - 1) / ratio;
        engine::with_move_last(whole_blocks(&block, most.max(1), capacity))
    })
}

/// The energy a controller takes a tick: past it an upgrader drinks nothing.
pub fn upgradable_at(level: i32) -> i32 {
    if engine::topped_out(level) {
        engine::MAX_UPGRADE_PER_TICK
    } else {
        i32::MAX
    }
}

/// So the dig finishes the source inside its regeneration window despite the
/// ticks a body loses walking and dropping its load.
const HARVEST_SPARE_WORK: i32 = 1;

/// `home`: a seat in the casting colony's own room, which it walks once.
pub fn harvester(home: bool, rate: i32, capacity: i32) -> Option<Vec<Part>> {
    let work = (rate + engine::HARVEST_PER_WORK - 1) / engine::HARVEST_PER_WORK;
    let work = work + HARVEST_SPARE_WORK;
    if home {
        home_harvester(work, capacity)
    } else {
        sized(Pattern::Harvester, work, capacity)
    }
}

/// Its legs never cost it a Work.
fn home_harvester(most: i32, capacity: i32) -> Option<Vec<Part>> {
    if capacity < engine::body_cost(block_of(Pattern::Harvester)) {
        return None;
    }
    let mut body = harvester_body(|_| 1, most.max(2), capacity);
    let legs = (part_count_in(&body, Part::Work) + 1) / 2 - 1;
    let spare = (capacity - engine::body_cost(&body)) / engine::part_cost(Part::Move);
    body.extend(replicate(legs.min(spare), Part::Move));
    Some(engine::with_move_last(body))
}

#[cfg(test)]
mod tests {
    use strum::IntoEnumIterator;

    use super::*;

    fn capacities() -> Vec<i32> {
        let mut out: Vec<i32> = (0..1_200).collect();
        out.extend((0..15_000).step_by(37));
        out
    }

    #[test]
    fn a_sized_body_fits_the_bank_and_the_part_cap_or_the_block_does_not_fit() {
        for capacity in capacities() {
            for most in 0..60 {
                for pattern in Pattern::iter() {
                    match sized(pattern, most, capacity) {
                        None => assert!(
                            capacity < engine::body_cost(block_of(pattern)),
                            "{pattern:?} {most} {capacity}"
                        ),
                        Some(body) => {
                            assert!(body.len() <= engine::MAX_BODY_PARTS);
                            assert!(engine::body_cost(&body) <= capacity);
                            assert_eq!(body.last(), Some(&Part::Move));
                        }
                    }
                }
                match home_harvester(most, capacity) {
                    None => assert!(capacity < engine::body_cost(block_of(Pattern::Harvester))),
                    Some(body) => {
                        assert!(body.len() <= engine::MAX_BODY_PARTS);
                        assert!(engine::body_cost(&body) <= capacity);
                        assert_eq!(body.last(), Some(&Part::Move));
                    }
                }
            }
        }
    }

    #[test]
    fn a_hauler_on_road_carries_two_a_move_and_on_plain_one() {
        for d in 1..60 {
            let road = Leg {
                even: d,
                roaded: d,
                empty: d,
            };
            let plain = Leg {
                even: d,
                roaded: 2 * d,
                empty: d,
            };
            assert_eq!(carry_per_move(road), 2);
            assert_eq!(carry_per_move(plain), 1);
        }
    }

    #[test]
    fn an_upgrader_buys_move_only_as_its_walk_grows_and_leaves_its_road() {
        let moves = |roads: i32, plains: i32, most: i32, capacity: i32| {
            part_count_in(
                &upgrader(roads, plains, most, capacity).unwrap(),
                Part::Move,
            )
        };
        for capacity in [550, 1300, 2300, 5600, 12900] {
            for most in [15, i32::MAX] {
                assert_eq!(moves(0, 0, most, capacity), 1);
                for d in 1..50 {
                    assert!(moves(d, 0, most, capacity) >= moves(d - 1, 0, most, capacity));
                    assert!(moves(0, d, most, capacity) >= moves(d, 0, most, capacity));
                }
            }
        }
        let rcl8 = upgrader(15, 0, 15, 12900).unwrap();
        assert_eq!(part_count_in(&rcl8, Part::Work), 15);
        assert_eq!(part_count_in(&rcl8, Part::Move), 2);
    }

    #[test]
    fn a_worker_held_to_k_work_costs_no_more_than_k_unit_blocks() {
        let unit = engine::body_cost(block_of(Pattern::Worker));
        for capacity in capacities() {
            for k in 1..=20 {
                if let Some(body) = sized(Pattern::Worker, k, capacity) {
                    assert!(part_count_in(&body, Part::Work) <= k);
                    assert!(engine::body_cost(&body) <= k * unit);
                }
            }
        }
    }
}
