use crate::core::engine;
use crate::core::vocabulary::{BodyPart, part_count_in};

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Pattern {
    Worker,
    Harvester,
    Hauler,
    Upgrader,
    Claimer,
    Scout,
}

impl Pattern {
    pub fn is_harvester(self) -> bool {
        self == Pattern::Harvester
    }
    pub fn is_hauler(self) -> bool {
        self == Pattern::Hauler
    }
    pub fn is_upgrader(self) -> bool {
        self == Pattern::Upgrader
    }
    pub fn is_claimer(self) -> bool {
        self == Pattern::Claimer
    }
    pub fn is_scout(self) -> bool {
        self == Pattern::Scout
    }
}

pub const ALL_PATTERNS: [Pattern; 6] = [
    Pattern::Worker,
    Pattern::Harvester,
    Pattern::Hauler,
    Pattern::Upgrader,
    Pattern::Claimer,
    Pattern::Scout,
];

pub fn name_of(pattern: Pattern) -> &'static str {
    match pattern {
        Pattern::Worker => "worker",
        Pattern::Harvester => "harvester",
        Pattern::Hauler => "hauler",
        Pattern::Upgrader => "upgrader",
        Pattern::Claimer => "claimer",
        Pattern::Scout => "scout",
    }
}

pub fn block_of(pattern: Pattern) -> &'static [BodyPart] {
    const WORKER: [BodyPart; 3] = [BodyPart::Work, BodyPart::Carry, BodyPart::Move];
    const HARVESTER: [BodyPart; 4] = [
        BodyPart::Work,
        BodyPart::Work,
        BodyPart::Carry,
        BodyPart::Move,
    ];
    const HAULER: [BodyPart; 3] = [BodyPart::Carry, BodyPart::Carry, BodyPart::Move];
    const UPGRADER: [BodyPart; 3] = [BodyPart::Work, BodyPart::Carry, BodyPart::Move];
    const CLAIMER: [BodyPart; 2] = [BodyPart::Claim, BodyPart::Move];
    match pattern {
        Pattern::Worker => &WORKER,
        Pattern::Harvester => &HARVESTER,
        Pattern::Hauler => &HAULER,
        Pattern::Upgrader => &UPGRADER,
        Pattern::Claimer => &CLAIMER,
        Pattern::Scout => &[BodyPart::Move],
    }
}

pub fn key_part_of(pattern: Pattern) -> BodyPart {
    match pattern {
        Pattern::Worker | Pattern::Harvester | Pattern::Upgrader => BodyPart::Work,
        Pattern::Hauler => BodyPart::Carry,
        Pattern::Claimer => BodyPart::Claim,
        Pattern::Scout => BodyPart::Move,
    }
}

/// Unique because a spawn casts one body a tick.
pub fn cast_name(pattern: Pattern, time: i32, spawn: &str) -> String {
    format!("{}-{}-{}", name_of(pattern), time, spawn)
}

/// The spawn a cast name names, `None` where the name is not one of ours.
pub fn spawn_of_name(name: &str) -> Option<&str> {
    pattern_of_name(name)?;
    let rest = &name[name.find('-')? + 1..];
    Some(&rest[rest.find('-')? + 1..])
}

/// Read off the name, not the parts: damage strips parts head-first, so no
/// reading of the standing parts holds the pattern for the body's life.
pub fn pattern_of_name(name: &str) -> Option<Pattern> {
    let prefix = match name.find('-') {
        Some(at) => &name[..at],
        None => name,
    };
    ALL_PATTERNS.into_iter().find(|p| name_of(*p) == prefix)
}

/// Under one CARRY to four WORK a delivery is a commute for its WORK. The
/// upgrader row casts only a standing body, or it is a worker by another name.
pub fn standing(work: i32, carry: i32) -> bool {
    carry * 4 < work
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

fn replicate(n: i32, part: BodyPart) -> Vec<BodyPart> {
    vec![part; n.max(0) as usize]
}

/// `work + carry <= 2 × move` is a tile a tick loaded on a road. Grouped so
/// damage strips Work first.
fn worker_body(most: i32, capacity: i32) -> Vec<BodyPart> {
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
    let carry_cost = engine::part_cost(BodyPart::Carry);
    let move_cost = engine::part_cost(BodyPart::Move);
    let mut go = true;

    while go {
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
            go = false;
        }
    }

    let mut body = replicate(work, BodyPart::Work);
    body.extend(replicate(carry, BodyPart::Carry));
    body.extend(replicate(moves, BodyPart::Move));
    body
}

/// The dig falls into the container under it with no Carry; only a link takes
/// it from the store, and a bank that holds links has the room.
fn harvester_body(moves: impl Fn(i32) -> i32, most: i32, capacity: i32) -> Vec<BodyPart> {
    let dig = |n: i32| -> Vec<BodyPart> {
        let mut body = replicate(n, BodyPart::Work);
        body.extend(replicate(moves(n), BodyPart::Move));
        body
    };

    let mut n = most.min((engine::MAX_BODY_PARTS as i32 - 1) * 2 / 3);

    while n > 2 && engine::body_cost(&dig(n)) > capacity {
        n -= 1;
    }

    let n = n.max(2);

    let mut with_carry = vec![BodyPart::Carry];
    with_carry.extend(dig(n));
    let carry = if engine::body_cost(&with_carry) <= capacity {
        vec![BodyPart::Carry]
    } else {
        Vec::new()
    };

    let mut body = replicate(n, BodyPart::Work);
    body.extend(carry);
    body.extend(replicate(moves(n), BodyPart::Move));
    body
}

/// The MOVEs walk it to the buffer once at road parity; the CARRY holds one
/// draw from the buffer at its feet.
fn upgrader_body(most: i32, capacity: i32) -> Vec<BodyPart> {
    let pairs = ((capacity - engine::part_cost(BodyPart::Carry))
        / engine::body_cost(&[BodyPart::Work, BodyPart::Move]))
    .min((engine::MAX_BODY_PARTS as i32 - 1) / 2)
    .min(most)
    .max(1);

    let mut body = replicate(pairs, BodyPart::Work);
    body.push(BodyPart::Carry);
    body.extend(replicate(pairs, BodyPart::Move));
    body
}

fn whole_blocks(block: &[BodyPart], most: i32, capacity: i32) -> Vec<BodyPart> {
    let repeats = (capacity / engine::body_cost(block))
        .min(engine::MAX_BODY_PARTS as i32 / block.len() as i32)
        .min(most);

    let mut body = Vec::new();
    for (i, part) in block.iter().enumerate() {
        if block.iter().position(|p| p == part) == Some(i) {
            for _ in 0..repeats * part_count_in(block, *part) {
                body.push(*part);
            }
        }
    }
    body
}

/// v1's: a deposit's miner digs a container full in a hauler's few trips.
pub const MINER_WORK: i32 = 20;

pub fn sized(pattern: Pattern, most: i32, capacity: i32) -> Option<Vec<BodyPart>> {
    if capacity < engine::body_cost(block_of(pattern)) {
        None
    } else {
        let most = most.max(1);
        let body = match pattern {
            Pattern::Worker => worker_body(most, capacity),
            Pattern::Harvester => harvester_body(|n| (n + 1) / 2, most.max(2), capacity),
            Pattern::Hauler => whole_blocks(block_of(pattern), (most + 1) / 2, capacity),
            Pattern::Upgrader => upgrader_body(most, capacity),
            Pattern::Claimer => whole_blocks(block_of(pattern), most, capacity),
            Pattern::Scout => block_of(pattern).to_vec(),
        };
        Some(engine::with_move_last(body))
    }
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
pub fn hauler(ratio: i32, carry: i32, capacity: i32) -> Option<Vec<BodyPart>> {
    let mut block = replicate(ratio, BodyPart::Carry);
    block.push(BodyPart::Move);
    (capacity >= engine::body_cost(&block)).then(|| {
        let most = (carry + ratio - 1) / ratio;
        engine::with_move_last(whole_blocks(&block, most.max(1), capacity))
    })
}

/// The energy a controller takes a tick: past it an upgrader drinks nothing.
pub fn upgradable_at(level: i32) -> i32 {
    if level >= engine::CONTROLLER_MAX_LEVEL {
        engine::MAX_UPGRADE_PER_TICK
    } else {
        i32::MAX
    }
}

/// A tick of what `n` upgraders of the body are hired at: their drink, as much
/// as the controller takes, and their cost over a life.
pub fn upgrader_hire(upgradable: i32, n: i32, body: &[BodyPart]) -> f64 {
    f64::from(
        upgradable.min(n * part_count_in(body, BodyPart::Work) * engine::UPGRADE_DRAIN_PER_WORK),
    ) + f64::from(n * engine::body_cost(body)) / f64::from(engine::CREEP_LIFETIME)
}

/// For a seat in the casting colony's own room, which it walks once: legs
/// never cost it a Work.
pub fn home_harvester(most: i32, capacity: i32) -> Option<Vec<BodyPart>> {
    if capacity < engine::body_cost(block_of(Pattern::Harvester)) {
        None
    } else {
        let mut body = harvester_body(|_| 1, most.max(2), capacity);
        let legs = (part_count_in(&body, BodyPart::Work) + 1) / 2 - 1;
        let spare = (capacity - engine::body_cost(&body)) / engine::part_cost(BodyPart::Move);
        body.extend(replicate(legs.min(spare), BodyPart::Move));
        Some(engine::with_move_last(body))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The F# drew these; a swept grid says the same over every size a bank
    /// can hold.
    fn capacities() -> Vec<i32> {
        let mut out: Vec<i32> = (0..1_200).collect();
        out.extend((0..15_000).step_by(37));
        out
    }

    #[test]
    fn a_sized_body_fits_the_bank_and_the_part_cap_or_the_block_does_not_fit() {
        for capacity in capacities() {
            for most in 0..60 {
                for pattern in ALL_PATTERNS {
                    match sized(pattern, most, capacity) {
                        None => assert!(
                            capacity < engine::body_cost(block_of(pattern)),
                            "{} {} {}",
                            name_of(pattern),
                            most,
                            capacity
                        ),
                        Some(body) => {
                            assert!(body.len() <= engine::MAX_BODY_PARTS);
                            assert!(engine::body_cost(&body) <= capacity);
                            assert_eq!(body.last(), Some(&BodyPart::Move));
                        }
                    }
                }
                match home_harvester(most, capacity) {
                    None => assert!(capacity < engine::body_cost(block_of(Pattern::Harvester))),
                    Some(body) => {
                        assert!(body.len() <= engine::MAX_BODY_PARTS);
                        assert!(engine::body_cost(&body) <= capacity);
                        assert_eq!(body.last(), Some(&BodyPart::Move));
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
    fn a_worker_held_to_k_work_costs_no_more_than_k_unit_blocks() {
        let unit = engine::body_cost(block_of(Pattern::Worker));
        for capacity in capacities() {
            for k in 1..=20 {
                if let Some(body) = sized(Pattern::Worker, k, capacity) {
                    assert!(part_count_in(&body, BodyPart::Work) <= k);
                    assert!(engine::body_cost(&body) <= k * unit);
                }
            }
        }
    }
}
