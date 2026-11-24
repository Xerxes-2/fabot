use super::vocabulary::{Part, ResourceType, damage_ratio, multiple, part_count_in};
use screeps::StructureType;

pub const MAX_BODY_PARTS: usize = screeps::MAX_CREEP_SIZE as usize;
pub const HARVEST_PER_WORK: i32 = screeps::HARVEST_POWER as i32;
pub const CARRY_PART_CAPACITY: i32 = screeps::CARRY_CAPACITY as i32;
pub const MINERAL_HARVEST_PER_WORK: i32 = screeps::HARVEST_MINERAL_POWER as i32;
/// The intent pass runs before the object pass, so digs land one tick later.
pub const MINERAL_HARVEST_CYCLE: i32 = screeps::EXTRACTOR_COOLDOWN as i32 + 1;
pub const CREEP_LIFETIME: i32 = screeps::CREEP_LIFE_TIME as i32;
pub const CLAIM_LIFETIME: i32 = screeps::CREEP_CLAIM_LIFE_TIME as i32;
/// ×1.1: the longest a deployed core stands.
pub const STRONGHOLD_LONGEST_COLLAPSE: i32 = screeps::STRONGHOLD_DECAY_TICKS as i32 * 11 / 10;
pub const SPAWN_TICKS_PER_PART: i32 = screeps::CREEP_SPAWN_TIME as i32;
pub const BOOST_ENERGY_PER_PART: i32 = screeps::LAB_BOOST_ENERGY as i32;
pub const BOOST_MINERAL_PER_PART: i32 = screeps::LAB_BOOST_MINERAL as i32;
pub const RESERVATION_CAP: i32 = screeps::CONTROLLER_RESERVE_MAX as i32;

pub fn extension_energy(level: i32) -> i32 {
    screeps::extension_energy_capacity(level.max(0) as u32) as i32
}
/// The reservation decays by one a tick, so N parts gain N - 1.
pub const RESERVE_PER_CLAIM: i32 = screeps::CONTROLLER_RESERVE as i32;
pub const CONTAINER_CAPACITY: i32 = screeps::CONTAINER_CAPACITY as i32;
pub const STORAGE_CAPACITY: i32 = screeps::STORAGE_CAPACITY as i32;
/// A dropped pile loses ceil(amount / this) a tick.
pub const ENERGY_DECAY: i32 = screeps::ENERGY_DECAY as i32;
pub const TERMINAL_MIN_SEND: i32 = screeps::TERMINAL_MIN_SEND as i32;

pub fn send_cost(amount: i32, distance: i32) -> i32 {
    (f64::from(amount)
        * (1.0 - (-f64::from(distance) / f64::from(screeps::TERMINAL_SEND_COST_SCALE)).exp()))
    .ceil() as i32
}
pub const LAB_MINERAL_CAPACITY: i32 = screeps::LAB_MINERAL_CAPACITY as i32;
pub const LAB_REACTION_AMOUNT: i32 = screeps::LAB_REACTION_AMOUNT as i32;
/// Chebyshev over the room grid; the observed room is visible the next tick alone.
pub const OBSERVER_RANGE: i32 = screeps::OBSERVER_RANGE as i32;
pub const MELEE_RANGE: i32 = 1;
pub const RANGED_RANGE: i32 = screeps::CREEP_RANGED_ACTION_RANGE as i32;
/// The reach of upgradeController, build and repair.
pub const UPGRADE_RANGE: i32 = 3;
/// A keeper guards the first source, else mineral, this near its lair.
pub const KEEPER_GUARD_RANGE: i32 = 5;

pub fn keeper_body() -> Vec<Part> {
    let mut body = [[Part::Tough; 17].as_slice(), &[Part::Move; 13]].concat();
    body.extend([Part::Attack, Part::RangedAttack].repeat(10));
    body
}
pub const ATTACK_POWER: i32 = screeps::ATTACK_POWER as i32;
pub const RANGED_ATTACK_POWER: i32 = screeps::RANGED_ATTACK_POWER as i32;
pub const DISMANTLE_POWER: i32 = screeps::DISMANTLE_POWER as i32;
pub const REPAIR_POWER: i32 = screeps::REPAIR_POWER as i32;
pub const BUILD_POWER: i32 = screeps::BUILD_POWER as i32;
pub const HEAL_POWER: i32 = screeps::HEAL_POWER as i32;
pub const RANGED_HEAL_POWER: i32 = screeps::RANGED_HEAL_POWER as i32;
pub const TOWER_POWER_ATTACK: i32 = screeps::TOWER_POWER_ATTACK as i32;
pub const TOWER_CAPACITY: i32 = screeps::TOWER_CAPACITY as i32;
/// A tower holding less does nothing.
pub const TOWER_ENERGY_COST: i32 = screeps::TOWER_ENERGY_COST as i32;
pub const TOWER_FALLOFF_RANGE: i32 = screeps::TOWER_FALLOFF_RANGE as i32;
pub const PART_HITS: i32 = screeps::CREEP_HITS_PER_PART as i32;
pub const HELD_OUTPUT_PER_TICK: i32 =
    (screeps::SOURCE_ENERGY_CAPACITY / screeps::ENERGY_REGEN_TIME) as i32;
pub const NEUTRAL_OUTPUT_PER_TICK: i32 =
    (screeps::SOURCE_ENERGY_NEUTRAL_CAPACITY / screeps::ENERGY_REGEN_TIME) as i32;
pub const SOURCE_REGEN_TIME: i32 = screeps::ENERGY_REGEN_TIME as i32;
pub const UPGRADE_DRAIN_PER_WORK: i32 = screeps::UPGRADE_CONTROLLER_POWER as i32;
/// At `controllerMaxLevel`, summed over every creep upgrading it; a creep past
/// the tally upgrades and spends nothing.
pub const MAX_UPGRADE_PER_TICK: i32 = screeps::CONTROLLER_MAX_UPGRADE_PER_TICK as i32;
pub const CONTROLLER_MAX_LEVEL: i32 = 8;

pub fn topped_out(level: i32) -> bool {
    level >= CONTROLLER_MAX_LEVEL
}

pub const ROAD_WEIGHT: i32 = screeps::MOVE_COST_ROAD as i32;
pub const PLAIN_WEIGHT: i32 = screeps::MOVE_COST_PLAIN as i32;
pub const SWAMP_WEIGHT: i32 = screeps::MOVE_COST_SWAMP as i32;
pub const ROAD_BUILD_COST: i32 = StructureType::Road.construction_cost().unwrap() as i32;
/// A swamp road's build, hits and decay.
pub const ROAD_SWAMP_RATIO: i32 = screeps::CONSTRUCTION_COST_ROAD_SWAMP_RATIO as i32;
/// Every ROAD_DECAY_TIME, on plain.
pub const ROAD_DECAY_AMOUNT: i32 = screeps::ROAD_DECAY_AMOUNT as i32;
pub const ROAD_DECAY_TIME: i32 = screeps::ROAD_DECAY_TIME as i32;
/// Decay-timer ticks a step brings forward, per body part.
pub const ROAD_WEAROUT: i32 = screeps::ROAD_WEAROUT as i32;
/// Every CONTAINER_DECAY_TIME, or _OWNED in an owned room.
pub const CONTAINER_DECAY_AMOUNT: i32 = screeps::CONTAINER_DECAY as i32;
pub const CONTAINER_DECAY_TIME: i32 = screeps::CONTAINER_DECAY_TIME as i32;
pub const CONTAINER_DECAY_TIME_OWNED: i32 = screeps::CONTAINER_DECAY_TIME_OWNED as i32;
pub const CONTAINER_BUILD_COST: i32 = StructureType::Container.construction_cost().unwrap() as i32;
/// A creep aged out is buried on its last tick: its tombstone's creep has this left.
pub const AGED_OUT_TICKS_TO_LIVE: i32 = 1;
pub const RAMPART_DECAY_AMOUNT: i32 = screeps::RAMPART_DECAY_AMOUNT as i32;
pub const RAMPART_DECAY_TIME: i32 = screeps::RAMPART_DECAY_TIME as i32;
/// REPAIR_COST: 0.01 energy a hit.
pub const REPAIR_HITS_PER_ENERGY: i32 = 100;
pub const MAX_CONSTRUCTION_SITES: i32 = screeps::MAX_CONSTRUCTION_SITES as i32;
pub const ROOM_SIDE: i32 = screeps::ROOM_SIZE as i32;

pub fn part_cost(part: Part) -> i32 {
    part.cost() as i32
}

pub fn lifetime(mut body: impl Iterator<Item = Part>) -> i32 {
    if body.any(|p| p == Part::Claim) {
        CLAIM_LIFETIME
    } else {
        CREEP_LIFETIME
    }
}

#[inline]
pub fn body_cost(body: &[Part]) -> i32 {
    body.iter().map(|part| part_cost(*part)).sum()
}

pub fn controller_downgrade(level: i32) -> i32 {
    u8::try_from(level)
        .ok()
        .and_then(screeps::controller_downgrade)
        .unwrap_or(0) as i32
}

/// The downgrade timer below which `activateSafeMode` is ERR_TIRED.
pub fn safe_mode_downgrade_line(level: i32) -> i32 {
    controller_downgrade(level) / 2 - screeps::CONTROLLER_DOWNGRADE_SAFEMODE_THRESHOLD as i32
}

/// `attack`, `ranged`: the parts' boosted counts, as `world::power` gives them.
pub fn strike_at(attack: i32, ranged: i32, range: i32) -> i32 {
    match range {
        r if r <= MELEE_RANGE => ATTACK_POWER * attack + RANGED_ATTACK_POWER * ranged,
        r if r <= RANGED_RANGE => RANGED_ATTACK_POWER * ranged,
        _ => 0,
    }
}

pub fn mass_at(ranged: i32, range: i32) -> i32 {
    match range {
        0 | 1 => RANGED_ATTACK_POWER * ranged,
        2 => screeps::RANGED_MASS_ATTACK_POWER_RANGE_2 as i32 * ranged,
        3 => screeps::RANGED_MASS_ATTACK_POWER_RANGE_3 as i32 * ranged,
        _ => 0,
    }
}

/// `power`: the heal parts' boosted count, as `world::power` gives it.
pub fn heal_at(power: i32, range: i32) -> i32 {
    match range {
        r if r <= MELEE_RANGE => HEAL_POWER * power,
        r if r <= RANGED_RANGE => RANGED_HEAL_POWER * power,
        _ => 0,
    }
}

fn tower_power_at(power: i32, range: i32) -> i32 {
    const OPTIMAL: i32 = screeps::TOWER_OPTIMAL_RANGE as i32;
    if range <= OPTIMAL {
        power
    } else {
        let full = f64::from(power);
        let r = range.min(TOWER_FALLOFF_RANGE);
        let gone = full * screeps::TOWER_FALLOFF * f64::from(r - OPTIMAL)
            / f64::from(TOWER_FALLOFF_RANGE - OPTIMAL);
        (full - gone).floor() as i32
    }
}

pub fn tower_attack_at(range: i32) -> i32 {
    tower_power_at(TOWER_POWER_ATTACK, range)
}

/// The engine destroys parts from the head; a part with any hit left acts.
pub fn live_parts(body: &[Part], hits: i32) -> Vec<Part> {
    let live = ((hits + PART_HITS - 1) / PART_HITS) as usize;
    let skip = body.len().saturating_sub(live);
    body[skip..].to_vec()
}

pub fn strike_reach(live: impl Fn(Part) -> bool) -> i32 {
    if live(Part::RangedAttack) {
        RANGED_RANGE
    } else if live(Part::Attack) {
        MELEE_RANGE
    } else {
        0
    }
}

/// The engine puts hits on parts from the head and only a live MOVE moves:
/// one MOVE last walks while any part lives.
pub fn with_move_last(mut body: Vec<Part>) -> Vec<Part> {
    match body.iter().rposition(|p| *p == Part::Move) {
        Some(at) if at < body.len() - 1 => {
            let part = body.remove(at);
            body.push(part);
            body
        }
        _ => body,
    }
}

pub fn part_hits_at(n: usize, hits: i32, i: usize) -> i32 {
    let n = n as i32;
    let i = i as i32;
    PART_HITS.min(hits - PART_HITS * (n - 1 - i)).max(0)
}

/// `calcBodyEffectiveness`. `boosts` is one a part, head first, or empty.
pub fn power_at<'a>(
    body: &'a [Part],
    boosts: &'a [Option<ResourceType>],
    hits: i32,
) -> Box<dyn Fn(Part) -> i32 + 'a> {
    if boosts.is_empty() {
        let live = live_parts(body, hits);
        return Box::new(move |part| part_count_in(&live, part));
    }
    let live = ((hits + PART_HITS - 1) / PART_HITS) as usize;
    let skip = body.len().saturating_sub(live);
    let acting: Vec<(Part, Option<ResourceType>)> = body[skip..]
        .iter()
        .copied()
        .zip(boosts[skip..].iter().copied())
        .collect();
    Box::new(move |part| {
        acting
            .iter()
            .filter(|(p, _)| *p == part)
            .map(|(p, boost)| multiple(*p, *boost))
            .sum()
    })
}

/// The engine's `_applyDamage`.
pub fn damage_taken(body: &[Part], boosts: &[Option<ResourceType>], hits: i32, damage: i32) -> i32 {
    if boosts.is_empty() || damage <= 0 {
        return damage;
    }
    let n = body.len();
    let mut left = f64::from(damage);
    let mut reduced = 0.0;
    for (i, (part, boost)) in body.iter().zip(boosts.iter()).enumerate() {
        if left <= 0.0 {
            break;
        }
        let ratio = damage_ratio(*part, *boost);
        let taken = (f64::from(part_hits_at(n, hits, i)) / ratio).min(left);
        left -= taken;
        reduced += taken * (1.0 - ratio);
    }
    // Math.round, which rounds a half up.
    damage - (reduced + 0.5).floor() as i32
}

pub fn tough_hits(body: &[Part], boosts: &[Option<ResourceType>], hits: i32) -> i32 {
    if boosts.is_empty() {
        return 0;
    }
    let n = body.len();
    body.iter()
        .zip(boosts.iter())
        .enumerate()
        .filter(|(_, (part, boost))| damage_ratio(**part, **boost) < 1.0)
        .map(|(i, _)| part_hits_at(n, hits, i))
        .sum()
}

pub fn damage_for(body: &[Part], boosts: &[Option<ResourceType>], hits: i32, taken: i32) -> i32 {
    if boosts.is_empty() || taken <= 0 {
        return taken;
    }
    let n = body.len();
    let mut left = taken;
    let mut damage = 0.0;
    for (i, (part, boost)) in body.iter().zip(boosts.iter()).enumerate() {
        let take = part_hits_at(n, hits, i).min(left);
        left -= take;
        damage += f64::from(take) / damage_ratio(*part, *boost);
    }
    (damage + f64::from(left) - 1e-9).ceil() as i32
}
