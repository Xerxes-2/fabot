use super::vocabulary::{
    BodyPart, Boost, Compound, Ore, Resource, boost, part_count_in, same_part,
};

/// MAX_CREEP_SIZE: a larger body is refused.
pub const MAX_BODY_PARTS: usize = 50;
pub const HARVEST_PER_WORK: i32 = 2;
pub const CARRY_PART_CAPACITY: i32 = 50;
pub const MINERAL_HARVEST_PER_WORK: i32 = 1;
pub const EXTRACTOR_COOLDOWN: i32 = 5;
/// The intent pass runs before the object pass, so digs land one tick later.
pub const MINERAL_HARVEST_CYCLE: i32 = EXTRACTOR_COOLDOWN + 1;
pub const CREEP_LIFETIME: i32 = 1500;
pub const CLAIM_LIFETIME: i32 = 600;
/// STRONGHOLD_DECAY_TICKS ×1.1: the longest a deployed core stands.
pub const STRONGHOLD_LONGEST_COLLAPSE: i32 = 82_500;
/// CREEP_SPAWN_TIME.
pub const SPAWN_TICKS_PER_PART: i32 = 3;
pub const BOOST_ENERGY_PER_PART: i32 = 20;
pub const BOOST_MINERAL_PER_PART: i32 = 30;
pub const RESERVATION_CAP: i32 = 5000;
/// CONTROLLER_RESERVE. The reservation decays by one a tick, so N parts gain N - 1.
pub const RESERVE_PER_CLAIM: i32 = 1;
pub const CONTAINER_CAPACITY: i32 = 2000;
pub const TERMINAL_MIN_SEND: i32 = 100;

/// The energy a terminal spends sending `amount` across `distance` rooms.
pub fn send_cost(amount: i32, distance: i32) -> i32 {
    (f64::from(amount) * (1.0 - (-f64::from(distance) / 30.0).exp())).ceil() as i32
}
pub const LAB_MINERAL_CAPACITY: i32 = 3000;
pub const LAB_REACTION_AMOUNT: i32 = 5;
pub const LAB_REACTION_RANGE: i32 = 2;
/// Chebyshev over the room grid; the observed room is visible the next tick alone.
pub const OBSERVER_RANGE: i32 = 10;
pub const MELEE_RANGE: i32 = 1;
pub const RANGED_RANGE: i32 = 3;
/// The reach of upgradeController, build and repair.
pub const UPGRADE_RANGE: i32 = 3;
/// A keeper guards the first source, else mineral, this near its lair.
pub const KEEPER_GUARD_RANGE: i32 = 5;

pub fn keeper_body() -> Vec<BodyPart> {
    let mut body = [[BodyPart::Tough; 17].as_slice(), &[BodyPart::Move; 13]].concat();
    body.extend([BodyPart::Attack, BodyPart::RangedAttack].repeat(10));
    body
}
/// The usernames the engine's NPCs carry.
pub const INVADER_USER: &str = "Invader";
pub const KEEPER_USER: &str = "Source Keeper";
pub const ATTACK_POWER: i32 = 30;
pub const RANGED_ATTACK_POWER: i32 = 10;
pub const DISMANTLE_POWER: i32 = 50;
pub const REPAIR_POWER: i32 = 100;
pub const BUILD_POWER: i32 = 5;
pub const HEAL_POWER: i32 = 12;
pub const RANGED_HEAL_POWER: i32 = 4;
pub const TOWER_POWER_ATTACK: i32 = 600;
pub const TOWER_CAPACITY: i32 = 1000;
/// A tower holding less does nothing.
pub const TOWER_ENERGY_COST: i32 = 10;
pub const TOWER_OPTIMAL_RANGE: i32 = 5;
pub const TOWER_FALLOFF_RANGE: i32 = 20;
pub const TOWER_FALLOFF: f64 = 0.75;
pub const PART_HITS: i32 = 100;
/// A source in a neutral room yields half.
pub const HELD_OUTPUT_PER_TICK: i32 = 10;
pub const NEUTRAL_OUTPUT_PER_TICK: i32 = 5;
pub const SOURCE_REGEN_TIME: i32 = 300;
/// UPGRADE_CONTROLLER_POWER.
pub const UPGRADE_DRAIN_PER_WORK: i32 = 1;
/// CONTROLLER_MAX_UPGRADE_PER_TICK at `controllerMaxLevel`, summed over every
/// creep upgrading it; a creep past the tally upgrades and spends nothing.
pub const MAX_UPGRADE_PER_TICK: i32 = 15;
pub const CONTROLLER_MAX_LEVEL: i32 = 8;
/// The fatigue a weighted part makes stepping onto a road, plain and swamp.
pub const ROAD_WEIGHT: i32 = 1;
pub const PLAIN_WEIGHT: i32 = 2;
pub const SWAMP_WEIGHT: i32 = 10;
pub const ROAD_BUILD_COST: i32 = 300;
/// CONSTRUCTION_COST_ROAD_SWAMP_RATIO: a swamp road's build, hits and decay.
pub const ROAD_SWAMP_RATIO: i32 = 5;
/// ROAD_DECAY_AMOUNT every ROAD_DECAY_TIME, on plain.
pub const ROAD_DECAY_AMOUNT: i32 = 100;
pub const ROAD_DECAY_TIME: i32 = 1000;
/// ROAD_WEAROUT: decay-timer ticks a step brings forward, per body part.
pub const ROAD_WEAROUT: i32 = 1;
/// CONTAINER_DECAY every CONTAINER_DECAY_TIME, or _OWNED in an owned room.
pub const CONTAINER_DECAY_AMOUNT: i32 = 5000;
pub const CONTAINER_DECAY_TIME: i32 = 100;
pub const CONTAINER_DECAY_TIME_OWNED: i32 = 500;
pub const CONTAINER_BUILD_COST: i32 = 5000;
pub const TOMBSTONE_DECAY_PER_PART: i32 = 5;
/// A creep aged out is buried on its last tick: its tombstone's creep has this left.
pub const AGED_OUT_TICKS_TO_LIVE: i32 = 1;
pub const RAMPART_DECAY_AMOUNT: i32 = 300;
pub const RAMPART_DECAY_TIME: i32 = 100;
/// REPAIR_COST: 0.01 energy a hit.
pub const REPAIR_HITS_PER_ENERGY: i32 = 100;
/// MAX_CONSTRUCTION_SITES, across every room.
pub const MAX_CONSTRUCTION_SITES: i32 = 100;
pub const ROOM_SIDE: i32 = 50;

pub fn part_cost(part: BodyPart) -> i32 {
    match part {
        BodyPart::Work => 100,
        BodyPart::Carry => 50,
        BodyPart::Move => 50,
        BodyPart::Attack => 80,
        BodyPart::RangedAttack => 150,
        BodyPart::Heal => 250,
        BodyPart::Claim => 600,
        BodyPart::Tough => 10,
    }
}

pub fn body_cost(body: &[BodyPart]) -> i32 {
    body.iter().map(|part| part_cost(*part)).sum()
}

pub fn reagents_of(compound: Compound) -> (Resource, Resource) {
    match compound {
        Compound::Hydroxide => (Resource::Ore(Ore::Hydrogen), Resource::Ore(Ore::Oxygen)),
        Compound::UtriumHydride => (Resource::Ore(Ore::Utrium), Resource::Ore(Ore::Hydrogen)),
        Compound::UtriumAcid => (
            Resource::Compound(Compound::Hydroxide),
            Resource::Compound(Compound::UtriumHydride),
        ),
    }
}

pub fn controller_downgrade(level: i32) -> i32 {
    match level {
        1 => 20_000,
        2 => 10_000,
        3 => 20_000,
        4 => 40_000,
        5 => 80_000,
        6 => 120_000,
        7 => 150_000,
        8 => 200_000,
        _ => 0,
    }
}

/// The downgrade timer below which `activateSafeMode` is ERR_TIRED.
pub fn safe_mode_downgrade_line(level: i32) -> i32 {
    controller_downgrade(level) / 2 - 5_000
}

pub fn mass_attack_power_at(range: i32) -> i32 {
    match range {
        0 | 1 => RANGED_ATTACK_POWER,
        2 => 4,
        3 => 1,
        _ => 0,
    }
}

fn tower_power_at(power: i32, range: i32) -> i32 {
    if range <= TOWER_OPTIMAL_RANGE {
        power
    } else {
        let full = f64::from(power);
        let r = range.min(TOWER_FALLOFF_RANGE);
        let gone = full * TOWER_FALLOFF * f64::from(r - TOWER_OPTIMAL_RANGE)
            / f64::from(TOWER_FALLOFF_RANGE - TOWER_OPTIMAL_RANGE);
        (full - gone).floor() as i32
    }
}

pub fn tower_attack_at(range: i32) -> i32 {
    tower_power_at(TOWER_POWER_ATTACK, range)
}

/// The engine destroys parts from the head; a part with any hit left acts.
pub fn live_parts(body: &[BodyPart], hits: i32) -> Vec<BodyPart> {
    let live = ((hits + PART_HITS - 1) / PART_HITS) as usize;
    let skip = body.len().saturating_sub(live);
    body[skip..].to_vec()
}

/// The engine puts hits on parts from the head and only a live MOVE moves:
/// one MOVE last walks while any part lives.
pub fn with_move_last(mut body: Vec<BodyPart>) -> Vec<BodyPart> {
    match body.iter().rposition(|p| *p == BodyPart::Move) {
        Some(at) if at < body.len() - 1 => {
            let part = body.remove(at);
            body.push(part);
            body
        }
        _ => body,
    }
}

fn part_hits_at(n: usize, hits: i32, i: usize) -> i32 {
    let n = n as i32;
    let i = i as i32;
    PART_HITS.min(hits - PART_HITS * (n - 1 - i)).max(0)
}

/// `calcBodyEffectiveness`. `boosts` is one a part, head first, or empty.
pub fn power_at<'a>(
    body: &'a [BodyPart],
    boosts: &'a [Boost],
    hits: i32,
) -> Box<dyn Fn(BodyPart) -> i32 + 'a> {
    if boosts.is_empty() {
        let live = live_parts(body, hits);
        return Box::new(move |part| part_count_in(&live, part));
    }
    let live = ((hits + PART_HITS - 1) / PART_HITS) as usize;
    let skip = body.len().saturating_sub(live);
    let acting: Vec<(BodyPart, Boost)> = body[skip..]
        .iter()
        .copied()
        .zip(boosts[skip..].iter().copied())
        .collect();
    Box::new(move |part| {
        acting
            .iter()
            .map(|(p, boost)| {
                if same_part(*p, part) {
                    boost::multiple(*p, *boost)
                } else {
                    0
                }
            })
            .sum()
    })
}

/// The engine's `_applyDamage`.
pub fn damage_taken(body: &[BodyPart], boosts: &[Boost], hits: i32, damage: i32) -> i32 {
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
        let ratio = boost::damage_ratio(*part, *boost);
        let taken = (f64::from(part_hits_at(n, hits, i)) / ratio).min(left);
        left -= taken;
        reduced += taken * (1.0 - ratio);
    }
    // Math.round, which rounds a half up.
    damage - (reduced + 0.5).floor() as i32
}

pub fn tough_hits(body: &[BodyPart], boosts: &[Boost], hits: i32) -> i32 {
    if boosts.is_empty() {
        return 0;
    }
    let n = body.len();
    body.iter()
        .zip(boosts.iter())
        .enumerate()
        .map(|(i, (part, boost))| {
            if boost::damage_ratio(*part, *boost) < 1.0 {
                part_hits_at(n, hits, i)
            } else {
                0
            }
        })
        .sum()
}

pub fn damage_for(body: &[BodyPart], boosts: &[Boost], hits: i32, taken: i32) -> i32 {
    if boosts.is_empty() || taken <= 0 {
        return taken;
    }
    let n = body.len();
    let mut left = taken;
    let mut damage = 0.0;
    for (i, (part, boost)) in body.iter().zip(boosts.iter()).enumerate() {
        let take = part_hits_at(n, hits, i).min(left);
        left -= take;
        damage += f64::from(take) / boost::damage_ratio(*part, *boost);
    }
    (damage + f64::from(left) - 1e-9).ceil() as i32
}

const SPAWN_ROW: [i32; 9] = [0, 1, 1, 1, 1, 1, 1, 2, 3];
const EXTENSION_ROW: [i32; 9] = [0, 0, 5, 10, 20, 30, 40, 50, 60];
const LINK_ROW: [i32; 9] = [0, 0, 0, 0, 0, 2, 3, 4, 6];
const WALL_ROW: [i32; 3] = [0, 0, 2500];
const STORAGE_ROW: [i32; 5] = [0, 0, 0, 0, 1];
const TOWER_ROW: [i32; 9] = [0, 0, 0, 1, 1, 2, 2, 3, 6];
const EIGHTH_ROW: [i32; 9] = [0, 0, 0, 0, 0, 0, 0, 0, 1];
const SIXTH_ROW: [i32; 7] = [0, 0, 0, 0, 0, 0, 1];
const LAB_ROW: [i32; 9] = [0, 0, 0, 0, 0, 0, 3, 6, 10];
const SEVENTH_ROW: [i32; 8] = [0, 0, 0, 0, 0, 0, 0, 1];

/// CONTROLLER_STRUCTURES, keyed by the engine's `STRUCTURE_*` string; sites
/// count with standing structures.
pub fn controller_structures(structure_type: &str, level: i32) -> i32 {
    let at = |row: &[i32]| -> i32 {
        if level < 0 {
            0
        } else if level as usize >= row.len() {
            row[row.len() - 1]
        } else {
            row[level as usize]
        }
    };
    match structure_type {
        "spawn" => at(&SPAWN_ROW),
        "extension" => at(&EXTENSION_ROW),
        "link" => at(&LINK_ROW),
        "road" => 2500,
        "constructedWall" | "rampart" => at(&WALL_ROW),
        "storage" => at(&STORAGE_ROW),
        "tower" => at(&TOWER_ROW),
        "observer" | "powerSpawn" | "nuker" => at(&EIGHTH_ROW),
        "extractor" | "terminal" => at(&SIXTH_ROW),
        "lab" => at(&LAB_ROW),
        "container" => 5,
        "factory" => at(&SEVENTH_ROW),
        _ => 0,
    }
}
