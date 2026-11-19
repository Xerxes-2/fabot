use std::collections::{BTreeMap, BTreeSet};

use crate::atlas::Derived;
use crate::fight::shape::{Engagement, Objective};
use fabot_base::declarations::{ColonyPlan, Declarations, Outpost, REACH_HOPS};
use fabot_base::engine;
use fabot_base::intel::Intel;
use fabot_base::tiles;
use fabot_base::vocabulary::Part;
use fabot_base::world::{Kind, RoomName, World};

/// What one intent costs the engine; a body acts about once a tick.
const INTENT_CPU: f64 = 0.2;

pub struct Candidate {
    pub outpost: Outpost,
    pub home: RoomName,
    /// One way, in ticks.
    pub walk: i32,
    /// Another player's bodies work it: ours are lost once a life.
    pub contested: bool,
    /// The operator's row, worked whatever it pays.
    pub declared: bool,
    pub stands: bool,
}

struct Price {
    net: f64,
    spawn: f64,
    cpu: f64,
}

/// Spenders' parts, their draw's carriers too, an energy a tick: measured at RCL5.
const SPEND_PARTS: f64 = 2.0;

fn price(c: &Candidate, capacity: i32) -> Price {
    use Part::{Carry, Claim, Move, Work};
    let life = f64::from(engine::CREEP_LIFETIME);
    let reserver = [Claim, Move];
    let reserved = capacity >= engine::body_cost(&reserver);
    let rate = if reserved {
        engine::HELD_OUTPUT_PER_TICK
    } else {
        engine::NEUTRAL_OUTPUT_PER_TICK
    };
    let n = c.outpost.sources.len() as f64;
    let work = (rate + engine::HARVEST_PER_WORK - 1) / engine::HARVEST_PER_WORK;
    let mut dig = vec![Work; work as usize];
    dig.push(Carry);
    dig.extend(vec![Move; ((work + 2) / 2) as usize]);
    // A new remote has no road: its haulers are 1:1.
    let carry = n * f64::from(rate * (2 * c.walk + 2)) / f64::from(engine::CARRY_PART_CAPACITY);
    let per_body = f64::from((capacity / engine::body_cost(&[Carry, Move])).clamp(1, 25));
    let haulers = (carry / per_body).ceil();

    let mut upkeep = (n * f64::from(engine::body_cost(&dig))
        + carry * f64::from(engine::body_cost(&[Carry, Move])))
        / life;
    let mut parts = n * dig.len() as f64 + 2.0 * carry;
    let mut bodies = n + haulers;
    if reserved {
        let lives = life / f64::from(engine::CLAIM_LIFETIME);
        upkeep += f64::from(engine::body_cost(&reserver)) / f64::from(engine::CLAIM_LIFETIME);
        parts += reserver.len() as f64 * lives;
        bodies += 1.0;
    }
    let lost = if c.contested { upkeep } else { 0.0 };
    Price {
        net: n * f64::from(rate) - upkeep - lost,
        spawn: parts * f64::from(engine::SPAWN_TICKS_PER_PART),
        cpu: bodies * INTENT_CPU,
    }
}

/// A claim, siege or sweep is not tied to the colony serving it, so it holds
/// every colony.
pub fn busy(
    plans: &[ColonyPlan],
    (declarations, claimed): (&Declarations, Option<RoomName>),
    (engagements, preyed, derived): (&[Engagement], &[RoomName], &Derived),
) -> BTreeSet<RoomName> {
    let hunts = |home: RoomName| {
        (derived.tree(home)).is_some_and(|tree| preyed.iter().any(|r| tree.contains(*r)))
    };
    let raising = |room: &RoomName| !derived.colonies.contains(room);
    let operation = (declarations.claims.iter()).any(|c| raising(&c.room))
        || claimed.is_some()
        || !declarations.sieges.is_empty()
        || (engagements.iter()).any(|e| !matches!(e.objective, Objective::Defend));
    let boosting = (declarations.labs())
        .filter(|(_, targets)| !targets.is_empty())
        .map(|(home, _)| home);
    (plans.iter())
        .filter(|p| operation || p.owes || boosting == Some(p.room) || hunts(p.room))
        .map(|p| p.room)
        .collect()
}

pub fn chosen(
    world: &World,
    candidates: Vec<Candidate>,
    (busy, spare): (&BTreeSet<RoomName>, &BTreeMap<RoomName, f64>),
) -> Vec<Outpost> {
    // A young home opens every room that pays at once, as The International does.
    let stocked = |home: RoomName| {
        world.room(home).is_some_and(|r| {
            (r.structures.iter()).any(|s| s.kind == Kind::Storage && s.owner.is_ours())
        })
    };
    // Before a storage the spawns' other half is the spend's.
    let spend = |home: RoomName, net: f64| {
        let ticks = net.max(0.0) * SPEND_PARTS * f64::from(engine::SPAWN_TICKS_PER_PART);
        if stocked(home) { ticks } else { 0.0 }
    };
    let mut spawn_left: BTreeMap<RoomName, f64> = BTreeMap::new();
    for room in world.spawns.iter().map(|s| s.pos.room) {
        let spawns = world.spawns.iter().filter(|s| s.pos.room == room).count() as f64;
        let half = f64::from(engine::CREEP_LIFETIME) * spawns / 2.0;
        let dug = world.room(room).map_or(0, |r| r.sources.len() as i32);
        let left = match spare.get(&room) {
            Some(spare) if stocked(room) => {
                spare - spend(room, f64::from(dug * engine::HELD_OUTPUT_PER_TICK))
            }
            _ => half,
        };
        spawn_left.insert(room, left);
    }
    let mut cpu_left = world.cpu_limit / 2.0;
    let capacity = |home: RoomName| world.room(home).map_or(0, |r| r.energy_capacity);
    let mut priced: Vec<(Price, Candidate)> = (candidates.into_iter())
        .map(|c| (price(&c, capacity(c.home)), c))
        .collect();
    let value = |p: &Price| p.net / p.spawn.max(1.0);
    priced.sort_by(|(pa, a), (pb, b)| {
        (b.declared.cmp(&a.declared))
            .then(b.stands.cmp(&a.stands))
            .then(value(pb).total_cmp(&value(pa)))
            .then(a.outpost.room.cmp(&b.outpost.room))
    });
    let mut out = Vec::new();
    let mut rising: BTreeSet<RoomName> = BTreeSet::new();
    for (p, c) in priced {
        let left = spawn_left.entry(c.home).or_default();
        let spawn = p.spawn + spend(c.home, p.net);
        let unpaid = p.net <= 0.0 || spawn > *left || p.cpu > cpu_left;
        let yields = busy.contains(&c.home) && !c.stands;
        if !c.declared && (unpaid || yields || rising.contains(&c.home)) {
            continue;
        }
        if !c.stands && stocked(c.home) {
            rising.insert(c.home);
        }
        *left -= spawn;
        cpu_left -= p.cpu;
        if !c.declared {
            out.push(c.outpost);
        }
    }
    out
}

pub fn survey(
    world: &World,
    derived: &Derived,
    (declarations, plans, claimed): (&Declarations, &[ColonyPlan], Option<RoomName>),
    intel: &Intel,
) -> Vec<Candidate> {
    let mut by_room: BTreeMap<RoomName, Candidate> = BTreeMap::new();
    let taken = |room: &RoomName| {
        derived.owned.contains(room)
            || declarations.avoid.contains(room)
            || declarations.sieges.contains(room)
            || declarations.claims.iter().any(|c| &c.room == room)
            || claimed == Some(*room)
    };
    for (home, tree) in &derived.trees {
        let Some(room) = world.room(*home) else {
            continue;
        };
        let Some(spawn) = world.spawns.iter().find(|s| s.pos.room == *home) else {
            continue;
        };
        // `ARCHITECTURE.md` §7.2.
        let level = room.controller.as_ref().map_or(0, |c| c.level);
        let banks = (room.structures.iter()).any(|s| s.kind == Kind::Storage && s.owner.is_ours())
            || plans.iter().any(|p| p.room == *home && p.pile.is_some());
        let picks = banks && level >= 2;
        let walk = |hops: i32, outpost: &Outpost| {
            let to = |s: &fabot_base::declarations::Declared| tiles::seam_range(&spawn.pos, &s.pos);
            let ranges: Vec<i32> = outpost.sources.iter().filter_map(to).collect();
            let mean = ranges.iter().sum::<i32>() / (ranges.len() as i32).max(1);
            mean.max(hops * engine::ROOM_SIDE)
        };
        let (hx, hy) = home.coords();
        for dx in -REACH_HOPS..=REACH_HOPS {
            for dy in -REACH_HOPS..=REACH_HOPS {
                let name = RoomName::new(hx + dx, hy + dy);
                let Some(hops) = tree.crossings(name, *home).filter(|h| *h > 0) else {
                    continue;
                };
                let declared = declarations.outposts.iter().find(|o| o.room == name);
                let candidate = match (declared, intel.get(&name)) {
                    (Some(o), _) => Candidate {
                        walk: walk(hops, o),
                        outpost: o.clone(),
                        home: *home,
                        contested: false,
                        declared: true,
                        stands: intel.get(&name).is_some_and(|r| r.contained),
                    },
                    _ if taken(&name) || !picks => continue,
                    (None, Some(row)) if row.sources.is_some() => {
                        let theirs = |user: &String| user != &world.me;
                        let lapsed =
                            |(user, lapse): &(String, i32)| !theirs(user) || *lapse <= world.time;
                        let (Some(controller), Some(sources)) = (&row.controller, &row.sources)
                        else {
                            continue;
                        };
                        let worker = row.worker.as_ref().filter(|u| theirs(u));
                        if sources.is_empty()
                            || row.owner.is_some()
                            || !row.reservation.as_ref().is_none_or(lapsed)
                            || worker.is_some_and(|u| declarations.allies.contains(u))
                        {
                            continue;
                        }
                        let outpost = Outpost {
                            room: name,
                            controller: controller.clone(),
                            sources: sources.clone(),
                        };
                        Candidate {
                            walk: walk(hops, &outpost),
                            outpost,
                            home: *home,
                            contested: worker.is_some(),
                            declared: false,
                            stands: row.contained,
                        }
                    }
                    _ => continue,
                };
                if by_room
                    .get(&name)
                    .is_none_or(|held| held.walk > candidate.walk)
                {
                    by_room.insert(name, candidate);
                }
            }
        }
    }
    by_room.into_values().collect()
}
