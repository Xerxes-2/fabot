use std::collections::{BTreeMap, BTreeSet};

use crate::expand;
use fabot_atlas::{self as atlas, Atlas, Derived, Home};
use fabot_base::declarations::{
    ColonyPlan, Declarations, OpKind, Operation, Outpost, REACH_HOPS, SIEGE_HOPS, Stage,
};
use fabot_base::engine;
use fabot_base::intel::Intel;
use fabot_base::tiles;
use fabot_base::tuning::Tuning;
use fabot_base::vocabulary::Part;
use fabot_base::world::{Pos, RoomName, StructureType, World};
use fabot_fight::casting;
use fabot_fight::shape::{Engagement, Objective};

/// What one intent costs the engine; a body acts about once a tick.
const INTENT_CPU: f64 = 0.2;

pub struct Candidate {
    pub outpost: Outpost,
    pub home: RoomName,
    /// One way, in ticks.
    pub walk: i32,
    /// Before a storage, the unroaded path the young home's budget charges.
    pub walked: i32,
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

fn price(c: &Candidate, capacity: i32, walk: i32) -> Price {
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
    let carry = n * f64::from(rate * (2 * walk + 2)) / f64::from(engine::CARRY_PART_CAPACITY);
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

/// An operation holds only the colonies it casts from: a claim its sponsor; a
/// siege from its declaration, and a sweep while its engagement is answered (a
/// Sweep row is every room intel saw an enemy in), the colonies Fight casts it
/// from at rest, since between waves none is owed; a Defend its owed casts
/// (`ColonyPlan.owes`).
pub fn busy(
    atlas: &mut Atlas,
    (plans, derived): (&[ColonyPlan], &Derived),
    (declarations, claimed): (&Declarations, Option<&Outpost>),
    (operations, engagements): (&[Operation], &[Engagement]),
) -> BTreeSet<RoomName> {
    // Merged as `Declarations::planned` merges them, so Economy picks the same.
    let given_up = |room| {
        (declarations.colonies.iter().chain(plans))
            .find(|p| p.room == room)
            .is_some_and(|p| p.stage == Stage::Abandon)
    };
    let mut serving = BTreeSet::new();
    for c in expand::raising(derived, declarations, claimed) {
        let ci = atlas::sponsor(atlas, derived, &given_up, &c.controller.pos);
        serving.extend(ci.map(|i| derived.colonies[i]));
    }
    let fights: BTreeSet<RoomName> = (operations.iter())
        .filter(|o| o.kind == OpKind::Siege)
        .map(|o| o.room)
        .chain(
            (engagements.iter())
                .filter(|e| e.objective != Objective::Defend && !e.unanswered)
                .map(|e| e.room),
        )
        .collect();
    for room in fights {
        serving.extend(casters(atlas, derived, &declarations.avoid, room));
    }
    (plans.iter())
        .filter(|p| serving.contains(&p.room) || p.owes)
        .map(|p| p.room)
        .collect()
}

/// The colonies Fight casts a fight in `room` from at rest; a home's own
/// fight only from it (`Scene::colonies_of`).
pub fn casters(
    atlas: &Atlas,
    derived: &Derived,
    avoid: &BTreeSet<RoomName>,
    room: RoomName,
) -> Vec<RoomName> {
    if derived.colonies.contains(&room) {
        return vec![room];
    }
    let colonies: Vec<(RoomName, i32)> = (derived.colonies.iter().zip(&derived.homes))
        .map(|(r, h)| (*r, h.capacity))
        .collect();
    let tree = atlas.tree(avoid, SIEGE_HOPS, room);
    casting::homes(&colonies, |from| tree.crossings(from, room))
}

fn stocked(world: &World, home: RoomName) -> bool {
    world.room(home).is_some_and(|r| {
        (r.structures.iter()).any(|s| s.kind == StructureType::Storage && s.owner.is_ours())
    })
}

pub fn chosen(
    world: &World,
    candidates: Vec<Candidate>,
    (busy, spare): (&BTreeSet<RoomName>, &BTreeMap<RoomName, f64>),
    tuning: &Tuning,
) -> Vec<Outpost> {
    // A young home opens every room that pays at once, as The International does.
    let stocked = |home: RoomName| stocked(world, home);
    // Before a storage the spawns' share the outposts leave is the spend's;
    // spend parts (spenders' and their carriers', an energy a tick) measured at RCL5.
    let spend = |home: RoomName, net: f64| {
        let ticks =
            net.max(0.0) * tuning.outpost_spend_parts * f64::from(engine::SPAWN_TICKS_PER_PART);
        if stocked(home) { ticks } else { 0.0 }
    };
    let mut spawn_left: BTreeMap<RoomName, f64> = BTreeMap::new();
    for room in world.spawns.iter().map(|s| s.pos.room) {
        let spawns = world
            .working_spawns()
            .filter(|s| s.pos.room == room)
            .count() as f64;
        let share = f64::from(engine::CREEP_LIFETIME) * spawns * tuning.outpost_spawn_share;
        let dug = world.room(room).map_or(0, |r| r.sources.len() as i32);
        let left = match spare.get(&room) {
            Some(spare) if stocked(room) => {
                spare - spend(room, f64::from(dug * engine::HELD_OUTPUT_PER_TICK))
            }
            _ => share,
        };
        spawn_left.insert(room, left);
    }
    let mut cpu_left = world.cpu_limit * tuning.outpost_cpu_share;
    let capacity = |home: RoomName| world.room(home).map_or(0, |r| r.energy_capacity);
    let mut priced: Vec<(Price, Candidate)> = (candidates.into_iter())
        .map(|c| (price(&c, capacity(c.home), c.walk), c))
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
        let walked = price(&c, capacity(c.home), c.walked).spawn;
        let spawn = walked + spend(c.home, p.net);
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
    (world, atlas): (&World, &Atlas),
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
        let Some(h @ Home { spawn, .. }) = derived.home(*home) else {
            continue;
        };
        // `ARCHITECTURE.md` §7.2.
        let banks = plans.iter().any(|p| p.room == *home && h.banks(p));
        let young = !stocked(world, *home);
        let mean_of = |hops: i32, outpost: &Outpost, along: &dyn Fn(&Pos) -> Option<i32>| {
            let ranges: Vec<i32> = (outpost.sources.iter())
                .filter_map(|s| along(&s.pos))
                .collect();
            let mean = ranges.iter().sum::<i32>() / (ranges.len() as i32).max(1);
            mean.max(hops * engine::ROOM_SIDE)
        };
        let straight = |at: &Pos| tiles::seam_range(spawn, at);
        let walk = |hops: i32, outpost: &Outpost| mean_of(hops, outpost, &straight);
        let walked = |hops: i32, outpost: &Outpost| {
            let path = |at: &Pos| {
                let line = straight(at)?;
                let path = young
                    .then(|| atlas.ticks_bare(REACH_HOPS, at, spawn))
                    .flatten();
                Some(path.map_or(line, |w| w.max(line)))
            };
            mean_of(hops, outpost, &path)
        };
        for dx in -REACH_HOPS..=REACH_HOPS {
            for dy in -REACH_HOPS..=REACH_HOPS {
                let Some(name) = home.offset(dx, dy) else {
                    continue;
                };
                let Some(hops) = tree.crossings(name, *home).filter(|h| *h > 0) else {
                    continue;
                };
                let declared = declarations.outposts.iter().find(|o| o.room == name);
                let candidate = match (declared, intel.get(&name)) {
                    (Some(o), _) => Candidate {
                        walk: walk(hops, o),
                        walked: walked(hops, o),
                        outpost: o.clone(),
                        home: *home,
                        contested: false,
                        declared: true,
                        stands: intel.get(&name).is_some_and(|r| r.contained),
                    },
                    _ if taken(&name) || !banks => continue,
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
                            walked: walked(hops, &outpost),
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
