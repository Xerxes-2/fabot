use std::collections::{BTreeMap, BTreeSet};

use crate::expand;
use fabot_atlas::{self as atlas, Atlas, Derived, Home};
use fabot_base::declarations::{ColonyPlan, Declarations, FAR_HOPS, Outpost, REACH_HOPS, Stage};
use fabot_base::engine;
use fabot_base::intel::Intel;
use fabot_base::tiles;
use fabot_base::tuning::Tuning;
use fabot_base::vocabulary::{CLAIMER, Part};
use fabot_base::world::{Owner, Pos, RoomName, World};
use fabot_economy::bodies;
use fabot_economy::slots;
use fabot_fight::casting;
use fabot_fight::shape::{Engagement, Objective};

/// What one intent costs the engine; a body acts about once a tick.
const INTENT_CPU: f64 = 0.2;
/// An outpost's containers serve its tenure: in soaks the mean outpost held 11-14k ticks.
const TENURE: f64 = 9_000.0;

pub struct Candidate {
    pub outpost: Outpost,
    pub home: RoomName,
    /// One way, in ticks.
    pub walk: i32,
    /// The path the spawn budget charges: unroaded while young, paved once stocked.
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
    unbuilt: f64,
    reserved: bool,
}

/// Bare or reserved, whichever nets more within `left`: affording a reserver
/// never by itself prices a kept room out. A declared row works reserved.
fn price(c: &Candidate, capacity: i32, left: f64, spend: &dyn Fn(f64) -> f64) -> Price {
    let affords = capacity >= engine::body_cost(&CLAIMER);
    let bare = worked(c, capacity, false, spend);
    match affords.then(|| worked(c, capacity, true, spend)) {
        Some(held)
            if c.declared || (held.spawn <= left, held.net) > (bare.spawn <= left, bare.net) =>
        {
            held
        }
        _ => bare,
    }
}

pub fn net(world: &World, c: &Candidate) -> f64 {
    let capacity = world.room(c.home).map_or(0, |r| r.energy_capacity);
    price(c, capacity, f64::INFINITY, &|_| 0.0).net
}

fn worked(c: &Candidate, capacity: i32, reserved: bool, spend: &dyn Fn(f64) -> f64) -> Price {
    use Part::{Carry, Move};
    let rate = if reserved {
        engine::HELD_OUTPUT_PER_TICK
    } else {
        engine::NEUTRAL_OUTPUT_PER_TICK
    };
    let n = c.outpost.sources.len() as f64;
    let dig = bodies::harvester(false, rate, capacity).unwrap_or_default();
    // A new remote has no road: its haulers are 1:1.
    let pair = [Carry, Move];
    let carry = |walk: i32| engine::carry_parts(n * f64::from(rate * (2 * walk + 2)));
    let (carry, walked) = (carry(c.walk), carry(c.walked));
    let per_body = f64::from((capacity / engine::body_cost(&pair)).clamp(1, 25));

    let mut upkeep = n * slots::upkeep(&dig) + carry * slots::upkeep(&pair);
    let mut spawn = n * slots::spawn_load(&dig) + walked * slots::spawn_load(&pair);
    let mut bodies = n + (carry / per_body).ceil();
    if reserved {
        upkeep += slots::upkeep(&CLAIMER);
        spawn += slots::spawn_load(&CLAIMER);
        bodies += 1.0;
    }
    let lost = if c.contested { upkeep } else { 0.0 };
    let net = n * f64::from(rate) - upkeep - lost;
    // Its haul is priced unroaded, so a road repays itself and is no setup.
    let setup = n * f64::from(engine::CONTAINER_BUILD_COST) / TENURE;
    Price {
        net,
        spawn: spawn + spend(net),
        cpu: bodies * INTENT_CPU,
        unbuilt: if c.stands { 0.0 } else { setup },
        reserved,
    }
}

/// An operation holds only the colonies it casts from: a claim its sponsor; a
/// stronghold while answered, the colonies Fight casts it from at rest, since
/// between waves none is owed; a sweep
/// none (a harasser spawns in a few ticks a life); a Defend its owed casts
/// (`ColonyPlan.owes`).
pub fn busy(
    atlas: &mut Atlas,
    (plans, derived): (&[ColonyPlan], &Derived),
    (declarations, claimed): (&Declarations, Option<&Outpost>),
    engagements: &[Engagement],
) -> BTreeSet<RoomName> {
    let given_up = |room| {
        (atlas::plan_of(declarations, plans, room)).is_some_and(|p| p.stage == Stage::Abandon)
    };
    let mut serving = BTreeSet::new();
    for c in expand::raising(derived, declarations, claimed) {
        let ci = atlas::sponsor(atlas, derived, &given_up, &c.controller.pos);
        serving.extend(ci.map(|i| derived.colonies[i]));
    }
    let fights: BTreeSet<RoomName> = (engagements.iter())
        .filter(|e| e.objective == Objective::Stronghold && !e.unanswered)
        .map(|e| e.room)
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
    let tree = atlas.tree(avoid, FAR_HOPS, room);
    casting::homes(&colonies, |from| tree.crossings(from, room))
}

fn stocked(world: &World, home: RoomName) -> bool {
    world.room(home).is_some_and(|r| r.storage().is_some())
}

pub fn chosen(
    world: &World,
    candidates: Vec<Candidate>,
    (busy, spare): (&BTreeSet<RoomName>, &BTreeMap<RoomName, f64>),
    (tuning, engagements): (&Tuning, &[Engagement]),
) -> Vec<Outpost> {
    // A young home opens every room that pays at once, as The International does.
    let stocked = |home: RoomName| stocked(world, home);
    // Before a storage the spawns' share the outposts leave is the spend's;
    // spend parts (spenders' and their carriers', an energy a tick) measured at RCL5.
    let spend = |home: RoomName, net: f64| {
        let ticks = net.max(0.0) * tuning.outpost_spend_parts * f64::from(engine::spawn_ticks(1));
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
    let priced_in =
        |c: &Candidate, left: f64| price(c, capacity(c.home), left, &|net| spend(c.home, net));
    let mut priced: Vec<(Price, Candidate)> = (candidates.into_iter())
        .map(|c| {
            (
                priced_in(&c, spawn_left.get(&c.home).copied().unwrap_or(0.0)),
                c,
            )
        })
        .collect();
    let value = |p: &Price| (p.net - p.unbuilt) / p.spawn.max(1.0);
    priced.sort_by(|(pa, a), (pb, b)| {
        (b.declared.cmp(&a.declared))
            .then(value(pb).total_cmp(&value(pa)))
            .then(a.outpost.room.cmp(&b.outpost.room))
    });
    let mut out = Vec::new();
    let mut rising: BTreeSet<RoomName> = BTreeSet::new();
    for (_, mut c) in priced {
        let left = spawn_left.entry(c.home).or_default();
        let p = priced_in(&c, *left);
        let unpaid = p.net <= 0.0 || p.spawn > *left || p.cpu > cpu_left;
        // A busy home holds back a new room, not the spawn time it ranks for;
        // a room a Defend stands in is not new, or its owed casts would drop it.
        let defended = (engagements.iter())
            .any(|e| e.room == c.outpost.room && e.objective == Objective::Defend);
        let yields = busy.contains(&c.home) && !c.stands && !defended;
        if !c.declared && (unpaid || rising.contains(&c.home)) {
            continue;
        }
        if !c.stands && stocked(c.home) {
            rising.insert(c.home);
        }
        *left -= p.spawn;
        cpu_left -= p.cpu;
        if !c.declared && !yields {
            c.outpost.reserved = p.reserved;
            out.push(c.outpost);
        }
    }
    out
}

pub fn survey(
    (world, atlas): (&World, &mut Atlas),
    derived: &Derived,
    (declarations, plans, claimed): (&Declarations, &[ColonyPlan], Option<RoomName>),
    intel: &Intel,
) -> Vec<Candidate> {
    let mut by_room: BTreeMap<RoomName, Vec<Candidate>> = BTreeMap::new();
    let taken = |room: &RoomName| {
        derived.owned.contains(room)
            || declarations.avoid.contains(room)
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
                let path = atlas.ticks_bare(!young, REACH_HOPS, at, spawn);
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
                        let (Some(controller), Some(sources)) = (&row.controller, &row.sources)
                        else {
                            continue;
                        };
                        let worker = row.worker.as_ref().filter(|u| *u != &world.me);
                        let ally = |u: &String| {
                            Owner::of_user(u.clone(), &world.me, declarations).is_ally()
                        };
                        if sources.is_empty()
                            || !row.unheld(&world.me, world.time)
                            || worker.is_some_and(ally)
                        {
                            continue;
                        }
                        let outpost = Outpost {
                            reserved: true,
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
                by_room.entry(name).or_default().push(candidate);
            }
        }
    }
    // Charged to the colony Economy hauls it home to; one no colony walks to
    // is no outpost unless the operator declared it.
    let planned = |room| atlas::plan_of(declarations, plans, room);
    (by_room.into_values())
        .filter_map(|homes| {
            let at = homes[0].outpost.controller.pos;
            let server = atlas::server(atlas, derived, &planned, (&at, false));
            let server = server.map(|ci| derived.colonies[ci]);
            let c = (homes.into_iter()).min_by_key(|c| (Some(c.home) != server, c.walk))?;
            (Some(c.home) == server || c.declared).then_some(c)
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use fabot_base::declarations::Declared;
    use fabot_base::world::Id;

    /// A reserved source is dug by the harvester Economy casts, its spare WORK
    /// counted: 6W1C3M at 800 energy, not 5W.
    #[test]
    fn a_reserved_dig_is_priced_at_the_harvester_economy_casts() {
        let room: RoomName = "W1N1".parse().unwrap();
        let at = Declared {
            id: Id::MISREAD,
            pos: Pos::new(room, 25, 25),
        };
        let c = Candidate {
            outpost: Outpost {
                reserved: true,
                room,
                controller: at.clone(),
                sources: vec![at],
            },
            home: room,
            walk: 0,
            walked: 0,
            contested: false,
            declared: false,
            stands: true,
        };
        let haul = f64::from(engine::HELD_OUTPUT_PER_TICK * 2) / 50.0 * 100.0 / 1500.0;
        let upkeep = 800.0 / 1500.0 + haul + 650.0 / 600.0;
        let net = f64::from(engine::HELD_OUTPUT_PER_TICK) - upkeep;
        assert!((worked(&c, 800, true, &|_| 0.0).net - net).abs() < 1e-9);
    }
}
