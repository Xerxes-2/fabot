use std::collections::{BTreeMap, BTreeSet};

use super::intel::Intel;
use crate::core::engine;
use crate::core::fight::shape::{Engagement, Objective};
use crate::core::tick::{ColonyPlan, Declarations, Derived, Outpost, REACH_HOPS};
use crate::core::tiles;
use crate::core::vocabulary::BodyPart;
use crate::core::world::{Kind, Room, RoomName, World};

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

fn price(c: &Candidate, capacity: i32) -> Price {
    use BodyPart::{Carry, Claim, Move, Work};
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
    declarations: &Declarations,
    (engagements, preyed, derived): (&[Engagement], &[RoomName], &Derived),
) -> BTreeSet<RoomName> {
    let hunts = |home: RoomName| {
        (derived.trees.iter())
            .any(|(h, tree)| *h == home && preyed.iter().any(|r| tree.contains(*r)))
    };
    let raising = |room: &RoomName| !derived.colonies.contains(room);
    let operation = (declarations.claims.iter()).any(|c| raising(&c.room))
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

/// The level rush ends with RCL6's extractor and terminal; past it dig is the
/// score, not upgrade.
fn outpost_spawn_ticks(home: Option<&Room>) -> f64 {
    let life = f64::from(engine::CREEP_LIFETIME);
    let built = |is: fn(&Kind) -> bool| {
        home.is_some_and(|r| (r.structures.iter()).any(|s| is(&s.kind) && s.owner.is_ours()))
    };
    if built(Kind::is_extractor) && built(Kind::is_terminal) {
        life
    } else {
        life / 2.0
    }
}

pub fn chosen(
    world: &World,
    candidates: Vec<Candidate>,
    busy: &BTreeSet<RoomName>,
) -> Vec<Outpost> {
    let mut spawn_left: BTreeMap<RoomName, f64> = BTreeMap::new();
    for s in &world.spawns {
        *spawn_left.entry(s.pos.room).or_default() += outpost_spawn_ticks(world.room(s.pos.room));
    }
    let mut cpu_left = world.cpu_limit / 2.0;
    let capacity = |home: RoomName| world.room(home).map_or(0, |r| r.energy_capacity);
    let mut priced: Vec<(Candidate, Price)> = (candidates.into_iter())
        .map(|c| {
            let p = price(&c, capacity(c.home));
            (c, p)
        })
        .collect();
    let value = |p: &Price| p.net / p.spawn.max(1.0);
    priced.sort_by(|(a, pa), (b, pb)| {
        (b.declared.cmp(&a.declared))
            .then(b.stands.cmp(&a.stands))
            .then(value(pb).total_cmp(&value(pa)))
            .then(a.outpost.room.cmp(&b.outpost.room))
    });
    let mut out = Vec::new();
    let mut rising: BTreeSet<RoomName> = BTreeSet::new();
    for (c, p) in priced {
        let left = spawn_left.entry(c.home).or_default();
        let unpaid = p.net <= 0.0 || p.spawn > *left || p.cpu > cpu_left;
        let yields = busy.contains(&c.home) && !c.stands;
        if !c.declared && (unpaid || yields || rising.contains(&c.home)) {
            continue;
        }
        if !c.stands {
            rising.insert(c.home);
        }
        *left -= p.spawn;
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
    (declarations, plans): (&Declarations, &[ColonyPlan]),
    intel: &Intel,
) -> Vec<Candidate> {
    let mut by_room: BTreeMap<RoomName, Candidate> = BTreeMap::new();
    let taken = |room: &RoomName| {
        derived.owned.contains(room)
            || declarations.avoid.contains(room)
            || declarations.sieges.contains(room)
            || declarations.claims.iter().any(|c| &c.room == room)
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
        let banks = (room.structures.iter()).any(|s| s.kind.is_storage() && s.owner.is_ours())
            || plans.iter().any(|p| p.room == *home && p.pile.is_some());
        let picks = banks && (2..engine::CONTROLLER_MAX_LEVEL).contains(&level);
        let walk = |hops: i32, outpost: &Outpost| {
            let to = |s: &crate::core::tick::Declared| tiles::seam_range(&spawn.pos, &s.pos);
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
                match by_room.get(&name) {
                    Some(held) if held.walk <= candidate.walk => (),
                    _ => {
                        by_room.insert(name, candidate);
                    }
                }
            }
        }
    }
    by_room.into_values().collect()
}
