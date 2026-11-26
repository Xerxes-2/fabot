use std::collections::{BTreeMap, BTreeSet};

use fabot_atlas::{self as atlas, Atlas, Derived};
use fabot_base::{
    boost::{BOOSTS, ORES, ores_in},
    declarations::{ColonyPlan, Declarations, Outpost, REACH_HOPS, Stage},
    engine,
    intel::{self, Intel, Row},
    tiles,
    vocabulary::ResourceType,
    world::{Id, Mineral, Room, RoomName, World},
};
use fabot_layout::{ground, plan};
use screeps::LocalRoomTerrain;

use crate::season;

#[derive(Clone, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
pub struct Expansion {
    pub room: RoomName,
    pub since: i32,
}

/// `ARCHITECTURE.md` §4 `expansion`.
#[derive(Clone, Debug, Default, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(default)]
pub struct Expansions {
    #[serde(flatten)]
    pub in_flight: Option<Expansion>,
    /// `since` the tick it failed.
    pub failed: Vec<Expansion>,
    /// Each kept only while ours: a room claimed again is a new colony.
    pub abandoned: Vec<Expansion>,
}

impl Expansions {
    pub fn abandoning(&self, room: RoomName) -> bool {
        self.abandoned.iter().any(|a| a.room == room)
    }
}

pub struct Around<'a> {
    pub rivals: &'a [(RoomName, i32)],
    pub outposts: &'a [Outpost],
    /// The colonies an operation holds (`outposts::busy`).
    pub busy: &'a BTreeSet<RoomName>,
    pub fought: &'a BTreeSet<RoomName>,
    pub stocks: &'a BTreeMap<RoomName, i32>,
}

/// Candidates are planned on a due tick, not each tick: a plan is a Layout draw.
const DUE: i32 = 100;
/// A storage first stands at RCL4.
const SPONSOR_LEVEL: i32 = 4;
/// The owner's.
const CLAIM_DEADLINE: i32 = 5 * engine::CREEP_LIFETIME;
/// About twice the claim to storage measured two crossings off (21k ticks).
const RAISE_DEADLINE: i32 = 40_000;
/// A fresh failure near a room outweighs the most a room scores (its dig and
/// four neighbours'), fading over this (Hivemind's).
const FORGET: i32 = 100_000;

/// What a room is worth holding, in energy a tick.
pub struct Facts {
    pub sources: usize,
    pub free_near: usize,
    pub crossings: i32,
    pub worth: f64,
    /// Failures in or beside it, each fading as it ages.
    pub failed: f64,
    pub mineral: f64,
}

/// A simple rule shaped as its priced replacement (`AGENTS.md`).
pub fn score(f: &Facts) -> f64 {
    let held = f64::from(engine::HELD_OUTPUT_PER_TICK);
    held * f.sources as f64 + held / 2.0 * f.free_near as f64 - f64::from(f.crossings) + f.worth
        - 6.0 * held * f.failed
        + f.mineral
}

fn demand(ore: usize) -> i32 {
    BOOSTS.iter().map(|(boost, rank)| rank * ores_in(*boost)[ore]).sum()
}

/// The most demanded mineral's first copy is a source's dig, the flat bonus
/// it replaced; copies fall off as Hivemind's 1/(held + 1).
pub fn mineral_worth(mineral: ResourceType, held: usize) -> f64 {
    let Some(ore) = ORES.iter().position(|o| *o == mineral) else {
        return 0.0;
    };
    let most = (0..ORES.len()).map(demand).max().unwrap_or(1);
    f64::from(engine::HELD_OUTPUT_PER_TICK) * f64::from(demand(ore))
        / f64::from(most)
        / (held + 1) as f64
}

fn free(world: &World, row: &Row) -> bool {
    row.unheld(&world.me, world.time) && row.worker.as_ref().is_none_or(|u| u == &world.me)
}

fn apart(owned: &BTreeSet<RoomName>, room: RoomName) -> bool {
    owned.iter().all(|c| tiles::rooms_apart(*c, room) >= 2)
}

fn copies(
    world: &World,
    owned: &BTreeSet<RoomName>,
    ore: ResourceType,
    but: Option<RoomName>,
) -> usize {
    (owned.iter().filter(|c| Some(**c) != but))
        .filter_map(|c| world.room(*c))
        .filter(|r| r.minerals.iter().any(|m| m.resource == Some(ore)))
        .count()
}

pub fn claimable(
    world: &World,
    (owned, rivals): (&BTreeSet<RoomName>, &[(RoomName, i32)]),
    declarations: &Declarations,
    (name, row): (RoomName, &Row),
) -> bool {
    let threat = declarations.tuning.expand_threat_rooms;
    row.controller.is_some()
        && row.sources.as_ref().is_some_and(|s| s.len() >= 2)
        && free(world, row)
        && apart(owned, name)
        && !declarations.avoid.contains(&name)
        && !world.closed.contains(&name)
        // A rival this near raids a young room before its towers stand.
        && !(rivals.iter()).any(|(t, _)| tiles::rooms_apart(*t, name) <= threat)
}

pub fn facts(
    world: &World,
    (owned, worked): (&BTreeSet<RoomName>, &dyn Fn(RoomName) -> bool),
    intel: &Intel,
    (name, row): (RoomName, &Row),
    (crossings, failed): (i32, f64),
) -> Facts {
    let mut free_near = 0;
    for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
        let Some(next) = name.offset(dx, dy) else {
            continue;
        };
        if let Some(r) = (intel.get(&next)).filter(|r| r.controller.is_some() && free(world, r))
            && apart(owned, next)
            && !worked(next)
        {
            free_near += r.sources.as_ref().map_or(0, Vec::len);
        }
    }
    Facts {
        sources: row.sources.as_ref().map_or(0, Vec::len),
        free_near,
        crossings,
        worth: season::worth(name),
        failed,
        mineral: (row.mineral)
            .map_or(0.0, |(m, _)| mineral_worth(m, copies(world, owned, m, None))),
    }
}

/// One an operation holds (`outposts::busy`) sponsors nothing.
fn spare(world: &World, p: &ColonyPlan, around: &Around) -> Option<i32> {
    let stock = around.stocks.get(&p.room)?;
    let level = (world.room(p.room)?.controller.as_ref()).map_or(0, |c| c.level);
    (p.stage == Stage::Exploit && !around.busy.contains(&p.room) && level >= SPONSOR_LEVEL)
        .then(|| stock - p.reserve)
}

/// `margin`: the stock past its reserve a sponsor carries a young room on.
fn sponsors(world: &World, plans: &[ColonyPlan], around: &Around, margin: i32) -> Vec<RoomName> {
    (plans.iter())
        .filter(|p| spare(world, p, around).is_some_and(|s| s >= margin))
        .map(|p| p.room)
        .collect()
}

/// The claims not yet a colony: the operator's and `claim`, an expansion's.
pub fn raising<'a>(
    derived: &'a Derived,
    declarations: &'a Declarations,
    claim: Option<&'a Outpost>,
) -> impl Iterator<Item = &'a Outpost> {
    (declarations.claims.iter().chain(claim)).filter(|c| !derived.colonies.contains(&c.room))
}

/// One claim in flight at a time (owner, 2026-11-13).
pub fn may_start(derived: &Derived, declarations: &Declarations, expansion: &Expansions) -> bool {
    expansion.in_flight.is_none() && raising(derived, declarations, None).next().is_none()
}

/// A refill past the gate keeps the stock off its line between due ticks (§5).
pub fn book(
    world: &World,
    derived: &Derived,
    plans: &mut [ColonyPlan],
    (around, margin): (&Around, i32),
    may_start: bool,
) {
    if derived.capped || !may_start {
        return;
    }
    let richest = (plans.iter_mut())
        .filter_map(|p| Some((spare(world, p, around)?, std::cmp::Reverse(p.room), p)))
        .max_by_key(|(s, room, _)| (*s, *room));
    if let Some((_, _, p)) = richest {
        p.reserve += margin + world.room(p.room).map_or(0, |r| r.energy_capacity);
    }
}

/// Layout plans the room before it is claimed: one it cannot fit is never
/// taken (The International's).
pub fn fits(world: &World, outpost: &Outpost, intel: &Intel, terrain: &LocalRoomTerrain) -> bool {
    let mut room = intel::blind(outpost, intel, world, &Declarations::default());
    room.terrain = Some(terrain);
    // Its rock walls the ground Layout plans once the room is ours.
    let rock = intel.get(&outpost.room).and_then(|r| r.mineral);
    room.minerals.extend(rock.and_then(|(resource, at)| {
        Some(Mineral {
            id: Id::of_engine("mineral"),
            pos: tiles::to_pos(outpost.room, at?),
            resource: Some(resource),
            amount: 0,
        })
    }));
    (ground::of_room(&room).and_then(|g| plan::first_of(&world.rules, &g)))
        .is_some_and(|p| p.unfit.is_empty())
}

#[derive(Debug, PartialEq, Eq)]
enum Fate {
    Raised,
    Failed,
    Flying,
}

fn fate(age: i32, owned: bool, lost: bool, stored: bool) -> Fate {
    let deadline = if owned { RAISE_DEADLINE } else { CLAIM_DEADLINE };
    match () {
        _ if stored => Fate::Raised,
        _ if lost || age > deadline => Fate::Failed,
        _ => Fate::Flying,
    }
}

pub fn step<'t>(
    (world, atlas): (&World, &Atlas),
    derived: &Derived,
    (declarations, plans): (&Declarations, &[ColonyPlan]),
    around: &Around,
    intel: &Intel,
    terrain: &dyn Fn(RoomName) -> Option<&'t LocalRoomTerrain>,
    expansion: &mut Expansions,
) {
    let Around { rivals, outposts, busy, fought, .. } = *around;
    let raising = raising(derived, declarations, None).next().is_some();
    let tuning = &declarations.tuning;
    (expansion.failed).retain(|f| world.time - f.since < FORGET);
    (expansion.abandoned).retain(|a| derived.owned.contains(&a.room));
    let given =
        |room: RoomName, e: &Expansions| e.abandoning(room) || declarations.abandoning(room);
    let walked = |o: &Outpost, e: &Expansions| {
        atlas::sponsor(atlas, derived, &|r| given(r, e), &o.controller.pos).is_some()
    };
    // Never the last colony nor one in a fight (owner).
    let may_give_up = |room: RoomName, expansion: &Expansions| {
        (derived.colonies.iter()).filter(|c| !given(**c, expansion)).count() > 1
            && !fought.contains(&room)
            && !given(room, expansion)
    };
    let give_up = |room: RoomName, expansion: &mut Expansions| {
        let e = Expansion { room, since: world.time };
        expansion.failed.push(e.clone());
        expansion.abandoned.push(e);
    };
    for c in &derived.colonies {
        if season::spent(*c) && may_give_up(*c, expansion) {
            give_up(*c, expansion);
        }
    }
    if let Some(e) = expansion.in_flight.take() {
        let owned = derived.owned.contains(&e.room);
        let theirs =
            (intel.get(&e.room).and_then(|r| r.owner.as_ref())).is_some_and(|o| o != &world.me);
        let stored = derived.home(e.room).is_some_and(|h| !h.young);
        // Nothing holds it once its safe mode is gone and no tower is up; a
        // cooldown tells a spent safe mode from RCL2's grant not yet given.
        let bare = owned
            && world.room(e.room).is_some_and(|r| {
                (r.controller.as_ref()).is_some_and(|c| !c.safe() && c.safe_mode_cooldown > 0)
                    && !(world.rules.towers(r)).any(|s| s.owner.is_ours())
            });
        let unwalked = || claim(intel, derived, Some(&e)).is_some_and(|o| !walked(&o, expansion));
        let lost = theirs || bare || (!owned && unwalked());
        match fate(world.time - e.since, owned, lost, stored) {
            Fate::Failed if owned && may_give_up(e.room, expansion) => give_up(e.room, expansion),
            // Given up once its fight is over.
            Fate::Failed if owned => expansion.in_flight = Some(e),
            Fate::Failed => expansion.failed.push(Expansion { since: world.time, ..e }),
            // One not yet owned yields to the operator's claim in flight.
            Fate::Flying if owned || !raising => expansion.in_flight = Some(e),
            _ => (),
        }
        return;
    }
    // The bucket fills only while the ticks run under the limit (Hivemind's CPU headroom).
    if world.time % DUE != 0
        || !may_start(derived, declarations, expansion)
        || world.bucket < tuning.expand_bucket
    {
        return;
    }
    let capped = derived.capped;
    // Any failure or give-up holds the next give-up off (Hivemind's abandonWeakRooms).
    if capped && (expansion.failed.iter()).any(|f| world.time - f.since < FORGET) {
        return;
    }
    let failed = |room: RoomName| -> f64 {
        (expansion.failed.iter())
            .filter(|f| tiles::rooms_apart(f.room, room) <= 1)
            .map(|f| 1.0 - f64::from(world.time - f.since) / f64::from(FORGET))
            .sum()
    };
    let sponsors = sponsors(world, plans, around, tuning.sponsor_margin);
    let worked =
        |room: RoomName| (declarations.outposts.iter().chain(outposts)).any(|o| o.room == room);
    let mut scored: Vec<(f64, Outpost)> = Vec::new();
    for (home, tree) in derived.trees.iter().filter(|(h, _)| sponsors.contains(h)) {
        for dx in -REACH_HOPS..=REACH_HOPS {
            for dy in -REACH_HOPS..=REACH_HOPS {
                let Some(name) = home.offset(dx, dy) else {
                    continue;
                };
                let (Some(crossings), Some(row)) = (tree.crossings(name, *home), intel.get(&name))
                else {
                    continue;
                };
                let (Some(controller), Some(sources)) = (&row.controller, &row.sources) else {
                    continue;
                };
                if !claimable(world, (&derived.owned, rivals), declarations, (name, row))
                    || scored.iter().any(|(_, o)| o.room == name)
                {
                    continue;
                }
                let ours = (&derived.owned, &worked as &dyn Fn(RoomName) -> bool);
                let facts = facts(world, ours, intel, (name, row), (crossings, failed(name)));
                let outpost = Outpost {
                    reserved: true,
                    room: name,
                    controller: controller.clone(),
                    sources: sources.clone(),
                };
                // A room that does not pay is never declared (§7.2).
                let score = score(&facts);
                if score > 0.0 {
                    scored.push((score, outpost));
                }
            }
        }
    }
    scored.sort_by(|(a, x), (b, y)| b.total_cmp(a).then(x.room.cmp(&y.room)));
    let best = (scored.into_iter()).find(|(_, o)| {
        walked(o, expansion) && terrain(o.room).is_some_and(|t| fits(world, o, intel, t))
    });
    if !capped {
        expansion.in_flight = best.map(|(_, o)| Expansion { room: o.room, since: world.time });
        return;
    }
    let held_score = |room: &Room| {
        let worked = |o: &&Outpost| tiles::rooms_apart(o.room, room.name) == 1;
        let near = (declarations.outposts.iter().chain(outposts)).filter(worked);
        score(&Facts {
            sources: room.sources.len(),
            free_near: near.map(|o| o.sources.len()).sum(),
            crossings: 0,
            worth: season::worth(room.name),
            failed: 0.0,
            mineral: (room.minerals.first().and_then(|m| m.resource)).map_or(0.0, |m| {
                mineral_worth(m, copies(world, &derived.owned, m, Some(room.name)))
            }),
        })
    };
    // Before RCL6's extractor and terminal a colony is cheap to give up for a
    // better room; past them it holds what a new one would take a long climb to.
    let worst = (derived.colonies.iter().filter_map(|c| world.room(*c)))
        .filter(|r| r.controller.as_ref().is_some_and(|c| c.level <= tuning.prune_level))
        .filter(|r| !busy.contains(&r.name) && may_give_up(r.name, expansion))
        .map(|r| (held_score(r), r.name))
        .min_by(|(a, x), (b, y)| a.total_cmp(b).then(x.cmp(y)));
    let held = f64::from(engine::HELD_OUTPUT_PER_TICK);
    if let (Some((beat, _)), Some((kept, room))) = (best, worst)
        && beat > kept + held
    {
        give_up(room, expansion);
    }
}

pub fn claim(intel: &Intel, derived: &Derived, held: Option<&Expansion>) -> Option<Outpost> {
    let e = held.filter(|e| !derived.colonies.contains(&e.room))?;
    let row = intel.get(&e.room)?;
    Some(Outpost {
        reserved: true,
        room: e.room,
        controller: row.controller.clone()?,
        sources: row.sources.clone()?,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    /// S11's E24S11 fits only without its mineral, E26S7 only with it.
    #[test]
    fn the_fit_check_plans_the_ground_layout_plans_once_ours() {
        use fabot_base::world::{Controller, Owner, Pos, Rules, Source};
        let text = include_str!("../../scripts/scenario/worlds/season-outpost-ring.json");
        let map: serde_json::Value = serde_json::from_str(text).unwrap();
        let rooms = map["rooms"].as_array().unwrap();
        let terrains: Vec<LocalRoomTerrain> = (rooms.iter())
            .map(|r| {
                let rows = r["terrain"].as_array().unwrap().iter();
                let bits = rows.flat_map(|row| row.as_str().unwrap().bytes());
                let bits: Vec<u8> = bits.map(|b| b - b'0').collect();
                LocalRoomTerrain::new_from_bits(bits.try_into().unwrap())
            })
            .collect();
        let mut world = World {
            time: 1,
            me: "us".into(),
            cpu_limit: 100.0,
            bucket: 10_000,
            rooms: Vec::new(),
            spawns: Vec::new(),
            creeps: Vec::new(),
            our_sites: 0,
            rooms_allowed: 1,
            closed: BTreeSet::new(),
            attacks: Vec::new(),
            destroyed: Vec::new(),
            rules: Rules::vanilla(),
        };
        for (r, terrain) in rooms.iter().zip(&terrains) {
            let name: RoomName = r["name"].as_str().unwrap().parse().unwrap();
            let mut room = Room { terrain: Some(terrain), ..Room::empty(name) };
            for (i, o) in r["objects"].as_array().unwrap().iter().enumerate() {
                let id = Id::of_engine(&format!("o{i}"));
                let xy = |k: &str| o[k].as_i64().unwrap() as i32;
                let pos = Pos::new(name, xy("x"), xy("y"));
                match o["type"].as_str().unwrap() {
                    "controller" => {
                        room.controller = Some(Controller {
                            id,
                            pos,
                            owner: Owner::Nobody,
                            level: 0,
                            progress: 0,
                            progress_total: 0,
                            ticks_to_downgrade: 0,
                            reservation: None,
                            sign: None,
                            safe_mode: 0,
                            safe_mode_available: 0,
                            safe_mode_cooldown: 0,
                            upgrade_blocked: 0,
                        })
                    }
                    "source" => room.sources.push(Source {
                        id,
                        pos,
                        energy: 0,
                        energy_capacity: 0,
                        ticks_to_regeneration: 0,
                    }),
                    "mineral" => room.minerals.push(Mineral {
                        id,
                        pos,
                        resource: o["mineralType"].as_str().unwrap().parse().ok(),
                        amount: 0,
                    }),
                    _ => {}
                }
            }
            world.rooms.push(room);
        }
        let mut intel = Intel::default();
        crate::intel::step(&world, &fabot_atlas::Atlas::default(), &mut intel);
        let mut checked = 0;
        for room in world.rooms.iter().filter(|r| r.controller.is_some()) {
            let row = &intel[&room.name];
            let outpost = Outpost {
                reserved: true,
                room: room.name,
                controller: row.controller.clone().unwrap(),
                sources: row.sources.clone().unwrap(),
            };
            let drawn = ground::of_room(room).and_then(|g| plan::first_of(&world.rules, &g));
            let layout = drawn.is_some_and(|p| p.unfit.is_empty());
            let terrain = room.terrain.unwrap();
            assert_eq!(fits(&world, &outpost, &intel, terrain), layout, "{}", room.name);
            checked += 1;
        }
        assert_eq!(checked, 20);
    }

    #[test]
    fn a_claim_fails_past_its_deadline_or_to_another_player() {
        assert_eq!(fate(CLAIM_DEADLINE + 1, false, false, false), Fate::Failed);
        assert_eq!(fate(CLAIM_DEADLINE + 1, true, false, false), Fate::Flying);
        assert_eq!(fate(RAISE_DEADLINE + 1, true, false, false), Fate::Failed);
        assert_eq!(fate(RAISE_DEADLINE + 1, true, false, true), Fate::Raised);
        assert_eq!(fate(0, false, true, false), Fate::Failed);
    }

    #[test]
    fn a_scarce_mineral_outbids_another_copy_of_one_we_hold() {
        let worth = |o: ResourceType, held: usize| mineral_worth(o, held);
        assert!(worth(ResourceType::Oxygen, 0) > worth(ResourceType::Hydrogen, 0));
        assert!(worth(ResourceType::Lemergium, 0) > worth(ResourceType::Utrium, 0));
        assert!(worth(ResourceType::Utrium, 1) < worth(ResourceType::Lemergium, 0));
        assert!(worth(ResourceType::Oxygen, 1) < worth(ResourceType::Oxygen, 0));
        assert_eq!(worth(ResourceType::Oxygen, 0), f64::from(engine::HELD_OUTPUT_PER_TICK));
        let have = Facts {
            sources: 2,
            free_near: 3,
            crossings: 2,
            worth: 0.0,
            failed: 0.0,
            mineral: worth(ResourceType::Utrium, 1),
        };
        let lack = Facts { mineral: worth(ResourceType::Lemergium, 0), ..have };
        assert!(score(&lack) > score(&have));
    }

    #[test]
    fn season_worth_outbids_dig() {
        let near =
            Facts { sources: 2, free_near: 4, crossings: 2, worth: 0.0, failed: 0.0, mineral: 0.0 };
        let scoring = Facts { free_near: 0, crossings: 3, worth: 25.0, ..near };
        assert!(score(&near) > score(&Facts { worth: 0.0, ..scoring }));
        assert!(score(&scoring) > score(&near));
    }
}
