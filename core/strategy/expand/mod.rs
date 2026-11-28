use std::collections::{BTreeMap, BTreeSet};

use fabot_atlas::{self as atlas, Atlas, Derived};
use fabot_base::{
    boost::{ORES, ores_in},
    declarations::{ColonyPlan, Declarations, Outpost, REACH_HOPS, Stage},
    engine,
    intel::{self, Intel, Row},
    tiles,
    tuning::Tuning,
    vocabulary::{self, RESOURCES_ALL, ResourceType},
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
/// At default weights a fresh failure near a room outweighs the most a room scores (its
/// dig and four neighbours'), fading over this (Hivemind's).
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
pub fn score(f: &Facts, t: &Tuning) -> f64 {
    let held = f64::from(engine::HELD_OUTPUT_PER_TICK);
    held * t.expand_source * f.sources as f64 + held * t.expand_near_source * f.free_near as f64
        - t.expand_crossing * f64::from(f.crossings)
        + t.expand_worth * f.worth
        - t.expand_failure * held * f.failed
        + t.expand_mineral * f.mineral
}

fn tier_of(c: ResourceType) -> usize {
    let lower = c.reaction_components().and_then(|r| r.into_iter().find(|r| r.boost().is_some()));
    lower.map_or(1, |l| 1 + tier_of(l))
}

fn top_tier() -> usize {
    boosts().map(|(b, _)| tier_of(b)).max().unwrap_or(1)
}

fn boosts() -> impl Iterator<Item = (ResourceType, i32)> {
    let (tier_step, fight) = (10_i32, 4);
    RESOURCES_ALL.iter().copied().filter(|c| c.boost().is_some()).map(move |c| {
        let fights = vocabulary::ALL_BODY_PARTS.iter().any(|p| vocabulary::takes(*p, c));
        (c, tier_step.pow(tier_of(c) as u32 - 1) * if fights { fight } else { 1 })
    })
}

/// Each boost's weight split evenly among the ores it still lacks (owner): the last earns it all.
fn unlock(ore: usize, held: [bool; ORES.len()], tier: usize) -> (f64, f64) {
    let lacks = |c| ores_in(c).into_iter().zip(held).filter(|(n, h)| *n > 0 && !h).count();
    let share = |(c, w): (ResourceType, i32)| f64::from(w) / lacks(c) as f64;
    let split =
        |(c, w)| if tier_of(c) <= tier { (share((c, w)), 0.0) } else { (0.0, share((c, w))) };
    let gained = boosts().filter(|(c, _)| ores_in(*c)[ore] > 0 && !held[ore]).map(split);
    gained.fold((0.0, 0.0), |(now, above), (n, a)| (now + n, above + a))
}

pub struct Ores {
    pub tier: usize,
    pub held: Vec<(RoomName, ResourceType)>,
}

impl Ores {
    pub fn of(world: &World, derived: &Derived) -> Ores {
        let level = |c: &RoomName| world.room(*c)?.controller.as_ref().map(|c| c.level);
        let labs = derived.colonies.iter().any(|c| level(c).is_some_and(engine::topped_out));
        let ore = |c: &RoomName| Some((*c, world.room(*c)?.minerals.first()?.resource?));
        let held = derived.owned.iter().filter_map(ore).collect();
        Ores { tier: if labs { top_tier() } else { 1 }, held }
    }
}

fn seen(intel: &Intel, near: impl Fn(RoomName) -> bool) -> [usize; ORES.len()] {
    let of = |o, w: &Row| w.mineral.is_some_and(|(m, ..)| m == o);
    ORES.map(|o| intel.iter().filter(|(r, w)| near(**r) && of(o, w)).count())
}

pub fn mineral_worth(
    (mineral, held, density): (ResourceType, [bool; ORES.len()], i32),
    (tier, rival): (usize, bool),
    seen: &[usize; ORES.len()],
    t: &Tuning,
) -> f64 {
    let last = |o| unlock(o, std::array::from_fn(|i| i != o), top_tier()).0;
    let most = (0..ORES.len()).map(last).fold(0.0, f64::max);
    let mean = seen.iter().sum::<usize>() as f64 / ORES.len() as f64;
    ORES.iter().position(|o| *o == mineral).map_or(0.0, |ore| {
        let (now, later) = unlock(ore, held, tier);
        let near = if rival { 1.0 + t.expand_rival_ore } else { 1.0 };
        let need = now + (t.expand_future_ore * near).min(1.0) * later;
        let known = seen.iter().sum::<usize>() >= ORES.len();
        let rarity = if known { (mean + 1.0) / (seen[ore] + 1) as f64 } else { 1.0 };
        let rich = engine::mineral_full(density)
            .map_or(1.0, |a| (f64::from(a) / f64::from(engine::MINERAL_HIGH_AMOUNT)).sqrt());
        f64::from(engine::HELD_OUTPUT_PER_TICK) * need * rich / most
            * rarity.powf(t.expand_scarcity)
    })
}

/// `but`, a colony of ours weighed as if it were not.
fn ore(
    (ores, rivals): (&Ores, &[(RoomName, i32)]),
    intel: &Intel,
    (room, rock, but): (RoomName, Option<(ResourceType, i32)>, Option<RoomName>),
    t: &Tuning,
) -> f64 {
    let Some((m, density)) = rock else { return 0.0 };
    let near = |r: RoomName| tiles::rooms_apart(r, room) <= REACH_HOPS;
    let rival = rivals.iter().any(|(r, _)| near(*r));
    let held = ORES.map(|o| ores.held.iter().any(|(c, h)| *h == o && Some(*c) != but));
    mineral_worth((m, held, density), (ores.tier, rival), &seen(intel, near), t)
}

fn free(world: &World, row: &Row) -> bool {
    row.unheld(&world.me, world.time) && row.worker.as_ref().is_none_or(|u| u == &world.me)
}

fn apart(owned: &BTreeSet<RoomName>, room: RoomName) -> bool {
    owned.iter().all(|c| tiles::rooms_apart(*c, room) >= 2)
}

pub type Ours<'a> = (&'a BTreeSet<RoomName>, &'a [(RoomName, i32)]);

pub fn claimable(
    world: &World,
    (owned, rivals): Ours,
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
    (ours, worked): (Ours, &dyn Fn(RoomName) -> bool),
    intel: &Intel,
    (name, row): (RoomName, &Row),
    (crossings, failed): (i32, f64),
    (ores, t): (&Ores, &Tuning),
) -> Facts {
    let beside = [(-1, 0), (1, 0), (0, -1), (0, 1)].into_iter();
    let free_near = (beside.filter_map(|(dx, dy)| name.offset(dx, dy)))
        .filter(|next| apart(ours.0, *next) && !worked(*next))
        .filter_map(|next| intel.get(&next).filter(|r| r.controller.is_some() && free(world, r)))
        .map(|r| r.sources.as_ref().map_or(0, Vec::len))
        .sum();
    Facts {
        sources: row.sources.as_ref().map_or(0, Vec::len),
        free_near,
        crossings,
        worth: season::worth(name),
        failed,
        mineral: ore((ores, ours.1), intel, (name, row.mineral.map(|(m, _, d)| (m, d)), None), t),
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
    room.minerals.extend(rock.and_then(|(resource, at, density)| {
        Some(Mineral {
            id: Id::of_engine("mineral"),
            pos: tiles::to_pos(outpost.room, at?),
            resource: Some(resource),
            amount: 0,
            density,
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
    // Any failure or give-up holds the next give-up off (Hivemind's abandonWeakRooms).
    if derived.capped && (expansion.failed.iter()).any(|f| world.time - f.since < FORGET) {
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
    let ores = Ores::of(world, derived);
    let mut scored: Vec<(f64, Outpost)> = Vec::new();
    for (home, tree) in derived.trees.iter().filter(|(h, _)| sponsors.contains(h)) {
        let reach = -REACH_HOPS..=REACH_HOPS;
        let around = reach.clone().flat_map(|x| reach.clone().map(move |y| (x, y)));
        for name in around.filter_map(|(dx, dy)| home.offset(dx, dy)) {
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
            let ours = ((&derived.owned, rivals), &worked as &dyn Fn(RoomName) -> bool);
            let facts =
                facts(world, ours, intel, (name, row), (crossings, failed(name)), (&ores, tuning));
            let outpost = Outpost {
                reserved: true,
                room: name,
                controller: controller.clone(),
                sources: sources.clone(),
            };
            // A room that does not pay is never declared (§7.2).
            let score = score(&facts, tuning);
            if score > 0.0 {
                scored.push((score, outpost));
            }
        }
    }
    scored.sort_by(|(a, x), (b, y)| b.total_cmp(a).then(x.room.cmp(&y.room)));
    let best = (scored.into_iter()).find(|(_, o)| {
        walked(o, expansion) && terrain(o.room).is_some_and(|t| fits(world, o, intel, t))
    });
    if !derived.capped {
        expansion.in_flight = best.map(|(_, o)| Expansion { room: o.room, since: world.time });
        return;
    }
    let held_score = |room: &Room| {
        let rock = room.minerals.first().and_then(|m| Some((m.resource?, m.density)));
        let worked = |o: &&Outpost| tiles::rooms_apart(o.room, room.name) == 1;
        let near = (declarations.outposts.iter().chain(outposts)).filter(worked);
        let facts = Facts {
            sources: room.sources.len(),
            free_near: near.map(|o| o.sources.len()).sum(),
            crossings: 0,
            worth: season::worth(room.name),
            failed: 0.0,
            mineral: ore((&ores, rivals), intel, (room.name, rock, Some(room.name)), tuning),
        };
        score(&facts, tuning)
    };
    // Before RCL6's extractor and terminal a colony is cheap to give up for a
    // better room; past them it holds what a new one would take a long climb to.
    let worst = (derived.colonies.iter().filter_map(|c| world.room(*c)))
        .filter(|r| r.controller.as_ref().is_some_and(|c| c.level <= tuning.prune_level))
        .filter(|r| !busy.contains(&r.name) && may_give_up(r.name, expansion))
        .map(|r| (held_score(r), r.name))
        .min_by(|(a, x), (b, y)| a.total_cmp(b).then(x.cmp(y)));
    if let (Some((beat, _)), Some((kept, room))) = (best, worst)
        && beat > kept + f64::from(engine::HELD_OUTPUT_PER_TICK) * tuning.expand_swap
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
        let text = include_str!("../../../scripts/scenario/worlds/season-outpost-ring.json");
        let world = fabot_base::testing::world(text);
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

    fn at(ore: ResourceType) -> usize {
        ORES.iter().position(|o| *o == ore).unwrap()
    }

    fn all_but(ore: usize) -> [bool; ORES.len()] {
        std::array::from_fn(|i| i != ore)
    }

    #[test]
    fn a_held_ore_unlocks_nothing_and_scores_below_one_that_completes_a_chain() {
        let t = Tuning::default();
        let worth =
            |o, held| mineral_worth((o, held, 0), (top_tier(), false), &[0; ORES.len()], &t);
        for (i, o) in ORES.iter().enumerate() {
            assert_eq!(unlock(i, [true; ORES.len()], top_tier()), (0.0, 0.0), "{o:?}");
            assert!(worth(*o, [false; ORES.len()]) < worth(*o, all_but(i)), "{o:?}");
        }
        let have = Facts {
            sources: 2,
            free_near: 3,
            crossings: 2,
            worth: 0.0,
            failed: 0.0,
            mineral: worth(ResourceType::Utrium, [true; ORES.len()]),
        };
        let lack = Facts {
            mineral: worth(ResourceType::Lemergium, all_but(at(ResourceType::Lemergium))),
            ..have
        };
        assert!(score(&lack, &t) > score(&have, &t));
    }

    #[test]
    fn catalyst_last_unlocks_every_t3_as_an_option_until_labs_stand() {
        use ResourceType::{CatalyzedGhodiumAlkalide, GhodiumAlkalide, GhodiumOxide};
        assert_eq!(
            [CatalyzedGhodiumAlkalide, GhodiumAlkalide, GhodiumOxide].map(tier_of),
            [3, 2, 1]
        );
        let x = at(ResourceType::Catalyst);
        let with_x = boosts().filter(|(b, _)| ores_in(*b)[x] > 0).map(|(_, w)| f64::from(w)).sum();
        assert_eq!(unlock(x, all_but(x), top_tier()), (with_x, 0.0));
        assert_eq!(unlock(x, all_but(x), 1), (0.0, with_x));
        let none = Tuning { expand_future_ore: 0.0, ..Tuning::default() };
        let worth = |tier, rival, t: &Tuning| {
            mineral_worth((ORES[x], all_but(x), 0), (tier, rival), &[1; ORES.len()], t)
        };
        assert_eq!(worth(1, false, &none), 0.0);
        let t = Tuning::default();
        assert!(0.0 < worth(1, false, &t) && worth(1, false, &t) < worth(1, true, &t));
        assert!(worth(1, true, &t) < worth(top_tier(), false, &t));
    }

    #[test]
    fn one_t3_fight_boost_outweighs_every_lower_one_and_its_last_ore_outbids_a_t2_ore() {
        let xgho2 = boosts().find(|(b, _)| *b == ResourceType::CatalyzedGhodiumAlkalide);
        let lower: i32 = boosts().filter(|(b, _)| tier_of(*b) < 3).map(|(_, w)| w).sum();
        assert!(xgho2.unwrap().1 > lower);
        let (t, x, l) =
            (Tuning::default(), at(ResourceType::Catalyst), at(ResourceType::Lemergium));
        let worth = |o, held| mineral_worth((ORES[o], held, 0), (3, false), &[0; ORES.len()], &t);
        let no_l_or_x = std::array::from_fn(|i| i != l && i != x);
        assert!(worth(x, all_but(x)) > 4.0 * worth(l, no_l_or_x));
    }

    #[test]
    fn a_third_ore_sharing_chains_with_two_held_outbids_one_that_does_not() {
        use ResourceType::{Hydrogen, Keanium, Oxygen, Utrium};
        let held = std::array::from_fn(|i| i == at(Utrium) || i == at(Hydrogen));
        assert!(unlock(at(Oxygen), held, 3).0 > unlock(at(Keanium), held, 3).0);
    }

    #[test]
    fn a_mineral_rarer_in_reach_weighs_more() {
        let seen = [5, 1, 3, 4, 3, 0, 2];
        let flat = Tuning { expand_scarcity: 0.0, ..Tuning::default() };
        let weight = |i: usize| {
            let worth = |t| mineral_worth((ORES[i], all_but(i), 0), (top_tier(), false), &seen, t);
            worth(&Tuning::default()) / worth(&flat)
        };
        let mut ores: Vec<_> = (0..ORES.len()).map(|i| (seen[i], weight(i))).collect();
        ores.sort_by_key(|o| o.0);
        assert!(ores.windows(2).all(|w| w[0].0 == w[1].0 || w[0].1 > w[1].1), "{ores:?}");
    }

    #[test]
    fn too_few_mineral_rooms_in_reach_weigh_every_ore_alike() {
        let (t, flat) = (Tuning::default(), Tuning { expand_scarcity: 0.0, ..Tuning::default() });
        let thin = [0, 1, 0, 0, 3, 0, 2];
        for (i, o) in ORES.iter().enumerate() {
            let worth = |t| mineral_worth((*o, all_but(i), 0), (top_tier(), false), &thin, t);
            assert_eq!(worth(&t), worth(&flat), "{o:?}");
        }
    }

    #[test]
    fn a_richer_sole_ore_at_any_tier_stays_within_four_digs_at_every_range_max() {
        let t = Tuning {
            expand_scarcity: 1.0,
            expand_future_ore: 1.0,
            expand_rival_ore: 1.0,
            ..Tuning::default()
        };
        let dig = f64::from(engine::HELD_OUTPUT_PER_TICK);
        let last = |i: usize, density| {
            mineral_worth((ORES[i], all_but(i), density), (3, false), &[0; 7], &Tuning::default())
        };
        assert_eq!((0..ORES.len()).map(|i| last(i, 3)).fold(0.0, f64::max), dig);
        assert!((0..ORES.len()).all(|i| last(i, 1) < last(i, 2) && last(i, 2) < last(i, 3)));
        assert!((0..ORES.len()).all(|i| last(i, 3) < last(i, 4) && last(i, 0) == last(i, 3)));
        for (i, ore) in ORES.iter().enumerate() {
            let mut seen = [6; ORES.len()];
            seen[i] = 1;
            for tier in [1, top_tier()] {
                let worth = mineral_worth((*ore, all_but(i), 4), (tier, true), &seen, &t);
                assert!(worth < 4.0 * dig, "{ore:?} at T{tier}: {worth}");
            }
        }
    }

    #[test]
    fn season_worth_outbids_dig() {
        let near =
            Facts { sources: 2, free_near: 4, crossings: 2, worth: 0.0, failed: 0.0, mineral: 0.0 };
        let scoring = Facts { free_near: 0, crossings: 3, worth: 25.0, ..near };
        let t = Tuning::default();
        assert!(score(&near, &t) > score(&Facts { worth: 0.0, ..scoring }, &t));
        assert!(score(&scoring, &t) > score(&near, &t));
    }
}
