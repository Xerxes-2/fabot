use std::collections::BTreeSet;

use super::intel::{self, Intel, Row};
use crate::core::engine;
use crate::core::layout::{ground, plan};
use crate::core::season;
use crate::core::tick::{
    ColonyPlan, Declarations, Derived, Outpost, REACH_HOPS, Stage, undeclared,
};
use crate::core::tiles;
use crate::core::vocabulary::Resource;
use crate::core::world::{Room, RoomName, World, terrain::Terrain};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expansion {
    pub room: RoomName,
    pub since: i32,
}

/// `ARCHITECTURE.md` §4 `expansion`.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Expansions {
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
}

/// Candidates are planned on a due tick, not each tick: a plan is a Layout draw.
const DUE: i32 = 100;
/// The bucket fills only while the ticks run under the limit (Hivemind's
/// CPU headroom).
const BUCKET: i32 = 9_000;
/// The stock past its reserve a sponsor carries a young room on.
const MARGIN: i32 = 30_000;
/// A storage first stands at RCL4.
const SPONSOR_LEVEL: i32 = 4;
/// A rival this near raids a young room before its towers stand.
const THREAT: i32 = 2;
/// The owner's.
const CLAIM_DEADLINE: i32 = 5 * engine::CREEP_LIFETIME;
/// About twice the claim to storage measured two crossings off (21k ticks).
const RAISE_DEADLINE: i32 = 40_000;
/// A fresh failure near a room outweighs the most a room scores (its dig and
/// four neighbours'), fading over this (Hivemind's).
const FORGET: i32 = 100_000;
/// Before RCL6's extractor and terminal a colony is cheap to give up for a
/// better room; past them it holds what a new one would take a long climb to.
const PRUNE_LEVEL: i32 = 5;

/// What a room is worth holding, in energy a tick.
pub struct Facts {
    pub sources: usize,
    pub free_near: usize,
    pub crossings: i32,
    pub worth: f64,
    /// Failures in or beside it, each fading as it ages.
    pub failed: f64,
}

/// A simple rule shaped as its priced replacement (`AGENTS.md`).
pub fn score(f: &Facts) -> f64 {
    let held = f64::from(engine::HELD_OUTPUT_PER_TICK);
    held * f.sources as f64 + held / 2.0 * f.free_near as f64 - f64::from(f.crossings) + f.worth
        - 6.0 * held * f.failed
}

fn free(world: &World, row: &Row) -> bool {
    let theirs = |user: &String| user != &world.me;
    row.owner.is_none()
        && (row.reservation.as_ref()).is_none_or(|(u, lapse)| !theirs(u) || *lapse <= world.time)
        && !row.worker.as_ref().is_some_and(theirs)
}

/// A colony an operation holds (`outposts::busy`) sponsors nothing: a siege's
/// successors lagged behind a claim's bodies.
fn sponsors(world: &World, plans: &[ColonyPlan], busy: &BTreeSet<RoomName>) -> Vec<RoomName> {
    (plans.iter())
        .filter(|p| p.stage == Stage::Exploit && !busy.contains(&p.room))
        .filter(|p| {
            world.room(p.room).is_some_and(|r| {
                let level = r.controller.as_ref().map_or(0, |c| c.level);
                let stock: i32 = (r.structures.iter())
                    .filter(|s| s.kind.is_storage() && s.owner.is_ours())
                    .map(|s| s.store.amount_of(&Resource::Energy))
                    .sum();
                level >= SPONSOR_LEVEL && stock >= p.reserve + MARGIN
            })
        })
        .map(|p| p.room)
        .collect()
}

/// Layout plans the room before it is claimed: one it cannot fit is never
/// taken (The International's).
fn fits(world: &World, outpost: &Outpost, intel: &Intel, terrain: Terrain) -> bool {
    let mut room = intel::blind(outpost, intel, world, &undeclared());
    room.terrain = Some(terrain);
    (ground::of_room(&room).and_then(|g| plan::of_ground(&world.rules, &g, &[])))
        .is_some_and(|p| p.unfit.is_empty())
}

#[derive(Debug, PartialEq, Eq)]
enum Fate {
    Raised,
    Failed,
    Flying,
}

fn fate(age: i32, owned: bool, theirs: bool, stored: bool) -> Fate {
    let deadline = if owned {
        RAISE_DEADLINE
    } else {
        CLAIM_DEADLINE
    };
    match () {
        _ if stored => Fate::Raised,
        _ if theirs || age > deadline => Fate::Failed,
        _ => Fate::Flying,
    }
}

/// One claim in flight at a time (owner, 2026-11-13); `held` the last tick's.
pub fn step(
    world: &World,
    derived: &Derived,
    (declarations, plans): (&Declarations, &[ColonyPlan]),
    around: &Around,
    intel: &Intel,
    terrain: &dyn Fn(RoomName) -> Option<Terrain>,
    expansion: &mut Expansions,
) {
    let Around {
        rivals,
        outposts,
        busy,
        fought,
    } = *around;
    let raising = (declarations.claims.iter()).any(|c| !derived.colonies.contains(&c.room));
    (expansion.failed).retain(|f| world.time - f.since < FORGET);
    (expansion.abandoned).retain(|a| derived.owned.contains(&a.room));
    let given =
        |room: RoomName, e: &Expansions| e.abandoning(room) || declarations.abandoning(room);
    // Never the last colony nor one in a fight (owner).
    let may_give_up = |room: RoomName, expansion: &Expansions| {
        (derived.colonies.iter())
            .filter(|c| !given(**c, expansion))
            .count()
            > 1
            && !fought.contains(&room)
            && !given(room, expansion)
    };
    let give_up = |room: RoomName, expansion: &mut Expansions| {
        let e = Expansion {
            room,
            since: world.time,
        };
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
        // In flight until it stands on its own: support ends with its storage.
        let stored = world.room(e.room).is_some_and(|r| {
            (r.structures.iter())
                .any(|s| (s.kind.is_storage() || s.kind.is_terminal()) && s.owner.is_ours())
        });
        match fate(world.time - e.since, owned, theirs, stored) {
            Fate::Failed if owned && may_give_up(e.room, expansion) => give_up(e.room, expansion),
            // Given up once its fight is over.
            Fate::Failed if owned => expansion.in_flight = Some(e),
            Fate::Failed => expansion.failed.push(Expansion {
                since: world.time,
                ..e
            }),
            // One not yet owned yields to the operator's claim in flight.
            Fate::Flying if owned || !raising => expansion.in_flight = Some(e),
            _ => (),
        }
        return;
    }
    if world.time % DUE != 0 || raising || world.bucket < BUCKET {
        return;
    }
    let capped = derived.owned.len() as i32 >= world.rooms_allowed;
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
    let sponsors = sponsors(world, plans, busy);
    let apart_from_ours = |room: RoomName| {
        derived
            .owned
            .iter()
            .all(|c| tiles::rooms_apart(*c, room) >= 2)
    };
    let free_near = |room: RoomName| -> usize {
        let (x, y) = room.coords();
        let mut n = 0;
        for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
            let next = RoomName::new(x + dx, y + dy);
            let worked = (declarations.outposts.iter().chain(outposts)).any(|o| o.room == next);
            if let Some(row) = intel
                .get(&next)
                .filter(|r| r.controller.is_some() && free(world, r))
                && apart_from_ours(next)
                && !worked
            {
                n += row.sources.as_ref().map_or(0, Vec::len);
            }
        }
        n
    };

    let mut scored: Vec<(f64, Outpost)> = Vec::new();
    for (home, tree) in derived.trees.iter().filter(|(h, _)| sponsors.contains(h)) {
        let (hx, hy) = home.coords();
        for dx in -REACH_HOPS..=REACH_HOPS {
            for dy in -REACH_HOPS..=REACH_HOPS {
                let name = RoomName::new(hx + dx, hy + dy);
                let (Some(crossings), Some(row)) = (tree.crossings(name, *home), intel.get(&name))
                else {
                    continue;
                };
                let (Some(controller), Some(sources)) = (&row.controller, &row.sources) else {
                    continue;
                };
                if sources.len() < 2
                    || !free(world, row)
                    || !apart_from_ours(name)
                    || declarations.avoid.contains(&name)
                    || world.closed.contains(&name)
                    || declarations.sieges.contains(&name)
                    || (rivals.iter()).any(|(t, _)| tiles::rooms_apart(*t, name) <= THREAT)
                    || scored.iter().any(|(_, o)| o.room == name)
                {
                    continue;
                }
                let facts = Facts {
                    sources: sources.len(),
                    free_near: free_near(name),
                    crossings,
                    worth: season::worth(name),
                    failed: failed(name),
                };
                let outpost = Outpost {
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
    let best = (scored.into_iter())
        .find(|(_, o)| terrain(o.room).is_some_and(|t| fits(world, o, intel, t)));
    if !capped {
        expansion.in_flight = best.map(|(_, o)| Expansion {
            room: o.room,
            since: world.time,
        });
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
        })
    };
    let worst = (derived.colonies.iter().filter_map(|c| world.room(*c)))
        .filter(|r| {
            r.controller
                .as_ref()
                .is_some_and(|c| c.level <= PRUNE_LEVEL)
        })
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
        room: e.room,
        controller: row.controller.clone()?,
        sources: row.sources.clone()?,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_claim_fails_past_its_deadline_or_to_another_player() {
        assert_eq!(fate(CLAIM_DEADLINE + 1, false, false, false), Fate::Failed);
        assert_eq!(fate(CLAIM_DEADLINE + 1, true, false, false), Fate::Flying);
        assert_eq!(fate(RAISE_DEADLINE + 1, true, false, false), Fate::Failed);
        assert_eq!(fate(RAISE_DEADLINE + 1, true, false, true), Fate::Raised);
        assert_eq!(fate(0, false, true, false), Fate::Failed);
    }

    #[test]
    fn season_worth_outbids_dig() {
        let near = Facts {
            sources: 2,
            free_near: 4,
            crossings: 2,
            worth: 0.0,
            failed: 0.0,
        };
        let scoring = Facts {
            free_near: 0,
            crossings: 3,
            worth: 25.0,
            ..near
        };
        assert!(
            score(&near)
                > score(&Facts {
                    worth: 0.0,
                    ..scoring
                })
        );
        assert!(score(&scoring) > score(&near));
    }
}
