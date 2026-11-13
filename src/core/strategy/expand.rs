use std::collections::BTreeSet;

use super::intel::{self, Intel, Row};
use crate::core::engine;
use crate::core::layout::{ground, plan};
use crate::core::season;
use crate::core::tick::{ColonyPlan, Declarations, Derived, Outpost, REACH_HOPS, undeclared};
use crate::core::tiles;
use crate::core::vocabulary::Resource;
use crate::core::world::{RoomName, World, terrain::Terrain};

/// The claim in flight (`ARCHITECTURE.md` §4 `expansion`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expansion {
    pub room: RoomName,
    pub since: i32,
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

/// What a room is worth holding, in energy a tick.
pub struct Facts {
    pub sources: usize,
    pub free_near: usize,
    pub crossings: i32,
    pub worth: f64,
}

/// A simple rule shaped as its priced replacement (`AGENTS.md`).
pub fn score(f: &Facts) -> f64 {
    let held = f64::from(engine::HELD_OUTPUT_PER_TICK);
    held * f.sources as f64 + held / 2.0 * f.free_near as f64 - f64::from(f.crossings) + f.worth
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
        .filter(|p| !busy.contains(&p.room))
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

/// One claim in flight at a time (owner, 2026-11-13); `held` the last tick's.
pub fn step(
    world: &World,
    derived: &Derived,
    (declarations, plans): (&Declarations, &[ColonyPlan]),
    (rivals, chosen, busy): (&[(RoomName, i32)], &[Outpost], &BTreeSet<RoomName>),
    intel: &Intel,
    terrain: &dyn Fn(RoomName) -> Option<Terrain>,
    held: Option<Expansion>,
) -> Option<Expansion> {
    let raising = (declarations.claims.iter()).any(|c| !derived.colonies.contains(&c.room));
    if let Some(e) = held {
        let owned = derived.owned.contains(&e.room);
        let theirs =
            (intel.get(&e.room).and_then(|r| r.owner.as_ref())).is_some_and(|o| o != &world.me);
        // The operator's claim takes the room GCL leaves.
        return (!derived.colonies.contains(&e.room) && !theirs && (owned || !raising))
            .then_some(e);
    }
    if world.time % DUE != 0
        || raising
        || derived.owned.len() as i32 >= world.rooms_allowed
        || world.bucket < BUCKET
    {
        return None;
    }
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
            let worked = (declarations.outposts.iter().chain(chosen)).any(|o| o.room == next);
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
                };
                let outpost = Outpost {
                    room: name,
                    controller: controller.clone(),
                    sources: sources.clone(),
                };
                scored.push((score(&facts), outpost));
            }
        }
    }
    scored.sort_by(|(a, x), (b, y)| b.total_cmp(a).then(x.room.cmp(&y.room)));
    let chosen = (scored.into_iter())
        .find(|(_, o)| terrain(o.room).is_some_and(|t| fits(world, o, intel, t)))?;
    Some(Expansion {
        room: chosen.1.room,
        since: world.time,
    })
}

pub fn claim(intel: &Intel, held: Option<&Expansion>) -> Option<Outpost> {
    let e = held?;
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
    fn season_worth_outbids_dig() {
        let near = Facts {
            sources: 2,
            free_near: 4,
            crossings: 2,
            worth: 0.0,
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
