/// `Sweep` (`ARCHITECTURE.md` §6.2); `step` closes it with None.
use std::collections::{BTreeMap, BTreeSet};

use super::super::casting;
use super::super::scene::{Hunting, Scene, fresh};
use super::super::shape::{Engagement, Objective, Phase, enter};
use super::defend;
use fabot_atlas::{self as atlas, Atlas};
use fabot_base::declarations::{Declarations, OpKind};
use fabot_base::intel::Row;
use fabot_base::tiles;
use fabot_base::vocabulary::Part;
use fabot_base::world::{Creep, Fallen, Owner, Pos, Room, RoomName, Source, hostiles};
use fabot_base::{engine, grid};
use fabot_exchange::Fighter;
use fabot_tactics::Tactic;

pub const CANDIDATES: &[Tactic] = &[Tactic::BlobKite, Tactic::Kite];

pub struct Facts {
    pub time: i32,
    pub standing: BTreeSet<String>,
    pub aging_out: BTreeSet<String>,
    pub asked: bool,
    pub arrived: bool,
    /// Whether its room is worth holding; None out of sight.
    pub worth: Option<bool>,
    /// The candidate a set holds that pays most a tick of its walk.
    pub elsewhere: Option<RoomName>,
    pub patrol: Option<RoomName>,
}

pub fn prey<'a>(room: &'a Room) -> Vec<&'a Creep> {
    room.hostiles()
        .filter(|h| matches!(h.owner, Owner::Enemy(_)))
        .filter(|h| !hostiles::weaponed(h) && hostiles::works(h))
        .collect()
}

pub fn worth(c: &Creep) -> i32 {
    let body = fabot_base::world::body_kinds(c);
    let life = engine::lifetime(body.iter().copied());
    engine::body_cost(&body) * c.ticks_to_live.clamp(0, life) / life + c.store.used()
}

/// Owner: denied harvest is worth as much as bodies killed; a source is seen
/// dug by its running regeneration timer.
pub fn room_worth(room: &Room, prey: &[&Creep], exposed: &[Pos], held: i32) -> i32 {
    let enemy = |o: &Owner| matches!(o, Owner::Enemy(_));
    let enemy_s =
        room.controller.as_ref().is_some_and(|c| {
            enemy(&c.owner) || c.reservation.as_ref().is_some_and(|(o, _)| enemy(o))
        }) || !prey.is_empty();
    let denied: i32 = room
        .sources
        .iter()
        .filter(|s| enemy_s && s.ticks_to_regeneration > 0 && exposed.contains(&s.pos))
        .map(|s| yield_of(s, held))
        .sum();

    prey.iter().map(|c| worth(c)).sum::<i32>() + denied
}

pub fn yield_of(s: &Source, held: i32) -> i32 {
    s.energy_capacity * held.max(0) / engine::SOURCE_REGEN_TIME
}

pub(crate) fn holds(
    scene: &Scene,
    e: &Engagement,
    (room, grounds): (RoomName, &BTreeMap<RoomName, Hunting>),
) -> bool {
    let ours: Vec<Fighter> = (scene.members_of(e).into_iter())
        .map(casting::fighter_of)
        .collect();
    let (Some(r), Some(g)) = (scene.in_sight(room), grounds.get(&room)) else {
        return false;
    };
    !ours.is_empty() && scene.hunt(g, r, &ours, 0).is_some()
}

/// Owner: small, cheap, fast bodies; the cheapest that holds a room is cast.
pub const HARASSERS: [&[Part]; 2] = [&casting::DEFENDER, &casting::KITER];

/// Capability, not intent: a guard it out-reaches at its pace never catches it.
pub fn outreaches(ours: &Fighter, theirs: &Fighter) -> bool {
    let step = |f: &Fighter, w: i32| grid::step_ticks(f.gait, w).unwrap_or(i32::MAX);
    ours.reach > theirs.reach
        && [engine::PLAIN_WEIGHT, engine::SWAMP_WEIGHT]
            .into_iter()
            .all(|w| step(ours, w) <= step(theirs, w))
}

/// Of `held`, what a body of ours that fell in `room` short of its life held past its walk.
pub fn kept(room: &Room, held: i32, walk: i32) -> i32 {
    (room.remains.iter().filter_map(|r| r.fallen.as_ref()))
        .filter_map(Fallen::lived)
        .map(|lived| lived - walk)
        .fold(held, i32::min)
        .max(0)
}

/// How many more harassers our spawns cast beside the sweeps fielded, within
/// a full body's spawn time a spawn, as a set's casts were bounded before.
pub(crate) fn room_for(scene: &Scene, sweeps: &[&Engagement]) -> i32 {
    let homes: Vec<&Room> = (scene.world.rooms.iter())
        .filter(|r| scene.homes.contains(&r.name))
        .collect();
    let spawn = engine::SPAWN_TICKS_PER_PART * engine::MAX_BODY_PARTS as i32;
    let ticks = |body: &[Part]| engine::SPAWN_TICKS_PER_PART * body.len() as i32;
    let fielded: i32 = (sweeps.iter())
        .flat_map(|e| scene.members_of(e))
        .map(|c| ticks(&fabot_base::world::body_kinds(c)))
        .chain(
            scene
                .spawning
                .iter()
                .filter(|c| sweeps.iter().any(|e| e.id == c.engagement))
                .map(|c| ticks(&c.body)),
        )
        .sum();
    let largest = HARASSERS
        .iter()
        .max_by_key(|b| b.len())
        .copied()
        .unwrap_or_default();
    casting::copies_within(i32::MAX, spawn * scene.spawns_in(&homes) - fielded, largest)
}

pub(crate) fn worth_held(
    scene: &Scene,
    e: &Engagement,
    grounds: &BTreeMap<RoomName, Hunting>,
) -> Option<bool> {
    let held = scene.members_of(e).is_empty() || holds(scene, e, (e.room, grounds));
    grounds.get(&e.room).map(|g| g.worth > 0 && held)
}

pub(crate) fn opened(
    scene: &Scene,
    (candidates, claimed): (&[(RoomName, i32, i32)], &mut BTreeSet<RoomName>),
    (engagements, first, declarations): (&[Engagement], i32, &Declarations),
) -> Vec<(Engagement, bool)> {
    let sweeps: Vec<&Engagement> = (engagements.iter())
        .filter(|e| e.objective == Objective::Sweep)
        .collect();
    (candidates.iter())
        .map(|(room, ..)| *room)
        .filter(|room| claimed.insert(*room))
        .take(room_for(scene, &sweeps).max(0) as usize)
        .enumerate()
        .map(|(i, room)| {
            let e = fresh(
                first + i as i32,
                Objective::Sweep,
                room,
                scene.world.time,
                Tactic::Kite,
            );
            (
                Engagement {
                    priority: declarations
                        .priority(OpKind::Sweep, room)
                        .unwrap_or_default(),
                    ..e
                },
                false,
            )
        })
        .collect()
}

/// Stalest first, beside his worked rooms too (his farther ones lie past our scouts), an
/// unseen one as stale as the stalest seen; none a sweep holds or a player not an enemy, past a
/// rival's towers, or where the tracker holds an armed group (owner: the escort it left).
pub(crate) fn patrol(
    scene: &Scene,
    atlas: &Atlas,
    e: &Engagement,
    (preyed, enemies, held): (&[RoomName], &BTreeSet<String>, &BTreeSet<RoomName>),
) -> Option<RoomName> {
    let from = scene.members_of(e).first()?.pos.room;
    let towered: BTreeSet<RoomName> = (scene.intel.iter())
        .filter(|(_, row)| row.towered)
        .map(|(room, _)| *room)
        .chain(scene.avoid.iter().copied())
        .collect();
    let sides = [(1, 0), (-1, 0), (0, 1), (0, -1)];
    let beside = (preyed.iter()).flat_map(|r| sides.map(|(dx, dy)| r.offset(dx, dy)));
    let open = (beside.flatten())
        .filter(|r| tiles::has_controller(r.coords()) && !scene.worked.contains(r))
        .chain(preyed.iter().copied())
        .filter(|r| !held.contains(r) && !towered.contains(r) && !scene.homes.contains(r))
        .filter(|r| (scene.by_id.values()).all(|g| g.room() != *r || !scene.armed_seen(g)));
    let rows: Vec<_> = (open.map(|r| (r, scene.intel.get(&r))))
        .filter(|(_, row)| {
            row.is_none_or(|row| {
                let users = row
                    .owner
                    .iter()
                    .chain(row.reservation.iter().map(|(u, _)| u));
                row.level == 0 && users.into_iter().all(|u| enemies.contains(u))
            })
        })
        .collect();
    let age = |row: Option<&&Row>| row.map(|row| scene.world.time - row.seen);
    let stalest = rows.iter().filter_map(|(_, row)| age(row.as_ref())).max();
    let walk = |r: RoomName| atlas.route(&towered, Scene::hops(e), from, r);
    atlas::onward(rows.iter().filter_map(|(r, row)| {
        let crossed = walk(*r)?.len() as i32 - 1;
        Some((
            *r,
            age(row.as_ref()).or(stalest)?,
            engine::ROOM_SIDE * crossed,
        ))
    }))
}

/// Never home (owner); each room's losses are its own.
pub fn onto(room: RoomName, time: i32, e: &Engagement) -> Engagement {
    let mut actual = e.actual.clone();
    actual.left_dead += e.lost;
    Engagement {
        room,
        unanswered: false,
        lost: 0,
        actual,
        ..enter(Phase::Approaching, time, e)
    }
}

pub fn step(facts: &Facts, e: &Engagement) -> Option<Engagement> {
    let mut next = e.clone();
    next.ours.retain(|n, _| facts.standing.contains(n));
    next.lost = e.lost + (e.ours.len() - next.ours.len()) as i32;
    next.ours.retain(|n, _| !facts.aging_out.contains(n));
    let e = &next;

    let hollow = e.ours.is_empty() && !facts.asked;
    let onward = (facts.elsewhere.as_ref())
        .filter(|_| e.unanswered || facts.worth == Some(false))
        .or(facts.patrol.as_ref())
        .filter(|_| !e.ours.is_empty());

    match facts.worth {
        _ if defend::beaten(e) => None,
        _ if let Some(room) = onward => Some(onto(*room, facts.time, e)),
        _ if e.unanswered => None,
        Some(false) => None,
        None if hollow && !e.unanswered => None,
        _ if e.phase == Phase::Forming && !facts.asked && !e.ours.is_empty() => {
            Some(enter(Phase::Approaching, facts.time, e))
        }
        _ if e.phase == Phase::Approaching && facts.arrived => {
            Some(enter(Phase::Fighting, facts.time, e))
        }
        _ => Some(e.clone()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fabot_base::world::{Id, Remains, Store};

    fn fell(body: Vec<Part>, ticks_to_live: i32) -> Room<'static> {
        let mut room = Room::empty(RoomName::W1N1);
        room.remains.push(Remains {
            id: Id::from("t1"),
            pos: Pos::new(RoomName::W1N1, 25, 25),
            store: Store::empty(),
            fallen: Some(Fallen {
                body,
                ticks_to_live,
                died: 0,
            }),
            theirs: None,
        });
        room
    }

    #[test]
    fn a_body_is_kept_for_its_own_lifetime_and_an_aged_out_one_was_never_cut_short() {
        let held = engine::CREEP_LIFETIME;
        let aged_out = fell(vec![Part::Move], engine::AGED_OUT_TICKS_TO_LIVE);
        assert_eq!(kept(&aged_out, held, 0), held);
        let reserver = fell(vec![Part::Claim, Part::Move], 100);
        assert_eq!(kept(&reserver, held, 0), engine::CLAIM_LIFETIME - 100);
    }
}
