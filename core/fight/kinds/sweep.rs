/// `Sweep` (`ARCHITECTURE.md` §6.2); `step` closes it with None.
use std::collections::{BTreeMap, BTreeSet};

use super::super::casting;
use super::super::scene::{Hunting, Scene};
use super::super::shape::{Engagement, Phase, enter};
use fabot_atlas::{self as atlas, Atlas};
use fabot_base::engine;
use fabot_base::intel::Row;
use fabot_base::tiles;
use fabot_base::vocabulary::Part;
use fabot_base::world::{Creep, Owner, Pos, Room, RoomName, Source};
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
    pub safe: bool,
    /// The candidate a set beats that pays most a tick of its walk.
    pub elsewhere: Option<RoomName>,
    pub patrol: Option<RoomName>,
}

pub fn prey<'a>(room: &'a Room) -> Vec<&'a Creep> {
    room.hostiles()
        .filter(|h| matches!(h.owner, Owner::Enemy(_)))
        .filter(|h| {
            h.body
                .iter()
                .all(|p| p.kind != Part::Attack && p.kind != Part::RangedAttack)
                && h.body
                    .iter()
                    .any(|p| p.kind == Part::Work || p.kind == Part::Carry || p.kind == Part::Claim)
        })
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

/// Whether `e`'s bodies beat `room`'s escort as they stand, casting none.
pub(crate) fn beats(
    scene: &Scene,
    atlas: &Atlas,
    e: &Engagement,
    (room, grounds): (RoomName, &BTreeMap<RoomName, Hunting>),
) -> bool {
    let ours: Vec<Fighter> = (scene.members_of(e).into_iter())
        .map(casting::fighter_of)
        .collect();
    let boosts = scene.boosts_for(atlas, e, &scene.boosts);
    match (scene.in_sight(room), grounds.get(&room)) {
        (Some(r), Some(g)) => {
            !ours.is_empty()
                && (scene.hunt(g, r, ours, 0, &boosts)).is_some_and(|p| p.set.is_empty())
        }
        _ => false,
    }
}

/// Stalest first, beside his worked rooms too (his farther ones lie past our scouts), an
/// unseen one as stale as the stalest seen; none held by a player not an enemy, past a
/// rival's towers, or where the tracker holds an armed group (owner: the escort it left).
pub(crate) fn patrol(
    scene: &Scene,
    atlas: &Atlas,
    e: &Engagement,
    (preyed, enemies): (&[RoomName], &BTreeSet<String>),
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
        .filter(|r| *r != e.room && !towered.contains(r) && !scene.homes.contains(r))
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

/// Never home (owner).
pub fn onto(room: RoomName, time: i32, e: &Engagement) -> Engagement {
    Engagement {
        room,
        unanswered: false,
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
        _ if facts.safe => None,
        _ if let Some(room) = onward => Some(onto(*room, facts.time, e)),
        _ if e.unanswered && facts.elsewhere.is_some() => None,
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
