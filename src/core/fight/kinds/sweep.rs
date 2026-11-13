/// `Sweep` (`ARCHITECTURE.md` §6.2); `step` closes it with None.
use std::collections::BTreeSet;

use super::super::shape::{Engagement, Phase};
use crate::core::engine;
use crate::core::tactics::Tactic;
use crate::core::vocabulary::Part;
use crate::core::world::{Creep, Owner, Pos, Room, RoomName, Source};

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
    /// The stalest room out of sight, read only once its room is worth nothing.
    pub patrol: Option<RoomName>,
}

pub fn prey(room: &Room) -> Vec<&Creep> {
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

/// v1's kill value less the work lost until it is replaced.
pub fn worth(c: &Creep) -> i32 {
    let body = crate::core::world::body_kinds(c);
    let claims = body.contains(&Part::Claim);
    let life = if claims {
        engine::CLAIM_LIFETIME
    } else {
        engine::CREEP_LIFETIME
    };

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

fn enter(phase: Phase, time: i32, e: &Engagement) -> Engagement {
    Engagement {
        phase,
        since: time,
        ..e.clone()
    }
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
    let alive: BTreeSet<String> = e.ours.intersection(&facts.standing).cloned().collect();
    let mut next = e.clone();
    next.lost = e.lost + e.ours.len() as i32 - alive.len() as i32;
    next.ours = alive.difference(&facts.aging_out).cloned().collect();
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
        Some(false) if facts.elsewhere.is_some() || hollow => None,
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
