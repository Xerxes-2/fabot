/// `Siege` (`ARCHITECTURE.md` §6.2); `step` closes it with None once nothing is left to take.
use std::collections::BTreeSet;

use super::super::casting;
use super::super::shape::{Engagement, Phase};
use crate::core::engine;
use crate::core::tactics::Tactic;
use crate::core::vocabulary::Resource;
use crate::core::world::{Controller, Kind, Owner, Pos, Room, Structure};

pub const CANDIDATES: &[Tactic] = &[Tactic::BlobClose];

/// Bouncer trips his towers may leave untouched before the drain gives up.
pub const IGNORED: i32 = 6;

/// The drain gave up: the engagement holds the edge, casting nothing more.
pub fn gave_up(e: &Engagement) -> bool {
    e.ignored >= IGNORED || casting::drains(e.capacity).is_empty()
}

pub struct Facts {
    pub time: i32,
    pub standing: BTreeSet<String>,
    pub aging_out: BTreeSet<String>,
    pub asked: bool,
    pub home: bool,
    /// One of ours, no picket, stands in his room: what opens the breach.
    pub under_fire: bool,
    pub charged: bool,
    /// A gate stands a drain works: his towers not out-striking the heal the set seats.
    pub drain: bool,
    /// His towers leave a border tile the pairs cover: the most a drain can pay.
    pub priced: bool,
    /// A pair stands or has tripped: a drain that never cast one holds nothing.
    pub worked: bool,
    /// A bouncer is back from his room this tick, and whether a tower answered.
    pub bounced: bool,
    pub drawn: bool,
    pub gave_up: bool,
    pub razed: bool,
    /// The rival's safe mode runs in its room, or the owner withdrew the siege.
    pub safe: bool,
    pub withdrawn: bool,
}

/// The towers and the whole economy (owner, 2026-10-18).
fn razed(kind: &Kind) -> bool {
    matches!(
        kind,
        Kind::Tower
            | Kind::Spawn
            | Kind::Extension
            | Kind::Storage
            | Kind::Terminal
            | Kind::Link
            | Kind::Lab
            | Kind::Extractor
            | Kind::Factory
            | Kind::PowerSpawn
            | Kind::Nuker
            | Kind::Observer
    )
}

/// What razing `s` takes from the rival (owner, 2026-10-19): a fixed order a
/// price of his loss replaces.
fn taken(s: &Structure, sources: &[Pos]) -> i32 {
    match s.kind {
        Kind::Link if sources.iter().any(|p| p.within(2, &s.pos)) => 4,
        Kind::Spawn | Kind::Extension | Kind::Tower => 3,
        Kind::Storage | Kind::Terminal | Kind::Link => 2,
        Kind::Extractor => 0,
        _ => 1,
    }
}

/// The rival's structure razed next: the one its charged towers strike
/// least, so the set the bank buys is the cheapest (owner: a tower is razed
/// whenever that is cheap); then the nearest `near`.
fn next_razed<'a>(room: &'a Room, near: Option<&Pos>) -> Option<&'a Structure> {
    let structures = &room.structures;
    let sources: Vec<Pos> = room.sources.iter().map(|s| s.pos.clone()).collect();
    let theirs = || structures.iter().filter(|s| s.owner.is_theirs());
    let struck = |at: &Pos| -> i32 {
        theirs()
            .filter(|t| {
                t.kind.is_tower()
                    && t.store.amount_of(&Resource::Energy) >= engine::TOWER_ENERGY_COST
            })
            .filter_map(|t| t.pos.range(at))
            .map(engine::tower_attack_at)
            .sum()
    };

    theirs().filter(|s| razed(&s.kind)).min_by_key(|s| {
        (
            struck(&s.pos),
            std::cmp::Reverse(taken(s, &sources)),
            near.and_then(|p| p.range(&s.pos)).unwrap_or(0),
            s.id.clone(),
        )
    })
}

fn declaimed(room: &Room) -> Option<&Controller> {
    (room.controller.as_ref()).filter(|c| matches!(c.owner, Owner::Enemy(_)))
}

pub fn next_target(room: &Room, near: Option<&Pos>) -> Option<String> {
    let razed = next_razed(room, near).map(|s| &s.id);
    razed.or(declaimed(room).map(|c| &c.id)).cloned()
}

pub fn tapped<'a>(room: &'a Room, target: &str) -> Option<&'a Controller> {
    declaimed(room).filter(|c| c.id == target)
}

/// The controller stands among the structures, a target only while tapped.
pub fn stands(room: &Room, target: &str) -> bool {
    match &room.controller {
        Some(c) if c.id == target => tapped(room, target).is_some(),
        _ => room.structures.iter().any(|s| s.id == target),
    }
}

/// Asked as early as a body cast now outlives the block's lapse by a room's
/// side (v1), so the spawn queue delays its start but not its tap.
pub fn tap_due(c: &Controller, spawn: i32) -> bool {
    c.upgrade_blocked <= spawn + engine::CLAIM_LIFETIME - engine::ROOM_SIDE
}

fn enter(phase: Phase, time: i32, e: &Engagement) -> Engagement {
    Engagement {
        phase,
        since: time,
        ignored: if phase == Phase::Draining {
            0
        } else {
            e.ignored
        },
        ..e.clone()
    }
}

pub fn step(facts: &Facts, e: &Engagement) -> Option<Engagement> {
    let alive: BTreeSet<String> = e.ours.intersection(&facts.standing).cloned().collect();
    let mut next = e.clone();
    next.lost = e.lost + e.ours.len() as i32 - alive.len() as i32;
    next.ours = alive.difference(&facts.aging_out).cloned().collect();
    next.ignored = match (facts.bounced, facts.drawn) {
        (true, true) => 0,
        (true, false) if facts.charged => (e.ignored + 1).min(IGNORED),
        _ => e.ignored,
    };
    let e = &next;

    // A drain holds the set while a pair works it — his room priced, or a gate
    // at its edge — and never once it fights; a safe mode it cannot work stands off.
    let holds = facts.drain
        || (facts.priced
            && !matches!(
                e.phase,
                Phase::Fighting | Phase::Pursuing | Phase::Disengaging
            )
            && (facts.worked || !facts.gave_up));
    let draining = holds
        && e.phase != Phase::Forming
        && (facts.drain || (e.phase == Phase::Draining && facts.charged && facts.worked));
    let breached = if facts.safe {
        Phase::Disengaging
    } else {
        Phase::Fighting
    };

    if facts.razed {
        None
    } else if e.phase == Phase::Disengaging {
        if facts.home { None } else { Some(e.clone()) }
    } else if facts.withdrawn || (facts.safe && !holds) {
        Some(enter(Phase::Disengaging, facts.time, e))
    } else if e.phase == Phase::Forming && !facts.asked && !e.ours.is_empty() {
        Some(enter(Phase::Approaching, facts.time, e))
    } else if draining {
        Some(if e.phase == Phase::Draining {
            e.clone()
        } else {
            enter(Phase::Draining, facts.time, e)
        })
    } else if e.phase == Phase::Draining || (e.phase == Phase::Approaching && facts.under_fire) {
        Some(enter(breached, facts.time, e))
    } else {
        Some(e.clone())
    }
}

/// A plan re-priced while fighting is fought from now; none disengages.
pub fn replanned(time: i32, held: bool, e: &Engagement) -> Engagement {
    match e.phase {
        _ if !held => enter(Phase::Disengaging, time, e),
        Phase::Fighting => enter(Phase::Fighting, time, e),
        _ => e.clone(),
    }
}
