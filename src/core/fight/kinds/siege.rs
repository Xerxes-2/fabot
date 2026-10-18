/// `Siege` (`ARCHITECTURE.md` §6.2); `step` closes it with None once nothing is left to raze.
use std::collections::BTreeSet;

use super::super::shape::{Engagement, Phase};
use crate::core::world::{Kind, Pos, Structure};

pub struct Facts {
    pub time: i32,
    pub standing: BTreeSet<String>,
    pub aging_out: BTreeSet<String>,
    pub asked: bool,
    pub home: bool,
    /// One of ours stands in the rival's room, all of it in its towers' reach.
    pub under_fire: bool,
    pub razed: bool,
    /// The rival's safe mode runs in its room.
    pub safe: bool,
}

/// The towers and the whole economy (owner, 2026-10-18), what strikes us first.
fn rank_of(kind: &Kind) -> Option<i32> {
    match kind {
        Kind::Tower => Some(0),
        Kind::Spawn => Some(1),
        Kind::Extension
        | Kind::Storage
        | Kind::Terminal
        | Kind::Link
        | Kind::Lab
        | Kind::Extractor
        | Kind::Factory
        | Kind::PowerSpawn
        | Kind::Nuker
        | Kind::Observer => Some(2),
        _ => None,
    }
}

/// The rival's structure razed next, the nearest of its rank to `near`.
pub fn next_razed<'a>(structures: &'a [Structure], near: Option<&Pos>) -> Option<&'a Structure> {
    structures
        .iter()
        .filter(|s| s.owner.is_theirs())
        .filter_map(|s| rank_of(&s.kind).map(|rank| (rank, s)))
        .min_by_key(|(rank, s)| {
            (
                *rank,
                near.and_then(|p| p.range(&s.pos)).unwrap_or(0),
                s.id.clone(),
            )
        })
        .map(|(_, s)| s)
}

fn enter(phase: Phase, time: i32, e: &Engagement) -> Engagement {
    Engagement {
        phase,
        since: time,
        ..e.clone()
    }
}

pub fn step(facts: &Facts, e: &Engagement) -> Option<Engagement> {
    let alive: BTreeSet<String> = e.ours.intersection(&facts.standing).cloned().collect();
    let mut next = e.clone();
    next.lost = e.lost + e.ours.len() as i32 - alive.len() as i32;
    next.ours = alive.difference(&facts.aging_out).cloned().collect();
    let e = &next;

    if facts.razed {
        None
    } else if e.phase == Phase::Disengaging {
        if facts.home { None } else { Some(e.clone()) }
    } else if e.lost > e.budget || facts.safe {
        Some(enter(Phase::Disengaging, facts.time, e))
    } else if e.phase == Phase::Forming && !facts.asked && !e.ours.is_empty() {
        Some(enter(Phase::Approaching, facts.time, e))
    } else if e.phase == Phase::Approaching && facts.under_fire {
        Some(enter(Phase::Fighting, facts.time, e))
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
