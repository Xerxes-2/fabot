/// `Siege` (`ARCHITECTURE.md` §6.2); `step` closes it with None once nothing is left to take.
use std::collections::{BTreeMap, BTreeSet};

use super::super::casting;
use super::super::shape::{self, Engagement, Phase};
use super::sweep;
use fabot_atlas::Atlas;
use fabot_base::engine;
use fabot_base::tiles::{self, Tile, TileSet};
use fabot_base::world::{Controller, Id, Pos, Room, RoomName, Rules, Structure, StructureType};
use fabot_exchange::{self as exchange, Outcome};
use fabot_tactics::{self as tactics, Offer, Tactic};

pub const CANDIDATES: &[Tactic] = &[Tactic::BlobClose];

/// A set shot in through a way no barrier stands on: it dismantles nothing.
pub const RAID: &[Tactic] = &[Tactic::BlobKite];

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
pub fn razed(kind: StructureType) -> bool {
    use StructureType::*;
    matches!(
        kind,
        Tower
            | Spawn
            | Extension
            | Storage
            | Terminal
            | Link
            | Lab
            | Extractor
            | Factory
            | PowerSpawn
            | Nuker
            | Observer
    )
}

/// What razing `s` takes from the rival (owner, 2026-10-19): a fixed order a
/// price of his loss replaces.
pub fn taken(s: &Structure, sources: &[Pos]) -> i32 {
    use StructureType::*;
    match s.kind {
        Link if sources.iter().any(|p| p.within(2, &s.pos)) => 4,
        Spawn | Extension | Tower => 3,
        Storage | Terminal | Link => 2,
        Extractor => 0,
        _ => 1,
    }
}

/// The rival's structure razed next: the one its charged towers strike
/// least, so the set the bank buys is the cheapest (owner: a tower is razed
/// whenever that is cheap); then the dearest to him, then the nearest `walk`.
pub fn next_razed<'a>(
    rules: &Rules,
    room: &'a Room,
    walk: impl Fn(&Structure) -> i32,
) -> Option<&'a Structure> {
    let sources: Vec<Pos> = room.sources.iter().map(|s| s.pos).collect();
    let theirs = || room.structures.iter().filter(|s| s.owner.is_theirs());
    let charged = super::raid::charged(rules, room);

    theirs().filter(|s| razed(s.kind)).min_by_key(|&s| {
        (
            exchange::tower_strike(&charged, &s.pos),
            std::cmp::Reverse(taken(s, &sources)),
            walk(s),
            s.id,
        )
    })
}

fn declaimed<'a>(room: &'a Room) -> Option<&'a Controller> {
    (room.controller.as_ref()).filter(|c| c.owner.is_enemy())
}

pub fn next_target(rules: &Rules, room: &Room, near: Option<&Pos>) -> Option<Id> {
    let razed = next_razed(rules, room, |s| {
        near.and_then(|p| p.range(&s.pos)).unwrap_or(0)
    })
    .map(|s| s.id);
    razed.or(declaimed(room).map(|c| c.id))
}

pub fn tapped<'a>(room: &'a Room, target: Id) -> Option<&'a Controller> {
    declaimed(room).filter(|c| c.id == target)
}

pub fn stands(room: &Room, target: Id) -> bool {
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
    let ignored = if phase == Phase::Draining {
        0
    } else {
        e.ignored
    };
    Engagement {
        ignored,
        ..shape::enter(phase, time, e)
    }
}

pub fn step(facts: &Facts, e: &Engagement) -> Option<Engagement> {
    let mut next = super::pruned(e, &facts.standing, &facts.aging_out);
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

/// What a siege breaches: a wall is nobody's, an ally's rampart never ours to raze.
pub fn bars(s: &Structure) -> bool {
    matches!(s.kind, StructureType::Rampart | StructureType::Wall) && !s.owner.is_friend()
}

pub fn barrier_at<'a>(room: &'a Room) -> BTreeMap<Id, &'a Structure> {
    (room.structures.iter())
        .filter(|s| bars(s))
        .map(|s| (s.id, s))
        .collect()
}

pub fn beside(reached_from_exits: &TileSet, at: &Pos) -> bool {
    tiles::around(tiles::of_pos(at))
        .iter()
        .any(|t| reached_from_exits.contains(*t))
}

/// A picket's worth, as a sweep's: the harvest its post denies.
pub fn denied(room: &Room, post: &Pos) -> i32 {
    room.sources
        .iter()
        .filter(|s| s.pos == *post)
        .map(|s| sweep::yield_of(s, engine::CREEP_LIFETIME))
        .sum()
}

/// Each source a picket denies him from and the squad that holds it under his towers.
pub fn posts(
    atlas: &Atlas,
    room: &Room,
    capacity: i32,
    towers: &[Pos],
) -> Vec<(Pos, Vec<casting::Body>)> {
    (exposed(atlas, room).into_iter())
        .map(|post| {
            let strike = exchange::tower_strike(towers, &post);
            (post, casting::squad(capacity, strike, denied(room, &post)))
        })
        .collect()
}

pub fn exposed(atlas: &Atlas, room: &Room) -> Vec<Pos> {
    exposed_in(&atlas.from_exits(room.name), room)
}

pub fn exposed_in(reached_from_exits: &TileSet, room: &Room) -> Vec<Pos> {
    room.sources
        .iter()
        .filter(|s| beside(reached_from_exits, &s.pos))
        .map(|s| s.pos)
        .collect()
}

/// The gate a drain works from `from`: one his volley leaves the pairs the
/// bank buys healing over (§6.2).
pub fn drainable(
    atlas: &Atlas,
    room: RoomName,
    from: RoomName,
    capacity: i32,
    towers: &[Pos],
) -> Option<tactics::Gate> {
    if towers.is_empty() {
        return None;
    }
    let gate = gate(atlas, room, from, towers)?;
    let at = tiles::landing(gate.bounce).unwrap_or(gate.bounce);
    let volley = exchange::tower_strike(towers, &tiles::to_pos(room, at));

    (volley <= casting::pair_heal(casting::pairs(capacity))).then_some(gate)
}

/// The edge a drain bounces through: his border tile his towers reach, the
/// nearest of them.
fn gate(atlas: &Atlas, room: RoomName, from: RoomName, towers: &[Pos]) -> Option<tactics::Gate> {
    let side = tiles::exit_toward(room, from)?;
    let nearest = |at: &Pos| {
        (towers.iter()).fold(engine::ROOM_SIDE, |least, t| {
            least.min(t.range(at).unwrap_or(least))
        })
    };
    let tiles: Vec<Tile> = (tiles::exit_tiles(side).iter())
        .copied()
        .filter(|t| atlas.passable(room, *t))
        .filter(|t| tiles::landing(*t).is_some_and(|l| atlas.passable(from, l)))
        .collect();

    // A trip his towers cannot answer draws nothing, so a tile one of them
    // reaches is the gate before a quieter one is.
    let reached = |at: &Pos| {
        towers
            .iter()
            .any(|t| t.within(engine::TOWER_FALLOFF_RANGE, at))
    };
    let tile = tiles.into_iter().min_by_key(|t| {
        let at = tiles::to_pos(room, *t);
        (!reached(&at), nearest(&at), *t)
    })?;
    let bounce = tiles::landing(tile)?;
    let rest = (tiles::around(bounce).iter())
        .filter(|t| !tiles::on_edge(**t))
        .filter(|t| atlas.passable(from, **t))
        .min_by_key(|t| tiles::range(**t, bounce))
        .copied()?;

    Some(tactics::Gate {
        room,
        from,
        bounce,
        rest,
    })
}

/// What a `Siege`'s set wins against what breaking in spends (§6.3).
pub fn offers(
    candidates: &[Tactic],
    room: &Room,
    set: &[casting::Body],
    outcome: Outcome,
) -> Vec<Offer> {
    let value = room
        .sources
        .iter()
        .map(|s| sweep::yield_of(s, engine::CREEP_LIFETIME))
        .sum();
    let cost = casting::spent(set, outcome.losses(), outcome.ticks(), 0);

    candidates
        .iter()
        .map(|&tactic| Offer {
            tactic,
            bodies: set.len() as i32,
            value,
            cost,
        })
        .collect()
}
