/// A colony given up (§5, owner 2026-10-18) digs, builds and upgrades nothing
/// more; its ring spawns the haulers and charges the towers that guard them.
use crate::engine;
use crate::intents::{ControllerAct, Intent, RoomAct};
use crate::tiles;
use crate::vocabulary::Resource;
use crate::world::{Creep, Kind, Room, RoomName, Structure};

use super::derived::Ground;
use super::task::Target;

fn shipped(s: &Structure) -> bool {
    s.owner.is_ours()
        && matches!(
            s.kind,
            Kind::Storage | Kind::Terminal | Kind::Lab | Kind::Factory | Kind::PowerSpawn
        )
}

/// What the room still holds to ship; the observe segment's `abandon:<room>`.
pub fn left(room: &Room) -> i32 {
    room.structures
        .iter()
        .filter(|s| shipped(s))
        .map(|s| s.store.used())
        .sum()
}

/// What stays once shipped: energy under the least send and its fee.
const LEFT: i32 = 200;

/// `destroy()` needs our controller: the unclaim waits for the last structure.
pub fn raze(room: &Room) -> Vec<Intent> {
    let Some(c) = room.controller.as_ref().filter(|c| c.owner.is_ours()) else {
        return Vec::new();
    };
    if left(room) > LEFT {
        return Vec::new();
    }
    let ours: Vec<Intent> = (room.structures.iter())
        .filter(|s| s.owner.is_ours() && s.kind.name() != "controller")
        .map(|s| Intent::Room(room.name, RoomAct::Destroy(s.id)))
        .collect();
    if ours.is_empty() {
        vec![Intent::Controller(c.id, ControllerAct::Unclaim)]
    } else {
        ours
    }
}

/// `to` is the receiving colony and its terminal's free room. Energy ships
/// last: the terminal's own pays every send's fee.
pub fn evacuate(g: &mut Ground, room: &Room, creeps: &[Creep], to: Option<(RoomName, i32)>) {
    g.abandoned = true;
    g.sources.clear();
    g.minerals.clear();
    g.sites.clear();
    g.repairs.clear();
    g.lab_plan = None;
    g.buffer = None;
    g.feed = None;
    g.receiver = None;
    // The storage's energy rides the shipment's draw alone, so no sink
    // elsewhere outbids the terminal for it.
    g.stores.retain(|s| {
        !s.kind.is_buffer()
            && !room
                .structures
                .iter()
                .any(|t| s.id == Target::Object(t.id) && shipped(t))
    });
    g.ores = room
        .structures
        .iter()
        .filter(|s| shipped(s))
        .flat_map(|s| {
            s.store
                .held
                .iter()
                .filter(|(_, n)| *n > 0)
                .map(|(r, n)| (s.id, s.pos, r.clone(), *n))
        })
        .collect();

    let mut owed: Vec<(Resource, i32)> = Vec::new();
    // A load aboard is still owed, or its carrier finds no terminal to fill.
    let aboard = creeps
        .iter()
        .filter(|c| c.owner.is_ours() && c.pos.room == room.name)
        .flat_map(|c| c.store.held.iter().cloned());
    for (r, n) in g.ores.iter().map(|o| (o.2.clone(), o.3)).chain(aboard) {
        match owed.iter_mut().find(|(each, _)| *each == r) {
            Some((_, total)) => *total += n,
            None => owed.push((r, n)),
        }
    }
    // Energy under the least send and its fee never leaves: what stays.
    let apart = to
        .as_ref()
        .map_or(0, |(home, _)| tiles::rooms_apart(g.room, *home));
    let least = engine::TERMINAL_MIN_SEND + engine::send_cost(engine::TERMINAL_MIN_SEND, apart);
    owed.retain(|(r, n)| to.is_some() && *n > 0 && (!r.is_energy() || *n >= least));
    owed.sort_by_key(|(r, _)| r.is_energy());
    g.shipment = owed;
    g.ship_to = to.filter(|_| !g.shipment.is_empty());
}
