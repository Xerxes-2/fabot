/// A colony given up (§5, owner 2026-10-18) digs, builds and upgrades nothing
/// more; its ring spawns the haulers and charges the towers that guard them.
use fabot_base::engine;
use fabot_base::{
    intents::{Action, ControllerAct, RoomAct},
    tiles,
    vocabulary::ResourceType::{self, Energy},
    world::{Creep, Room, RoomName, Structure, StructureType},
};

use super::{
    balance::{LEAST_SEND, send_fee},
    derived::Ground,
    task::Target,
};

fn shipped(s: &Structure) -> bool {
    s.owner.is_ours()
        && matches!(
            s.kind,
            StructureType::Storage
                | StructureType::Terminal
                | StructureType::Lab
                | StructureType::Factory
                | StructureType::PowerSpawn
        )
}

/// What the room still holds to ship; the observe segment's `abandon:<room>`.
pub fn left(room: &Room) -> i32 {
    room.structures.iter().filter(|s| shipped(s)).map(|s| s.store.used()).sum()
}

/// `destroy()` needs our controller: the unclaim waits for the last structure.
/// Nothing is razed while `g`'s shipment is owed, or its terminal goes first.
pub fn raze(room: &Room, g: Option<&Ground>) -> Vec<Action> {
    let Some(c) = room.controller.as_ref().filter(|c| c.owner.is_ours()) else {
        return Vec::new();
    };
    if g.is_some_and(|g| !g.shipment.is_empty()) {
        return Vec::new();
    }
    let ours: Vec<Action> = (room.structures.iter())
        .filter(|s| s.owner.is_ours() && s.kind != StructureType::Controller)
        .map(|s| Action::Room(room.name, RoomAct::Destroy(s.id)))
        .collect();
    if ours.is_empty() { vec![Action::Controller(c.id, ControllerAct::Unclaim)] } else { ours }
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
            && !room.structures.iter().any(|t| s.id == Target::Object(t.id) && shipped(t))
    });
    g.ores = room
        .structures
        .iter()
        .filter(|s| shipped(s))
        .flat_map(|s| {
            s.store.held.iter().filter(|(_, n)| *n > 0).map(|(r, n)| (s.id, s.pos, *r, *n))
        })
        .collect();

    let mut owed: Vec<(ResourceType, i32)> = Vec::new();
    // A load aboard is still owed, or its carrier finds no terminal to fill.
    let aboard = creeps
        .iter()
        .filter(|c| c.owner.is_ours() && c.pos.room == room.name)
        .flat_map(|c| c.store.held.iter().copied());
    for (r, n) in g.ores.iter().map(|o| (o.2, o.3)).chain(aboard) {
        match owed.iter_mut().find(|(each, _)| *each == r) {
            Some((_, total)) => *total += n,
            None => owed.push((r, n)),
        }
    }
    let apart = to.as_ref().map_or(0, |(home, _)| tiles::rooms_apart(g.room, *home));
    let energy_kept_under = LEAST_SEND + engine::send_cost(LEAST_SEND, apart);
    let held = |ore: bool| (owed.iter()).filter(move |(r, _)| (*r != Energy) == ore);
    // What the room's energy cannot send is no shipment: the raze waits on none.
    let paid = held(false).map(|o| o.1).sum::<i32>() >= send_fee(held(true).map(|o| o.1), apart);
    let kept = |r, n| (r == Energy && n >= energy_kept_under) || (r != Energy && paid);
    owed.retain(|(r, n)| to.is_some() && *n > 0 && kept(*r, *n));
    owed.sort_by_key(|(r, _)| *r == Energy);
    g.shipment = owed;
    g.ship_to = to.filter(|_| !g.shipment.is_empty());
}
