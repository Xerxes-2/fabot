/// A colony declared `abandon` is emptied into another colony's terminal for
/// the owner to unclaim by hand (owner, 2026-10-18): it digs, builds, repairs
/// and upgrades nothing more, and keeps only its ring, which spawns the
/// haulers and charges the towers that guard them.
use crate::core::engine;
use crate::core::tiles;
use crate::core::vocabulary::Resource;
use crate::core::world::{Kind, Room, Structure};

use super::derived::Ground;

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

/// `to` is the receiving colony and its terminal's free room. Energy ships
/// last: the terminal's own pays every send's fee.
pub fn evacuate(g: &mut Ground, room: &Room, to: Option<(String, i32)>) {
    g.abandoned = true;
    g.sources.clear();
    g.minerals.clear();
    g.sites.clear();
    g.repairs.clear();
    g.income = 0;
    g.posted = 0;
    g.lab_plan = None;
    g.buffer = None;
    g.feed = None;
    g.receiver = None;
    let terminal = g.terminal.as_ref().map(|t| t.id.clone());
    g.stores
        .retain(|s| !s.kind.is_buffer() && Some(&s.id) != terminal.as_ref());

    // The storage's energy is the stores' draw already.
    g.ores = room
        .structures
        .iter()
        .filter(|s| shipped(s))
        .flat_map(|s| {
            s.store
                .held
                .iter()
                .filter(|(r, n)| *n > 0 && !(r.is_energy() && s.kind.is_storage()))
                .map(|(r, n)| (s.id.clone(), s.pos.clone(), r.clone(), *n))
        })
        .collect();

    let mut owed: Vec<(Resource, i32)> = Vec::new();
    let stored = g
        .stores
        .iter()
        .filter(|s| s.kind.is_storage())
        .map(|s| (Resource::Energy, s.energy));
    for (r, n) in g.ores.iter().map(|o| (o.2.clone(), o.3)).chain(stored) {
        match owed.iter_mut().find(|(each, _)| *each == r) {
            Some((_, total)) => *total += n,
            None => owed.push((r, n)),
        }
    }
    // Energy under the least send and its fee never leaves: what stays.
    let apart = to
        .as_ref()
        .map_or(0, |(home, _)| tiles::rooms_apart(&g.room, home));
    let least = engine::TERMINAL_MIN_SEND + engine::send_cost(engine::TERMINAL_MIN_SEND, apart);
    owed.retain(|(r, n)| *n > 0 && (!r.is_energy() || *n >= least));
    owed.sort_by_key(|(r, _)| r.is_energy());
    g.shipment = owed;
    g.ship_to = to.filter(|_| !g.shipment.is_empty());
}
