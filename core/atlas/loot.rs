use fabot_base::declarations::Declared;
use fabot_base::tiles;
use fabot_base::vocabulary::ResourceType;
use fabot_base::world::{Pos, Room, StructureType};

use super::Atlas;

/// What remains and containers hold besides energy where no rampart not ours
/// stands over them and a body can stand beside: what a carrier can take.
pub fn loot_in(room: &Room, atlas: &Atlas) -> Vec<(Declared, ResourceType, i32)> {
    let reached = |at: &Pos| {
        let here = tiles::of_pos(at);
        std::iter::once(&here)
            .chain(tiles::around(here))
            .any(|t| atlas.passable(at.room, *t))
    };
    let walled = |at: &Pos| {
        (room.structures.iter())
            .any(|s| s.kind == StructureType::Rampart && s.pos == *at && !s.owner.is_ours())
    };
    let containers = (room.structures.iter())
        .filter(|s| s.kind == StructureType::Container && !s.owner.is_ours())
        .map(|s| (s.id, s.pos, &s.store));
    (room.remains.iter().map(|r| (r.id, r.pos, &r.store)))
        .chain(containers)
        .filter(|(_, at, _)| !walled(at) && reached(at))
        .flat_map(|(id, pos, store)| {
            let at = Declared { id, pos };
            (store.held.iter()).map(move |(r, n)| (at.clone(), *r, *n))
        })
        .filter(|(_, r, n)| *r != ResourceType::Energy && *n > 0)
        .collect()
}
