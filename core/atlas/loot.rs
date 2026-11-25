use std::collections::BTreeMap;

use fabot_base::declarations::Declared;
use fabot_base::tiles;
use fabot_base::vocabulary::ResourceType;
use fabot_base::world::{Creep, Id, Owner, Pos, Room, Structure, StructureType};

use super::Atlas;

pub fn invader_rampart(s: &Structure) -> bool {
    s.kind == StructureType::Rampart && s.owner == Owner::Invader
}

/// A stronghold whose core fell: its ramparts stand to its collapse.
pub fn fallen(room: &Room) -> bool {
    room.invader_core().is_none() && room.structures.iter().any(invader_rampart)
}

/// The engine names an Invader core's creeps `defender<i>`, a raid's `invader_<room>_<n>`.
pub fn cast_by_core(c: &Creep) -> bool {
    c.owner == Owner::Invader && c.name.starts_with("defender")
}

pub fn garrison(room: &Room, c: &Creep) -> bool {
    cast_by_core(c) && fallen(room)
}

pub fn invader_rampart_at<'a>(room: &'a Room, at: &Pos) -> Option<&'a Structure> {
    (room.structures.iter()).find(|s| invader_rampart(s) && s.pos == *at)
}

pub fn loot(room: &Room) -> Vec<Pos> {
    let mut at: Vec<Pos> = loot_in(room).into_iter().map(|(at, _, _)| at.pos).collect();
    at.dedup();
    at
}

pub fn fallen_with_loot(room: &Room) -> bool {
    fallen(room) && !loot(room).is_empty()
}

/// The ramparts between a carrier and loot at `at`: the one over it, and,
/// walled in on every side, those over its walkable sides.
pub fn walls_of<'a>(room: &'a Room, atlas: &Atlas, at: &Pos) -> Vec<&'a Structure> {
    let walkable = |p: &Pos| {
        (room.structures.iter())
            .all(|s| s.pos != *p || s.walkable || s.kind == StructureType::Rampart)
    };
    let open = |p: &Pos| {
        walkable(p)
            && atlas.passable(room.name, tiles::of_pos(p))
            && invader_rampart_at(room, p).is_none()
    };
    let around: Vec<Pos> = (tiles::around(tiles::of_pos(at)).iter())
        .map(|t| tiles::to_pos(room.name, *t))
        .collect();
    let sides = (!around.iter().any(open)).then_some(around.iter().filter(|p| walkable(p)));
    (invader_rampart_at(room, at).into_iter())
        .chain(
            sides
                .into_iter()
                .flatten()
                .filter_map(|p| invader_rampart_at(room, p)),
        )
        .collect()
}

/// Of every loot's `walls_of`, the fewest hits, then the most it frees.
pub fn loot_wall(room: &Room, atlas: &Atlas) -> Option<(Id, Pos)> {
    let mut walls: BTreeMap<(Id, Pos), (i32, i32)> = BTreeMap::new();
    for at in loot(room) {
        for s in walls_of(room, atlas, &at) {
            walls.entry((s.id, s.pos)).or_insert((s.hits, 0)).1 -= 1;
        }
    }
    let mut walls: Vec<_> = walls.into_iter().collect();
    walls.sort_by_key(|(at, rank)| (*rank, *at));
    // A dismantler stands beside it, so a wall walled off round the garrison waits.
    (walls.into_iter().map(|(at, _)| at)).find(|(_, at)| atlas.reaches_live(None, at))
}

/// What remains and containers not ours hold besides energy, walled or not.
pub fn loot_in(room: &Room) -> Vec<(Declared, ResourceType, i32)> {
    let containers = (room.structures.iter())
        .filter(|s| s.kind == StructureType::Container && !s.owner.is_ours())
        .map(|s| (s.id, s.pos, &s.store));
    (room.remains.iter().map(|r| (r.id, r.pos, &r.store)))
        .chain(containers)
        .flat_map(|(id, pos, store)| {
            let at = Declared { id, pos };
            (store.held.iter()).map(move |(r, n)| (at.clone(), *r, *n))
        })
        .filter(|(_, r, n)| *r != ResourceType::Energy && *n > 0)
        .collect()
}
