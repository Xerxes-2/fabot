//! Built from the room alone, so a plan drawn on it moves only when the
//! ground does.
use crate::tiles::{self, Tile};
use crate::world::{Kind, Room, RoomName, terrain};

use super::place::{SWAMP, WALL};

#[derive(Clone, Debug)]
pub struct Ground {
    pub room: RoomName,
    /// Hivemind's terrain codes, 0 plain, 1 wall, 2 swamp; another's
    /// structure no creep crosses is a wall. Our own are not read: the plan is
    /// drawn on the ground, never on itself.
    pub terrain: Vec<u8>,
    pub sources: Vec<Tile>,
    pub minerals: Vec<Tile>,
    pub controller: Option<Tile>,
}

/// The only part of a room's ground that can change.
pub fn obstacles(room: &Room) -> Vec<Tile> {
    let mut found: Vec<Tile> = room
        .structures
        .iter()
        .filter(|s| !s.owner.is_ours() && !s.walkable)
        .map(|s| (s, tiles::of_pos(&s.pos)))
        // A wall has no owner: off the border, where the server alone builds, it is our ring's.
        .filter_map(|(s, t)| (s.kind != Kind::Wall || tiles::on_edge(t)).then_some(t))
        .collect();
    found.sort_unstable();
    found
}

pub fn of_room(room: &Room) -> Option<Ground> {
    let mut codes: Vec<u8> = (room.terrain?.get_bits().iter())
        .map(|t| {
            if t & terrain::WALL != 0 {
                WALL
            } else if t & terrain::SWAMP != 0 {
                SWAMP
            } else {
                0
            }
        })
        .collect();
    // The engine stands rocks and controllers on wall; a crafted room may not.
    let fixed = (room.sources.iter().map(|s| &s.pos))
        .chain(room.minerals.iter().map(|m| &m.pos))
        .chain(room.controller.iter().map(|c| &c.pos));
    for t in obstacles(room).into_iter().chain(fixed.map(tiles::of_pos)) {
        codes[t.index()] = WALL;
    }
    let sorted = |mut v: Vec<Tile>| {
        v.sort_unstable();
        v
    };
    Some(Ground {
        room: room.name,
        terrain: codes,
        sources: sorted(room.sources.iter().map(|s| tiles::of_pos(&s.pos)).collect()),
        minerals: sorted(
            room.minerals
                .iter()
                .map(|m| tiles::of_pos(&m.pos))
                .collect(),
        ),
        controller: room.controller.as_ref().map(|c| tiles::of_pos(&c.pos)),
    })
}
