//! A declared outpost has no spawn to anchor a plan on, so its container and
//! road are placed off the haul's own walk home.
use std::collections::{BTreeMap, BTreeSet};

use crate::core::HashMap;

use crate::core::atlas::{Atlas, Mover, Target, Tree};
use crate::core::grid::Footprint;
use crate::core::intents::{Intent, RoomAct};
use crate::core::tick::{self, Outpost};
use crate::core::tiles::{self, Tile, TileSet};
use crate::core::world::{Kind, Owner, Pos, Room, RoomName, World, terrain};

use super::sites::SITES_IN_FLIGHT;

const HAULER: Mover = Mover {
    footprint: Footprint::One,
    factor: crate::core::grid::FatigueFactor {
        weight: 2,
        moves: 1,
    },
};

/// Bounds a walk that never arrives.
const LONGEST: usize = ((tick::REACH_HOPS + 1) * 2 * tiles::SIDE) as usize;

/// The engine refuses a site under anyone else's hold.
fn ours(room: &Room) -> bool {
    room.controller.as_ref().is_none_or(|c| match &c.owner {
        Owner::Ours => true,
        Owner::Nobody => c.reservation.as_ref().is_none_or(|(by, _)| by.is_ours()),
        _ => false,
    })
}

/// The engine builds nothing on the border.
fn walk(atlas: &mut Atlas, target: &Target, from: &Pos) -> Vec<Pos> {
    let mut path: Vec<Pos> = Vec::new();
    let mut room = from.room;
    let mut tile = tiles::of_pos(from);
    let mut field = atlas.field(HAULER, target, from);

    while field.is_some() && path.len() < LONGEST {
        let step = field
            .as_ref()
            .and_then(|f| f.step(tile))
            .and_then(|direction| tiles::neighbour(tile, direction));

        match step {
            None => field = None,
            Some(next) => {
                if tiles::on_edge(next) {
                    match (
                        tiles::room_toward(room, tiles::exit_of(next)),
                        tiles::landing(next),
                    ) {
                        (Some(onto), Some(landing)) => {
                            room = onto;
                            tile = landing;
                            field = atlas.field(HAULER, target, &tiles::to_pos(room, tile));
                        }
                        _ => field = None,
                    }
                } else {
                    tile = next;
                    path.push(tiles::to_pos(room, tile));
                }
            }
        }
    }

    path
}

fn opened(
    taken: &mut BTreeMap<RoomName, TileSet>,
    pending: &mut BTreeMap<RoomName, i32>,
    planned: &[Intent],
    room: &Room,
) {
    if taken.contains_key(&room.name) {
        return;
    }

    let mut tiles_open = TileSet::default();

    for s in &room.structures {
        if !s.kind.is_rampart() {
            tiles_open.insert(tiles::of_pos(&s.pos));
        }
    }

    for s in &room.sites {
        tiles_open.insert(tiles::of_pos(&s.pos));
    }

    let mut n = room.sites.iter().filter(|s| s.owner.is_ours()).count() as i32;

    for i in planned {
        if let Intent::Room(r, RoomAct::CreateSite(x, y, _)) = i
            && r == &room.name
        {
            tiles_open.insert(tiles::index(*x, *y));
            n += 1;
        }
    }

    taken.insert(room.name, tiles_open);
    pending.insert(room.name, n);
}

#[allow(clippy::too_many_arguments)]
fn place(
    rooms: &BTreeMap<RoomName, &Room>,
    taken: &mut BTreeMap<RoomName, TileSet>,
    pending: &mut BTreeMap<RoomName, i32>,
    planned: &[Intent],
    budget: &mut i32,
    intents: &mut Vec<Intent>,
    pos: &Pos,
    kind: &Kind,
) {
    let room = match rooms.get(&pos.room) {
        Some(room) => *room,
        None => return,
    };

    if *budget <= 0 {
        return;
    }

    opened(taken, pending, planned, room);
    let tile = tiles::of_pos(pos);
    let now = pending[&room.name];

    if now < SITES_IN_FLIGHT && taken.get_mut(&room.name).unwrap().insert(tile) {
        pending.insert(room.name, now + 1);
        *budget -= 1;
        intents.push(Intent::Room(
            room.name,
            RoomAct::CreateSite(pos.x, pos.y, kind.clone()),
        ));
    }
}

fn sink_for<'t>(
    atlas: &mut Atlas,
    trees: &dyn Fn(RoomName) -> Option<&'t Tree>,
    homes: &[Pos],
    rooms: &BTreeMap<RoomName, &Room>,
    room: &Room,
) -> Option<Pos> {
    let from = room
        .controller
        .as_ref()
        .map(|c| c.pos)
        .or_else(|| room.sources.first().map(|s| s.pos))?;

    let storage = |i: usize| {
        let home = rooms.get(&homes[i].room)?;
        let s = home
            .structures
            .iter()
            .find(|s| s.kind.is_storage() && s.owner.is_ours())?;
        Some(s.pos)
    };

    let i = tick::nearest(atlas, trees, homes, |i| storage(i).is_some(), &from)?;
    storage(i)
}

/// v1's seat: the shortest walk out toward home. The owner's rule: the walk
/// is paved once its container stands.
pub fn sites(
    atlas: &mut Atlas,
    world: &World,
    outposts: &[Outpost],
    planned: &[Intent],
    budget: i32,
) -> Vec<Intent> {
    let mut intents: Vec<Intent> = Vec::new();
    let mut budget = budget;
    let mut taken: BTreeMap<RoomName, TileSet> = BTreeMap::new();
    let mut pending: BTreeMap<RoomName, i32> = BTreeMap::new();
    let mut rooms: BTreeMap<RoomName, &Room> = BTreeMap::new();

    let colonies = tick::colonies(world);

    for room in &world.rooms {
        if colonies.contains(&room.name)
            || (outposts.iter().any(|o| o.room == room.name) && ours(room))
        {
            rooms.insert(room.name, room);
        }
    }

    // Every outpost in one `sites` is priced to the same homes: grow each
    // home's tree once.
    let trees: HashMap<RoomName, Tree> = (colonies.iter())
        .map(|home| (*home, atlas.tree(&BTreeSet::new(), tick::REACH_HOPS, *home)))
        .collect();

    let homes: Vec<Pos> = colonies
        .iter()
        .filter_map(|c| {
            world
                .spawns
                .iter()
                .find(|s| &s.pos.room == c)
                .map(|s| s.pos)
        })
        .collect();

    for o in outposts {
        let room = match rooms.get(&o.room) {
            Some(room) => *room,
            None => continue,
        };

        // A room at its site limit places nothing: the walk is not drawn.
        opened(&mut taken, &mut pending, planned, room);

        if budget > 0 && pending[&room.name] < SITES_IN_FLIGHT {
            let sink = sink_for(atlas, &|room| trees.get(&room), &homes, &rooms, room);

            if let Some(sink) = sink {
                let home = tick::target_at(&sink, 1);

                for s in &room.sources {
                    match Atlas::post_of(room, &s.pos) {
                        Some((post, Some(_))) => {
                            for p in walk(atlas, &home, &post) {
                                place(
                                    &rooms,
                                    &mut taken,
                                    &mut pending,
                                    planned,
                                    &mut budget,
                                    &mut intents,
                                    &p,
                                    &Kind::Road,
                                );
                            }
                        }
                        Some((_, None)) => (),
                        None => {
                            let open_tiles = taken[&room.name].clone();
                            let mut seats: Vec<(i32, Tile)> = Vec::new();

                            for t in tiles::within(1, tiles::of_pos(&s.pos)) {
                                if t != tiles::of_pos(&s.pos)
                                    && !tiles::on_edge(t)
                                    && !open_tiles.contains(t)
                                    && !terrain::is_wall(
                                        room.terrain.as_deref(),
                                        tiles::x_of(t),
                                        tiles::y_of(t),
                                    )
                                    && let Some(ticks) = Atlas::travel_ticks(
                                        atlas,
                                        HAULER,
                                        &home,
                                        &tiles::to_pos(room.name, t),
                                    )
                                {
                                    seats.push((ticks, t));
                                }
                            }

                            seats.sort_unstable();

                            if let Some((_, t)) = seats.first() {
                                place(
                                    &rooms,
                                    &mut taken,
                                    &mut pending,
                                    planned,
                                    &mut budget,
                                    &mut intents,
                                    &tiles::to_pos(room.name, *t),
                                    &Kind::Container,
                                );
                            }
                        }
                    }
                }
            }
        }
    }

    intents
}
