//! A declared outpost has no spawn to anchor a plan on, so its container and
//! road are placed off the haul's own walk home.
use std::collections::BTreeMap;

use fabot_atlas::{self as atlas, Atlas, Mover};
use fabot_base::declarations::{self, ColonyPlan};
use fabot_base::grid::Footprint;
use fabot_base::intents::{Action, RoomAct};
use fabot_base::tiles::{self, Tile, TileSet};
use fabot_base::world::{Controller, Pos, Room, RoomName, StructureType, World, terrain};

use super::shape::Plan;
use super::sites::SITES_IN_FLIGHT;

const HAULER: Mover = Mover {
    footprint: Footprint::One,
    factor: fabot_base::grid::FatigueFactor {
        weight: 2,
        moves: 1,
    },
};

/// Bounds a walk that never arrives.
const LONGEST: usize = ((declarations::REACH_HOPS + 1) * 2 * tiles::SIDE) as usize;

fn opened(
    taken: &mut BTreeMap<RoomName, TileSet>,
    pending: &mut BTreeMap<RoomName, i32>,
    planned: &[Action],
    room: &Room,
) {
    if taken.contains_key(&room.name) {
        return;
    }

    let mut tiles_open: TileSet = (room.structures.iter())
        .filter(|s| s.kind != StructureType::Rampart)
        .map(|s| &s.pos)
        .chain(room.sites.iter().map(|s| &s.pos))
        .map(tiles::of_pos)
        .collect();

    let mut n = room.sites.iter().filter(|s| s.owner.is_ours()).count() as i32;

    for i in planned {
        if let Action::Room(r, RoomAct::CreateSite(x, y, _)) = i
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
    planned: &[Action],
    budget: &mut i32,
    intents: &mut Vec<Action>,
    pos: &Pos,
    kind: StructureType,
) {
    let Some(&room) = rooms.get(&pos.room) else {
        return;
    };

    if *budget <= 0 {
        return;
    }

    opened(taken, pending, planned, room);
    let (Some(open), Some(now)) = (taken.get_mut(&room.name), pending.get_mut(&room.name)) else {
        return;
    };

    if *now < SITES_IN_FLIGHT && open.insert(tiles::of_pos(pos)) {
        *now += 1;
        *budget -= 1;
        intents.push(Action::Room(
            room.name,
            RoomAct::CreateSite(pos.x, pos.y, kind),
        ));
    }
}

/// The haul's end, at the colony Economy hauls to (`atlas::server`).
fn sink_for<'p>(
    atlas: &mut Atlas,
    (world, derived): (&World, &atlas::Derived),
    planned: &dyn Fn(RoomName) -> Option<&'p ColonyPlan>,
    room: &Room,
) -> Option<Pos> {
    let anchors = (
        room.controller.as_ref(),
        room.sources.first().map(|s| s.pos),
    );
    let home = derived.colonies[atlas::server_of(atlas, derived, planned, anchors)?];

    (world.room(home)?.storage())
        .map(|s| s.pos)
        .or_else(|| planned(home)?.pile)
}

/// v1's seat: the shortest walk out toward home. The owner's rule: the walk
/// is paved once its container stands.
pub fn sites<'p>(
    atlas: &mut Atlas,
    world: &World,
    derived: &atlas::Derived,
    (planned, plan_of): (&[Action], &dyn Fn(RoomName) -> Option<&'p ColonyPlan>),
    plans: &[&Plan],
    mut budget: i32,
) -> Vec<Action> {
    let (colonies, outposts) = (&derived.colonies, &derived.worked);
    let mut intents: Vec<Action> = Vec::new();
    let mut taken: BTreeMap<RoomName, TileSet> = BTreeMap::new();
    let mut pending: BTreeMap<RoomName, i32> = BTreeMap::new();
    let mut rooms: BTreeMap<RoomName, &Room> = BTreeMap::new();

    for room in &world.rooms {
        if colonies.contains(&room.name)
            || (outposts.iter().any(|o| o.room == room.name)
                && room.controller.as_ref().is_none_or(Controller::open_to_us))
        {
            rooms.insert(room.name, room);
        }
    }

    // A planned structure would bury the road.
    let mut held: BTreeMap<RoomName, TileSet> = BTreeMap::new();
    for plan in plans {
        let solid = held.entry(plan.room).or_default();
        for t in plan.solid() {
            solid.insert(t);
        }
    }

    for o in outposts {
        let Some(&room) = rooms.get(&o.room) else {
            continue;
        };

        // A room at its site limit places nothing: the walk is not drawn.
        opened(&mut taken, &mut pending, planned, room);

        if budget > 0 && pending[&room.name] < SITES_IN_FLIGHT {
            let sink = sink_for(atlas, (world, derived), plan_of, room);

            if let Some(sink) = sink {
                let home = atlas::target_at(&sink, 1);

                // A reserver acts from beside the controller: a post there
                // takes a tile no other seat can give it.
                let beside = |p: &Pos| (room.controller.iter()).any(|c| c.pos.within(1, p));
                for s in &room.sources {
                    let road_to = match Atlas::post_of(room, &s.pos) {
                        Some((_, None)) => continue,
                        Some((post, Some(_))) if !beside(&post) => Some(post),
                        held => {
                            let mut seats: Vec<(bool, i32, Tile)> = Vec::new();

                            for t in tiles::within(1, tiles::of_pos(&s.pos)) {
                                if t != tiles::of_pos(&s.pos)
                                    && !tiles::on_edge(t)
                                    && !taken[&room.name].contains(t)
                                    && !terrain::is_wall(
                                        room.terrain,
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
                                    seats.push((beside(&tiles::to_pos(room.name, t)), ticks, t));
                                }
                            }

                            match (seats.iter().min(), held) {
                                (Some(&(on, _, t)), held) if !on || held.is_none() => {
                                    place(
                                        &rooms,
                                        &mut taken,
                                        &mut pending,
                                        planned,
                                        &mut budget,
                                        &mut intents,
                                        &tiles::to_pos(room.name, t),
                                        StructureType::Container,
                                    );
                                    None
                                }
                                (_, held) => held.map(|(post, _)| post),
                            }
                        }
                    };
                    for p in road_to.iter().flat_map(|post| {
                        atlas.walk(HAULER, &home, held.get(&home.room), post, LONGEST)
                    }) {
                        place(
                            &rooms,
                            &mut taken,
                            &mut pending,
                            planned,
                            &mut budget,
                            &mut intents,
                            &p,
                            StructureType::Road,
                        );
                    }
                }
            }
        }
    }

    intents
}
