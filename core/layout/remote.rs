//! A declared outpost has no spawn to anchor a plan on, so its container and
//! road are placed off the haul's own walk home.
use std::collections::BTreeMap;

use fabot_atlas::{self as atlas, Atlas, Mover};
use fabot_base::{
    declarations::{self, ColonyPlan},
    grid::Footprint,
    intents::{Action, RoomAct},
    tiles::{self, Tile, TileSet},
    world::{Controller, Pos, Room, RoomName, StructureType, World, terrain},
};

use super::{
    shape::Plan,
    sites::{self, Census, SITES_IN_FLIGHT},
};

const HAULER: Mover = Mover {
    footprint: Footprint::One,
    factor: fabot_base::grid::FatigueFactor { weight: 2, moves: 1 },
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
) -> bool {
    let Some(&room) = rooms.get(&pos.room) else {
        return false;
    };

    if *budget <= 0 {
        return false;
    }

    opened(taken, pending, planned, room);
    let (Some(open), Some(now)) = (taken.get_mut(&room.name), pending.get_mut(&room.name)) else {
        return false;
    };

    let placed = *now < SITES_IN_FLIGHT && open.insert(tiles::of_pos(pos));
    if placed {
        *now += 1;
        *budget -= 1;
        intents.push(Action::Room(room.name, RoomAct::CreateSite(pos.x, pos.y, kind)));
    }
    placed
}

/// The colony Economy hauls to (`atlas::server`), the haul's end there, and
/// whether a storage banks its income.
fn sink_for<'p>(
    atlas: &mut Atlas,
    (world, derived): (&World, &atlas::Derived),
    planned: &dyn Fn(RoomName) -> Option<&'p ColonyPlan>,
    room: &Room,
) -> Option<(RoomName, Pos, bool)> {
    let anchors = (room.controller.as_ref(), room.sources.first().map(|s| s.pos));
    let home = derived.colonies[atlas::server_of(atlas, derived, planned, anchors)?];
    let at = world.room(home)?;
    if !at.controller.as_ref().is_some_and(|c| atlas::builds_away(&world.rules, c.level)) {
        return None;
    }

    let banked = at.storage().is_some();
    Some((home, at.storage().map(|s| s.pos).or_else(|| planned(home)?.pile)?, banked))
}

/// v1's seat: the shortest walk out toward home. The owner's rule: the walk
/// is paved once its container stands.
pub fn sites<'p, 'q>(
    atlas: &mut Atlas,
    world: &World,
    derived: &atlas::Derived,
    (planned, plan_of): (&[Action], &dyn Fn(RoomName) -> Option<&'p ColonyPlan>),
    plans: impl IntoIterator<Item = (&'q Plan, &'q Census)>,
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
    for (plan, census) in plans {
        if let Some(&room) = rooms.get(&plan.room) {
            opened(&mut taken, &mut pending, planned, room);
            let open = taken.entry(plan.room).or_default();
            sites::unpaved(plan, census).for_each(|t| _ = open.insert(t));
        }
        let solid = held.entry(plan.room).or_default();
        for t in plan.solid() {
            solid.insert(t);
        }
    }

    // A harvester's dig raises the container under it, so a colony with no
    // storage banking its income raises one outpost's at a time, not all at once.
    let mut raising: BTreeMap<RoomName, RoomName> = BTreeMap::new();
    let mut sinks = BTreeMap::new();
    for o in outposts {
        if let Some(&room) = rooms.get(&o.room)
            && (room.sites.iter()).any(|s| s.kind == StructureType::Container && s.owner.is_ours())
        {
            let sink = *sinks
                .entry(room.name)
                .or_insert_with(|| sink_for(atlas, (world, derived), plan_of, room));
            if let Some((home, ..)) = sink {
                raising.entry(home).or_insert(room.name);
            }
        }
    }

    for o in outposts {
        let Some(&room) = rooms.get(&o.room) else {
            continue;
        };

        // A room at its site limit places nothing: the walk is not drawn.
        opened(&mut taken, &mut pending, planned, room);

        if budget > 0 && pending[&room.name] < SITES_IN_FLIGHT {
            let sink = *sinks
                .entry(room.name)
                .or_insert_with(|| sink_for(atlas, (world, derived), plan_of, room));

            if let Some((colony, sink, banked)) = sink {
                let raises = banked || raising.get(&colony).is_none_or(|r| *r == room.name);
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
                                (Some(&(on, _, t)), held) if raises && (!on || held.is_none()) => {
                                    if place(
                                        &rooms,
                                        &mut taken,
                                        &mut pending,
                                        planned,
                                        &mut budget,
                                        &mut intents,
                                        &tiles::to_pos(room.name, t),
                                        StructureType::Container,
                                    ) {
                                        raising.insert(colony, room.name);
                                    }
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

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use fabot_base::{
        declarations::{Declared, Outpost, Stage},
        testing::{self, RealRoom},
        world::Rules,
    };

    use super::*;
    use crate::{ground, plan};

    /// `home` planned as a colony, its neighbours its outposts, their sites
    /// built as placed until none is left to place; `on_site` sees each site
    /// with the tiles the home's plan holds solid or owes a rampart.
    fn built_out(
        real: &BTreeMap<RoomName, RealRoom>,
        home: RoomName,
        mut on_site: impl FnMut(&BTreeSet<Tile>, RoomName, Tile, StructureType),
    ) -> Option<World<'_>> {
        let drawn = ground::of_room(&real[&home].room());
        let mut plan = drawn.and_then(|g| plan::of_ground(&Rules::vanilla(), &g))?;
        let pile = plan.pile()?;
        let near: Vec<RoomName> = [(0, -1), (1, 0), (0, 1), (-1, 0)]
            .into_iter()
            .filter_map(|(x, y)| home.offset(x, y))
            .filter(|n| real.contains_key(n))
            .collect();
        let mut world = testing::world(r#"{"rooms":[]}"#);
        world.rooms = [home].iter().chain(&near).map(|n| real[n].room()).collect();
        world.closed = ((-4..=4).flat_map(|x| (-4..=4).map(move |y| (x, y))))
            .filter_map(|(x, y)| home.offset(x, y))
            .filter(|n| *n != home && !near.contains(n))
            .collect();
        let spawn = plan.items.iter().find(|i| i.kind == StructureType::Spawn);
        testing::colonize(&mut world, home, 8, spawn.expect("a spawn").tile);
        let declared = |id, pos| Declared { id, pos };
        let outposts: Vec<Outpost> = (world.rooms.iter())
            .filter(|r| r.name != home)
            .filter_map(|r| {
                let c = r.controller.as_ref()?;
                Some(Outpost {
                    room: r.name,
                    controller: declared(c.id, c.pos),
                    sources: r.sources.iter().map(|s| declared(s.id, s.pos)).collect(),
                    reserved: true,
                })
            })
            .collect();
        let colony = ColonyPlan {
            room: home,
            stage: Stage::Exploit,
            target: 8,
            reserve: 0,
            rampart_hits: 0,
            ring: true,
            owes: false,
            fights: false,
            pile: Some(pile),
            muster: None,
        };
        let plan_of = |room: RoomName| (room == home).then_some(&colony);
        let mut solid: BTreeSet<Tile> = plan.solid().collect();
        solid.extend(plan.ring.iter().map(|i| i.tile));
        plan.items.append(&mut plan.ring);
        for _ in 0..100 {
            let mut atlas = Atlas::of_world(&world);
            let mut derived = atlas::derive(&world, &mut atlas);
            derived.worked = (outposts.iter())
                .filter(|o| derived.trees.iter().any(|(_, t)| t.contains(o.room)))
                .cloned()
                .collect();
            let census = sites::census_of(world.room(home).expect("home"), &[]);
            let plans = [(&plan, &census)];
            let placed = sites(&mut atlas, &world, &derived, (&[], &plan_of), plans, 100);
            if placed.is_empty() {
                break;
            }
            for p in placed {
                let Action::Room(r, RoomAct::CreateSite(x, y, kind)) = p else {
                    continue;
                };
                let t = tiles::index(x, y);
                on_site(&solid, r, t, kind);
                let room = world.rooms.iter_mut().find(|o| o.name == r).expect("placed in");
                let id = format!("{:.2}{r}{t}", format!("{kind:?}"));
                room.structures.push(testing::built(&id, kind, Pos::new(r, x, y)));
            }
        }
        Some(world)
    }

    #[test]
    fn no_outpost_road_lies_on_a_tile_its_homes_plan_holds_solid_or_owes_a_rampart() {
        let real: BTreeMap<RoomName, RealRoom> =
            testing::real_rooms().map(|r| (r.name, r)).collect();
        let fixture = "E25S8".parse::<RoomName>().expect("a room name");
        let mut paved = 0;
        for home in real.keys().copied().step_by(40).chain([fixture]) {
            built_out(&real, home, |solid, r, t, kind| {
                if r == home && kind == StructureType::Road {
                    let (x, y) = (tiles::x_of(t), tiles::y_of(t));
                    assert!(!solid.contains(&t), "{home}: a road on {x},{y}");
                    paved += 1;
                }
            });
        }
        assert!(paved > 0);
    }

    /// An outpost's containers built and their harvesters at work: its
    /// sources, controller, exits and road ends still walk to each other.
    #[test]
    fn every_outpost_end_is_walked_to_with_its_sites_built_and_its_harvesters_at_work() {
        use crate::fixtures::{self as fx, End, Holder};
        let real: BTreeMap<RoomName, RealRoom> =
            testing::real_rooms().map(|r| (r.name, r)).collect();
        let mut cut: BTreeMap<(End, fx::Cause), BTreeSet<RoomName>> = BTreeMap::new();
        let mut outposts = 0;
        for home in real.keys().copied().step_by(5) {
            let Some(world) = built_out(&real, home, |_, _, _, _| {}) else {
                continue;
            };
            for room in world.rooms.iter().filter(|r| r.name != home) {
                let of = |k: StructureType| {
                    (room.structures.iter())
                        .filter(move |s| s.kind == k)
                        .map(|s| tiles::of_pos(&s.pos))
                };
                if of(StructureType::Container).next().is_none() {
                    continue;
                }
                outposts += 1;
                let rocks: Vec<Tile> = (room.sources.iter().map(|s| tiles::of_pos(&s.pos)))
                    .chain(room.controller.iter().map(|c| tiles::of_pos(&c.pos)))
                    .chain(room.minerals.iter().map(|m| tiles::of_pos(&m.pos)))
                    .collect();
                let wall = |t: Tile| terrain::is_wall(room.terrain, tiles::x_of(t), tiles::y_of(t));
                let roads: Vec<Tile> = of(StructureType::Road).collect();
                let bare: Vec<bool> = (tiles::all())
                    .map(|t| (!wall(t) || roads.contains(&t)) && !rocks.contains(&t))
                    .collect();
                let holders: Vec<(Holder, Tile)> =
                    of(StructureType::Container).map(|t| (Holder::Outpost, t)).collect();
                let mut ends: fx::Ends = Vec::new();
                for s in &room.sources {
                    let s = tiles::of_pos(&s.pos);
                    let posts: Vec<Tile> = (holders.iter())
                        .filter(|(_, p)| tiles::range(*p, s) == 1)
                        .map(|h| h.1)
                        .collect();
                    ends.push((End::Source, if posts.is_empty() { vec![s] } else { posts }, 1));
                }
                let controller = room.controller.iter().map(|c| tiles::of_pos(&c.pos));
                ends.extend(controller.map(|c| (End::Controller, vec![c], 1)));
                ends.extend(fx::exit_groups(&bare).into_iter().map(|g| (End::Exit, g, 0)));
                let held = |t: &Tile| holders.iter().any(|(_, h)| h == t);
                let road_ends = fx::road_ends(&roads).into_iter().filter(|t| !held(t));
                ends.extend(road_ends.map(|t| (End::Road, vec![t], 0)));
                for (end, _, cause) in fx::cut_off((&bare, &bare), &ends, &holders) {
                    cut.entry((end, cause)).or_default().insert(room.name);
                }
            }
        }
        let rooms: BTreeSet<&RoomName> = cut.values().flatten().collect();
        for ((end, cause), at) in &cut {
            let some: Vec<String> = at.iter().take(10).map(RoomName::to_string).collect();
            eprintln!("{end:?} cut by {cause:?}: {} rooms, {}", at.len(), some.join(" "));
        }
        assert!(rooms.is_empty(), "{} of {outposts} outposts cut", rooms.len());
    }
}
