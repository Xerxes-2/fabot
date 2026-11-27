use std::collections::BTreeSet;

use fabot_atlas::Atlas;
use fabot_base::{
    engine,
    tiles::{self, Tile, TileSet},
    world::{Built, Creep, KIND_COUNT, Pos, Room, Rules, Structure, StructureType, terrain},
};

use super::{
    plan::{controller_link, haul_level, sized},
    shape::{Item, Plan},
};

/// A level's first batch of extensions, and few enough that the builders'
/// energy goes to the plan's first items before its later ones.
pub const SITES_IN_FLIGHT: i32 = 5;

/// `(tile, ticks a hauler needs to the source, the tiles a post may stand on)`.
type Post = (Tile, Option<(Tile, Vec<Tile>)>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Placement {
    pub x: i32,
    pub y: i32,
    pub kind: StructureType,
}

/// A `byte` per tile, not a `bool`: a `bool[]` costs `Tiles.count` element
/// writes a room a tick, where a typed array is zeroed by its allocation.
#[derive(Clone, Debug)]
pub struct Census {
    pub solid: [u8; tiles::COUNT],
    pub road: [u8; tiles::COUNT],
    pub rampart: [u8; tiles::COUNT],
    pub site: [u8; tiles::COUNT],
    pub occupied: [u8; tiles::COUNT],
    pub walkable: [u8; tiles::COUNT],
    /// Natural wall: a road there is a tunnel.
    pub wall: [u8; tiles::COUNT],
    /// Tiles whose container's rock, else controller, has one already: one
    /// standing off the plan's tile is kept, never doubled.
    pub served: [u8; tiles::COUNT],
    pub rocked: Vec<(Tile, Tile)>,
    /// None once a link serves the source: one standing off the plan's tile is
    /// kept, never doubled.
    pub posts: Vec<Post>,
    pub buffer: Option<(Tile, Tile)>,
    /// A solid site there waits on its upgrader as under any creep.
    pub seat: [u8; tiles::COUNT],
    /// Every owner's, and sites: CONTROLLER_STRUCTURES counts them all. By
    /// `kind as usize`.
    pub held: [i32; KIND_COUNT],
    /// One under a creep is not counted, so it never holds back the rest.
    pub pending: i32,
    pub drawn: i32,
    pub ramparts: i32,
    /// The storage holds the reserve: the surplus pays a rampart's mend.
    pub stocked: bool,
}

pub fn census_of(room: &Room, ours: &[Creep]) -> Census {
    let mut solid = [0u8; tiles::COUNT];
    let mut road = [0u8; tiles::COUNT];
    let mut rampart = [0u8; tiles::COUNT];
    let mut site = [0u8; tiles::COUNT];
    let mut held = [0i32; KIND_COUNT];
    let mut occupied = [0u8; tiles::COUNT];
    let mut walkable = [0u8; tiles::COUNT];

    for (t, w) in walkable.iter_mut().enumerate() {
        if room.terrain.map_or(0, |terrain| terrain.get_bits()[t]) & terrain::WALL == 0 {
            *w = 1;
        }
    }

    let wall = walkable.map(|w| 1 - w);

    for c in ours.iter().chain(room.hostiles()) {
        occupied[tiles::of_pos(&c.pos)] = 1;
    }

    for s in &room.structures {
        let t = tiles::of_pos(&s.pos);

        if s.kind == StructureType::Road {
            road[t] = 1;
        } else if s.kind == StructureType::Rampart {
            rampart[t] = 1;
        } else {
            solid[t] = 1;
        }

        if !s.walkable {
            walkable[t] = 0;
        }

        held[s.kind as usize] += 1;
    }

    for t in room.blocked.iter().map(tiles::of_pos) {
        (solid[t], walkable[t]) = (1, 0);
    }

    let (mut pending, mut drawn, mut ramparts) = (0, 0, 0);

    for s in &room.sites {
        let t = tiles::of_pos(&s.pos);
        site[t] = 1;
        held[s.kind as usize] += 1;

        if !s.kind.walkable() {
            walkable[t] = 0;
        }

        // A site no creep crosses is not built while a creep stands on it.
        if s.owner.is_ours() && (s.kind.walkable() || occupied[t] == 0) {
            pending += 1;
            drawn += i32::from(s.kind.drawn_for());
            ramparts += i32::from(s.kind == StructureType::Rampart);
        }
    }

    let standing = |of: StructureType| -> Vec<Pos> {
        (room.structures.iter().map(|s| (s.kind, s.pos)))
            .chain(room.sites.iter().map(|s| (s.kind, s.pos)))
            .filter(|(kind, _)| *kind == of)
            .map(|(_, pos)| pos)
            .collect()
    };

    let containers = standing(StructureType::Container);

    let rocks: Vec<Pos> =
        room.sources.iter().map(|s| s.pos).chain(room.minerals.iter().map(|m| m.pos)).collect();

    let mut served = [0u8; tiles::COUNT];

    if let Some(c) = &room.controller
        && containers.iter().any(|p| Atlas::is_buffer(room, p))
    {
        for t in tiles::within(engine::UPGRADE_RANGE, tiles::of_pos(&c.pos)) {
            served[t] = 1;
        }
    }

    let mut rocked: Vec<(Tile, Tile)> = Vec::new();
    for rock in &rocks {
        for p in containers.iter().filter(|p| p.within(1, rock)) {
            rocked.push((tiles::of_pos(rock), tiles::of_pos(p)));
            // A container on a tile two rocks share serves neither, and a shared
            // tile is never served: either rock may still need its own post there.
            if !Atlas::shared(room, p) {
                let near = tiles::within(1, tiles::of_pos(rock)).into_iter();
                for t in near.filter(|t| !Atlas::shared(room, &tiles::to_pos(room.name, *t))) {
                    served[t] = 1;
                }
            }
        }
    }

    for rock in &rocks {
        walkable[tiles::of_pos(rock)] = 0;
    }

    if let Some(c) = &room.controller {
        walkable[tiles::of_pos(&c.pos)] = 0;
    }

    let mut seat = [0u8; tiles::COUNT];

    let buffer: Option<(Tile, &Structure)> = (room.controller.as_ref())
        .zip(Atlas::buffer(room))
        .map(|(c, b)| (tiles::of_pos(&c.pos), b));

    if let Some((c, b)) = buffer {
        for t in Atlas::upgrader_seats(|t: Tile| walkable[t] > 0, c, tiles::of_pos(&b.pos)) {
            seat[t] = 1;
        }
    }

    let links = standing(StructureType::Link);

    let unheld = |t: Tile| -> bool {
        walkable[t] > 0
            && solid[t] == 0
            && road[t] == 0
            && site[t] == 0
            && tiles::x_of(t).min(tiles::y_of(t)) >= 2
            && tiles::x_of(t).max(tiles::y_of(t)) < tiles::SIDE - 2
    };

    let mut posts: Vec<Post> = Vec::new();

    for s in &room.sources {
        if links.iter().any(|p| p.within(2, &s.pos)) {
            posts.push((tiles::of_pos(&s.pos), None));
        } else if let Some((p, _)) = Atlas::post_of(room, &s.pos) {
            let post = tiles::of_pos(&p);
            let beside = tiles::around(post).iter().copied().filter(|t| unheld(*t)).collect();
            posts.push((tiles::of_pos(&s.pos), Some((post, beside))));
        }
    }

    Census {
        solid,
        road,
        rampart,
        site,
        occupied,
        walkable,
        wall,
        served,
        rocked,
        posts,
        buffer: buffer
            .filter(|(_, b)| b.kind == StructureType::Container)
            .map(|(c, b)| (c, tiles::of_pos(&b.pos))),
        seat,
        held,
        pending,
        drawn,
        ramparts,
        stocked: false,
    }
}

/// Whether a structure on `t` parts the tiles beside it but across `post`,
/// where a miner works and steps aside for no walker.
fn closes(census: &Census, post: Tile, t: Tile) -> bool {
    fabot_atlas::parts(t, |n| n != post && census.walkable[n] > 0)
}

fn held_of(census: &Census, kind: StructureType) -> i32 {
    census.held[kind as usize]
}

/// Below the haul's level only a kind short of its allowance can be placed;
/// false lets the caller skip the plan.
pub fn may_place(rules: &Rules, census: &Census, level: i32) -> bool {
    let mut kinds = vec![StructureType::Link, StructureType::Extractor];
    kinds.extend(sized());

    level >= haul_level(rules)
        || kinds.iter().any(|kind| held_of(census, *kind) < rules.allowed(*kind, level))
}

pub fn posted(plan: &Plan, t: Tile) -> bool {
    (plan.items.iter()).any(|i| i.kind == StructureType::Container && i.tile == t)
}

/// Each rock and our container beside it off the plan's post: seated before
/// the room had a plan, it gives way to the post the bays and the link stand
/// around.
pub fn astray(plan: &Plan, census: &Census) -> Vec<(Tile, Tile)> {
    (census.rocked.iter()).filter(|(_, c)| !posted(plan, *c)).copied().collect()
}

/// Labs wait for RCL8 (owner), but for the one `lab` asks: `labs::upgrade_boosted`'s.
pub fn placeable(item: &Item, (level, lab): (i32, bool)) -> bool {
    item.level <= level && (item.kind != StructureType::Lab || engine::topped_out(level) || lab)
}

fn allowed(rules: &Rules, kind: StructureType, level: i32) -> i32 {
    let one = kind == StructureType::Lab && !engine::topped_out(level);
    let allowed = rules.allowed(kind, level);
    if one { allowed.min(1) } else { allowed }
}

/// Whether a planned rampart's site may go down on a tile: one over a
/// structure due by this level waits for it to stand; a later one would hold
/// a ring tile open until it stands.
pub fn rampart_fits(plan: &Plan, due: (i32, bool)) -> impl Fn(&Census, Tile) -> bool {
    let mut under = [false; tiles::COUNT];
    for i in plan.items.iter().filter(|i| placeable(i, due)) {
        under[i.tile] |= !matches!(i.kind, StructureType::Road | StructureType::Rampart);
    }
    move |census, t| (census.solid[t] > 0 || !under[t]) && census.rampart[t] == 0
}

/// A planned rampart's tile until it stands: the engine takes one site a tile.
pub fn unpaved<'a>(plan: &'a Plan, census: &'a Census) -> impl Iterator<Item = Tile> + 'a {
    let ramparts = plan.items.iter().filter(|i| i.kind == StructureType::Rampart);
    ramparts.map(|i| i.tile).filter(|t| census.rampart[*t] == 0)
}

/// The engine stacks a road only with a rampart, any other kind only with a
/// road or a rampart.
pub fn place(
    rules: &Rules,
    census: &Census,
    (level, lab): (i32, bool),
    plan: &Plan,
    budget: i32,
) -> Vec<Placement> {
    let mut placed: Vec<Placement> = Vec::new();
    let mut drawn = census.drawn;
    let mut added = [0i32; KIND_COUNT];
    let added_of = |added: &[i32; KIND_COUNT], kind: StructureType| -> i32 { added[kind as usize] };

    let mut taken: BTreeSet<Tile> = BTreeSet::new();

    let mut planned = [0u8; tiles::COUNT];
    let mut solid = [0u8; tiles::COUNT];
    let rampart_fits = rampart_fits(plan, (level, lab));

    for i in &plan.items {
        planned[i.tile] = 1;

        if !i.kind.walkable() {
            solid[i.tile] = 1;
        }
    }

    // A source's link stands beside the post its miner works from, which
    // another bot may have set off the plan's container tile; on the plan's
    // own, where the plan puts it unless it parts the tiles round it. The roads the
    // plan drew there lead to that unbuilt container, and the tile farthest
    // from the anchor lies behind the miner, off the way in. The last open
    // tile is the miner's way in.
    // A claim's first spawn may stand in a harvest bay, where a pioneer digs:
    // its site waits under the creep and is built once it steps off.
    let founding = |kind: StructureType| {
        kind == StructureType::Spawn && held_of(census, StructureType::Spawn) == 0
    };
    let tile_of = |item: &Item| -> Option<Tile> {
        let post = census.posts.iter().find(|(rock, _)| {
            item.kind == StructureType::Link && tiles::range(item.tile, *rock) <= 2
        });

        match (post, census.buffer) {
            (Some((_, None)), _) => None,
            (None, Some((c, b)))
                if item.kind == StructureType::Link
                    && census.seat[item.tile] > 0
                    && !posted(plan, b) =>
            {
                let walkable = |t: Tile| census.walkable[t] > 0;
                let open = |t: Tile| {
                    walkable(t)
                        && census.solid[t] == 0
                        && census.road[t] == 0
                        && census.site[t] == 0
                        && planned[t] == 0
                };

                controller_link(walkable, open, |t: Tile| t == item.tile || solid[t] == 0, c, b)
            }
            (Some((_, Some((post, beside)))), _)
                if closes(census, *post, item.tile)
                    || (!posted(plan, *post) && tiles::range(item.tile, *post) != 1) =>
            {
                let head = (beside.iter().copied())
                    .filter(|t| {
                        plan.items.iter().all(|i| i.tile != *t || i.kind == StructureType::Road)
                            && !closes(census, *post, *t)
                    })
                    .min_by_key(|t| {
                        (
                            plan.items.iter().any(|i| i.tile == *t),
                            -(tiles::range(*t, plan.anchor)),
                            *t,
                        )
                    });
                if beside.len() > 1 { head } else { None }
            }
            _ => Some(item.tile),
        }
    };

    let astray = astray(plan, census);
    // Past the container's own level a room keeps what stands rather than
    // rebuild it.
    let replaces = |own: i32, t: Tile| {
        level <= own
            && (census.rocked.iter())
                .filter(|(rock, _)| tiles::range(*rock, t) == 1)
                .all(|rc| astray.contains(rc))
    };

    let unpaved: TileSet = unpaved(plan, census).collect();
    // A tunnel takes only the slots nothing else wants: its site would hold
    // one for thousands of ticks.
    let tunnel = |i: &&Item| i.kind == StructureType::Road && census.wall[i.tile] > 0;
    let rest = plan.items.iter().filter(|i| !tunnel(i));
    let due = rest.chain(plan.items.iter().filter(tunnel)).filter(|i| placeable(i, (level, lab)));
    for item in due {
        match tile_of(item) {
            Some(t)
                if (placed.len() as i32) < budget
                    // Below the reserve builders take only drawn sites.
                    && if item.kind.drawn_for() {
                        drawn
                    } else {
                        census.pending + placed.len() as i32
                    } < SITES_IN_FLIGHT
                    && census.site[t] == 0
                    && (if item.kind == StructureType::Rampart {
                        // Its mend outranks the build-out's sites.
                        rampart_fits(census, t)
                            && (census.stocked
                                || drawn == census.ramparts + added_of(&added, item.kind))
                    } else {
                        census.solid[t] == 0
                    })
                    && (item.kind.walkable() || census.occupied[t] == 0 || founding(item.kind))
                    && !taken.contains(&t)
                    && (item.kind != StructureType::Container
                        || census.served[t] == 0
                        || replaces(item.level, t))
                    && (item.kind != StructureType::Road
                        || census.road[t] == 0 && !unpaved.contains(t))
                    && held_of(census, item.kind) + added_of(&added, item.kind)
                        < allowed(rules, item.kind, level) =>
            {
                taken.insert(t);
                drawn += i32::from(item.kind.drawn_for());
                added[item.kind as usize] = added_of(&added, item.kind) + 1;

                placed.push(Placement { x: tiles::x_of(t), y: tiles::y_of(t), kind: item.kind });
            }
            _ => (),
        }
    }

    placed
}

#[cfg(test)]
mod tests {
    use fabot_base::world::{Mineral, Owner, Source, Structure};

    use super::*;
    use crate::fixtures as fx;

    #[test]
    fn no_controller_links_site_lands_on_a_seat_of_a_buffer_another_bot_set() {
        let mut rng = fx::Rng::new(10);
        let made =
            (0..fx::MAX_TEST).map(|_| (fx::colony(&mut rng).room, rng.choose(-1_000, 1_000)));
        let made: Vec<_> = made.collect();
        let real: Vec<_> = fabot_base::testing::real_rooms().step_by(10).collect();
        let real = real.iter().enumerate().map(|(n, r)| (r.room(), n as i32 * 7));

        for (room, pick) in made.into_iter().chain(real) {
            let Some(ground) = crate::ground::of_room(&room) else {
                continue;
            };
            let Some(plan) = crate::plan::of_ground(&Rules::vanilla(), &ground) else {
                continue;
            };

            let Some(controller) = ground.controller else {
                continue;
            };

            let rocks: Vec<Tile> =
                ground.sources.iter().chain(ground.minerals.iter()).copied().collect();

            let off_plan: Vec<Tile> = tiles::within(engine::UPGRADE_RANGE, controller)
                .into_iter()
                .filter(|t| {
                    ground.terrain[*t] != 1
                        && rocks.iter().all(|r| tiles::range(*r, *t) > 1)
                        && plan.items.iter().all(|i| i.tile != *t || i.kind == StructureType::Road)
                })
                .collect();

            if off_plan.is_empty() {
                continue;
            }

            let buffer = { i64::from(pick) % off_plan.len() as i64 }.unsigned_abs() as usize;
            let buffer = off_plan[buffer];

            let mut room = room;
            room.structures.push(Structure {
                pos: tiles::to_pos(room.name, buffer),
                ..fx::structure("buffer", StructureType::Container, Owner::Ours, buffer)
            });

            let census = census_of(&room, &[]);

            for i in plan.items.iter().filter(|i| i.kind == StructureType::Link) {
                let mut one = plan.clone();
                one.items = vec![i.clone()];

                for p in place(
                    &Rules::vanilla(),
                    &census,
                    (engine::CONTROLLER_MAX_LEVEL, false),
                    &one,
                    1,
                ) {
                    let t = tiles::index(p.x, p.y);

                    assert!(
                        tiles::range(t, buffer) > 1
                            || tiles::range(t, controller) > engine::UPGRADE_RANGE
                            || ground.sources.iter().any(|r| tiles::range(*r, t) <= 2),
                        "{}",
                        room.name
                    );
                }
            }
        }
    }

    #[test]
    fn sites_keep_to_the_level_the_allowance_and_the_caps() {
        let mut rng = fx::Rng::new(11);

        for _ in 0..fx::MAX_TEST {
            let case = fx::census_case(&mut rng);

            let Some((_, plan)) = fx::plan_of(&case.colony) else {
                continue;
            };

            let pick = |ranks: &[i32]| -> Vec<Item> {
                let mut distinct: Vec<i32> = Vec::new();
                for r in ranks {
                    if !distinct.contains(r) {
                        distinct.push(*r);
                    }
                }

                distinct
                    .into_iter()
                    .filter(|r| *r >= 0 && (*r as usize) < plan.items.len())
                    .map(|r| plan.items[r as usize].clone())
                    .collect()
            };

            let built = pick(&case.built);

            let pending: Vec<Item> = pick(&case.pending)
                .into_iter()
                .filter(|i| !built.iter().any(|b| b.tile == i.tile))
                .collect();

            let mut room = case.colony.room.clone();
            room.structures.extend(
                built
                    .iter()
                    .map(|i| fx::structure(&format!("b{}", i.tile), i.kind, Owner::Ours, i.tile)),
            );
            room.sites =
                pending.iter().map(|i| fx::site(&format!("x{}", i.tile), i.kind, i.tile)).collect();

            let counted = census_of(&room, &[]);
            let rules = Rules::vanilla();
            let placed = place(&rules, &counted, (case.level, false), &plan, case.budget);

            let held = |kind: StructureType| -> i32 {
                (room.structures.iter().filter(|s| s.kind == kind).count()
                    + room.sites.iter().filter(|s| s.kind == kind).count()) as i32
            };

            let tiles_placed: Vec<Tile> = placed.iter().map(|p| tiles::index(p.x, p.y)).collect();

            let drawn_of = |kinds: &mut dyn Iterator<Item = StructureType>| {
                kinds.filter(|k| k.drawn_for()).count() as i32
            };
            let drawn = drawn_of(&mut room.sites.iter().map(|s| s.kind));
            let drawn_after = drawn + drawn_of(&mut placed.iter().map(|p| p.kind));
            let after = room.sites.len() as i32 + placed.len() as i32;

            let solid =
                |s: &Structure| s.kind != StructureType::Road && s.kind != StructureType::Rampart;

            // What a kind may go down over: a rampart only over a standing
            // structure no rampart covers yet; a road shares a rampart, any
            // other kind a road or a rampart.
            let clear = |kind: StructureType, here: &[Structure]| -> bool {
                if kind == StructureType::Rampart {
                    here.iter().any(solid) && !here.iter().any(|s| s.kind == StructureType::Rampart)
                } else {
                    here.iter().all(|s| {
                        s.kind == StructureType::Rampart
                            || (s.kind == StructureType::Road && kind != StructureType::Road)
                    })
                }
            };

            let standing = |t: Tile| -> Vec<Structure> {
                room.structures.iter().filter(|s| tiles::of_pos(&s.pos) == t).cloned().collect()
            };

            // An item the rules admit after what was placed: its level
            // reached, its kind short, its tile clear.
            let admitted = |i: &Item| -> bool {
                let t = i.tile;
                let added = placed.iter().filter(|q| q.kind == i.kind).count() as i32;

                placeable(i, (case.level, false))
                    && !room.sites.iter().any(|x| tiles::of_pos(&x.pos) == t)
                    && !tiles_placed.contains(&t)
                    && !(i.kind == StructureType::Container && counted.served[t] > 0)
                    && clear(i.kind, &standing(t))
                    && held(i.kind) + added < rules.allowed(i.kind, case.level)
            };

            assert!(may_place(&rules, &counted, case.level) || placed.is_empty());
            let full = |i: &Item| {
                (placed.len() as i32) >= case.budget
                    || if i.kind.drawn_for() { drawn_after } else { after } >= SITES_IN_FLIGHT
            };
            assert!((placed.len() as i32) <= case.budget.max(0));
            assert!(drawn_after <= SITES_IN_FLIGHT.max(drawn));
            assert!((placed.iter().enumerate()).all(|(i, p)| {
                p.kind.drawn_for() || room.sites.len() + i < SITES_IN_FLIGHT as usize
            }));
            assert!(plan.items.iter().all(|i| !admitted(i) || full(i)));
            assert_eq!(fx::distinct(&tiles_placed), tiles_placed);

            for p in &placed {
                let t = tiles::index(p.x, p.y);

                assert!(!room.sites.iter().any(|x| tiles::of_pos(&x.pos) == t));
                assert!(clear(p.kind, &standing(t)));
            }

            for p in &placed {
                let added = placed.iter().filter(|q| q.kind == p.kind).count() as i32;

                assert!(held(p.kind) + added <= rules.allowed(p.kind, case.level));
            }

            for p in &placed {
                assert!(plan.items.iter().any(|i| {
                    i.tile == tiles::index(p.x, p.y) && i.kind == p.kind && i.level <= case.level
                }));
            }
        }
    }

    #[test]
    fn every_ring_tile_holds_a_rampart_from_the_lowest_ring_level() {
        let rules = Rules::vanilla();
        for colony in fx::samples(14).take(fx::MAX_TEST) {
            let Some((_, mut plan)) = fx::plan_of(&colony) else {
                continue;
            };
            let level = plan.ring.iter().map(|i| i.level).min().unwrap_or(0);
            let ring: Vec<Tile> = (plan.ring.iter())
                .filter(|i| i.kind == StructureType::Rampart)
                .map(|i| i.tile)
                .collect();
            plan.items.append(&mut plan.ring);
            let mut room = colony.room.clone();
            loop {
                let placed = place(&rules, &census_of(&room, &[]), (level, false), &plan, i32::MAX);
                if placed.is_empty() {
                    break;
                }
                for p in placed {
                    let t = tiles::index(p.x, p.y);
                    room.structures.push(fx::structure(
                        &format!("s{t}.{}", p.kind as usize),
                        p.kind,
                        Owner::Ours,
                        t,
                    ));
                }
            }
            for t in ring {
                assert!(
                    (room.structures.iter())
                        .any(|s| s.kind == StructureType::Rampart && tiles::of_pos(&s.pos) == t),
                    "{t} open"
                );
            }
        }
    }

    /// Below the reserve no builder takes a road site, so none may hold a
    /// rampart's slot or tile.
    #[test]
    fn every_ring_rampart_is_placed_while_no_road_site_finishes() {
        let rules = Rules::vanilla();
        for real in fabot_base::testing::real_rooms().step_by(3).take(8) {
            let mut room = real.room();
            let Some(mut plan) =
                crate::ground::of_room(&room).and_then(|g| crate::plan::of_ground(&rules, &g))
            else {
                continue;
            };
            let ring: Vec<Item> =
                plan.ring.iter().filter(|i| i.kind == StructureType::Rampart).cloned().collect();
            plan.items.append(&mut plan.ring);
            for level in crate::RING_LEVEL..=engine::CONTROLLER_MAX_LEVEL {
                loop {
                    let placed =
                        place(&rules, &census_of(&room, &[]), (level, false), &plan, i32::MAX);
                    if placed.is_empty() {
                        break;
                    }
                    for p in placed {
                        let t = tiles::index(p.x, p.y);
                        let id = format!("s{t}.{}", p.kind as usize);
                        if p.kind == StructureType::Road {
                            room.sites.push(fx::site(&id, p.kind, t));
                        } else {
                            room.structures.push(fx::structure(&id, p.kind, Owner::Ours, t));
                        }
                    }
                }
                for i in ring.iter().filter(|i| placeable(i, (level, false))) {
                    assert!(
                        (room.structures.iter())
                            .any(|s| s.kind == StructureType::Rampart
                                && tiles::of_pos(&s.pos) == i.tile),
                        "{} {level} {}",
                        real.name,
                        i.tile
                    );
                }
            }
        }
    }

    #[test]
    fn below_the_top_level_only_the_lab_asked_for_is_placed() {
        let rules = Rules::vanilla();
        for colony in fx::samples(14).take(fx::MAX_TEST) {
            let Some((_, mut plan)) = fx::plan_of(&colony) else {
                continue;
            };
            plan.items.retain(|i| i.kind == StructureType::Lab);
            for (level, lab) in
                (6..=engine::CONTROLLER_MAX_LEVEL).flat_map(|l| [(l, false), (l, true)])
            {
                let mut room = colony.room.clone();
                loop {
                    let placed =
                        place(&rules, &census_of(&room, &[]), (level, lab), &plan, i32::MAX);
                    if placed.is_empty() {
                        break;
                    }
                    for p in placed {
                        let t = tiles::index(p.x, p.y);
                        room.structures.push(fx::structure(
                            &format!("l{t}"),
                            p.kind,
                            Owner::Ours,
                            t,
                        ));
                    }
                }
                let labs = room.structures.iter().filter(|s| s.kind == StructureType::Lab).count();
                let all =
                    if engine::topped_out(level) { plan.items.len() } else { usize::from(lab) };
                assert_eq!(labs, all, "{} {level} {lab}", colony.room.name);
            }
        }
    }

    #[test]
    fn a_container_beside_two_rocks_leaves_each_its_own_post() {
        let rules = Rules::vanilla();
        let colony = fx::samples(16).next().expect("a sample");
        let at = |x, y| tiles::index(x, y);
        let (source, mineral) = (colony.room.sources[0].clone(), colony.room.minerals[0].clone());
        for ore in [false, true] {
            let mut room = colony.room.clone();
            room.sources = vec![Source { pos: fx::at(at(12, 13)), ..source.clone() }];
            room.minerals = vec![Mineral { pos: fx::at(at(10, 13)), ..mineral.clone() }];
            if !ore {
                room.sources.push(Source { pos: fx::at(at(10, 13)), ..source.clone() });
                room.minerals.clear();
            }
            for (stood, own) in [(at(11, 12), at(13, 12)), (at(13, 12), at(11, 12))] {
                room.structures =
                    vec![fx::structure("c", StructureType::Container, Owner::Nobody, stood)];
                let plan = Plan {
                    room: fx::ROOM,
                    anchor: at(25, 25),
                    items: vec![Item { kind: StructureType::Container, tile: own, level: 2 }],
                    ring: Vec::new(),
                    unfit: Vec::new(),
                    muster: None,
                };
                let placed = place(&rules, &census_of(&room, &[]), (6, false), &plan, 1);
                assert_eq!(placed.len(), 1, "{own} not placed beside {stood}, ore {ore}");
            }
        }
    }

    /// Real rooms with their posts built and another bot's extensions round
    /// the plan's link: a digger holds its post, so a link that parts the
    /// tiles beside it, the post left out, leaves the post the only way on.
    #[test]
    fn no_source_link_site_parts_the_tiles_beside_it_but_across_its_post() {
        let (rules, mut rng, mut linked) = (Rules::vanilla(), fx::Rng::new(18), 0);
        for real in fabot_base::testing::real_rooms().step_by(20) {
            let mut room = real.room();
            let Some(plan) =
                crate::ground::of_room(&room).and_then(|g| crate::plan::of_ground(&rules, &g))
            else {
                continue;
            };
            let at = |t: Tile| tiles::to_pos(real.name, t);
            let built =
                |kind, owner, t| Structure { pos: at(t), ..fx::structure("s", kind, owner, t) };
            let kept = |i: &&Item| matches!(i.kind, StructureType::Container | StructureType::Link);
            let one =
                Plan { items: plan.items.iter().filter(kept).cloned().collect(), ..plan.clone() };
            let posts: Vec<Tile> = (one.items.iter())
                .filter(|i| i.kind == StructureType::Container)
                .filter(|i| real.sources.iter().any(|s| tiles::range(*s, i.tile) == 1))
                .map(|i| i.tile)
                .collect();
            for (n, p) in posts.iter().enumerate() {
                let id = format!("c{n}");
                let post = Structure {
                    id: id.as_str().into(),
                    ..built(StructureType::Container, Owner::Ours, *p)
                };
                room.structures.push(post);
            }
            let links = one.items.iter().filter(|i| i.kind == StructureType::Link);
            for (n, t) in links.flat_map(|l| tiles::around(l.tile).iter()).enumerate() {
                let free = !posts.contains(t) && !one.items.iter().any(|i| i.tile == *t);
                if free && !tiles::on_edge(*t) && rng.choose(0, 2) == 0 {
                    let id = format!("x{n}");
                    let ext = built(StructureType::Extension, Owner::Stranger("x".into()), *t);
                    room.structures.push(Structure { id: id.as_str().into(), ..ext });
                }
            }
            let census = census_of(&room, &[]);
            let placed =
                place(&rules, &census, (engine::CONTROLLER_MAX_LEVEL, false), &one, i32::MAX);
            for p in placed.iter().filter(|p| p.kind == StructureType::Link) {
                let t = tiles::index(p.x, p.y);
                let Some(post) = posts.iter().find(|post| tiles::range(**post, t) == 1) else {
                    continue;
                };
                let open = |n: Tile| n != *post && census.walkable[n] > 0;
                let beside: Vec<Tile> =
                    tiles::around(t).iter().copied().filter(|n| open(*n)).collect();
                let walked = tiles::reach(beside.iter().copied().take(1), |n| open(n) && n != t);
                let parted = beside.iter().any(|n| walked[*n] == tiles::UNREACHED);
                assert!(!parted, "{}: a link on {},{} beside {post}", real.name, p.x, p.y);
                linked += 1;
            }
        }
        assert!(linked > 100, "{linked}");
    }

    #[test]
    fn a_container_off_the_plans_post_gives_way_to_the_plans_own() {
        let rules = Rules::vanilla();
        for colony in fx::samples(16).take(fx::MAX_TEST) {
            let Some((ground, plan)) = fx::plan_of(&colony) else {
                continue;
            };
            for rock in &ground.sources {
                let Some(post) = (plan.items.iter()).find(|i| {
                    i.kind == StructureType::Container && tiles::range(i.tile, *rock) == 1
                }) else {
                    continue;
                };
                let Some(stray) = tiles::around(*rock)
                    .iter()
                    .copied()
                    .find(|t| ground.terrain[*t] != 1 && !tiles::on_edge(*t) && !posted(&plan, *t))
                else {
                    continue;
                };
                let mut room = colony.room.clone();
                room.structures.push(fx::structure(
                    "stray",
                    StructureType::Container,
                    Owner::Nobody,
                    stray,
                ));
                let mut one = plan.clone();
                one.items = vec![post.clone()];
                let placed = place(&rules, &census_of(&room, &[]), (post.level, false), &one, 1);
                assert_eq!(placed.len(), 1, "{} beside {rock} not placed", post.tile);
            }
        }
    }
}
