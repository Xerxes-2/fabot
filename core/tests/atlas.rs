//! Invariants of the Atlas over generated rooms, and the tile and pace
//! arithmetic it rests on.
use std::collections::BTreeSet;

use fabot_atlas::{Atlas, Mover, Target};
use fabot_base::engine;
use fabot_base::grid::{
    FatigueFactor, Footprint, empty_factor, factor_of, loaded_factor, on_edge, step_ticks,
};
use fabot_base::testing::Dice;
use fabot_base::tiles::{self, Tile, TileSet};
use fabot_base::vocabulary::{ALL_BODY_PARTS, Part, ResourceType};
use fabot_base::world::{
    BodyPart, Creep, Kind, Mineral, Owner, Pos, Room, RoomName, Source, Store, Structure, World,
    terrain,
};

const ROOM: RoomName = RoomName::named("W1N1");
const CASES: u64 = 100;

fn structure(kind: Kind, walkable: bool, x: i32, y: i32) -> Structure {
    Structure {
        id: format!("s{x},{y}").as_str().into(),
        kind,
        pos: Pos::new(ROOM, x, y),
        owner: Owner::Nobody,
        hits: 1,
        hits_max: 1,
        store: Store::empty(),
        cooldown: 0,
        collapse: 0,
        spawn: None,
        level: 0,
        walkable,
    }
}

/// One generated room: raw terrain, roads and walls built on it.
#[derive(Clone)]
struct Ground {
    terrain: Vec<u8>,
    roads: Vec<(i32, i32)>,
    walls: Vec<(i32, i32)>,
}

impl Ground {
    /// Whether nothing may stand on a tile: terrain wall without a road, or a
    /// built wall.
    fn blocked(&self, x: i32, y: i32) -> bool {
        self.walls.contains(&(x, y))
            || (self.terrain[tiles::index(x, y)] & terrain::WALL != 0
                && !self.roads.contains(&(x, y)))
    }
}

fn room_of(name: RoomName, terrain: Vec<u8>, structures: Vec<Structure>) -> Room<'static> {
    Room {
        name,
        terrain: terrain::leak(terrain),
        controller: None,
        energy_available: 0,
        energy_capacity: 0,
        sources: Vec::new(),
        minerals: Vec::new(),
        structures,
        sites: Vec::new(),
        piles: Vec::new(),
        remains: Vec::new(),
        others: Vec::new(),
    }
}

fn world_of_rooms(rooms: Vec<Room>) -> World {
    World {
        time: 1,
        me: String::new(),
        cpu_limit: 20.0,
        bucket: 10_000,
        rooms,
        spawns: Vec::new(),
        creeps: Vec::new(),
        attacks: Vec::new(),
        destroyed: Vec::new(),
        our_sites: 0,
        rooms_allowed: 1,
        closed: Default::default(),
        rules: fabot_base::world::Rules::vanilla(),
    }
}

fn world_of(ground: &Ground) -> World<'static> {
    let structures = ground
        .roads
        .iter()
        .map(|&(x, y)| structure(Kind::Road, true, x, y))
        .chain(
            ground
                .walls
                .iter()
                .map(|&(x, y)| structure(Kind::Wall, false, x, y)),
        )
        .collect();
    world_of_rooms(vec![room_of(ROOM, ground.terrain.clone(), structures)])
}

/// A walk to price: the ground, a mover, a goal tile with its range, a start.
#[derive(Clone)]
struct Walk {
    ground: Ground,
    mover: Mover,
    goal: Tile,
    range: i32,
    start: Tile,
}

fn tile(dice: &mut Dice) -> Tile {
    Tile::of_index(dice.below(tiles::COUNT as i32) as usize)
}

fn coords(dice: &mut Dice) -> Vec<(i32, i32)> {
    let n = dice.below(60);
    (0..n)
        .map(|_| (dice.below(tiles::SIDE), dice.below(tiles::SIDE)))
        .collect()
}

fn ground(dice: &mut Dice) -> Ground {
    // Rooms from open to a quarter wall, so a 2×2 body finds a way often.
    let walls = dice.between(0, 3);
    let terrain = (0..tiles::COUNT)
        .map(|_| match dice.below(9 + walls) {
            0..=6 => 0,
            7 | 8 => terrain::SWAMP,
            _ => terrain::WALL,
        })
        .collect();
    Ground {
        terrain,
        roads: coords(dice),
        walls: coords(dice),
    }
}

fn parts(dice: &mut Dice, most: i32) -> Vec<Part> {
    let n = dice.between(1, most);
    (0..n).map(|_| dice.pick(&ALL_BODY_PARTS)).collect()
}

fn mover(dice: &mut Dice) -> Mover {
    let body = parts(dice, engine::MAX_BODY_PARTS as i32);
    Mover {
        factor: if dice.chance(2) {
            loaded_factor(&body)
        } else {
            empty_factor(&body)
        },
        footprint: dice.pick(&[Footprint::One, Footprint::Quad]),
    }
}

fn creep(owner: Owner, pos: Pos) -> Creep {
    Creep {
        id: "c".into(),
        name: "c".into(),
        owner,
        pos,
        body: Vec::new(),
        hits: 1,
        hits_max: 1,
        ticks_to_live: 1,
        fatigue: 0,
        store: Store::empty(),
    }
}

fn walk(dice: &mut Dice) -> Walk {
    Walk {
        ground: ground(dice),
        mover: mover(dice),
        goal: tile(dice),
        range: dice.between(0, 3),
        start: tile(dice),
    }
}

fn walks(seed: u64) -> impl Iterator<Item = (Walk, Dice)> {
    let mut dice = Dice::new(seed);
    (0..CASES).map(move |i| (walk(&mut dice), Dice::new(seed * 1_000 + i)))
}

fn target_of(room: RoomName, walk: &Walk) -> Target {
    Target {
        room,
        tiles: vec![walk.goal],
        range: walk.range,
        avoid: BTreeSet::new(),
        max_hops: 4,
    }
}

/// Chebyshev range from what a footprint anchored at `anchor` covers to `tile`.
fn reach(footprint: Footprint, anchor: Tile, tile: Tile) -> i32 {
    let span = i32::from(footprint == Footprint::Quad);
    let gap = |a: i32, t: i32| 0.max((a - t).max(t - a - span));
    gap(tiles::x_of(anchor), tiles::x_of(tile)).max(gap(tiles::y_of(anchor), tiles::y_of(tile)))
}

fn covered(footprint: Footprint, anchor: Tile) -> Vec<(i32, i32)> {
    let (x, y) = (tiles::x_of(anchor), tiles::y_of(anchor));
    match footprint {
        Footprint::One => vec![(x, y)],
        Footprint::Quad => vec![(x, y), (x + 1, y), (x, y + 1), (x + 1, y + 1)],
    }
}

/// The walk followed by `first_step` from the start: each tile and its ticks,
/// until a goal or no step.
fn followed(atlas: &Atlas, walk: &Walk, target: &Target) -> Vec<(Tile, i32)> {
    let mut out = Vec::new();
    let mut tile = walk.start;
    for seen in 0.. {
        let pos = tiles::to_pos(ROOM, tile);
        let Some(ticks) = atlas.travel_ticks(walk.mover, target, &pos) else {
            break;
        };
        out.push((tile, ticks));
        match atlas
            .first_step(walk.mover, target, &pos)
            .and_then(|d| tiles::neighbour(tile, d))
        {
            Some(next) if seen < 2 * tiles::COUNT => tile = next,
            _ => break,
        }
    }
    out
}

#[test]
fn a_tiles_index_round_trips_through_its_column_and_row_in_terrain_order() {
    for tile in tiles::all() {
        assert_eq!(
            tiles::index(tiles::x_of(tile), tiles::y_of(tile)),
            tile,
            "tile {tile}"
        );
    }
    assert_eq!(tiles::index(3, 1).index(), 50 + 3, "y * 50 + x");
}

#[test]
fn a_room_name_round_trips_through_world_coordinates() {
    for x in -299..300 {
        for y in (-299..300).step_by(7) {
            let name = RoomName::new(x, y).to_string();
            assert_eq!(
                RoomName::parse(&name).map(RoomName::coords),
                Some((x, y)),
                "{name}"
            );
        }
    }
}

#[test]
fn names_read_as_the_engine_spells_them() {
    assert_eq!(RoomName::parse("W12S28"), Some(RoomName::new(-13, 28)));
    assert_eq!(RoomName::parse("E0N0"), Some(RoomName::new(0, -1)));
    assert_eq!(RoomName::parse("W1"), None, "half a name");
    let sim = RoomName::parse("sim").expect("the simulator's room");
    assert_eq!(sim.to_string(), "sim");
    assert_eq!(sim.on_grid(), None, "off the grid");
    assert_eq!(tiles::room_toward(sim, tiles::TOP), None, "no neighbours");
}

#[test]
fn a_step_and_the_direction_back_to_it_agree_and_stay_within_range_one() {
    for tile in tiles::all() {
        for direction in 1..=8 {
            if let Some(next) = tiles::neighbour(tile, direction) {
                assert_eq!(tiles::direction(tile, next), direction, "direction");
                assert_eq!(tiles::range(tile, next), 1, "adjacent");
            }
        }
    }
}

#[test]
fn an_exits_landing_is_on_the_opposite_exit_and_lands_back() {
    for exit in [tiles::TOP, tiles::RIGHT, tiles::BOTTOM, tiles::LEFT] {
        let room = tiles::room_toward(ROOM, exit).unwrap();
        for &tile in tiles::exit_tiles(exit) {
            let landing = tiles::landing(tile).unwrap();
            assert_eq!(tiles::exit_of(tile), exit, "on its exit");
            assert_eq!(tiles::exit_of(landing), (exit + 3) % 8 + 1, "opposite");
            assert_eq!(tiles::landing(landing), Some(tile), "and back");
            assert_eq!(
                tiles::seam_range(&tiles::to_pos(ROOM, tile), &tiles::to_pos(room, landing)),
                Some(0),
                "one tile across the seam"
            );
        }
        assert_eq!(
            tiles::exit_toward(ROOM, room),
            Some(exit),
            "the room across"
        );
    }
}

#[test]
fn a_step_takes_the_fatigue_it_makes_over_twice_the_move_at_least_one_tick() {
    let table = [
        (1, 1, engine::PLAIN_WEIGHT, Some(1)),
        (1, 1, engine::SWAMP_WEIGHT, Some(5)),
        (2, 1, engine::PLAIN_WEIGHT, Some(2)),
        (2, 1, engine::ROAD_WEIGHT, Some(1)),
        (49, 1, engine::SWAMP_WEIGHT, Some(245)),
        (0, 1, engine::SWAMP_WEIGHT, Some(1)),
        (5, 0, engine::PLAIN_WEIGHT, None),
    ];
    for (weight, moves, ground, ticks) in table {
        assert_eq!(
            step_ticks(FatigueFactor { weight, moves }, ground),
            ticks,
            "{weight}W {moves}M on {ground}"
        );
    }
}

#[test]
fn a_steps_ticks_are_the_ceiling_of_its_fatigue_over_the_moves_pay() {
    for weight in 0..50 {
        for moves in 1..=50 {
            let factor = FatigueFactor { weight, moves };
            for ground in [
                engine::ROAD_WEIGHT,
                engine::PLAIN_WEIGHT,
                engine::SWAMP_WEIGHT,
            ] {
                let fatigue = ground * weight;
                assert_eq!(
                    step_ticks(factor, ground),
                    Some(1.max((fatigue + 2 * moves - 1) / (2 * moves))),
                    "{weight}W {moves}M on {ground}"
                );
            }
        }
    }
}

#[test]
fn a_loaded_body_weighs_at_least_its_empty_self_and_a_creep_reads_as_its_parts() {
    let mut dice = Dice::new(5);
    for _ in 0..CASES {
        let kinds = parts(&mut dice, 60);
        let (empty, loaded) = (empty_factor(&kinds), loaded_factor(&kinds));
        let creep = Creep {
            id: "c".into(),
            name: "c".into(),
            owner: Owner::Ours,
            pos: Pos::new(ROOM, 1, 1),
            body: kinds
                .iter()
                .map(|&kind| BodyPart {
                    kind,
                    hits: 100,
                    boost: None,
                })
                .collect(),
            hits: 100,
            hits_max: 100,
            ticks_to_live: 1,
            fatigue: 0,
            store: Store {
                held: vec![(ResourceType::Energy, engine::CARRY_PART_CAPACITY * 50)],
                capacity: 0,
            },
        };
        assert!(loaded.weight >= empty.weight, "{kinds:?}");
        assert_eq!(loaded.moves, empty.moves, "{kinds:?}");
        let unloaded = Creep {
            store: Store::empty(),
            ..creep.clone()
        };
        assert_eq!(factor_of(&unloaded), empty, "{kinds:?}");
        assert_eq!(factor_of(&creep), loaded, "{kinds:?}");
    }
}

#[test]
fn a_walk_takes_at_least_a_tick_a_tile_of_range_it_must_close() {
    for (walk, _) in walks(11) {
        let atlas = Atlas::of_world(&world_of(&walk.ground));
        let target = target_of(ROOM, &walk);
        if let Some(ticks) =
            atlas.travel_ticks(walk.mover, &target, &tiles::to_pos(ROOM, walk.start))
        {
            assert!(ticks >= reach(walk.mover.footprint, walk.start, walk.goal) - walk.range);
        }
    }
}

#[test]
fn each_first_step_lands_where_the_walk_is_shorter_on_open_ground_to_a_seat() {
    for (walk, _) in walks(12) {
        let atlas = Atlas::of_world(&world_of(&walk.ground));
        let target = target_of(ROOM, &walk);
        let path = followed(&atlas, &walk, &target);
        let open = |tile: Tile| {
            covered(walk.mover.footprint, tile)
                .iter()
                .all(|&(x, y)| tiles::in_room(x, y) && !walk.ground.blocked(x, y))
        };
        let seats: Vec<Tile> = atlas
            .seats(
                walk.mover.footprint,
                walk.range,
                &tiles::to_pos(ROOM, walk.goal),
            )
            .iter()
            .map(tiles::of_pos)
            .collect();
        for pair in path.windows(2) {
            assert!(pair[1].1 < pair[0].1, "descending");
            assert!(open(pair[1].0), "open ground");
        }
        match path.last() {
            Some(&(tile, 0)) => assert!(seats.contains(&tile), "on a seat"),
            Some(_) => panic!("the walk stopped short of a seat"),
            None => (),
        }
    }
}

#[test]
fn a_seat_is_open_ground_off_the_ring_within_range() {
    for (walk, _) in walks(13) {
        let atlas = Atlas::of_world(&world_of(&walk.ground));
        let footprint = walk.mover.footprint;
        for pos in atlas.seats(footprint, walk.range, &tiles::to_pos(ROOM, walk.goal)) {
            let tile = tiles::of_pos(&pos);
            assert!(reach(footprint, tile, walk.goal) <= walk.range);
            assert!(!on_edge(footprint, tile));
            assert!(
                covered(footprint, tile)
                    .iter()
                    .all(|&(x, y)| !walk.ground.blocked(x, y))
            );
        }
    }
}

#[test]
fn no_seat_is_a_sources_a_minerals_or_a_controllers_tile() {
    for (walk, mut dice) in walks(14) {
        let at = Pos::new(ROOM, dice.between(1, 48), dice.between(1, 48));
        let across = Pos::new(ROOM, 49 - at.x, at.y);
        let mut world = world_of(&walk.ground);
        world.rooms[0].sources = vec![Source {
            id: "src".into(),
            pos: at,
            energy: 0,
            energy_capacity: 0,
            ticks_to_regeneration: 0,
        }];
        world.rooms[0].minerals = vec![Mineral {
            id: "min".into(),
            pos: across,
            resource: None,
            amount: 0,
        }];
        let atlas = Atlas::of_world(&world);
        for obstacle in [at, across] {
            assert!(
                !atlas
                    .seats(Footprint::One, 1, &obstacle)
                    .contains(&obstacle)
            );
        }
    }
}

#[test]
fn a_memoised_answer_is_a_fresh_atlass_answer() {
    let mut befores = walks(16);
    for (walk, mut dice) in walks(15) {
        let (before, _) = befores.next().unwrap();
        let kept = dice.chance(2);
        let world = world_of(&walk.ground);
        let target = target_of(ROOM, &walk);
        let start = tiles::to_pos(ROOM, walk.start);
        let stale = Atlas::of_world(&world_of(if kept { &walk.ground } else { &before.ground }));
        stale.travel_ticks(walk.mover, &target, &start);
        let warm = Atlas::following(stale, &world, [], []);
        let first = warm.travel_ticks(walk.mover, &target, &start);
        let again = warm.travel_ticks(walk.mover, &target, &start);
        let step = warm.first_step(walk.mover, &target, &start);
        let fresh = Atlas::of_world(&world);
        assert_eq!(first, again);
        assert_eq!(first, fresh.travel_ticks(walk.mover, &target, &start));
        assert_eq!(step, fresh.first_step(walk.mover, &target, &start));
        if walk.mover.factor.moves != 0 {
            assert_eq!(warm.misses(), i32::from(!kept));
        }
    }
}

#[test]
fn toward_another_room_the_walk_ends_on_an_exit() {
    for (walk, mut dice) in walks(17) {
        let toward = dice.pick(&[tiles::TOP, tiles::RIGHT, tiles::BOTTOM, tiles::LEFT]);
        let onto = tiles::room_toward(ROOM, toward).unwrap();
        let atlas = Atlas::of_world(&world_of(&walk.ground));
        let target = target_of(onto, &walk);
        if let Some(&(tile, ticks)) = followed(&atlas, &walk, &target).last() {
            assert_eq!(ticks, 0);
            let edge = covered(walk.mover.footprint, tile);
            assert!(
                edge.iter()
                    .any(|&(x, y)| tiles::exit_of(tiles::index(x, y)) != 0)
            );
        }
    }
}

#[test]
fn a_route_read_off_its_targets_tree_is_the_route_searched() {
    let mut dice = Dice::new(18);
    for _ in 0..CASES {
        let walled: Vec<(i32, i32, i32)> = (0..dice.below(20))
            .map(|_| {
                (
                    dice.between(-128, 127),
                    dice.between(-128, 127),
                    dice.below(256),
                )
            })
            .collect();
        let mut rooms = Vec::new();
        for x in -2..=2 {
            for y in -2..=2 {
                let mut terrain = vec![0u8; tiles::COUNT];
                for &(wx, wy, exit) in &walled {
                    if wx % 3 == x && wy % 3 == y {
                        for &t in tiles::exit_tiles(1 + 2 * (exit % 4)) {
                            terrain[t] = terrain::WALL;
                        }
                    }
                }
                rooms.push(room_of(RoomName::new(x, y), terrain, Vec::new()));
            }
        }
        let atlas = Atlas::of_world(&world_of_rooms(rooms));
        let hops = 1 + dice.below(4);
        let from = RoomName::new(0, 0);
        let onto = RoomName::new(dice.between(-128, 127) % 3, dice.between(-128, 127) % 3);
        let tree = atlas.tree(&BTreeSet::new(), hops, onto);
        let routed = atlas.route(&BTreeSet::new(), hops, from, onto);
        assert_eq!(tree.route(from, onto), routed);
        let crossed = routed.map(|r| r.len() as i32 - 1);
        assert_eq!(tree.crossings(from, onto), crossed);
        assert_eq!(tree.crossings(onto, from), crossed);
    }
}

#[test]
fn a_route_crosses_shared_borders_around_the_avoided_by_fewest_crossings() {
    let mut dice = Dice::new(19);
    let atlas = Atlas::of_world(&world_of_rooms(Vec::new()));
    for _ in 0..CASES {
        let (dx, dy) = (dice.between(-128, 127) % 4, dice.between(-128, 127) % 4);
        let onto = RoomName::new(dx, dy);
        let from = RoomName::new(0, 0);
        let avoid: BTreeSet<RoomName> = (0..dice.below(6))
            .map(|_| RoomName::new(dice.between(-128, 127) % 4, dice.between(-128, 127) % 4))
            .collect();
        let hops = dx.abs() + dy.abs();
        match atlas.route(&avoid, 12, from, onto) {
            Some(chain) => {
                assert_eq!(chain[0], from);
                assert_eq!(chain[chain.len() - 1], onto);
                assert!(
                    chain
                        .windows(2)
                        .all(|p| tiles::exit_toward(p[0], p[1]).is_some())
                );
                assert!(
                    chain
                        .iter()
                        .all(|r| *r == from || *r == onto || !avoid.contains(r))
                );
                assert!(chain.len() as i32 > hops);
                assert!(!avoid.is_empty() || chain.len() as i32 - 1 == hops);
            }
            None => assert!(!avoid.is_empty()),
        }
    }
}

#[test]
fn no_route_enters_a_closed_room_nor_ends_in_one() {
    let mut dice = Dice::new(21);
    for _ in 0..CASES {
        let mut room = || RoomName::new(dice.between(-128, 127) % 4, dice.between(-128, 127) % 4);
        let (from, onto) = (room(), room());
        let mut world = world_of_rooms(Vec::new());
        world.closed = (0..4).map(|_| room()).filter(|r| *r != from).collect();
        let atlas = Atlas::of_world(&world);
        if let Some(chain) = atlas.route(&BTreeSet::new(), 12, from, onto) {
            assert!(chain.iter().all(|r| !world.closed.contains(r)));
        }
    }
}

#[test]
fn any_open_neighbour_is_one_crossing_away_keeper_room_or_not() {
    let mut dice = Dice::new(20);
    let atlas = Atlas::of_world(&world_of_rooms(Vec::new()));
    for _ in 0..CASES {
        let from = RoomName::new(dice.below(24) - 12, dice.below(24) - 12);
        let exit = dice.pick(&[tiles::TOP, tiles::RIGHT, tiles::BOTTOM, tiles::LEFT]);
        let onto = tiles::room_toward(from, exit).unwrap();
        assert_eq!(
            atlas.route(&BTreeSet::new(), 1, from, onto),
            Some(vec![from, onto])
        );
    }
}

#[test]
fn every_tile_a_keeper_strikes_is_walled_and_a_room_of_keepers_not_due_walls_none() {
    let mut dice = Dice::new(21);
    for _ in 0..CASES {
        let ground = ground(&mut dice);
        let mut world = world_of(&ground);
        let (lair, rock, at) = (tile(&mut dice), tile(&mut dice), tile(&mut dice));
        let pos = |t: Tile| tiles::to_pos(ROOM, t);
        let room = &mut world.rooms[0];
        let mut lair_of = structure(Kind::KeeperLair, false, pos(lair).x, pos(lair).y);
        lair_of.spawn = Some(dice.between(-1, 2 * Atlas::CROSSING_TICKS)).filter(|t| *t >= 0);
        let spawns = lair_of.spawn.is_some_and(|t| t <= Atlas::CROSSING_TICKS);
        room.structures.push(lair_of);
        room.sources.push(Source {
            id: "rock".into(),
            pos: pos(rock),
            energy: 0,
            energy_capacity: 0,
            ticks_to_regeneration: 0,
        });
        let alive = dice.chance(2);
        if alive {
            room.others.push(Creep {
                body: engine::keeper_body()
                    .into_iter()
                    .map(|kind| BodyPart {
                        kind,
                        hits: engine::PART_HITS,
                        boost: None,
                    })
                    .collect(),
                hits: 5000,
                hits_max: 5000,
                ..creep(Owner::Keeper, pos(at))
            });
        }
        let bare = Atlas::of_world(&world_of(&ground))
            .held_weights(ROOM)
            .unwrap();
        let weights = Atlas::of_world(&world).held_weights(ROOM).unwrap();
        for t in tiles::all() {
            let solid = t == lair || t == rock;
            assert!(weights[t].is_none() || (bare[t] == weights[t] && !solid));
            assert!(weights[t].is_none() || !alive || tiles::range(at, t) > engine::RANGED_RANGE);
            assert!(alive || spawns || weights[t] == bare[t] || solid);
        }
    }
}

/// Three rooms square, each open inside and to the next only where both
/// open a stretch of their shared border, ringed by closed rooms; a stretch
/// is walled on one side at times, a dead end only its terrain shows.
fn walled_rooms(dice: &mut Dice) -> World<'static> {
    let mut rooms = Vec::new();
    let mut borders: std::collections::BTreeMap<(i32, i32, i32), std::ops::Range<i32>> =
        Default::default();
    for x in 0..3 {
        for y in 0..3 {
            let mut terrain = vec![0u8; tiles::COUNT];
            for side in [tiles::TOP, tiles::RIGHT, tiles::BOTTOM, tiles::LEFT] {
                let back = (x + tiles::dx(side), y + tiles::dy(side), (side + 3) % 8 + 1);
                let from = dice.below(40);
                let open = match borders.get(&back) {
                    Some(_) if dice.below(4) == 0 => 0..0,
                    Some(open) => open.clone(),
                    None if dice.chance(3) => 0..0,
                    None => from..from + 1 + dice.below(8),
                };
                borders.insert((x, y, side), open.clone());
                for (i, &t) in tiles::exit_tiles(side).iter().enumerate() {
                    if !open.contains(&(i as i32)) {
                        terrain[t] = terrain::WALL;
                    }
                }
            }
            for t in &mut terrain {
                if *t == 0 && dice.below(5) == 0 {
                    *t = terrain::SWAMP;
                }
            }
            rooms.push(room_of(RoomName::new(x, y), terrain, Vec::new()));
        }
    }
    let mut world = world_of_rooms(rooms);
    let ring = (-1..=3).flat_map(|x| (-1..=3).map(move |y| (x, y)));
    world.closed = ring
        .filter(|&(x, y)| !(0..3).contains(&x) || !(0..3).contains(&y))
        .map(|(x, y)| RoomName::new(x, y))
        .collect();
    world
}

#[test]
fn a_body_walking_first_steps_never_enters_a_room_twice_and_arrives_where_a_route_runs() {
    let mut dice = Dice::new(22);
    for _ in 0..CASES {
        let world = walled_rooms(&mut dice);
        let atlas = Atlas::of_world(&world);
        let open = |dice: &mut Dice| tiles::index(1 + dice.below(48), 1 + dice.below(48));
        let room = |dice: &mut Dice| RoomName::new(dice.below(3), dice.below(3));
        let mut body = parts(&mut dice, 10);
        body.push(Part::Move);
        let mover = Mover {
            footprint: Footprint::One,
            factor: loaded_factor(&body),
        };
        let (from, onto) = (room(&mut dice), room(&mut dice));
        let target = Target {
            room: onto,
            tiles: vec![open(&mut dice)],
            range: dice.below(3),
            avoid: BTreeSet::new(),
            max_hops: 8,
        };
        let mut at = tiles::to_pos(from, open(&mut dice));
        let mut entered = vec![from];
        let mut arrived = false;
        for _ in 0..4 * tiles::COUNT {
            if at.room == onto && atlas.travel_ticks(mover, &target, &at) == Some(0) {
                arrived = true;
                break;
            }
            let step = atlas.first_step(mover, &target, &at);
            let Some(next) = step.and_then(|d| tiles::neighbour(tiles::of_pos(&at), d)) else {
                break;
            };
            let across = tiles::room_toward(at.room, tiles::exit_of(next));
            at = match (across, tiles::landing(next)) {
                (Some(room), Some(landing)) => {
                    assert!(!entered.contains(&room), "{entered:?} back into {room}");
                    entered.push(room);
                    tiles::to_pos(room, landing)
                }
                _ => tiles::to_pos(at.room, next),
            };
        }
        let routed = atlas.route(&BTreeSet::new(), 8, from, onto).is_some();
        assert_eq!(arrived, routed, "{entered:?} toward {onto}");
    }
}

#[test]
fn a_route_goes_round_a_stronghold_where_another_way_stands() {
    let rooms: Vec<Room> = (0..3)
        .flat_map(|x| (0..2).map(move |y| RoomName::new(x, y)))
        .map(|name| room_of(name, vec![0; tiles::COUNT], Vec::new()))
        .collect();
    let world = world_of_rooms(rooms);
    let (from, core, onto) = (
        RoomName::new(0, 0),
        RoomName::new(1, 0),
        RoomName::new(2, 0),
    );
    let route = |stronghold: bool| {
        let atlas = Atlas::following(Atlas::default(), &world, [(core, None, stronghold)], []);
        atlas.route(&BTreeSet::new(), 4, from, onto).unwrap()
    };
    assert!(route(false).contains(&core));
    assert!(!route(true).contains(&core));
}

#[test]
fn a_walk_round_a_set_never_steps_on_it() {
    for (walk, mut dice) in walks(23) {
        let atlas = Atlas::of_world(&world_of(&walk.ground));
        let target = target_of(ROOM, &walk);
        let mut round = TileSet::default();
        for _ in 0..dice.below(200) {
            let t = tile(&mut dice);
            if t != walk.start {
                round.insert(t);
            }
        }
        let start = tiles::to_pos(ROOM, walk.start);
        for pos in atlas.walk(walk.mover, &target, Some(&round), &start, tiles::COUNT) {
            let on = covered(walk.mover.footprint, tiles::of_pos(&pos));
            assert!(on.iter().all(|&(x, y)| !round.contains(tiles::index(x, y))));
        }
    }
}

#[test]
fn a_hostiles_step_redoes_no_flood_tree_or_walk_and_no_first_step_lands_on_one() {
    for (walk, mut dice) in walks(24) {
        let hostile = |at: Tile| creep(Owner::Invader, tiles::to_pos(ROOM, at));
        let mut world = world_of(&walk.ground);
        let target = target_of(ROOM, &walk);
        let start = tiles::to_pos(ROOM, walk.start);
        let kept = |atlas: &Atlas| {
            atlas.ticks_to(walk.mover, &target, &start);
            atlas.walk(walk.mover, &target, None, &start, 100);
            atlas.tree(&BTreeSet::new(), 1, ROOM);
        };
        world.rooms[0].others = vec![hostile(tile(&mut dice))];
        let mut before = Atlas::of_world(&world);
        before.check = true;
        kept(&before);
        let near = tiles::around(walk.start);
        world.rooms[0].others = vec![hostile(near[dice.below(near.len() as i32) as usize])];
        let after = Atlas::following(before, &world, [], []);
        kept(&after);
        assert_eq!(after.recomputed(), 0);
        if walk.mover.factor.moves != 0 {
            assert_eq!(after.misses(), 0);
        }
        let step = after.first_step(walk.mover, &target, &start);
        let landed = step.and_then(|d| tiles::neighbour(walk.start, d));
        assert!(landed.is_none_or(|t| {
            (covered(walk.mover.footprint, t).iter())
                .all(|&(x, y)| after.passable(ROOM, tiles::index(x, y)))
        }));
    }
}

#[test]
fn an_unseen_keeper_room_reads_as_its_intel_once_intel_arrives() {
    let (from, keeper, onto) = (
        RoomName::new(3, 4),
        RoomName::new(4, 4),
        RoomName::new(5, 4),
    );
    let rooms: Vec<Room> = (3..=5)
        .flat_map(|x| (3..=4).map(move |y| RoomName::new(x, y)))
        .filter(|name| *name != keeper)
        .map(|name| room_of(name, vec![0; tiles::COUNT], Vec::new()))
        .collect();
    let world = world_of_rooms(rooms);
    let open = [0u8; tiles::COUNT];
    let posts = [(tiles::index(25, 25), tiles::index(25, 27))];
    let route = |atlas: &Atlas| atlas.route(&BTreeSet::new(), 4, from, onto).unwrap();
    let cold = Atlas::following(Atlas::default(), &world, [], [(keeper, &open)]);
    assert!(!route(&cold).contains(&keeper));
    let intel = || [(keeper, Some(&posts[..]), false)];
    let mut warm = Atlas::following(cold, &world, intel(), [(keeper, &open)]);
    let fresh = Atlas::following(Atlas::default(), &world, intel(), [(keeper, &open)]);
    assert!(route(&fresh).contains(&keeper));
    assert_eq!(route(&warm), route(&fresh));
    warm.check = true;
    let kept = Atlas::following(warm, &world, intel(), [(keeper, &open)]);
    assert_eq!(route(&kept), route(&fresh));
}

/// The root's serpentine makes its east border dearer than the way round
/// north, so the east room's north region is settled first and only the
/// root's border reaches its south one.
#[test]
fn each_stretch_into_a_room_split_by_walls_is_a_way_into_its_own_region() {
    let open = || vec![0u8; tiles::COUNT];
    let mut root = open();
    for (n, x) in [6, 12, 18, 24, 30, 36, 42].into_iter().enumerate() {
        let gap = if n % 2 == 0 { 48 } else { 1 };
        for y in (0..50).filter(|y| *y != gap) {
            root[tiles::index(x, y)] = terrain::WALL;
        }
    }
    let mut split = open();
    for x in 0..50 {
        split[tiles::index(x, 25)] = terrain::WALL;
    }
    let (r, north, round, east) = (
        RoomName::new(0, 0),
        RoomName::new(0, -1),
        RoomName::new(1, -1),
        RoomName::new(1, 0),
    );
    let mut world = world_of_rooms(vec![
        room_of(r, root, Vec::new()),
        room_of(north, open(), Vec::new()),
        room_of(round, open(), Vec::new()),
        room_of(east, split, Vec::new()),
    ]);
    world.closed = (-1..=2)
        .flat_map(|x| (-2..=1).map(move |y| RoomName::new(x, y)))
        .filter(|c| ![r, north, round, east].contains(c))
        .collect();
    let atlas = Atlas::of_world(&world);
    let mover = Mover {
        footprint: Footprint::One,
        factor: empty_factor(&[Part::Move]),
    };
    let target = Target {
        room: r,
        tiles: vec![tiles::index(2, 2)],
        range: 0,
        avoid: BTreeSet::new(),
        max_hops: 4,
    };
    for y in [10, 40] {
        let at = Pos::new(east, 10, y);
        assert!(atlas.travel_ticks(mover, &target, &at).is_some(), "{at:?}");
    }
}

#[test]
fn a_walk_toward_a_tile_follows_each_regions_way_on_where_split_rooms_lead_into_each_other() {
    // R and S are each cut in two: R's north half leaves through S's, round
    // by W, X and U; S's south half leaves through R's.
    let (r, s, t) = (
        RoomName::new(0, 0),
        RoomName::new(1, 0),
        RoomName::new(0, 1),
    );
    let rooms: Vec<Room> = [(0, 0), (1, 0), (2, 0), (0, 1), (1, 1), (2, 1)]
        .into_iter()
        .map(|(x, y)| {
            let mut terrain = vec![0u8; tiles::COUNT];
            let walls = match (x, y) {
                (0, 0) => vec![25],
                (1, 0) => vec![25, 49],
                _ => vec![],
            };
            for (i, row) in (0..tiles::SIDE).flat_map(|i| walls.iter().map(move |r| (i, *r))) {
                terrain[tiles::index(i, row)] = terrain::WALL;
            }
            if (x, y) == (1, 0) {
                (25..tiles::SIDE).for_each(|y| terrain[tiles::index(49, y)] = terrain::WALL);
            }
            room_of(RoomName::new(x, y), terrain, Vec::new())
        })
        .collect();
    let mut world = world_of_rooms(rooms);
    world.closed = (-1..=3)
        .flat_map(|x| (-1..=2).map(move |y| (x, y)))
        .filter(|&(x, y)| !(0..3).contains(&x) || !(0..2).contains(&y))
        .map(|(x, y)| RoomName::new(x, y))
        .collect();
    let atlas = Atlas::of_world(&world);
    let to = Pos::new(t, 25, 25);
    let ticks = |room, y| atlas.ticks_toward(8, &to, &Pos::new(room, 25, y)).unwrap();
    assert!(ticks(r, 10) > ticks(s, 10));
    assert!(ticks(s, 10) > ticks(s, 40));
    assert!(ticks(s, 40) > ticks(r, 40));
}
