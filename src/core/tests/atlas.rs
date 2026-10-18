//! Invariants of the Atlas over generated rooms, and the tile and pace
//! arithmetic it rests on.
use std::collections::BTreeSet;

use super::Dice;
use crate::core::atlas::{Atlas, Mover, Target};
use crate::core::engine;
use crate::core::grid::{
    FatigueFactor, Footprint, empty_factor, factor_of, loaded_factor, on_edge, step_ticks,
};
use crate::core::tiles::{self, Tile};
use crate::core::vocabulary::{ALL_BODY_PARTS, BodyPart, Boost, Resource};
use crate::core::world::{
    Creep, Kind, Mineral, Owner, Part, Pos, Room, Source, Store, Structure, World, terrain,
};

const ROOM: &str = "W1N1";
const CASES: u64 = 100;

fn structure(kind: Kind, walkable: bool, x: i32, y: i32) -> Structure {
    Structure {
        id: format!("s{x},{y}"),
        kind,
        pos: Pos::new(ROOM, x, y),
        owner: Owner::Nobody,
        hits: 1,
        hits_max: 1,
        store: Store::empty(),
        cooldown: 0,
        collapse: 0,
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

fn room_of(name: &str, terrain: Vec<u8>, structures: Vec<Structure>) -> Room {
    Room {
        name: name.to_string(),
        terrain,
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
        our_sites: 0,
        rooms_allowed: 1,
    }
}

fn world_of(ground: &Ground) -> World {
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

fn parts(dice: &mut Dice, most: i32) -> Vec<BodyPart> {
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

fn target_of(room: &str, walk: &Walk) -> Target {
    Target {
        room: room.to_string(),
        tiles: vec![walk.goal],
        range: walk.range,
        avoid: BTreeSet::new(),
        max_hops: 4,
    }
}

/// Chebyshev range from what a footprint anchored at `anchor` covers to `tile`.
fn reach(footprint: Footprint, anchor: Tile, tile: Tile) -> i32 {
    let span = i32::from(footprint.is_quad());
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
fn followed(atlas: &mut Atlas, walk: &Walk, target: &Target) -> Vec<(Tile, i32)> {
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
            assert_eq!(
                tiles::room_coords(&tiles::room_name(x, y)),
                Some((x, y)),
                "{x},{y}"
            );
        }
    }
}

#[test]
fn names_read_as_the_engine_spells_them() {
    assert_eq!(tiles::room_coords("W12S28"), Some((-13, 28)));
    assert_eq!(tiles::room_coords("E0N0"), Some((0, -1)));
    assert_eq!(tiles::room_coords("sim"), None, "outside the grammar");
    assert_eq!(tiles::room_coords("W1"), None, "half a name");
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
                tiles::seam_range(&tiles::to_pos(ROOM, tile), &tiles::to_pos(&room, landing)),
                Some(0),
                "one tile across the seam"
            );
        }
        assert_eq!(
            tiles::exit_toward(ROOM, &room),
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
                .map(|&kind| Part {
                    kind,
                    hits: 100,
                    boost: Boost::Unboosted,
                })
                .collect(),
            hits: 100,
            hits_max: 100,
            ticks_to_live: 1,
            fatigue: 0,
            store: Store {
                held: vec![(Resource::Energy, engine::CARRY_PART_CAPACITY * 50)],
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
        let mut atlas = Atlas::of_world(&world_of(&walk.ground));
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
        let mut atlas = Atlas::of_world(&world_of(&walk.ground));
        let target = target_of(ROOM, &walk);
        let path = followed(&mut atlas, &walk, &target);
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
            pos: at.clone(),
            energy: 0,
            energy_capacity: 0,
            ticks_to_regeneration: 0,
        }];
        world.rooms[0].minerals = vec![Mineral {
            id: "min".into(),
            pos: across.clone(),
            resource: Resource::Energy,
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
        let mut stale =
            Atlas::of_world(&world_of(if kept { &walk.ground } else { &before.ground }));
        stale.travel_ticks(walk.mover, &target, &start);
        let mut warm = Atlas::following(&stale, &world);
        let first = warm.travel_ticks(walk.mover, &target, &start);
        let again = warm.travel_ticks(walk.mover, &target, &start);
        let step = warm.first_step(walk.mover, &target, &start);
        let mut fresh = Atlas::of_world(&world);
        assert_eq!(first, again);
        assert_eq!(first, fresh.travel_ticks(walk.mover, &target, &start));
        assert_eq!(step, fresh.first_step(walk.mover, &target, &start));
        if walk.mover.factor.moves != 0 {
            assert_eq!(warm.misses(), i32::from(!kept));
        }
    }
}

#[test]
fn toward_another_room_the_walk_ends_on_an_exit_into_it() {
    for (walk, mut dice) in walks(17) {
        let toward = dice.pick(&[tiles::TOP, tiles::RIGHT, tiles::BOTTOM, tiles::LEFT]);
        let onto = tiles::room_toward(ROOM, toward).unwrap();
        let mut atlas = Atlas::of_world(&world_of(&walk.ground));
        let target = target_of(&onto, &walk);
        let route = atlas.route(&BTreeSet::new(), target.max_hops, ROOM, &onto);
        match (followed(&mut atlas, &walk, &target).last(), route) {
            (Some(&(tile, 0)), Some(chain)) if chain.len() >= 2 => {
                let exit = tiles::exit_toward(ROOM, &chain[1]).unwrap();
                assert!(
                    covered(walk.mover.footprint, tile)
                        .iter()
                        .any(|&(x, y)| tiles::exit_of(tiles::index(x, y)) == exit)
                );
            }
            (Some(&(_, ticks)), _) => assert_eq!(ticks, 0),
            (None, _) => (),
        }
    }
}

#[test]
fn a_route_read_off_a_tree_is_the_route_searched() {
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
                rooms.push(room_of(&tiles::room_name(x, y), terrain, Vec::new()));
            }
        }
        let atlas = Atlas::of_world(&world_of_rooms(rooms));
        let hops = 1 + dice.below(4);
        let from = tiles::room_name(0, 0);
        let onto = tiles::room_name(dice.between(-128, 127) % 3, dice.between(-128, 127) % 3);
        assert_eq!(
            atlas.tree(hops, &from).route(&from, &onto),
            atlas.route(&BTreeSet::new(), hops, &from, &onto)
        );
    }
}

#[test]
fn a_route_crosses_shared_borders_around_the_avoided_by_fewest_crossings() {
    let mut dice = Dice::new(19);
    let atlas = Atlas::of_world(&world_of_rooms(Vec::new()));
    for _ in 0..CASES {
        let (dx, dy) = (dice.between(-128, 127) % 4, dice.between(-128, 127) % 4);
        let onto = tiles::room_name(dx, dy);
        let from = tiles::room_name(0, 0);
        let avoid: BTreeSet<String> = (0..dice.below(6))
            .map(|_| tiles::room_name(dice.between(-128, 127) % 4, dice.between(-128, 127) % 4))
            .collect();
        let hops = dx.abs() + dy.abs();
        match atlas.route(&avoid, 12, &from, &onto) {
            Some(chain) => {
                assert_eq!(chain[0], from);
                assert_eq!(chain[chain.len() - 1], onto);
                assert!(
                    chain
                        .windows(2)
                        .all(|p| tiles::exit_toward(&p[0], &p[1]).is_some())
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
