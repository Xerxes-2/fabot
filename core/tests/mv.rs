//! Invariants of the movement arbitration over generated crowds (wishes packed
//! into a small walled box, with foreign bodies among them), and of the
//! wishes `plan` builds over generated rooms.
use std::collections::{BTreeMap, BTreeSet};

use fabot_atlas::{Atlas, Mover, Target};
use fabot_base::grid::{Footprint, factor_of};
use fabot_base::testing::Dice;
use fabot_base::tiles::{self, Tile, TileSet};
use fabot_base::vocabulary::Part;
use fabot_base::wish::Wish;
use fabot_base::world::{BodyPart, Creep, Owner, Room, RoomName, Store, World, terrain};
use fabot_mv::{arbitrate::arbitrate, plan};

const ROOM: RoomName = RoomName::named("W1N1");
const CASES: u64 = 500;

/// A room's ground, the tiles bodies outside the wishes hold, the wishes, and
/// the same wishes and holds in another order.
struct Crowd {
    passable: Vec<bool>,
    held: Vec<Tile>,
    wishes: Vec<Wish>,
    shuffled: Vec<Wish>,
    held_shuffled: Vec<Tile>,
}

/// A creep in a generated room with somewhere to walk: this room's tile or
/// the room west of it.
struct Errand {
    terrain: Vec<u8>,
    creep: Creep,
    target: Target,
}

fn shuffle<T>(dice: &mut Dice, xs: &mut [T]) {
    for i in (1..xs.len()).rev() {
        xs.swap(i, dice.below(i as i32 + 1) as usize);
    }
}

fn sub_list(dice: &mut Dice, xs: &[Tile]) -> Vec<Tile> {
    xs.iter().copied().filter(|_| dice.chance(2)).collect()
}

/// The box every crowd stands in: eight tiles a side, so bodies jostle.
fn crowd_box() -> Vec<Tile> {
    (20..28)
        .flat_map(|y| (20..28).map(move |x| tiles::index(x, y)))
        .collect()
}

fn around(tile: Tile) -> Vec<Tile> {
    tiles::within(1, tile)
        .into_iter()
        .filter(|&t| t != tile)
        .collect()
}

fn wish(dice: &mut Dice, name: String, at: Tile) -> Wish {
    let near = around(at);
    let stays = dice.chance(3);
    let step = dice.pick(&near);
    let onward = if dice.chance(around(step).len() as i32 + 1) {
        None
    } else {
        Some(dice.pick(&around(step)))
    };
    let mut tail = sub_list(dice, &near);
    shuffle(dice, &mut tail);
    // A work area is only ever handed to a body standing inside it.
    let mut working = sub_list(dice, &near);
    working.insert(0, at);
    let working = if dice.chance(2) { Vec::new() } else { working };
    Wish {
        creep: name,
        footprint: Footprint::One,
        at,
        head: if stays { at } else { step },
        onward: if stays { None } else { onward },
        tail,
        working,
        weight: dice.between(1, 6),
        moves: !dice.chance(10),
    }
}

fn crowd(dice: &mut Dice) -> Crowd {
    let cells = crowd_box();
    let mut passable = vec![true; tiles::COUNT];
    for &tile in &cells {
        if dice.chance(7) {
            passable[tile] = false;
        }
    }
    let mut open: Vec<Tile> = cells.into_iter().filter(|&t| passable[t]).collect();
    shuffle(dice, &mut open);
    let bodies = dice.between(0, 30.min(open.len() as i32)) as usize;
    let foreign = dice.between(0, 6.min((open.len() - bodies) as i32)) as usize;
    let wishes: Vec<Wish> = (0..bodies)
        .map(|i| wish(dice, format!("c{i}"), open[i]))
        .collect();
    let held = open[bodies..bodies + foreign].to_vec();
    let mut shuffled = wishes.clone();
    shuffle(dice, &mut shuffled);
    let mut held_shuffled = held.clone();
    shuffle(dice, &mut held_shuffled);
    Crowd {
        passable,
        held,
        wishes,
        shuffled,
        held_shuffled,
    }
}

fn crowds(seed: u64) -> impl Iterator<Item = Crowd> {
    let mut dice = Dice::new(seed);
    (0..CASES).map(move |_| crowd(&mut dice))
}

fn errand(dice: &mut Dice) -> Errand {
    let terrain = (0..tiles::COUNT)
        .map(|_| match dice.below(10) {
            0..=5 => 0,
            6 | 7 => terrain::SWAMP,
            _ => terrain::WALL,
        })
        .collect();
    let at = Tile::of_index(dice.below(tiles::COUNT as i32) as usize);
    let goal = Tile::of_index(dice.below(tiles::COUNT as i32) as usize);
    let range = dice.between(0, 3);
    let away = dice.chance(4);
    let body = (0..dice.between(1, 40))
        .map(|_| BodyPart {
            kind: dice.pick(&[Part::Move, Part::Work, Part::Carry]),
            hits: 100,
            boost: None,
        })
        .collect();
    let fatigue = if dice.chance(5) {
        dice.between(1, 10)
    } else {
        0
    };
    Errand {
        terrain,
        creep: Creep {
            id: "c".into(),
            name: "c".into(),
            owner: Owner::Ours,
            pos: tiles::to_pos(ROOM, at),
            body,
            hits: 100,
            hits_max: 100,
            ticks_to_live: 1000,
            fatigue,
            store: Store::empty(),
        },
        target: Target {
            room: if away { RoomName::named("W2N1") } else { ROOM },
            tiles: vec![goal],
            range,
            avoid: BTreeSet::new(),
            max_hops: 4,
        },
    }
}

fn errands(seed: u64) -> impl Iterator<Item = Errand> {
    let mut dice = Dice::new(seed);
    (0..CASES).map(move |_| errand(&mut dice))
}

fn resolve(crowd: &Crowd) -> Vec<(String, i32)> {
    arbitrate(|t| crowd.passable[t], &crowd.held, &crowd.wishes)
}

/// Each wish's index with the tile it ends on.
fn ends(wishes: &[Wish], moves: &[(String, i32)]) -> Vec<Tile> {
    let steps: BTreeMap<&str, i32> = moves.iter().map(|(n, d)| (n.as_str(), *d)).collect();
    wishes
        .iter()
        .map(|w| match steps.get(w.creep.as_str()) {
            Some(&direction) => {
                tiles::neighbour(w.at, direction).expect("a step stays in the room")
            }
            None => w.at,
        })
        .collect()
}

fn placed_in(crowd: &Crowd) -> Vec<Tile> {
    ends(&crowd.wishes, &resolve(crowd))
}

fn met(crowd: &Crowd, wish: &Wish) -> bool {
    crowd
        .wishes
        .iter()
        .any(|o| !o.movable() && o.at == wish.head && o.head == wish.at)
}

fn head_on(crowd: &Crowd, wish: &Wish) -> bool {
    crowd.wishes.iter().any(|o| {
        o.movable() && o.head == wish.head && o.onward == Some(wish.at) && o.creep != wish.creep
    })
}

fn listed(wish: &Wish, tile: Tile) -> bool {
    tile == wish.head || wish.tail.contains(&tile)
}

/// The crowd with its first wish the only one moving, and every other body
/// standing on its work.
fn duel(crowd: &Crowd) -> Vec<Wish> {
    crowd
        .wishes
        .iter()
        .enumerate()
        .map(|(i, w)| {
            if i == 0 {
                w.clone()
            } else {
                Wish {
                    head: w.at,
                    onward: None,
                    working: if w.working.is_empty() {
                        vec![w.at]
                    } else {
                        w.working.clone()
                    },
                    ..w.clone()
                }
            }
        })
        .collect()
}

fn world_of(errand: &Errand) -> World<'static> {
    World {
        time: 1,
        me: String::new(),
        cpu_limit: 20.0,
        bucket: 10_000,
        rooms: vec![Room {
            name: ROOM,
            terrain: terrain::leak(errand.terrain.clone()),
            controller: None,
            energy_available: 0,
            energy_capacity: 0,
            sources: Vec::new(),
            minerals: Vec::new(),
            structures: Vec::new(),
            sites: Vec::new(),
            piles: Vec::new(),
            remains: Vec::new(),
            others: Vec::new(),
        }],
        spawns: Vec::new(),
        creeps: vec![errand.creep.clone()],
        attacks: Vec::new(),
        destroyed: Vec::new(),
        our_sites: 0,
        rooms_allowed: 1,
        closed: Default::default(),
        rules: fabot_base::world::Rules::vanilla(),
    }
}

/// The errand's Atlas, its creep's pace, and the wish `plan::toward` builds.
fn planned(errand: &Errand) -> (Atlas, Mover, Wish) {
    let mut atlas = Atlas::of_world(&world_of(errand));
    let mover = Mover {
        footprint: Footprint::One,
        factor: factor_of(&errand.creep),
    };
    let wish = plan::toward(&mut atlas, &errand.creep, mover, &errand.target, 3);
    (atlas, mover, wish)
}

#[test]
fn no_two_bodies_end_on_one_tile_wished_or_held() {
    for crowd in crowds(21) {
        let mut all = placed_in(&crowd);
        all.extend(&crowd.held);
        let distinct: BTreeSet<Tile> = all.iter().copied().collect();
        assert_eq!(distinct.len(), all.len());
    }
}

#[test]
fn a_creep_steps_once_onto_a_passable_tile_beside_it_it_listed() {
    for crowd in crowds(22) {
        let moves = resolve(&crowd);
        let names: BTreeSet<&str> = moves.iter().map(|(n, _)| n.as_str()).collect();
        assert_eq!(names.len(), moves.len(), "once");
        for (name, direction) in &moves {
            let wish = crowd.wishes.iter().find(|w| &w.creep == name).unwrap();
            assert!((1..=8).contains(direction));
            let tile = tiles::neighbour(wish.at, *direction).expect("in the room");
            assert_eq!(tiles::range(wish.at, tile), 1);
            assert!(crowd.passable[tile]);
            assert!(listed(wish, tile));
        }
    }
}

#[test]
fn a_creep_that_cannot_move_is_never_moved() {
    for crowd in crowds(23) {
        let moved: BTreeSet<String> = resolve(&crowd).into_iter().map(|(n, _)| n).collect();
        for wish in &crowd.wishes {
            assert!(wish.moves || !moved.contains(&wish.creep));
        }
    }
}

#[test]
fn the_rooms_score_ends_above_everyone_staying_and_equal_only_when_nobody_moves() {
    for crowd in crowds(24) {
        let moves = resolve(&crowd);
        let before: i32 = crowd.wishes.iter().map(|w| w.value(w.at)).sum();
        let after: i32 = crowd
            .wishes
            .iter()
            .zip(ends(&crowd.wishes, &moves))
            .map(|(w, t)| w.value(t))
            .sum();
        if moves.is_empty() {
            assert_eq!(after, before);
        } else {
            assert!(after > before);
        }
    }
}

#[test]
fn a_displaced_staying_creeps_tile_is_held_by_a_creep_that_stepped_onto_it() {
    for crowd in crowds(25) {
        let placed = placed_in(&crowd);
        for (wish, &tile) in crowd.wishes.iter().zip(&placed) {
            assert!(
                !wish.stays()
                    || tile == wish.at
                    || crowd
                        .wishes
                        .iter()
                        .zip(&placed)
                        .any(|(other, &onto)| onto == wish.at && onto != other.at)
            );
        }
    }
}

#[test]
fn a_working_body_is_pushed_off_its_work_only_by_a_mover_two_weights_heavier() {
    for crowd in crowds(26) {
        let wishes = duel(&crowd);
        let Some(mover) = wishes.first() else {
            continue;
        };
        let moves = arbitrate(|t| crowd.passable[t], &crowd.held, &wishes);
        for (wish, tile) in wishes.iter().zip(ends(&wishes, &moves)).skip(1) {
            assert!(wish.working.contains(&tile) || mover.weight.max(1) >= wish.weight.max(1) + 2);
        }
    }
}

#[test]
fn a_body_shoved_off_a_step_lands_on_the_shovers_onward_step_only_when_nothing_else_is_free() {
    for crowd in crowds(27) {
        let Some(mover) = crowd.wishes.first() else {
            continue;
        };
        let Some(onward) = mover.onward else {
            continue;
        };
        if mover.stays()
            || !crowd.passable[mover.head]
            || onward == mover.at
            || !crowd.passable[onward]
        {
            continue;
        }
        let mut tail = vec![onward];
        tail.extend(around(mover.head).into_iter().filter(|&t| t != onward));
        let pushed = Wish {
            creep: "pushed".into(),
            footprint: Footprint::One,
            at: mover.head,
            head: mover.head,
            onward: None,
            tail,
            working: Vec::new(),
            weight: 1,
            moves: true,
        };
        let wishes = vec![
            Wish {
                moves: true,
                ..mover.clone()
            },
            pushed,
        ];
        let moves = arbitrate(|t| crowd.passable[t], &[], &wishes);
        assert_ne!(ends(&wishes, &moves)[1], onward);
    }
}

#[test]
fn no_creep_could_raise_its_own_value_stepping_alone_onto_a_free_tile_it_listed() {
    for crowd in crowds(28) {
        let placed = placed_in(&crowd);
        let taken: BTreeSet<Tile> = placed.iter().chain(&crowd.held).copied().collect();
        for (wish, &tile) in crowd.wishes.iter().zip(&placed) {
            if !wish.moves || met(&crowd, wish) || head_on(&crowd, wish) {
                continue;
            }
            for &free in std::iter::once(&wish.head).chain(&wish.tail) {
                assert!(
                    taken.contains(&free)
                        || tiles::range(wish.at, free) != 1
                        || !crowd.passable[free]
                        || wish.value(free) <= wish.value(tile)
                );
            }
        }
    }
}

#[test]
fn the_same_crowd_in_any_order_resolves_the_same() {
    for crowd in crowds(29) {
        let first = resolve(&crowd);
        assert_eq!(first, resolve(&crowd));
        let reordered = arbitrate(|t| crowd.passable[t], &crowd.held_shuffled, &crowd.shuffled);
        assert_eq!(first, reordered);
    }
}

#[test]
fn a_planned_walk_lists_passable_tiles_beside_it_its_tail_nearer_the_target_on_the_band_out_or_beside_its_head()
 {
    for errand in errands(30) {
        let (atlas, mover, wish) = planned(&errand);
        let at = wish.at;
        for &tile in std::iter::once(&wish.head).chain(&wish.tail) {
            assert!(tile == at || (tiles::range(at, tile) == 1 && atlas.passable(ROOM, tile)));
        }
        let far = atlas.travel_ticks(mover, &errand.target, &tiles::to_pos(ROOM, at));
        if wish.stays() || tiles::on_edge(at) && far.is_none() {
            continue;
        }
        for &tile in &wish.tail {
            let near = atlas.travel_ticks(mover, &errand.target, &tiles::to_pos(ROOM, tile));
            match (near, far) {
                (Some(near), Some(far)) => {
                    assert!(near < far || near == 0 || tiles::range(tile, wish.head) == 1)
                }
                _ => panic!("a tail tile off the walk"),
            }
        }
    }
}

#[test]
fn a_planned_wish_steps_onto_the_border_ring_only_where_its_walk_leaves_the_room() {
    for errand in errands(31) {
        let (atlas, mover, wish) = planned(&errand);
        for &tile in std::iter::once(&wish.head).chain(&wish.tail) {
            assert!(
                tile == wish.at
                    || !tiles::on_edge(tile)
                    || (errand.target.room != ROOM
                        && atlas.travel_ticks(mover, &errand.target, &tiles::to_pos(ROOM, tile))
                            == Some(0))
            );
        }
    }
}

#[test]
fn a_planned_wishs_working_tiles_hold_where_it_stands() {
    for errand in errands(32) {
        let (_, _, wish) = planned(&errand);
        assert!(wish.working.is_empty() || wish.working.contains(&wish.at));
    }
}

#[test]
fn a_planned_walk_never_steps_into_danger_and_steps_on_out_of_it_while_a_clear_tile_is_beside_it() {
    let mut dice = Dice::new(34);
    for errand in errands(34) {
        let mut danger = TileSet::default();
        let centre = Tile::of_index(dice.below(tiles::COUNT as i32) as usize);
        for t in tiles::within(dice.between(1, 5), centre) {
            danger.insert(t);
        }
        let mut atlas = Atlas::of_world(&world_of(&errand));
        let mover = Mover {
            footprint: Footprint::One,
            factor: factor_of(&errand.creep),
        };
        let wish = plan::toward_round(
            &mut atlas,
            &errand.creep,
            mover,
            &errand.target,
            3,
            Some(&danger),
        );
        let clear = |t: Tile| atlas.passable(ROOM, t) && !danger.contains(t) && !tiles::on_edge(t);
        if !danger.contains(wish.at) {
            assert!(wish.stays() || !danger.contains(wish.head));
        } else if wish.moves && around(wish.at).into_iter().any(clear) {
            assert!(!wish.stays());
        }
    }
}

#[test]
fn a_creep_that_cannot_move_is_planned_to_stay_without_a_flood() {
    for errand in errands(33) {
        let (atlas, mover, wish) = planned(&errand);
        assert!(
            plan::can_move(&errand.creep, mover.factor)
                || (!wish.moves
                    && (plan::rests_soon(&errand.creep, mover.factor)
                        || wish.stays() && atlas.misses() == 0))
        );
    }
}

#[test]
fn a_mover_met_head_on_by_a_held_body_that_steps_onto_its_tile_once_rested_waits_to_swap() {
    for crowd in crowds(34) {
        for (wish, tile) in crowd.wishes.iter().zip(placed_in(&crowd)) {
            assert!(!met(&crowd, wish) || tile == wish.at);
        }
    }
}

#[test]
fn a_mover_met_head_on_through_its_step_ends_on_its_step_or_its_own_tile() {
    for crowd in crowds(35) {
        for (wish, tile) in crowd.wishes.iter().zip(placed_in(&crowd)) {
            let free = crowd.passable[wish.head]
                && !crowd.held.contains(&wish.head)
                && crowd
                    .wishes
                    .iter()
                    .all(|o| o.movable() || o.at != wish.head);
            assert!(!(head_on(&crowd, wish) && free) || tile == wish.head || tile == wish.at);
        }
    }
}
