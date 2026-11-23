//! Invariants of the movement arbitration over generated crowds (wishes packed
//! into a small walled box, with foreign bodies among them), and of the
//! wishes `plan` builds over generated rooms.
use std::collections::{BTreeMap, BTreeSet};

use crate::{arbitrate::arbitrate, plan};
use fabot_atlas::{Atlas, Mover, Target};
use fabot_base::grid::{Footprint, factor_of};
use fabot_base::tiles::{self, Tile, TileSet};
use fabot_base::vocabulary::Part;
use fabot_base::wish::Wish;
use fabot_base::world::{BodyPart, Creep, Owner, Room, RoomName, Store, World, terrain};
use proptest::bool::weighted;
use proptest::collection::vec;
use proptest::prelude::*;
use proptest::sample::{Index, select};

const ROOM: RoomName = RoomName::W1N1;
const CASES: u32 = 500;

/// A room's ground, the tiles bodies outside the wishes hold, the wishes, and
/// the same wishes and holds in another order.
#[derive(Debug)]
struct Crowd {
    passable: Vec<bool>,
    held: Vec<Tile>,
    wishes: Vec<Wish>,
    shuffled: Vec<Wish>,
    held_shuffled: Vec<Tile>,
}

/// A creep in a generated room with somewhere to walk: this room's tile or
/// the room west of it.
#[derive(Debug)]
struct Errand {
    terrain: Vec<u8>,
    creep: Creep,
    target: Target,
}

/// The order `0..n` comes in.
fn order(n: usize) -> impl Strategy<Value = Vec<usize>> {
    Just((0..n).collect::<Vec<_>>()).prop_shuffle()
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

type Draw = (
    bool,
    Index,
    Index,
    (Vec<usize>, Vec<bool>),
    Option<Vec<bool>>,
    i32,
    bool,
);

fn draw() -> impl Strategy<Value = Draw> {
    (
        weighted(1.0 / 3.0),
        any::<Index>(),
        any::<Index>(),
        (order(8), vec(any::<bool>(), 8)),
        proptest::option::of(vec(any::<bool>(), 8)),
        1..=6,
        weighted(0.9),
    )
}

fn wish(draw: Draw, name: String, at: Tile) -> Wish {
    let (stays, step, onward, (order, tail), working, weight, moves) = draw;
    let near = around(at);
    let step = near[step.index(near.len())];
    let next = around(step);
    let onward = next.get(onward.index(next.len() + 1)).copied();
    let tail = order
        .into_iter()
        .filter(|&i| i < near.len() && tail[i])
        .map(|i| near[i])
        .collect();
    // A work area is only ever handed to a body standing inside it.
    let working = working.map_or(Vec::new(), |w| {
        let kept = near.iter().zip(w).filter(|(_, k)| *k).map(|(t, _)| *t);
        std::iter::once(at).chain(kept).collect()
    });
    Wish {
        creep: name,
        footprint: Footprint::One,
        at,
        head: if stays { at } else { step },
        onward: if stays { None } else { onward },
        tail,
        working,
        weight,
        moves,
    }
}

fn crowd() -> impl Strategy<Value = Crowd> {
    (
        vec(weighted(1.0 / 7.0), 64),
        Just(crowd_box()).prop_shuffle(),
        any::<(Index, Index)>(),
        vec(draw(), 30),
        (order(30), order(6)),
    )
        .prop_map(
            |(walls, cells, (bodies, foreign), draws, (order, held_order))| {
                let mut passable = vec![true; tiles::COUNT];
                for (tile, wall) in crowd_box().into_iter().zip(walls) {
                    passable[tile] = !wall;
                }
                let open: Vec<Tile> = cells.into_iter().filter(|&t| passable[t]).collect();
                let bodies = bodies.index(30.min(open.len()) + 1);
                let foreign = foreign.index(6.min(open.len() - bodies) + 1);
                let wishes: Vec<Wish> = (draws.into_iter().zip(&open).take(bodies).enumerate())
                    .map(|(i, (draw, &at))| wish(draw, format!("c{i}"), at))
                    .collect();
                let held = open[bodies..bodies + foreign].to_vec();
                let shuffled = order.iter().filter(|&&i| i < bodies);
                let held_shuffled = held_order.iter().filter(|&&i| i < foreign);
                Crowd {
                    passable,
                    shuffled: shuffled.map(|&i| wishes[i].clone()).collect(),
                    held_shuffled: held_shuffled.map(|&i| held[i]).collect(),
                    held,
                    wishes,
                }
            },
        )
}

fn errand() -> impl Strategy<Value = Errand> {
    let ground = prop_oneof![6 => Just(0), 2 => Just(terrain::SWAMP), 2 => Just(terrain::WALL)];
    let fatigue = prop_oneof![4 => Just(0), 1 => 1..=10];
    let part = select(&[Part::Move, Part::Work, Part::Carry][..]);
    // The server's map generator walls the border but exits, and opens no exit
    // on a corner (`@screeps/backend` lib/cli/map.js, `_genExit`).
    let walled = vec(ground, tiles::COUNT).prop_map(|mut terrain| {
        for corner in [(0, 0), (49, 0), (0, 49), (49, 49)] {
            terrain[tiles::index(corner.0, corner.1)] = terrain::WALL;
        }
        terrain
    });
    (
        walled,
        (0..tiles::COUNT, 0..tiles::COUNT),
        (0..=3, weighted(0.25)),
        (vec(part, 1..=40), fatigue),
    )
        .prop_map(
            |(terrain, (at, goal), (range, away), (parts, fatigue))| Errand {
                terrain,
                creep: Creep {
                    id: "c".into(),
                    name: "c".into(),
                    owner: Owner::Ours,
                    pos: tiles::to_pos(ROOM, Tile::of_index(at)),
                    body: (parts.into_iter())
                        .map(|kind| BodyPart {
                            kind,
                            hits: 100,
                            boost: None,
                        })
                        .collect(),
                    hits: 100,
                    hits_max: 100,
                    ticks_to_live: 1000,
                    fatigue,
                    store: Store::empty(),
                },
                target: Target {
                    room: if away {
                        "W2N1".parse::<RoomName>().unwrap()
                    } else {
                        ROOM
                    },
                    tiles: vec![Tile::of_index(goal)],
                    range,
                    avoid: BTreeSet::new(),
                    max_hops: 4,
                },
            },
        )
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
/// standing on its work; at `edge`, the mover steps onto a neighbour one
/// weight lighter, the rule's boundary.
fn duel(crowd: &Crowd, edge: bool) -> Vec<Wish> {
    let mut wishes: Vec<Wish> = crowd
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
        .collect();
    let first = wishes.first().map(|m| m.at);
    let beside = wishes
        .iter()
        .skip(1)
        .find(|w| first.is_some_and(|at| tiles::range(w.at, at) == 1));
    if let (true, Some((head, weight))) = (edge, beside.map(|w| (w.at, w.weight.max(1) + 1))) {
        wishes[0] = Wish {
            head,
            onward: None,
            weight,
            moves: true,
            ..wishes[0].clone()
        };
    }
    wishes
}

fn world_of(errand: &Errand) -> World<'static> {
    World {
        time: 1,
        me: String::new(),
        cpu_limit: 20.0,
        bucket: 10_000,
        rooms: vec![Room {
            terrain: terrain::leak(errand.terrain.clone()),
            ..Room::empty(ROOM)
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

proptest! {
    #![proptest_config(fabot_base::testing::config(CASES))]

    #[test]
    fn no_two_bodies_end_on_one_tile_wished_or_held(crowd in crowd()) {
        let mut all = placed_in(&crowd);
        all.extend(&crowd.held);
        let distinct: BTreeSet<Tile> = all.iter().copied().collect();
        prop_assert_eq!(distinct.len(), all.len());
    }

    #[test]
    fn a_creep_steps_once_onto_a_passable_tile_beside_it_it_listed(crowd in crowd()) {
        let moves = resolve(&crowd);
        let names: BTreeSet<&str> = moves.iter().map(|(n, _)| n.as_str()).collect();
        prop_assert_eq!(names.len(), moves.len(), "once");
        for (name, direction) in &moves {
            let wish = crowd.wishes.iter().find(|w| &w.creep == name).unwrap();
            prop_assert!((1..=8).contains(direction));
            let tile = tiles::neighbour(wish.at, *direction).expect("in the room");
            prop_assert_eq!(tiles::range(wish.at, tile), 1);
            prop_assert!(crowd.passable[tile]);
            prop_assert!(listed(wish, tile));
        }
    }

    #[test]
    fn a_creep_that_cannot_move_is_never_moved(crowd in crowd()) {
        let moved: BTreeSet<String> = resolve(&crowd).into_iter().map(|(n, _)| n).collect();
        for wish in &crowd.wishes {
            prop_assert!(wish.moves || !moved.contains(&wish.creep));
        }
    }

    #[test]
    fn the_rooms_score_ends_above_everyone_staying_and_equal_only_when_nobody_moves(crowd in crowd()) {
        let moves = resolve(&crowd);
        let before: i32 = crowd.wishes.iter().map(|w| w.value(w.at)).sum();
        let after: i32 = crowd
            .wishes
            .iter()
            .zip(ends(&crowd.wishes, &moves))
            .map(|(w, t)| w.value(t))
            .sum();
        if moves.is_empty() {
            prop_assert_eq!(after, before);
        } else {
            prop_assert!(after > before);
        }
    }

    #[test]
    fn a_displaced_staying_creeps_tile_is_held_by_a_creep_that_stepped_onto_it(crowd in crowd()) {
        let placed = placed_in(&crowd);
        for (wish, &tile) in crowd.wishes.iter().zip(&placed) {
            prop_assert!(
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

    #[test]
    fn a_working_body_is_pushed_off_its_work_only_by_a_mover_two_weights_heavier(crowd in crowd(), edge in any::<bool>()) {
        let wishes = duel(&crowd, edge);
        let Some(mover) = wishes.first() else {
            return Ok(());
        };
        let moves = arbitrate(|t| crowd.passable[t], &crowd.held, &wishes);
        for (wish, tile) in wishes.iter().zip(ends(&wishes, &moves)).skip(1) {
            prop_assert!(wish.working.contains(&tile) || mover.weight.max(1) >= wish.weight.max(1) + 2);
        }
    }

    #[test]
    fn a_body_shoved_off_a_step_lands_on_the_shovers_onward_step_only_when_nothing_else_is_free(crowd in crowd()) {
        let Some(mover) = crowd.wishes.first() else {
            return Ok(());
        };
        let Some(onward) = mover.onward else {
            return Ok(());
        };
        if mover.stays()
            || !crowd.passable[mover.head]
            || onward == mover.at
            || !crowd.passable[onward]
        {
            return Ok(());
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
        prop_assert_ne!(ends(&wishes, &moves)[1], onward);
    }

    #[test]
    fn no_creep_could_raise_its_own_value_stepping_alone_onto_a_free_tile_it_listed(crowd in crowd()) {
        let placed = placed_in(&crowd);
        let taken: BTreeSet<Tile> = placed.iter().chain(&crowd.held).copied().collect();
        for (wish, &tile) in crowd.wishes.iter().zip(&placed) {
            if !wish.moves || met(&crowd, wish) || head_on(&crowd, wish) {
                continue;
            }
            for &free in std::iter::once(&wish.head).chain(&wish.tail) {
                prop_assert!(
                    taken.contains(&free)
                        || tiles::range(wish.at, free) != 1
                        || !crowd.passable[free]
                        || wish.value(free) <= wish.value(tile)
                );
            }
        }
    }

    #[test]
    fn the_same_crowd_in_any_order_resolves_the_same(crowd in crowd()) {
        let first = resolve(&crowd);
        prop_assert_eq!(&first, &resolve(&crowd));
        let reordered = arbitrate(|t| crowd.passable[t], &crowd.held_shuffled, &crowd.shuffled);
        prop_assert_eq!(first, reordered);
    }

    #[test]
    fn a_planned_walk_lists_passable_tiles_beside_it_its_tail_nearer_the_target_on_the_band_out_or_beside_its_head(errand in errand()) {
        let (atlas, mover, wish) = planned(&errand);
        let at = wish.at;
        for &tile in std::iter::once(&wish.head).chain(&wish.tail) {
            prop_assert!(tile == at || (tiles::range(at, tile) == 1 && atlas.passable(ROOM, tile)));
        }
        let far = atlas.travel_ticks(mover, &errand.target, &tiles::to_pos(ROOM, at));
        if wish.stays() || tiles::on_edge(at) && far.is_none() {
            return Ok(());
        }
        for &tile in &wish.tail {
            let near = atlas.travel_ticks(mover, &errand.target, &tiles::to_pos(ROOM, tile));
            match (near, far) {
                (Some(near), Some(far)) => {
                    prop_assert!(near < far || near == 0 || tiles::range(tile, wish.head) == 1)
                }
                _ => panic!("a tail tile off the walk"),
            }
        }
    }

    #[test]
    fn a_planned_wish_steps_onto_the_border_ring_only_where_its_walk_leaves_the_room(errand in errand()) {
        let (atlas, mover, wish) = planned(&errand);
        for &tile in std::iter::once(&wish.head).chain(&wish.tail) {
            prop_assert!(
                tile == wish.at
                    || !tiles::on_edge(tile)
                    || (errand.target.room != ROOM
                        && atlas.travel_ticks(mover, &errand.target, &tiles::to_pos(ROOM, tile))
                            == Some(0))
            );
        }
    }

    #[test]
    fn a_planned_wishs_working_tiles_hold_where_it_stands(errand in errand()) {
        let (_, _, wish) = planned(&errand);
        prop_assert!(wish.working.is_empty() || wish.working.contains(&wish.at));
    }

    #[test]
    fn a_planned_walk_never_steps_into_danger_and_steps_on_out_of_it_while_a_clear_tile_is_beside_it(
        errand in errand(),
        centre in 0..tiles::COUNT,
        radius in 1..=5,
    ) {
        let mut danger = TileSet::default();
        for t in tiles::within(radius, Tile::of_index(centre)) {
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
            // Left out: a body on the border steps in onto danger (open finding).
            let edge = tiles::on_edge(wish.at);
            prop_assert!(edge || wish.stays() || !danger.contains(wish.head));
        } else if wish.moves && around(wish.at).into_iter().any(clear) {
            prop_assert!(!wish.stays());
        }
    }

    #[test]
    fn a_creep_that_cannot_move_is_planned_to_stay_without_a_flood(errand in errand()) {
        let (atlas, _, wish) = planned(&errand);
        // The engine's rule, not the planner's: a body steps with no fatigue and
        // a MOVE, and each MOVE sheds two fatigue a tick.
        let legs = errand.creep.body.iter().filter(|p| p.kind == Part::Move).count() as i32;
        let fatigue = errand.creep.fatigue;
        prop_assert!(
            fatigue == 0 && legs > 0
                || (!wish.moves
                    && (legs > 0 && fatigue <= 4 * legs
                        || wish.stays() && atlas.misses() == 0))
        );
    }

    #[test]
    fn a_mover_met_head_on_by_a_held_body_that_steps_onto_its_tile_once_rested_waits_to_swap(crowd in crowd()) {
        for (wish, tile) in crowd.wishes.iter().zip(placed_in(&crowd)) {
            prop_assert!(!met(&crowd, wish) || tile == wish.at);
        }
    }

    #[test]
    fn a_mover_met_head_on_through_its_step_ends_on_its_step_or_its_own_tile(crowd in crowd()) {
        for (wish, tile) in crowd.wishes.iter().zip(placed_in(&crowd)) {
            let free = crowd.passable[wish.head]
                && !crowd.held.contains(&wish.head)
                && crowd
                    .wishes
                    .iter()
                    .all(|o| o.movable() || o.at != wish.head);
            prop_assert!(!(head_on(&crowd, wish) && free) || tile == wish.head || tile == wish.at);
        }
    }
}
