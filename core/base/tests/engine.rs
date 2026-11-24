//! The engine's arithmetic, against values read off the engine and against
//! its own inverses.
use crate::engine;
use crate::testing::boosted;
use crate::vocabulary::{ALL_BODY_PARTS, Part, ResourceType};
use proptest::collection::vec;
use proptest::option;
use proptest::prelude::*;
use proptest::sample::{Index, select};

const CASES: u32 = 256;

/// A body head first with a boost per part, and hits within its parts.
fn bodied() -> impl Strategy<Value = (Vec<Part>, Vec<Option<ResourceType>>, i32)> {
    let part = (select(ALL_BODY_PARTS.to_vec()), 0..=3usize);
    let hits = option::weighted(0.875, any::<Index>());
    (vec(part, 1..=engine::MAX_BODY_PARTS), hits).prop_map(|(parts, hits)| {
        let most = parts.len() * engine::PART_HITS as usize;
        let hits = hits.map_or(most, |at| 1 + at.index(most)) as i32;
        let (body, boosts) = (parts.iter())
            .map(|&(p, tier)| (p, boosted(p, tier)))
            .unzip();
        (body, boosts, hits)
    })
}

fn damage() -> impl Strategy<Value = i32> {
    prop_oneof![1 => Just(1), 1 => Just(10_000), 6 => 1..=10_000]
}

#[test]
fn a_towers_power_falls_from_range_5_to_range_20_and_stays() {
    let table = [
        (1, 600),
        (5, 600),
        (6, 570),
        (10, 450),
        (15, 300),
        (20, 150),
        (30, 150),
    ];
    for (range, expected) in table {
        assert_eq!(
            engine::tower_attack_at(range),
            expected,
            "attack at {range}"
        );
    }
}

#[test]
fn a_mass_attack_falls_10_4_1_and_stops_past_three() {
    assert_eq!([1, 2, 3, 4].map(|r| engine::mass_at(2, r)), [20, 8, 2, 0]);
}

#[test]
fn a_swing_lands_beside_and_a_shot_within_three() {
    assert_eq!([1, 2, 4].map(|r| engine::strike_at(2, 4, r)), [100, 40, 0]);
}

#[test]
fn a_boosted_heal_lands_48_beside_16_within_three() {
    assert_eq!([1, 3, 4].map(|r| engine::heal_at(4, r)), [48, 16, 0]);
}

#[test]
fn the_live_parts_are_the_tail_the_hits_cover() {
    use Part::{Attack, Move, Tough};
    let body = [Tough, Tough, Attack, Move];
    assert_eq!(engine::live_parts(&body, 400), body.to_vec(), "whole");
    assert_eq!(
        engine::live_parts(&body, 201),
        vec![Tough, Attack, Move],
        "a part with a hit left counts"
    );
    assert_eq!(
        engine::live_parts(&body, 200),
        vec![Attack, Move],
        "two parts gone"
    );
}

proptest! {
    #![proptest_config(crate::testing::config(CASES))]

    #[test]
    fn an_unboosted_body_takes_every_hit_of_a_strike((body, _, hits) in bodied(), damage in damage()) {
        prop_assert_eq!(engine::damage_taken(&body, &[], hits, damage), damage);
    }

    #[test]
    fn a_boosted_body_never_takes_more_than_the_strike((body, boosts, hits) in bodied(), damage in damage()) {
        let taken = engine::damage_taken(&body, &boosts, hits, damage);
        prop_assert!((0..=damage).contains(&taken), "{} of {}", taken, damage);
    }

    #[test]
    fn the_damage_priced_for_a_loss_takes_at_least_that_loss((body, boosts, hits) in bodied(), loss in damage()) {
        let taken = loss.min(hits);
        let damage = engine::damage_for(&body, &boosts, hits, taken);
        prop_assert!(engine::damage_taken(&body, &boosts, hits, damage) >= taken - 1);
    }
}
