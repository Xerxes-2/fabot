//! The engine's arithmetic, against values read off the engine and against
//! its own inverses.
use proptest::{
    collection::vec,
    option,
    prelude::*,
    sample::{Index, select},
};

use crate::{
    engine,
    testing::boosted,
    vocabulary::{ALL_BODY_PARTS, Part, ResourceType},
};

const CASES: u32 = 256;

/// A body head first with a boost per part, and hits within its parts.
fn bodied() -> impl Strategy<Value = (Vec<Part>, Vec<Option<ResourceType>>, i32)> {
    let part = (select(ALL_BODY_PARTS.to_vec()), 0..=3usize);
    let hits = option::weighted(0.875, any::<Index>());
    (vec(part, 1..=engine::MAX_BODY_PARTS), hits).prop_map(|(parts, hits)| {
        let most = parts.len() * engine::PART_HITS as usize;
        let hits = hits.map_or(most, |at| 1 + at.index(most)) as i32;
        let (body, boosts) = (parts.iter()).map(|&(p, tier)| (p, boosted(p, tier))).unzip();
        (body, boosts, hits)
    })
}

fn damage() -> impl Strategy<Value = i32> {
    prop_oneof![1 => Just(1), 1 => Just(10_000), 6 => 1..=10_000]
}

#[test]
fn a_towers_power_falls_from_range_5_to_range_20_and_stays() {
    let table = [(1, 600), (5, 600), (6, 570), (10, 450), (15, 300), (20, 150), (30, 150)];
    for (range, expected) in table {
        assert_eq!(engine::tower_attack_at(range), expected, "attack at {range}");
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
    assert_eq!(engine::live_parts(&body, 200), vec![Attack, Move], "two parts gone");
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

#[test]
fn a_carry_holds_its_boosts_multiple() {
    use ResourceType::*;
    let boosts = [None, Some(KeaniumHydride), Some(KeaniumAcid), Some(CatalyzedKeaniumAcid)];
    assert_eq!(boosts.map(engine::part_capacity), [50, 100, 150, 200]);
}

#[test]
fn a_loaded_carry_weighs_and_a_boosted_one_holds_its_multiple() {
    use ResourceType::CatalyzedKeaniumAcid as Carry;

    use crate::grid::factor;
    let parts = |boost| [Part::Carry, Part::Carry, Part::Move].map(|p| (p, boost, true));
    assert_eq!(factor(parts(None).into_iter(), 0).weight, 0);
    assert_eq!(factor(parts(None).into_iter(), 51).weight, 2);
    assert_eq!(factor(parts(Some(Carry)).into_iter(), 51).weight, 1, "an XKH2O CARRY holds 200");
    let dead = [(Part::Carry, None, false), (Part::Carry, None, true)];
    assert_eq!(factor(dead.into_iter(), 100).weight, 1, "a destroyed CARRY carries nothing");
}

#[test]
fn a_volley_counts_a_creeps_whole_share_and_a_structures_hits_alone() {
    let worth = |m: &(i32, i32, bool), hit: &dyn Fn(i32) -> i32| {
        i64::from(engine::landed(hit(m.0), m.1, m.2))
    };
    let pays = |creep| {
        let marks = [(3, 100, creep), (1, 2, creep), (1, 2, creep)];
        engine::mass_pays(1, Some(marks[0]), marks.into_iter(), worth)
    };
    assert_eq!(pays(true), Some(21), "a creep's heal nets at the tick's end");
    assert_eq!(pays(false), None, "a structure of 2 hits takes 2 of a 10 share");
}

#[test]
fn a_room_sinks_its_climb_and_each_build_of_ours_read_off_the_engine() {
    use screeps::StructureType::{Extension, Spawn};

    use crate::{
        testing,
        world::{Owner, Pos, RoomName, terrain},
    };
    let plain = terrain::leak(vec![0; 2500]).unwrap();
    let room = |level, progress| {
        let mut r = testing::ours(RoomName::W1N1, level, plain);
        r.controller.as_mut().unwrap().progress = progress;
        r
    };
    let at = Pos::new(RoomName::W1N1, 10, 10);
    let theirs = testing::built("t", Spawn, at);
    let theirs = crate::world::Structure { owner: Owner::Enemy("x".into()), ..theirs };
    let built = [testing::built("s", Spawn, at), testing::built("e", Extension, at), theirs];
    let sunk =
        |level, progress, b: &[_]| engine::invested(room(level, progress).controller.as_ref(), b);
    assert_eq!(sunk(1, 150, &[]), 150);
    assert_eq!(sunk(3, 0, &built), 200 + 45_000 + 15_000 + 3_000);
    assert!(sunk(5, 0, &[]) > sunk(4, 134_999, &[]));
}
