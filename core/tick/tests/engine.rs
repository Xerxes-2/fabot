//! The engine's arithmetic, against values read off the engine and against
//! its own inverses.
use fabot_base::engine;
use fabot_base::testing::{Dice, boosted};
use fabot_base::vocabulary::{ALL_BODY_PARTS, Part, ResourceType};

const CASES: usize = 100;

/// A body head first with a boost per part, and hits within its parts.
fn bodied(dice: &mut Dice) -> (Vec<Part>, Vec<Option<ResourceType>>, i32) {
    let n = dice.between(1, engine::MAX_BODY_PARTS as i32);
    let body: Vec<Part> = (0..n).map(|_| dice.pick(&ALL_BODY_PARTS)).collect();
    let boosts = (body.iter())
        .map(|p| boosted(*p, dice.pick(&[0, 1, 2, 3])))
        .collect();
    (body, boosts, dice.between(1, n * engine::PART_HITS))
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
    let falloff: Vec<i32> = (1..=4).map(engine::mass_attack_power_at).collect();
    assert_eq!(falloff, vec![10, 4, 1, 0]);
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

#[test]
fn an_unboosted_body_takes_every_hit_of_a_strike() {
    let mut dice = Dice::new(41);
    for _ in 0..CASES {
        let (body, _, hits) = bodied(&mut dice);
        let damage = dice.between(1, 10_000);
        assert_eq!(engine::damage_taken(&body, &[], hits, damage), damage);
    }
}

#[test]
fn a_boosted_body_never_takes_more_than_the_strike() {
    let mut dice = Dice::new(42);
    for _ in 0..CASES {
        let (body, boosts, hits) = bodied(&mut dice);
        let damage = dice.between(1, 10_000);
        let taken = engine::damage_taken(&body, &boosts, hits, damage);
        assert!((0..=damage).contains(&taken), "{taken} of {damage}");
    }
}

#[test]
fn the_damage_priced_for_a_loss_takes_at_least_that_loss() {
    let mut dice = Dice::new(43);
    for _ in 0..CASES {
        let (body, boosts, hits) = bodied(&mut dice);
        let taken = dice.between(1, 10_000).min(hits);
        let damage = engine::damage_for(&body, &boosts, hits, taken);
        assert!(engine::damage_taken(&body, &boosts, hits, damage) >= taken - 1);
    }
}
