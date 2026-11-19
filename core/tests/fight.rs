//! The fight's cast invariants: what a body a set walks out with is made of.
use crate::fight::casting;
use fabot_base::engine;
use fabot_base::vocabulary::Part;

use fabot_base::testing::Dice;

/// Every body cast for a kind that walks out of our rooms carries a HEAL
/// (owner): shot up it heals the damage off instead of losing its MOVE to it
/// and crawling a tile at a time. One MOVE at the tail is the leg it keeps
/// longest, so the body still walks while any part lives.
#[test]
fn every_body_a_set_walks_out_with_carries_a_heal() {
    let mut dice = Dice::new(7);
    for _ in 0..200 {
        let capacity = dice.between(430, 12_900);
        let worth = dice.between(0, 40_000);
        let strike = dice.between(0, 600);

        let walked: Vec<casting::Body> = casting::walking(capacity)
            .into_iter()
            .chain(casting::squad(capacity, strike, worth))
            .chain(casting::razer(capacity, 100_000, &[]))
            .chain(casting::tapper(capacity))
            .collect();

        for body in walked {
            assert!(
                body.contains(&Part::Heal),
                "{body:?} at {capacity} for {worth} under {strike}"
            );
            assert!(body.len() <= engine::MAX_BODY_PARTS, "{body:?}");
            assert!(engine::body_cost(&body) <= capacity, "{body:?}");
            assert_eq!(body.last(), Some(&Part::Move), "{body:?}");
        }
    }
}
