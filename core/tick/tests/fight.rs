//! The fight's cast invariants: what a body a set walks out with is made of.
use fabot_base::engine;
use fabot_base::vocabulary::Part;
use fabot_fight::casting;
use proptest::prelude::*;

const CASES: u32 = 256;

proptest! {
    #![proptest_config(fabot_base::testing::config(CASES))]

    /// Every body cast for a kind that walks out of our rooms carries a HEAL
    /// (owner): shot up it heals the damage off instead of losing its MOVE to it
    /// and crawling a tile at a time. One MOVE at the tail is the leg it keeps
    /// longest, so the body still walks while any part lives.
    #[test]
    fn every_body_a_set_walks_out_with_carries_a_heal(
        capacity in prop_oneof![Just(430), Just(12_900), 430..=12_900],
    ) {
        let walked: Vec<casting::Body> = casting::walking(capacity)
            .into_iter()
            .chain(casting::razer(capacity, 100_000, &[]))
            .collect();

        for body in walked {
            prop_assert!(body.contains(&Part::Heal), "{body:?} at {capacity}");
            prop_assert!(body.len() <= engine::MAX_BODY_PARTS, "{body:?}");
            prop_assert!(engine::body_cost(&body) <= capacity, "{body:?}");
            prop_assert_eq!(body.last(), Some(&Part::Move), "{:?}", body);
        }
    }
}
