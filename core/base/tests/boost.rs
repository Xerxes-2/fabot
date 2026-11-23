//! `boosted_by`: each part type's strongest covering compound, spent in visit
//! order; its stock held over generated bodies.
use crate::boost::{Held, Stock, Visits, boosted_by};
use crate::vocabulary::ResourceType::{
    self, CatalyzedGhodiumAlkalide as Tough3, CatalyzedUtriumAcid as T3, GhodiumOxide as Tough1,
    UtriumAcid as T2, UtriumHydride as T1, ZynthiumOxide as Move1,
};
use crate::vocabulary::{
    ALL_BODY_PARTS, BOOSTS, Part, RESOURCES_ALL, ores_in, part_count_in, takes,
};
use proptest::prelude::*;
use proptest::sample::subsequence;

fn stock(held: &[(ResourceType, i32)], energy: i32) -> Stock {
    let held = (held.iter())
        .map(|(compound, parts)| Held {
            compound: *compound,
            parts: *parts,
            in_labs: 0,
        })
        .collect();
    Stock { held, energy }
}

#[test]
fn each_part_type_takes_the_strongest_compound_covering_all_its_parts() {
    let body = [[Part::Attack; 10], [Part::Move; 10]].concat();
    let all = [(T1, 50), (T2, 10), (T3, 9), (Move1, 10)];
    let compounds =
        |body: &[Part], held: &[_], energy| boosted_by(body, &mut stock(held, energy)).compounds;
    assert_eq!(compounds(&body, &all, 40), [Move1, T2]);
    assert_eq!(compounds(&body, &[(T1, 50)], 40), [T1]);
    assert_eq!(compounds(&body[10..], &[(T1, 50)], 40), []);
    let tough = [Part::Tough, Part::Move];
    assert_eq!(compounds(&tough, &[(Tough1, 1), (Tough3, 1)], 2), [Tough3]);
}

#[test]
fn energy_runs_short_of_the_later_part_types_first() {
    let body = [[Part::Attack; 10], [Part::Move; 10]].concat();
    let mut held = stock(&[(T1, 50), (Move1, 10)], 15);
    assert_eq!(boosted_by(&body, &mut held).compounds, [Move1]);
    assert_eq!((held.energy, held.held[1].parts), (5, 0));
}

const CASES: u32 = 256;

/// Every compound that boosts some part, as `Stock::of` reads them.
fn compounds() -> Vec<ResourceType> {
    (RESOURCES_ALL.iter().copied())
        .filter(|c| ALL_BODY_PARTS.iter().any(|p| takes(*p, *c)))
        .collect()
}

/// The one part type a compound boosts: nothing but its own visit spends it.
fn part_of(c: ResourceType) -> Part {
    let parts: Vec<Part> = (ALL_BODY_PARTS.into_iter())
        .filter(|p| takes(*p, c))
        .collect();
    assert_eq!(parts.len(), 1, "{c:?} boosts {parts:?}");
    parts[0]
}

fn held_of(stock: &Stock, c: ResourceType) -> &Held {
    let found = stock.held.iter().find(|h| h.compound == c);
    found.expect("a visit to a compound the stock never held")
}

/// The owner's table, apart from `boosted_by`'s own read of it.
fn rank(p: Part) -> Option<i32> {
    BOOSTS.iter().find(|(c, _)| takes(p, *c)).map(|(_, r)| *r)
}

/// T1 < T2 < T3 by the ores reacted in.
fn tier(c: ResourceType) -> i32 {
    ores_in(c).iter().sum()
}

fn near(x: i32) -> impl Strategy<Value = i32> {
    (x - 1..=x + 1).prop_map(|n| n.max(0))
}

fn body() -> impl Strategy<Value = Vec<Part>> {
    let part = || prop::sample::select(&ALL_BODY_PARTS[..]);
    prop_oneof![
        Just(Vec::new()),
        (part(), 1..=50usize).prop_map(|(p, n)| vec![p; n]),
        prop::collection::vec(part(), 0..=50),
    ]
}

/// At least one part: `Stock::of` leaves out a compound it holds none of.
fn holding(compound: ResourceType, n: i32) -> impl Strategy<Value = Held> {
    let parts = prop_oneof![near(n), 0..=60i32, Just(1_000)].prop_map(|p| p.max(1));
    parts.prop_flat_map(move |parts| {
        let in_labs = prop_oneof![
            Just(0),
            Just(parts),
            0..=parts,
            near(n).prop_map(move |l| l.min(parts)),
        ];
        in_labs.prop_map(move |in_labs| Held {
            compound,
            parts,
            in_labs,
        })
    })
}

fn case() -> impl Strategy<Value = (Vec<Part>, Stock)> {
    let all = compounds();
    let held = subsequence(all.clone(), 0..=all.len()).prop_shuffle();
    (body(), held).prop_flat_map(|(body, held)| {
        let total = (body.iter())
            .filter(|p| held.iter().any(|c| takes(**p, *c)))
            .count() as i32;
        let held: Vec<_> = (held.into_iter())
            .map(|c| holding(c, part_count_in(&body, part_of(c))))
            .collect();
        let energy = prop_oneof![Just(0), near(total), 0..=60i32, Just(1_000)];
        (Just(body), held, energy).prop_map(|(body, held, energy)| (body, Stock { held, energy }))
    })
}

fn visit(body: &[Part], before: &Stock) -> (Visits, Stock) {
    let mut after = before.clone();
    (boosted_by(body, &mut after), after)
}

proptest! {
    #![proptest_config(crate::testing::config(CASES))]

    #[test]
    fn a_visit_spends_its_part_count_of_its_compound_and_of_energy((body, before) in case()) {
        let (visits, after) = visit(&body, &before);
        let n = |c: &ResourceType| part_count_in(&body, part_of(*c));
        let spent: i32 = visits.compounds.iter().map(n).sum();
        prop_assert_eq!(before.energy - after.energy, spent);
        prop_assert!(after.energy >= 0);
        prop_assert_eq!(before.held.len(), after.held.len());
        for (b, a) in before.held.iter().zip(&after.held) {
            prop_assert_eq!(b.compound, a.compound);
            let of: i32 = visits.compounds.iter().filter(|c| **c == b.compound).map(n).sum();
            prop_assert_eq!(b.parts - a.parts, of, "{:?} -> {:?}", b, a);
            prop_assert!(
                a.parts >= 0 && a.in_labs >= 0 && a.in_labs <= b.in_labs,
                "{b:?} -> {a:?}"
            );
        }
    }

    #[test]
    fn each_visit_boosts_a_part_type_of_the_body_once_with_a_held_compound(
        (body, before) in case(),
    ) {
        let (visits, _) = visit(&body, &before);
        let parts: Vec<Part> = visits.compounds.iter().map(|c| part_of(*c)).collect();
        for (i, (c, p)) in visits.compounds.iter().zip(&parts).enumerate() {
            prop_assert!(body.contains(p), "{c:?} boosts {p:?}, not in the body");
            prop_assert!(before.held.iter().any(|h| h.compound == *c), "{c:?} never held");
            prop_assert!(!parts[..i].contains(p), "{p:?} boosted twice: {visits:?}");
        }
    }

    #[test]
    fn a_fill_is_a_visit_its_labs_held_too_little_for((body, before) in case()) {
        let (visits, _) = visit(&body, &before);
        let short = (visits.compounds.iter())
            .filter(|c| held_of(&before, **c).in_labs < part_count_in(&body, part_of(**c)))
            .count();
        prop_assert_eq!(visits.fills, short);
    }

    #[test]
    fn a_part_type_goes_bare_only_short_of_a_compound_or_of_what_lower_ranks_left(
        (body, before) in case(),
    ) {
        let (visits, after) = visit(&body, &before);
        let boosted: Vec<Part> = visits.compounds.iter().map(|c| part_of(*c)).collect();
        for p in ALL_BODY_PARTS.into_iter().filter(|p| body.contains(p) && !boosted.contains(p)) {
            let n = part_count_in(&body, p);
            let covered = before.held.iter().any(|h| takes(p, h.compound) && h.parts >= n);
            // Its visit found at least what is left and what the lower ranks spent after it.
            let lower: i32 = (boosted.iter())
                .filter(|q| rank(**q) < rank(p))
                .map(|q| part_count_in(&body, *q))
                .sum();
            prop_assert!(
                !covered || after.energy + lower < n,
                "{p:?} x{n} bare: {visits:?}, energy left {}", after.energy
            );
        }
    }

    #[test]
    fn with_stock_to_spare_every_part_type_held_for_takes_its_strongest(
        (body, scarce) in case(),
    ) {
        let held = (scarce.held.into_iter())
            .map(|h| Held { parts: h.parts + 50, ..h })
            .collect();
        let before = Stock { held, energy: scarce.energy + 50 };
        let (visits, _) = visit(&body, &before);
        for p in ALL_BODY_PARTS.into_iter().filter(|p| body.contains(p)) {
            let strongest = (before.held.iter())
                .filter(|h| takes(p, h.compound))
                .map(|h| h.compound)
                .max_by_key(|c| tier(*c));
            let took = visits.compounds.iter().copied().find(|c| part_of(*c) == p);
            prop_assert_eq!(took, strongest, "{:?}", p);
        }
    }
}
