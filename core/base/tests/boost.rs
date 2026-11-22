//! `boosted_by`: each part type's strongest covering compound, spent in visit order.
use crate::boost::{Held, Stock, boosted_by};
use crate::vocabulary::Part;
use crate::vocabulary::ResourceType::{
    self, CatalyzedGhodiumAlkalide as Tough3, CatalyzedUtriumAcid as T3, GhodiumOxide as Tough1,
    UtriumAcid as T2, UtriumHydride as T1, ZynthiumOxide as Move1,
};

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

#[test]
fn a_compound_its_labs_already_hold_needs_no_fill() {
    let body = [[Part::Attack; 10], [Part::Move; 10]].concat();
    let mut held = stock(&[(T1, 50), (Move1, 10)], 40);
    held.held[0].in_labs = 10;
    assert_eq!(boosted_by(&body, &mut held).fills, 1);
}
