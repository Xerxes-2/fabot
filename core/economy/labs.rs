/// The lab cluster's reaction plan, pure over stocks and labs: which compound
/// it makes next, which lab plays which part, what each lab is filled with or
/// drained of. Derived whole every tick from what the stores hold, so a lost
/// tick or a reset loses nothing.
use std::collections::BTreeMap;

use crate::engine;
use crate::vocabulary::{Compound, Resource};
use crate::world::Pos;

/// Toward the first target (in compound order) its stock stands short of. A
/// reagent compound short of the shortfall is made first; once none can be,
/// the compound itself from what stands, so a reagent run out leaves the
/// cluster working on with the other.
pub fn next_reaction(
    targets: &BTreeMap<Compound, i32>,
    stock: &dyn Fn(&Resource) -> i32,
) -> Option<Compound> {
    fn toward(compound: Compound, want: i32, stock: &dyn Fn(&Resource) -> i32) -> Option<Compound> {
        let (first, second) = engine::reagents_of(compound);
        let deeper = [&first, &second]
            .into_iter()
            .find_map(|reagent| match reagent {
                Resource::Compound(inner) if stock(reagent) < want => {
                    toward(*inner, want - stock(reagent), stock)
                }
                _ => None,
            });
        deeper.or_else(|| {
            (stock(&first) >= engine::LAB_REACTION_AMOUNT
                && stock(&second) >= engine::LAB_REACTION_AMOUNT)
                .then_some(compound)
        })
    }

    targets.iter().find_map(|(compound, target)| {
        let short = target - stock(&Resource::Compound(*compound));
        (short > 0)
            .then(|| toward(*compound, short, stock))
            .flatten()
    })
}

/// The ore the cluster lacks to bring every target up: each shortfall broken
/// down through its recipe, a compound or ore already in `stock` spent before
/// the ore under it is counted. What the other colonies ship, and no more.
pub fn ore_short(
    targets: &BTreeMap<Compound, i32>,
    stock: &dyn Fn(&Resource) -> i32,
) -> BTreeMap<Resource, i32> {
    fn need(
        resource: &Resource,
        amount: i32,
        left: &mut BTreeMap<Resource, i32>,
        short: &mut BTreeMap<Resource, i32>,
        stock: &dyn Fn(&Resource) -> i32,
    ) {
        let have = left
            .entry(resource.clone())
            .or_insert_with(|| stock(resource));
        let spent = (*have).min(amount);
        *have -= spent;
        let rest = amount - spent;
        match resource {
            _ if rest <= 0 => (),
            Resource::Compound(c) => {
                let (first, second) = engine::reagents_of(*c);
                need(&first, rest, left, short, stock);
                need(&second, rest, left, short, stock);
            }
            Resource::Ore(_) => *short.entry(resource.clone()).or_insert(0) += rest,
            _ => (),
        }
    }

    let mut left = BTreeMap::new();
    let mut short = BTreeMap::new();
    for (compound, target) in targets {
        need(
            &Resource::Compound(*compound),
            *target,
            &mut left,
            &mut short,
            stock,
        );
    }
    short
}

/// The two reagent labs, and every lab within reach of both as an output.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Roles {
    pub reagents: (String, String),
    pub outputs: Vec<String>,
}

/// Off the labs' tiles alone, so they hold from tick to tick: the pair the
/// most others stand within reach of both of, ties to the lower ids.
pub fn roles_of(labs: &[(String, Pos)]) -> Option<Roles> {
    let mut sorted: Vec<&(String, Pos)> = labs.iter().collect();
    sorted.sort_by(|a, b| a.0.cmp(&b.0));
    let reaches = |a: &Pos, b: &Pos| a.range(b).is_some_and(|r| r <= engine::LAB_REACTION_RANGE);

    let mut best: Option<Roles> = None;
    for (i, (a, at_a)) in sorted.iter().map(|l| (&l.0, &l.1)).enumerate() {
        for (b, at_b) in sorted.iter().skip(i + 1).map(|l| (&l.0, &l.1)) {
            let outputs: Vec<String> = sorted
                .iter()
                .filter(|(id, at)| id != a && id != b && reaches(at, at_a) && reaches(at, at_b))
                .map(|(id, _)| id.clone())
                .collect();
            if best
                .as_ref()
                .map_or(!outputs.is_empty(), |r| outputs.len() > r.outputs.len())
            {
                best = Some(Roles {
                    reagents: (a.clone(), b.clone()),
                    outputs,
                });
            }
        }
    }
    best
}

/// One tick of the cluster: the reaction and the labs that run it, the fills
/// (lab, mineral, the room it has for it) and the drains (lab, mineral).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Plan {
    pub reaction: Option<(Compound, Roles)>,
    pub fills: Vec<(String, Resource, i32)>,
    pub drains: Vec<(String, Resource)>,
}

/// Each reagent lab holds its reagent: anything else is drained, and one under
/// half full is filled to the brim, so a load arrives whole and a reaction
/// never starves. Each output holds the product: anything else is drained, and
/// the product once it stands half full. A reagent goes to the lab that already
/// holds it. With no reaction to run, the outputs are emptied.
pub fn plan_for(
    targets: &BTreeMap<Compound, i32>,
    stock: &dyn Fn(&Resource) -> i32,
    labs: &[(String, Pos)],
    held: &dyn Fn(&str) -> Option<(Resource, i32)>,
) -> Plan {
    let half = engine::LAB_MINERAL_CAPACITY / 2;
    let Some(roles) = roles_of(labs) else {
        return Plan::default();
    };

    let Some(compound) = next_reaction(targets, stock) else {
        return Plan {
            drains: roles
                .outputs
                .iter()
                .filter_map(|lab| held(lab).map(|(mineral, _)| (lab.clone(), mineral)))
                .collect(),
            ..Plan::default()
        };
    };

    let (first, second) = engine::reagents_of(compound);
    let (one, other) = roles.reagents.clone();
    let holds = |lab: &str, mineral: &Resource| held(lab).is_some_and(|(m, _)| &m == mineral);
    let (first_lab, second_lab) = if holds(&one, &second) || holds(&other, &first) {
        (other, one)
    } else {
        (one, other)
    };

    let mut plan = Plan::default();
    for (lab, reagent) in [(&first_lab, &first), (&second_lab, &second)] {
        match held(lab) {
            Some((m, _)) if &m != reagent => plan.drains.push((lab.clone(), m)),
            Some((_, amount)) if amount >= half => (),
            Some((_, amount)) => plan.fills.push((
                lab.clone(),
                reagent.clone(),
                engine::LAB_MINERAL_CAPACITY - amount,
            )),
            None => plan
                .fills
                .push((lab.clone(), reagent.clone(), engine::LAB_MINERAL_CAPACITY)),
        }
    }

    let product = Resource::Compound(compound);
    for lab in &roles.outputs {
        match held(lab) {
            Some((m, _)) if m != product => plan.drains.push((lab.clone(), m)),
            Some((_, amount)) if amount >= half => plan.drains.push((lab.clone(), product.clone())),
            _ => (),
        }
    }

    plan.reaction = Some((
        compound,
        Roles {
            reagents: (first_lab, second_lab),
            outputs: roles.outputs,
        },
    ));
    plan
}

/// One run per output lab holding nothing or the product with room for one
/// more, off cooldown, beside reagent labs stocked for one reaction:
/// (output, first reagent's lab, second's).
pub fn ready_runs(
    plan: &Plan,
    held: &dyn Fn(&str) -> Option<(Resource, i32)>,
    cooldown: &dyn Fn(&str) -> i32,
) -> Vec<(String, String, String)> {
    let Some((compound, roles)) = &plan.reaction else {
        return Vec::new();
    };
    let (one, other) = &roles.reagents;
    let (first, second) = engine::reagents_of(*compound);
    let product = Resource::Compound(*compound);
    let stocked = |lab: &str, reagent: &Resource| {
        held(lab).is_some_and(|(m, amount)| &m == reagent && amount >= engine::LAB_REACTION_AMOUNT)
    };

    if !(stocked(one, &first) && stocked(other, &second)) {
        return Vec::new();
    }
    roles
        .outputs
        .iter()
        .filter(|lab| {
            cooldown(lab) == 0
                && held(lab).is_none_or(|(m, amount)| {
                    m == product
                        && amount + engine::LAB_REACTION_AMOUNT <= engine::LAB_MINERAL_CAPACITY
                })
        })
        .map(|lab| (lab.clone(), one.clone(), other.clone()))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vocabulary::Ore;
    use crate::world::RoomName;

    fn stocked(pairs: &[(Resource, i32)]) -> impl Fn(&Resource) -> i32 + '_ {
        move |r| pairs.iter().find(|(p, _)| p == r).map_or(0, |(_, n)| *n)
    }

    #[test]
    fn a_short_compound_makes_its_short_reagent_first() {
        let targets = BTreeMap::from([(Compound::UtriumAcid, 3000)]);
        let ore = |o| (Resource::Ore(o), 10_000);
        let stock = [ore(Ore::Hydrogen), ore(Ore::Oxygen), ore(Ore::Utrium)];
        assert_eq!(
            next_reaction(&targets, &stocked(&stock)),
            Some(Compound::Hydroxide)
        );
        let mut more = stock.to_vec();
        more.push((Resource::Compound(Compound::Hydroxide), 3000));
        more.push((Resource::Compound(Compound::UtriumHydride), 3000));
        assert_eq!(
            next_reaction(&targets, &stocked(&more)),
            Some(Compound::UtriumAcid)
        );
        more.push((Resource::Compound(Compound::UtriumAcid), 3000));
        assert_eq!(next_reaction(&targets, &stocked(&more)), None);
    }

    #[test]
    fn the_ore_short_is_the_target_less_what_stands_through_its_recipe() {
        let targets = BTreeMap::from([(Compound::UtriumAcid, 3000)]);
        let stock = [
            (Resource::Compound(Compound::Hydroxide), 1000),
            (Resource::Ore(Ore::Hydrogen), 2000),
        ];
        let short = ore_short(&targets, &stocked(&stock));
        // OH: 2000 to make, of H and O; UH: 3000, of U and H; H: 5000 less 2000.
        assert_eq!(short.get(&Resource::Ore(Ore::Hydrogen)), Some(&3000));
        assert_eq!(short.get(&Resource::Ore(Ore::Oxygen)), Some(&2000));
        assert_eq!(short.get(&Resource::Ore(Ore::Utrium)), Some(&3000));
    }

    #[test]
    fn the_reagent_pair_is_the_one_most_labs_reach() {
        let at = |x, y| Pos::new(RoomName::named("W1N1"), x, y);
        let labs: Vec<(String, Pos)> = [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (10, 10)]
            .iter()
            .enumerate()
            .map(|(i, (x, y))| (format!("l{i}"), at(*x + 10, *y + 10)))
            .collect();
        let roles = roles_of(&labs).unwrap();
        assert_eq!(roles.reagents, ("l0".to_string(), "l3".to_string()));
        assert_eq!(roles.outputs, vec!["l1", "l2"]);
    }
}
