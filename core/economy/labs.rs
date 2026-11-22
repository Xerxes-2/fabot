/// The lab cluster's reaction plan, pure over stocks and labs: which compound
/// it makes next, which lab plays which part, what each lab is filled with or
/// drained of. Derived whole every tick from what the stores hold, so a lost
/// tick or a reset loses nothing.
use fabot_base::boost::{Roles, roles_of};
use fabot_base::engine;
use fabot_base::vocabulary::{ORES, ResourceType};
use fabot_base::world::{Id, Pos};

/// Toward the first target its stock stands short of. A reagent compound short
/// of the shortfall is made first; once none can be, the compound itself from
/// what stands, so a reagent run out leaves the cluster working on with the other.
pub fn next_reaction(
    targets: &[(ResourceType, i32)],
    stock: &dyn Fn(ResourceType) -> i32,
) -> Option<(ResourceType, [ResourceType; 2])> {
    fn toward(
        compound: ResourceType,
        want: i32,
        stock: &dyn Fn(ResourceType) -> i32,
    ) -> Option<(ResourceType, [ResourceType; 2])> {
        let [first, second] = compound.reaction_components()?;
        let deeper = [first, second].into_iter().find_map(|reagent| {
            (stock(reagent) < want)
                .then(|| toward(reagent, want - stock(reagent), stock))
                .flatten()
        });
        deeper.or_else(|| {
            (stock(first) >= engine::LAB_REACTION_AMOUNT
                && stock(second) >= engine::LAB_REACTION_AMOUNT)
                .then_some((compound, [first, second]))
        })
    }

    targets.iter().find_map(|(compound, target)| {
        let short = target - stock(*compound);
        (short > 0)
            .then(|| toward(*compound, short, stock))
            .flatten()
    })
}

/// The ore the cluster lacks to bring every target up: each shortfall broken
/// down through its recipe, a compound or ore already in `stock` spent before
/// the ore under it is counted. What the other colonies ship, and no more.
pub fn ore_short(
    targets: &[(ResourceType, i32)],
    stock: &dyn Fn(ResourceType) -> i32,
) -> Vec<(ResourceType, i32)> {
    fn need(
        resource: ResourceType,
        amount: i32,
        left: &mut Vec<(ResourceType, i32)>,
        short: &mut [i32; ORES.len()],
        stock: &dyn Fn(ResourceType) -> i32,
    ) {
        let at = (left.iter().position(|(r, _)| *r == resource)).unwrap_or_else(|| {
            left.push((resource, stock(resource)));
            left.len() - 1
        });
        let spent = left[at].1.min(amount);
        left[at].1 -= spent;
        let rest = amount - spent;
        if rest <= 0 {
            return;
        }
        if let Some(reagents) = resource.reaction_components() {
            for reagent in reagents {
                need(reagent, rest, left, short, stock);
            }
        } else if let Some(ore) = ORES.iter().position(|o| *o == resource) {
            short[ore] += rest;
        }
    }

    let mut left = Vec::new();
    let mut short = [0; ORES.len()];
    for (compound, target) in targets {
        need(*compound, *target, &mut left, &mut short, stock);
    }
    ORES.into_iter()
        .zip(short)
        .filter(|(_, n)| *n > 0)
        .collect()
}

/// One tick of the cluster: the reaction and the labs that run it, the fills
/// (lab, mineral, the room it has for it) and the drains (lab, mineral).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Plan {
    pub reaction: Option<(ResourceType, [ResourceType; 2], Roles)>,
    pub fills: Vec<(Id, ResourceType, i32)>,
    pub drains: Vec<(Id, ResourceType)>,
}

/// Each reagent lab holds its reagent: anything else is drained, and one under
/// half full is filled to the brim, so a load arrives whole and a reaction
/// never starves. Each output holds the product: anything else is drained, and
/// the product once it stands half full. A reagent goes to the lab that already
/// holds it. With no reaction to run, the outputs are emptied.
pub fn plan_for(
    targets: &[(ResourceType, i32)],
    stock: &dyn Fn(ResourceType) -> i32,
    labs: &[(Id, Pos)],
    held: &dyn Fn(Id) -> Option<(ResourceType, i32)>,
) -> Plan {
    let half = engine::LAB_MINERAL_CAPACITY / 2;
    let Some(roles) = roles_of(labs) else {
        return Plan::default();
    };

    let Some((compound, [first, second])) = next_reaction(targets, stock) else {
        return Plan {
            drains: roles
                .outputs
                .iter()
                .filter_map(|lab| held(*lab).map(|(mineral, _)| (*lab, mineral)))
                .collect(),
            ..Plan::default()
        };
    };

    let (one, other) = roles.reagents;
    let holds = |lab: Id, mineral: ResourceType| held(lab).is_some_and(|(m, _)| m == mineral);
    let (first_lab, second_lab) = if holds(one, second) || holds(other, first) {
        (other, one)
    } else {
        (one, other)
    };

    let mut plan = Plan::default();
    for (lab, reagent) in [(first_lab, first), (second_lab, second)] {
        match held(lab) {
            Some((m, _)) if m != reagent => plan.drains.push((lab, m)),
            Some((_, amount)) if amount >= half => (),
            h => {
                let amount = h.map_or(0, |(_, amount)| amount);
                plan.fills
                    .push((lab, reagent, engine::LAB_MINERAL_CAPACITY - amount));
            }
        }
    }

    for &lab in &roles.outputs {
        match held(lab) {
            Some((m, _)) if m != compound => plan.drains.push((lab, m)),
            Some((_, amount)) if amount >= half => plan.drains.push((lab, compound)),
            _ => (),
        }
    }

    plan.reaction = Some((
        compound,
        [first, second],
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
    held: &dyn Fn(Id) -> Option<(ResourceType, i32)>,
    cooldown: &dyn Fn(Id) -> i32,
) -> Vec<(Id, Id, Id)> {
    let Some((compound, [first, second], roles)) = &plan.reaction else {
        return Vec::new();
    };
    let (one, other) = roles.reagents;
    let stocked = |lab: Id, reagent: ResourceType| {
        held(lab).is_some_and(|(m, amount)| m == reagent && amount >= engine::LAB_REACTION_AMOUNT)
    };

    if !(stocked(one, *first) && stocked(other, *second)) {
        return Vec::new();
    }
    roles
        .outputs
        .iter()
        .filter(|lab| {
            cooldown(**lab) == 0
                && held(**lab).is_none_or(|(m, amount)| {
                    m == *compound
                        && amount + engine::LAB_REACTION_AMOUNT <= engine::LAB_MINERAL_CAPACITY
                })
        })
        .map(|lab| (*lab, one, other))
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use ResourceType::*;
    use fabot_base::vocabulary::RESOURCES_ALL;
    use fabot_base::world::RoomName;

    fn stocked(pairs: &[(ResourceType, i32)]) -> impl Fn(ResourceType) -> i32 + '_ {
        move |r| pairs.iter().find(|(p, _)| *p == r).map_or(0, |(_, n)| *n)
    }

    #[test]
    fn a_short_compound_makes_its_short_reagent_first() {
        let targets = [(UtriumAcid, 3000)];
        let stock = [(Hydrogen, 10_000), (Oxygen, 10_000), (Utrium, 10_000)];
        assert_eq!(
            next_reaction(&targets, &stocked(&stock)).map(|r| r.0),
            Some(UtriumHydride)
        );
        let mut more = stock.to_vec();
        more.extend([(Hydroxide, 3000), (UtriumHydride, 3000)]);
        assert_eq!(
            next_reaction(&targets, &stocked(&more)).map(|r| r.0),
            Some(UtriumAcid)
        );
        more.push((UtriumAcid, 3000));
        assert_eq!(next_reaction(&targets, &stocked(&more)), None);
    }

    #[test]
    fn the_ore_short_is_the_target_less_what_stands_through_its_recipe() {
        let targets = [(UtriumAcid, 3000)];
        let stock = [(Hydroxide, 1000), (Hydrogen, 2000)];
        // OH: 2000 to make, of H and O; UH: 3000, of U and H; H: 5000 less 2000.
        assert_eq!(
            ore_short(&targets, &stocked(&stock)),
            vec![(Hydrogen, 3000), (Oxygen, 2000), (Utrium, 3000)]
        );
    }

    #[test]
    fn every_compound_breaks_down_to_base_ores() {
        fn bottoms_out(r: ResourceType) -> bool {
            ORES.contains(&r)
                || (r.reaction_components()).is_some_and(|rs| rs.into_iter().all(bottoms_out))
        }
        let all: Vec<ResourceType> = (RESOURCES_ALL.iter().copied())
            .filter(|r| r.reaction_components().is_some())
            .collect();
        assert!(!all.is_empty());
        for c in all {
            assert!(bottoms_out(c), "{c:?}");
        }
    }

    #[test]
    fn a_t3_from_base_ores_makes_each_intermediate_before_its_product() {
        let xgho2 = CatalyzedGhodiumAlkalide;
        let targets = [(xgho2, 3000)];
        let mut stock: Vec<(ResourceType, i32)> = ORES.map(|o| (o, 10_000)).to_vec();
        let mut made = Vec::new();
        loop {
            let next = next_reaction(&targets, &stocked(&stock));
            let Some((next, reagents)) = next else { break };
            assert!(!made.contains(&next), "{next:?} made twice");
            for reagent in reagents {
                if !ORES.contains(&reagent) {
                    assert!(made.contains(&reagent), "{next:?} before its {reagent:?}");
                }
            }
            made.push(next);
            stock.push((next, 3000));
        }
        assert_eq!(made.last(), Some(&xgho2));
    }

    #[test]
    fn the_reagent_pair_is_the_one_most_labs_reach() {
        let at = |x, y| Pos::new(RoomName::named("W1N1"), x, y);
        let labs: Vec<(Id, Pos)> = [(0, 0), (1, 0), (2, 0), (3, 0), (4, 0), (10, 10)]
            .iter()
            .enumerate()
            .map(|(i, (x, y))| (Id::from(format!("l{i}").as_str()), at(*x + 10, *y + 10)))
            .collect();
        let roles = roles_of(&labs).unwrap();
        assert_eq!(roles.reagents, ("l0".into(), "l3".into()));
        assert_eq!(roles.outputs, vec![Id::from("l1"), Id::from("l2")]);
    }
}
