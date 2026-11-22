/// Each lab cluster's reaction plan, pure over stocks and labs: which compound
/// it makes next, which lab plays which part, what each lab is filled with or
/// drained of. Derived whole every tick from what the stores hold, so a lost
/// tick or a reset loses nothing.
use fabot_base::boost::{Roles, roles_of};
use fabot_base::engine;
use fabot_base::vocabulary::{BOOSTS, Boost, ORES, ResourceType, ores_in};
use fabot_base::world::{Id, Pos};

/// Hivemind's "high": past it the first-ranked boost leaves the ore to the next.
const BOOST_STOCK: i32 = 7_500;

/// An ore is held while a compound made of it is, or a target drops mid-tree.
pub fn targets(have: &[ResourceType]) -> Vec<(ResourceType, i32)> {
    let held = |ore: usize| have.iter().any(|r| ores_in(*r)[ore] > 0);
    (BOOSTS.iter())
        .filter(|(boost, _)| {
            use Boost::*;
            matches!(
                boost.boost(),
                Some(Attack(_) | RangedAttack(_) | Heal(_) | Tough(_) | Move(_))
            )
        })
        .filter(|(boost, _)| {
            (ores_in(*boost).iter().enumerate()).all(|(ore, n)| *n == 0 || held(ore))
        })
        .map(|(boost, _)| (*boost, BOOST_STOCK))
        .collect()
}

/// Its product, its reagents, and how much of the product its target asks.
pub type Reaction = (ResourceType, [ResourceType; 2], i32);

/// Every reaction toward a target its stock stands short of, depth-first: a
/// reagent compound short of the shortfall comes before the compound itself,
/// which is wanted once both reagents stand, so a reagent run out leaves the
/// labs working on with the other.
pub fn wanted(
    targets: &[(ResourceType, i32)],
    stock: &dyn Fn(ResourceType) -> i32,
) -> Vec<Reaction> {
    fn toward(
        compound: ResourceType,
        want: i32,
        stock: &dyn Fn(ResourceType) -> i32,
        out: &mut Vec<Reaction>,
    ) {
        let Some([first, second]) = compound.reaction_components() else {
            return;
        };
        for reagent in [first, second].into_iter().filter(|r| stock(*r) < want) {
            toward(reagent, want - stock(reagent), stock, out);
        }
        if stock(first) >= engine::LAB_REACTION_AMOUNT
            && stock(second) >= engine::LAB_REACTION_AMOUNT
            && out.iter().all(|(c, _, _)| *c != compound)
        {
            out.push((compound, [first, second], want));
        }
    }

    let mut out = Vec::new();
    for (compound, target) in makeable(targets, stock) {
        let short = target - stock(compound);
        if short > 0 {
            toward(compound, short, stock, &mut out);
        }
    }
    out
}

/// What a cluster's reagent labs hold of a reaction's reagents, with nothing
/// foreign in them.
#[derive(Clone, Copy)]
enum Holds {
    Both,
    One,
    Any,
}

impl Holds {
    fn of(self, held: &[ResourceType], (_, reagents, _): &Reaction) -> bool {
        let covered = reagents.iter().filter(|r| held.contains(r)).count();
        let clean = held.iter().all(|m| reagents.contains(m));
        match self {
            Holds::Both => clean && covered == 2,
            Holds::One => clean && covered >= 1,
            Holds::Any => true,
        }
    }
}

/// No two clusters run one reaction, nor one without reagent labs any, nor
/// one whose reagents cannot reach it (`reach(cluster, resource)`). One keeps
/// a wanted reaction its reagent labs (`held`) hold nothing foreign to, as it
/// costs no drain, both reagents held before one; the rest take the first left.
pub fn assign(
    targets: &[(ResourceType, i32)],
    stock: &dyn Fn(ResourceType) -> i32,
    held: &[Option<Vec<ResourceType>>],
    reach: &dyn Fn(usize, ResourceType) -> i32,
) -> Vec<Option<Reaction>> {
    let wanted = wanted(targets, stock);
    let supplied = |i: usize, (_, reagents, _): &Reaction| {
        (reagents.iter()).all(|r| reach(i, *r) >= engine::LAB_REACTION_AMOUNT)
    };
    let mut taken: Vec<Option<Reaction>> = vec![None; held.len()];
    for holds in [Holds::Both, Holds::One, Holds::Any] {
        for (i, labs) in held.iter().enumerate() {
            if let Some(labs) = labs
                && taken[i].is_none()
            {
                let free = |r: &Reaction| taken.iter().flatten().all(|t| t.0 != r.0);
                taken[i] = (wanted.iter())
                    .find(|r| holds.of(labs, r) && free(r) && supplied(i, r))
                    .copied();
            }
        }
    }
    taken
}

/// A reagent made past its target spends its sibling's ore; a part reaction
/// left over hands the target's intermediates to the next.
pub fn makeable(
    targets: &[(ResourceType, i32)],
    stock: &dyn Fn(ResourceType) -> i32,
) -> Vec<(ResourceType, i32)> {
    let mut capped: Vec<(ResourceType, i32)> = Vec::new();
    for (compound, target) in targets {
        let have = stock(*compound);
        let made = |n: i32| {
            let asked = [capped.as_slice(), &[(*compound, have + n)]].concat();
            ore_short(&asked, stock).is_empty()
        };
        let (mut short, mut over) = (0, target - have + 1);
        while over - short > 1 {
            let mid = (short + over) / 2;
            if made(mid) {
                short = mid;
            } else {
                over = mid;
            }
        }
        let reactions = short / engine::LAB_REACTION_AMOUNT;
        capped.push((*compound, have + reactions * engine::LAB_REACTION_AMOUNT));
    }
    capped
}

/// The ore the cluster lacks to bring every target up: each shortfall broken
/// down through its recipe, a compound or ore already in `stock` spent before
/// the ore under it is counted. What the other colonies ship, and no more.
fn ore_short(
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
    reaction: Option<Reaction>,
    labs: &[(Id, Pos)],
    held: &dyn Fn(Id) -> Option<(ResourceType, i32)>,
) -> Plan {
    let half = engine::LAB_MINERAL_CAPACITY / 2;
    let Some(roles) = roles_of(labs) else {
        return Plan::default();
    };

    let Some((compound, [first, second], _)) = reaction else {
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

    fn next_reaction(
        targets: &[(ResourceType, i32)],
        stock: &dyn Fn(ResourceType) -> i32,
    ) -> Option<(ResourceType, [ResourceType; 2])> {
        (wanted(targets, stock).first()).map(|(c, reagents, _)| (*c, *reagents))
    }

    #[test]
    fn a_cluster_holding_both_reagents_keeps_its_reaction_before_one_holding_one() {
        let stock = [(Hydrogen, 10_000), (Oxygen, 10_000), (Utrium, 10_000)];
        let held = [Some(vec![Oxygen]), Some(vec![Oxygen, Hydrogen]), None];
        let now = assign(&[(UtriumAcid, 3000)], &stocked(&stock), &held, &|_, r| {
            stocked(&stock)(r)
        });
        let made: Vec<_> = now.iter().map(|r| r.map(|r| r.0)).collect();
        assert_eq!(made, [Some(UtriumHydride), Some(Hydroxide), None]);
    }

    #[test]
    fn clusters_never_share_a_reaction_and_leave_one_only_once_unwanted() {
        let targets = targets(&ORES);
        let mut stock: Vec<(ResourceType, i32)> = ORES.map(|o| (o, 3000)).to_vec();
        let mut held: Vec<Option<Vec<ResourceType>>> = vec![Some(Vec::new()); 3];
        let mut kinds = Vec::new();
        for _ in 0..10_000 {
            let now = assign(&targets, &stocked(&stock), &held, &|_, r| {
                stocked(&stock)(r)
            });
            let running: Vec<Reaction> = now.iter().flatten().copied().collect();
            if running.is_empty() {
                assert!(kinds.len() > 3);
                return;
            }
            for (i, r) in running.iter().enumerate() {
                assert!(running[..i].iter().all(|o| o.0 != r.0), "{running:?}");
            }
            let wanted = wanted(&targets, &stocked(&stock));
            for (labs, reaction) in held.iter_mut().flatten().zip(&now) {
                let was = wanted.iter().find(|r| r.1.iter().all(|m| labs.contains(m)));
                assert!(
                    was.is_none_or(|w| reaction.is_some_and(|r| r.0 == w.0)),
                    "{was:?} left"
                );
                *labs = reaction.map_or_else(Vec::new, |r| r.1.to_vec());
            }
            for (next, reagents, _) in running {
                kinds.extend((!kinds.contains(&next)).then_some(next));
                react(&mut stock, (next, reagents));
            }
        }
        panic!("the reactions never end");
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

    fn react(
        stock: &mut Vec<(ResourceType, i32)>,
        (next, reagents): (ResourceType, [ResourceType; 2]),
    ) {
        for (r, n) in reagents.into_iter().map(|r| (r, -5)).chain([(next, 5)]) {
            match stock.iter_mut().find(|(s, _)| *s == r) {
                Some(at) => at.1 += n,
                None => stock.push((r, n)),
            }
        }
    }

    #[test]
    fn a_target_past_the_ore_makes_all_the_ore_can() {
        let xgho2 = CatalyzedGhodiumAlkalide;
        let mut stock: Vec<(ResourceType, i32)> = ORES.map(|o| (o, 3000)).to_vec();
        loop {
            let Some(next) = next_reaction(&[(xgho2, 7500)], &stocked(&stock)) else {
                break;
            };
            react(&mut stock, next);
        }
        // O is two to a unit: 3000 make 1500.
        assert_eq!(stocked(&stock)(xgho2), 1500);
    }

    #[test]
    fn targets_sharing_an_ore_take_it_in_turn() {
        fn within(r: ResourceType, of: ResourceType) -> bool {
            r == of || (of.reaction_components()).is_some_and(|rs| rs.iter().any(|c| within(r, *c)))
        }
        // XGHO2 is first and Z too short to make it; the rest leave leftovers.
        let ore = |o| (o, if o == Zynthium { 3 } else { 3003 });
        let mut stock: Vec<(ResourceType, i32)> = ORES.map(ore).to_vec();
        let targets = targets(&ORES);
        assert!(targets.len() > 1);
        for _ in 0..10_000 {
            let Some(next) = next_reaction(&targets, &stocked(&stock)) else {
                return;
            };
            let first = (makeable(&targets, &stocked(&stock)).into_iter())
                .find(|(c, n)| *n > stocked(&stock)(*c))
                .map(|(c, _)| c);
            assert!(
                first.is_some_and(|c| within(next.0, c)),
                "{next:?} for {first:?}"
            );
            react(&mut stock, next);
        }
        panic!("the reactions never end");
    }

    #[test]
    fn the_reagent_pair_is_the_one_most_labs_reach() {
        let at = |x, y| Pos::new(RoomName::W1N1, x, y);
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
