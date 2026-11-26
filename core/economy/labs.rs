/// Each lab cluster's reaction plan, pure over stocks and labs: which compound
/// it makes next, which lab plays which part, what each lab is filled with or
/// drained of. Derived whole every tick from what the stores hold, so a lost
/// tick or a reset loses nothing.
use std::cmp::Reverse;

use fabot_base::{
    boost::{BOOSTS, Roles, ores_in},
    engine,
    vocabulary::{Boost, ResourceType},
    world::Id,
};

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
/// labs working on with the other. One target drained short by less than a
/// reaction could never be refilled.
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
        let asked = whole_reactions(want);
        for reagent in [first, second].into_iter().filter(|r| stock(*r) < asked) {
            toward(reagent, asked - stock(reagent), stock, out);
        }
        if stock(first) >= engine::LAB_REACTION_AMOUNT
            && stock(second) >= engine::LAB_REACTION_AMOUNT
            && out.iter().all(|(c, ..)| *c != compound)
        {
            out.push((compound, [first, second], want));
        }
    }

    let mut out = Vec::new();
    let capped = makeable(targets, stock);
    for (i, (compound, target)) in capped.iter().enumerate() {
        let free = |r| {
            let kept: i32 = (capped[..i].iter()).filter(|(c, _)| *c == r).map(|(_, n)| n).sum();
            (stock(r) - kept).max(0)
        };
        let short = target - free(*compound);
        if short > 0 {
            toward(*compound, short, &free, &mut out);
        }
    }
    out
}

/// Rooms to the nearest colony (0 for its own) holding a reaction's worth of a
/// reagent; `None` when none can reach the cluster.
pub type Bring<'a> = &'a dyn Fn(usize, ResourceType) -> Option<i32>;

#[derive(Clone, Debug, Default)]
pub struct Bench {
    pub reagents_held: Vec<ResourceType>,
    pub outputs: usize,
}

impl Bench {
    fn holds(&self, reagents: &[ResourceType; 2]) -> usize {
        let clean = self.reagents_held.iter().all(|m| reagents.contains(m));
        let held = reagents.iter().filter(|r| self.reagents_held.contains(r));
        usize::from(clean) * held.count()
    }
}

/// Output labs set the rate: under 1/SLOW_SHARE of the fastest keeps nothing (owner: half).
const SLOW_SHARE: usize = 2;

fn keeps<'b>(benches: &[Option<&'b Bench>]) -> impl Fn(&Bench) -> bool + use<'b> {
    let fastest = benches.iter().flatten().map(|b| b.outputs).max();
    move |b| SLOW_SHARE * b.outputs >= fastest.unwrap_or(0)
}

/// The wanted reaction each cluster's reagent labs run and it keeps (ARCHITECTURE.md, labs).
pub fn running<'w>(
    benches: &[Option<&Bench>],
    wanted: &'w [Reaction],
) -> Vec<Option<&'w Reaction>> {
    let keeps = keeps(benches);
    let runs = |b: &Bench| wanted.iter().find(|(_, reagents, _)| b.holds(reagents) == 2);
    (benches.iter()).map(|b| b.filter(|b| keeps(b)).and_then(runs)).collect()
}

/// No two clusters run one reaction, nor one whose reagents cannot reach it;
/// ranked as ARCHITECTURE.md's labs paragraph says.
pub fn assign(
    wanted: &[Reaction],
    benches: &[Option<&Bench>],
    bring: Bring,
) -> Vec<Option<Reaction>> {
    let running = running(benches, wanted);
    let keeps = keeps(benches);
    let mut offers = Vec::new();
    for (i, bench) in benches.iter().enumerate() {
        let Some(bench) = bench else { continue };
        for (rank, (product, reagents, _)) in wanted.iter().enumerate() {
            let runs = running[i].is_some_and(|r| r.0 == *product);
            if let [Some(a), Some(b)] = reagents.map(|r| bring(i, r))
                && (keeps(bench) || a + b == 0)
            {
                let key = (
                    Reverse(runs),
                    Reverse(bench.outputs),
                    Reverse(usize::from(a == 0) + usize::from(b == 0)),
                    a + b,
                    rank,
                );
                offers.push((key, i));
            }
        }
    }
    offers.sort_unstable();
    let mut taken: Vec<Option<Reaction>> = vec![None; benches.len()];
    for ((.., rank), i) in offers {
        let free = taken.iter().flatten().all(|t| t.0 != wanted[rank].0);
        if taken[i].is_none() && free {
            taken[i] = Some(wanted[rank]);
        }
    }
    taken
}

/// The other reagent of the first reaction toward a target that takes `product`.
pub fn sibling(targets: &[(ResourceType, i32)], product: ResourceType) -> Option<ResourceType> {
    fn within(compound: ResourceType, product: ResourceType) -> Option<ResourceType> {
        let [a, b] = compound.reaction_components()?;
        match product {
            p if p == a => Some(b),
            p if p == b => Some(a),
            _ => within(a, product).or_else(|| within(b, product)),
        }
    }
    targets.iter().find_map(|(t, _)| within(*t, product))
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
            let last = (*compound, have + n * engine::LAB_REACTION_AMOUNT);
            carries(&[&capped[..], &[last]].concat(), stock)
        };
        let (mut most, mut over) = (0, (target - have) / engine::LAB_REACTION_AMOUNT + 1);
        while over - most > 1 {
            let mid = (most + over) / 2;
            if made(mid) {
                most = mid;
            } else {
                over = mid;
            }
        }
        capped.push((*compound, have + most * engine::LAB_REACTION_AMOUNT));
    }
    capped
}

/// Whether the stock brings every target up, each shortfall broken down
/// through its recipe in whole reactions, a compound or ore in `stock` spent
/// before what it is made of.
fn carries(targets: &[(ResourceType, i32)], stock: &dyn Fn(ResourceType) -> i32) -> bool {
    fn need(
        resource: ResourceType,
        amount: i32,
        left: &mut Vec<(ResourceType, i32)>,
        stock: &dyn Fn(ResourceType) -> i32,
    ) -> bool {
        let at = (left.iter().position(|(r, _)| *r == resource)).unwrap_or_else(|| {
            left.push((resource, stock(resource)));
            left.len() - 1
        });
        let rest = amount - left[at].1.min(amount);
        left[at].1 -= amount - rest;
        let Some(reagents) = resource.reaction_components().filter(|_| rest > 0) else {
            return rest <= 0;
        };
        let made = whole_reactions(rest);
        left[at].1 += made - rest;
        reagents.into_iter().all(|r| need(r, made, left, stock))
    }

    let mut left = Vec::new();
    (targets.iter()).all(|(compound, target)| need(*compound, *target, &mut left, stock))
}

fn whole_reactions(n: i32) -> i32 {
    (n + engine::LAB_REACTION_AMOUNT - 1) / engine::LAB_REACTION_AMOUNT
        * engine::LAB_REACTION_AMOUNT
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
    roles: Option<&Roles>,
    held: &dyn Fn(Id) -> Option<(ResourceType, i32)>,
) -> Plan {
    let half = engine::LAB_MINERAL_CAPACITY / 2;
    let Some(roles) = roles else {
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
    let (first_lab, second_lab) =
        if holds(one, second) || holds(other, first) { (other, one) } else { (one, other) };

    let mut plan = Plan::default();
    for (lab, reagent) in [(first_lab, first), (second_lab, second)] {
        match held(lab) {
            Some((m, _)) if m != reagent => plan.drains.push((lab, m)),
            Some((_, amount)) if amount >= half => (),
            h => {
                let amount = h.map_or(0, |(_, amount)| amount);
                plan.fills.push((lab, reagent, engine::LAB_MINERAL_CAPACITY - amount));
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
        Roles { reagents: (first_lab, second_lab), outputs: roles.outputs.clone() },
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
    use ResourceType::*;
    use fabot_base::{
        boost::{ORES, roles_of},
        vocabulary::RESOURCES_ALL,
        world::{Pos, RoomName},
    };

    use super::*;

    fn stocked(pairs: &[(ResourceType, i32)]) -> impl Fn(ResourceType) -> i32 + '_ {
        move |r| pairs.iter().find(|(p, _)| *p == r).map_or(0, |(_, n)| *n)
    }

    fn placed(
        stock: &[(ResourceType, i32)],
        held: &[Option<Vec<ResourceType>>],
        i: usize,
        r: ResourceType,
    ) -> Option<i32> {
        let here = held[i].as_ref().is_some_and(|h| h.contains(&r));
        (stocked(stock)(r) >= engine::LAB_REACTION_AMOUNT).then_some(i32::from(!here))
    }

    fn benches(held: &[Option<Vec<ResourceType>>]) -> Vec<Option<Bench>> {
        let bench = |h: &Vec<ResourceType>| Bench { reagents_held: h.clone(), outputs: 1 };
        held.iter().map(|h| h.as_ref().map(bench)).collect()
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
        let now = assign(
            &wanted(&[(UtriumAcid, 3000)], &stocked(&stock)),
            &benches(&held).iter().map(Option::as_ref).collect::<Vec<_>>(),
            &|i, r| placed(&stock, &held, i, r),
        );
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
            let wanted = wanted(&targets, &stocked(&stock));
            let now = assign(
                &wanted,
                &benches(&held).iter().map(Option::as_ref).collect::<Vec<_>>(),
                &|i, r| placed(&stock, &held, i, r),
            );
            let running: Vec<Reaction> = now.iter().flatten().copied().collect();
            if running.is_empty() {
                assert!(kinds.len() > 3);
                return;
            }
            for (i, r) in running.iter().enumerate() {
                assert!(running[..i].iter().all(|o| o.0 != r.0), "{running:?}");
            }
            for (labs, reaction) in held.iter_mut().flatten().zip(&now) {
                let was = wanted.iter().find(|r| r.1.iter().all(|m| labs.contains(m)));
                assert!(was.is_none_or(|w| reaction.is_some_and(|r| r.0 == w.0)), "{was:?} left");
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
    fn a_product_s_sibling_is_the_other_reagent_of_the_step_after_it() {
        let targets = [(CatalyzedKeaniumAlkalide, 7500)];
        assert_eq!(sibling(&targets, KeaniumAlkalide), Some(Catalyst));
        assert_eq!(sibling(&targets, KeaniumOxide), Some(Hydroxide));
        assert_eq!(sibling(&targets, CatalyzedKeaniumAlkalide), None);
    }

    #[test]
    fn a_short_compound_makes_its_short_reagent_first() {
        let targets = [(UtriumAcid, 3000)];
        let stock = [(Hydrogen, 10_000), (Oxygen, 10_000), (Utrium, 10_000)];
        assert_eq!(next_reaction(&targets, &stocked(&stock)).map(|r| r.0), Some(UtriumHydride));
        let mut more = stock.to_vec();
        more.extend([(Hydroxide, 3000), (UtriumHydride, 3000)]);
        assert_eq!(next_reaction(&targets, &stocked(&more)).map(|r| r.0), Some(UtriumAcid));
        more.push((UtriumAcid, 3000));
        assert_eq!(next_reaction(&targets, &stocked(&more)), None);
    }

    #[test]
    fn a_target_carries_only_what_its_ore_makes_in_whole_reactions() {
        let carries =
            |stock: &[(ResourceType, i32)], n| carries(&[(UtriumAcid, n)], &stocked(stock));
        // OH: 2000 to make, of H and O; UH: 3000, of U and H.
        let stock = [(Hydroxide, 1000), (Hydrogen, 5000), (Oxygen, 2000), (Utrium, 3000)];
        assert!(carries(&stock, 3000));
        assert!(!carries(&stock[..3], 3000));
        // UH 6 and H 4 make one reaction's worth of UH, not two.
        let odd = [(UtriumHydride, 6), (Hydrogen, 4), (Utrium, 10), (Hydroxide, 10)];
        assert!(carries(&odd, 5) && !carries(&odd, 6));
    }

    #[test]
    fn every_compound_breaks_down_to_base_ores() {
        fn bottoms_out(r: ResourceType) -> bool {
            ORES.contains(&r)
                || (r.reaction_components()).is_some_and(|rs| rs.into_iter().all(bottoms_out))
        }
        let all: Vec<ResourceType> =
            (RESOURCES_ALL.iter().copied()).filter(|r| r.reaction_components().is_some()).collect();
        assert!(!all.is_empty());
        for c in all {
            assert!(bottoms_out(c), "{c:?}");
        }
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
            assert!(first.is_some_and(|c| within(next.0, c)), "{next:?} for {first:?}");
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
