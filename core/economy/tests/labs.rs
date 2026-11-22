//! The lab planner held over generated stocks, targets, and clusters: what
//! `makeable` promises the ore carries, `wanted` schedules and plays out to,
//! and `assign` ranks as ARCHITECTURE.md's labs paragraph says.
use std::cmp::Reverse;

use fabot_base::engine::LAB_REACTION_AMOUNT as BATCH;
use fabot_base::vocabulary::{ORES, RESOURCES_ALL, ResourceType};
use fabot_economy::labs::{Bench, Reaction, assign, makeable, wanted};
use proptest::prelude::*;
use proptest::sample::{select, subsequence};
use proptest::test_runner::FileFailurePersistence;

const CASES: u32 = 256;

type Stock = Vec<(ResourceType, i32)>;

fn compounds() -> Vec<ResourceType> {
    (RESOURCES_ALL.iter().copied())
        .filter(|r| r.reaction_components().is_some())
        .collect()
}

fn stock_of(stock: &[(ResourceType, i32)], r: ResourceType) -> i32 {
    stock.iter().find(|(s, _)| *s == r).map_or(0, |(_, n)| *n)
}

fn tree(c: ResourceType) -> Vec<ResourceType> {
    let below = c
        .reaction_components()
        .map_or_else(Vec::new, |rs| rs.into_iter().flat_map(tree).collect());
    [vec![c], below].concat()
}

/// Ore each leaf asks to bring `c` to `total`, each compound's stock standing in
/// for its subtree; sound since no recipe holds a compound twice (checked below).
fn ore_asked(c: ResourceType, total: i32, stock: &Stock, asked: &mut [i32; ORES.len()]) {
    match c.reaction_components() {
        Some(rs) => {
            let make = (total - stock_of(stock, c)).max(0);
            rs.into_iter()
                .for_each(|r| ore_asked(r, make, stock, asked));
        }
        None => (asked.iter_mut().zip(ORES))
            .filter(|(_, o)| *o == c)
            .for_each(|(n, _)| *n += total),
    }
}

/// The most of `c`, up to `target` in whole reactions past its stock, its ore carries.
fn ore_cap(c: ResourceType, target: i32, stock: &Stock) -> i32 {
    let have = stock_of(stock, c);
    let fits = |total: i32| {
        let mut asked = [0; ORES.len()];
        ore_asked(c, total, stock, &mut asked);
        (ORES.iter().zip(asked)).all(|(o, n)| n <= stock_of(stock, *o))
    };
    let mut total = have;
    while total + BATCH <= target && fits(total + BATCH) {
        total += BATCH;
    }
    total
}

fn react(stock: &mut Stock, (product, reagents): (ResourceType, [ResourceType; 2])) {
    for (r, n) in reagents
        .map(|r| (r, -BATCH))
        .into_iter()
        .chain([(product, BATCH)])
    {
        match stock.iter_mut().find(|(s, _)| *s == r) {
            Some(at) => at.1 += n,
            None => stock.push((r, n)),
        }
    }
}

fn around(x: i32) -> impl Strategy<Value = i32> {
    (x - 1..=x + 1).prop_map(|n| n.max(0))
}

type Case = (Vec<(ResourceType, i32)>, Stock);

/// Targets, then a stock around what they ask: none, exactly, one short, one
/// over, a reaction's worth, plenty; pinned, a T3 from base ores only, one
/// past the ore it counts twice (O in both GO and OH), and a T2 whose reagents
/// fall short, both standing a reaction's worth or one a unit under it.
fn targets_with_stock() -> impl Strategy<Value = Case> {
    use ResourceType::{CatalyzedGhodiumAlkalide as Xgho2, Hydroxide, UtriumAcid, UtriumHydride};
    let ores = |n: i32| ORES.map(|o| (o, n)).to_vec();
    let short_t2 = |uh: i32| [ores(10_000), vec![(UtriumHydride, uh), (Hydroxide, BATCH)]].concat();
    prop_oneof![
        8 => generated(),
        1 => Just((vec![(Xgho2, 3000)], ores(10_000))),
        1 => Just((vec![(Xgho2, 7500)], ores(3000))),
        1 => Just((vec![(UtriumAcid, 3000)], short_t2(BATCH))),
        1 => Just((vec![(UtriumAcid, 3000)], short_t2(BATCH - 1))),
    ]
}

/// Stock and targets cut to whole reactions: `ore_cap` (and `makeable`) pool a
/// compound's odd units with its ore's, which no reaction can join.
fn in_whole_reactions() -> impl Strategy<Value = Case> {
    let whole = |(r, n): (ResourceType, i32)| (r, n / BATCH * BATCH);
    targets_with_stock().prop_map(move |(targets, stock)| {
        (
            targets.into_iter().map(whole).collect(),
            stock.into_iter().map(whole).collect(),
        )
    })
}

fn generated() -> impl Strategy<Value = Case> {
    let target = (select(compounds()), prop_oneof![Just(0), 1..=1_500i32]);
    prop::collection::vec(target, 1..=3).prop_flat_map(|targets| {
        let ores = ORES.map(|o| {
            let need: i32 = (targets.iter())
                .map(|(t, n)| n * tree(*t).iter().filter(|r| **r == o).count() as i32)
                .sum();
            prop_oneof![
                Just(0),
                around(need),
                around(need - BATCH),
                around(BATCH),
                0..=3_000i32,
                Just(20_000)
            ]
        });
        let made = compounds().into_iter().map(|c| {
            let need = (targets.iter()).find(|(t, _)| tree(*t).contains(&c));
            let need = need.map_or(BATCH, |(_, n)| *n);
            prop_oneof![
                6 => Just(0),
                1 => around(need),
                1 => around(BATCH),
                1 => 0..=2_000i32,
            ]
        });
        let stock = (ores.to_vec(), made.collect::<Vec<_>>()).prop_map(|(ores, made)| {
            let all = ORES
                .into_iter()
                .zip(ores)
                .chain(compounds().into_iter().zip(made));
            all.filter(|(_, n)| *n > 0).collect()
        });
        (Just(targets), stock)
    })
}

/// Holding nothing but a pair's reagents, how many of them: the doc's "holds".
fn holds(bench: &Bench, reagents: [ResourceType; 2]) -> usize {
    let clean = bench.reagents_held.iter().all(|m| reagents.contains(m));
    let held = reagents
        .iter()
        .filter(|r| bench.reagents_held.contains(r))
        .count();
    if clean { held } else { 0 }
}

/// The first wanted reaction a bench holds both reagents of, unless it reacts
/// under half as fast as the fastest.
fn runs(bench: &Bench, wanted: &[Reaction], fastest: usize) -> Option<ResourceType> {
    let fast = 2 * bench.outputs >= fastest;
    let first = wanted.iter().find(|(_, rs, _)| holds(bench, *rs) == 2);
    first.filter(|_| fast).map(|(p, _, _)| *p)
}

#[derive(Clone, Debug)]
struct Labs {
    wanted: Vec<Reaction>,
    benches: Vec<Option<Bench>>,
    /// Per bench, per reagent: the rooms to it, `None` out of reach.
    reach: Vec<Vec<(ResourceType, Option<i32>)>>,
}

impl Labs {
    fn bring(&self, i: usize, r: ResourceType) -> Option<i32> {
        self.reach[i]
            .iter()
            .find(|(s, _)| *s == r)
            .and_then(|(_, d)| *d)
    }

    fn reached(&self, i: usize, (_, [a, b], _): &Reaction) -> Option<i32> {
        Some(self.bring(i, *a)? + self.bring(i, *b)?)
    }

    /// ARCHITECTURE.md's labs rank (lines 397-402), best first: running, most
    /// outputs, reagents held, nearest, first wanted.
    fn claim(&self, i: usize, at: usize) -> Option<impl Ord> {
        let bench = self.benches[i].as_ref()?;
        let r = &self.wanted[at];
        let fastest = self
            .benches
            .iter()
            .flatten()
            .map(|b| b.outputs)
            .max()
            .unwrap_or(0);
        let running = runs(bench, &self.wanted, fastest) == Some(r.0);
        let held = holds(bench, r.1);
        Some((
            Reverse(running),
            Reverse(bench.outputs),
            Reverse(held),
            self.reached(i, r)?,
            at,
        ))
    }

    fn assigned(&self) -> Vec<Option<Reaction>> {
        let benches: Vec<Option<&Bench>> = self.benches.iter().map(Option::as_ref).collect();
        assign(&self.wanted, &benches, &|i, r| self.bring(i, r))
    }
}

/// Generated, or pinned: two benches reaching UH, one holding its pair with
/// three outputs then one, beside an empty one with four.
fn labs() -> impl Strategy<Value = Labs> {
    use ResourceType::{Hydrogen, Utrium, UtriumHydride};
    let pinned = |outputs: usize| {
        let bench = |reagents_held: Vec<ResourceType>, outputs| {
            Some(Bench {
                reagents_held,
                outputs,
            })
        };
        let near = vec![(Utrium, Some(0)), (Hydrogen, Some(0))];
        Labs {
            wanted: vec![(UtriumHydride, [Utrium, Hydrogen], 1)],
            benches: vec![bench(vec![Utrium, Hydrogen], outputs), bench(Vec::new(), 4)],
            reach: vec![near.clone(), near],
        }
    };
    prop_oneof![8 => generated_labs(), 1 => Just(pinned(3)), 1 => Just(pinned(1))]
}

fn generated_labs() -> impl Strategy<Value = Labs> {
    let reactions: Vec<Reaction> = (compounds().into_iter())
        .filter_map(|p| Some((p, p.reaction_components()?, 1)))
        .collect();
    let reaction = select(reactions);
    let wanted = prop::collection::vec(reaction, 0..=6).prop_map(|mut w| {
        let mut seen = Vec::new();
        w.retain(|(p, _, _)| {
            !seen.contains(p) && {
                seen.push(*p);
                true
            }
        });
        w
    });
    (wanted, prop_oneof![0..=1usize, 2..=4usize]).prop_flat_map(|(wanted, n)| {
        let reagents: Vec<ResourceType> = wanted.iter().flat_map(|(_, rs, _)| *rs).collect();
        let pool: Vec<ResourceType> = [reagents.clone(), ORES.to_vec()].concat();
        // Benches hold a wanted pair whole or one reagent of it, so some run a
        // reaction and some tie on all but reagents held.
        let pairs: Vec<Vec<ResourceType>> = reagents.chunks(2).map(<[_]>::to_vec).collect();
        let one = reagents.len().min(1);
        let held = prop_oneof![
            subsequence(pool, 0..=2),
            subsequence(pairs.clone(), 0..=pairs.len().min(1)).prop_map(|p| p.concat()),
            subsequence(reagents.clone(), 0..=one),
        ];
        let outputs = prop_oneof![1..=8usize, Just(4)];
        let bench = (held, outputs).prop_map(|(reagents_held, outputs)| Bench {
            reagents_held,
            outputs,
        });
        let benches = prop::collection::vec(prop::option::weighted(0.8, bench), n);
        let rooms =
            prop_oneof![1 => Just(None), 2 => Just(Some(0)), 2 => (1..=3i32).prop_map(Some)];
        let row = (reagents.iter().copied())
            .map(|r| (Just(r), rooms.clone()))
            .collect::<Vec<_>>();
        let reach = prop::collection::vec(row, n);
        (Just(wanted), benches, reach).prop_map(|(wanted, benches, reach)| Labs {
            wanted,
            benches,
            reach,
        })
    })
}

#[test]
fn no_recipe_holds_a_compound_twice() {
    for c in compounds() {
        let made: Vec<ResourceType> = tree(c).into_iter().filter(|r| !ORES.contains(r)).collect();
        for (i, r) in made.iter().enumerate() {
            assert!(!made[..i].contains(r), "{c:?} holds {r:?} twice");
        }
    }
}

proptest! {
    // Beside this file: the default reads `lib.rs` as the crate root and seeds `core/`.
    #![proptest_config(ProptestConfig {
        failure_persistence: Some(Box::new(FileFailurePersistence::WithSource("proptest-regressions"))),
        ..ProptestConfig::with_cases(CASES)
    })]

    /// Whole reactions only: past them the cap over-promises (ARCHITECTURE.md labs `❓`).
    #[test]
    fn the_first_target_is_cut_to_whole_reactions_its_ore_carries((targets, stock) in in_whole_reactions()) {
        let capped = makeable(&targets, &|r| stock_of(&stock, r));
        let (first, target) = targets[0];
        prop_assert_eq!(capped[0], (first, ore_cap(first, target, &stock)));
        for ((c, target), (k, cap)) in targets.iter().zip(&capped) {
            let have = stock_of(&stock, *c);
            prop_assert_eq!(c, k);
            prop_assert!(have <= *cap && *cap <= have.max(*target), "{c:?} {cap} of {target}");
            prop_assert_eq!((cap - have) % BATCH, 0);
        }
    }

    #[test]
    fn every_wanted_reaction_is_the_engine_s_over_reagents_that_stand((targets, stock) in targets_with_stock()) {
        let wanted = wanted(&targets, &|r| stock_of(&stock, r));
        let within: Vec<ResourceType> = targets.iter().flat_map(|(t, _)| tree(*t)).collect();
        for (i, (product, reagents, want)) in wanted.iter().enumerate() {
            prop_assert_eq!(Some(*reagents), product.reaction_components());
            prop_assert!(reagents.iter().all(|r| stock_of(&stock, *r) >= BATCH), "{product:?}");
            prop_assert!(within.contains(product) && *want > 0, "{product:?} {want}");
            prop_assert!(wanted[..i].iter().all(|w| w.0 != *product), "{product:?} twice");
        }
        if let [(target, _)] = targets[..] {
            for (i, (product, _, _)) in wanted.iter().enumerate() {
                let under = tree(*product);
                prop_assert!(
                    wanted[i + 1..].iter().all(|w| !under[1..].contains(&w.0)),
                    "{product:?} before its reagent toward {target:?}: {wanted:?}"
                );
            }
        }
    }

    #[test]
    fn played_out_the_first_target_reaches_all_its_ore_carries((targets, mut stock) in in_whole_reactions()) {
        let (first, target) = targets[0];
        let promised = ore_cap(first, target, &stock).min(target);
        while let Some(&(product, reagents, _)) = wanted(&targets, &|r| stock_of(&stock, r)).first() {
            react(&mut stock, (product, reagents));
        }
        prop_assert!(stock_of(&stock, first) >= promised, "{first:?} {} of {promised}", stock_of(&stock, first));
    }

    #[test]
    fn each_reaction_goes_to_one_reaching_cluster_none_ranks_above(labs in labs()) {
        let taken = labs.assigned();
        prop_assert_eq!(taken.len(), labs.benches.len());
        let at = |r: &Reaction| labs.wanted.iter().position(|w| w.0 == r.0);
        for (i, r) in taken.iter().enumerate() {
            let Some(r) = r else {
                // Idle: every reaction it reaches went elsewhere.
                for w in &labs.wanted {
                    let free = taken.iter().flatten().all(|t| t.0 != w.0);
                    prop_assert!(labs.benches[i].is_none() || labs.reached(i, w).is_none() || !free,
                        "{i} idle beside {w:?}");
                }
                continue;
            };
            let Some(rank) = at(r) else {
                return Err(TestCaseError::fail(format!("{r:?} unwanted")));
            };
            prop_assert!(labs.benches[i].is_some() && labs.reached(i, r).is_some(), "{i} {r:?}");
            prop_assert!(taken[..i].iter().flatten().all(|t| t.0 != r.0), "{r:?} twice");
            let mine = labs.claim(i, rank);
            for j in (0..taken.len()).filter(|j| *j != i) {
                let Some(theirs) = labs.claim(j, rank) else { continue };
                if Some(&theirs) < mine.as_ref() {
                    let kept = taken[j].as_ref().and_then(|t| labs.claim(j, at(t)?));
                    prop_assert!(kept.is_some_and(|k| k <= theirs), "{j} outranks {i} on {r:?}");
                }
            }
        }
    }
}
