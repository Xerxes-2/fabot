//! The owner's terminal bands, held by `balance` over generated colonies.
use std::collections::BTreeMap;

use fabot_base::{
    engine::{self, LAB_REACTION_AMOUNT},
    tiles,
    vocabulary::ResourceType::{self, *},
    world::RoomName,
};
use fabot_economy::balance::{
    BATCH, BOOST, Holding, LEAST_SEND, Level, REAGENT, Shipment, balance, want,
};
use proptest::{prelude::*, sample::subsequence};

const CASES: u32 = 256;
const KINDS: [ResourceType; 5] =
    [Oxygen, Hydrogen, UtriumHydride, CatalyzedGhodiumAlkalide, CatalyzedUtriumAcid];
const TERMINAL: i32 = 300_000;

#[derive(Clone, Debug)]
struct Colony {
    room: RoomName,
    levels: Vec<(ResourceType, Level)>,
    energy: i32,
    free: i32,
}

impl Colony {
    fn holding(&self) -> Holding {
        Holding {
            room: self.room,
            levels: self.levels.clone(),
            energy: self.energy,
            free: self.free,
        }
    }

    fn level(&self, r: ResourceType) -> Level {
        self.holding().level(r)
    }
}

/// The owner's band, stated apart from `Level::spare` so it is an independent oracle.
fn keep(l: &Level) -> i32 {
    if l.want > 0 || l.boost { l.want + BATCH } else { 0 }
}

fn rooms(n: usize) -> impl Strategy<Value = Vec<RoomName>> {
    let grid: Vec<RoomName> = (1..9)
        .flat_map(|x| (1..9).map(move |y| format!("W{x}N{y}")))
        .filter_map(|name| name.parse().ok())
        .collect();
    subsequence(grid, n).prop_shuffle()
}

fn short(l: &Level) -> bool {
    l.want - l.held >= LEAST_SEND
}

fn spares(l: &Level) -> bool {
    l.held - keep(l) >= LAB_REACTION_AMOUNT && l.shippable >= LAB_REACTION_AMOUNT
}

fn near(x: i32) -> impl Strategy<Value = i32> {
    (x - 1..=x + 1).prop_map(|n| n.max(0))
}

/// Held, and what of it ships: a running cluster's labs and partner, or a
/// full terminal, keep the rest.
fn stock(want: i32) -> impl Strategy<Value = (i32, i32)> {
    let band = want + BATCH;
    let held = prop_oneof![
        0..=30_000i32,
        near(want),
        near(want - LEAST_SEND),
        near(band),
        near(band + LEAST_SEND),
        near(band + LAB_REACTION_AMOUNT),
    ];
    held.prop_flat_map(|held| {
        let least = near(LEAST_SEND).prop_map(move |n| n.min(held));
        let reaction = near(LAB_REACTION_AMOUNT).prop_map(move |n| n.min(held));
        (Just(held), prop_oneof![Just(held), 0..=held, least, reaction])
    })
}

/// Levels read as `Stocks::level` reads them: a want per the cluster's
/// reagents and the fight, every target boost marked a boost.
fn colony(room: RoomName, boosts: Vec<ResourceType>) -> impl Strategy<Value = Colony> {
    (prop::collection::vec(any::<bool>(), KINDS.len()), any::<bool>())
        .prop_flat_map(move |(spends, fights)| {
            let reagents: Vec<ResourceType> =
                (KINDS.iter().zip(&spends)).filter_map(|(r, s)| s.then_some(*r)).collect();
            let wants: Vec<i32> =
                (KINDS.iter()).map(|r| want(*r, &reagents, &boosts, fights)).collect();
            let boosted: Vec<bool> = KINDS.iter().map(|r| boosts.contains(r)).collect();
            (
                Just((wants.clone(), boosted)),
                wants.into_iter().map(stock).collect::<Vec<_>>(),
                prop_oneof![Just(0), 0..3_000i32, 0..=100_000i32],
                prop_oneof![Just(0), near(LEAST_SEND), 0..12_000i32, 0..=TERMINAL],
            )
        })
        .prop_map(move |((wants, boosted), stocks, energy, free)| {
            let levels = (KINDS.iter().zip(wants).zip(boosted).zip(stocks))
                .map(|(((r, want), boost), (held, shippable))| {
                    let level = Level { want, held, shippable, boost };
                    (*r, level)
                })
                .collect();
            Colony { room, levels, energy, free }
        })
}

fn colonies() -> impl Strategy<Value = Vec<Colony>> {
    (2..=5usize, prop::collection::vec(any::<bool>(), KINDS.len())).prop_flat_map(|(n, boosted)| {
        let boosts: Vec<ResourceType> =
            (KINDS.iter().zip(&boosted)).filter_map(|(r, b)| b.then_some(*r)).collect();
        rooms(n).prop_flat_map(move |rooms| {
            (rooms.into_iter()).map(|room| colony(room, boosts.clone())).collect::<Vec<_>>()
        })
    })
}

fn shipped(colonies: &[Colony]) -> Vec<Shipment> {
    let mut holdings: Vec<Holding> = colonies.iter().map(Colony::holding).collect();
    balance(&mut holdings)
}

/// Each band rule, checked send by send over a ledger of what each terminal
/// holds and has room for once the sends before it land.
fn hold_bands(colonies: &[Colony], ships: &[Shipment]) -> Result<(), TestCaseError> {
    let mut ledger: BTreeMap<RoomName, Colony> =
        colonies.iter().map(|c| (c.room, c.clone())).collect();
    let mut sent = Vec::new();
    for s in ships {
        prop_assert!(s.from != s.to, "a terminal sends to itself: {s:?}");
        prop_assert!(!sent.contains(&s.from), "two sends a tick from {}", s.from);
        sent.push(s.from);
        let (Some(giver), Some(taker)) = (ledger.get(&s.from), ledger.get(&s.to)) else {
            return Err(TestCaseError::fail(format!("a send off our terminals: {s:?}")));
        };
        let total: i32 = s.load.iter().map(|(_, n)| n).sum();
        prop_assert!(total <= taker.free, "{s:?} past the taker's room {}", taker.free);
        let apart = tiles::rooms_apart(s.from, s.to);
        let fee: i32 = s.load.iter().map(|(_, n)| engine::send_cost(*n, apart)).sum();
        prop_assert!(fee <= giver.energy, "{s:?} costs {fee} of {}", giver.energy);
        for (r, n) in &s.load {
            let (g, t) = (giver.level(*r), taker.level(*r));
            let spare = (g.held - keep(&g)).min(g.shippable);
            prop_assert!(*n >= LEAST_SEND.min(spare), "{s:?} below the least send");
            prop_assert!(*n >= LAB_REACTION_AMOUNT, "{s:?} under a reaction's worth");
            prop_assert!(*n <= g.shippable, "{s:?}: {r:?} past sendable {g:?}");
            prop_assert!(g.held - n >= keep(&g), "{s:?}: {r:?} giver below keep {g:?}");
            prop_assert!(t.held < t.want, "{s:?}: {r:?} to a taker not short {t:?}");
            prop_assert!(t.held + n <= t.want + BATCH, "{s:?}: {r:?} lifts {t:?}");
        }
        for (room, sign) in [(s.from, -1), (s.to, 1)] {
            let Some(c) = ledger.get_mut(&room) else {
                continue;
            };
            if sign > 0 {
                c.free -= total;
            }
            for (r, n) in &s.load {
                if let Some((_, l)) = c.levels.iter_mut().find(|(k, _)| k == r) {
                    l.held += sign * n;
                    l.shippable += sign * n;
                }
            }
        }
    }
    Ok(())
}

proptest! {
    #![proptest_config(fabot_base::testing::config(CASES))]

    #[test]
    fn want_is_a_reagent_batch_and_a_boost_only_while_fighting(
        r in prop::sample::select(&KINDS[..]),
        reagent: bool,
        boost: bool,
        fights: bool,
    ) {
        let reagents: &[ResourceType] = if reagent { &[r] } else { &[] };
        let boosts: &[ResourceType] = if boost { &[r] } else { &[] };
        let unlisted = want(r, &[], &[], fights);
        let quiet = want(r, reagents, boosts, false);
        let fighting = want(r, reagents, boosts, true);
        prop_assert_eq!(unlisted, 0);
        prop_assert_eq!(quiet, if reagent { REAGENT } else { 0 });
        prop_assert_eq!(fighting - quiet, if boost { BOOST } else { 0 });
    }

    #[test]
    fn spare_is_all_of_what_is_wanted_none_of_and_only_past_the_band_else(
        (want, (held, shippable)) in
            prop_oneof![Just(0), Just(BOOST), Just(REAGENT), Just(REAGENT + BOOST)]
                .prop_flat_map(|want| (Just(want), stock(want))),
        boost: bool,
    ) {
        let l = Level { want, held, shippable, boost };
        prop_assert_eq!(l.spare(), (held - keep(&l)).min(shippable));
    }

    #[test]
    fn every_send_keeps_the_bands(colonies in colonies()) {
        let ships = shipped(&colonies);
        hold_bands(&colonies, &ships)?;
    }

    #[test]
    fn a_short_taker_is_served_whenever_a_paid_giver_can_spare(colonies in colonies()) {
        let colonies: Vec<Colony> = (colonies.into_iter())
            .map(|c| Colony { energy: 1_000_000, ..c })
            .collect();
        let serves = |g: &Colony| {
            (colonies.iter()).any(|t| {
                g.room != t.room
                    && t.free >= LEAST_SEND
                    && (t.levels.iter()).any(|(r, l)| short(l) && spares(&g.level(*r)))
            })
        };
        let ships = shipped(&colonies);
        prop_assert!(!colonies.iter().any(serves) || !ships.is_empty());
        // The first giver reads stocks no send has moved yet.
        let first = &colonies[0];
        prop_assert!(!serves(first) || ships.iter().any(|s| s.from == first.room));
    }

    #[test]
    fn a_lone_taker_gets_every_kind_it_is_short_of_and_a_giver_spares(
        colonies in colonies(),
    ) {
        let [giver, taker] = [&colonies[0], &colonies[1]].map(|c| Colony {
            energy: 1_000_000,
            free: TERMINAL,
            ..c.clone()
        });
        let ships = shipped(&[giver.clone(), taker.clone()]);
        let to_taker = ships.iter().find(|s| s.from == giver.room && s.to == taker.room);
        for (r, l) in &taker.levels {
            if short(l) && spares(&giver.level(*r)) {
                prop_assert!(
                    to_taker.is_some_and(|s| s.load.iter().any(|(k, _)| k == r)),
                    "{r:?} short {l:?}, giver {:?}, sent {ships:?}",
                    giver.level(*r)
                );
            }
        }
        if let Some(sent) = to_taker {
            let apart = tiles::rooms_apart(giver.room, taker.room);
            let fee = sent.load.iter().map(|(_, n)| engine::send_cost(*n, apart)).sum();
            let paid = Colony { energy: fee, ..giver.clone() };
            let again = shipped(&[paid, taker.clone()]);
            prop_assert_eq!(again.iter().find(|s| s.from == giver.room), Some(sent));
        }
    }
}
