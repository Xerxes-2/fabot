//! Our own terminals' minerals, Hivemind's trade-route-manager over its cutoffs.
use fabot_base::engine;
use fabot_base::tiles;
use fabot_base::vocabulary::ResourceType;
use fabot_base::world::RoomName;

/// Hivemind's "low" cutoff for a reaction's reagents.
pub const REAGENT: i32 = 10_000;
/// Hivemind's "low" cutoff for a combat boost in a room under attack.
pub const BOOST: i32 = 2_500;
/// One load: the most a reagent taker asks a send, the band past want that
/// keeps a filled taker from giving, and the room aid leaves for a load.
pub const BATCH: i32 = 5_000;

/// Ours, so a send's cooldown buys a load: the engine enforces no TERMINAL_MIN_SEND.
pub const LEAST_SEND: i32 = 100;

pub fn sends(n: i32) -> bool {
    n >= LEAST_SEND
}

/// A giver spares a reagent from a reaction's worth: `assign` counts it reaching a cluster.
pub fn gives(spare: i32) -> bool {
    spare >= engine::LAB_REACTION_AMOUNT
}

/// A cluster's reagents, and the target boosts while the colony fights.
pub fn want(
    r: ResourceType,
    reagents: &[ResourceType],
    boosts: &[ResourceType],
    fights: bool,
) -> i32 {
    let boost = fights && boosts.contains(&r);
    REAGENT * i32::from(reagents.contains(&r)) + BOOST * i32::from(boost)
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Level {
    pub want: i32,
    pub held: i32,
    /// Not its labs', nor past its terminal's room: a load from storage ships.
    pub shippable: i32,
    /// A boost sent to a fight stays once the fight ends.
    pub boost: bool,
}

impl Level {
    /// Hivemind's "high": what a colony wants none of is spare whole.
    pub fn spare(&self) -> i32 {
        let kept = if self.want > 0 || self.boost {
            self.want + BATCH
        } else {
            0
        };
        (self.held - kept).min(self.shippable)
    }

    pub fn short(&self) -> i32 {
        self.want - self.held
    }

    /// Up to a batch, to the giving band, so a draining lab is topped up a
    /// batch at a time, not per hundred.
    fn load(&self) -> i32 {
        if self.short() <= 0 {
            0
        } else if self.want >= BATCH {
            BATCH.min(self.want + BATCH - self.held)
        } else {
            self.short()
        }
    }
}

#[derive(Clone)]
pub struct Holding {
    pub room: RoomName,
    pub levels: Vec<(ResourceType, Level)>,
    pub energy: i32,
    pub free: i32,
}

impl Holding {
    pub fn level(&self, r: ResourceType) -> Level {
        (self.levels.iter())
            .find_map(|(l, level)| (*l == r).then_some(*level))
            .unwrap_or_default()
    }

    fn book(&mut self, r: ResourceType, n: i32) {
        if let Some((_, l)) = self.levels.iter_mut().find(|(l, _)| *l == r) {
            l.held += n;
            l.shippable += n;
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Shipment {
    pub from: RoomName,
    pub to: RoomName,
    pub taker_free: i32,
    pub load: Vec<(ResourceType, i32)>,
}

/// What sending `loads` over `apart` rooms costs, a hundred over as the fee
/// rounds up per send; none for no load.
pub fn send_fee(loads: impl Iterator<Item = i32>, apart: i32) -> i32 {
    let fee: i32 = loads.map(|n| engine::send_cost(n, apart)).sum();
    if fee > 0 { fee + 100 } else { 0 }
}

/// v1's `oreShipFee`: each giver's sends' fee were its terminal's energy no bound.
pub fn fees(holdings: &[Holding]) -> Vec<(RoomName, i32)> {
    let mut unbound = holdings.to_vec();
    unbound.iter_mut().for_each(|h| h.energy = i32::MAX);
    (balance(&mut unbound).iter())
        .map(|s| {
            (
                s.from,
                send_fee(
                    s.load.iter().map(|(_, n)| *n),
                    tiles::rooms_apart(s.from, s.to),
                ),
            )
        })
        .collect()
}

/// A terminal sends to one colony a tick. Each batch is booked on both, so
/// givers never pile on one taker; never a send its energy cannot pay.
pub fn balance(holdings: &mut [Holding]) -> Vec<Shipment> {
    let mut out = Vec::new();
    for gi in 0..holdings.len() {
        let giver = &holdings[gi];
        let apart = |ti: usize| tiles::rooms_apart(giver.room, holdings[ti].room);
        // Short for its want, so a fight's boosts wait behind no reagent.
        let Some(ti) = (0..holdings.len())
            .filter(|ti| *ti != gi && sends(holdings[*ti].free))
            .flat_map(|ti| holdings[ti].levels.iter().map(move |(r, l)| (ti, *r, *l)))
            .filter(|(_, r, l)| sends(l.short()) && gives(giver.level(*r).spare()))
            .min_by_key(|(ti, _, l)| (-1000 * l.short() / l.want, apart(*ti), holdings[*ti].room))
            .map(|(ti, ..)| ti)
        else {
            continue;
        };
        let apart = apart(ti);
        let (mut load, mut fee) = (Vec::new(), 0);
        for (r, l) in holdings[ti].levels.clone() {
            let room = holdings[ti].free - load.iter().map(|(_, n)| n).sum::<i32>();
            let spare = holdings[gi].level(r).spare();
            let n = (l.load().min(spare)).min(room);
            let cost = engine::send_cost(n, apart);
            if !gives(n) || n < LEAST_SEND.min(spare) || fee + cost > holdings[gi].energy {
                continue;
            }
            fee += cost;
            load.push((r, n));
            holdings[gi].book(r, -n);
            holdings[ti].book(r, n);
        }
        if !load.is_empty() {
            let (from, to, taker_free) = (holdings[gi].room, holdings[ti].room, holdings[ti].free);
            holdings[ti].free -= load.iter().map(|(_, n)| n).sum::<i32>();
            out.push(Shipment {
                from,
                to,
                taker_free,
                load,
            });
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use ResourceType::*;

    fn holding(room: &str, levels: &[(ResourceType, i32, i32)]) -> Holding {
        Holding {
            room: room.parse().unwrap(),
            levels: (levels.iter())
                .map(|(r, want, held)| {
                    let boost = *r == CatalyzedGhodiumAlkalide;
                    let (want, held, shippable) = (*want, *held, *held);
                    let level = Level {
                        want,
                        held,
                        shippable,
                        boost,
                    };
                    (*r, level)
                })
                .collect(),
            energy: 100_000,
            free: 100_000,
        }
    }

    fn one(from: &str, to: &str, r: ResourceType, n: i32) -> Vec<Shipment> {
        let (from, to) = (from.parse().unwrap(), to.parse().unwrap());
        let (taker_free, load) = (100_000, vec![(r, n)]);
        vec![Shipment {
            from,
            to,
            taker_free,
            load,
        }]
    }

    #[test]
    fn a_fighting_colony_wants_each_target_boost_and_its_reagents() {
        let boosts = [CatalyzedGhodiumAlkalide];
        let want = |r, fights| want(r, &[Oxygen], &boosts, fights);
        assert_eq!(want(CatalyzedGhodiumAlkalide, true), BOOST);
        assert_eq!(want(CatalyzedGhodiumAlkalide, false), 0);
        assert_eq!(want(Oxygen, false), REAGENT);
    }

    #[test]
    fn a_boost_leaves_only_past_its_band_and_only_to_a_fighting_colony() {
        let ghodium = CatalyzedGhodiumAlkalide;
        for fights in [false, true] {
            let want = if fights { BOOST } else { 0 };
            let mut holdings = [
                holding("W1N1", &[(ghodium, 0, 7_000)]),
                holding("W3N1", &[(ghodium, want, 0)]),
                holding("W2N1", &[(ghodium, 0, 4_000)]),
            ];
            let made = if fights {
                one("W1N1", "W3N1", ghodium, 2_000)
            } else {
                vec![]
            };
            assert_eq!(balance(&mut holdings), made);
        }
    }

    #[test]
    fn a_reagent_taker_below_want_gets_a_batch_and_gives_none_back() {
        let mut holdings = [
            holding("W1N1", &[(Oxygen, 0, 20_000)]),
            holding("W2N1", &[(Oxygen, REAGENT, REAGENT - 100)]),
        ];
        assert_eq!(balance(&mut holdings), one("W1N1", "W2N1", Oxygen, BATCH));
        assert_eq!(balance(&mut holdings), vec![]);
    }

    #[test]
    fn a_taker_whose_terminal_is_full_never_holds_up_a_giver() {
        let mut holdings = [
            holding("W1N1", &[(Oxygen, 0, 20_000), (Hydrogen, 0, 20_000)]),
            holding("W2N1", &[(Oxygen, REAGENT, 0)]),
            holding("W3N1", &[(Hydrogen, REAGENT, REAGENT - 1_000)]),
        ];
        holdings[1].free = 0;
        assert_eq!(balance(&mut holdings), one("W1N1", "W3N1", Hydrogen, BATCH));
    }

    #[test]
    fn a_giver_never_sends_what_it_cannot_pay_the_fee_of() {
        let mut holdings = [
            holding("W1N1", &[(Oxygen, 0, 20_000)]),
            holding("W9N1", &[(Oxygen, REAGENT, 0)]),
        ];
        holdings[0].energy = engine::send_cost(BATCH, 8) - 1;
        assert_eq!(balance(&mut holdings), vec![]);
    }

    #[test]
    fn the_furthest_short_for_its_want_takes_first_then_the_nearest() {
        let ghodium = CatalyzedGhodiumAlkalide;
        let mut holdings = [
            holding("W1N1", &[(Hydrogen, 0, 30_000), (ghodium, 0, 30_000)]),
            holding("W5N1", &[(Hydrogen, REAGENT, 0)]),
            holding("W2N1", &[(Hydrogen, REAGENT, 4_000)]),
            holding("W3N1", &[(ghodium, BOOST, 0)]),
        ];
        assert_eq!(balance(&mut holdings), one("W1N1", "W3N1", ghodium, BOOST));
    }
}
