/// A price is what the world loses, in energy a tick, while a piece of work
/// waits: the one number the pool ranks a Task by and the spawn weighs a body
/// by.
use crate::core::engine;

pub const HELD_RATE: f64 = engine::HELD_OUTPUT_PER_TICK as f64;

/// Ranks are coarse (a factor of √2) so a rank does not jitter with every
/// unit a store gains.
const RANKS_PER_HALVING: f64 = 2.0;

pub const BAND_RANKS: i32 = 10;

/// Work with no energy price of its own is anchored a band apart.
pub const SURPLUS_RANK: i32 = 2 * BAND_RANKS;

pub const UPGRADE_RANK: i32 = 3 * BAND_RANKS;

pub const BOTTOM_RANK: i32 = 4 * BAND_RANKS;

/// Over the stock's own refill: a load no sink takes stands at the controller
/// before it is banked.
pub const BUFFER_RANK: i32 = UPGRADE_RANK + BAND_RANKS / 2;

/// Not a price but an engine fact: the controller downgrades, and safe mode
/// needs the timer.
pub const DEADLINE_RANK: i32 = -BAND_RANKS;

/// Priced values stay under it, clear of the fight's and the deadline's bands.
pub const URGENT_RANK: i32 = -BAND_RANKS / 2;

pub fn rank_of(value: f64) -> i32 {
    if !(value > 0.0) {
        BOTTOM_RANK
    } else {
        let rank = RANKS_PER_HALVING * (HELD_RATE / value).ln() / 2.0f64.ln();
        if rank >= BOTTOM_RANK as f64 {
            BOTTOM_RANK
        } else {
            (rank + 0.5).floor() as i32
        }
    }
}

pub fn value_of_rank(rank: i32) -> f64 {
    HELD_RATE / 2.0f64.powf(rank as f64 / RANKS_PER_HALVING)
}

pub fn economy_rank(value: f64) -> i32 {
    (URGENT_RANK + 1).max(rank_of(value))
}

/// A rank and a walk on one scale: a walk a whole life long costs the ranks
/// from the held rate to the bottom, the only tick count the ranks have.
pub fn walked(rank: i32, walk: i32) -> i32 {
    rank * engine::CREEP_LIFETIME + walk * BOTTOM_RANK
}

pub fn flow_draw(loss: f64) -> f64 {
    HELD_RATE + 0.0f64.max(loss)
}

/// A full container drops every dig on the floor.
pub fn overflow_loss(rate: i32, fill: f64) -> f64 {
    rate as f64 * 0.0f64.max(1.0f64.min(fill))
}

/// ENERGY_DECAY: a dropped pile loses `ceil(amount / 1000)` a tick.
pub fn pile_decay(amount: i32) -> f64 {
    if amount <= 0 {
        0.0
    } else {
        ((amount + 999) / 1000) as f64
    }
}

/// Capped at a full source container's loss, so the biggest grave ties one and
/// travel decides.
pub fn transient_loss(stored: i32) -> f64 {
    HELD_RATE.min(
        stored.max(0) as f64
            / (engine::TOMBSTONE_DECAY_PER_PART * engine::MAX_BODY_PARTS as i32) as f64,
    )
}

/// Halved because the stock was hauled once already, unless the bank is short
/// and the spawn waits on the walk.
pub fn stock_draw(starved: bool, hungriest: f64) -> f64 {
    let worth = HELD_RATE.min(0.0f64.max(hungriest));
    if starved {
        worth
    } else {
        worth / 2.0
    }
}

pub const HOME_RAID_PRIOR: f64 = 1.0 / engine::CREEP_LIFETIME as f64;

/// Priced against the cheapest hostile part (50 energy) at the far end of the
/// tower's range.
pub fn tower_exchange() -> f64 {
    engine::tower_attack_at(engine::TOWER_FALLOFF_RANGE) as f64 / engine::TOWER_ENERGY_COST as f64
        * 50.0
        / engine::PART_HITS as f64
}

pub fn tower_short(hazard: f64, shortfall: i32) -> f64 {
    hazard * shortfall.max(0) as f64 * tower_exchange()
}

pub fn rebuild_at_risk(rebuild: i32, hits: i32, decay_per_tick: f64) -> f64 {
    rebuild.max(0) as f64 * 0.0f64.max(decay_per_tick) / hits.max(1) as f64
}

pub fn body_return(lifetime_value: f64, energy: i32, spawn_ticks: i32, tick_worth: f64) -> f64 {
    let cost = energy as f64 + spawn_ticks as f64 * tick_worth;
    if cost <= 0.0 {
        0.0
    } else {
        lifetime_value / cost
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::economy::dice::Dice;

    #[test]
    fn a_rank_reads_back_off_its_value_across_the_scale() {
        for rank in URGENT_RANK + 1..=BOTTOM_RANK {
            assert_eq!(rank_of(value_of_rank(rank)), rank, "rank {}", rank);
        }
    }

    #[test]
    fn more_value_never_ranks_lower() {
        let mut dice = Dice::new(3);
        let mut values: Vec<f64> = vec![0.0, 1e-9, 0.5, 1.0, 7.3, 10.0, 1e3, 1e9, 1e18];
        for _ in 0..200 {
            values.push(dice.next() as f64 / (1u64 << (dice.below(40) as u32)) as f64);
        }
        for a in &values {
            for b in &values {
                let (lo, hi) = (a.min(*b), a.max(*b));
                assert!(rank_of(hi) <= rank_of(lo), "{} to {}", lo, hi);
            }
        }
    }

    #[test]
    fn a_held_sources_flow_ranks_zero_a_neutrals_two_ranks_under() {
        assert_eq!(rank_of(engine::HELD_OUTPUT_PER_TICK as f64), 0, "held");
        assert_eq!(
            rank_of(engine::NEUTRAL_OUTPUT_PER_TICK as f64),
            2,
            "neutral"
        );
    }

    #[test]
    fn a_bodys_return_is_its_value_over_its_energy_and_its_spawn_time() {
        let table = [
            (15_000.0, 800, 24, 20.0, 15_000.0 / 1_280.0),
            (600.0, 0, 0, 5.0, 0.0),
        ];
        for (value, energy, ticks, worth, expected) in table {
            let got = body_return(value, energy, ticks, worth);
            assert!(
                (got - expected).abs() <= 1e-9 * expected.abs().max(1.0),
                "{} over {}: {} vs {}",
                value,
                energy,
                got,
                expected
            );
        }
    }
}
