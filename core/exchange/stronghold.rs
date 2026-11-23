/// An Invader stronghold as the engine runs it: its towers fire on one body
/// of ours while its garrison stays on its ramparts, so no strategy of his is
/// guessed and its price is a run of its towers on our set, tick by tick.
use fabot_base::boost::Stock;
use fabot_base::engine;
use fabot_base::vocabulary::{Part, ResourceType, damage_ratio, multiple, takes};

use super::{Body, Fighter, Losses, Outcome, Side, Terrain, fighter, walk_in};

/// `strongholds.json` by level: its towers and each rampart's hits.
const TOWERS: [i32; 6] = [0, 1, 2, 3, 4, 6];
const RAMPART_HITS: [i32; 6] = [0, 100_000, 200_000, 500_000, 1_000_000, 2_000_000];
/// `coreAmounts` plus `containerAmounts` in `calc_reward`'s density, twice
/// over: a commodity draws about twice its density in raw resources through
/// its recipes (a wire 21 for 10, a switch about 350 for 220), each taken as
/// an energy, as no market prices it.
const LOOT: [i32; 6] = [0, 3_000, 40_000, 140_000, 900_000, 6_720_000];
pub const CORE_HITS: i32 = 100_000;

fn at(table: &[i32; 6], level: i32) -> i32 {
    table[level.clamp(0, 5) as usize]
}

pub fn loot(level: i32) -> i32 {
    at(&LOOT, level)
}

/// What our set meets at its post: the strike on the body the towers pick,
/// the hits it shoots through to the core, and what is repaired of them a tick.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Bunker {
    pub strike: i32,
    pub hits: i32,
    pub repair: i32,
}

impl Bunker {
    /// Every tower stands beside its core, so a post a shot off the core is
    /// within each one's optimal range.
    pub fn of_level(level: i32) -> Bunker {
        Bunker {
            strike: at(&TOWERS, level) * engine::tower_attack_at(engine::RANGED_RANGE + 1),
            hits: at(&RAMPART_HITS, level) + CORE_HITS,
            repair: 0,
        }
    }
}

/// The hits `raw` damage takes off `f`, its boosted TOUGH taking its share first.
fn struck_off(f: &Fighter, raw: i32) -> i32 {
    let n = f.body.len();
    let mut left = f64::from(raw);
    let mut lost = 0.0;
    for (i, part) in f.body.iter().enumerate() {
        let ratio = damage_ratio(*part, f.boosts.get(i).copied().flatten());
        let off = (left * ratio).min(f64::from(engine::part_hits_at(n, f.hits, i)));
        lost += off;
        left -= off / ratio;
    }
    (lost + left).round() as i32
}

fn live(f: &Fighter) -> Fighter {
    fighter::of_parts(&f.body, &f.boosts, f.hits, f.pos)
}

/// The set lands and shoots the rampart over the core, then the core; the
/// towers strike its first body standing, which heals itself alone: the
/// towers pick the nearest body as the set walks in, apart, so no mate's
/// heal is priced (seven bare-TOUGH rangers pooling theirs died one by
/// one). A body fights out its life at most.
pub fn outcome(ours: &Side, bunker: &Bunker, terrain: Terrain) -> Outcome {
    let lands: Vec<i32> = (ours.bodies.iter())
        .map(|f| walk_in(terrain, ours.cast_walk, f, &[]))
        .collect();
    let (Some(first), Some(last)) = (lands.iter().min(), lands.iter().max()) else {
        return Outcome::Lose;
    };
    let mut set = ours.bodies.clone();
    let mut dealt = 0;
    for tick in 1..=engine::CREEP_LIFETIME - last {
        let standing: Vec<Fighter> = set.iter().filter(|f| f.hits > 0).map(live).collect();
        let Some(struck) = set.iter_mut().find(|f| f.hits > 0) else {
            return Outcome::Lose;
        };
        let heal = fighter::healing(&live(struck));
        let shot: i32 =
            standing.iter().map(|f| f.ranged).sum::<i32>() * engine::RANGED_ATTACK_POWER;
        let max = engine::PART_HITS * struck.body.len() as i32;
        let taken = struck_off(struck, bunker.strike);
        struck.hits = (struck.hits - taken + heal).min(max).max(0);
        dealt += shot - bunker.repair;
        if dealt >= bunker.hits {
            return Outcome::Win(last - first + tick, losses(&ours.bodies, &set));
        }
        // A set the towers no longer wear down shoots on unchanged.
        if taken <= heal && set.iter().all(|f| f.hits == max_of(f) || f.hits == 0) {
            let shot = shot - bunker.repair;
            if shot <= 0 {
                return Outcome::Lose;
            }
            let more = (bunker.hits - dealt + shot - 1) / shot;
            return if tick + more <= engine::CREEP_LIFETIME - last {
                Outcome::Win(last - first + tick + more, losses(&ours.bodies, &set))
            } else {
                Outcome::Lose
            };
        }
    }
    Outcome::Lose
}

/// A set's cost at `bunker`: each cast at the share of its life its walk and
/// kill take, as it lives on for the next row (owner), and what it loses.
fn spent(
    side: &Side,
    (body, n): (&Body, i32),
    bunker: &Bunker,
    terrain: Terrain,
) -> Option<(Outcome, i32)> {
    let lands = (side.bodies.iter())
        .map(|f| walk_in(terrain, side.cast_walk, f, &[]))
        .min()?;
    let won = outcome(side, bunker, terrain);
    let Outcome::Win(ticks, losses) = won else {
        return None;
    };
    let spawned = engine::SPAWN_TICKS_PER_PART * body.len() as i32;
    let used = (lands + ticks - spawned).min(engine::CREEP_LIFETIME);
    let cast = engine::body_cost(body) * n * used / engine::CREEP_LIFETIME;
    Some((won, cast + losses.energy))
}

/// The cheapest set beside `ours` that takes the core: `n` copies of one
/// body, bare or boosted from `stock`, its outcome and cost. Every count is
/// priced, not bisected as `cheapest_win` does: a boosted copy waits on its
/// own lab fills, so one more can lose where fewer win.
pub fn cheapest_kill(
    ours: &Side,
    (bodies, copies): (&[Body], i32),
    stock: &Stock,
    bunker: &Bunker,
    terrain: Terrain,
) -> Option<(Vec<Body>, bool, Outcome, i32)> {
    let bare = Stock::default();
    let priced = |body: &Body, n: i32, boosted: bool| {
        let mut side = ours.clone();
        let cast = std::iter::repeat_n(&body[..], n as usize);
        (side.bodies).extend(fighter::cast_boosted(
            cast,
            if boosted { stock } else { &bare },
        ));
        let (won, cost) = spent(&side, (body, n), bunker, terrain)?;
        Some((vec![body.clone(); n as usize], boosted, won, cost))
    };
    (bodies.iter())
        .flat_map(|b| (1..=copies).flat_map(move |n| [(b, n, false), (b, n, true)]))
        .filter_map(|(b, n, boosted)| priced(b, n, boosted))
        .min_by_key(|(set, boosted, _, cost)| (*cost, set.len(), *boosted))
}

fn max_of(f: &Fighter) -> i32 {
    engine::PART_HITS * f.body.len() as i32
}

fn losses(was: &[Fighter], now: &[Fighter]) -> Losses {
    let mut out = Losses::default();
    for (w, n) in was.iter().zip(now) {
        let lost = w.hits - n.hits;
        out.dead += i32::from(n.hits == 0);
        out.hits += lost;
        out.energy += engine::body_cost(&w.body) * lost / w.hits.max(1);
    }
    out
}

/// What the strongest compound `stock` holds for `part` gives it, by `strength`.
fn best(stock: &Stock, part: Part, strength: impl Fn(ResourceType) -> f64) -> Option<f64> {
    (stock.held.iter())
        .filter(|h| takes(part, h.compound))
        .map(|h| strength(h.compound))
        .max_by(f64::total_cmp)
}

/// S11's leaders' single ranger, sized off the strike: TOUGH whose boost
/// holds it, HEAL that heals what passes, MOVE for a plain's pace, the rest
/// RANGED; none where `stock` boosts no TOUGH or the body cannot hold it.
pub fn kit(strike: i32, stock: &Stock, capacity: i32) -> Option<Body> {
    let ratio = 1.0
        / best(stock, Part::Tough, |c| {
            1.0 / damage_ratio(Part::Tough, Some(c))
        })?;
    let times = |part| best(stock, part, |c| f64::from(multiple(part, Some(c)))).unwrap_or(1.0);
    let (heal, legs) = (times(Part::Heal), times(Part::Move));
    let passed = f64::from(strike) * ratio;
    let tough = (passed / f64::from(engine::PART_HITS)).ceil() as usize;
    let heals = (passed / (f64::from(engine::HEAL_POWER) * heal)).ceil() as usize;
    let moves = |rest: usize| (rest as f64 / legs).ceil() as usize;
    let most = engine::MAX_BODY_PARTS;
    let body = |guns: usize| {
        let mut b = vec![Part::Tough; tough];
        b.extend(vec![Part::RangedAttack; guns]);
        b.extend(vec![Part::Heal; heals]);
        b.extend(vec![Part::Move; moves(tough + guns + heals)]);
        engine::with_move_last(b)
    };
    (1..most)
        .rev()
        .map(body)
        .find(|b| b.len() <= most && engine::body_cost(b) <= capacity)
}

#[cfg(test)]
mod tests {
    use super::*;
    use fabot_base::boost::Held;
    use fabot_base::vocabulary::part_count_in;

    fn t3() -> Stock {
        use ResourceType::*;
        let held = |compound| Held {
            compound,
            parts: 100,
            in_labs: 0,
        };
        Stock {
            held: [
                CatalyzedGhodiumAlkalide,
                CatalyzedLemergiumAlkalide,
                CatalyzedKeaniumAlkalide,
                CatalyzedZynthiumAlkalide,
            ]
            .map(held)
            .to_vec(),
            energy: 1000,
        }
    }

    /// Kalgen's and Robalian's ranger by tower count, from the strike alone.
    #[test]
    fn the_kit_is_the_leaders_ranger_per_tower() {
        for (level, tough, heal, ranged) in [(1, 2, 4, 34), (2, 4, 8, 28), (3, 6, 12, 22)] {
            let b = kit(Bunker::of_level(level).strike, &t3(), 12_900).unwrap();
            let count = |p| part_count_in(&b, p);
            assert_eq!(
                (
                    count(Part::Tough),
                    count(Part::Heal),
                    count(Part::RangedAttack)
                ),
                (tough, heal, ranged)
            );
            assert_eq!(count(Part::Move), 10);
        }
    }

    #[test]
    fn a_boosted_kit_holds_its_towers_and_a_bare_one_falls() {
        for level in 1..=3 {
            let bunker = Bunker::of_level(level);
            let b = kit(bunker.strike, &t3(), 12_900).unwrap();
            let terrain = Terrain {
                weight: engine::PLAIN_WEIGHT,
                run_off: 10,
            };
            let side = |stock: &Stock| Side {
                bodies: fighter::cast_boosted([&b[..]], stock),
                cast_walk: Some(50),
                ..Side::default()
            };
            assert!(
                matches!(outcome(&side(&t3()), &bunker, terrain), Outcome::Win(_, l) if l.dead == 0)
            );
            assert_eq!(
                outcome(&side(&Stock::default()), &bunker, terrain),
                Outcome::Lose
            );
        }
    }
}
