/// Who wins a fight between two sides standing in one room: two clocks, each
/// side's kill of the other, at the ranges the faster side picks.
pub mod fighter;

pub use fighter::Fighter;

use std::collections::{BTreeMap, BTreeSet};

use crate::core::engine;
use crate::core::grid::{self, FatigueFactor, Weight, step_ticks};
use crate::core::tiles::{self, Table, Tile};
use crate::core::vocabulary::BodyPart;
use crate::core::world::Pos;

#[derive(Clone, Debug, PartialEq)]
pub struct Tower {
    pub pos: Pos,
    pub energy: i32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Side {
    pub bodies: Vec<Fighter>,
    pub ramparts: BTreeMap<Pos, i32>,
    pub towers: Vec<Tower>,
}

/// `weight` is the step weight of the ground fought on; `run_off` the tiles a
/// kiter backs away before a wall or an exit corners it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Terrain {
    pub weight: i32,
    pub run_off: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Losses {
    pub dead: i32,
    pub hits: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcome {
    Win(i32, Losses),
    Kite(i32),
    Lose,
}

impl Outcome {
    pub fn is_lose(&self) -> bool {
        matches!(self, Outcome::Lose)
    }

    pub fn is_win(&self) -> bool {
        matches!(self, Outcome::Win(_, _))
    }
}

pub type Body = Vec<BodyPart>;

/// Holding a post the raid must come to, every pair stands at a swing's
/// range; else the faster side picks the range.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Stance {
    Hold,
    Standoff,
}

/// A rival room as a breach is planned over it: each tile's step weight and
/// the hits on each barrier tile, its rival ramparts' and walls' together.
#[derive(Clone, Debug, PartialEq)]
pub struct Ground {
    pub room: String,
    pub weights: Table<Weight>,
    pub barriers: BTreeMap<Tile, i32>,
}

/// A cast body lands no sooner than half a room's walk; no fight is read past
/// two rooms' walk.
const CAST_WALK: i32 = engine::ROOM_SIDE / 2;

const LONGEST: i32 = 2 * engine::ROOM_SIDE;

/// A raider by its place in its side, so a repeated body counts as many.
#[derive(Clone, Debug)]
struct Foe {
    at: i32,
    f: Fighter,
}

fn range_to(tile: &Pos, f: &Fighter) -> Option<i32> {
    f.pos.as_ref().and_then(|at| tile.range(at))
}

fn between(a: &Foe, b: &Foe) -> Option<i32> {
    if a.at == b.at {
        Some(0)
    } else {
        match &a.f.pos {
            Some(p) => range_to(p, &b.f),
            None => None,
        }
    }
}

#[derive(Clone)]
struct Healer {
    mender: Foe,
    casts_heal: bool,
    casts_ranged_heal: bool,
}

/// Read at the range it closes to within `horizon` ticks. A `rangedHeal`
/// drops the shot beside it, and either heal drops the swing.
fn heal_onto(weight: i32, horizon: i32, healer: &Healer, target: &Foe) -> i32 {
    match between(&healer.mender, target) {
        None => 0,
        Some(range) => {
            let closes = if horizon <= 0 || range <= engine::MELEE_RANGE {
                0
            } else {
                match step_ticks(healer.mender.f.gait, weight) {
                    Some(ticks) => horizon / ticks,
                    None => 0,
                }
            };

            let range = range - closes;

            if range <= engine::MELEE_RANGE && healer.casts_heal {
                fighter::healing(&healer.mender.f)
            } else if range <= engine::RANGED_RANGE && healer.casts_ranged_heal {
                fighter::healing(&healer.mender.f) * engine::RANGED_HEAL_POWER / engine::HEAL_POWER
            } else {
                0
            }
        }
    }
}

fn healers_of(raid: &[Foe]) -> Vec<Healer> {
    raid.iter()
        .filter(|h| h.f.heal > 0)
        .map(|h| Healer {
            mender: h.clone(),
            casts_heal: true,
            casts_ranged_heal: true,
        })
        .collect()
}

fn heal_from(weight: i32, horizon: i32, healers: &[Healer], target: &Foe) -> i32 {
    healers
        .iter()
        .map(|healer| heal_onto(weight, horizon, healer, target))
        .sum()
}

fn rampart_over(side: &Side, f: &Fighter) -> i32 {
    match &f.pos {
        Some(at) => side.ramparts.get(at).copied().unwrap_or(0),
        None => 0,
    }
}

/// A raid's price at a horizon: its striking bodies' hits in three phases (the
/// ramparts under them, which no heal puts back; the damage that breaks their
/// boosted TOUGH; the rest at `Healing`), against the damage it deals.
#[derive(Clone, Debug)]
struct Price {
    damage: i32,
    healing: i32,
    /// What a hit healed on its TOUGH costs our damage.
    tough_healing: i32,
    shield: i32,
    tough: i32,
    hits: i32,
    /// It dismantles: a raid dealing our bodies nothing is won only by outhealing it.
    breaches: bool,
    dismantlers: Vec<(Fighter, i32)>,
    /// Its dismantlers' hits that strike nothing: the kill where no striker stands.
    razing: i32,
}

/// Its healers heal the body they heal most. Where `heals`, a healer that
/// fights heals wherever its heal reaches and drops the weapon it heals over.
fn price_as(
    terrain: Terrain,
    theirs: &Side,
    raid: &[Foe],
    horizon: i32,
    heals: bool,
    range_of: &dyn Fn(&Foe) -> i32,
) -> Price {
    let striking: Vec<Foe> = raid
        .iter()
        .filter(|h| fighter::strikes(&h.f))
        .cloned()
        .collect();

    let weapons = |h: &Foe| -> (i32, i32) {
        let range = range_of(h);

        (
            if range <= engine::MELEE_RANGE {
                engine::ATTACK_POWER * h.f.attack
            } else {
                0
            },
            if range <= engine::RANGED_RANGE {
                engine::RANGED_ATTACK_POWER * h.f.ranged
            } else {
                0
            },
        )
    };

    let healers: Vec<Healer> = healers_of(raid)
        .into_iter()
        .map(|healer| {
            let (swing, shot) = weapons(&healer.mender);

            Healer {
                casts_heal: heals || swing == 0,
                casts_ranged_heal: heals || swing + shot == 0,
                ..healer
            }
        })
        .collect();

    let heal_on = |target: &Foe| heal_from(terrain.weight, horizon, &healers, target);
    let breachers: Vec<Foe> = raid.iter().filter(|h| h.f.work > 0).cloned().collect();

    let patients: &[Foe] = if !striking.is_empty() {
        &striking
    } else if !breachers.is_empty() {
        &breachers
    } else {
        raid
    };

    let focus: Option<Foe> = if patients.is_empty() {
        None
    } else {
        patients.iter().rev().max_by_key(|h| heal_on(h)).cloned()
    };

    let dropped: i32 = match &focus {
        None => 0,
        Some(focus) => healers
            .iter()
            .map(|healer| {
                let (swing, shot) = weapons(&healer.mender);

                if heal_onto(
                    terrain.weight,
                    horizon,
                    &Healer {
                        casts_ranged_heal: false,
                        ..healer.clone()
                    },
                    focus,
                ) > 0
                {
                    swing
                } else if heal_onto(terrain.weight, horizon, healer, focus) > 0 {
                    swing + shot
                } else {
                    0
                }
            })
            .sum(),
    };

    let healed = match &focus {
        Some(focus) => heal_on(focus),
        None => 0,
    };
    let tough: Vec<Foe> = striking
        .iter()
        .filter(|h| fighter::tough_hits(&h.f) > 0)
        .cloned()
        .collect();

    Price {
        breaches: !breachers.is_empty(),
        damage: striking
            .iter()
            .map(|h| {
                let (swing, shot) = weapons(h);
                swing + shot
            })
            .sum::<i32>()
            - dropped,
        healing: healed,
        tough_healing: if tough.is_empty() {
            healed
        } else {
            tough
                .iter()
                .map(|h| fighter::damage_for(&h.f, heal_on(h)))
                .max()
                .unwrap_or(healed)
        },
        shield: striking.iter().map(|h| rampart_over(theirs, &h.f)).sum(),
        tough: tough
            .iter()
            .map(|h| fighter::damage_for(&h.f, fighter::tough_hits(&h.f)))
            .sum(),
        hits: striking
            .iter()
            .map(|h| h.f.hits - fighter::tough_hits(&h.f))
            .sum(),
        dismantlers: raid
            .iter()
            .filter(|h| h.f.work > 0)
            .map(|h| (h.f.clone(), h.f.work * engine::DISMANTLE_POWER))
            .collect(),
        razing: breachers
            .iter()
            .filter(|h| !fighter::strikes(&h.f))
            .map(|h| fighter::effective_hits(&h.f))
            .sum(),
    }
}

fn prices(
    terrain: Terrain,
    theirs: &Side,
    raid: &[Foe],
    horizon: i32,
    range_of: &dyn Fn(&Foe) -> i32,
) -> Vec<Price> {
    let weapons = price_as(terrain, theirs, raid, horizon, false, range_of);

    if raid.iter().any(|h| h.f.heal > 0 && fighter::strikes(&h.f)) {
        vec![
            weapons,
            price_as(terrain, theirs, raid, horizon, true, range_of),
        ]
    } else {
        vec![weapons]
    }
}

fn out_heals(price: &Price, our_damage: i32) -> bool {
    our_damage > price.healing && (price.tough == 0 || our_damage > price.tough_healing)
}

#[derive(Clone, Copy, Debug)]
struct Focused {
    deals: i32,
    heals: i32,
    hits: i32,
    /// What its own swing is struck back a tick, focused or not.
    back: i32,
}

#[derive(Clone, Copy, Debug)]
struct Fight {
    won: bool,
    /// None where neither side ends the other.
    ticks: Option<f64>,
    losses: Losses,
}

/// The raid's layers (ramparts, TOUGH, hits) struck at `dealt` for `span`
/// ticks: Ok the tick it falls, else what is left of them.
fn spend(price: &Price, dealt: i32, span: f64, layers: &[f64]) -> Result<f64, Vec<f64>> {
    let rates = [
        f64::from(dealt),
        f64::from(dealt - price.tough_healing),
        f64::from(dealt - price.healing),
    ];
    let mut left = span;
    let mut spent = 0.0;

    for i in 0..layers.len().min(rates.len()) {
        let layer = layers[i];
        let rate = rates[i];

        if layer <= 0.0 || (rate > 0.0 && layer / rate < left) {
            let took = if layer <= 0.0 { 0.0 } else { layer / rate };
            left -= took;
            spent += took;
        } else {
            let mut rest = vec![0.0; i];
            rest.push(if rate <= 0.0 {
                layer
            } else {
                layer - left * rate
            });
            rest.extend_from_slice(&layers[i + 1..]);
            return Err(rest);
        }
    }

    Ok(spent)
}

/// Past our ramparts (`broken` is what its dismantlers take a tick) the raid
/// kills ours one at a time, fewest hits first; ours fight with those standing.
fn focused_against(
    price: &Price,
    beside: i32,
    bodies: &[Focused],
    (our_shield, broken): (i32, i32),
    (towers, approach): (i32, f64),
) -> Fight {
    let dealt = |alive: &[(Focused, f64)]| -> i32 {
        beside + alive.iter().map(|(b, _)| b.deals).sum::<i32>()
    };

    let whole: Vec<(Focused, f64)> = bodies.iter().map(|b| (*b, f64::from(b.hits))).collect();
    let total = price.damage + bodies.iter().map(|b| b.back).sum::<i32>();

    let price = if price.shield + price.tough + price.hits == 0 {
        Price {
            hits: price.razing,
            ..price.clone()
        }
    } else {
        price.clone()
    };

    let worn = |span: f64, alive: &[(Focused, f64)]| -> Vec<(Focused, f64)> {
        alive
            .iter()
            .map(|(b, left)| (*b, (left - f64::from(b.back) * span).max(0.0)))
            .collect()
    };

    let ends = |won: bool, ticks: Option<f64>, dead: i32, standing: &[(Focused, f64)]| -> Fight {
        Fight {
            won,
            ticks,
            losses: Losses {
                dead,
                hits: bodies.iter().map(|b| b.hits).sum::<i32>()
                    - standing.iter().map(|(_, left)| *left).sum::<f64>() as i32,
            },
        }
    };

    let phase = |mut layers: Vec<f64>,
                 mut alive: Vec<(Focused, f64)>,
                 mut elapsed: f64,
                 mut dead: i32|
     -> Fight {
        loop {
            if alive.is_empty() {
                return ends(false, Some(elapsed), dead, &[]);
            }

            let (focus, left) = alive[0];
            let rest = alive[1..].to_vec();
            let rate = f64::from(
                price.damage + focus.back - alive.iter().map(|(b, _)| b.heals).sum::<i32>(),
            );
            let span = if rate <= 0.0 {
                f64::INFINITY
            } else {
                left / rate
            };

            match spend(&price, dealt(&alive), span, &layers) {
                Ok(t) => {
                    let mut standing = vec![(focus, left - t * rate.max(0.0))];
                    standing.extend(worn(t, &rest));
                    return ends(true, Some(elapsed + t), dead, &standing);
                }
                Err(_) if span.is_infinite() => return ends(false, None, dead, &alive),
                Err(rest_layers) => {
                    layers = rest_layers;
                    alive = worn(span, &rest);
                    elapsed += span;
                    dead += 1;
                }
            }
        }
    };

    let mut order = whole.clone();
    order.sort_by_key(|(b, _)| b.hits);

    let fight = {
        let breaks = total + broken;

        match spend(
            &price,
            towers,
            approach,
            &[
                f64::from(price.shield),
                f64::from(price.tough),
                f64::from(price.hits),
            ],
        ) {
            Ok(t) => ends(true, Some(t), 0, &whole),
            Err(layers) => {
                if our_shield == 0 {
                    phase(layers, order, 0.0, 0)
                } else {
                    let span = if breaks <= 0 {
                        f64::INFINITY
                    } else {
                        f64::from(our_shield) / f64::from(breaks)
                    };

                    match spend(&price, dealt(&whole), span, &layers) {
                        Ok(t) => ends(true, Some(t), 0, &whole),
                        Err(_) if span.is_infinite() => ends(false, None, 0, &whole),
                        Err(layers) => phase(layers, worn(span, &order), span, 0),
                    }
                }
            }
        }
    };

    if total == 0 {
        Fight {
            won: !price.breaches || out_heals(&price, dealt(&whole)),
            ..fight
        }
    } else {
        fight
    }
}

/// The ticks `hostile` takes, struck alone at `damage`, net of its side's
/// heal, to fall behind `ours` on this ground, lose its last ATTACK, or die.
fn lamed_in(
    terrain: Terrain,
    raid: &[Foe],
    ours: FatigueFactor,
    damage: i32,
    hostile: &Foe,
) -> Option<i32> {
    let healers = healers_of(raid);
    let f = &hostile.f;

    let net_within = |ticks: i32| {
        damage - fighter::damage_for(f, heal_from(terrain.weight, ticks, &healers, hostile))
    };

    let needed = {
        let live = (f.body.len() as i32).min((f.hits + engine::PART_HITS - 1) / engine::PART_HITS);
        let mut live = live - 1;

        let hits = loop {
            let hits = engine::PART_HITS * live;
            let power = engine::power_at(&f.body, &f.boosts, hits);
            let gait = FatigueFactor {
                weight: f.gait.weight,
                moves: power(BodyPart::Move),
            };

            if live <= 0
                || power(BodyPart::Attack) <= 0
                || fighter::falls_behind(terrain.weight, ours, gait)
            {
                break hits;
            }

            live -= 1;
        };

        fighter::damage_for(f, (f.hits - hits).max(0))
    };

    // The window and the heal within it read each other: widened until it holds.
    let mut ticks = 0;
    let mut tries = 4;

    loop {
        let net = net_within(ticks);

        if net <= 0 {
            return None;
        }

        let lamed = (needed + net - 1) / net;

        if lamed <= ticks || tries == 0 {
            return Some(lamed);
        }

        ticks = lamed;
        tries -= 1;
    }
}

/// One body shoots one target a tick: every raider pressing it alike is lamed
/// in turn, the soonest first.
fn presses_for(terrain: Terrain, raid: &[Foe], fighter: &Fighter, hostile: &Foe) -> Option<i32> {
    let damage = fighter::damage_at(fighter, fighter.reach);

    lamed_in(terrain, raid, fighter.gait, damage, hostile).map(|mine| {
        mine + raid
            .iter()
            .map(|h| {
                if h.at == hostile.at
                    || !fighter::strikes(&h.f)
                    || h.f.reach >= fighter.reach
                    || fighter::falls_behind(terrain.weight, fighter.gait, h.f.gait)
                {
                    0
                } else {
                    match lamed_in(terrain, raid, fighter.gait, damage, h) {
                        Some(ticks) if ticks < mine || (ticks == mine && h.at < hostile.at) => {
                            ticks
                        }
                        _ => 0,
                    }
                }
            })
            .sum::<i32>()
    })
}

/// With both stepping at once, a pursuer at its pace never closes while the
/// kiter has ground to step onto; one quicker closes the ground between reaches.
fn keeps_off(terrain: Terrain, raid: &[Foe], fighter: &Fighter, hostile: &Foe) -> bool {
    let strike = hostile.f.reach;

    strike < fighter.reach
        && (fighter::falls_behind(terrain.weight, fighter.gait, hostile.f.gait)
            || strike > 0
                && (match (
                    step_ticks(fighter.gait, terrain.weight),
                    step_ticks(hostile.f.gait, terrain.weight),
                ) {
                    (Some(ours), Some(them)) => presses_for(terrain, raid, fighter, hostile)
                        .is_some_and(|presses| {
                            (them == ours
                                || presses < (fighter.reach - strike) * ours * them / (ours - them))
                                && presses <= terrain.run_off * ours
                        }),
                    _ => false,
                }))
}

/// Whether a hostile closes onto a body where the range is its own pick: it
/// swings and closing costs it nothing its shot keeps off, or its swing is its weapon.
fn closes_in(fighter: &Fighter, hostile: &Fighter) -> bool {
    hostile.attack > 0
        && (hostile.invader && hostile.heal == 0
            || fighter::damage_at(fighter, engine::RANGED_RANGE)
                >= fighter::damage_at(fighter, engine::MELEE_RANGE)
            || engine::ATTACK_POWER * hostile.attack
                >= engine::RANGED_ATTACK_POWER * hostile.ranged)
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Picker {
    Kites,
    Closes,
    Theirs,
}

/// The faster side picks the range.
fn standoff_pick(
    terrain: Terrain,
    raid: &[Foe],
    fighter: &Fighter,
    hostile: &Foe,
) -> (i32, Picker) {
    if keeps_off(terrain, raid, fighter, hostile) {
        (fighter.reach, Picker::Kites)
    } else if fighter::falls_behind(terrain.weight, fighter.gait, hostile.f.gait)
        || fighter::stationary(&hostile.f)
    {
        (fighter::best_range(fighter), Picker::Closes)
    } else if closes_in(fighter, &hostile.f) {
        (engine::MELEE_RANGE, Picker::Theirs)
    } else {
        (engine::RANGED_RANGE, Picker::Theirs)
    }
}

fn standoff(terrain: Terrain, raid: &[Foe], fighter: &Fighter, hostile: &Foe) -> i32 {
    standoff_pick(terrain, raid, fighter, hostile).0
}

/// Each of ours' stand range and the range it heals at.
type Stands = Vec<(i32, i32)>;

/// Each raider and the range each of ours meets it at.
type Rows = Vec<(Foe, Vec<i32>)>;

fn ranges_between(terrain: Terrain, raid: &[Foe], fighters: &[Fighter]) -> (Stands, Rows) {
    let rows: Vec<(Foe, Vec<i32>)> = raid
        .iter()
        .filter(|h| fighter::strikes(&h.f))
        .map(|h| {
            let ranges = fighters
                .iter()
                .map(|f| standoff(terrain, raid, f, h))
                .collect();

            (h.clone(), ranges)
        })
        .collect();

    let ours = fighters
        .iter()
        .enumerate()
        .map(|(i, _)| {
            let ranges: Vec<i32> = rows.iter().map(|(_, ranges)| ranges[i]).collect();

            if ranges.is_empty() {
                (engine::MELEE_RANGE, engine::MELEE_RANGE)
            } else {
                let furthest = ranges.iter().copied().max().unwrap_or(engine::MELEE_RANGE);
                let nearest = ranges.iter().copied().min().unwrap_or(engine::MELEE_RANGE);
                (engine::MELEE_RANGE.max(furthest), nearest)
            }
        })
        .collect();

    (ours, rows)
}

/// The range a raider is met at: the nearest of `ranges_between`'s, or its
/// own when it strikes none of ours.
fn range_of(
    terrain: Terrain,
    raid: &[Foe],
    fighters: &[Fighter],
    rows: &[(Foe, Vec<i32>)],
    hostile: &Foe,
) -> i32 {
    match rows.iter().find(|(h, _)| h.at == hostile.at) {
        Some((_, ranges)) if !ranges.is_empty() => {
            ranges.iter().copied().min().unwrap_or(engine::MELEE_RANGE)
        }
        Some(_) => engine::MELEE_RANGE,
        None => {
            if fighters.is_empty() {
                engine::MELEE_RANGE
            } else {
                fighters
                    .iter()
                    .map(|f| standoff(terrain, raid, f, hostile))
                    .min()
                    .unwrap_or(engine::MELEE_RANGE)
            }
        }
    }
}

#[derive(Debug)]
struct HitBack {
    taken: Vec<i32>,
    struck: i32,
    swingers: i32,
    /// The hits of the raiders that strike back a swing of ours.
    strikers: i32,
}

/// A swing on a body with a live ATTACK is struck back by it; a shot never.
/// Who swings at whom is either side's pick, so neither is priced on its best.
fn hit_back(
    theirs: &Side,
    fighters: &[Fighter],
    deals: &[i32],
    rows: &[(Foe, Vec<i32>)],
) -> HitBack {
    let swings: Vec<(Fighter, i32, Vec<i32>)> = rows
        .iter()
        .filter(|(h, _)| rampart_over(theirs, &h.f) == 0)
        .map(|(h, ranges)| {
            (
                h.f.clone(),
                engine::ATTACK_POWER * h.f.attack,
                ranges.clone(),
            )
        })
        .filter(|(_, swing, _)| *swing > 0)
        .collect();

    let swinging: Vec<bool> = fighters
        .iter()
        .zip(deals.iter())
        .map(|(f, range)| f.strikes_back > 0 && *range <= engine::MELEE_RANGE)
        .collect();

    let taken = swinging
        .iter()
        .enumerate()
        .map(|(i, swings_here)| {
            if !*swings_here {
                0
            } else {
                swings.iter().fold(0, |most, (_, swing, ranges)| {
                    if ranges[i] <= engine::MELEE_RANGE {
                        most.max(*swing)
                    } else {
                        most
                    }
                })
            }
        })
        .collect();

    let strikers = swings
        .iter()
        .filter(|(_, _, ranges)| {
            swinging
                .iter()
                .zip(ranges.iter())
                .any(|(swings_here, range)| *swings_here && *range <= engine::MELEE_RANGE)
        })
        .map(|(h, _, _)| fighter::effective_hits(h))
        .sum();

    let struck: Vec<(Fighter, i32)> = swings
        .iter()
        .filter_map(|(h, _, ranges)| {
            let least =
                fighters
                    .iter()
                    .zip(ranges.iter())
                    .fold(None, |least: Option<i32>, (f, range)| {
                        if *range <= engine::MELEE_RANGE {
                            Some(f.strikes_back.min(least.unwrap_or(f.strikes_back)))
                        } else {
                            least
                        }
                    });

            least.filter(|back| *back > 0).map(|back| (h.clone(), back))
        })
        .collect();

    HitBack {
        taken,
        struck: struck.iter().map(|(_, back)| *back).sum(),
        swingers: struck.iter().map(|(h, _)| fighter::effective_hits(h)).sum(),
        strikers,
    }
}

/// Our swings' hit back is taken only while its strikers stand, and they fall first.
fn taken_while(price: &Price, damage: i32, taken: i32, strikers: i32) -> i32 {
    let net = damage - price.healing;

    if taken == 0 || net <= 0 || strikers >= price.tough + price.hits {
        taken
    } else {
        let kill = f64::from(price.shield) * f64::from(net) / f64::from(damage)
            + f64::from(price.tough + price.hits);

        (f64::from(taken) * f64::from(strikers) / kill).ceil() as i32
    }
}

fn struck_back(price: &Price, damage: i32, struck: i32, swingers: i32) -> i32 {
    let hits = i64::from(price.shield + price.tough + price.hits);
    let net = i64::from(damage - price.healing);
    let all = net + i64::from(struck);

    if struck == 0 || all <= 0 {
        0
    } else if hits <= i64::from(swingers) || i64::from(struck) * hits <= i64::from(swingers) * all {
        struck
    } else if net <= 0 {
        0
    } else {
        (hits * net / (hits - i64::from(swingers)) - net) as i32
    }
}

/// A body that kites walks off its rampart.
fn stays_on(terrain: Terrain, stance: Stance, raid: &[Foe], fighter: &Fighter, tile: &Pos) -> bool {
    stance == Stance::Hold
        || raid.iter().all(|h| {
            !fighter::strikes(&h.f)
                || match standoff_pick(terrain, raid, fighter, h) {
                    (_, Picker::Theirs) => true,
                    (range, Picker::Closes) => range_to(tile, &h.f).is_some_and(|r| r <= range),
                    (_, Picker::Kites) => false,
                }
        })
}

fn on_ramparts(
    terrain: Terrain,
    stance: Stance,
    ours: &Side,
    raid: &[Foe],
) -> (Vec<Fighter>, Vec<(Pos, i32)>) {
    let mut held: Vec<(Pos, i32)> = Vec::new();

    let fighters = ours
        .bodies
        .iter()
        .map(|fighter| {
            match fighter
                .pos
                .as_ref()
                .and_then(|at| ours.ramparts.get(at).map(|hits| (at.clone(), *hits)))
            {
                Some((tile, hits)) if stays_on(terrain, stance, raid, fighter, &tile) => {
                    held.insert(0, (tile, hits));
                    Fighter {
                        strikes_back: 0,
                        ..fighter.clone()
                    }
                }
                _ => fighter.clone(),
            }
        })
        .collect();

    (fighters, held)
}

fn broken_by(terrain: Terrain, horizon: i32, price: &Price, ramparts: &[(Pos, i32)]) -> i32 {
    if ramparts.is_empty() {
        0
    } else {
        price
            .dismantlers
            .iter()
            .map(|(h, rate)| {
                let ticks = step_ticks(h.gait, terrain.weight).unwrap_or(i32::MAX);

                let reaches = |tile: &Pos| {
                    range_to(tile, h).is_some_and(|r| {
                        r <= engine::MELEE_RANGE
                            || i64::from(r - 1) * i64::from(ticks) <= i64::from(horizon)
                    })
                };

                if ramparts.iter().any(|(tile, _)| reaches(tile)) {
                    *rate
                } else {
                    0
                }
            })
            .sum()
    }
}

/// The slowest of our walks to within reach of the raid; every heal that
/// reaches the raid before we land counts.
fn arrival_of(terrain: Terrain, fighters: &[Fighter], raid: &[Fighter]) -> i32 {
    fighters.iter().fold(0, |most, f| {
        let walk = match &f.pos {
            None => CAST_WALK,
            Some(at) => {
                let tiles: Vec<i32> = raid.iter().filter_map(|h| range_to(at, h)).collect();

                if tiles.is_empty() {
                    0
                } else {
                    let nearest = tiles.iter().copied().min().unwrap_or(0);
                    (nearest - f.reach).max(0)
                        * step_ticks(f.gait, terrain.weight).unwrap_or(CAST_WALK)
                }
            }
        };

        most.max(walk)
    })
}

/// Our towers strike the raid until a weapon of either side reaches: the
/// nearest pair's gap, both walking in, a cast body landing at `castWalk`.
fn approach_of(terrain: Terrain, stance: Stance, fighters: &[Fighter], raid: &[Fighter]) -> f64 {
    let closing = |f: &Fighter, h: &Fighter| -> Option<f64> {
        match (&f.pos, h.pos.as_ref().and_then(|at| range_to(at, f))) {
            (None, _) => Some(f64::from(CAST_WALK)),
            (Some(_), None) => None,
            (Some(_), Some(range)) => {
                let gap = f64::from((range - f.reach.max(h.reach)).max(0));

                let walks = if stance == Stance::Hold {
                    None
                } else {
                    step_ticks(f.gait, terrain.weight)
                };

                match (walks, step_ticks(h.gait, terrain.weight)) {
                    (Some(ours), Some(theirs)) => {
                        Some(gap * f64::from(ours * theirs) / f64::from(ours + theirs))
                    }
                    (Some(ticks), None) | (None, Some(ticks)) => Some(gap * f64::from(ticks)),
                    (None, None) => None,
                }
            }
        }
    };

    let all: Vec<f64> = fighters
        .iter()
        .flat_map(|f| raid.iter().filter_map(|h| closing(f, h)))
        .collect();

    all.into_iter().reduce(f64::min).unwrap_or(0.0)
}

/// A raider that walks in strikes from its range of the body it strikes, the
/// one farthest from the tower.
fn towers_on(
    towers: &[Tower],
    strikes_from: &dyn Fn(&Foe) -> Vec<(Fighter, i32)>,
    raid: &[Foe],
) -> i32 {
    let loaded: Vec<&Tower> = towers
        .iter()
        .filter(|t| t.energy >= engine::TOWER_ENERGY_COST)
        .collect();

    let on = |h: &Foe| -> i32 {
        loaded
            .iter()
            .map(|t| {
                let here = range_to(&t.pos, &h.f).unwrap_or(engine::TOWER_FALLOFF_RANGE);

                let struck: Vec<i32> = strikes_from(h)
                    .iter()
                    .filter_map(|(f, range)| range_to(&t.pos, f).map(|r| r + range))
                    .collect();

                let range = if struck.is_empty() {
                    here
                } else {
                    struck.iter().copied().max().unwrap_or(here)
                };

                engine::tower_attack_at(range)
            })
            .sum()
    };

    let striking: Vec<&Foe> = raid.iter().filter(|h| fighter::strikes(&h.f)).collect();

    if loaded.is_empty() {
        0
    } else if striking.is_empty() {
        loaded.len() as i32 * engine::tower_attack_at(engine::TOWER_FALLOFF_RANGE)
    } else {
        striking
            .iter()
            .map(|h| on(h))
            .min()
            .unwrap_or(engine::TOWER_FALLOFF_RANGE)
    }
}

fn exchange(stance: Stance, ours: &Side, theirs: &Side, terrain: Terrain) -> Outcome {
    let raid: Vec<Foe> = theirs
        .bodies
        .iter()
        .enumerate()
        .map(|(i, f)| Foe {
            at: i as i32,
            f: f.clone(),
        })
        .collect();

    let (fighters, ramparts) = on_ramparts(terrain, stance, ours, &raid);

    let (mine, rows): (Stands, Rows) = match stance {
        Stance::Hold => {
            let swing: Vec<i32> = fighters.iter().map(|_| engine::MELEE_RANGE).collect();
            let mine: Vec<(i32, i32)> = swing.iter().map(|range| (*range, *range)).collect();
            let rows = raid
                .iter()
                .filter(|h| fighter::strikes(&h.f))
                .map(|h| (h.clone(), swing.clone()))
                .collect();

            (mine, rows)
        }
        Stance::Standoff => ranges_between(terrain, &raid, &fighters),
    };

    let range_of = |hostile: &Foe| -> i32 {
        if stance == Stance::Hold {
            engine::MELEE_RANGE
        } else {
            range_of(terrain, &raid, &fighters, &rows, hostile)
        }
    };

    let back = hit_back(
        theirs,
        &fighters,
        &mine.iter().map(|(range, _)| *range).collect::<Vec<_>>(),
        &rows,
    );

    let kited: Vec<Fighter> = match stance {
        Stance::Hold => Vec::new(),
        Stance::Standoff => theirs
            .bodies
            .iter()
            .filter(|f| fighter::strikes(f))
            .cloned()
            .collect(),
    };

    let pairs: Vec<(&Fighter, (i32, i32))> = fighters.iter().zip(mine.iter().copied()).collect();

    let strikes_from = |h: &Foe| -> Vec<(Fighter, i32)> {
        match (stance, rows.iter().find(|(r, _)| r.at == h.at)) {
            (_, Some(_)) if fighter::stationary(&h.f) => Vec::new(),
            (Stance::Hold, Some(_)) => fighters.iter().map(|f| (f.clone(), h.f.reach)).collect(),
            (Stance::Standoff, Some((_, ranges))) => fighters
                .iter()
                .cloned()
                .zip(ranges.iter().copied())
                .collect(),
            (_, None) => Vec::new(),
        }
    };

    let towers = towers_on(&ours.towers, &|h: &Foe| strikes_from(h), &raid);
    let walking_in = towers_on(&ours.towers, &|_: &Foe| Vec::new(), &raid);

    let bodies: Vec<Focused> = pairs
        .iter()
        .map(|(f, (range, heal_range))| Focused {
            deals: fighter::damage_at(f, *range),
            heals: fighter::heal_at(f, *heal_range),
            hits: f.hits,
            back: 0,
        })
        .collect();

    let dealt = towers + bodies.iter().map(|b| b.deals).sum::<i32>();
    let approach = approach_of(terrain, stance, &fighters, &theirs.bodies);

    let settle = |horizon: i32, price: &Price| -> (Outcome, i32) {
        let struck = struck_back(price, dealt, back.struck, back.swingers);
        let damage = dealt + struck;

        let bodies: Vec<Focused> = bodies
            .iter()
            .zip(back.taken.iter())
            .map(|(b, taken)| Focused {
                back: taken_while(price, damage, *taken, back.strikers),
                ..*b
            })
            .collect();

        let priced = Price {
            damage: price.damage + bodies.iter().map(|b| b.back).sum::<i32>(),
            ..price.clone()
        };

        let shield = (
            ramparts.iter().map(|(_, hits)| *hits).sum::<i32>(),
            broken_by(terrain, horizon, price, &ramparts),
        );

        let fight = focused_against(
            price,
            towers + struck,
            &bodies,
            shield,
            (walking_in, approach),
        );

        let ticks = fight.ticks.map_or(LONGEST, |t| t.ceil() as i32);

        let outcome = if priced.damage == 0 && !kited.is_empty() {
            let least = kited
                .iter()
                .map(|h| fighter::damage_for(h, priced.healing))
                .min()
                .unwrap_or(0);

            if damage > least {
                Outcome::Kite(ticks)
            } else {
                Outcome::Lose
            }
        } else if fight.won {
            Outcome::Win(ticks, fight.losses)
        } else {
            Outcome::Lose
        };

        (outcome, ticks)
    };

    // Every heal and dismantle reaching the fight before it ends counts: the
    // horizon widens to the fight's length, read off both pricings.
    let widen = |mut horizon: i32, mut rounds: i32| -> Vec<Outcome> {
        loop {
            let settled: Vec<(Outcome, i32)> = prices(terrain, theirs, &raid, horizon, &range_of)
                .iter()
                .map(|price| settle(horizon, price))
                .collect();

            let lasts = settled.iter().map(|(_, ticks)| *ticks).max().unwrap_or(0);

            if lasts <= horizon || rounds == 0 {
                return settled.into_iter().map(|(outcome, _)| outcome).collect();
            }

            horizon = LONGEST.min(lasts);
            rounds -= 1;
        }
    };

    let outcomes = widen(arrival_of(terrain, &fighters, &theirs.bodies), 3);

    if outcomes.iter().any(Outcome::is_lose) {
        Outcome::Lose
    } else {
        outcomes.into_iter().next().unwrap_or(Outcome::Lose)
    }
}

/// A raid comes to our post only while it must: one that strikes from past
/// the reach of any striker of ours and outlasts our towers picks its range,
/// and the fight is a standoff (owner's call).
pub fn stance_against(stance: Stance, ours: &Side, theirs: &Side, terrain: Terrain) -> Stance {
    let striking: Vec<&Fighter> = ours.bodies.iter().filter(|f| fighter::strikes(f)).collect();

    let reach = if striking.is_empty() {
        0
    } else {
        striking
            .iter()
            .map(|f| f.reach)
            .min()
            .unwrap_or(engine::MELEE_RANGE)
    };

    if stance == Stance::Hold
        && theirs.bodies.iter().any(|h| h.reach > reach)
        && !exchange(
            Stance::Hold,
            &Side {
                bodies: Vec::new(),
                ..ours.clone()
            },
            theirs,
            terrain,
        )
        .is_win()
    {
        Stance::Standoff
    } else {
        stance
    }
}

/// Towers not ours strike the kite ground and the ring alike, refilled, and
/// nothing prices them: no side of ours wins under them.
pub fn outcome(stance: Stance, ours: &Side, theirs: &Side, terrain: Terrain) -> Outcome {
    if theirs.bodies.is_empty() {
        Outcome::Win(0, Losses { dead: 0, hits: 0 })
    } else if !theirs.towers.is_empty() {
        Outcome::Lose
    } else {
        exchange(
            stance_against(stance, ours, theirs, terrain),
            ours,
            theirs,
            terrain,
        )
    }
}

pub fn pooled_heal(seated: i32, ours: &[Fighter]) -> i32 {
    let mut heals = ours.to_vec();
    heals.sort_by_key(|f| std::cmp::Reverse(f.heal));
    let seated = seated.min(ours.len() as i32).max(0) as usize;
    let (beside, apart) = heals.split_at(seated);

    engine::HEAL_POWER * beside.iter().map(|f| f.heal).sum::<i32>()
        + engine::RANGED_HEAL_POWER * apart.iter().map(|f| f.heal).sum::<i32>()
}

/// What towers deal a body at `at`, the one strike a breach is gated and
/// re-priced by.
pub fn tower_strike(towers: &[Pos], at: &Pos) -> i32 {
    towers
        .iter()
        .map(|p| p.range(at).map_or(0, engine::tower_attack_at))
        .sum()
}

/// The cheapest way from `sources` onto a goal: each step's walk at `gait`,
/// and the ticks to break a barrier stepped onto at `rate`. A tile the towers
/// strike past `heal` is never stepped on. Its barriers in order, each with the
/// tile it is broken from, and the ticks.
pub fn breach_plan(
    ground: &Ground,
    gait: FatigueFactor,
    towers: &[Pos],
    heal: i32,
    rate: i32,
    sources: &[Tile],
    goals: &[Tile],
) -> Option<(Vec<(Tile, Tile)>, i32)> {
    let struck = |t: Tile| tower_strike(towers, &tiles::to_pos(&ground.room, t));

    let cost = |t: Tile| -> Option<i32> {
        let step = step_ticks(gait, grid::value(ground.weights[t])?)?;

        if tiles::on_edge(t) || struck(t) > heal {
            None
        } else {
            match ground.barriers.get(&t) {
                Some(hits) if rate > 0 => Some(step + (hits + rate - 1) / rate),
                Some(_) => None,
                None => Some(step),
            }
        }
    };

    let mut dist = [i32::MAX; tiles::COUNT];
    let mut prev: [Option<Tile>; tiles::COUNT] = [None; tiles::COUNT];
    let goal: BTreeSet<Tile> = goals.iter().copied().collect();

    for &t in sources {
        dist[t] = 0;
    }

    let mut queue: BTreeSet<(i32, Tile)> = sources.iter().map(|t| (0, *t)).collect();

    let found = loop {
        let Some((d, t)) = queue.pop_first() else {
            break None;
        };

        if d > dist[t] {
            continue;
        }

        if goal.contains(&t) {
            break Some(t);
        }

        for &n in tiles::around(t) {
            match cost(n) {
                Some(c) if d + c < dist[n] => {
                    dist[n] = d + c;
                    prev[n] = Some(t);
                    queue.insert((d + c, n));
                }
                _ => {}
            }
        }
    };

    found.map(|last| {
        let mut walked = Vec::new();
        let mut t = last;

        while let Some(p) = prev[t] {
            walked.push(t);
            t = p;
        }

        walked.push(t);
        walked.reverse();

        let broken = walked
            .windows(2)
            .filter(|pair| ground.barriers.contains_key(&pair[1]))
            .map(|pair| (pair[1], pair[0]))
            .collect();

        (broken, dist[last])
    })
}

/// The cheapest cast can be a marginal win that loses its bodies, so lost hits
/// and ticks are priced too. A win is monotone in one body's count, so its
/// fewest is bisected, then grown while one more saves what it costs.
///
/// The bodies to cast, not one: `n` whole copies of the cheapest.
pub fn cheapest_win(
    stance: Stance,
    ours: &Side,
    bodies: &[Body],
    copies_of: impl Fn(&Body) -> i32,
    tick_worth: i32,
    theirs: &Side,
    terrain: Terrain,
) -> Option<Vec<Body>> {
    let cost = |body: &Body, n: i32| -> Option<i32> {
        let side = Side {
            bodies: ours
                .bodies
                .iter()
                .cloned()
                .chain(std::iter::repeat_n(fighter::cast(body), n.max(0) as usize))
                .collect(),
            ..ours.clone()
        };

        match outcome(stance, &side, theirs, terrain) {
            Outcome::Win(ticks, losses) => {
                let each = engine::body_cost(body);

                Some(
                    each * n
                        + each * losses.hits / (engine::PART_HITS * body.len() as i32)
                        + tick_worth * ticks,
                )
            }
            Outcome::Kite(ticks) => Some(engine::body_cost(body) * n + tick_worth * ticks),
            Outcome::Lose => None,
        }
    };

    let cheapest = |body: &Body| -> Option<(Vec<Body>, i32)> {
        let most = copies_of(body);

        let between = |mut loses: i32, mut won: i32| -> i32 {
            while won - loses > 1 {
                let mid = (loses + won) / 2;

                if cost(body, mid).is_some() {
                    won = mid;
                } else {
                    loses = mid;
                }
            }

            won
        };

        let onward = |mut n: i32, mut spent: i32| -> (i32, i32) {
            while n < most {
                match cost(body, n + 1) {
                    Some(next) if next < spent => {
                        n += 1;
                        spent = next;
                    }
                    _ => break,
                }
            }

            (n, spent)
        };

        if most > 0 && cost(body, most).is_some() {
            let fewest = between(0, most);

            match cost(body, fewest) {
                Some(spent) => {
                    let (n, spent) = onward(fewest, spent);

                    Some((vec![body.clone(); n.max(0) as usize], spent))
                }
                None => None,
            }
        } else {
            None
        }
    };

    let priced: Vec<(Vec<Body>, i32)> = bodies.iter().filter_map(cheapest).collect();

    priced
        .into_iter()
        .min_by_key(|(set, spent)| (*spent, set.iter().map(Vec::len).sum::<usize>(), set.len()))
        .map(|(set, _)| set)
}

/// What a body standing on each tile takes next tick: the charged towers'
/// falloff and each striker's reach, one more where it can step. Built once a
/// room and tick, shared by every body there.
pub fn damage_map(room: &str, towers: &[Pos], foes: &[Fighter]) -> Table<i32> {
    let mut map = tiles::table(0);
    for t in tiles::all() {
        let at = tiles::to_pos(room, t);
        let struck = |range: i32, damage: fn(&Fighter) -> i32| {
            foes.iter()
                .filter(|f| {
                    f.pos
                        .as_ref()
                        .is_some_and(|p| p.within(range + i32::from(f.gait.moves > 0), &at))
                })
                .map(damage)
                .sum::<i32>()
        };
        map[t] = towers
            .iter()
            .filter_map(|p| p.range(&at))
            .map(engine::tower_attack_at)
            .sum::<i32>()
            + struck(engine::MELEE_RANGE, |f| engine::ATTACK_POWER * f.attack)
            + struck(engine::RANGED_RANGE, |f| {
                engine::RANGED_ATTACK_POWER * f.ranged
            });
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::vocabulary::Boost;

    const ROOM: &str = "W1N1";

    /// `maxTest = 1000` of the F# generated cases, from a fixed seed.
    const CASES: usize = 1000;

    struct Rng(u64);

    impl Rng {
        fn new(seed: u64) -> Rng {
            Rng(seed | 1)
        }

        fn next(&mut self) -> u64 {
            let mut x = self.0;
            x ^= x >> 12;
            x ^= x << 25;
            x ^= x >> 27;
            self.0 = x;
            x.wrapping_mul(0x2545_f491_4f6c_dd1d)
        }

        fn between(&mut self, low: i32, high: i32) -> i32 {
            low + (self.next() % (high - low + 1) as u64) as i32
        }

        fn element<T: Copy>(&mut self, items: &[T]) -> T {
            items[(self.next() % items.len() as u64) as usize]
        }
    }

    fn fighter(rng: &mut Rng) -> Fighter {
        let n = rng.between(1, 20);
        let body: Vec<BodyPart> = (0..n)
            .map(|_| {
                rng.element(&[
                    BodyPart::Attack,
                    BodyPart::RangedAttack,
                    BodyPart::Heal,
                    BodyPart::Move,
                    BodyPart::Tough,
                    BodyPart::Work,
                ])
            })
            .collect();
        let tier = rng.element(&[Boost::Unboosted, Boost::Tier1, Boost::Tier2, Boost::Tier3]);
        let boosts = vec![tier; n as usize];
        let x = rng.between(1, 48);
        let y = rng.between(1, 48);
        let pos = Some(Pos::new(ROOM, x, y));

        fighter::of_parts(&body, &boosts, 100 * n, pos)
    }

    fn side(rng: &mut Rng) -> Side {
        let n = rng.between(1, 4);
        let bodies = (0..n).map(|_| fighter(rng)).collect();
        let towers = rng.between(0, 2);

        Side {
            bodies,
            ramparts: BTreeMap::new(),
            towers: (0..towers)
                .map(|k| Tower {
                    pos: Pos::new(ROOM, 10 + k, 10),
                    energy: 1000,
                })
                .collect(),
        }
    }

    fn terrain(rng: &mut Rng) -> Terrain {
        let weight = rng.element(&[1, 2, 10]);
        let run_off = rng.between(0, 40);

        Terrain { weight, run_off }
    }

    fn bare(s: &Side) -> Side {
        Side {
            towers: Vec::new(),
            ..s.clone()
        }
    }

    #[test]
    fn a_fight_won_leaves_one_of_our_bodies_standing() {
        let mut rng = Rng::new(0x5eed_1234);

        for case in 0..CASES {
            let ours = side(&mut rng);
            let theirs = side(&mut rng);
            let ground = terrain(&mut rng);

            if let Outcome::Win(_, losses) =
                outcome(Stance::Standoff, &ours, &bare(&theirs), ground)
            {
                assert!(
                    losses.dead < ours.bodies.len() as i32,
                    "case {}: {} of {} bodies dead",
                    case,
                    losses.dead,
                    ours.bodies.len()
                );
            }
        }
    }

    #[test]
    fn a_way_in_found_with_some_heal_is_found_with_more_and_no_slower() {
        let mut rng = Rng::new(0xfeed_4321);
        let tile = tiles::index;

        for case in 0..CASES {
            let hits: Vec<i32> = (0..48).map(|_| rng.between(1, 300000)).collect();
            let heal = rng.between(0, 600);
            let x = rng.between(1, 48);
            let y = rng.between(1, 48);

            let ground = Ground {
                room: ROOM.to_string(),
                weights: tiles::table(grid::weight(2)),
                barriers: hits
                    .iter()
                    .enumerate()
                    .map(|(i, h)| (tile(25, i as i32 + 1), *h))
                    .collect(),
            };

            let sources: Vec<Tile> = (1..=48).map(|y| tile(49, y)).collect();

            let plan = |heal: i32| {
                breach_plan(
                    &ground,
                    FatigueFactor {
                        weight: 1,
                        moves: 1,
                    },
                    &[Pos::new(ROOM, x, y)],
                    heal,
                    1000,
                    &sources,
                    &[tile(5, 25)],
                )
            };

            let holds = match (plan(heal), plan(heal + 100)) {
                (Some((_, ticks)), Some((_, fewer))) => fewer <= ticks,
                (Some(_), None) => false,
                _ => true,
            };

            assert!(holds, "case {case}: heal {heal} from {x},{y}");
        }
    }
}
