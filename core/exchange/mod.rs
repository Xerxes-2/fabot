/// Who wins a fight between two sides standing in one room: two clocks, each
/// side's kill of the other, at the ranges the faster side picks.
pub mod fighter;

pub use fighter::Fighter;

use std::collections::{BTreeMap, BTreeSet};

use fabot_base::grid::{self, FatigueFactor, Weight, step_ticks};
use fabot_base::tiles::{self, Table, Tile};
use fabot_base::vocabulary::Part;
use fabot_base::world::{Pos, RoomName, Striker};
use fabot_base::{boost, engine};

#[derive(Clone, Debug, PartialEq)]
pub struct Tower {
    pub pos: Pos,
    pub energy: i32,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Side {
    pub bodies: Vec<Fighter>,
    pub ramparts: BTreeMap<Pos, i32>,
    pub towers: Vec<Tower>,
    /// The tiles a body not yet cast walks once spawned; none, it lands at half a room's walk.
    pub cast_walk: Option<i32>,
    /// It walks in as one, so no body is fed in alone.
    pub together: bool,
    /// Our structures a raid razes for value, and their hits.
    pub keeps: Vec<(Pos, i32)>,
    /// The spawns its casts come off, each in series; none, one.
    pub spawns: i32,
    /// It forms: every body waits for its last cast, whichever arm it is priced by.
    pub forming: bool,
}

impl Side {
    fn lands_as_one(&self) -> bool {
        self.together || self.forming
    }
}

/// `weight` is the step weight of the ground fought on; `run_off` the tiles a
/// kiter backs away before a wall or an exit corners it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Terrain {
    pub weight: i32,
    pub run_off: i32,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct Losses {
    pub dead: i32,
    pub hits: i32,
    /// The energy of our bodies the hits strike off: a dead one whole, a
    /// standing one its share of the hits it lost.
    pub energy: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(from = "Stored", into = "Stored")]
pub enum Outcome {
    Win(i32, Losses),
    Kite(i32),
    Lose,
}

/// As `npm run exchange` writes a prediction.
#[derive(Clone, Copy, serde::Serialize, serde::Deserialize)]
#[serde(tag = "kind", rename_all = "lowercase")]
enum Stored {
    Win { ticks: i32, dead: i32, hits: i32 },
    Kite { ticks: i32 },
    Lose,
}

impl From<Stored> for Outcome {
    fn from(s: Stored) -> Outcome {
        match s {
            Stored::Win { ticks, dead, hits } => Outcome::Win(
                ticks,
                Losses {
                    dead,
                    hits,
                    ..Losses::default()
                },
            ),
            Stored::Kite { ticks } => Outcome::Kite(ticks),
            Stored::Lose => Outcome::Lose,
        }
    }
}

impl From<Outcome> for Stored {
    fn from(o: Outcome) -> Stored {
        match o {
            Outcome::Win(ticks, Losses { dead, hits, .. }) => Stored::Win { ticks, dead, hits },
            Outcome::Kite(ticks) => Stored::Kite { ticks },
            Outcome::Lose => Stored::Lose,
        }
    }
}

impl Outcome {
    pub fn is_lose(&self) -> bool {
        matches!(self, Outcome::Lose)
    }

    pub fn is_win(&self) -> bool {
        matches!(self, Outcome::Win(_, _))
    }

    /// What a Win is priced to lose; none on a Kite or Lose.
    pub fn losses(&self) -> Losses {
        match self {
            Outcome::Win(_, losses) => *losses,
            _ => Losses::default(),
        }
    }

    /// The ticks a Win or Kite is priced to run.
    pub fn ticks(&self) -> i32 {
        match self {
            Outcome::Win(ticks, _) | Outcome::Kite(ticks) => *ticks,
            Outcome::Lose => 0,
        }
    }
}

pub type Body = Vec<Part>;

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
    pub room: RoomName,
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
        a.f.pos.as_ref().and_then(|p| range_to(p, &b.f))
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
                step_ticks(healer.mender.f.gait, weight).map_or(0, |ticks| horizon / ticks)
            };

            let range = range - closes;

            if range <= engine::MELEE_RANGE && healer.casts_heal {
                fighter::healing(&healer.mender.f)
            } else if range <= engine::RANGED_RANGE && healer.casts_ranged_heal {
                engine::heal_at(healer.mender.f.heal, engine::RANGED_RANGE)
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
    (f.pos.as_ref())
        .and_then(|at| side.ramparts.get(at).copied())
        .unwrap_or(0)
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
    /// What `hits` heals back to: struck under its heal, the raid regrows.
    whole: i32,
    /// It dismantles or holds our ground: a raid dealing our bodies nothing is won only by outhealing it.
    breaches: bool,
    dismantlers: Vec<(Fighter, i32)>,
    /// Its breachers' hits that strike nothing: the kill where no striker stands.
    razing: i32,
    /// Its swings land, struck back by ours; else it holds them (a dismantle drops a swing).
    swings: bool,
}

/// Its healers heal the body they heal most. Where `heals`, a healer that
/// fights heals wherever its heal reaches and drops the weapon it heals over;
/// where `swings`, its swings land and are struck back, else it holds them.
fn price_as(
    terrain: Terrain,
    theirs: &Side,
    raid: &[Foe],
    horizon: i32,
    (heals, swings): (bool, bool),
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
            engine::strike_at(h.f.attack, 0, range) * i32::from(swings),
            engine::strike_at(0, h.f.ranged, range),
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
    let breachers: Vec<Foe> = (raid.iter())
        .filter(|h| h.f.work > 0 || h.f.harmless)
        .cloned()
        .collect();

    let focus: Option<Foe> = raid
        .iter()
        .min_by_key(|h| fighter::kill_rank(&h.f))
        .cloned();

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

    let healed = focus.as_ref().map_or(0, &heal_on);
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
        tough_healing: (tough.iter())
            .map(|h| fighter::damage_for(&h.f, heal_on(h)))
            .max()
            .unwrap_or(healed),
        shield: striking.iter().map(|h| rampart_over(theirs, &h.f)).sum(),
        tough: tough
            .iter()
            .map(|h| fighter::damage_for(&h.f, fighter::tough_hits(&h.f)))
            .sum(),
        hits: striking
            .iter()
            .map(|h| h.f.hits - fighter::tough_hits(&h.f))
            .sum(),
        whole: (striking.iter())
            .map(|h| {
                let full = engine::PART_HITS * h.f.body.len() as i32;
                full - engine::tough_hits(&h.f.body, &h.f.boosts, full)
            })
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
        swings,
    }
}

fn prices(
    terrain: Terrain,
    theirs: &Side,
    raid: &[Foe],
    horizon: i32,
    range_of: &dyn Fn(&Foe) -> i32,
) -> Vec<Price> {
    let price = |picks| price_as(terrain, theirs, raid, horizon, picks, range_of);
    let fighting_healer = raid.iter().any(|h| h.f.heal > 0 && fighter::strikes(&h.f));
    let swinger = raid.iter().any(|h| h.f.attack > 0);

    // Whether to swing into our strike-back is the raid's pick: priced both ways.
    [true, false]
        .into_iter()
        .flat_map(|swings| [(false, swings), (true, swings)])
        .filter(|&(heals, swings)| (!heals || fighting_healer) && (swings || swinger))
        .map(price)
        .collect()
}

fn out_heals(price: &Price, our_damage: i32) -> bool {
    our_damage > price.healing && (price.tough == 0 || our_damage > price.tough_healing)
}

#[derive(Clone, Copy, Debug, PartialEq)]
struct Acts {
    deals: i32,
    heals: i32,
    swing: i32,
}

#[derive(Clone, Copy, Debug)]
struct Focused<'a> {
    /// Its `Acts` by its live parts: the engine breaks them head first.
    by_live: &'a [Acts],
    hits: i32,
    full: i32,
    cost: i32,
    /// What its own swing is struck back a tick, focused or not.
    back: i32,
    /// The tick from which the raid can strike it.
    lands: f64,
    /// The tick from which it strikes and heals: struck in place, it may still walk to its reach.
    acts: f64,
}

impl Focused<'_> {
    fn live(&self, left: f64) -> usize {
        ((left / f64::from(engine::PART_HITS)).ceil() as usize).min(self.by_live.len() - 1)
    }

    fn at(&self, left: f64) -> Acts {
        self.by_live[self.live(left)]
    }

    fn weakens(&self, left: f64) -> f64 {
        let now = self.live(left);
        let weaker = self.by_live[..now]
            .iter()
            .rposition(|a| *a != self.by_live[now]);
        weaker.map_or(0.0, |k| f64::from(engine::PART_HITS * k as i32))
    }

    fn taken(&self, left: f64) -> i32 {
        self.back * self.at(left).swing / self.at(f64::from(self.hits)).swing.max(1)
    }
}

#[derive(Clone, Copy, Debug)]
struct Fight {
    won: bool,
    /// None where neither side ends the other.
    ticks: Option<f64>,
    losses: Losses,
}

const HITS: usize = 2;

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
            rest.push(if rate < 0.0 && i == HITS {
                (layer - left * rate).min(f64::from(price.whole).max(layer))
            } else if rate <= 0.0 {
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
/// kills ours one at a time, fewest hits first; ours fight with those landed.
fn focused_against(
    price: &Price,
    (beside, struck): (i32, i32),
    bodies: &[Focused<'_>],
    (our_shield, broken): (i32, i32),
    (towers, approach): (i32, f64),
) -> Fight {
    let whole: Vec<(Focused, f64)> = bodies.iter().map(|b| (*b, f64::from(b.hits))).collect();
    let sum = |xs: &[(Focused, f64)], of: fn(Acts) -> i32| -> i64 {
        xs.iter().map(|(b, left)| i64::from(of(b.at(*left)))).sum()
    };
    let dealt = |acting: &[(Focused, f64)], alive: &[(Focused, f64)]| -> i32 {
        let swing = |a: Acts| a.swing;
        let struck = i64::from(struck) * sum(alive, swing) / sum(&whole, swing).max(1);
        (i64::from(beside) + struck + sum(acting, |a| a.deals)) as i32
    };

    let total = price.damage + bodies.iter().map(|b| b.back).sum::<i32>();

    let mut price = price.clone();
    if price.shield + price.tough + price.hits == 0 {
        price.hits = price.razing;
    }

    let ends = |won: bool, ticks: Option<f64>, dead: i32, standing: &[(Focused, f64)]| -> Fight {
        Fight {
            won,
            ticks,
            losses: Losses {
                dead,
                hits: bodies.iter().map(|b| b.hits).sum::<i32>()
                    - standing.iter().map(|(_, left)| *left).sum::<f64>() as i32,
                energy: bodies.iter().map(|b| b.cost).sum::<i32>()
                    - (standing.iter())
                        .map(|(b, left)| {
                            f64::from(b.cost) * (f64::from(b.full - b.hits) + left)
                                / f64::from(b.full)
                        })
                        .sum::<f64>() as i32,
            },
        }
    };

    let fight = |mut layers: Vec<f64>| -> Fight {
        let mut alive = whole.clone();
        alive.sort_by_key(|(b, _)| b.hits);
        let (mut shield, mut elapsed, mut dead) = (f64::from(our_shield), 0.0, 0);

        loop {
            if alive.is_empty() && !whole.is_empty() {
                return ends(false, Some(elapsed), dead, &[]);
            }
            let landed: Vec<(Focused, f64)> = (alive.iter())
                .filter(|(b, _)| b.lands <= elapsed)
                .copied()
                .collect();
            let acting: Vec<(Focused, f64)> = (landed.iter())
                .filter(|(b, _)| b.acts <= elapsed)
                .copied()
                .collect();
            let next = (alive.iter().flat_map(|(b, _)| [b.lands, b.acts]))
                .filter(|lands| *lands > elapsed)
                .reduce(f64::min);
            let gap = next.map_or(f64::INFINITY, |next| next - elapsed);

            let walled = shield > 0.0;
            let focus = (alive.iter().position(|(b, _)| b.lands <= elapsed)).filter(|_| !walled);
            let (rate, layer) = match focus {
                None if walled => {
                    let rate = price.damage
                        + broken
                        + landed.iter().map(|(b, left)| b.taken(*left)).sum::<i32>();
                    (f64::from(rate), shield)
                }
                Some(at) => {
                    let heals = sum(&acting, |a| a.heals) as i32;
                    (
                        f64::from(price.damage + alive[at].0.taken(alive[at].1) - heals),
                        alive[at].1,
                    )
                }
                None => (0.0, 0.0),
            };
            let span = if rate <= 0.0 {
                f64::INFINITY
            } else {
                layer / rate
            };
            let weakens = match focus {
                Some(at) if rate > 0.0 => (alive[at].1 - alive[at].0.weakens(alive[at].1)) / rate,
                _ => f64::INFINITY,
            };
            let window = span.min(gap).min(weakens);
            let after = |t: f64| -> Vec<(Focused, f64)> {
                (alive.iter().enumerate())
                    .map(|(i, (b, left))| match b.lands <= elapsed {
                        true if Some(i) == focus => (*b, left - t * rate.max(0.0)),
                        true => (*b, (left - f64::from(b.taken(*left)) * t).max(0.0)),
                        false => (*b, *left),
                    })
                    .collect()
            };

            match spend(&price, dealt(&acting, &alive), window, &layers) {
                Ok(t) if walled => return ends(true, Some(elapsed + t), dead, &alive),
                Ok(t) => return ends(true, Some(elapsed + t), dead, &after(t)),
                Err(_) if window.is_infinite() => return ends(false, None, dead, &alive),
                Err(rest) => {
                    layers = rest;
                    let falls = span <= window;
                    alive = after(window);

                    if walled {
                        shield = if falls { 0.0 } else { shield - window * rate };
                    } else if let Some(at) = focus.filter(|_| falls) {
                        alive.remove(at);
                        dead += 1;
                    }

                    elapsed = match next.filter(|next| !falls && *next - elapsed <= window) {
                        Some(next) => next,
                        None => elapsed + window,
                    };
                }
            }
        }
    };

    let fight = match spend(
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
        Err(layers) => fight(layers),
    };

    if total == 0 {
        Fight {
            won: !price.breaches || out_heals(&price, dealt(&whole, &whole)),
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
            let gait = fighter::gait_of(&f.body, &f.boosts, hits, f.load);

            if live <= 0
                || power(Part::Attack) <= 0
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
            || engine::strike_at(hostile.attack, 0, engine::MELEE_RANGE)
                >= engine::strike_at(0, hostile.ranged, engine::RANGED_RANGE))
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

            let furthest = ranges.iter().copied().max().unwrap_or(engine::MELEE_RANGE);
            let nearest = ranges.iter().copied().min().unwrap_or(engine::MELEE_RANGE);
            (engine::MELEE_RANGE.max(furthest), nearest)
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
        Some((_, ranges)) => ranges.iter().copied().min(),
        None => (fighters.iter())
            .map(|f| standoff(terrain, raid, f, hostile))
            .min(),
    }
    .unwrap_or(engine::MELEE_RANGE)
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
                engine::strike_at(h.f.attack, 0, engine::MELEE_RANGE),
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
            let least = (fighters.iter().zip(ranges))
                .filter(|(_, range)| **range <= engine::MELEE_RANGE)
                .map(|(f, _)| f.strikes_back)
                .min();

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
                .and_then(|at| ours.ramparts.get(at).map(|hits| (*at, *hits)))
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
    (price.dismantlers.iter())
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

pub fn spawned(parts: impl IntoIterator<Item = usize>, spawns: i32) -> Vec<i32> {
    spawned_after(&vec![0; spawns.max(0) as usize], parts)
}

/// The tick each body of `parts` parts stands spawned, in ask order, the
/// first spawn free making the next, each spawn free at its `free` (none read as one free now).
pub fn spawned_after(free: &[i32], parts: impl IntoIterator<Item = usize>) -> Vec<i32> {
    let mut free = if free.is_empty() {
        vec![0]
    } else {
        free.to_vec()
    };
    (parts.into_iter())
        .map(|n| {
            free.iter_mut().min().map_or(0, |next| {
                *next += engine::SPAWN_TICKS_PER_PART * n as i32;
                *next
            })
        })
        .collect()
}

/// A set that walks in `together`, or forms, gathers first, so its casts land as one, with the last.
fn walks_in(terrain: Terrain, side: &Side, bodies: &[Fighter], raid: &[Fighter]) -> Vec<i32> {
    let cast = |f: &Fighter| f.pos.is_none() && side.cast_walk.is_some();
    let casts = bodies.iter().filter(|f| cast(f));
    let mut queued = spawned(casts.map(|f| f.body.len()), side.spawns).into_iter();
    let walks: Vec<i32> = (bodies.iter())
        .map(|f| {
            let walk = walk_in(terrain, side.cast_walk, f, raid);
            match cast(f).then(|| queued.next()).flatten() {
                Some(made) => walk + made - engine::SPAWN_TICKS_PER_PART * f.body.len() as i32,
                None => walk,
            }
        })
        .collect();
    let gathered = (bodies.iter().zip(&walks))
        .filter(|(f, _)| cast(f))
        .map(|(_, w)| *w)
        .max();
    (bodies.iter().zip(walks))
        .map(|(f, w)| match gathered {
            Some(last) if side.lands_as_one() && cast(f) => last,
            _ => w,
        })
        .collect()
}

fn walk_in(terrain: Terrain, cast_walk: Option<i32>, f: &Fighter, raid: &[Fighter]) -> i32 {
    let step = |weight| step_ticks(f.gait, weight).unwrap_or(CAST_WALK);
    f.lab_wait
        + match (&f.pos, cast_walk) {
            (None, None) => CAST_WALK,
            (None, Some(tiles)) => {
                engine::SPAWN_TICKS_PER_PART * f.body.len() as i32
                    + tiles * step(engine::PLAIN_WEIGHT)
            }
            (Some(at), _) => (raid.iter())
                .filter_map(|h| Some(at.crow_flies(h.pos.as_ref()?)))
                .min()
                .map_or(0, |nearest| {
                    (nearest - f.reach).max(0) * step(terrain.weight)
                }),
        }
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

    (fighters.iter())
        .flat_map(|f| raid.iter().filter_map(|h| closing(f, h)))
        .reduce(f64::min)
        .unwrap_or(0.0)
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
                let here = range_to(&t.pos, &h.f).unwrap_or(engine::ROOM_SIDE);

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
        loaded.len() as i32 * engine::tower_attack_at(engine::ROOM_SIDE)
    } else {
        striking.iter().map(|h| on(h)).min().unwrap_or(0)
    }
}

/// With none of ours fielded the raid strikes our nearest keep, its rampart
/// first, from the tick it walks in (Consequence pricing, §6.3).
fn razes(ours: &Side, theirs: &Side, terrain: Terrain, price: &Price, ticks: i32) -> bool {
    let strike = price.damage + (price.dismantlers.iter()).map(|(_, d)| *d).sum::<i32>();
    let walk = |h: &Fighter, at: &Pos| {
        let gap = (h.pos.as_ref()?.crow_flies(at) - h.reach).max(0);
        Some(gap * step_ticks(h.gait, terrain.weight)?)
    };
    let nearest = (ours.keeps.iter())
        .filter_map(|(at, hits)| {
            let first = theirs.bodies.iter().filter_map(|h| walk(h, at)).min()?;
            Some((first, hits + ours.ramparts.get(at).copied().unwrap_or(0)))
        })
        .min_by_key(|(first, _)| *first);
    nearest.is_some_and(|(first, cover)| {
        i64::from(strike) * i64::from((ticks - first).max(0)) >= i64::from(cover)
    })
}

/// Each loaded tower spends its energy a shot a tick; no hauler refill is counted.
fn towers_last(towers: &[Tower], ticks: i32) -> bool {
    let shots = i64::from(ticks) * i64::from(engine::TOWER_ENERGY_COST);
    (towers.iter())
        .filter(|t| t.energy >= engine::TOWER_ENERGY_COST)
        .all(|t| shots <= i64::from(t.energy))
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

    let walks = walks_in(terrain, ours, &fighters, &theirs.bodies);
    let first = walks.iter().copied().min().unwrap_or(0);
    let last = walks.iter().copied().max().unwrap_or(0);
    let struck: Vec<bool> = (fighters.iter())
        .map(|f| {
            (theirs.bodies.iter()).any(|h| match (&f.pos, &h.pos) {
                (Some(at), Some(from)) => at.crow_flies(from) <= h.reach,
                _ => false,
            })
        })
        .collect();
    // A body in the raid's reach is struck from now, though it reaches its own range later.
    let in_reach = struck.contains(&true);
    let waits = if ours.lands_as_one() && !in_reach {
        last - first
    } else {
        0
    };
    let since = if in_reach { 0 } else { first };
    let joins = if in_reach { last } else { 0 };
    let lands = |walk: i32, struck: bool| match (struck, ours.lands_as_one()) {
        (true, _) => 0,
        (false, false) => walk - since,
        (false, true) => joins,
    };
    let acts = |walk: i32, struck: bool| match (struck, ours.lands_as_one()) {
        (true, _) | (false, false) => walk - since,
        (false, true) => joins,
    };

    let by_live: Vec<Vec<Acts>> = (pairs.iter())
        .map(|(f, (range, heal_range))| {
            (0..=f.body.len() as i32)
                .map(|live| {
                    fighter::of_parts(&f.body, &f.boosts, engine::PART_HITS * live, None, f.load)
                })
                .map(|at| Acts {
                    deals: fighter::damage_at(&at, *range),
                    heals: fighter::heal_at(&at, *heal_range),
                    swing: at.strikes_back.min(f.strikes_back),
                })
                .collect()
        })
        .collect();
    let bodies: Vec<Focused> = (pairs.iter().zip(&by_live).zip(walks.iter().zip(&struck)))
        .map(|(((f, _), by_live), (walk, struck))| Focused {
            by_live,
            hits: f.hits,
            full: engine::PART_HITS * f.body.len() as i32,
            cost: engine::body_cost(&f.body),
            back: 0,
            lands: f64::from(lands(*walk, *struck)),
            acts: f64::from(acts(*walk, *struck)),
        })
        .collect();

    let dealt: i32 = bodies.iter().map(|b| b.at(f64::from(b.hits)).deals).sum();
    let dealt = towers + dealt;
    let approach = approach_of(terrain, stance, &fighters, &theirs.bodies);

    let settle = |horizon: i32, price: &Price| -> (Outcome, i32) {
        let struck = if price.swings {
            struck_back(price, dealt, back.struck, back.swingers)
        } else {
            0
        };
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
            (towers, struck),
            &bodies,
            shield,
            (walking_in, approach),
        );

        let ticks = waits + fight.ticks.map_or(LONGEST, |t| t.ceil() as i32);

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
        } else if fight.won
            && (!fighters.is_empty()
                || towers_last(&ours.towers, ticks) && !razes(ours, theirs, terrain, price, ticks))
        {
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

    // Every heal that reaches the raid before our last body lands counts.
    let outcomes = widen(last.min(LONGEST), 3);

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
    let striking = ours.bodies.iter().filter(|f| fighter::strikes(f));
    let reach = striking.map(|f| f.reach).min().unwrap_or(0);

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
        Outcome::Win(0, Losses::default())
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

    engine::heal_at(beside.iter().map(|f| f.heal).sum(), engine::MELEE_RANGE)
        + engine::heal_at(apart.iter().map(|f| f.heal).sum(), engine::RANGED_RANGE)
}

/// What towers deal a body at `at`, the one strike a breach is gated and
/// re-priced by.
pub fn tower_strike(towers: &[Pos], at: &Pos) -> i32 {
    towers
        .iter()
        .map(|p| p.range(at).map_or(0, engine::tower_attack_at))
        .sum()
}

/// What his armed bodies deal a body at `at` next tick, from where they stand
/// (owner, 2026-10-21).
pub fn guard_strike(guards: &[Striker], at: &Pos) -> i32 {
    guards.iter().map(|g| g.on(at, false)).sum()
}

pub fn struck(towers: &[Pos], guards: &[Striker], at: &Pos) -> i32 {
    tower_strike(towers, at) + guard_strike(guards, at)
}

/// The simple rule; a fire priced in ticks replaces it. A medic heals one body a
/// tick, so heal that cannot reach a worker covers nothing: each needs its own.
pub fn crowded(set: &[Fighter], abreast: i32, seated: i32, struck: i32) -> Option<i32> {
    let n = set.len() as i32;
    let abreast = abreast.clamp(1, n.max(1));
    let front = set.iter().filter(|f| f.work > 0).count().max(1) as i32;
    let healers = set.iter().filter(|f| f.heal > 0).count() as i32;

    (healers >= front && struck <= pooled_heal(seated, set))
        .then_some((n + abreast - 1) / abreast - 1)
}

/// A source is a tile, the walk onto it and how many its stretch lands
/// abreast. A landing and a barrier's work tile hold only the medics seated
/// there; the barriers come back in order, each with its work tile.
#[allow(clippy::too_many_arguments)]
pub fn breach_plan(
    ground: &Ground,
    gait: FatigueFactor,
    towers: &[Pos],
    guards: &[Striker],
    set: &[Fighter],
    seated: i32,
    seats: impl Fn(Tile) -> i32,
    rate: i32,
    sources: &[(Tile, i32, i32)],
    goals: &[Tile],
) -> Option<(Vec<(Tile, Tile)>, i32)> {
    let struck = |t: Tile| struck(towers, guards, &tiles::to_pos(ground.room, t));
    let n = set.len() as i32;
    let abreast: BTreeMap<Tile, i32> = sources.iter().map(|(t, _, w)| (*t, *w)).collect();
    let goal: BTreeSet<Tile> = goals.iter().copied().collect();

    let cost = |from: Tile, to: Tile| -> Option<i32> {
        if tiles::on_edge(to) {
            return None;
        }
        let step = step_ticks(gait, grid::value(ground.weights[to])?)?;
        let queued = match abreast.get(&from).filter(|_| tiles::on_edge(from)) {
            Some(w) => crowded(set, *w, seats(to), struck(to))?,
            None => crowded(set, n, seated, struck(to))?,
        };

        match ground.barriers.get(&to) {
            Some(hits) if rate > 0 => {
                crowded(set, n, seats(from), struck(from))?;
                Some(step + queued + (hits + rate - 1) / rate)
            }
            Some(_) => None,
            None => Some(step + queued),
        }
    };

    let mut dist = [i32::MAX; tiles::COUNT];
    let mut prev: [Option<Tile>; tiles::COUNT] = [None; tiles::COUNT];

    for &(t, walk, _) in sources {
        dist[t] = dist[t].min(walk);
    }

    let mut queue: BTreeSet<(i32, Tile)> = sources.iter().map(|(t, _, _)| (dist[*t], *t)).collect();

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
            match cost(t, n) {
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

        (broken, dist[last] - dist[walked[0]])
    })
}

/// The cheapest cast can be a marginal win that loses its bodies, so lost hits
/// and ticks are priced too. A win is monotone in one body's count, so its
/// fewest is bisected, then grown while one more saves what it costs.
///
/// The bodies to cast, not one: `n` whole copies of the cheapest, and whether
/// they are boosted from `boosts`: a boosted cast lands later by its lab wait,
/// so it is taken only where it wins cheaper than the bare one (owner).
#[allow(clippy::too_many_arguments)]
pub fn cheapest_win(
    stance: Stance,
    ours: &Side,
    bodies: &[Body],
    copies_of: impl Fn(&Body) -> i32,
    tick_worth: i32,
    theirs: &Side,
    terrain: Terrain,
    boosts: &boost::Stock,
) -> Option<(Vec<Body>, bool)> {
    let cost_from = |body: &Body, n: i32, boosts: &boost::Stock| -> Option<i32> {
        let side = Side {
            bodies: ours
                .bodies
                .iter()
                .cloned()
                .chain(fighter::cast_boosted(
                    std::iter::repeat_n(&body[..], n.max(0) as usize),
                    boosts,
                ))
                .collect(),
            ..ours.clone()
        };

        // An outcome's ticks run from the first landing; the objective is lost
        // the ticks before it too, a boosted set's lab wait among them.
        let lands = (walks_in(terrain, &side, &side.bodies, &theirs.bodies).into_iter())
            .min()
            .unwrap_or(0);
        match outcome(stance, &side, theirs, terrain) {
            Outcome::Win(ticks, losses) => {
                Some(engine::body_cost(body) * n + losses.energy + tick_worth * (lands + ticks))
            }
            Outcome::Kite(ticks) => {
                Some(engine::body_cost(body) * n + tick_worth * (lands + ticks))
            }
            Outcome::Lose => None,
        }
    };

    let cheapest = |body: &Body, stock: &boost::Stock| -> Option<(Vec<Body>, i32)> {
        let most = copies_of(body);
        let cost = |body: &Body, n: i32| cost_from(body, n, stock);

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

    let bare = boost::Stock::default();
    let priced = [(&bare, false), (boosts, true)]
        .into_iter()
        .flat_map(|(stock, boosted)| {
            (bodies.iter()).filter_map(move |body| Some((cheapest(body, stock)?, boosted)))
        });

    priced
        .min_by_key(|((set, spent), boosted)| {
            (
                *spent,
                set.iter().map(Vec::len).sum::<usize>(),
                set.len(),
                *boosted,
            )
        })
        .map(|((set, _), boosted)| (set, boosted))
}

/// What a body standing on each tile takes next tick: the charged towers'
/// falloff and each striker's reach, one more where it can step. Built once a
/// room and tick, shared by every body there.
pub fn damage_map(room: RoomName, towers: &[Pos], foes: &[Fighter]) -> Table<i32> {
    let mut map = tiles::table(0);
    for t in tiles::all() {
        let at = tiles::to_pos(room, t);
        let step = |f: &Fighter| i32::from(f.gait.moves > 0);
        map[t] = tower_strike(towers, &at)
            + (foes.iter())
                .filter_map(|f| Some(fighter::damage_at(f, f.pos?.range(&at)? - step(f))))
                .sum::<i32>();
    }
    map
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_cast_lands_on_the_first_spawn_free() {
        assert_eq!(spawned_after(&[0, 15], [10, 10, 10]), vec![30, 45, 60]);
        assert_eq!(
            spawned_after(&[], [10, 10]),
            vec![30, 60],
            "no spawn reads as one free"
        );
        assert_eq!(spawned([10, 10, 10], 3), vec![30, 30, 30]);
    }
    use proptest::collection::vec;
    use proptest::prelude::*;
    use proptest::sample::select;

    const ROOM: RoomName = RoomName::W1N1;

    const CASES: u32 = 1000;

    fn fighter() -> impl Strategy<Value = Fighter> {
        use Part::{Attack, Heal, Move, RangedAttack, Tough, Work};
        let part = select(vec![Attack, RangedAttack, Heal, Move, Tough, Work]);
        let at = (1..=48, 1..=48).prop_map(|(x, y)| Some(Pos::new(ROOM, x, y)));
        (vec(part, 1..=20), 0..=3usize, at).prop_map(|(body, tier, pos)| {
            let boosts: Vec<_> = (body.iter())
                .map(|p| fabot_base::testing::boosted(*p, tier))
                .collect();
            fighter::of_parts(&body, &boosts, 100 * body.len() as i32, pos, 0)
        })
    }

    fn side() -> impl Strategy<Value = Side> {
        (vec(fighter(), 1..=4), 0..=2).prop_map(|(bodies, towers)| Side {
            bodies,
            ramparts: BTreeMap::new(),
            towers: (0..towers)
                .map(|k| Tower {
                    pos: Pos::new(ROOM, 10 + k, 10),
                    energy: 1000,
                })
                .collect(),
            ..Side::default()
        })
    }

    fn terrain() -> impl Strategy<Value = Terrain> {
        (select(vec![1, 2, 10]), 0..=40).prop_map(|(weight, run_off)| Terrain { weight, run_off })
    }

    fn bare(s: &Side) -> Side {
        Side {
            towers: Vec::new(),
            ..s.clone()
        }
    }

    fn healing(healing: i32, whole: i32) -> Price {
        Price {
            damage: 0,
            healing,
            tough_healing: healing,
            shield: 0,
            tough: 0,
            hits: 0,
            whole,
            breaches: false,
            dismantlers: Vec::new(),
            razing: 0,
            swings: true,
        }
    }

    #[test]
    fn a_raid_heals_back_what_our_strike_falls_short_of_up_to_whole_and_never_below_it() {
        let hits = |layer: f64| vec![0.0, 0.0, layer];
        let left = |price: &Price, dealt: i32, span: f64, layer: f64| {
            spend(price, dealt, span, &hits(layer)).unwrap_err()[HITS]
        };

        assert_eq!(left(&healing(300, 5000), 200, 10.0, 350.0), 1350.0);
        assert_eq!(left(&healing(300, 1000), 200, 10.0, 350.0), 1000.0);
        assert_eq!(left(&healing(300, 5000), 100, f64::INFINITY, 350.0), 5000.0);
        // Struck at its heal for ever: it holds what it has.
        assert_eq!(left(&healing(300, 5000), 300, f64::INFINITY, 350.0), 350.0);
        // A dismantle-only raid's razing layer stands with no striker's whole.
        assert_eq!(left(&healing(0, 0), 0, f64::INFINITY, 4000.0), 4000.0);
        assert_eq!(left(&healing(300, 0), 0, 10.0, 4000.0), 4000.0);
    }

    #[test]
    fn each_worker_of_a_struck_tile_needs_a_medic_of_its_own() {
        let dismantler = fighter::cast(&[Part::Work; 25]);
        let medic = fighter::cast(&[Part::Heal; 25]);
        // One medic seated beside a worker heals it as much as a tower strikes it,
        // so pooling alone would price the pair in.
        let struck = engine::heal_at(25, engine::MELEE_RANGE);

        let one = [dismantler.clone(), dismantler.clone(), medic.clone()];
        let two = [dismantler.clone(), dismantler, medic.clone(), medic];

        assert_eq!(crowded(&one, 2, 2, struck), None);
        assert!(crowded(&two, 2, 2, struck).is_some());
    }

    proptest! {
        #![proptest_config(fabot_base::testing::config(CASES))]

        #[test]
        fn towers_alone_win_against_a_raid_with_no_heal_they_outlast(
            theirs in side(),
            ground in terrain(),
            stance in select(vec![Stance::Hold, Stance::Standoff]),
            energy in [prop_oneof![1 => Just(1000), 3 => 0..=1000], prop_oneof![1 => Just(1000), 3 => 0..=1000]],
        ) {
            let unhealed = |f: &Fighter| {
                let body: Vec<Part> = f.body.iter().copied().filter(|p| *p != Part::Heal).collect();
                let hits = engine::PART_HITS * body.len() as i32;
                (!body.is_empty()).then(|| fighter::of_parts(&body, &[], hits, f.pos, f.load))
            };
            let raid: Vec<Fighter> = theirs.bodies.iter().filter_map(unhealed).collect();
            prop_assume!(!raid.is_empty());
            let towers = Side {
                towers: (energy.iter().zip([25, 30]))
                    .map(|(energy, x)| Tower { pos: Pos::new(ROOM, x, 25), energy: *energy })
                    .collect(),
                ..Side::default()
            };
            let raid = Side { bodies: raid, ..Side::default() };

            match outcome(stance, &towers, &raid, ground) {
                Outcome::Win(ticks, _) => {
                    let loaded = energy.iter().filter(|e| **e >= engine::TOWER_ENERGY_COST);
                    for e in loaded {
                        prop_assert!(ticks * engine::TOWER_ENERGY_COST <= *e, "dry by t{ticks}");
                    }
                }
                _ => prop_assert!(energy != [1000, 1000], "full towers outlast every raid drawn"),
            }
        }

        #[test]
        fn a_fight_won_leaves_one_of_our_bodies_standing(
            ours in side(),
            theirs in side(),
            ground in terrain(),
        ) {
            if let Outcome::Win(_, losses) =
                outcome(Stance::Standoff, &ours, &bare(&theirs), ground)
            {
                prop_assert!(
                    losses.dead < ours.bodies.len() as i32,
                    "{} of {} bodies dead",
                    losses.dead,
                    ours.bodies.len()
                );
            }
        }

        #[test]
        fn a_way_in_found_with_some_heal_is_found_with_more_and_no_slower(
            hits in vec(prop_oneof![1 => Just(1), 1 => Just(300_000), 6 => 1..=300_000], 48),
            heal in 0..=50usize,
            (x, y, wide) in (1..=48, 1..=48, 1..=8),
        ) {
            let tile = tiles::index;
            let ground = Ground {
                room: ROOM,
                weights: tiles::table(grid::weight(2)),
                barriers: hits
                    .iter()
                    .enumerate()
                    .map(|(i, h)| (tile(25, i as i32 + 1), *h))
                    .collect(),
            };

            let sources: Vec<(Tile, i32, i32)> = (1..=48).map(|y| (tile(49, y), 0, wide)).collect();

            let plan = |heal: usize| {
                let set = [
                    fighter::cast(&[Part::Work]),
                    fighter::cast(&vec![Part::Heal; heal]),
                ];
                breach_plan(
                    &ground,
                    FatigueFactor {
                        weight: 1,
                        moves: 1,
                    },
                    &[Pos::new(ROOM, x, y)],
                    &[],
                    &set,
                    2,
                    |t| tiles::x_of(t) % 3,
                    1000,
                    &sources,
                    &[tile(5, 25)],
                )
            };

            let holds = match (plan(heal), plan(heal + 8)) {
                (Some((_, ticks)), Some((_, fewer))) => fewer <= ticks,
                (Some(_), None) => false,
                _ => true,
            };

            prop_assert!(holds, "heal {} from {},{}", heal, x, y);
        }
    }

    #[test]
    fn a_together_set_pays_for_its_bodies_in_the_raids_reach_while_the_rest_walk_in() {
        use Part::{Attack, Heal, Move, RangedAttack};
        let body = |parts: &[Part], x| {
            let hits = 100 * parts.len() as i32;
            fighter::of_parts(parts, &[], hits, Some(Pos::new(ROOM, x, 25)), 0)
        };
        let ranger = |x| body(&[RangedAttack, RangedAttack, Heal, Move, Move, Move], x);
        let medic = |x| body(&[Heal, Heal, Move, Move], x);
        let gunner = body(&[[RangedAttack; 6].as_slice(), &[Move; 6]].concat(), 20);
        let swinger = body(&[[Attack; 6].as_slice(), &[Move; 6]].concat(), 20);
        let hits = |near: &dyn Fn(i32) -> Fighter, x: i32, raider: &Fighter| {
            let mut bodies = vec![near(x), near(x)];
            bodies.extend((0..4).map(|_| ranger(12)));
            bodies.push(body(
                &[Attack, Attack, Attack, Heal, Move, Move, Move, Move],
                5,
            ));
            let ours = Side {
                bodies,
                together: true,
                ..Side::default()
            };
            let theirs = Side {
                bodies: vec![raider.clone()],
                ..Side::default()
            };
            let ground = Terrain {
                weight: 2,
                run_off: 40,
            };
            outcome(Stance::Standoff, &ours, &theirs, ground)
                .losses()
                .hits
        };

        assert!(hits(&ranger, 17, &gunner) > hits(&ranger, 12, &gunner));
        assert!(hits(&medic, 18, &gunner) > hits(&medic, 12, &gunner));
        assert_eq!(hits(&ranger, 17, &swinger), hits(&ranger, 12, &swinger));
    }

    #[test]
    fn copies_of_a_cast_spend_one_stock_in_turn() {
        use fabot_base::vocabulary::ResourceType::{
            CatalyzedUtriumAcid as T3, UtriumHydride as T1, ZynthiumOxide as Move1,
        };
        let body = [[Part::Attack; 10], [Part::Move; 10]].concat();
        let held = [(T1, 50), (T3, 10), (Move1, 10)].map(|(compound, parts)| boost::Held {
            compound,
            parts,
            in_labs: 0,
        });
        let stock = boost::Stock {
            held: held.to_vec(),
            energy: 40,
        };
        let cast = fighter::cast_boosted([&body[..], &body[..]], &stock);
        assert_eq!((cast[0].attack, cast[0].gait.moves), (40, 20));
        assert_eq!((cast[1].attack, cast[1].gait.moves), (20, 10));
        assert_eq!((cast[0].lab_wait, cast[1].lab_wait), (2 * 85, 3 * 85));
    }
}
