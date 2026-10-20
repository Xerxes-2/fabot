/// What a fight casts and where (`ARCHITECTURE.md` §6.3, §6.5). From v1's
/// Caster: a body goes to the spawn that lands it soonest, a home's own Defend
/// to the home's.
use std::collections::BTreeMap;

use crate::core::engine;
use crate::core::exchange::{self, Fighter, fighter};
use crate::core::vocabulary::{BodyPart, Boost, Resource};
use crate::core::world::{Creep, Kind, Owner, Pos, Room, Spawn, Store, Structure, terrain};

pub type Body = Vec<BodyPart>;

/// v1's guard, ranger and defender blocks.
const GUARD: [BodyPart; 10] = [
    BodyPart::Tough,
    BodyPart::Move,
    BodyPart::Move,
    BodyPart::Move,
    BodyPart::Move,
    BodyPart::Move,
    BodyPart::Attack,
    BodyPart::Attack,
    BodyPart::Attack,
    BodyPart::Heal,
];
const RANGER: [BodyPart; 6] = [
    BodyPart::Move,
    BodyPart::Move,
    BodyPart::Move,
    BodyPart::RangedAttack,
    BodyPart::RangedAttack,
    BodyPart::Heal,
];
const DEFENDER: [BodyPart; 2] = [BodyPart::Move, BodyPart::Attack];

fn blocks() -> [Body; 3] {
    [GUARD.to_vec(), RANGER.to_vec(), DEFENDER.to_vec()]
}

/// v1's `guardCap`: a home's Defend never disengages, and its loss budget,
/// not a yield, bounds what it asks (§6.2).
pub fn home_copies(_: &Body) -> i32 {
    2
}

pub fn copies_within(worth: i32, spawn_ticks: i32, body: &[BodyPart]) -> i32 {
    (worth / engine::body_cost(body))
        .min(spawn_ticks / (engine::SPAWN_TICKS_PER_PART * body.len() as i32))
}

pub fn affordable(capacity: i32) -> Vec<Body> {
    let mut out = Vec::new();

    for block in blocks() {
        let most = (capacity / engine::body_cost(&block))
            .min(engine::MAX_BODY_PARTS as i32 / block.len() as i32);

        for n in 0..most.max(0) as usize {
            let mut body = Vec::new();

            for _ in 0..=n {
                body.extend(block.iter().copied());
            }

            out.push(engine::with_move_last(body));
        }
    }

    out
}

pub fn fighter_of(c: &Creep) -> Fighter {
    let body: Vec<BodyPart> = c.body.iter().map(|p| p.kind).collect();
    let boosts: Vec<Boost> = c.body.iter().map(|p| p.boost).collect();
    let mut f = fighter::of_parts(&body, &boosts, c.hits, Some(c.pos.clone()));
    f.invader = c.owner == Owner::Invader;
    f
}

/// A level-0 Invader core holds its room's controller and strikes nothing: a
/// hostile without a body, which no exchange prices.
pub fn core_as_hostile(core: &Structure) -> Creep {
    Creep {
        id: core.id.clone(),
        name: core.id.clone(),
        owner: core.owner.clone(),
        pos: core.pos.clone(),
        body: Vec::new(),
        hits: core.hits,
        hits_max: core.hits_max,
        ticks_to_live: core.collapse,
        fatigue: 0,
        store: Store::empty(),
    }
}

/// v1's guard on a core: the biggest melee body the bank buys, as a small one
/// dies of age short of its 100,000 hits; none where ours raze `hits` in a life.
pub fn razer(capacity: i32, hits: i32, ours: &[Fighter]) -> Vec<Body> {
    let swing = engine::ATTACK_POWER * ours.iter().map(|f| f.attack).sum::<i32>();

    biggest(capacity, &[BodyPart::Move, BodyPart::Attack])
        .filter(|_| swing * engine::CREEP_LIFETIME < hits)
        .into_iter()
        .collect()
}

/// Taps land a block apart whatever the body, so only more CLAIM a tap
/// shortens the declaim (v1).
pub fn tapper(capacity: i32) -> Option<Body> {
    biggest(capacity, &[BodyPart::Move, BodyPart::Claim])
}

fn biggest(capacity: i32, block: &[BodyPart]) -> Option<Body> {
    let most = (capacity / engine::body_cost(block))
        .min(engine::MAX_BODY_PARTS as i32 / block.len() as i32);

    (most > 0).then(|| engine::with_move_last(block.repeat(most as usize)))
}

pub fn terrain_at(room: &Room, at: &Pos) -> exchange::Terrain {
    let last = engine::ROOM_SIDE - 1;

    exchange::Terrain {
        weight: if terrain::is_swamp(&room.terrain, at.x, at.y) {
            engine::SWAMP_WEIGHT
        } else {
            engine::PLAIN_WEIGHT
        },
        run_off: at.x.max(last - at.x).max(at.y.max(last - at.y)) - 1,
    }
}

pub fn side_in(room: &Room, bodies: Vec<Fighter>) -> exchange::Side {
    let ours = |kind: Kind| -> Vec<&Structure> {
        room.structures
            .iter()
            .filter(|s| s.kind == kind && s.owner.is_ours())
            .collect()
    };

    exchange::Side {
        bodies,
        ramparts: ours(Kind::Rampart)
            .into_iter()
            .map(|s| (s.pos.clone(), s.hits))
            .collect::<BTreeMap<_, _>>(),
        towers: ours(Kind::Tower)
            .into_iter()
            .map(|s| exchange::Tower {
                pos: s.pos.clone(),
                energy: s.store.amount_of(&Resource::Energy),
            })
            .collect(),
    }
}

pub fn wins(
    stance: exchange::Stance,
    ours: &exchange::Side,
    theirs: &exchange::Side,
    terrain: exchange::Terrain,
) -> bool {
    !exchange::outcome(stance, ours, theirs, terrain).is_lose()
}

/// The bodies to ask beside `ours`, the outcome they land and the side they
/// land as; None: no body the spawns buy wins. Which tactic fights them is the
/// kind's to price (§6.3).
pub fn price(
    stance: exchange::Stance,
    ours: &exchange::Side,
    theirs: &exchange::Side,
    terrain: exchange::Terrain,
    capacity: i32,
    copies_of: impl Fn(&Body) -> i32,
    tick_worth: i32,
) -> Option<(Vec<Body>, exchange::Outcome, exchange::Side)> {
    let set: Vec<Body> = if wins(stance, ours, theirs, terrain) {
        Vec::new()
    } else {
        exchange::cheapest_win(
            stance,
            ours,
            &affordable(capacity),
            copies_of,
            tick_worth,
            theirs,
            terrain,
        )?
    };

    let mut landed = ours.clone();
    landed
        .bodies
        .extend(set.iter().map(|body| fighter::cast(body)));

    let outcome = exchange::outcome(stance, &landed, theirs, terrain);

    Some((set, outcome, landed))
}

/// The energy a set spends over `ticks`: its cast, the share of that cast the
/// hits strike off, and the room's worth a tick it stands there.
pub fn spent(set: &[Body], losses: exchange::Losses, ticks: i32, tick_worth: i32) -> i32 {
    let cast: i32 = set.iter().map(|body| engine::body_cost(body)).sum();
    let whole = set
        .iter()
        .map(|body| engine::PART_HITS * body.len() as i32)
        .sum::<i32>()
        .max(1);

    cast + cast * losses.hits.min(whole) / whole + tick_worth * ticks
}

/// No more medics heal one body from beside it than the tiles around it.
const AROUND_ONE: i32 = 8;

fn most(capacity: i32, block: &[BodyPart]) -> Body {
    let n = (capacity / engine::body_cost(block))
        .min(engine::MAX_BODY_PARTS as i32 / block.len() as i32);
    let mut body = Vec::new();

    for _ in 0..n.max(0) {
        body.extend(block.iter().copied());
    }

    engine::with_move_last(body)
}

/// Two dismantlers of the most WORK the bank buys, then the fewest MOVE:HEAL
/// medics `priced` breaks the way in with.
pub fn siege<T>(
    capacity: i32,
    mut priced: impl FnMut(&[Body]) -> Option<T>,
) -> Option<(Vec<Body>, T)> {
    let dismantler = most(capacity, &[BodyPart::Work, BodyPart::Move]);
    let medic = most(capacity, &[BodyPart::Move, BodyPart::Heal]);

    if dismantler.is_empty() {
        None
    } else {
        for n in 0..=AROUND_ONE {
            let mut set = vec![dismantler.clone(), dismantler.clone()];
            set.extend((0..n).map(|_| medic.clone()));

            if let Some(plan) = priced(&set) {
                return Some((set, plan));
            }
        }

        None
    }
}

/// The dismantlers the fielded set lacks of two, then the fewest medics, that
/// `priced` breaks the way in with: a set re-planned onto a harder target or
/// short of a body lost is topped up, not recast.
pub fn reinforce<T>(
    capacity: i32,
    fielded: &[Body],
    mut priced: impl FnMut(&[Body]) -> Option<T>,
) -> Option<(Vec<Body>, T)> {
    let dismantler = most(capacity, &[BodyPart::Work, BodyPart::Move]);
    let medic = most(capacity, &[BodyPart::Move, BodyPart::Heal]);

    if medic.is_empty() || dismantler.is_empty() {
        return None;
    }

    let standing = fielded
        .iter()
        .filter(|b| b.contains(&BodyPart::Work))
        .count();
    let short = 2usize.saturating_sub(standing);

    (0..=AROUND_ONE as usize)
        .filter(|n| short + n > 0)
        .find_map(|n| {
            let mut added = vec![dismantler.clone(); short];
            added.extend(vec![medic.clone(); n]);
            let mut set = fielded.to_vec();
            set.extend(added.iter().cloned());
            priced(&set).map(|plan| (added, plan))
        })
}

/// The fewest bodies whose HEAL holds `strike`, each with the RANGED its share
/// of `worth` buys: the kills pay for the cast (v1's `castBodiesPaid`). A
/// siege's pickets and a sweep's set.
pub fn squad(capacity: i32, strike: i32, worth: i32) -> Vec<Body> {
    // The towers' strike falls on one body; it and one body beside it heal it.
    let heal = ((strike + 2 * engine::HEAL_POWER - 1) / (2 * engine::HEAL_POWER)) as usize;
    let n = if heal > 0 { 2 } else { 1 };
    let body = |ranged: usize| -> Body {
        let mut b = vec![BodyPart::RangedAttack; ranged];
        b.extend(vec![BodyPart::Move; heal + ranged]);
        b.extend(vec![BodyPart::Heal; heal]);
        engine::with_move_last(b)
    };
    let fits = |b: &Body| b.len() <= engine::MAX_BODY_PARTS && engine::body_cost(b) <= capacity;
    let paid = (worth / n as i32 / engine::body_cost(&[BodyPart::RangedAttack, BodyPart::Move]))
        .max(1) as usize;
    (1..=paid)
        .take_while(|r| fits(&body(*r)))
        .last()
        .map_or_else(Vec::new, |r| vec![body(r); n])
}

/// Whether `set` holds `strike` falling on any one body: that body's heal and
/// the most another lends it from beside.
pub fn holds(set: &[Fighter], strike: i32) -> bool {
    strike == 0
        || (0..set.len()).all(|i| {
            let lent = (0..set.len())
                .filter(|j| *j != i)
                .map(|j| set[j].heal)
                .max()
                .unwrap_or(0);
            engine::HEAL_POWER * (set[i].heal + lent) >= strike
        })
}

pub fn kill_order(hostiles: &[Creep]) -> Vec<String> {
    let ranked: Vec<(String, (bool, f64))> = hostiles
        .iter()
        .map(|h| {
            let f = fighter_of(h);
            let damage = engine::ATTACK_POWER * f.attack + engine::RANGED_ATTACK_POWER * f.ranged;
            (
                h.id.clone(),
                (f.heal == 0, -(f64::from(damage) / f64::from(h.hits.max(1)))),
            )
        })
        .collect();

    let mut ranked = ranked;
    ranked.sort_by(|(a_id, a), (b_id, b)| {
        a.0.cmp(&b.0)
            .then(a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal))
            .then(a_id.cmp(b_id))
    });
    ranked.into_iter().map(|(id, _)| id).collect()
}

/// The colony that lands a body soonest of those whose bank buys it: its
/// queue, then the walk (v1's Caster). None: no colony pays for it.
pub fn colony_for(
    colonies: &[&Room],
    spawns: &[Spawn],
    queued: &BTreeMap<String, i32>,
    walk: impl Fn(&str) -> Option<i32>,
    body: &[BodyPart],
) -> Option<String> {
    let queue = |room: &str| -> i64 {
        let idle = spawns
            .iter()
            .filter(|s| s.pos.room == room)
            .map(|s| s.spawning.as_ref().map_or(0, |o| o.remaining))
            .fold(i32::MAX, i32::min);

        i64::from(idle) + i64::from(queued.get(room).copied().unwrap_or(0))
    };

    let ranked: Vec<(i64, String)> = colonies
        .iter()
        .filter(|r| r.energy_capacity >= engine::body_cost(body))
        .filter_map(|r| walk(&r.name).map(|w| (i64::from(w) + queue(&r.name), r.name.clone())))
        .collect();

    let mut ranked = ranked;
    ranked.sort();
    ranked.into_iter().next().map(|(_, room)| room)
}
