/// What a fight casts and where (`ARCHITECTURE.md` §6.3, §6.5). From v1's
/// Caster: a body goes to the spawn that lands it soonest, a home's own Defend
/// to the home's.
use std::collections::BTreeMap;

use fabot_base::declarations::Declarations;
use fabot_base::engine;
use fabot_base::vocabulary::{Compound, Part, Resource, ResourceType};
use fabot_base::world::{
    Creep, Id, Kind, Owner, Pos, Room, RoomName, Spawn, Store, Structure, World, terrain,
};
use fabot_exchange::{self as exchange, Fighter, fighter};

pub type Body = Vec<Part>;

/// v1's guard, ranger and defender blocks.
const GUARD: [Part; 10] = [
    Part::Tough,
    Part::Move,
    Part::Move,
    Part::Move,
    Part::Move,
    Part::Move,
    Part::Attack,
    Part::Attack,
    Part::Attack,
    Part::Heal,
];
const RANGER: [Part; 6] = [
    Part::Move,
    Part::Move,
    Part::Move,
    Part::RangedAttack,
    Part::RangedAttack,
    Part::Heal,
];
const DEFENDER: [Part; 2] = [Part::Move, Part::Attack];

/// v1's `guardCap`: a home's Defend never disengages, and its loss budget,
/// not a yield, bounds what it asks (§6.2).
pub const HOME_COPIES: i32 = 2;

pub fn copies_within(worth: i32, spawn_ticks: i32, body: &[Part]) -> i32 {
    (worth / engine::body_cost(body))
        .min(spawn_ticks / (engine::SPAWN_TICKS_PER_PART * body.len() as i32))
}

fn blocks_fit(capacity: i32, block: &[Part]) -> usize {
    let n = (capacity / engine::body_cost(block))
        .min(engine::MAX_BODY_PARTS as i32 / block.len() as i32);
    n.max(0) as usize
}

pub fn affordable(capacity: i32) -> Vec<Body> {
    [&GUARD[..], &RANGER, &DEFENDER]
        .into_iter()
        .flat_map(|block| {
            (1..=blocks_fit(capacity, block)).map(|n| engine::with_move_last(block.repeat(n)))
        })
        .collect()
}

/// The bodies a set that walks into a rival room is priced from: every one
/// carries a HEAL (owner), so shot up it heals the damage instead of losing
/// its MOVE to it and crawling. A home's Defend keeps `affordable`'s bare
/// DEFENDER: it stands beside its towers.
pub fn walking(capacity: i32) -> Vec<Body> {
    let mut out: Vec<Body> = affordable(capacity)
        .into_iter()
        .filter(|b| b.contains(&Part::Heal))
        .collect();

    // `affordable`'s smallest HEAL is a RANGER at 700: the pair keeps a bank
    // under that answering.
    out.extend(biggest(capacity, &[Part::Move, Part::Attack]));
    out
}

/// The body a set walks out with: one that heals what it is shot for, or its
/// MOVE parts die to it one walk at a time (owner).
fn with_heal(mut body: Body) -> Body {
    if !body.contains(&Part::Heal) {
        body.extend([Part::Heal, Part::Move]);
    }

    engine::with_move_last(body)
}

pub fn fighter_of(c: &Creep) -> Fighter {
    let (body, boosts) = (
        fabot_base::world::body_kinds(c),
        fabot_base::world::body_boosts(c),
    );
    let mut f = fighter::of_parts(&body, &boosts, c.hits, Some(c.pos));
    f.invader = c.owner == Owner::Invader;
    f
}

/// A level-0 Invader core holds its room's controller and strikes nothing: a
/// hostile without a body, which no exchange prices.
pub fn core_as_hostile(core: &Structure) -> Creep {
    Creep {
        id: core.id,
        name: core.id.to_string(),
        owner: core.owner.clone(),
        pos: core.pos,
        body: Vec::new(),
        hits: core.hits,
        hits_max: core.hits_max,
        ticks_to_live: core.collapse,
        fatigue: 0,
        store: Store::empty(),
    }
}

/// v1's guard on a core: a small one dies of age short of its 100,000 hits;
/// none where ours raze `hits` in a life.
pub fn razer(capacity: i32, hits: i32, ours: &[Fighter]) -> Vec<Body> {
    let swing = engine::ATTACK_POWER * ours.iter().map(|f| f.attack).sum::<i32>();

    biggest(capacity, &[Part::Move, Part::Attack])
        .filter(|_| swing * engine::CREEP_LIFETIME < hits)
        .into_iter()
        .collect()
}

/// Taps land a block apart whatever the body, so only more CLAIM a tap
/// shortens the declaim (v1). The HEAL a body out of our rooms takes costs it
/// a block, so none below 950.
pub fn tapper(capacity: i32) -> Option<Body> {
    biggest(capacity, &[Part::Move, Part::Claim])
}

/// The most of `block` the bank buys, with the HEAL and MOVE a body that walks
/// out of our rooms takes (owner): sized here, so it fits the colony the body
/// is cast from.
fn biggest(capacity: i32, block: &[Part]) -> Option<Body> {
    let legs = [Part::Heal, Part::Move];
    let most = ((capacity - engine::body_cost(&legs)) / engine::body_cost(block))
        .min((engine::MAX_BODY_PARTS as i32 - legs.len() as i32) / block.len() as i32);

    (most > 0).then(|| with_heal(block.repeat(most as usize)))
}

pub fn terrain_at(room: &Room, at: &Pos) -> exchange::Terrain {
    let last = engine::ROOM_SIDE - 1;

    exchange::Terrain {
        weight: if terrain::is_swamp(room.terrain, at.x, at.y) {
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
            .map(|s| (s.pos, s.hits))
            .collect::<BTreeMap<_, _>>(),
        towers: ours(Kind::Tower)
            .into_iter()
            .map(|s| exchange::Tower {
                pos: s.pos,
                energy: s.store.amount_of(&Resource::Energy),
            })
            .collect(),
    }
}

/// The compound a body cast from the declared lab cluster carries, and the
/// parts the cluster's colony holds for it; None while no cluster is declared
/// or it holds under a part's worth. A body the stock does not all cover is
/// cast unboosted rather than held.
pub fn cluster_boost(world: &World, declarations: &Declarations) -> Option<(ResourceType, i32)> {
    let (home, _) = declarations.labs()?;
    let (compound, energy) = fabot_base::boost::home_holds(world, home, Compound::UtriumAcid);
    let parts =
        (compound / engine::BOOST_MINERAL_PER_PART).min(energy / engine::BOOST_ENERGY_PER_PART);

    (parts > 0).then_some((ResourceType::UtriumAcid, parts))
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
/// kind's to price (§6.3); `boost` is the compound the cluster's labs cover
/// (see `cluster_boost`), which a body's parts carry when the stock buys them.
#[allow(clippy::too_many_arguments)]
pub fn price(
    stance: exchange::Stance,
    ours: &exchange::Side,
    theirs: &exchange::Side,
    terrain: exchange::Terrain,
    bodies: &[Body],
    copies_of: impl Fn(&Body) -> i32,
    tick_worth: i32,
    boost: Option<(ResourceType, i32)>,
) -> Option<(Vec<Body>, exchange::Outcome, exchange::Side)> {
    let set: Vec<Body> = if wins(stance, ours, theirs, terrain) {
        Vec::new()
    } else {
        exchange::cheapest_win(
            stance, ours, bodies, copies_of, tick_worth, theirs, terrain, boost,
        )?
    };

    let mut landed = ours.clone();
    landed
        .bodies
        .extend(set.iter().map(|body| fighter::cast_boosted(body, boost)));

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

pub fn most(capacity: i32, block: &[Part]) -> Body {
    engine::with_move_last(block.repeat(blocks_fit(capacity, block)))
}

/// Two dismantlers of the most WORK the bank buys, then the fewest MOVE:HEAL
/// medics `priced` breaks the way in with.
pub fn siege<T>(
    capacity: i32,
    mut priced: impl FnMut(&[Body]) -> Option<T>,
) -> Option<(Vec<Body>, T)> {
    let dismantler = most(capacity, &[Part::Work, Part::Move]);
    let medic = most(capacity, &[Part::Move, Part::Heal]);

    if dismantler.is_empty() {
        return None;
    }
    (1..=AROUND_ONE).find_map(|n| {
        let mut set = vec![dismantler.clone(), dismantler.clone()];
        set.extend((0..n).map(|_| medic.clone()));
        priced(&set).map(|plan| (set, plan))
    })
}

/// The dismantlers the fielded set lacks of two, then the fewest medics, that
/// `priced` breaks the way in with: a set re-planned onto a harder target or
/// short of a body lost is topped up, not recast.
pub fn reinforce<T>(
    capacity: i32,
    fielded: &[Body],
    mut priced: impl FnMut(&[Body]) -> Option<T>,
) -> Option<(Vec<Body>, T)> {
    let dismantler = most(capacity, &[Part::Work, Part::Move]);
    let medic = most(capacity, &[Part::Move, Part::Heal]);

    if medic.is_empty() || dismantler.is_empty() {
        return None;
    }

    let standing = fielded.iter().filter(|b| b.contains(&Part::Work)).count();
    let short = 2usize.saturating_sub(standing);

    (1..=AROUND_ONE as usize).find_map(|n| {
        let mut added = vec![dismantler.clone(); short];
        added.extend(vec![medic.clone(); n]);
        let mut set = fielded.to_vec();
        set.extend(added.iter().cloned());
        priced(&set).map(|plan| (added, plan))
    })
}

/// A drain's pair (`ARCHITECTURE.md` §6.2): TOUGH×20 MOVE×20, 1,200 energy of
/// 4,000 hits spent on a step a tick, and HEAL×13 MOVE×13, 3,900 energy of 2,600
/// that heals the volley back. Three pairs at most: a tower fires at one body a
/// tick (owner, 2026-10-21: a pair walks at full speed on plain).
const BOUNCER_TOUGH: usize = 20;
const MEDIC_HEAL: i32 = 13;

pub fn pair() -> [Body; 2] {
    let bouncer = [
        vec![Part::Tough; BOUNCER_TOUGH],
        vec![Part::Move; BOUNCER_TOUGH],
    ]
    .concat();
    let medic = [
        vec![Part::Heal; MEDIC_HEAL as usize],
        vec![Part::Move; MEDIC_HEAL as usize],
    ]
    .concat();
    [bouncer, medic]
}

fn pair_price() -> i32 {
    pair().iter().map(|b| engine::body_cost(b)).sum()
}

pub fn pairs(capacity: i32) -> i32 {
    (capacity / pair_price()).clamp(0, 3)
}

/// The heal the pairs' medics seat on the bouncer they work with a tick: what
/// his towers' volley on the gate has to fall under to be worth drawing.
pub fn pair_heal(pairs: i32) -> i32 {
    pairs * MEDIC_HEAL * engine::HEAL_POWER
}

pub fn drains(capacity: i32) -> Vec<Body> {
    let pair = pair();
    (0..pairs(capacity)).flat_map(|_| pair.clone()).collect()
}

/// The fewest bodies whose HEAL holds `strike`, each with the RANGED its share
/// of `worth` buys: the kills pay for the cast (v1's `castBodiesPaid`). A
/// siege's pickets and a sweep's set.
pub fn squad(capacity: i32, strike: i32, worth: i32) -> Vec<Body> {
    // The towers' strike falls on one body; it and one body beside it heal it.
    // A body under no strike still takes one (owner): shot up, it heals the
    // damage instead of losing its MOVE to it and crawling.
    let heal = (((strike + 2 * engine::HEAL_POWER - 1) / (2 * engine::HEAL_POWER)) as usize).max(1);
    let n = if strike > 0 { 2 } else { 1 };
    let body = |ranged: usize| -> Body {
        let mut b = vec![Part::RangedAttack; ranged];
        b.extend(vec![Part::Move; heal + ranged]);
        b.extend(vec![Part::Heal; heal]);
        engine::with_move_last(b)
    };
    let fits = |b: &Body| b.len() <= engine::MAX_BODY_PARTS && engine::body_cost(b) <= capacity;
    let paid =
        (worth / n as i32 / engine::body_cost(&[Part::RangedAttack, Part::Move])).max(1) as usize;
    (1..=paid)
        .take_while(|r| fits(&body(*r)))
        .last()
        .map_or_else(Vec::new, |r| vec![body(r); n])
}

/// Whether `set` holds `strike` falling on any one body: that body's heal and
/// the most another lends it from beside.
pub fn holds(set: &[Fighter], strike: i32) -> bool {
    let lent = |i: usize| {
        (0..set.len())
            .filter(|j| *j != i)
            .map(|j| set[j].heal)
            .max()
            .unwrap_or(0)
    };

    !set.is_empty()
        && (strike == 0
            || (0..set.len()).all(|i| engine::HEAL_POWER * (set[i].heal + lent(i)) >= strike))
}

pub fn kill_order(hostiles: &[Creep]) -> Vec<Id> {
    let ranked: Vec<(Id, (bool, f64))> = hostiles
        .iter()
        .map(|h| {
            let f = fighter_of(h);
            let damage = engine::ATTACK_POWER * f.attack + engine::RANGED_ATTACK_POWER * f.ranged;
            (
                h.id,
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

/// The colony that lands a body soonest of those whose bank buys it: its spawns'
/// queue, spread over them (v1's Caster), the refill a bank short of the body
/// waits for, then the walk. None: no colony pays for it.
pub fn colony_for(
    colonies: &[&Room],
    spawns: &[Spawn],
    queued: &BTreeMap<RoomName, i32>,
    asked: &BTreeMap<RoomName, i32>,
    walk: impl Fn(RoomName) -> Option<i32>,
    body: &[Part],
) -> Option<RoomName> {
    let cost = engine::body_cost(body);
    let spawn_ticks = engine::SPAWN_TICKS_PER_PART * body.len() as i32;

    let wait = |room: &Room| -> i64 {
        let of: Vec<&Spawn> = spawns.iter().filter(|s| s.pos.room == room.name).collect();
        let count = (of.len() as i64).max(1);
        let busy: i64 = of
            .iter()
            .map(|s| i64::from(s.spawning.as_ref().map_or(0, |o| o.remaining)))
            .sum();
        // The economy refills a ring at the pace its spawns spend it, so a body
        // past what the bank holds waits a spawn time for each of itself the
        // refill owes.
        let spends = i64::from(asked.get(&room.name).copied().unwrap_or(0)) + i64::from(cost);
        let short = (spends - i64::from(room.energy_available)).max(0);

        (busy + i64::from(queued.get(&room.name).copied().unwrap_or(0))) / count
            + i64::from(spawn_ticks) * short / i64::from(cost)
    };

    let ranked: Vec<(i64, RoomName)> = colonies
        .iter()
        .filter(|r| r.energy_capacity >= cost)
        .filter_map(|r| walk(r.name).map(|w| (i64::from(w) + wait(r), r.name)))
        .collect();

    let mut ranked = ranked;
    ranked.sort();
    ranked.into_iter().next().map(|(_, room)| room)
}

#[cfg(test)]
mod tests {
    use super::*;
    use fabot_base::grid;

    /// A pair is 5,100 of the bank and three pairs are the most a drain casts,
    /// a pair's medic seating 156 of heal a tick.
    #[test]
    fn a_drain_casts_a_pair_per_5100_and_three_at_most() {
        let price = pair_price();
        assert_eq!(price, 5_100);
        assert!(drains(price - 1).is_empty());
        assert_eq!(drains(price).len(), 2, "the bouncer then its medic");
        assert_eq!(drains(3 * price).len(), 6);
        assert_eq!(drains(4 * price).len(), 6, "three pairs");
        assert_eq!(pairs(price), 1);
        assert_eq!(pair_heal(pairs(price)), 156);
        assert_eq!(pair_heal(pairs(2 * price)), 312);
    }

    /// The owner's rule: a pair walks a plain tile a tick (a swamp in five).
    #[test]
    fn a_drain_body_steps_a_plain_tile_a_tick() {
        for body in pair() {
            let factor = grid::empty_factor(&body);
            assert_eq!(grid::step_ticks(factor, engine::PLAIN_WEIGHT), Some(1));
            assert_eq!(
                grid::step_ticks(factor, engine::SWAMP_WEIGHT),
                Some(engine::SWAMP_WEIGHT / engine::PLAIN_WEIGHT)
            );
        }
    }

    fn colony(name: &str, bank: i32) -> Room<'static> {
        Room {
            name: RoomName::named(name),
            terrain: None,
            controller: None,
            energy_available: bank,
            energy_capacity: 12_900,
            sources: Vec::new(),
            minerals: Vec::new(),
            structures: Vec::new(),
            sites: Vec::new(),
            piles: Vec::new(),
            remains: Vec::new(),
            others: Vec::new(),
        }
    }

    fn spawns_in(room: RoomName, count: i32) -> Vec<Spawn> {
        (1..=count)
            .map(|x| Spawn {
                id: format!("{room}-{x}").as_str().into(),
                name: String::new(),
                pos: Pos { room, x, y: 25 },
                spawning: None,
            })
            .collect()
    }

    /// v1's Caster with the link v2 dropped: a colony's queue is spread over its
    /// spawns, and a body past what its bank holds waits for the refill, so the
    /// nearest colony is not the one that lands it soonest.
    #[test]
    fn a_body_is_cast_to_the_colony_that_lands_it_soonest_not_the_nearest() {
        let body: Body = vec![Part::Move; 20];
        let (near, far) = (colony("W1N1", 12_000), colony("W3N1", 12_000));
        let spawns = [
            spawns_in(RoomName::named("W1N1"), 3),
            spawns_in(RoomName::named("W3N1"), 1),
        ]
        .concat();
        let queued = BTreeMap::from([(RoomName::named("W1N1"), 600)]);
        let walk = |from: RoomName| {
            Some(if from == RoomName::named("W1N1") {
                200
            } else {
                450
            })
        };

        assert_eq!(
            colony_for(
                &[&near, &far],
                &spawns,
                &queued,
                &BTreeMap::new(),
                walk,
                &body
            ),
            Some(RoomName::named("W1N1")),
            "a 600-tick queue over three spawns beats a walk 250 longer"
        );

        let broke = colony("W1N1", 0);
        let walk = |from: RoomName| {
            Some(if from == RoomName::named("W1N1") {
                200
            } else {
                250
            })
        };

        assert_eq!(
            colony_for(
                &[&broke, &far],
                &spawns,
                &BTreeMap::new(),
                &BTreeMap::new(),
                walk,
                &body
            ),
            Some(RoomName::named("W3N1")),
            "a bank with nothing for the body waits for the refill"
        );
    }
}
