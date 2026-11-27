use std::collections::BTreeMap;

use fabot_atlas::Atlas;
use fabot_base::{
    boost::{self, Boosting, Stock},
    engine, tiles,
    vocabulary::{Part, ResourceType},
    world::{
        self, Creep, Id, Owner, Pos, Room, RoomName, Rules, Spawn, Store, Structure, StructureType,
        terrain,
    },
};
use fabot_exchange::{self as exchange, Fighter, fighter};

use crate::shape::{Cast, Engagement};

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
const RANGER: [Part; 6] =
    [Part::Move, Part::Move, Part::Move, Part::RangedAttack, Part::RangedAttack, Part::Heal];
pub const DEFENDER: [Part; 2] = [Part::Move, Part::Attack];
pub const KITER: [Part; 3] = [Part::RangedAttack, Part::Move, Part::Move];

/// v1's `guardCap`: a home's Defend never disengages, and its loss budget,
/// not a yield, bounds what it asks (§6.2).
pub const HOME_COPIES: i32 = 2;

pub fn copies_within(worth: i32, spawn_ticks: i32, body: &[Part]) -> i32 {
    (worth / engine::body_cost(body)).min(spawn_ticks / (engine::spawn_ticks(body.len())))
}

fn blocks_fit(capacity: i32, block: &[Part]) -> usize {
    let n = (capacity / engine::body_cost(block))
        .min(engine::MAX_BODY_PARTS as i32 / block.len() as i32);
    n.max(0) as usize
}

pub fn affordable(capacity: i32) -> Vec<Body> {
    [&GUARD[..], &RANGER, &DEFENDER, &KITER]
        .into_iter()
        .flat_map(|block| {
            (1..=blocks_fit(capacity, block)).map(|n| engine::with_move_last(block.repeat(n)))
        })
        .collect()
}

/// Every body that walks into a rival room carries a HEAL (owner): shot up, it
/// heals the damage instead of losing its MOVE to it and crawling. A home's
/// Defend keeps `affordable`'s bare bodies: they stand beside its towers.
pub fn walking(capacity: i32) -> Vec<Body> {
    let mut out: Vec<Body> =
        affordable(capacity).into_iter().filter(|b| b.contains(&Part::Heal)).collect();

    // `affordable`'s smallest HEAL is a RANGER at 700: the pair keeps a bank
    // under that answering.
    out.extend(biggest(capacity, &[Part::Move, Part::Attack]));
    out
}

/// Bare bodies only short of a RANGER: a HEAL a bank cannot cast answers no raid (§6.5).
pub fn defending(capacity: i32) -> Vec<Body> {
    let mut out = walking(capacity);
    if !out.iter().any(|b| b.contains(&Part::RangedAttack)) {
        out.extend(affordable(capacity));
    }
    out
}

fn with_heal(mut body: Body) -> Body {
    if !body.contains(&Part::Heal) {
        body.extend([Part::Heal, Part::Move]);
    }

    engine::with_move_last(body)
}

pub fn fighter_of(c: &Creep) -> Fighter {
    let (body, boosts) = (world::body_kinds(c), world::body_boosts(c));
    let mut f = fighter::of_parts(&body, &boosts, c.hits, Some(c.pos), c.store.used());
    // The stand-down and the price must agree on whether a hostile strikes.
    if !c.owner.is_friend() && world::hostiles::foe(c) && world::hostiles::strikes(c) {
        f.reach = f.reach.max(engine::strike_reach(|p| body.contains(&p)));
    }
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
        store: Store::default(),
    }
}

/// v1's guard on a core: a small one dies of age short of its 100,000 hits;
/// none where ours raze `hits` in a life.
pub fn razer(capacity: i32, hits: i32, ours: &[Fighter]) -> Vec<Body> {
    let swing = engine::strike_at(ours.iter().map(|f| f.attack).sum(), 0, engine::MELEE_RANGE);

    biggest(capacity, &[Part::Move, Part::Attack])
        .filter(|_| swing * engine::CREEP_LIFETIME < hits)
        .into_iter()
        .collect()
}

/// Sized with the HEAL and MOVE a body out of our rooms takes (owner), so it
/// fits the colony the body is cast from.
fn biggest(capacity: i32, block: &[Part]) -> Option<Body> {
    let legs = [Part::Heal, Part::Move];
    let most = ((capacity - engine::body_cost(&legs)) / engine::body_cost(block))
        .min((engine::MAX_BODY_PARTS as i32 - legs.len() as i32) / block.len() as i32);

    (most > 0).then(|| with_heal(block.repeat(most as usize)))
}

pub fn terrain_at(atlas: &Atlas, room: Option<&Room>, at: &Pos) -> exchange::Terrain {
    let last = engine::ROOM_SIDE - 1;
    let bare = if terrain::is_swamp(room.and_then(|r| r.terrain), at.x, at.y) {
        engine::SWAMP_WEIGHT
    } else {
        engine::PLAIN_WEIGHT
    };

    exchange::Terrain {
        weight: atlas.ground_at(at.room, tiles::of_pos(at)).unwrap_or(bare),
        run_off: at.x.max(last - at.x).max(at.y.max(last - at.y)) - 1,
    }
}

pub fn side_in(rules: &Rules, room: Option<&Room>, bodies: Vec<Fighter>) -> exchange::Side {
    let ours = |kind: StructureType| move |s: &&Structure| s.kind == kind && s.owner.is_ours();

    exchange::Side {
        bodies,
        ramparts: (room.iter().flat_map(|r| &r.structures))
            .filter(ours(StructureType::Rampart))
            .map(|s| (s.pos, s.hits))
            .collect::<BTreeMap<_, _>>(),
        towers: (room.iter().flat_map(|r| rules.towers(r)))
            .filter(|s| s.owner.is_ours())
            .map(|s| exchange::Tower {
                pos: s.pos,
                energy: s.store.amount_of(ResourceType::Energy),
            })
            .collect(),
        keeps: (room.iter().flat_map(|r| &r.structures))
            .filter(|s| s.owner.is_ours() && crate::kinds::defend::keeps(s.kind))
            .map(|s| (s.pos, s.hits))
            .collect(),
        ..exchange::Side::default()
    }
}

pub type Boosts = BTreeMap<RoomName, Stock>;

/// By cluster, what its waiting bodies and its casts priced boosted still to
/// spawn leave: a body is boosted only from its own home's labs.
pub fn cluster_boosts(
    boosting: &[(RoomName, Boosting)],
    spawning: &[Cast],
    engagements: &[Engagement],
) -> Boosts {
    let mut left: Boosts = (boosting.iter()).map(|(r, b)| (*r, b.left.clone())).collect();
    let priced = |c: &&Cast| crate::scene::boosted(engagements).any(|b| b.name == c.name);
    for c in spawning.iter().filter(priced) {
        if let Some(stock) = left.get_mut(&c.room) {
            boost::boosted_by(&c.body, stock);
        }
    }
    left
}

pub fn wins(
    stance: exchange::Stance,
    ours: &exchange::Side,
    theirs: &exchange::Side,
    terrain: exchange::Terrain,
) -> bool {
    !exchange::outcome(stance, ours, theirs, terrain).is_lose()
}

/// Which tactic fights the set is the kind's to price (§6.3).
#[allow(clippy::too_many_arguments)]
pub fn price(
    stance: exchange::Stance,
    ours: &exchange::Side,
    theirs: &exchange::Side,
    terrain: exchange::Terrain,
    bodies: &[Body],
    copies_of: impl Fn(&Body) -> i32,
    tick_worth: i32,
    boosts: &Stock,
) -> Option<Pricing> {
    let (set, boosted) = if wins(stance, ours, theirs, terrain) {
        (Vec::new(), false)
    } else {
        exchange::cheapest_win(
            stance, ours, bodies, copies_of, tick_worth, theirs, terrain, boosts,
        )?
    };
    let bare = Stock::default();
    let stock = if boosted { boosts } else { &bare };

    let mut landed = ours.clone();
    landed.bodies.extend(fighter::cast_boosted(set.iter().map(|b| &b[..]), stock));

    let outcome = exchange::outcome(stance, &landed, theirs, terrain);

    Some(Pricing { set, outcome, landed, boosted })
}

pub struct Pricing {
    pub set: Vec<Body>,
    pub outcome: exchange::Outcome,
    pub landed: exchange::Side,
    pub boosted: bool,
}

pub struct Asking<'a> {
    pub bodies: &'a [Body],
    pub boosted: bool,
}

pub fn bare(bodies: &[Body]) -> Asking<'_> {
    Asking { bodies, boosted: false }
}

/// A cast body the outcome prices costs only what it strikes off: one that
/// lives is still ours for the next fight.
pub fn spent(unpriced: &[Body], losses: exchange::Losses, ticks: i32, tick_worth: i32) -> i32 {
    let cast: i32 = unpriced.iter().map(|body| engine::body_cost(body)).sum();
    cast + losses.energy + tick_worth * ticks
}

pub fn kill_order(hostiles: &[Creep]) -> Vec<Id> {
    let mut ranked: Vec<&Creep> = hostiles.iter().collect();
    ranked.sort_by_cached_key(|h| fighter::kill_rank(&fighter_of(h)));
    ranked.into_iter().map(|h| h.id).collect()
}

/// Soonest, not nearest (v1's Caster).
pub fn colony_for(
    colonies: &[&Room],
    spawns: &[&Spawn],
    queued: &BTreeMap<RoomName, Vec<usize>>,
    asked: &BTreeMap<RoomName, i32>,
    walk: impl Fn(RoomName) -> Option<i32>,
    body: &[Part],
) -> Option<RoomName> {
    let cost = engine::body_cost(body);
    let spawn_ticks = engine::spawn_ticks(body.len());

    let wait = |room: &Room| -> i64 {
        let busy: Vec<i32> = (spawns.iter().filter(|s| s.pos.room == room.name))
            .map(|s| s.spawning.as_ref().map_or(0, |o| o.remaining))
            .collect();
        let ahead = queued.get(&room.name).into_iter().flatten().copied();
        let landed = exchange::spawned_after(&busy, ahead.chain([body.len()])).pop();
        // The economy refills a ring at the pace its spawns spend it, so a body
        // past what the bank holds waits a spawn time for each of itself the
        // refill owes.
        let spends = i64::from(asked.get(&room.name).copied().unwrap_or(0)) + i64::from(cost);
        let short = (spends - i64::from(room.energy_available)).max(0);

        i64::from(landed.unwrap_or(0) - spawn_ticks)
            + i64::from(spawn_ticks) * short / i64::from(cost)
    };

    (colonies.iter())
        .filter(|r| r.energy_capacity >= cost)
        .filter_map(|r| walk(r.name).map(|w| (i64::from(w) + wait(r), r.name)))
        .min()
        .map(|(_, room)| room)
}

/// Each colony `colony_for` picks at rest (no queue, full banks) for some body.
pub fn homes(banks: &[(RoomName, i32)], walk: impl Fn(RoomName) -> Option<i32>) -> Vec<RoomName> {
    let mut reached: Vec<_> = (banks.iter()).filter_map(|&(r, c)| Some((walk(r)?, r, c))).collect();
    reached.sort();
    let mut most = 0;
    (reached.into_iter())
        .filter(|&(.., capacity)| (capacity > most).then(|| most = capacity).is_some())
        .map(|(_, room, _)| room)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn homes_are_the_colonies_no_nearer_one_out_banks() {
        let room = |x| RoomName::new(x, 1);
        let banks = [(room(1), 12_700), (room(3), 5_500), (room(5), 700), (room(7), 12_700)];
        let walk = |r: RoomName| banks.iter().position(|(b, _)| *b == r).map(|i| [3, 1, 0, 5][i]);
        assert_eq!(homes(&banks, walk), vec![room(5), room(3), room(1)]);
    }

    fn colony(name: &str, bank: i32) -> Room<'static> {
        Room {
            energy_available: bank,
            energy_capacity: 12_900,
            ..Room::empty(name.parse::<RoomName>().unwrap())
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
        let spawns =
            [spawns_in(RoomName::W1N1, 3), spawns_in("W3N1".parse::<RoomName>().unwrap(), 1)]
                .concat();
        let queued = BTreeMap::from([(RoomName::W1N1, vec![50; 4])]);
        let walk = |from: RoomName| Some(if from == RoomName::W1N1 { 200 } else { 450 });

        assert_eq!(
            colony_for(
                &[&near, &far],
                &spawns.iter().collect::<Vec<_>>(),
                &queued,
                &BTreeMap::new(),
                walk,
                &body
            ),
            Some(RoomName::W1N1),
            "a 600-tick queue over three spawns beats a walk 250 longer"
        );

        let broke = colony("W1N1", 0);
        let walk = |from: RoomName| Some(if from == RoomName::W1N1 { 200 } else { 250 });

        assert_eq!(
            colony_for(
                &[&broke, &far],
                &spawns.iter().collect::<Vec<_>>(),
                &BTreeMap::new(),
                &BTreeMap::new(),
                walk,
                &body
            ),
            Some("W3N1".parse::<RoomName>().unwrap()),
            "a bank with nothing for the body waits for the refill"
        );
    }
}
