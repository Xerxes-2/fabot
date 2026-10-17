/// What a fight casts and where (`ARCHITECTURE.md` §6.3, §6.5). From v1's
/// Caster: a body goes to the spawn that lands it soonest, a home's own Defend
/// to the home's.
use std::collections::BTreeMap;

use super::shape::Tactic;
use crate::core::exchange::{exchange, fighter, Fighter};
use crate::core::vocabulary::{BodyPart, Boost, Resource};
use crate::core::world::{terrain, Creep, Kind, Owner, Pos, Room, Spawn, Store, Structure};
use crate::core::{engine, lists};

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

            out.push(body);
        }
    }

    out
}

pub fn fighter_of(c: &Creep) -> Fighter {
    let body: Vec<BodyPart> = c.body.iter().map(|p| p.kind).collect();
    let boosts: Vec<Boost> = c.body.iter().map(|p| p.boost).collect();
    let mut f = fighter::of_parts(&body, &boosts, c.hits, Some(c.pos.clone()));
    f.invader = matches!(&c.owner, Owner::Theirs(user) if user == engine::INVADER_USER);
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
    let block = vec![BodyPart::Move, BodyPart::Attack];

    let most = (capacity / engine::body_cost(&block))
        .min(engine::MAX_BODY_PARTS as i32 / block.len() as i32);

    let swing = engine::ATTACK_POWER * ours.iter().map(|f| f.attack).sum::<i32>();

    if most == 0 || swing * engine::CREEP_LIFETIME >= hits {
        Vec::new()
    } else {
        let mut body = Vec::new();

        for _ in 0..most {
            body.extend(block.iter().copied());
        }

        vec![body]
    }
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

/// The bodies to ask beside `ours`, the outcome they land and the tactic it
/// is won by; None: no body the spawns buy wins.
pub fn price(
    stance: exchange::Stance,
    ours: &exchange::Side,
    theirs: &exchange::Side,
    terrain: exchange::Terrain,
    capacity: i32,
    copies_of: impl Fn(&Body) -> i32,
    tick_worth: i32,
) -> Option<(Vec<Body>, exchange::Outcome, Tactic)> {
    let set: Vec<Body> = if wins(stance, ours, theirs, terrain) {
        Vec::new()
    } else {
        // `Exchange::cheapest_win` answers the copies of one block as a single
        // body here; the set it stood for is that block once.
        vec![exchange::cheapest_win(
            stance,
            ours,
            &affordable(capacity),
            copies_of,
            tick_worth,
            theirs,
            terrain,
        )?]
    };

    let mut landed = ours.clone();
    landed
        .bodies
        .extend(set.iter().map(|body| fighter::cast(body)));

    let tactic = |won: Tactic| -> Tactic {
        if exchange::stance_against(stance, &landed, theirs, terrain) == exchange::Stance::Hold {
            Tactic::Hold
        } else if landed.bodies.len() > 1 && landed.bodies.iter().all(|f| f.attack == 0) {
            Tactic::Blob(Box::new(won))
        } else {
            won
        }
    };

    match exchange::outcome(stance, &landed, theirs, terrain) {
        won @ exchange::Outcome::Win(_, _) => Some((set, won, tactic(Tactic::Close))),
        other => Some((set, other, tactic(Tactic::Kite))),
    }
}

/// No more medics heal one body from beside it than the tiles around it.
const AROUND_ONE: i32 = 8;

/// Two dismantlers of the most WORK the bank buys, then the fewest MOVE:HEAL
/// medics `priced` breaks the way in with.
pub fn siege<T>(
    capacity: i32,
    mut priced: impl FnMut(&[Body]) -> Option<T>,
) -> Option<(Vec<Body>, T)> {
    let most = |block: &Body| -> Body {
        let n = (capacity / engine::body_cost(block))
            .min(engine::MAX_BODY_PARTS as i32 / block.len() as i32);
        let mut body = Vec::new();

        for _ in 0..n.max(0) {
            body.extend(block.iter().copied());
        }

        body
    };

    let dismantler = most(&vec![BodyPart::Work, BodyPart::Move]);
    let medic = most(&vec![BodyPart::Move, BodyPart::Heal]);

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

pub fn kill_order(hostiles: &[Creep]) -> Vec<String> {
    let ranked: Vec<(String, (bool, f64))> = hostiles
        .iter()
        .map(|h| {
            let f = fighter_of(h);
            let damage = engine::ATTACK_POWER * f.attack + engine::RANGED_ATTACK_POWER * f.ranged;
            (
                h.id.clone(),
                (f.heal == 0, -(damage as f64 / h.hits.max(1) as f64)),
            )
        })
        .collect();

    lists::sorted_by_key_by(
        |(id, rank): &(String, (bool, f64))| (rank.0, rank.1, id.clone()),
        |a: &(bool, f64, String), b: &(bool, f64, String)| {
            a.0.cmp(&b.0)
                .then(a.1.partial_cmp(&b.1).unwrap_or(std::cmp::Ordering::Equal))
                .then(a.2.cmp(&b.2))
        },
        &ranked,
    )
    .into_iter()
    .map(|(id, _)| id)
    .collect()
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
            .map(|s| s.spawning.as_ref().map(|o| o.remaining).unwrap_or(0))
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
