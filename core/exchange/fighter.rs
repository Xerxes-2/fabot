use std::cmp::Reverse;

use fabot_base::grid::{self, FatigueFactor, step_ticks};
use fabot_base::vocabulary::{Part, ResourceType, takes};
use fabot_base::world::Pos;
use fabot_base::{boost, engine};

/// A body as the exchange reads it, on either side.
#[derive(Clone, Debug, PartialEq)]
pub struct Fighter {
    /// Head first, as the engine destroys it.
    pub body: Vec<Part>,
    /// One a part, head first, or empty.
    pub boosts: Vec<Option<ResourceType>>,
    pub hits: i32,
    /// None for a body not yet cast.
    pub pos: Option<Pos>,
    pub attack: i32,
    pub ranged: i32,
    pub heal: i32,
    pub work: i32,
    pub reach: i32,
    pub gait: FatigueFactor,
    /// What its CARRY holds, which weighs as the engine reads it.
    pub load: i32,
    /// Zero on a rampart, where a swing at it lands on the rampart.
    pub strikes_back: i32,
    pub invader: bool,
    /// The ticks it still stands at its home's labs before it walks.
    pub lab_wait: i32,
}

pub fn of_parts(
    body: &[Part],
    boosts: &[Option<ResourceType>],
    hits: i32,
    pos: Option<Pos>,
    load: i32,
) -> Fighter {
    let power = engine::power_at(body, boosts, hits);
    let attack = power(Part::Attack);
    let ranged = power(Part::RangedAttack);
    Fighter {
        body: body.to_vec(),
        boosts: boosts.to_vec(),
        hits,
        pos,
        attack,
        ranged,
        heal: power(Part::Heal),
        work: power(Part::Work),
        reach: engine::strike_reach(|p| power(p) > 0),
        gait: gait_of(body, boosts, hits, load),
        load,
        strikes_back: engine::strike_at(attack, 0, engine::MELEE_RANGE),
        invader: false,
        lab_wait: 0,
    }
}

pub fn gait_of(
    body: &[Part],
    boosts: &[Option<ResourceType>],
    hits: i32,
    load: i32,
) -> FatigueFactor {
    let dead = body.len() - engine::live_parts(body, hits).len();
    let boost = |i: usize| boosts.get(i).copied().flatten();
    grid::factor(
        body.iter()
            .enumerate()
            .map(|(i, p)| (*p, boost(i), i >= dead)),
        load,
    )
}

pub fn cast(body: &[Part]) -> Fighter {
    of_parts(body, &[], engine::PART_HITS * body.len() as i32, None, 0)
}

/// Not-yet-cast bodies, each boosted from what the ones before it left.
pub fn cast_boosted<'a>(
    bodies: impl IntoIterator<Item = &'a [Part]>,
    stock: &boost::Stock,
) -> Vec<Fighter> {
    let mut left = stock.clone();
    let mut queue = boost::LabQueue::default();
    (bodies.into_iter())
        .map(|body| {
            let by = boost::boosted_by(body, &mut left);
            if by.compounds.is_empty() {
                return cast(body);
            }
            queue.0 += by.fills;
            let boosts: Vec<Option<ResourceType>> = (body.iter())
                .map(|p| by.compounds.iter().copied().find(|c| takes(*p, *c)))
                .collect();
            Fighter {
                lab_wait: queue.wait_with(0),
                ..of_parts(
                    body,
                    &boosts,
                    engine::PART_HITS * body.len() as i32,
                    None,
                    0,
                )
            }
        })
        .collect()
}

/// Unlike `Hostiles.armed`, a healer or a dismantler beside a striker does not count: it deals no damage.
pub fn strikes(f: &Fighter) -> bool {
    f.reach > 0
}

/// The kill order, least first, which the Exchange prices and our bodies strike alike.
pub fn kill_rank(f: &Fighter) -> (u8, bool, Reverse<i32>, Option<Pos>) {
    let band = match (strikes(f), f.work > 0) {
        (true, _) => 0,
        (false, true) => 1,
        (false, false) => 2,
    };
    let damage = damage_at(f, engine::MELEE_RANGE);
    let per_thousand_hits = 1000 * damage / f.hits.max(1);
    (band, f.heal == 0, Reverse(per_thousand_hits), f.pos)
}

pub fn damage_at(f: &Fighter, range: i32) -> i32 {
    engine::strike_at(f.attack, f.ranged, range)
}

/// A `heal` beside an `attack` drops the attack, so a body that swings heals nothing.
pub fn heal_at(f: &Fighter, range: i32) -> i32 {
    if f.attack > 0 && range <= engine::MELEE_RANGE {
        0
    } else {
        healing(f)
    }
}

pub fn healing(f: &Fighter) -> i32 {
    engine::heal_at(f.heal, engine::MELEE_RANGE)
}

pub fn damage_for(f: &Fighter, hits: i32) -> i32 {
    engine::damage_for(&f.body, &f.boosts, f.hits, hits)
}

pub fn effective_hits(f: &Fighter) -> i32 {
    damage_for(f, f.hits)
}

pub fn tough_hits(f: &Fighter) -> i32 {
    engine::tough_hits(&f.body, &f.boosts, f.hits)
}

/// A body with no MOVE steps nowhere of itself: a puller's pace is the puller's.
pub fn falls_behind(weight: i32, ours: FatigueFactor, theirs: FatigueFactor) -> bool {
    match (step_ticks(theirs, weight), step_ticks(ours, weight)) {
        (None, Some(_)) => true,
        (Some(t), Some(o)) => t > o,
        (_, None) => false,
    }
}

/// Read on plain, as the ground a kite runs on: a swamp tile's pace at a MOVE a part.
pub fn stationary(f: &Fighter) -> bool {
    step_ticks(f.gait, engine::PLAIN_WEIGHT).is_some_and(|ticks| ticks >= 5)
}

pub fn best_range(f: &Fighter) -> i32 {
    if damage_at(f, f.reach) >= damage_at(f, engine::MELEE_RANGE) {
        engine::MELEE_RANGE.max(f.reach)
    } else {
        engine::MELEE_RANGE
    }
}
