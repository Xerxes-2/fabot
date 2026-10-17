use crate::core::engine;
use crate::core::grid::{FatigueFactor, step_ticks};
use crate::core::vocabulary::{BodyPart, Boost, part_count_in};
use crate::core::world::Pos;

/// A body as the exchange reads it, on either side.
#[derive(Clone, Debug, PartialEq)]
pub struct Fighter {
    /// Head first, as the engine destroys it.
    pub body: Vec<BodyPart>,
    /// One a part, head first, or empty.
    pub boosts: Vec<Boost>,
    pub hits: i32,
    /// None for a body not yet cast.
    pub pos: Option<Pos>,
    pub attack: i32,
    pub ranged: i32,
    pub heal: i32,
    pub work: i32,
    pub reach: i32,
    pub gait: FatigueFactor,
    /// Zero on a rampart, where a swing at it lands on the rampart.
    pub strikes_back: i32,
    pub invader: bool,
}

pub fn of_parts(body: &[BodyPart], boosts: &[Boost], hits: i32, pos: Option<Pos>) -> Fighter {
    let power = engine::power_at(body, boosts, hits);
    let attack = power(BodyPart::Attack);
    let ranged = power(BodyPart::RangedAttack);
    Fighter {
        body: body.to_vec(),
        boosts: boosts.to_vec(),
        hits,
        pos,
        attack,
        ranged,
        heal: power(BodyPart::Heal),
        work: power(BodyPart::Work),
        reach: if ranged > 0 {
            engine::RANGED_RANGE
        } else if attack > 0 {
            engine::MELEE_RANGE
        } else {
            0
        },
        gait: FatigueFactor {
            weight: body.len() as i32
                - part_count_in(body, BodyPart::Move)
                - part_count_in(body, BodyPart::Carry),
            moves: power(BodyPart::Move),
        },
        strikes_back: engine::ATTACK_POWER * attack,
        invader: false,
    }
}

pub fn cast(body: &[BodyPart]) -> Fighter {
    of_parts(body, &[], engine::PART_HITS * body.len() as i32, None)
}

/// Unlike `Hostiles.armed`, a healer or a dismantler beside a striker does not count: it deals no damage.
pub fn strikes(f: &Fighter) -> bool {
    f.reach > 0
}

pub fn swings(f: &Fighter, range: i32) -> bool {
    f.attack > 0 && range <= engine::MELEE_RANGE
}

/// The engine's ATTACK and RANGED_ATTACK act in separate intent chains.
pub fn damage_at(f: &Fighter, range: i32) -> i32 {
    (if swings(f, range) {
        engine::ATTACK_POWER * f.attack
    } else {
        0
    }) + (if range <= engine::RANGED_RANGE {
        engine::RANGED_ATTACK_POWER * f.ranged
    } else {
        0
    })
}

/// A `heal` beside an `attack` drops the attack, so a body that swings heals nothing.
pub fn heal_at(f: &Fighter, range: i32) -> i32 {
    if swings(f, range) {
        0
    } else {
        engine::HEAL_POWER * f.heal
    }
}

pub fn healing(f: &Fighter) -> i32 {
    engine::HEAL_POWER * f.heal
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
