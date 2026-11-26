pub use screeps::{Boost, Part, RESOURCES_ALL, ResourceType};

pub fn part_letter(part: Part) -> char {
    match part {
        Part::Work => 'W',
        Part::Carry => 'C',
        Part::Move => 'M',
        Part::Attack => 'A',
        Part::RangedAttack => 'R',
        Part::Heal => 'H',
        Part::Claim => 'K',
        Part::Tough => 'T',
        _ => unreachable!(),
    }
}

pub const ALL_BODY_PARTS: [Part; 8] = [
    Part::Work,
    Part::Carry,
    Part::Move,
    Part::Attack,
    Part::RangedAttack,
    Part::Heal,
    Part::Claim,
    Part::Tough,
];

/// A fight's `calcBodyEffectiveness`: a WORK boost counts for dismantle
/// only; TOUGH's boost is in `damage_ratio`.
pub fn multiple(part: Part, compound: Option<ResourceType>) -> i32 {
    match (part, compound.and_then(ResourceType::boost)) {
        (Part::Attack, Some(Boost::Attack(n)))
        | (Part::RangedAttack, Some(Boost::RangedAttack(n)))
        | (Part::Heal, Some(Boost::Heal(n)))
        | (Part::Move, Some(Boost::Move(n)))
        | (Part::Work, Some(Boost::Dismantle(n))) => n as i32,
        _ => 1,
    }
}

pub fn damage_ratio(part: Part, compound: Option<ResourceType>) -> f64 {
    match (part, compound.and_then(ResourceType::boost)) {
        // The crate holds the engine's tenths as f32.
        (Part::Tough, Some(Boost::Tough(r))) => (f64::from(r) * 10.0).round() / 10.0,
        _ => 1.0,
    }
}

pub const CLAIMER: [Part; 2] = [Part::Claim, Part::Move];

#[inline]
pub fn part_count_in(body: &[impl std::borrow::Borrow<Part>], part: Part) -> i32 {
    body.iter().filter(|each| *(*each).borrow() == part).count() as i32
}
