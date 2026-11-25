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

pub fn takes(part: Part, compound: ResourceType) -> bool {
    multiple(part, Some(compound)) > 1 || damage_ratio(part, Some(compound)) < 1.0
}

pub const CLAIMER: [Part; 2] = [Part::Claim, Part::Move];

#[inline]
pub fn part_count_in(body: &[impl std::borrow::Borrow<Part>], part: Part) -> i32 {
    body.iter().filter(|each| *(*each).borrow() == part).count() as i32
}

pub const ORES: [ResourceType; 7] = {
    use ResourceType::*;
    [
        Hydrogen, Oxygen, Utrium, Keanium, Lemergium, Zynthium, Catalyst,
    ]
};

/// The owner's rank of the part each T3 boosts.
pub const BOOSTS: [(ResourceType, i32); 6] = {
    use ResourceType::*;
    [
        (CatalyzedGhodiumAlkalide, 4),
        (CatalyzedLemergiumAlkalide, 3),
        (CatalyzedKeaniumAlkalide, 2),
        (CatalyzedGhodiumAcid, 1),
        (CatalyzedZynthiumAlkalide, 1),
        (CatalyzedUtriumAcid, 1),
    ]
};

pub fn ores_in(resource: ResourceType) -> [i32; ORES.len()] {
    match resource.reaction_components() {
        Some([a, b]) => {
            let (a, b) = (ores_in(a), ores_in(b));
            std::array::from_fn(|i| a[i] + b[i])
        }
        None => ORES.map(|o| i32::from(o == resource)),
    }
}

pub fn is_mineral(resource: ResourceType) -> bool {
    ORES.contains(&resource) || resource.reaction_components().is_some()
}
