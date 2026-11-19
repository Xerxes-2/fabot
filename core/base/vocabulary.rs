use std::{borrow::Cow, cmp::Ordering};

use screeps::Boost;
pub use screeps::{Part, ResourceType};

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

#[inline]
pub fn part_count_in(body: &[Part], part: Part) -> i32 {
    body.iter().filter(|each| **each == part).count() as i32
}

/// Each union compares by a hand-written ordinal: Fable's generic union
/// equality walks the case structure, and v1 measured it on hot paths.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Ore {
    Hydrogen,
    Oxygen,
    Utrium,
    Keanium,
    Lemergium,
    Zynthium,
    Catalyst,
}

impl Ore {
    pub fn ordinal(self) -> i32 {
        self as i32
    }
}

/// The UH line alone; a compound joins the day a decision names it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Compound {
    Hydroxide,
    UtriumHydride,
    UtriumAcid,
}

impl Compound {
    pub fn ordinal(self) -> i32 {
        self as i32
    }
}

/// A season's own resource stays `Season`'s and joins here only as `Other`.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Resource {
    Energy,
    Ore(Ore),
    Compound(Compound),
    Other(String),
}

impl Resource {
    pub fn ordinal(&self) -> i32 {
        match self {
            Resource::Energy => 0,
            Resource::Ore(ore) => 1 + ore.ordinal(),
            Resource::Compound(compound) => 16 + compound.ordinal(),
            Resource::Other(_) => 64,
        }
    }

    #[inline]
    pub fn is_energy(&self) -> bool {
        self == &Resource::Energy
    }
}

impl PartialOrd for Resource {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Resource {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Resource::Other(a), Resource::Other(b)) => a.cmp(b),
            _ => self.ordinal().cmp(&other.ordinal()),
        }
    }
}

pub fn resource_name(resource: &Resource) -> Cow<'static, str> {
    match resource {
        Resource::Energy => "energy".into(),
        Resource::Ore(Ore::Hydrogen) => "H".into(),
        Resource::Ore(Ore::Oxygen) => "O".into(),
        Resource::Ore(Ore::Utrium) => "U".into(),
        Resource::Ore(Ore::Keanium) => "K".into(),
        Resource::Ore(Ore::Lemergium) => "L".into(),
        Resource::Ore(Ore::Zynthium) => "Z".into(),
        Resource::Ore(Ore::Catalyst) => "X".into(),
        Resource::Compound(Compound::Hydroxide) => "OH".into(),
        Resource::Compound(Compound::UtriumHydride) => "UH".into(),
        Resource::Compound(Compound::UtriumAcid) => "UH2O".into(),
        Resource::Other(name) => name.to_owned().into(),
    }
}

pub fn resource_of_name(name: &str) -> Resource {
    match name {
        "energy" => Resource::Energy,
        "H" => Resource::Ore(Ore::Hydrogen),
        "O" => Resource::Ore(Ore::Oxygen),
        "U" => Resource::Ore(Ore::Utrium),
        "K" => Resource::Ore(Ore::Keanium),
        "L" => Resource::Ore(Ore::Lemergium),
        "Z" => Resource::Ore(Ore::Zynthium),
        "X" => Resource::Ore(Ore::Catalyst),
        "OH" => Resource::Compound(Compound::Hydroxide),
        "UH" => Resource::Compound(Compound::UtriumHydride),
        "UH2O" => Resource::Compound(Compound::UtriumAcid),
        other => Resource::Other(other.to_string()),
    }
}

pub fn is_mineral(resource: &Resource) -> bool {
    matches!(resource, Resource::Ore(_) | Resource::Compound(_))
}
