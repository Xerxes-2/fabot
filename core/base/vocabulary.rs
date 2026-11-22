use std::{borrow::Cow, cmp::Ordering};

use screeps::Boost;
pub use screeps::{Part, RESOURCES_ALL, ResourceType};
use strum::{EnumString, IntoStaticStr};

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

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString, IntoStaticStr)]
pub enum Ore {
    #[strum(serialize = "H")]
    Hydrogen,
    #[strum(serialize = "O")]
    Oxygen,
    #[strum(serialize = "U")]
    Utrium,
    #[strum(serialize = "K")]
    Keanium,
    #[strum(serialize = "L")]
    Lemergium,
    #[strum(serialize = "Z")]
    Zynthium,
    #[strum(serialize = "X")]
    Catalyst,
}

impl Ore {
    pub fn ordinal(self) -> i32 {
        self as i32
    }
}

/// A lab product: the engine's REACTIONS, read from the crate, name its reagents.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Compound(ResourceType, [ResourceType; 2]);

impl Compound {
    pub const fn of(resource: ResourceType) -> Option<Compound> {
        match resource.reaction_components() {
            Some(reagents) => Some(Compound(resource, reagents)),
            None => None,
        }
    }

    /// For a compound the source names: in a `const`, a wrong name fails the build.
    pub const fn new(resource: ResourceType) -> Compound {
        match Compound::of(resource) {
            Some(compound) => compound,
            None => panic!("not a lab product"),
        }
    }

    pub fn resource_type(self) -> ResourceType {
        self.0
    }

    pub fn reagents(self) -> [Resource; 2] {
        self.1.map(resource_of_type)
    }
}

impl PartialOrd for Compound {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Compound {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.0 as i32).cmp(&(other.0 as i32))
    }
}

/// A season's own resource stays `Season`'s and joins here only as `Other`.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Resource {
    Energy,
    Ore(Ore),
    Compound(Compound),
    Other(String),
}

impl Resource {
    #[inline]
    pub fn is_energy(&self) -> bool {
        self == &Resource::Energy
    }
}

pub fn resource_name(resource: &Resource) -> Cow<'static, str> {
    match resource {
        Resource::Energy => "energy".into(),
        Resource::Ore(ore) => Cow::Borrowed(ore.into()),
        Resource::Compound(compound) => compound.0.to_string().into(),
        Resource::Other(name) => name.to_owned().into(),
    }
}

pub fn resource_of_name(name: &str) -> Resource {
    (name.parse()).map_or_else(|_| Resource::Other(name.to_string()), resource_of_type)
}

pub fn resource_of_type(resource: ResourceType) -> Resource {
    use ResourceType as T;
    let ore = match resource {
        T::Energy => return Resource::Energy,
        T::Hydrogen => Ore::Hydrogen,
        T::Oxygen => Ore::Oxygen,
        T::Utrium => Ore::Utrium,
        T::Keanium => Ore::Keanium,
        T::Lemergium => Ore::Lemergium,
        T::Zynthium => Ore::Zynthium,
        T::Catalyst => Ore::Catalyst,
        _ => {
            return Compound::of(resource)
                .map_or_else(|| Resource::Other(resource.to_string()), Resource::Compound);
        }
    };
    Resource::Ore(ore)
}

pub fn is_mineral(resource: &Resource) -> bool {
    matches!(resource, Resource::Ore(_) | Resource::Compound(_))
}
