use std::{borrow::Cow, cmp::Ordering};

/// Each union compares by a hand-written ordinal: Fable's generic union
/// equality walks the case structure, and v1 measured it on hot paths.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BodyPart {
    Work,
    Carry,
    Move,
    Attack,
    RangedAttack,
    Heal,
    Claim,
    Tough,
}

impl BodyPart {
    pub fn ordinal(self) -> i32 {
        self as i32
    }

    pub fn is_work(self) -> bool {
        self == BodyPart::Work
    }

    pub fn is_carry(self) -> bool {
        self == BodyPart::Carry
    }

    pub fn is_attack(self) -> bool {
        self == BodyPart::Attack
    }

    pub fn is_ranged_attack(self) -> bool {
        self == BodyPart::RangedAttack
    }

    pub fn is_heal(self) -> bool {
        self == BodyPart::Heal
    }
}

pub fn same_part(a: BodyPart, b: BodyPart) -> bool {
    a == b
}

pub fn part_name(part: BodyPart) -> &'static str {
    match part {
        BodyPart::Work => "work",
        BodyPart::Carry => "carry",
        BodyPart::Move => "move",
        BodyPart::Attack => "attack",
        BodyPart::RangedAttack => "ranged_attack",
        BodyPart::Heal => "heal",
        BodyPart::Claim => "claim",
        BodyPart::Tough => "tough",
    }
}

pub fn part_of_name(name: &str) -> Option<BodyPart> {
    Some(match name {
        "work" => BodyPart::Work,
        "carry" => BodyPart::Carry,
        "move" => BodyPart::Move,
        "attack" => BodyPart::Attack,
        "ranged_attack" => BodyPart::RangedAttack,
        "heal" => BodyPart::Heal,
        "claim" => BodyPart::Claim,
        "tough" => BodyPart::Tough,
        _ => return None,
    })
}

pub fn part_letter(part: BodyPart) -> char {
    match part {
        BodyPart::Work => 'W',
        BodyPart::Carry => 'C',
        BodyPart::Move => 'M',
        BodyPart::Attack => 'A',
        BodyPart::RangedAttack => 'R',
        BodyPart::Heal => 'H',
        BodyPart::Claim => 'K',
        BodyPart::Tough => 'T',
    }
}

pub const ALL_BODY_PARTS: [BodyPart; 8] = [
    BodyPart::Work,
    BodyPart::Carry,
    BodyPart::Move,
    BodyPart::Attack,
    BodyPart::RangedAttack,
    BodyPart::Heal,
    BodyPart::Claim,
    BodyPart::Tough,
];

/// A fight's view of a part's boost (`BOOSTS`): a WORK boost for harvest,
/// build or upgrade and a CARRY boost read as `Unboosted`.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Boost {
    Unboosted,
    Tier1,
    Tier2,
    Tier3,
}

pub mod boost {
    use super::{BodyPart, Boost};

    /// The T2 UH-line compound, the strongest our labs make: `^UH2O` on ATTACK.
    pub const UTRIUM_ACID: &str = "UH2O";

    pub fn of_compound(part: BodyPart, compound: &str) -> Boost {
        match (part, compound) {
            (BodyPart::Attack, "UH")
            | (BodyPart::RangedAttack, "KO")
            | (BodyPart::Heal, "LO")
            | (BodyPart::Tough, "GO")
            | (BodyPart::Move, "ZO")
            | (BodyPart::Work, "ZH") => Boost::Tier1,
            (BodyPart::Attack, UTRIUM_ACID)
            | (BodyPart::RangedAttack, "KHO2")
            | (BodyPart::Heal, "LHO2")
            | (BodyPart::Tough, "GHO2")
            | (BodyPart::Move, "ZHO2")
            | (BodyPart::Work, "ZH2O") => Boost::Tier2,
            (BodyPart::Attack, "XUH2O")
            | (BodyPart::RangedAttack, "XKHO2")
            | (BodyPart::Heal, "XLHO2")
            | (BodyPart::Tough, "XGHO2")
            | (BodyPart::Move, "XZHO2")
            | (BodyPart::Work, "XZH2O") => Boost::Tier3,
            _ => Boost::Unboosted,
        }
    }

    /// `calcBodyEffectiveness`; TOUGH's boost is in `damageRatio`.
    pub fn multiple(part: BodyPart, boost: Boost) -> i32 {
        match (part, boost) {
            (
                BodyPart::Attack
                | BodyPart::RangedAttack
                | BodyPart::Heal
                | BodyPart::Move
                | BodyPart::Work,
                Boost::Tier1,
            ) => 2,
            (
                BodyPart::Attack
                | BodyPart::RangedAttack
                | BodyPart::Heal
                | BodyPart::Move
                | BodyPart::Work,
                Boost::Tier2,
            ) => 3,
            (
                BodyPart::Attack
                | BodyPart::RangedAttack
                | BodyPart::Heal
                | BodyPart::Move
                | BodyPart::Work,
                Boost::Tier3,
            ) => 4,
            _ => 1,
        }
    }

    pub fn damage_ratio(part: BodyPart, boost: Boost) -> f64 {
        match (part, boost) {
            (BodyPart::Tough, Boost::Tier1) => 0.7,
            (BodyPart::Tough, Boost::Tier2) => 0.5,
            (BodyPart::Tough, Boost::Tier3) => 0.3,
            _ => 1.0,
        }
    }
}

pub fn part_count_in(body: &[BodyPart], part: BodyPart) -> i32 {
    body.iter().filter(|each| same_part(**each, part)).count() as i32
}

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

pub const ALL_ORES: [Ore; 7] = [
    Ore::Hydrogen,
    Ore::Oxygen,
    Ore::Utrium,
    Ore::Keanium,
    Ore::Lemergium,
    Ore::Zynthium,
    Ore::Catalyst,
];

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

pub const ALL_COMPOUNDS: [Compound; 3] = [
    Compound::Hydroxide,
    Compound::UtriumHydride,
    Compound::UtriumAcid,
];

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

    pub fn is_energy(&self) -> bool {
        self == &Resource::Energy
    }

    pub fn is_ore(&self) -> bool {
        matches!(self, Resource::Ore(_))
    }

    pub fn is_compound(&self) -> bool {
        matches!(self, Resource::Compound(_))
    }

    pub fn is_other(&self) -> bool {
        matches!(self, Resource::Other(_))
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
