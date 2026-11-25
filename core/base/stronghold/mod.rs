//! An Invader stronghold as the engine stands it, and what its loot is worth.
use crate::vocabulary::{ResourceType, is_mineral, ores_in};

pub const INVADER_CORE_HITS: i32 = screeps::INVADER_CORE_HITS as i32;
pub const STRONGHOLD_RAMPART_HITS: [i32; 4] = [0, 100_000, 200_000, 500_000];
pub const STRONGHOLD_CORE_DENSITY: [i32; 4] = [0, 1_000, 16_000, 60_000];

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Stronghold {
    pub level: i32,
    pub deploys_at: i32,
    pub collapses_at: i32,
    pub loot: i32,
}

impl Stronghold {
    pub fn text(&self) -> String {
        let (l, d, c) = (self.level, self.deploys_at, self.collapses_at);
        format!("{l}.{d}.{c}.{}", self.loot)
    }

    /// A row written before loot was read has none.
    pub fn of_text(text: &str) -> Option<Stronghold> {
        let mut n = text.split('.').map(|n| n.parse().ok());
        Some(Stronghold {
            level: n.next()??,
            deploys_at: n.next()??,
            collapses_at: n.next()??,
            loot: n.next().flatten().unwrap_or(0),
        })
    }
}

/// The raw units one unit is made of, energy, ores, deposits and power one each: no market prices them.
pub fn raw(r: ResourceType) -> f64 {
    match r.commodity_recipe().filter(|_| r != ResourceType::Energy) {
        _ if is_mineral(r) => f64::from(ores_in(r).iter().sum::<i32>()),
        Some(c) => {
            c.components
                .iter()
                .map(|(k, n)| f64::from(*n) * raw(*k))
                .sum::<f64>()
                / f64::from(c.amount)
        }
        None => 1.0,
    }
}
