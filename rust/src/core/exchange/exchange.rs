use std::collections::BTreeMap;

use crate::core::exchange::Fighter;
use crate::core::grid::FatigueFactor;
use crate::core::vocabulary::BodyPart;
use crate::core::world::Pos;

#[derive(Clone, Debug, PartialEq)]
pub struct Tower {
    pub pos: Pos,
    pub energy: i32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Side {
    pub bodies: Vec<Fighter>,
    pub ramparts: BTreeMap<Pos, i32>,
    pub towers: Vec<Tower>,
}

/// `weight` is the step weight of the ground fought on; `run_off` the tiles a
/// kiter backs away before a wall or an exit corners it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Terrain {
    pub weight: i32,
    pub run_off: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Losses {
    pub dead: i32,
    pub hits: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Outcome {
    Win(i32, Losses),
    Kite(i32),
    Lose,
}

impl Outcome {
    pub fn is_lose(&self) -> bool {
        matches!(self, Outcome::Lose)
    }

    pub fn is_win(&self) -> bool {
        matches!(self, Outcome::Win(_, _))
    }
}

pub type Body = Vec<BodyPart>;

/// Holding a post the raid must come to, every pair stands at a swing's
/// range; else the faster side picks the range.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Stance {
    Hold,
    Standoff,
}

/// A rival room as a breach is planned over it: each tile's step weight (-1
/// where no body walks) and the hits on each barrier tile, its rival ramparts' and walls' together.
#[derive(Clone, Debug, PartialEq)]
pub struct Ground {
    pub room: String,
    pub weights: Vec<i32>,
    pub barriers: BTreeMap<i32, i32>,
}

pub fn stance_against(stance: Stance, ours: &Side, theirs: &Side, terrain: Terrain) -> Stance {
    let _ = (stance, ours, theirs, terrain);
    todo!("ported by the Exchange agent")
}

pub fn outcome(stance: Stance, ours: &Side, theirs: &Side, terrain: Terrain) -> Outcome {
    let _ = (stance, ours, theirs, terrain);
    todo!("ported by the Exchange agent")
}

pub fn pooled_heal(seated: i32, ours: &[Fighter]) -> i32 {
    let _ = (seated, ours);
    todo!("ported by the Exchange agent")
}

/// What towers deal a body at `at`, the one strike a breach is gated and
/// re-priced by.
pub fn tower_strike(towers: &[Pos], at: &Pos) -> i32 {
    let _ = (towers, at);
    todo!("ported by the Exchange agent")
}

pub fn breach_plan(
    ground: &Ground,
    gait: FatigueFactor,
    towers: &[Pos],
    heal: i32,
    rate: i32,
    sources: &[i32],
    goals: &[i32],
) -> Option<(Vec<(i32, i32)>, i32)> {
    let _ = (ground, gait, towers, heal, rate, sources, goals);
    todo!("ported by the Exchange agent")
}

pub fn cheapest_win(
    stance: Stance,
    ours: &Side,
    bodies: &[Body],
    copies_of: impl Fn(&Body) -> i32,
    tick_worth: i32,
    theirs: &Side,
    terrain: Terrain,
) -> Option<Body> {
    let _ = (stance, ours, bodies, copies_of, tick_worth, theirs, terrain);
    todo!("ported by the Exchange agent")
}
