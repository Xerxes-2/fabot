use std::collections::BTreeSet;

use crate::core::vocabulary::BodyPart;
use crate::core::world::Pos;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Group {
    pub id: i32,
    pub members: BTreeSet<String>,
    pub room: String,
    pub last_seen: i32,
    pub centre: Pos,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Phase {
    Forming,
    Approaching,
    Fighting,
    Pursuing,
    Disengaging,
}

/// How the plan fights while it approaches and fights; a phase's own tactic
/// (pursue, retreat) is the phase's.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Tactic {
    Kite,
    Close,
    /// On our ramparts: a home is a post the raid comes to.
    Hold,
    /// A ranged set that approaches as one, then fights by `fights`.
    Blob(Box<Tactic>),
}

/// Set when a side is priced, never between: `order` is the hostiles' ids,
/// first struck first.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Plan {
    pub tactic: Tactic,
    pub order: Vec<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Objective {
    Defend,
    /// Into its room to the rival structure `target`; the plan's order is the
    /// barriers on the way, first broken first.
    Siege(String),
}

/// A Defend's target is `Group`, and `room` the room held or worked it
/// threatened; a Siege's is in its objective, and `group` none (0).
#[derive(Clone, Debug, PartialEq)]
pub struct Engagement {
    pub id: i32,
    pub objective: Objective,
    pub group: i32,
    pub room: String,
    pub ours: BTreeSet<String>,
    pub plan: Plan,
    pub phase: Phase,
    pub opened: i32,
    pub since: i32,
    pub lost: i32,
    /// The bodies it may lose before it disengages.
    pub budget: i32,
    pub priority: f64,
    /// Starved: towers and ramparts answer it alone.
    pub unanswered: bool,
    /// The bank it was last priced against; a larger one is an event.
    pub capacity: i32,
    /// What `Exchange` said each time it was answered after standing
    /// unanswered (at open, or a given-up home's re-answered), set against
    /// what happened (§10.1); None while it never was.
    pub predicted: Option<crate::core::exchange::Outcome>,
}

/// A body asked of `room`'s spawns under `name`, until it stands.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Cast {
    pub engagement: i32,
    pub name: String,
    pub room: String,
    pub body: Vec<BodyPart>,
}
