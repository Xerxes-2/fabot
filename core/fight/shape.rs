use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};

use crate::tactics::{Offer, Tactic};
use crate::vocabulary::Part;
use crate::world::{Id, Pos, RoomName};

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(from = "crate::memory::Seen", into = "crate::memory::Seen")]
pub struct Group {
    pub id: i32,
    pub members: BTreeSet<Id>,
    pub room: RoomName,
    pub last_seen: i32,
    pub centre: Pos,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Phase {
    Forming,
    Approaching,
    /// His towers are spent at his border before the set breaches: a bouncer
    /// steps onto it and back across to the medics, a volley a trip.
    Draining,
    Fighting,
    Pursuing,
    Disengaging,
}

/// Set when a side is priced, never between: `order` is the hostiles' ids,
/// first struck first; `offers` what the price found, the chosen one among
/// them, kept in this tick's heap only (§4) for the observe row to print.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Plan {
    pub tactic: Tactic,
    pub order: Vec<Id>,
    #[serde(skip)]
    pub offers: Vec<Offer>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(from = "crate::memory::Kind", into = "crate::memory::Kind")]
pub enum Objective {
    Defend,
    /// Into its room to the rival structure `target`; the plan's order is the
    /// barriers on the way, first broken first.
    Siege(Id),
    /// A rival's remote, `room`, its economy bodies hunted.
    Sweep,
}

/// A Defend's target is `Group`, and `room` the room held or worked it
/// threatened; a Siege's and a Sweep's `group` is none (0).
#[derive(Clone, Debug, PartialEq, Serialize, Deserialize)]
pub struct Engagement {
    #[serde(flatten)]
    pub objective: Objective,
    pub id: i32,
    pub group: i32,
    pub room: RoomName,
    pub ours: BTreeSet<String>,
    #[serde(flatten)]
    pub plan: Plan,
    pub phase: Phase,
    pub opened: i32,
    pub since: i32,
    pub lost: i32,
    /// The bodies it may lose before it disengages.
    pub budget: i32,
    #[serde(serialize_with = "crate::memory::js_number")]
    pub priority: f64,
    /// Starved: towers and ramparts answer it alone.
    pub unanswered: bool,
    /// Bouncer trips onto his border his towers left untouched, since the last
    /// one they struck: a drain counts them and gives up (§6.2).
    #[serde(default)]
    pub ignored: i32,
    /// The bank it was last priced against; a larger one is an event.
    pub capacity: i32,
    /// What `Exchange` said each time it was answered after standing
    /// unanswered (at open, or a given-up home's re-answered), set against
    /// what happened (§10.1); None while it never was.
    pub predicted: Option<crate::exchange::Outcome>,
}

/// A body asked of `room`'s spawns under `name`, until it stands.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Cast {
    pub engagement: i32,
    pub name: String,
    pub room: RoomName,
    #[serde(with = "crate::memory::letters")]
    pub body: Vec<Part>,
}
