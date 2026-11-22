use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};

use fabot_base::vocabulary::Part;
use fabot_base::world::{Id, Pos, RoomName};
use fabot_tactics::{Offer, Tactic};

#[derive(Clone, Debug, Default, PartialEq)]
pub struct FightState {
    pub groups: Vec<Group>,
    pub engagements: Vec<Engagement>,
    pub casts: Vec<Cast>,
    /// The colonies a siege successor falls due at within a body's spawn time.
    pub due: BTreeSet<RoomName>,
}

#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Group {
    #[serde(flatten)]
    pub centre: Pos,
    pub id: i32,
    #[serde(rename = "seen")]
    pub last_seen: i32,
    pub members: BTreeSet<Id>,
}

impl Group {
    pub fn room(&self) -> RoomName {
        self.centre.room
    }
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
#[serde(from = "Kind", into = "Kind")]
pub enum Objective {
    Defend,
    /// Into its room to the rival structure `target`; the plan's order is the
    /// barriers on the way, first broken first.
    Siege(Id),
    /// A rival's remote, `room`, its economy bodies hunted.
    Sweep,
}

#[derive(Serialize, Deserialize)]
struct Kind {
    #[serde(skip_serializing_if = "Option::is_none")]
    kind: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    target: Option<Id>,
}

impl From<Kind> for Objective {
    fn from(k: Kind) -> Objective {
        match (k.kind.as_deref(), k.target) {
            (Some("siege"), Some(target)) => Objective::Siege(target),
            (Some("sweep"), _) => Objective::Sweep,
            _ => Objective::Defend,
        }
    }
}

impl From<Objective> for Kind {
    fn from(o: Objective) -> Kind {
        let (kind, target) = match o {
            Objective::Defend => (None, None),
            Objective::Siege(target) => (Some("siege"), Some(target)),
            Objective::Sweep => (Some("sweep"), None),
        };
        Kind {
            kind: kind.map(str::to_string),
            target,
        }
    }
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
    pub ours: BTreeMap<String, Slot>,
    #[serde(flatten)]
    pub plan: Plan,
    pub phase: Phase,
    pub opened: i32,
    pub since: i32,
    pub lost: i32,
    /// The bodies it may lose before it disengages.
    pub budget: i32,
    #[serde(serialize_with = "fabot_base::wire::js_number")]
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
    pub predicted: Option<fabot_exchange::Outcome>,
}

/// A body's place in its set, which its successor takes (§6.2's rotation).
pub type Slot = u16;

impl Engagement {
    pub fn free(&self, casts: &[Cast]) -> impl Iterator<Item = Slot> + use<> {
        let held: BTreeSet<Slot> = (self.ours.values().copied())
            .chain(
                casts
                    .iter()
                    .filter(|c| c.engagement == self.id)
                    .map(|c| c.slot),
            )
            .collect();
        (0..).filter(move |s| !held.contains(s))
    }

    pub fn join(&mut self, names: impl IntoIterator<Item = String>, casts: &[Cast]) {
        let joined: Vec<(String, Slot)> = names.into_iter().zip(self.free(casts)).collect();
        self.ours.extend(joined);
    }
}

pub fn enter(phase: Phase, time: i32, e: &Engagement) -> Engagement {
    Engagement {
        phase,
        since: time,
        ..e.clone()
    }
}

/// A body asked of `room`'s spawns under `name`, until it stands.
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub struct Cast {
    pub engagement: i32,
    pub name: String,
    pub slot: Slot,
    pub room: RoomName,
    #[serde(with = "fabot_base::wire::letters")]
    pub body: Vec<Part>,
}
