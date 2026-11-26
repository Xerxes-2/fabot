use std::collections::BTreeSet;

use crate::world::{Id, Pos, RoomName};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declared {
    pub id: Id,
    pub pos: Pos,
}

/// What is known of a room before it is seen.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Outpost {
    pub room: RoomName,
    pub controller: Declared,
    pub sources: Vec<Declared>,
    pub reserved: bool,
}

#[derive(Clone, Debug, PartialEq, Eq, serde::Deserialize)]
pub struct TacticRule {
    pub rival: Option<String>,
    pub room: Option<RoomName>,
    pub pin: Option<String>,
    #[serde(default)]
    pub forbid: Vec<String>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Stage {
    Exploit,
    /// Emptied, razed and unclaimed.
    Abandon,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ColonyPlan {
    pub room: RoomName,
    pub stage: Stage,
    pub target: i32,
    /// The stock its economy keeps before it spends a surplus.
    pub reserve: i32,
    pub rampart_hits: i32,
    pub ring: bool,
    pub owes: bool,
    pub fights: bool,
    /// Where its haul is banked while no storage stands (`ARCHITECTURE.md` §5).
    pub pile: Option<Pos>,
    pub muster: Option<Pos>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum OpKind {
    Defend,
    Sweep,
    Stronghold,
    /// A room whose loot Economy hauls home.
    Pickup,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Operation {
    pub kind: OpKind,
    pub room: RoomName,
    pub priority: f64,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Declarations {
    pub tuning: crate::tuning::Tuning,
    pub outposts: Vec<Outpost>,
    /// Declared as an outpost is, so a claimer walks to one unseen.
    pub claims: Vec<Outpost>,
    pub allies: BTreeSet<String>,
    pub enemies: BTreeSet<String>,
    pub colonies: Vec<ColonyPlan>,
    /// Rooms no fight body walks through on its way: a rival's towers.
    pub avoid: BTreeSet<RoomName>,
    pub tactics: Vec<TacticRule>,
    /// Strategy's rows: the rooms each fight may open on, ranked (§7.3).
    pub operations: Vec<Operation>,
    /// `debug.idle`: each idle body says why, off by default as a say costs CPU.
    pub says_idle: bool,
}

pub type Planned = (Vec<ColonyPlan>, Vec<Outpost>, Option<Outpost>, Vec<Operation>);

/// What `refuse` dropped: the rooms the engine keeps a creep out
/// of, and the tactic rules no name answers to.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Refused {
    pub rooms: Vec<RoomName>,
    pub rules: Vec<String>,
}

impl Declarations {
    pub fn refuse_rooms(&mut self, closed: &BTreeSet<RoomName>) -> Vec<RoomName> {
        let mut refused = Vec::new();
        let mut open = |room: &RoomName| {
            refused.extend(closed.get(room).copied());
            !closed.contains(room)
        };
        self.outposts.retain(|o| open(&o.room));
        self.claims.retain(|o| open(&o.room));
        refused
    }

    pub fn planned(mut self, (plans, outposts, claim, operations): Planned) -> Declarations {
        self.operations = operations;
        (self.claims).extend(claim.filter(|c| !self.claims.iter().any(|o| o.room == c.room)));
        let declared: BTreeSet<RoomName> = self.colonies.iter().map(|p| p.room).collect();
        (self.colonies).extend(plans.into_iter().filter(|p| !declared.contains(&p.room)));
        let declared: BTreeSet<RoomName> = self.outposts.iter().map(|o| o.room).collect();
        (self.outposts).extend(outposts.into_iter().filter(|o| !declared.contains(&o.room)));
        self
    }

    pub fn rooms(&self, kind: OpKind) -> impl Iterator<Item = RoomName> + '_ {
        (self.operations.iter()).filter(move |o| o.kind == kind).map(|o| o.room)
    }

    pub fn priority(&self, kind: OpKind, room: RoomName) -> Option<f64> {
        (self.operations.iter()).find(|o| o.kind == kind && o.room == room).map(|o| o.priority)
    }

    #[inline]
    pub fn abandoning(&self, room: RoomName) -> bool {
        (self.colonies.iter()).any(|p| p.room == room && p.stage == Stage::Abandon)
    }
}

/// A body cast under this name is Fight's for its life, so no creep is in
/// both Fight and Economy (`ARCHITECTURE.md` §2).
pub const FIGHT_PREFIX: &str = "fight-";

pub fn fights(name: &str) -> bool {
    name.starts_with(FIGHT_PREFIX)
}

/// The route search needs a bound; nothing is worked further than this.
pub const REACH_HOPS: i32 = 3;

/// A sweep, stronghold or scout walks further off than any room worked.
pub const FAR_HOPS: i32 = 10;

pub const MUSTER_RANGE: i32 = 2;
