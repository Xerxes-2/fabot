use std::collections::{BTreeMap, BTreeSet};

use crate::vocabulary::{Compound, Resource};
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
    /// Where its haul is banked while no storage stands (`ARCHITECTURE.md` §5).
    pub pile: Option<Pos>,
    /// Where its free fight bodies wait (`Plan::muster`).
    pub muster: Option<Pos>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Node {
    Colony(RoomName),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Flow {
    pub what: Resource,
    pub to: Node,
    pub stock: i32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declarations {
    /// The creep lifetimes of tower fire a colony's reserve holds.
    pub reserve_lifetimes: f64,
    pub outposts: Vec<Outpost>,
    /// Declared as an outpost is, so a claimer walks to one unseen.
    pub claims: Vec<Outpost>,
    pub allies: BTreeSet<String>,
    pub enemies: BTreeSet<String>,
    /// The rival rooms whose towers and economy are razed.
    pub sieges: Vec<RoomName>,
    pub colonies: Vec<ColonyPlan>,
    pub flows: Vec<Flow>,
    /// Rooms no fight body walks through on its way: a rival's towers.
    pub avoid: BTreeSet<RoomName>,
    /// The operator's tactic rules, per rival and room (§6.3).
    pub tactics: Vec<TacticRule>,
}

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
            let open = !closed.contains(room);
            if !open {
                refused.push(*room);
            }
            open
        };
        self.outposts.retain(|o| open(&o.room));
        self.claims.retain(|o| open(&o.room));
        self.sieges.retain(|room| open(room));
        refused
    }

    pub fn planned(
        mut self,
        (plans, outposts, claim): (Vec<ColonyPlan>, Vec<Outpost>, Option<Outpost>),
    ) -> Declarations {
        (self.claims).extend(claim.filter(|c| !self.claims.iter().any(|o| o.room == c.room)));
        let declared: BTreeSet<RoomName> = self.colonies.iter().map(|p| p.room).collect();
        (self.colonies).extend(plans.into_iter().filter(|p| !declared.contains(&p.room)));
        let declared: BTreeSet<RoomName> = self.outposts.iter().map(|o| o.room).collect();
        (self.outposts).extend(outposts.into_iter().filter(|o| !declared.contains(&o.room)));
        self
    }

    #[inline]
    pub fn abandoning(&self, room: RoomName) -> bool {
        (self.colonies.iter()).any(|p| p.room == room && p.stage == Stage::Abandon)
    }

    pub fn labs(&self) -> Option<(RoomName, BTreeMap<Compound, i32>)> {
        let Node::Colony(home) = self.flows.first()?.to;
        let targets = (self.flows.iter())
            .filter_map(|f| match f.what {
                Resource::Compound(c) if f.to == Node::Colony(home) && f.stock > 0 => {
                    Some((c, f.stock))
                }
                _ => None,
            })
            .collect();
        Some((home, targets))
    }
}

pub fn undeclared() -> Declarations {
    Declarations {
        reserve_lifetimes: 2.0,
        outposts: Vec::new(),
        claims: Vec::new(),
        allies: BTreeSet::new(),
        enemies: BTreeSet::new(),
        sieges: Vec::new(),
        colonies: Vec::new(),
        flows: Vec::new(),
        avoid: BTreeSet::new(),
        tactics: Vec::new(),
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

/// A declared siege's room stands further off than any room worked.
pub const SIEGE_HOPS: i32 = 10;

/// A free fight body holds within this of its colony's muster.
pub const MUSTER_RANGE: i32 = 2;
