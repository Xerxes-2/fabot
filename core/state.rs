/// What crosses a tick (`ARCHITECTURE.md` §4); each field has one owner.
use std::collections::{BTreeMap, BTreeSet};

use super::economy::task::Assignment;
use super::fight::shape::{Cast, Engagement, Group};
use super::intel::Scout;
use super::layout::shape::Standing;
use super::strategy::expand::Expansions;
use super::world::RoomName;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct State {
    /// A module named here is skipped and its last output not replayed.
    pub off: BTreeSet<String>,
    pub assignments: BTreeMap<String, Assignment>,
    /// Heap only, never in Memory: empty after a reset, so the first tick redraws.
    pub layout: BTreeMap<RoomName, Standing>,
    pub groups: Vec<Group>,
    pub engagements: Vec<Engagement>,
    pub casts: Vec<Cast>,
    /// The colonies a siege successor falls due at within a body's spawn time.
    pub due: BTreeSet<RoomName>,
    /// Strategy's `Scout room` declarations, rebuilt each tick before Economy reads them.
    pub scouts: Vec<Scout>,
    /// Strategy's sweep candidates this tick, the stalest seen first.
    pub preyed: Vec<RoomName>,
    pub spare: BTreeMap<RoomName, f64>,
    pub expansion: Expansions,
}
