/// What crosses a tick (`ARCHITECTURE.md` §4); each field has one owner.
use std::collections::BTreeSet;

use super::strategy::StrategyState;
use fabot_economy::EconomyState;
use fabot_fight::shape::FightState;
use fabot_layout::shape::LayoutState;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct State {
    /// A module named here is skipped and its last output not replayed.
    pub off: BTreeSet<String>,
    pub economy: EconomyState,
    /// Heap only, never in Memory: empty after a reset, so the first tick redraws.
    pub layout: LayoutState,
    pub fight: FightState,
    pub strategy: StrategyState,
}
