/// What crosses a tick (`ARCHITECTURE.md` §4); each field has one owner.
use std::collections::BTreeSet;

use super::economy::EconomyState;
use super::fight::shape::FightState;
use super::layout::shape::LayoutState;
use super::strategy::StrategyState;

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
