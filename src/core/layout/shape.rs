use std::borrow::Cow;

/// Ahead of `State` so `State` can hold the standing plan.
use crate::core::tiles::Tile;
use crate::core::world::{Kind, RoomName};

/// `Level`: from which the engine allows it (its rank among its kind against
/// CONTROLLER_STRUCTURES), or the level a rule gives a road or container.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Item {
    pub kind: Kind,
    pub tile: Tile,
    pub level: i32,
}

/// `items` in the order they are placed.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Plan {
    pub room: RoomName,
    pub anchor: Tile,
    pub items: Vec<Item>,
    /// Built only while Strategy declares it (`ColonyPlan.ring`).
    pub ring: Vec<Item>,
    pub unfit: Vec<Cow<'static, str>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Standing {
    pub key: String,
    pub plan: Option<Plan>,
}
