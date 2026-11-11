use std::borrow::Cow;

/// Ahead of `State` so `State` can hold the standing plan.
use crate::core::tiles::{self, Tile};
use crate::core::world::{Kind, Pos, RoomName};

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

impl Plan {
    /// The hub's centre: Hivemind's storage location (`ARCHITECTURE.md` §5).
    /// None where a structure takes it, as the controller's container can: a
    /// drop there fills the container and spills beside it unread.
    pub fn pile(&self) -> Option<Pos> {
        let built = |i: &Item| i.tile == self.anchor && !i.kind.is_road() && !i.kind.is_rampart();
        (!self.items.iter().any(built)).then(|| tiles::to_pos(self.room, self.anchor))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Standing {
    pub key: String,
    pub plan: Option<Plan>,
}
