use fabot_base::declarations::MUSTER_RANGE;
/// Ahead of `State` so `State` can hold the standing plan.
use fabot_base::tiles::{self, Tile};
use fabot_base::world::{Pos, RoomName, StructureType};

/// `Level`: from which the engine allows it (its rank among its kind against
/// CONTROLLER_STRUCTURES), or the level a rule gives a road or container.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Item {
    pub kind: StructureType,
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
    pub unfit: Vec<StructureType>,
    /// Where a free fight set waits (`muster`).
    pub muster: Option<Pos>,
}

impl Plan {
    /// The hub's centre: Hivemind's storage location (`ARCHITECTURE.md` §5).
    /// None where a structure takes it, as the controller's container can: a
    /// drop there fills the container and spills beside it unread.
    pub fn pile(&self) -> Option<Pos> {
        let built = |i: &Item| {
            i.tile == self.anchor
                && i.kind != StructureType::Road
                && i.kind != StructureType::Rampart
        };
        (!self.items.iter().any(built)).then(|| tiles::to_pos(self.room, self.anchor))
    }
}

pub type LayoutState = std::collections::BTreeMap<RoomName, Standing>;

#[derive(Clone, Debug, PartialEq)]
pub struct Standing {
    pub key: Vec<Tile>,
    pub plan: Option<Plan>,
    pub drawing: Option<super::plan::Drawing>,
}

/// The towers, planned beside the anchor, still fire at two thirds this far off.
const MUSTER_REACH: i32 = 10;
/// A body held at the muster stays off the exit tiles, which carry it across.
const MUSTER_EDGE: i32 = MUSTER_RANGE + 1;

/// None where every tile near the anchor has a road or a spawn's door within
/// reach: the bodies then wait by a spawn.
pub fn muster(
    room: RoomName,
    anchor: Tile,
    planned: &[&Item],
    open: impl Fn(Tile) -> bool,
) -> Option<Pos> {
    let mut taken = [false; tiles::COUNT];
    let mut barred = [false; tiles::COUNT];
    for i in planned {
        taken[i.tile] = true;
        if i.kind == StructureType::Road {
            barred[i.tile] = true;
        }
        if i.kind == StructureType::Spawn {
            for n in tiles::within(1, i.tile) {
                barred[n] = true;
            }
        }
    }
    let inside = |t: Tile| {
        let span = MUSTER_EDGE..tiles::SIDE - MUSTER_EDGE;
        span.contains(&tiles::x_of(t)) && span.contains(&tiles::y_of(t))
    };
    let square = |t: Tile| tiles::within(MUSTER_RANGE, t).into_iter();
    (tiles::within(MUSTER_REACH, anchor).into_iter())
        .filter(|t| inside(*t) && open(*t) && !taken[*t])
        .filter(|t| square(*t).all(|n| !barred[n]))
        .min_by_key(|t| {
            let crowd = square(*t).filter(|n| taken[*n] || !open(*n)).count();
            (crowd, tiles::range(*t, anchor), *t)
        })
        .map(|t| tiles::to_pos(room, t))
}
