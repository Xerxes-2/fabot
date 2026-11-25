use fabot_base::declarations::MUSTER_RANGE;
use fabot_base::engine;
/// Ahead of `State` so `State` can hold the standing plan.
use fabot_base::tiles::{self, Tile};
use fabot_base::world::{Built, Pos, RoomName, StructureType};

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

    pub fn walls(&self, t: Tile) -> bool {
        (self.items.iter().chain(&self.ring)).any(|i| i.tile == t && i.kind == StructureType::Wall)
    }

    /// Hivemind's navigation matrix: the ring's walls count whether or not
    /// Strategy builds the ring yet, since a room it later keeps walls them.
    pub fn solid(&self) -> impl Iterator<Item = Tile> + '_ {
        (self.items.iter().chain(&self.ring))
            .filter(|i| !i.kind.walkable())
            .map(|i| i.tile)
    }

    /// The directions a spawn at `spawn` bears a body onto: `open` and walked
    /// from to the core now, first those no planned structure or post takes
    /// (a post's holder never steps aside); the rest after, or the spawn
    /// would wait while one of them is free.
    pub fn doors(&self, spawn: Tile, open: impl Fn(Tile) -> bool) -> Vec<i32> {
        let mut shut = [false; tiles::COUNT];
        for i in self.items.iter().chain(&self.ring) {
            shut[i.tile] |= !i.kind.walkable() || i.kind == StructureType::Container;
        }
        let steps = tiles::reach([self.anchor], &open);
        let mut doors: Vec<Tile> = (tiles::around(spawn).iter().copied())
            .filter(|t| open(*t) && steps[*t] != tiles::UNREACHED)
            .collect();
        doors.sort_by_key(|t| shut[*t]);
        doors
            .into_iter()
            .map(|t| tiles::direction(spawn, t))
            .collect()
    }
}

pub type LayoutState = std::collections::BTreeMap<RoomName, Standing>;

#[derive(Clone, Debug, PartialEq)]
pub struct Standing {
    pub key: Vec<Tile>,
    pub plan: Option<Plan>,
    pub drawing: Option<super::plan::Drawing>,
    /// Drawn on a heap that held no plan for the room (a reset, or a new
    /// colony), not over a changed ground.
    pub cold: bool,
}

/// The towers, planned beside the anchor, still fire at two thirds this far off.
const MUSTER_REACH: i32 = 10;
/// A body held at the muster stays off the exit tiles, which carry it across.
const MUSTER_EDGE: i32 = MUSTER_RANGE + 1;

/// A road in reach is ranked, not barred, so a plan dense with roads still
/// musters; a spawn's door is barred, as a body waiting there blocks it.
pub fn muster(
    room: RoomName,
    anchor: Tile,
    planned: &[&Item],
    open: impl Fn(Tile) -> bool,
) -> Option<Pos> {
    let mut taken = [false; tiles::COUNT];
    let mut paved = [false; tiles::COUNT];
    let mut doorway = [false; tiles::COUNT];
    for i in planned {
        taken[i.tile] = true;
        paved[i.tile] |= i.kind == StructureType::Road;
        if i.kind == StructureType::Spawn {
            for n in tiles::all().filter(|n| engine::spawn_door(i.tile, *n)) {
                doorway[n] = true;
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
        .filter(|t| square(*t).all(|n| !doorway[n]))
        .min_by_key(|t| {
            let roads = square(*t).filter(|n| paved[*n]).count();
            let crowd = square(*t).filter(|n| taken[*n] || !open(*n)).count();
            (roads, crowd, tiles::range(*t, anchor), *t)
        })
        .map(|t| tiles::to_pos(room, t))
}

#[cfg(test)]
mod tests {
    use crate::fixtures as fx;
    use crate::place::WALL;
    use fabot_base::tiles::Tile;
    use fabot_base::world::{Built, StructureType};

    #[test]
    fn every_planned_spawn_has_a_door() {
        for colony in fx::samples(5).take(fx::MAX_TEST) {
            let Some((ground, plan)) = fx::plan_of(&colony) else {
                continue;
            };
            let rocks = ground.sources.iter().chain(&ground.minerals);
            let rocks: Vec<Tile> = rocks.chain(&ground.controller).copied().collect();
            let road =
                |t| (plan.items.iter()).any(|i| i.tile == t && i.kind == StructureType::Road);
            let taken = |t| {
                (plan.items.iter().chain(&plan.ring)).any(|i| {
                    i.tile == t && (!i.kind.walkable() || i.kind == StructureType::Container)
                })
            };
            let open =
                |t| (ground.terrain[t] != WALL || road(t)) && !rocks.contains(&t) && !taken(t);
            for s in plan.items.iter().filter(|i| i.kind == StructureType::Spawn) {
                assert!(!plan.doors(s.tile, open).is_empty(), "spawn at {}", s.tile);
            }
        }
    }
}
