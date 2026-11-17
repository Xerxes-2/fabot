use std::collections::{BTreeMap, BTreeSet};

use crate::engine;
use crate::tick::{ColonyPlan, Stage};
use crate::tiles::{self, Tile, TileSet};
use crate::vocabulary::Resource;
use crate::world::{Creep, Id, Pos, RoomName, Structure, World};

use super::ground;
use super::plan;
use super::shape::Standing;
use super::sites::{self, Census};

pub struct Colony {
    pub room: RoomName,
    pub target: i32,
    pub plan: Option<super::shape::Plan>,
    pub census: Census,
    pub towers: Vec<Structure>,
    pub hostiles: Vec<Creep>,
    /// Not a road or a container: anything walking by stands beside one.
    pub struck: Vec<Pos>,
    pub ours: Vec<Creep>,
    pub ramparts: Vec<Structure>,
    pub buried: Vec<Id>,
    pub astray: Vec<Id>,
}

/// `ARCHITECTURE.md` §5: Hivemind's search spread over ticks.
const VARIATIONS_PER_TICK: usize = 4;

pub fn colonies(
    world: &World,
    owned: &BTreeSet<RoomName>,
    plans: &[ColonyPlan],
    mut standing: BTreeMap<RoomName, Standing>,
) -> (Vec<Colony>, BTreeMap<RoomName, Standing>) {
    let mut next: BTreeMap<RoomName, Standing> = BTreeMap::new();
    let mut found: Vec<Colony> = Vec::new();
    let mut budget = VARIATIONS_PER_TICK;

    for room in owned.iter().filter_map(|name| world.room(*name)) {
        let Some(colony) = plans.iter().find(|p| p.room == room.name) else {
            continue;
        };

        // Terrain, sources, minerals and the controller never move, so the
        // key leaves them out.
        let key = ground::obstacles(room);

        let held = standing.remove(&room.name).filter(|s| s.key == key);
        let ground = (held.as_ref())
            .is_none_or(|s| s.drawing.is_some())
            .then(|| ground::of_room(room))
            .flatten();
        let mut kept = held.unwrap_or_else(|| Standing {
            key,
            plan: None,
            drawing: ground.as_ref().map(plan::Drawing::of),
        });
        if let (Some(d), Some(g)) = (&mut kept.drawing, &ground)
            && let plan::Drawn::Done(done) = d.advance(&world.rules, g, &mut budget)
        {
            kept.plan = done;
            kept.drawing = None;
        }

        let plan = match colony.stage {
            Stage::Abandon => None,
            Stage::Exploit => kept.plan.clone().map(|mut plan| {
                if colony.ring {
                    plan.items.append(&mut plan.ring);
                }
                plan
            }),
        };
        next.insert(room.name, kept);

        let ours: Vec<Creep> = world
            .creeps
            .iter()
            .filter(|creep| creep.pos.room == room.name)
            .cloned()
            .collect();

        let mut towers: Vec<Structure> = room
            .structures
            .iter()
            .filter(|s| {
                s.kind.is_tower()
                    && s.owner.is_ours()
                    && s.store.amount_of(&Resource::Energy) >= engine::TOWER_ENERGY_COST
            })
            .cloned()
            .collect();

        towers.sort_by_key(|a| a.id);

        let blocked: TileSet = (room.structures.iter())
            .filter(|s| s.owner.is_ours() && !s.walkable)
            .map(|s| tiles::of_pos(&s.pos))
            .collect();
        let census = sites::census_of(room, &ours);
        let astray: Vec<Tile> = match &plan {
            Some(plan) => (sites::astray(plan, &census).into_iter())
                .filter(|(rock, _)| {
                    (census.rocked.iter())
                        .any(|(r, p)| r == rock && sites::posted(plan, *p) && census.site[*p] == 0)
                })
                .map(|(_, c)| c)
                .collect(),
            None => Vec::new(),
        };

        found.push(Colony {
            room: room.name,
            target: colony.target,
            plan,
            census,
            towers,
            hostiles: room.hostiles().cloned().collect(),
            struck: room
                .structures
                .iter()
                .filter(|s| s.owner.is_ours() && !s.kind.is_road() && !s.kind.is_container())
                .map(|s| &s.pos)
                .chain(ours.iter().map(|c| &c.pos))
                .copied()
                .collect(),
            ours,
            ramparts: (room.structures.iter())
                .filter(|s| s.kind.is_rampart() && s.owner.is_ours())
                .cloned()
                .collect(),
            buried: (room.structures.iter())
                .filter(|r| r.kind.is_road() && blocked.contains(tiles::of_pos(&r.pos)))
                .map(|r| r.id)
                .collect(),
            astray: (room.structures.iter())
                .filter(|c| c.kind.is_container() && astray.contains(&tiles::of_pos(&c.pos)))
                .map(|c| c.id)
                .collect(),
        });
    }

    (found, next)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::layout::fixtures as fx;
    use crate::layout::shape::Plan;
    use crate::tiles;
    use crate::world::{Kind, Owner, hostiles};

    fn agree(a: &Option<Plan>, b: &Option<Plan>) -> bool {
        match (a, b) {
            (Some(x), Some(y)) => fx::same(x, y),
            (None, None) => true,
            _ => false,
        }
    }

    fn settled(
        world: &World,
        standing: BTreeMap<RoomName, Standing>,
    ) -> BTreeMap<RoomName, Standing> {
        let owned = hostiles::owned(&world.rooms);
        let plans = fx::exploited(&owned);
        let mut standing = standing;
        loop {
            let (_, next) = colonies(world, &owned, &plans, standing.clone());
            standing = next;
            if standing.values().all(|s| s.drawing.is_none()) {
                return standing;
            }
        }
    }

    #[test]
    fn a_standing_plan_is_kept_while_its_key_holds_and_drawn_afresh_when_the_ground_moves() {
        let mut rng = fx::Rng::new(12);

        for _ in 0..4 {
            let colony = fx::colony(&mut rng);
            let wall = rng.choose(0, tiles::COUNT as i32 - 1);
            let world = fx::world_of(colony.room.clone(), Vec::new(), Vec::new());
            let standing = settled(&world, BTreeMap::new());
            let plan_of =
                |s: &BTreeMap<RoomName, Standing>| s.values().next().and_then(|s| s.plan.clone());

            let owned = hostiles::owned(&world.rooms);
            let (_, kept) = colonies(&world, &owned, &fx::exploited(&owned), standing.clone());
            assert!(agree(&plan_of(&standing), &plan_of(&kept)));

            let mut ringed = colony.room.clone();
            ringed.structures.push(fx::structure(
                "r",
                Kind::Wall,
                Owner::Nobody,
                tiles::index(25, 25),
            ));
            let ringed = fx::world_of(ringed, Vec::new(), Vec::new());
            let (_, kept) = colonies(&ringed, &owned, &fx::exploited(&owned), standing.clone());
            assert!(agree(&plan_of(&standing), &plan_of(&kept)));

            for edge in [tiles::index(0, 25), tiles::index(25, 49)] {
                let mut edged = colony.room.clone();
                let wall = fx::structure("e", Kind::Wall, Owner::Nobody, edge);
                edged.structures.push(wall);
                assert_ne!(ground::obstacles(&edged), ground::obstacles(&colony.room));
            }

            let mut walled = colony.room.clone();
            walled.structures.push(fx::structure(
                "w",
                Kind::Spawn,
                Owner::Stranger("x".into()),
                Tile::of_index(wall.unsigned_abs() as usize % tiles::COUNT),
            ));
            let walled = fx::world_of(walled, Vec::new(), Vec::new());
            let moved = settled(&walled, standing);
            let fresh = settled(&walled, BTreeMap::new());
            assert!(agree(&plan_of(&moved), &plan_of(&fresh)));
        }
    }
}
