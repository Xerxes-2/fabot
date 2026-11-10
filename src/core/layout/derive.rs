use std::collections::{BTreeMap, BTreeSet};

use crate::core::engine;
use crate::core::tick::{ColonyPlan, Stage};
use crate::core::tiles::{self, Tile};
use crate::core::vocabulary::Resource;
use crate::core::world::{Creep, Pos, Room, RoomName, Structure, World};

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
}

fn spawns_in(world: &World, room: RoomName) -> Vec<Tile> {
    let mut found: Vec<Tile> = world
        .spawns
        .iter()
        .filter(|s| s.pos.room == room)
        .map(|s| tiles::of_pos(&s.pos))
        .collect();
    found.sort_unstable();
    found
}

fn join(values: &[Tile]) -> String {
    values
        .iter()
        .map(std::string::ToString::to_string)
        .collect::<Vec<String>>()
        .join(",")
}

/// Terrain, sources, minerals and the controller never move, so the key
/// leaves them out.
pub fn key_of(room: &Room, spawns: &[Tile]) -> String {
    join(spawns) + "|" + &join(&ground::obstacles(room))
}

pub fn colonies(
    world: &World,
    owned: &BTreeSet<RoomName>,
    plans: &[ColonyPlan],
    standing: &BTreeMap<RoomName, Standing>,
) -> (Vec<Colony>, BTreeMap<RoomName, Standing>) {
    let mut next: BTreeMap<RoomName, Standing> = BTreeMap::new();
    let mut found: Vec<Colony> = Vec::new();

    for room in owned.iter().filter_map(|name| world.room(*name)) {
        let Some(colony) = plans.iter().find(|p| p.room == room.name) else {
            continue;
        };

        let spawns = spawns_in(world, room.name);
        let key = key_of(room, &spawns);

        let kept = match standing.get(&room.name) {
            Some(s) if s.key == key => s.clone(),
            _ => Standing {
                key,
                plan: ground::of_room(room)
                    .and_then(|ground| plan::of_ground(&world.rules, &ground, &spawns)),
            },
        };

        let plan = match colony.stage {
            Stage::Abandon => None,
            Stage::Exploit => kept.plan.clone(),
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

        towers.sort_by(|a, b| a.id.cmp(&b.id));

        found.push(Colony {
            room: room.name,
            target: colony.target,
            plan,
            census: sites::census_of(room, &ours),
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
        });
    }

    (found, next)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::layout::fixtures as fx;
    use crate::core::layout::shape::Plan;
    use crate::core::world::{Kind, Owner, hostiles};

    fn agree(a: &Option<Plan>, b: &Option<Plan>) -> bool {
        match (a, b) {
            (Some(x), Some(y)) => fx::same(x, y),
            (None, None) => true,
            _ => false,
        }
    }

    #[test]
    fn a_standing_plan_is_the_plan_a_fresh_draw_gives_kept_while_its_key_holds_and_redrawn_when_it_moves()
     {
        let mut rng = fx::Rng::new(12);

        for _ in 0..fx::MAX_TEST {
            let colony = fx::colony(&mut rng);
            let wall = rng.choose(0, tiles::COUNT as i32 - 1);

            let spawns: Vec<crate::core::world::Spawn> = colony
                .spawns
                .iter()
                .enumerate()
                .map(|(i, t)| fx::spawn_named(&format!("p{i}"), *t))
                .collect();

            let world = fx::world_of(colony.room.clone(), spawns.clone(), Vec::new());

            let owned = hostiles::owned(&world.rooms);
            let plans = fx::exploited(&owned);
            let (first, standing) = colonies(&world, &owned, &plans, &BTreeMap::new());
            let (kept, _) = colonies(&world, &owned, &plans, &standing);

            let mut walled = colony.room.clone();
            walled.structures.push(fx::structure(
                "w",
                Kind::Wall,
                Owner::Nobody,
                Tile::of_index(wall.unsigned_abs() as usize % tiles::COUNT),
            ));

            let (moved, _) = colonies(
                &fx::world_of(walled.clone(), spawns.clone(), Vec::new()),
                &owned,
                &plans,
                &standing,
            );
            let (fresh, _) = colonies(
                &fx::world_of(walled, spawns, Vec::new()),
                &owned,
                &plans,
                &BTreeMap::new(),
            );

            let plan_in =
                |cs: &[Colony]| -> Option<Plan> { cs.first().and_then(|c| c.plan.clone()) };

            assert!(agree(&plan_in(&first), &plan_in(&kept)));
            assert!(agree(&plan_in(&moved), &plan_in(&fresh)));
        }
    }
}
