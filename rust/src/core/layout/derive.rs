use std::collections::BTreeMap;

use crate::core::engine;
use crate::core::tiles;
use crate::core::vocabulary::Resource;
use crate::core::world::{Creep, Room, Structure, World};

use super::ground;
use super::plan;
use super::shape::Standing;
use super::sites::{self, Census};

pub struct Colony {
    pub room: String,
    pub level: i32,
    pub plan: Option<super::shape::Plan>,
    pub census: Census,
    pub towers: Vec<Structure>,
    pub hostiles: Vec<Creep>,
    pub ours: Vec<Creep>,
}

fn spawns_in(world: &World, room: &str) -> Vec<i32> {
    let mut found: Vec<i32> = world
        .spawns
        .iter()
        .filter(|s| s.pos.room == room)
        .map(|s| tiles::of_pos(&s.pos))
        .collect();
    found.sort_unstable();
    found
}

fn join(values: &[i32]) -> String {
    values
        .iter()
        .map(|v| v.to_string())
        .collect::<Vec<String>>()
        .join(",")
}

/// Terrain, sources, minerals and the controller never move, so the key
/// leaves them out.
pub fn key_of(room: &Room, spawns: &[i32]) -> String {
    join(spawns) + "|" + &join(&ground::obstacles(room))
}

pub fn colonies(
    world: &World,
    standing: &BTreeMap<String, Standing>,
) -> (Vec<Colony>, BTreeMap<String, Standing>) {
    let mut next: BTreeMap<String, Standing> = BTreeMap::new();
    let mut found: Vec<Colony> = Vec::new();

    let mut rooms: Vec<&Room> = world.rooms.iter().collect();
    rooms.sort_by_key(|r| r.name.clone());

    for room in rooms {
        let level = match &room.controller {
            Some(c) if c.owner.is_ours() => c.level,
            _ => continue,
        };

        let spawns = spawns_in(world, &room.name);
        let key = key_of(room, &spawns);

        let kept = match standing.get(&room.name) {
            Some(s) if s.key == key => s.clone(),
            _ => Standing {
                key,
                plan: ground::of_room(room).and_then(|ground| plan::of_ground(&ground, &spawns)),
            },
        };

        let plan = kept.plan.clone();
        next.insert(room.name.clone(), kept);

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

        towers.sort_by_key(|t| t.id.clone());

        found.push(Colony {
            room: room.name.clone(),
            level,
            plan,
            census: sites::census_of(room, &ours),
            towers,
            hostiles: room.hostiles.clone(),
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
    use crate::core::world::{Kind, Owner};
    use std::collections::BTreeMap;

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
                .map(|(i, t)| fx::spawn_named(&format!("p{}", i), *t))
                .collect();

            let world = fx::world_of(colony.room.clone(), spawns.clone(), Vec::new());

            let (first, standing) = colonies(&world, &BTreeMap::new());
            let (kept, _) = colonies(&world, &standing);

            let mut walled = colony.room.clone();
            walled.structures.push(fx::structure(
                "w",
                Kind::Wall,
                Owner::Nobody,
                wall.abs() % tiles::COUNT as i32,
            ));

            let (moved, _) = colonies(&fx::world_of(walled.clone(), spawns.clone(), Vec::new()), &standing);
            let (fresh, _) = colonies(&fx::world_of(walled, spawns, Vec::new()), &BTreeMap::new());

            let plan_in = |cs: &[Colony]| -> Option<Plan> { cs.first().and_then(|c| c.plan.clone()) };

            assert!(agree(&plan_in(&first), &plan_in(&kept)));
            assert!(agree(&plan_in(&moved), &plan_in(&fresh)));
        }
    }
}
