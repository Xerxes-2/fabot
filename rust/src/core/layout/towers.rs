//! Repair is economy work and not here.
use crate::core::engine;
use crate::core::intents::{Intent, TowerAct};
use crate::core::lists;
use crate::core::world::{self, Creep, Pos, Structure};

fn dealt(from: &Pos, creep: &Creep) -> i32 {
    let range = from.range(&creep.pos).unwrap_or(0);
    let body = world::body_kinds(creep);
    let boosts = world::body_boosts(creep);
    engine::damage_taken(&body, &boosts, creep.hits, engine::tower_attack_at(range))
}

/// `towers` each hold the energy for one act.
pub fn fire(towers: &[Structure], hostiles: &[Creep], ours: &[Creep]) -> Vec<Intent> {
    let wounded = lists::sort_by(
        |c: &Creep| (c.hits - c.hits_max, c.name.clone()),
        &ours
            .iter()
            .filter(|c| c.hits < c.hits_max)
            .cloned()
            .collect::<Vec<_>>(),
    )
    .into_iter()
    .next();

    let mut acts: Vec<Intent> = Vec::new();

    for tower in towers {
        if !hostiles.is_empty() {
            let target = lists::sort_by(
                |h: &Creep| (-dealt(&tower.pos, h), h.hits, h.id.clone()),
                hostiles,
            )
            .into_iter()
            .next();

            if let Some(target) = target {
                acts.push(Intent::Tower(tower.id.clone(), TowerAct::Attack(target.id)));
            }
        } else if let Some(creep) = &wounded {
            acts.push(Intent::Tower(
                tower.id.clone(),
                TowerAct::Heal(creep.id.clone()),
            ));
        }
    }

    acts
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::intents::Intent;
    use crate::core::layout::derive;
    use crate::core::layout::fixtures as fx;
    use crate::core::layout::run;
    use crate::core::tiles;
    use crate::core::world::Owner;
    use std::collections::BTreeMap;

    #[test]
    fn a_tower_acts_once_on_a_hostile_in_its_room_while_one_stands_else_heals() {
        let mut rng = fx::Rng::new(13);

        for _ in 0..fx::MAX_TEST {
            let hostiles = rng.choose(0, 255) as u8;
            let wounded = rng.choose(0, 255) as u8;
            let towers = rng.choose(0, 255) as u8;

            let colony = fx::colony(&mut rng);

            let enemies: Vec<world::Creep> = (1..=(hostiles % 3) as i32)
                .map(|i| {
                    fx::creep(
                        &format!("h{}", i),
                        Owner::Theirs("x".to_string()),
                        100,
                        tiles::index(25, i % 40 + 5),
                    )
                })
                .collect();
            let ours: Vec<world::Creep> = (1..=(wounded % 3) as i32)
                .map(|i| {
                    fx::creep(
                        &format!("o{}", i),
                        Owner::Ours,
                        50 + i,
                        tiles::index(25, i % 40 + 5),
                    )
                })
                .collect();

            let stood: Vec<world::Structure> = (1..=(towers % 4) as i32)
                .map(|i| {
                    fx::tower(
                        &format!("t{}", i),
                        tiles::index(10 + i, 10),
                        if i == 1 { 5 } else { 500 },
                    )
                })
                .collect();

            let mut room = colony.room.clone();
            room.structures = stood;
            room.hostiles = enemies.clone();

            let world = fx::world_of(room, Vec::new(), ours.clone());

            let (colonies, _) = derive::colonies(&world, &BTreeMap::new());
            let acts = run::decide_over(&colonies, 0);

            let ids: Vec<String> = acts.iter().map(|i| i.actor()).collect();
            let mut distinct: Vec<String> = Vec::new();
            for id in &ids {
                if !distinct.contains(id) {
                    distinct.push(id.clone());
                }
            }

            assert_eq!(distinct, ids, "a tower acted twice");

            for act in &acts {
                match act {
                    Intent::Tower(id, TowerAct::Attack(target)) => {
                        assert_ne!(id, "t1");
                        assert!(enemies.iter().any(|h| &h.id == target));
                    }
                    Intent::Tower(id, TowerAct::Heal(target)) => {
                        assert_ne!(id, "t1");
                        assert!(enemies.is_empty());
                        assert!(ours.iter().any(|c| &c.id == target));
                    }
                    other => panic!("{} is no tower act", other.actor()),
                }
            }
        }
    }
}
