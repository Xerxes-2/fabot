//! Repair is economy work, but for a rampart about to decay away: a colony
//! may hold no worker for a life.
use crate::core::engine;
use crate::core::intents::{Intent, TowerAct};
use crate::core::vocabulary::{BodyPart, Resource};
use crate::core::world::{self, Creep, Pos, Structure};

fn power(creep: &Creep, part: BodyPart) -> i32 {
    engine::power_at(
        &world::body_kinds(creep),
        &world::body_boosts(creep),
        creep.hits,
    )(part)
}

fn range(a: &Creep, b: &Creep) -> i32 {
    a.pos.range(&b.pos).unwrap_or(i32::MAX)
}

/// The worst case: every healer in reach pre-heals this one target.
fn heal_on(target: &Creep, hostiles: &[Creep]) -> i32 {
    hostiles
        .iter()
        .map(|h| match range(h, target) {
            r if r <= engine::MELEE_RANGE => engine::HEAL_POWER * power(h, BodyPart::Heal),
            r if r <= engine::RANGED_RANGE => engine::RANGED_HEAL_POWER * power(h, BodyPart::Heal),
            _ => 0,
        })
        .sum()
}

fn ours_on(target: &Creep, ours: &[Creep]) -> i32 {
    ours.iter()
        .map(|c| {
            let ranged = engine::RANGED_ATTACK_POWER * power(c, BodyPart::RangedAttack);
            match range(c, target) {
                r if r <= engine::MELEE_RANGE => {
                    engine::ATTACK_POWER * power(c, BodyPart::Attack) + ranged
                }
                r if r <= engine::RANGED_RANGE => ranged,
                _ => 0,
            }
        })
        .sum()
}

fn strikes(hostile: &Creep, struck: &[Pos]) -> bool {
    let melee = power(hostile, BodyPart::Attack) + power(hostile, BodyPart::Work) > 0;
    let ranged = power(hostile, BodyPart::RangedAttack) > 0;
    struck.iter().any(|p| match hostile.pos.range(p) {
        Some(r) if r <= engine::MELEE_RANGE => melee || ranged,
        Some(r) if r <= engine::RANGED_RANGE => ranged,
        _ => false,
    })
}

fn taken(target: &Creep, damage: i32) -> i32 {
    engine::damage_taken(
        &world::body_kinds(target),
        &world::body_boosts(target),
        target.hits,
        damage,
    )
}

/// `towers` each hold the energy for one act. An out-healed body is left so a
/// heal tank drains nothing, but not a striker nor one our creeps fight, which
/// Exchange priced with the towers firing: `heal_on` assumes every healer
/// pre-heals, and one that heals after the shot loses its TOUGH to sustained fire.
pub fn fire(
    towers: &[Structure],
    hostiles: &[Creep],
    ours: &[Creep],
    struck: &[Pos],
    ramparts: &[Structure],
) -> Vec<Intent> {
    let mut targets: Vec<(&Creep, bool, i32, i32, i32)> = hostiles
        .iter()
        .map(|h| {
            let heal = heal_on(h, hostiles);
            let need = engine::damage_for(
                &world::body_kinds(h),
                &world::body_boosts(h),
                h.hits,
                h.hits + heal,
            );
            let dealt = ours_on(h, ours);
            (h, dealt > 0 || strikes(h, struck), heal, dealt, need)
        })
        .collect();
    targets.sort_by_key(|(h, urgent, _, dealt, need)| (!urgent, need - dealt, h.id.clone()));

    let mut free: Vec<&Structure> = towers.iter().collect();
    let mut acts: Vec<Intent> = Vec::new();

    for (target, urgent, heal, dealt, need) in targets {
        let at = |t: &Structure| t.pos.range(&target.pos).map_or(0, engine::tower_attack_at);
        free.sort_by_key(|t| (-at(t), t.id.clone()));
        let every: i32 = free.iter().map(|t| at(t)).sum();
        if dealt >= need || !urgent && taken(target, dealt + every) <= heal {
            continue;
        }
        let mut landed = dealt;
        let mut committed = 0;
        for tower in &free {
            if landed >= need {
                break;
            }
            landed += at(tower);
            committed += 1;
            acts.push(Intent::Tower(
                tower.id.clone(),
                TowerAct::Attack(target.id.clone()),
            ));
        }
        free.drain(..committed);
    }

    if let Some(wounded) = ours
        .iter()
        .filter(|c| c.hits < c.hits_max)
        .min_by_key(|c| (c.hits - c.hits_max, &c.name))
    {
        acts.extend(
            free.iter()
                .map(|t| Intent::Tower(t.id.clone(), TowerAct::Heal(wounded.id.clone()))),
        );
    } else if let Some(dying) = (ramparts.iter())
        .filter(|r| r.hits <= 2 * engine::RAMPART_DECAY_AMOUNT)
        .min_by_key(|r| (r.hits, &r.id))
        && let Some(tower) = (free.iter())
            .find(|t| t.store.amount_of(&Resource::Energy) >= engine::TOWER_CAPACITY / 2)
    {
        acts.push(Intent::Tower(
            tower.id.clone(),
            TowerAct::Repair(dying.id.clone()),
        ));
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
    use crate::core::tiles::{self};
    use crate::core::world::Owner;
    use std::collections::BTreeMap;

    #[test]
    fn a_tower_acts_once_shooting_a_hostile_in_its_room_or_healing_ours() {
        let mut rng = fx::Rng::new(13);

        for _ in 0..fx::MAX_TEST {
            let hostiles = rng.choose(0, 255) as u8;
            let wounded = rng.choose(0, 255) as u8;
            let towers = rng.choose(0, 255) as u8;

            let colony = fx::colony(&mut rng);

            let enemies: Vec<world::Creep> = (1..=i32::from(hostiles % 3))
                .map(|i| {
                    fx::creep(
                        &format!("h{i}"),
                        Owner::Stranger("x".to_string()),
                        100,
                        tiles::index(25, i % 40 + 5),
                    )
                })
                .collect();
            let ours: Vec<world::Creep> = (1..=i32::from(wounded % 3))
                .map(|i| {
                    fx::creep(
                        &format!("o{i}"),
                        Owner::Ours,
                        50 + i,
                        tiles::index(25, i % 40 + 5),
                    )
                })
                .collect();

            let stood: Vec<world::Structure> = (1..=i32::from(towers % 4))
                .map(|i| {
                    fx::tower(
                        &format!("t{i}"),
                        tiles::index(10 + i, 10),
                        if i == 1 { 5 } else { 500 },
                    )
                })
                .collect();

            let mut room = colony.room.clone();
            room.structures = stood;
            room.others = enemies.clone();

            let world = fx::world_of(room, Vec::new(), ours.clone());

            let owned = crate::core::world::hostiles::owned(&world.rooms);
            let plans = fx::exploited(&owned);
            let (colonies, _) = derive::colonies(&world, &owned, &plans, &BTreeMap::new());
            let acts = run::decide_over(&world.rules, &colonies, 0);

            let ids: Vec<String> = acts
                .iter()
                .map(crate::core::intents::Intent::actor)
                .collect();
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
                        assert!(ours.iter().any(|c| &c.id == target));
                    }
                    other => panic!("{} is no tower act", other.actor()),
                }
            }
        }
    }
}
