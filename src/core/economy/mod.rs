//! The task pool and matcher.
//!
//! `decide` is `run.rs`'s: one `Derived` built once from the world, the pool,
//! the match, the quota, and the intents they imply. `tick::MODULES` registers
//! it as `"economy"` in `Main`.
pub mod bodies;
pub mod derived;
pub mod labs;
pub mod matcher;
pub mod pool;
pub mod price;
pub mod quota;
pub mod run;
pub mod task;

pub use run::decide;

#[cfg(test)]
pub(crate) mod dice {
    use std::collections::BTreeSet;

    use crate::core::engine;
    use crate::core::tick::{Declared, Outpost};
    use crate::core::vocabulary::{BodyPart, Boost, Compound, Ore, Resource, part_count_in};
    use crate::core::world::{
        Controller, Creep, Kind, Mineral, Owner, Part, Pile, Pos, Remains, Room, Site, Source,
        Spawn, Store, Structure, World,
    };

    use super::bodies;
    use super::derived::Body;
    use super::pool::{Capacity, Draw, Offer};
    use super::task::{self, Task};

    pub use crate::core::tests::Dice;

    pub fn object_id(dice: &mut Dice) -> String {
        let n = dice.between(1, 12);
        (0..n)
            .map(|_| "0123456789abcdef".as_bytes()[dice.below(16) as usize] as char)
            .collect()
    }

    pub fn resources() -> Vec<Resource> {
        vec![
            Resource::Energy,
            Resource::Ore(Ore::Hydrogen),
            Resource::Ore(Ore::Catalyst),
            Resource::Compound(Compound::UtriumAcid),
            Resource::Other("T".to_string()),
        ]
    }

    /// The scenes' tile: every body and every offer stand on it.
    pub fn at() -> Pos {
        Pos::new("r", 25, 25)
    }

    pub fn task(dice: &mut Dice, target: &str) -> Task {
        let resource = resources()[dice.below(5) as usize].clone();
        match dice.between(0, 9) {
            0 => Task::Harvest(target.to_string()),
            1 => Task::Withdraw(target.to_string(), resource),
            2 => Task::Pickup(target.to_string(), resource),
            3 => Task::Refill(target.to_string(), resource),
            4 => Task::Build(target.to_string()),
            5 => Task::Repair(target.to_string()),
            6 => Task::Upgrade(target.to_string()),
            7 => Task::Reserve(target.to_string()),
            8 => Task::Scout(target.to_string()),
            _ => Task::Claim(target.to_string()),
        }
    }

    pub fn body(dice: &mut Dice, i: usize) -> Body {
        let work = dice.between(0, 6);
        let carry = dice.between(0, 6);
        let moves = dice.between(0, 6);
        let claim = if dice.chance(5) {
            dice.between(1, 3)
        } else {
            0
        };
        let capacity = carry * engine::CARRY_PART_CAPACITY;
        let energy = dice.between(0, capacity);
        let ore = if dice.chance(5) {
            dice.between(0, capacity - energy)
        } else {
            0
        };
        let life = dice.between(1, engine::CREEP_LIFETIME);

        let held: Vec<(Resource, i32)> = vec![
            (Resource::Energy, energy),
            (Resource::Ore(Ore::Hydrogen), ore),
        ]
        .into_iter()
        .filter(|(_, n)| *n > 0)
        .collect();

        let store = Store { held, capacity };

        Body {
            name: format!("b{i}"),
            pos: at(),
            work,
            carry,
            moves,
            claim,
            size: work + carry + moves + claim,
            energy,
            free: store.free(),
            store,
            ticks_to_live: life,
            pattern: bodies::pattern_of_parts(work, carry, moves, claim),
        }
    }

    pub fn offer(dice: &mut Dice, i: usize) -> Offer {
        let task = task(dice, &format!("o{i}"));
        let priority = dice.between(-10, 40);
        let draw = dice.pick(&[Draw::Open, Draw::Spenders, Draw::Buffer]);

        let capacity = match dice.below(5) {
            0 => Capacity::Unbounded,
            1 => Capacity::Bodies(dice.between(0, 3)),
            2 => Capacity::Work(dice.between(0, 12)),
            3 => Capacity::Load(dice.between(0, 400)),
            _ => {
                let seats = dice.between(0, 4);
                Capacity::Seated {
                    seats,
                    posts: seats.min(dice.between(0, 1)),
                    work: dice.between(0, 12),
                }
            }
        };

        Offer {
            id: task::task_id(&task),
            task,
            target: at(),
            priority,
            capacity,
            draw,
            post: None,
            shut: BTreeSet::new(),
        }
    }

    pub fn store(held: Vec<(Resource, i32)>, capacity: i32) -> Store {
        Store { held, capacity }
    }

    pub fn pos(dice: &mut Dice, room: &str) -> Pos {
        Pos::new(room, dice.between(1, 48), dice.between(1, 48))
    }

    pub fn structure(dice: &mut Dice, room: &str, owned: bool, i: usize) -> Structure {
        let kind = if owned {
            dice.pick_of(&[
                Kind::Extension,
                Kind::Tower,
                Kind::Container,
                Kind::Storage,
                Kind::Road,
                Kind::Extractor,
            ])
        } else {
            dice.pick_of(&[Kind::Container, Kind::Road])
        };

        let at = pos(dice, room);
        let capacity = if kind == Kind::Road { 0 } else { 2000 };
        let energy = dice.between(0, capacity);
        let hits_max = dice.between(1, 250_000);
        let hits = dice.between(1, hits_max);

        Structure {
            id: format!("{room}s{i}"),
            owner: if owned && kind != Kind::Road && kind != Kind::Container {
                Owner::Ours
            } else {
                Owner::Nobody
            },
            kind: kind.clone(),
            pos: at,
            hits,
            hits_max,
            store: store(vec![(Resource::Energy, energy)], capacity),
            cooldown: 0,
            collapse: 0,
            level: 0,
            walkable: kind.walkable(),
        }
    }

    pub fn oven_of(name: &str, parts: &[BodyPart]) -> crate::core::world::Oven {
        crate::core::world::Oven {
            name: name.to_string(),
            remaining: 5,
            body: parts
                .iter()
                .map(|p| Part {
                    kind: *p,
                    hits: engine::PART_HITS,
                    boost: Boost::Unboosted,
                })
                .collect(),
        }
    }

    pub fn room_gen(dice: &mut Dice, i: usize) -> (Room, Vec<Spawn>) {
        let name = format!("r{i}");
        let owned = dice.below(3) < 2;
        let reserved = dice.chance(2);
        let level = dice.between(1, 8);
        let downgrade = dice.between(0, 200_000);
        let cpos = pos(dice, &name);
        let holder = dice.pick_of(&[Owner::Stranger("someone".to_string()), Owner::Ours]);

        let controller = Controller {
            id: format!("{name}c"),
            pos: cpos,
            owner: if owned { Owner::Ours } else { Owner::Nobody },
            level: if owned { level } else { 0 },
            progress: 0,
            progress_total: 0,
            ticks_to_downgrade: if owned { downgrade } else { 0 },
            reservation: if !owned && reserved {
                Some((holder, 1200))
            } else {
                None
            },
            safe_mode: 0,
            safe_mode_available: 0,
            safe_mode_cooldown: 0,
            upgrade_blocked: 0,
        };

        let sources: Vec<Source> = (0..dice.between(0, 2))
            .map(|k| Source {
                id: format!("{name}src{k}"),
                pos: pos(dice, &name),
                energy: 1000,
                energy_capacity: 3000,
                ticks_to_regeneration: 0,
            })
            .collect();

        let structures: Vec<Structure> = (0..dice.between(0, 8))
            .map(|k| structure(dice, &name, owned, k as usize))
            .collect();

        let mut spawns: Vec<Spawn> = Vec::new();
        for k in 0..if owned { dice.between(1, 3) } else { 0 } {
            let busy = dice.pick(&[0, 0, 1, 2]);
            spawns.push(Spawn {
                id: format!("{name}p{k}"),
                name: format!("{name}sp{k}"),
                pos: pos(dice, &name),
                spawning: match busy {
                    0 => None,
                    1 => Some(oven_of(
                        &format!("harvester-1-{name}sp{k}"),
                        &[
                            BodyPart::Work,
                            BodyPart::Work,
                            BodyPart::Work,
                            BodyPart::Carry,
                            BodyPart::Move,
                            BodyPart::Move,
                        ],
                    )),
                    _ => Some(oven_of(
                        &format!("hauler-1-{name}sp{k}"),
                        &[BodyPart::Carry, BodyPart::Carry, BodyPart::Move],
                    )),
                },
            });
        }

        let bank_capacity = dice.pick(&[300, 550, 800, 1300, 5600]);
        let bank = dice.between(0, bank_capacity);
        let site_pos = pos(dice, &name);
        let has_site = dice.chance(2);
        let pile_amount = dice.between(0, 3000);

        let room = Room {
            name: name.clone(),
            terrain: vec![0; 2500],
            controller: Some(controller),
            energy_available: if owned { bank } else { 0 },
            energy_capacity: if owned { bank_capacity } else { 0 },
            sources,
            minerals: Vec::<Mineral>::new(),
            structures,
            sites: if has_site {
                vec![Site {
                    id: format!("{name}site"),
                    kind: Kind::Extension,
                    pos: site_pos.clone(),
                    owner: Owner::Ours,
                    progress: 100,
                    progress_total: 3000,
                }]
            } else {
                Vec::new()
            },
            piles: if pile_amount > 0 {
                vec![Pile {
                    id: format!("{name}pile"),
                    pos: site_pos,
                    resource: Resource::Energy,
                    amount: pile_amount,
                }]
            } else {
                Vec::new()
            },
            remains: Vec::<Remains>::new(),
            others: Vec::new(),
        };

        (room, spawns)
    }

    pub fn creep_gen(dice: &mut Dice, rooms: &[String], i: usize) -> Creep {
        let room = dice.pick_of(rooms);
        let at = pos(dice, &room);
        let prefix = dice.pick(&["worker", "harvester", "hauler", "claimer", "x"]);
        let n = dice.between(1, 12);
        let parts: Vec<BodyPart> = (0..n)
            .map(|_| {
                dice.pick(&[
                    BodyPart::Work,
                    BodyPart::Carry,
                    BodyPart::Move,
                    BodyPart::Claim,
                ])
            })
            .collect();
        let life = dice.between(1, engine::CREEP_LIFETIME);
        let carry = part_count_in(&parts, BodyPart::Carry) * engine::CARRY_PART_CAPACITY;
        let energy = dice.between(0, carry);

        Creep {
            id: format!("c{i}"),
            name: format!("{prefix}-{i}-x"),
            owner: Owner::Ours,
            pos: at,
            body: parts
                .iter()
                .map(|p| Part {
                    kind: *p,
                    hits: engine::PART_HITS,
                    boost: Boost::Unboosted,
                })
                .collect(),
            hits: n * engine::PART_HITS,
            hits_max: n * engine::PART_HITS,
            ticks_to_live: life,
            fatigue: 0,
            store: store(vec![(Resource::Energy, energy)], carry),
        }
    }

    /// A world of up to four rooms, the unowned ones declared worked or claimed.
    pub struct Generated {
        pub world: World,
        pub worked: Vec<Outpost>,
        pub claims: Vec<Outpost>,
    }

    pub fn world(dice: &mut Dice) -> Generated {
        let nr = dice.between(1, 4);
        let rooms: Vec<(Room, Vec<Spawn>)> = (0..nr).map(|i| room_gen(dice, i as usize)).collect();
        let names: Vec<String> = rooms.iter().map(|(r, _)| r.name.clone()).collect();

        let creeps: Vec<Creep> = (0..dice.between(0, 12))
            .map(|i| creep_gen(dice, &names, i as usize))
            .collect();

        let time = dice.between(1, 1_000_000);
        let split = dice.chance(2);

        let unowned: Vec<(&Room, &Controller)> = rooms
            .iter()
            .filter_map(|(r, _)| {
                r.controller
                    .as_ref()
                    .filter(|c| c.owner == Owner::Nobody)
                    .map(|c| (r, c))
            })
            .collect();

        let declared = |(r, c): &(&Room, &Controller)| Outpost {
            room: r.name.clone(),
            controller: Declared {
                id: c.id.clone(),
                pos: c.pos.clone(),
            },
            sources: r
                .sources
                .iter()
                .map(|s| Declared {
                    id: s.id.clone(),
                    pos: s.pos.clone(),
                })
                .collect(),
        };

        let world = World {
            time,
            me: "me".to_string(),
            cpu_limit: 20.0,
            bucket: 10_000,
            rooms: rooms.iter().map(|(r, _)| r.clone()).collect(),
            spawns: rooms.iter().flat_map(|(_, s)| s.clone()).collect(),
            creeps,
            our_sites: 0,
            rooms_allowed: rooms.len() as i32 + 1,
        };

        Generated {
            world,
            worked: if split {
                unowned.iter().map(declared).collect()
            } else {
                Vec::new()
            },
            claims: if split {
                Vec::new()
            } else {
                unowned.iter().map(declared).collect()
            },
        }
    }
}
