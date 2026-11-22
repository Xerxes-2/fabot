pub mod abandon;
pub mod balance;
pub mod bodies;
pub mod derived;
pub mod jobs;
pub mod labs;
pub mod run;
pub mod slots;
pub mod task;

pub use run::{EconomyState, decide};

#[cfg(test)]
pub(crate) mod dice {
    use fabot_base::declarations::{Declared, Outpost};
    use fabot_base::engine;
    use fabot_base::vocabulary::{Part, ResourceType, part_count_in};
    use fabot_base::world::{
        BodyPart, Built, Controller, Creep, Id, Owner, Pile, Pos, Room, RoomName, Site, Source,
        Spawn, Store, Structure, StructureType, World,
    };

    use super::task::{Target, Task};

    pub use fabot_base::testing::Dice;

    pub fn target(dice: &mut Dice) -> Target {
        let n = dice.between(1, 24);
        let hex: String = (0..n)
            .map(|_| "0123456789abcdef".as_bytes()[dice.below(16) as usize] as char)
            .collect();
        match dice.below(8) {
            0 => Target::Ring(at().room),
            1 => Target::Pile(at().room),
            _ => Target::Object(Id::parse(&hex).expect("hex")),
        }
    }

    pub fn resources() -> Vec<ResourceType> {
        vec![
            ResourceType::Energy,
            ResourceType::Hydrogen,
            ResourceType::Catalyst,
            ResourceType::UtriumAcid,
            ResourceType::Power,
        ]
    }

    /// The scenes' tile: every body and every offer stand on it.
    pub fn at() -> Pos {
        Pos::new("W9N9".parse::<RoomName>().unwrap(), 25, 25)
    }

    pub fn task(dice: &mut Dice, target: Target) -> Task {
        let resource = resources()[dice.below(5) as usize];
        match dice.between(0, 10) {
            0 => Task::Harvest(target),
            1 => Task::Withdraw(target, resource),
            2 => Task::Pickup(target, resource),
            3 => Task::Refill(target, resource),
            4 => Task::Build(target),
            5 => Task::Repair(target),
            6 => Task::Upgrade(target),
            7 => Task::Reserve(target),
            8 => Task::Scout(at().room),
            9 => Task::Meet(target),
            _ => Task::Claim(target),
        }
    }

    pub fn store(held: Vec<(ResourceType, i32)>, capacity: i32) -> Store {
        Store { held, capacity }
    }

    pub fn pos(dice: &mut Dice, room: RoomName) -> Pos {
        Pos::new(room, dice.between(1, 48), dice.between(1, 48))
    }

    pub fn structure(dice: &mut Dice, room: RoomName, owned: bool, i: usize) -> Structure {
        let kind = if owned {
            dice.pick_of(&[
                StructureType::Extension,
                StructureType::Tower,
                StructureType::Container,
                StructureType::Storage,
                StructureType::Road,
                StructureType::Extractor,
            ])
        } else {
            dice.pick_of(&[StructureType::Container, StructureType::Road])
        };

        let at = pos(dice, room);
        let capacity = if kind == StructureType::Road { 0 } else { 2000 };
        let energy = dice.between(0, capacity);
        let hits_max = dice.between(1, 250_000);
        let hits = dice.between(1, hits_max);

        Structure {
            id: format!("{room}s{i}").as_str().into(),
            owner: if owned && kind != StructureType::Road && kind != StructureType::Container {
                Owner::Ours
            } else {
                Owner::Nobody
            },
            kind,
            pos: at,
            hits,
            hits_max,
            store: store(vec![(ResourceType::Energy, energy)], capacity),
            cooldown: 0,
            collapse: 0,
            spawn: None,
            level: 0,
            walkable: kind.walkable(),
        }
    }

    pub fn oven_of(name: &str, parts: &[Part]) -> fabot_base::world::Oven {
        fabot_base::world::Oven {
            name: name.to_string(),
            remaining: 5,
            body: parts
                .iter()
                .map(|p| BodyPart {
                    kind: *p,
                    hits: engine::PART_HITS,
                    boost: None,
                })
                .collect(),
        }
    }

    pub fn room_gen(dice: &mut Dice, i: usize) -> (Room<'static>, Vec<Spawn>) {
        let name = RoomName::new(i as i32, 0);
        let owned = dice.below(3) < 2;
        let reserved = dice.chance(2);
        let level = dice.between(1, 8);
        let downgrade = dice.between(0, 200_000);
        let cpos = pos(dice, name);
        let holder = dice.pick_of(&[Owner::Stranger("someone".to_string()), Owner::Ours]);

        let controller = Controller {
            id: format!("{name}c").as_str().into(),
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
                id: format!("{name}src{k}").as_str().into(),
                pos: pos(dice, name),
                energy: 1000,
                energy_capacity: 3000,
                ticks_to_regeneration: 0,
            })
            .collect();

        let mut structures: Vec<Structure> = (0..dice.between(0, 8))
            .map(|k| structure(dice, name, owned, k as usize))
            .collect();

        let mut spawns: Vec<Spawn> = Vec::new();
        for k in 0..if owned { dice.between(1, 3) } else { 0 } {
            let busy = dice.pick(&[0, 0, 1, 2]);
            spawns.push(Spawn {
                id: format!("{name}p{k}").as_str().into(),
                name: format!("{name}sp{k}"),
                pos: pos(dice, name),
                spawning: match busy {
                    0 => None,
                    1 => Some(oven_of(
                        &format!("harvester-1-{name}sp{k}"),
                        &[
                            Part::Work,
                            Part::Work,
                            Part::Work,
                            Part::Carry,
                            Part::Move,
                            Part::Move,
                        ],
                    )),
                    _ => Some(oven_of(
                        &format!("hauler-1-{name}sp{k}"),
                        &[Part::Carry, Part::Carry, Part::Move],
                    )),
                },
            });
            structures.push(Structure {
                id: spawns[k as usize].id,
                kind: StructureType::Spawn,
                pos: spawns[k as usize].pos,
                owner: Owner::Ours,
                hits: 5000,
                hits_max: 5000,
                store: store(vec![], 300),
                cooldown: 0,
                collapse: 0,
                spawn: None,
                level: 0,
                walkable: false,
            });
        }

        let bank_capacity = dice.pick(&[300, 550, 800, 1300, 5600]);
        let bank = dice.between(0, bank_capacity);
        let site_pos = pos(dice, name);
        let has_site = dice.chance(2);
        let pile_amount = dice.between(0, 3000);

        let room = Room {
            terrain: fabot_base::world::terrain::leak(vec![0; 2500]),
            controller: Some(controller),
            energy_available: if owned { bank } else { 0 },
            energy_capacity: if owned { bank_capacity } else { 0 },
            sources,
            structures,
            sites: if has_site {
                vec![Site {
                    id: format!("{name}site").as_str().into(),
                    kind: StructureType::Extension,
                    pos: site_pos,
                    owner: Owner::Ours,
                    progress: 100,
                    progress_total: 3000,
                }]
            } else {
                Vec::new()
            },
            piles: if pile_amount > 0 {
                vec![Pile {
                    id: format!("{name}pile").as_str().into(),
                    pos: site_pos,
                    resource: ResourceType::Energy,
                    amount: pile_amount,
                }]
            } else {
                Vec::new()
            },
            ..Room::empty(name)
        };

        (room, spawns)
    }

    pub fn creep_gen(dice: &mut Dice, rooms: &[RoomName], i: usize) -> Creep {
        let room = dice.pick_of(rooms);
        let at = pos(dice, room);
        let prefix = dice.pick(&["worker", "harvester", "hauler", "claimer", "x"]);
        let n = dice.between(1, 12);
        let parts: Vec<Part> = (0..n)
            .map(|_| dice.pick(&[Part::Work, Part::Carry, Part::Move, Part::Claim]))
            .collect();
        let life = dice.between(1, engine::CREEP_LIFETIME);
        let carry = part_count_in(&parts, Part::Carry) * engine::CARRY_PART_CAPACITY;
        let energy = dice.between(0, carry);

        Creep {
            id: format!("c{i}").as_str().into(),
            name: format!("{prefix}-{i}-x"),
            owner: Owner::Ours,
            pos: at,
            body: parts
                .iter()
                .map(|p| BodyPart {
                    kind: *p,
                    hits: engine::PART_HITS,
                    boost: None,
                })
                .collect(),
            hits: n * engine::PART_HITS,
            hits_max: n * engine::PART_HITS,
            ticks_to_live: life,
            fatigue: 0,
            store: store(vec![(ResourceType::Energy, energy)], carry),
        }
    }

    /// A world of up to four rooms, the unowned ones declared worked or claimed.
    pub struct Generated {
        pub world: World<'static>,
        pub worked: Vec<Outpost>,
        pub claims: Vec<Outpost>,
    }

    pub fn world(dice: &mut Dice) -> Generated {
        let nr = dice.between(1, 4);
        let rooms: Vec<(Room, Vec<Spawn>)> = (0..nr).map(|i| room_gen(dice, i as usize)).collect();
        let names: Vec<RoomName> = rooms.iter().map(|(r, _)| r.name).collect();

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
            room: r.name,
            controller: Declared {
                id: c.id,
                pos: c.pos,
            },
            sources: r
                .sources
                .iter()
                .map(|s| Declared {
                    id: s.id,
                    pos: s.pos,
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
            attacks: Vec::new(),
            destroyed: Vec::new(),
            our_sites: 0,
            rooms_allowed: rooms.len() as i32 + 1,
            closed: Default::default(),
            rules: fabot_base::world::Rules::vanilla(),
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
