pub mod abandon;
pub mod balance;
pub mod bodies;
pub mod derived;
pub mod jobs;
pub mod labs;
pub mod run;
pub mod slots;
pub mod task;

pub use run::{EconomyState, decide, spare};

#[cfg(test)]
pub(crate) mod arb {
    use fabot_base::declarations::{Declared, Outpost};
    use fabot_base::engine;
    use fabot_base::vocabulary::{Part, ResourceType, part_count_in};
    use fabot_base::world::{
        BodyPart, Built, Controller, Creep, Id, Owner, Pile, Pos, Room, RoomName, Site, Source,
        Spawn, Store, Structure, StructureType, World,
    };
    use proptest::collection::vec;
    use proptest::prelude::*;
    use proptest::sample::{Index, select};

    use super::task::{Target, Task};

    pub fn target() -> impl Strategy<Value = Target> {
        prop_oneof![
            1 => Just(Target::Ring(at().room)),
            1 => Just(Target::Pile(at().room)),
            6 => "[0-9a-f]{1,24}".prop_map(|hex| Target::Object(Id::parse(&hex).expect("hex"))),
        ]
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

    pub fn task() -> impl Strategy<Value = Task> {
        task_on(target())
    }

    pub fn task_on(target: impl Strategy<Value = Target>) -> impl Strategy<Value = Task> {
        (target, select(resources()), 0..=10).prop_map(|(target, resource, kind)| match kind {
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
        })
    }

    pub fn store(held: Vec<(ResourceType, i32)>, capacity: i32) -> Store {
        Store { held, capacity }
    }

    pub fn pos(room: RoomName) -> impl Strategy<Value = Pos> {
        (1..=48, 1..=48).prop_map(move |(x, y)| Pos::new(room, x, y))
    }

    /// Unnamed: the room ids it by its place among the room's draws.
    pub fn structure(room: RoomName, owned: bool) -> impl Strategy<Value = Structure> {
        let kinds = if owned {
            vec![
                StructureType::Extension,
                StructureType::Tower,
                StructureType::Container,
                StructureType::Storage,
                StructureType::Road,
                StructureType::Extractor,
            ]
        } else {
            vec![StructureType::Container, StructureType::Road]
        };
        let draws = (
            select(kinds),
            pos(room),
            0..=2000,
            1..=250_000,
            any::<Index>(),
        );
        draws.prop_map(move |(kind, at, energy, hits_max, hits)| {
            let capacity = if kind == StructureType::Road { 0 } else { 2000 };
            Structure {
                id: Id::from("?"),
                owner: if owned && kind != StructureType::Road && kind != StructureType::Container {
                    Owner::Ours
                } else {
                    Owner::Nobody
                },
                kind,
                pos: at,
                hits: 1 + hits.index(hits_max as usize) as i32,
                hits_max,
                store: store(
                    vec![(ResourceType::Energy, i32::min(energy, capacity))],
                    capacity,
                ),
                cooldown: 0,
                collapse: 0,
                deploy: 0,
                spawn: None,
                level: 0,
                walkable: kind.walkable(),
            }
        })
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

    pub fn room(name: RoomName) -> impl Strategy<Value = (Room<'static>, Vec<Spawn>)> {
        (0..3).prop_flat_map(move |owned| room_of(name, owned < 2))
    }

    fn room_of(name: RoomName, owned: bool) -> impl Strategy<Value = (Room<'static>, Vec<Spawn>)> {
        let holder = select(vec![Owner::Stranger("someone".to_string()), Owner::Ours]);
        let controller = (any::<bool>(), 1..=8, 0..=200_000, pos(name), holder);
        let count = if owned { 1..=3 } else { 0..=0 };
        let ovens = vec((select(vec![0, 0, 1, 2]), pos(name)), count);
        let bank = (select(vec![300, 550, 800, 1300, 5600]), any::<Index>());
        let site = (pos(name), any::<bool>(), 0..=3000);
        let draws = (
            controller,
            vec(pos(name), 0..=2),
            vec(structure(name, owned), 0..=8),
            ovens,
            bank,
            site,
        );
        draws.prop_map(move |draws| {
            let (
                (reserved, level, downgrade, cpos, holder),
                sources,
                structures,
                ovens,
                bank,
                site,
            ) = draws;
            let (bank_capacity, bank) = bank;
            let (site_pos, has_site, pile_amount) = site;

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
                sign: None,
                safe_mode: 0,
                safe_mode_available: 0,
                safe_mode_cooldown: 0,
                upgrade_blocked: 0,
            };

            let sources: Vec<Source> = (sources.into_iter().enumerate())
                .map(|(k, pos)| Source {
                    id: format!("{name}src{k}").as_str().into(),
                    pos,
                    energy: 1000,
                    energy_capacity: 3000,
                    ticks_to_regeneration: 0,
                })
                .collect();

            let mut structures: Vec<Structure> = (structures.into_iter().enumerate())
                .map(|(k, s)| Structure {
                    id: format!("{name}s{k}").as_str().into(),
                    ..s
                })
                .collect();

            let mut spawns: Vec<Spawn> = Vec::new();
            for (k, (busy, pos)) in ovens.into_iter().enumerate() {
                spawns.push(Spawn {
                    id: format!("{name}p{k}").as_str().into(),
                    name: format!("{name}sp{k}"),
                    pos,
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
                    id: spawns[k].id,
                    kind: StructureType::Spawn,
                    pos: spawns[k].pos,
                    owner: Owner::Ours,
                    hits: 5000,
                    hits_max: 5000,
                    store: store(vec![], 300),
                    cooldown: 0,
                    collapse: 0,
                    deploy: 0,
                    spawn: None,
                    level: 0,
                    walkable: false,
                });
            }

            let bank = bank.index(bank_capacity as usize + 1) as i32;

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
        })
    }

    /// Named by its role alone: the world numbers it.
    pub fn creep(rooms: Vec<RoomName>) -> impl Strategy<Value = Creep> {
        let at = select(rooms).prop_flat_map(pos);
        let prefix = select(vec!["worker", "harvester", "hauler", "claimer", "x"]);
        let parts = vec(
            select(vec![Part::Work, Part::Carry, Part::Move, Part::Claim]),
            1..=12,
        );
        let draws = (
            at,
            prefix,
            parts,
            1..=engine::CREEP_LIFETIME,
            any::<Index>(),
        );
        draws.prop_map(|(at, prefix, parts, life, energy)| {
            let n = parts.len() as i32;
            let carry = part_count_in(&parts, Part::Carry) * engine::CARRY_PART_CAPACITY;
            let energy = energy.index(carry as usize + 1) as i32;

            Creep {
                id: Id::from("?"),
                name: prefix.to_string(),
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
        })
    }

    /// A world of up to four rooms, the unowned ones declared worked or claimed.
    #[derive(Debug)]
    pub struct Generated {
        pub world: World<'static>,
        pub worked: Vec<Outpost>,
        pub claims: Vec<Outpost>,
    }

    pub fn world() -> impl Strategy<Value = Generated> {
        let draws = (1..=4).prop_flat_map(|nr| {
            let names: Vec<RoomName> = (0..nr).map(|i| RoomName::new(i, 0)).collect();
            let rooms: Vec<_> = names.iter().map(|&name| room(name)).collect();
            (
                rooms,
                vec(creep(names), 0..=12),
                1..=1_000_000,
                any::<bool>(),
            )
        });
        draws.prop_map(|(rooms, creeps, time, split)| {
            let creeps: Vec<Creep> = (creeps.into_iter().enumerate())
                .map(|(i, c)| Creep {
                    id: format!("c{i}").as_str().into(),
                    name: format!("{}-{i}-x", c.name),
                    ..c
                })
                .collect();

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
                reserved: true,
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
        })
    }
}
