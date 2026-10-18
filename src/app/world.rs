//! `World.ofGame`: the one reader of the game's objects. `read.js` walks them
//! in one pass and hands over its numbers and its strings whole; this decodes
//! them in the order it wrote them. A call across the seam costs more than the
//! field it reads, so the reader makes a handful a tick.
use crate::core::HashMap;
use std::str::Split;

use js_sys::{Array, Int32Array, Reflect};
use screeps::{ResourceType, game};
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::{JsCast, JsValue};

use super::js::field;

use crate::core::engine;
use crate::core::tick::Declarations;
use crate::core::vocabulary::{self, BodyPart, Boost};
use crate::core::world::*;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_name = __fabotRead)]
    fn read() -> Array;
}

fn absent(v: &JsValue) -> bool {
    v.is_undefined() || v.is_null()
}

pub type Terrains = HashMap<String, Vec<u8>>;

struct Reader<'a> {
    ints: &'a [i32],
    at: usize,
    strs: Split<'a, char>,
    terrain: &'a mut Terrains,
    me: String,
    declarations: &'a Declarations,
}

impl Reader<'_> {
    fn n(&mut self) -> i32 {
        self.at += 1;
        self.ints[self.at - 1]
    }

    fn flag(&mut self) -> bool {
        self.n() != 0
    }

    fn s(&mut self) -> String {
        self.strs.next().unwrap_or_default().to_string()
    }

    fn list<T>(&mut self, mut each: impl FnMut(&mut Self) -> T) -> Vec<T> {
        let count = self.n();
        (0..count).map(|_| each(self)).collect()
    }

    fn owner(&mut self) -> Owner {
        let tag = self.n();
        let name = self.s();
        match tag {
            1 => Owner::Ours,
            2 => self.user(name),
            _ => Owner::Nobody,
        }
    }

    fn user(&self, name: String) -> Owner {
        Owner::of_user(name, &self.me, self.declarations)
    }

    fn pos(&mut self) -> Pos {
        let room = self.s();
        let x = self.n();
        let y = self.n();
        Pos::new(room, x, y)
    }

    fn pos_in(&mut self, room: &str) -> Pos {
        let x = self.n();
        let y = self.n();
        Pos::new(room, x, y)
    }

    /// Only the resources `screeps-game-api` names count, as its `store_types` read them.
    fn store(&mut self) -> Store {
        let held = self
            .list(|r| (r.s(), r.n()))
            .into_iter()
            .filter(|(name, amount)| *amount > 0 && name.parse::<ResourceType>().is_ok())
            .map(|(name, amount)| (vocabulary::resource_of_name(&name), amount))
            .collect();
        Store {
            held,
            capacity: self.n(),
        }
    }

    fn body(&mut self) -> Vec<Part> {
        self.list(|r| {
            let kind = vocabulary::part_of_name(&r.s()).unwrap_or(BodyPart::Tough);
            let hits = r.n();
            let boost = r.s();
            Part {
                kind,
                hits,
                boost: if boost.is_empty() {
                    Boost::Unboosted
                } else {
                    vocabulary::boost::of_compound(kind, &boost)
                },
            }
        })
    }

    fn creep(&mut self) -> Creep {
        Creep {
            id: self.s(),
            name: self.s(),
            owner: self.owner(),
            pos: self.pos(),
            body: self.body(),
            hits: self.n(),
            hits_max: self.n(),
            ticks_to_live: self.n(),
            fatigue: self.n(),
            store: self.store(),
        }
    }

    fn controller(&mut self) -> Controller {
        Controller {
            id: self.s(),
            pos: self.pos(),
            owner: self.owner(),
            level: self.n(),
            progress: self.n(),
            progress_total: self.n(),
            ticks_to_downgrade: self.n(),
            reservation: if self.flag() {
                let user = self.s();
                Some((self.user(user), self.n()))
            } else {
                None
            },
            safe_mode: self.n(),
            safe_mode_available: self.n(),
            safe_mode_cooldown: self.n(),
            upgrade_blocked: self.n(),
        }
    }

    fn structure(&mut self, room: &str) -> Structure {
        let kind = Kind::of_name(&self.s());
        let id = self.s();
        let pos = self.pos_in(room);
        let owner = self.owner();
        let hits = self.n();
        let hits_max = self.n();
        let store = if kind.has_store() {
            self.store()
        } else {
            Store::empty()
        };
        let cooldown = if kind.has_cooldown() { self.n() } else { 0 };
        let (collapse, level) = if kind.is_invader_core() {
            let running = self.flag();
            let ticks = self.n();
            let collapse = if running {
                ticks
            } else {
                ticks + engine::STRONGHOLD_LONGEST_COLLAPSE
            };
            (collapse, self.n())
        } else {
            (0, 0)
        };
        let walkable = if kind.is_rampart() {
            let public = self.flag();
            owner.is_ours() || public
        } else {
            kind.walkable()
        };
        Structure {
            id,
            kind,
            pos,
            owner,
            hits,
            hits_max,
            store,
            cooldown,
            collapse,
            level,
            walkable,
        }
    }

    /// A tombstone's creep keeps no hits or fatigue.
    fn remains(&mut self) -> Remains {
        Remains {
            id: self.s(),
            pos: self.pos(),
            store: self.store(),
            fallen: if self.flag() {
                let mut creep = self.creep();
                creep.hits = 0;
                creep.fatigue = 0;
                Some(Fallen {
                    creep,
                    died: self.n(),
                })
            } else {
                None
            },
        }
    }

    fn room(&mut self) -> Room {
        let name = self.s();
        let terrain = if let Some(terrain) = self.terrain.get(&name) {
            terrain.clone()
        } else {
            let terrain = terrain_of(&name);
            if !terrain.is_empty() {
                self.terrain.insert(name.clone(), terrain.clone());
            }
            terrain
        };
        Room {
            terrain,
            energy_available: self.n(),
            energy_capacity: self.n(),
            controller: if self.flag() {
                Some(self.controller())
            } else {
                None
            },
            sources: self.list(|r| Source {
                id: r.s(),
                pos: r.pos(),
                energy: r.n(),
                energy_capacity: r.n(),
                ticks_to_regeneration: r.n(),
            }),
            minerals: self.list(|r| Mineral {
                id: r.s(),
                pos: r.pos(),
                resource: vocabulary::resource_of_name(&r.s()),
                amount: r.n(),
            }),
            structures: self.list(|r| r.structure(&name)),
            sites: self.list(|r| Site {
                kind: Kind::of_name(&r.s()),
                id: r.s(),
                pos: r.pos_in(&name),
                owner: r.owner(),
                progress: r.n(),
                progress_total: r.n(),
            }),
            piles: self.list(|r| Pile {
                id: r.s(),
                pos: r.pos(),
                resource: vocabulary::resource_of_name(&r.s()),
                amount: r.n(),
            }),
            remains: self.list(Reader::remains),
            others: self.list(Reader::creep),
            name,
        }
    }

    fn spawn(&mut self) -> Spawn {
        Spawn {
            id: self.s(),
            name: self.s(),
            pos: self.pos(),
            spawning: if self.flag() {
                Some(Oven {
                    name: self.s(),
                    remaining: self.n(),
                    body: self.body(),
                })
            } else {
                None
            },
        }
    }
}

/// What `read.js` handed over, whole: the recorder's capture of the tick.
pub struct Pass {
    pub ints: Vec<i32>,
    pub strs: String,
}

pub fn of_game(terrain: &mut Terrains, declarations: &Declarations) -> (World, Pass) {
    let packed = read();
    let ints = packed.get(0).unchecked_into::<Int32Array>().to_vec();
    let text = packed.get(1).as_string().unwrap_or_default();
    let mut r = Reader {
        ints: &ints,
        at: 0,
        strs: text.split('\0'),
        terrain,
        me: String::new(),
        declarations,
    };
    r.me = r.s();
    let time = r.n();
    let bucket = r.n();
    let rooms = r.list(Reader::room);
    let spawns = r.list(Reader::spawn);
    let creeps = r.list(Reader::creep);
    let world = World {
        time,
        me: r.me.clone(),
        cpu_limit: f64::from(game::cpu::limit()),
        bucket,
        rooms,
        spawns,
        creeps,
        our_sites: r.n(),
        rooms_allowed: r.n(),
    };
    (world, Pass { ints, strs: text })
}

/// `Game.map.getRoomTerrain(name).getRawBuffer()`: the map serves rooms the
/// bot cannot see, and reading it needs no `Room` object.
fn terrain_of(name: &str) -> Vec<u8> {
    let read = || -> Option<Vec<u8>> {
        let game = Reflect::get(&js_sys::global(), &JsValue::from_str("Game")).ok()?;
        let map = field(&game, "map");
        let get_terrain = field(&map, "getRoomTerrain");
        let arguments = Array::of1(&JsValue::from_str(name));
        let terrain = Reflect::apply(
            get_terrain.unchecked_ref::<js_sys::Function>(),
            &map,
            &arguments,
        )
        .ok()?;
        let get_raw = field(&terrain, "getRawBuffer");
        let raw = Reflect::apply(
            get_raw.unchecked_ref::<js_sys::Function>(),
            &terrain,
            &Array::new(),
        )
        .ok()?;
        Some(js_sys::Uint8Array::new(&raw).to_vec())
    };
    read().unwrap_or_default()
}

/// `Game.getObjectById` as `Executor` reads it: absent is a refusal, never an error.
pub fn object_by_id(id: &str) -> Option<JsValue> {
    let game = Reflect::get(&js_sys::global(), &JsValue::from_str("Game")).ok()?;
    let get = field(&game, "getObjectById");
    let arguments = Array::new();
    arguments.push(&JsValue::from_str(id));
    let found = Reflect::apply(get.unchecked_ref::<js_sys::Function>(), &game, &arguments).ok()?;
    if absent(&found) { None } else { Some(found) }
}

/// `Game.creeps[name]`, for the executor's creep.
pub fn creep_by_name(name: &str) -> Option<JsValue> {
    let game = Reflect::get(&js_sys::global(), &JsValue::from_str("Game")).ok()?;
    let creeps = field(&game, "creeps");
    let creep = field(&creeps, name);
    if absent(&creep) { None } else { Some(creep) }
}

pub fn room_by_name(name: &str) -> Option<JsValue> {
    let game = Reflect::get(&js_sys::global(), &JsValue::from_str("Game")).ok()?;
    let rooms = field(&game, "rooms");
    let room = field(&rooms, name);
    if absent(&room) { None } else { Some(room) }
}
