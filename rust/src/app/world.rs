//! `World.ofGame`: the one reader of the game's objects. Fields are read the
//! way the F# bot read them (the object's own property, absent = 0), so a
//! modded structure or a lost field costs nothing; `screeps-game-api` supplies
//! `Game`, the find passes and the object handles.
use js_sys::{Array, Object, Reflect};
use screeps::{find, game, objects, prelude::*, Room as GameRoom};
use wasm_bindgen::{JsCast, JsValue};
use js_sys::Function;

use crate::core::engine;
use crate::core::vocabulary::{self, BodyPart, Boost};
use crate::core::world::*;

fn field(v: &JsValue, name: &str) -> JsValue {
    Reflect::get(v, &JsValue::from_str(name)).unwrap_or(JsValue::UNDEFINED)
}

fn call1(f: &JsValue, this: &JsValue, a: &JsValue) -> JsValue {
    let args = Array::new();
    args.push(a);
    Reflect::apply(f.unchecked_ref::<Function>(), this, &args).unwrap_or(JsValue::UNDEFINED)
}

fn call0(f: &JsValue, this: &JsValue) -> JsValue {
    Reflect::apply(f.unchecked_ref::<Function>(), this, &Array::new()).unwrap_or(JsValue::UNDEFINED)
}

fn absent(v: &JsValue) -> bool {
    v.is_undefined() || v.is_null()
}

fn int_of(v: &JsValue) -> i32 {
    v.as_f64().map(|n| n as i32).unwrap_or(0)
}

fn entries(v: &JsValue) -> Vec<(String, JsValue)> {
    let array = Object::entries(&Object::from(v.clone()));
    (0..array.length())
        .filter_map(|i| array.get(i).dyn_into::<Array>().ok())
        .filter_map(|pair| {
            let key = pair.get(0).as_string()?;
            Some((key, pair.get(1)))
        })
        .collect()
}

fn jsval<T: JsCast>(v: &T) -> JsValue {
    v.unchecked_ref::<JsValue>().clone()
}

fn pos_of(v: &JsValue) -> Pos {
    let p = field(v, "pos");
    Pos::new(
        field(&p, "roomName").as_string().unwrap_or_default(),
        int_of(&field(&p, "x")),
        int_of(&field(&p, "y")),
    )
}

fn owner_of(my: &JsValue, owner: &JsValue) -> Owner {
    if my.as_bool() == Some(true) {
        Owner::Ours
    } else if absent(owner) {
        Owner::Nobody
    } else {
        Owner::Theirs(field(owner, "username").as_string().unwrap_or_default())
    }
}

fn capacity_of(store: &JsValue) -> i32 {
    if absent(store) {
        return 0;
    }
    let get = field(store, "getCapacity");
    if absent(&get) {
        return 0;
    }
    match call0(&get, store).as_f64() {
        Some(n) => n as i32,
        None => match call1(&get, store, &JsValue::from_str("energy")).as_f64() {
            Some(n) => n as i32,
            None => 0,
        },
    }
}

fn store_of(store: &JsValue) -> Store {
    if absent(store) {
        return Store::empty();
    }
    Store {
        held: entries(store)
            .into_iter()
            .filter_map(|(name, amount)| match amount.as_f64() {
                Some(n) if n > 0.0 => Some((vocabulary::resource_of_name(&name), n as i32)),
                _ => None,
            })
            .collect(),
        capacity: capacity_of(store),
    }
}

fn kind_of(v: &JsValue) -> Kind {
    Kind::of_name(&field(v, "structureType").as_string().unwrap_or_default())
}

fn collapse_of(s: &JsValue, kind: &Kind) -> i32 {
    if !kind.is_invader_core() {
        0
    } else {
        let effects = field(s, "effects");
        let found = if let Ok(array) = effects.dyn_into::<Array>() {
            (0..array.length())
                .map(|i| array.get(i))
                .find(|e| int_of(&field(e, "effect")) == 1002)
        } else {
            None
        };
        match found {
            Some(e) => int_of(&field(&e, "ticksRemaining")),
            None => int_of(&field(s, "ticksToDeploy")) + engine::STRONGHOLD_LONGEST_COLLAPSE,
        }
    }
}

fn structure_of(s: &JsValue) -> Structure {
    let kind = kind_of(s);
    let owner = owner_of(&field(s, "my"), &field(s, "owner"));
    Structure {
        id: field(s, "id").as_string().unwrap_or_default(),
        pos: pos_of(s),
        owner: owner.clone(),
        hits: int_of(&field(s, "hits")),
        hits_max: int_of(&field(s, "hitsMax")),
        store: store_of(&field(s, "store")),
        cooldown: int_of(&field(s, "cooldown")),
        collapse: collapse_of(s, &kind),
        level: if kind.is_invader_core() {
            int_of(&field(s, "level"))
        } else {
            0
        },
        walkable: if kind.is_rampart() {
            owner.is_ours() || field(s, "isPublic").as_bool() == Some(true)
        } else {
            kind.walkable()
        },
        kind,
    }
}

fn site_of(s: &JsValue) -> Site {
    Site {
        id: field(s, "id").as_string().unwrap_or_default(),
        kind: kind_of(s),
        pos: pos_of(s),
        owner: owner_of(&field(s, "my"), &field(s, "owner")),
        progress: int_of(&field(s, "progress")),
        progress_total: int_of(&field(s, "progressTotal")),
    }
}

fn controller_of(c: &JsValue) -> Controller {
    let reservation = field(c, "reservation");
    Controller {
        id: field(c, "id").as_string().unwrap_or_default(),
        pos: pos_of(c),
        owner: owner_of(&field(c, "my"), &field(c, "owner")),
        level: int_of(&field(c, "level")),
        progress: int_of(&field(c, "progress")),
        progress_total: int_of(&field(c, "progressTotal")),
        ticks_to_downgrade: int_of(&field(c, "ticksToDowngrade")),
        reservation: if absent(&reservation) {
            None
        } else {
            Some((
                field(&reservation, "username").as_string().unwrap_or_default(),
                int_of(&field(&reservation, "ticksToEnd")),
            ))
        },
        safe_mode: int_of(&field(c, "safeMode")),
        safe_mode_available: int_of(&field(c, "safeModeAvailable")),
        safe_mode_cooldown: int_of(&field(c, "safeModeCooldown")),
        upgrade_blocked: int_of(&field(c, "upgradeBlocked")),
    }
}

fn part_of(p: &JsValue) -> Part {
    let kind = field(p, "type")
        .as_string()
        .as_deref()
        .and_then(vocabulary::part_of_name)
        .unwrap_or(BodyPart::Tough);
    let boost = field(p, "boost");
    Part {
        kind,
        hits: int_of(&field(p, "hits")),
        boost: if absent(&boost) {
            Boost::Unboosted
        } else {
            vocabulary::boost::of_compound(kind, &boost.as_string().unwrap_or_default())
        },
    }
}

fn body_of(v: &JsValue) -> Vec<Part> {
    match field(v, "body").dyn_into::<Array>() {
        Ok(array) => (0..array.length()).map(|i| part_of(&array.get(i))).collect(),
        Err(_) => Vec::new(),
    }
}

fn creep_of(creep: &JsValue) -> Creep {
    Creep {
        id: field(creep, "id").as_string().unwrap_or_default(),
        name: field(creep, "name").as_string().unwrap_or_default(),
        owner: owner_of(&field(creep, "my"), &field(creep, "owner")),
        pos: pos_of(creep),
        body: body_of(creep),
        hits: int_of(&field(creep, "hits")),
        hits_max: int_of(&field(creep, "hitsMax")),
        ticks_to_live: int_of(&field(creep, "ticksToLive")),
        fatigue: int_of(&field(creep, "fatigue")),
        store: store_of(&field(creep, "store")),
    }
}

fn found(room: &GameRoom, ty: find::RoomObject) -> Vec<JsValue> {
    room.find(ty, None)
        .into_iter()
        .map(|o: objects::RoomObject| jsval(&o))
        .collect()
}

fn remains_of(t: &JsValue) -> Remains {
    let creep = field(t, "creep");
    Remains {
        id: field(t, "id").as_string().unwrap_or_default(),
        pos: pos_of(t),
        store: store_of(&field(t, "store")),
        fallen: if absent(&creep) {
            None
        } else {
            let mut c = creep_of(&creep);
            // The engine's fallen creep keeps no hits or fatigue.
            c.hits = 0;
            c.fatigue = 0;
            Some(Fallen {
                creep: c,
                died: int_of(&field(t, "deathTime")),
            })
        },
    }
}

/// `Game.map.getRoomTerrain(name).getRawBuffer()`: the map serves rooms the
/// bot cannot see, and the engine builds no `Room` global for the crate's
/// static call to land on.
fn terrain_of(name: &str) -> Vec<u8> {
    let Ok(game) = Reflect::get(&js_sys::global(), &JsValue::from_str("Game")) else {
        return Vec::new();
    };
    let map = field(&game, "map");
    let terrain = call1(
        &field(&map, "getRoomTerrain"),
        &map,
        &JsValue::from_str(name),
    );
    let raw = call0(&field(&terrain, "getRawBuffer"), &terrain);
    js_sys::Uint8Array::new(&raw).to_vec()
}

fn room_of(room: &GameRoom) -> Room {
    let name = room.name().to_string();
    let controller = room.controller();
    let terrain = terrain_of(&name);

    let mut remains: Vec<Remains> = Vec::new();
    for ty in [find::RoomObject::Tombstones, find::RoomObject::Ruins] {
        remains.extend(found(room, ty).iter().map(remains_of));
    }

    Room {
        name,
        terrain,
        controller: controller.as_ref().map(|c| controller_of(&jsval(c))),
        energy_available: room.energy_available() as i32,
        energy_capacity: room.energy_capacity_available() as i32,
        sources: room
            .find(find::SOURCES, None)
            .into_iter()
            .map(|s: objects::Source| Source {
                id: s.id().to_string(),
                pos: pos_of(&jsval(&s)),
                energy: field(&jsval(&s), "energy").as_f64().unwrap_or(0.0) as i32,
                energy_capacity: int_of(&field(&jsval(&s), "energyCapacity")),
                ticks_to_regeneration: int_of(&field(&jsval(&s), "ticksToRegeneration")),
            })
            .collect(),
        minerals: room
            .find(find::MINERALS, None)
            .into_iter()
            .map(|m: objects::Mineral| Mineral {
                id: m.id().to_string(),
                pos: pos_of(&jsval(&m)),
                resource: vocabulary::resource_of_name(
                    &field(&jsval(&m), "mineralType").as_string().unwrap_or_default(),
                ),
                amount: int_of(&field(&jsval(&m), "mineralAmount")),
            })
            .collect(),
        structures: found(room, find::RoomObject::Structures)
            .iter()
            .map(structure_of)
            .collect(),
        sites: found(room, find::RoomObject::ConstructionSites)
            .iter()
            .map(site_of)
            .collect(),
        piles: room
            .find(find::DROPPED_RESOURCES, None)
            .into_iter()
            .map(|r: objects::Resource| Pile {
                id: r.id().to_string(),
                pos: pos_of(&jsval(&r)),
                resource: vocabulary::resource_of_name(
                    &field(&jsval(&r), "resourceType").as_string().unwrap_or_default(),
                ),
                amount: int_of(&field(&jsval(&r), "amount")),
            })
            .collect(),
        remains,
        hostiles: found(room, find::RoomObject::HostileCreeps)
            .iter()
            .map(creep_of)
            .collect(),
    }
}

fn spawn_of(spawn: &objects::StructureSpawn, creeps: &JsValue) -> Spawn {
    let spawning = spawn.spawning();
    Spawn {
        id: spawn.id().to_string(),
        name: spawn.name(),
        pos: pos_of(&jsval(spawn)),
        spawning: spawning.map(|s| {
            let name: String = s.name().into();
            let creep = field(creeps, &name);
            Oven {
                name,
                remaining: s.remaining_time() as i32,
                body: if absent(&creep) { Vec::new() } else { body_of(&creep) },
            }
        }),
    }
}

pub fn of_game() -> World {
    let time = game::time() as i32;
    let cpu_limit = game::cpu::limit() as f64;
    let bucket = game::cpu::bucket();

    let mut room_entries: Vec<(String, GameRoom)> = game::rooms()
        .entries()
        .map(|(name, room)| (name.to_string(), room))
        .collect();
    room_entries.sort_by(|a, b| a.0.cmp(&b.0));

    let mut creep_entries: Vec<(String, objects::Creep)> = game::creeps().entries().collect();
    creep_entries.sort_by(|a, b| a.0.cmp(&b.0));

    let mut spawn_entries: Vec<(String, objects::StructureSpawn)> = game::spawns().entries().collect();
    spawn_entries.sort_by(|a, b| a.0.cmp(&b.0));

    let creeps_js = game_creeps();
    let rooms = room_entries.iter().map(|(_, r)| room_of(r)).collect::<Vec<_>>();
    let spawns = spawn_entries
        .iter()
        .map(|(_, s)| spawn_of(s, &creeps_js))
        .collect::<Vec<_>>();
    let creeps = creep_entries
        .iter()
        .filter(|(_, c)| !c.spawning())
        .map(|(_, c)| creep_of(&jsval(c)))
        .collect::<Vec<_>>();

    let me = match (spawn_entries.first(), creep_entries.first()) {
        (Some((_, spawn)), _) => field(&field(&jsval(spawn), "owner"), "username")
            .as_string()
            .unwrap_or_default(),
        (None, Some((_, creep))) => field(&field(&jsval(creep), "owner"), "username")
            .as_string()
            .unwrap_or_default(),
        (None, None) => room_entries
            .iter()
            .find_map(|(_, room)| {
                room.controller().and_then(|c| {
                    let v = jsval(&c);
                    if field(&v, "my").as_bool() == Some(true) {
                        field(&field(&v, "owner"), "username").as_string()
                    } else {
                        None
                    }
                })
            })
            .unwrap_or_default(),
    };

    World {
        time,
        me,
        cpu_limit,
        bucket,
        rooms,
        spawns,
        creeps,
        our_sites: game::construction_sites().entries().count() as i32,
    }
}

fn game_creeps() -> JsValue {
    match Reflect::get(&js_sys::global(), &JsValue::from_str("Game")) {
        Ok(game) => field(&game, "creeps"),
        Err(_) => JsValue::UNDEFINED,
    }
}

/// `Game.getObjectById` as `Executor` reads it: absent is a refusal, never an error.
pub fn object_by_id(id: &str) -> Option<JsValue> {
    let game = Reflect::get(&js_sys::global(), &JsValue::from_str("Game")).ok()?;
    let get = field(&game, "getObjectById");
    let found = call1(&get, &game, &JsValue::from_str(id));
    if absent(&found) {
        None
    } else {
        Some(found)
    }
}

/// `Game.creeps[name]`, for the executor's creep.
pub fn creep_by_name(name: &str) -> Option<JsValue> {
    let game = Reflect::get(&js_sys::global(), &JsValue::from_str("Game")).ok()?;
    let creeps = field(&game, "creeps");
    let creep = field(&creeps, name);
    if absent(&creep) {
        None
    } else {
        Some(creep)
    }
}

pub fn room_by_name(name: &str) -> Option<JsValue> {
    let game = Reflect::get(&js_sys::global(), &JsValue::from_str("Game")).ok()?;
    let rooms = field(&game, "rooms");
    let room = field(&rooms, name);
    if absent(&room) {
        None
    } else {
        Some(room)
    }
}
