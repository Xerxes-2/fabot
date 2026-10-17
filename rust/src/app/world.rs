//! `World.ofGame`: the one reader of the game's objects. `screeps-game-api`
//! supplies the typed bindings and `Game`, the find passes and the object
//! handles; the handful of fields the crate does not model (a structure's
//! `structureType`, `store` and `cooldown`, an Invader core's collapse) are
//! read from the object itself, so a modded structure or a lost field costs
//! nothing.
use js_sys::{Array, Reflect};
use screeps::{
    HasPosition, MaybeHasId, OwnedStructureProperties, Position, ResourceType,
    SharedCreepProperties, find, game, objects,
};
use wasm_bindgen::{JsCast, JsValue};

use crate::core::engine;
use crate::core::vocabulary::{self, BodyPart, Boost, Resource};
use crate::core::world::*;

fn field(v: &JsValue, name: &str) -> JsValue {
    Reflect::get(v, &JsValue::from_str(name)).unwrap_or(JsValue::UNDEFINED)
}

fn absent(v: &JsValue) -> bool {
    v.is_undefined() || v.is_null()
}

fn int_of(v: &JsValue) -> i32 {
    v.as_f64().map(|n| n as i32).unwrap_or(0)
}

fn jsval<T: JsCast>(v: &T) -> JsValue {
    v.unchecked_ref::<JsValue>().clone()
}

/// The engine's `__packedPos` packs the room name y-major and this crate's
/// `Position` reads it x-major, so the coordinate getters are the only source
/// of a room name: `Position` is right only where the two agree (W1N1).
fn pos_of(pos: &objects::RoomPosition) -> Pos {
    Pos::new(pos.room_name().to_string(), pos.x() as i32, pos.y() as i32)
}

fn resource_of(ty: ResourceType) -> Resource {
    vocabulary::resource_of_name(&ty.to_string())
}

fn store_of(store: &objects::Store) -> Store {
    Store {
        held: store
            .store_types()
            .into_iter()
            .filter_map(|ty| match store.get(ty) {
                Some(amount) if amount > 0 => Some((resource_of(ty), amount as i32)),
                _ => None,
            })
            .collect(),
        capacity: capacity_of(store),
    }
}

/// `getCapacity() ?? getCapacity('energy') ?? 0`.
fn capacity_of(store: &objects::Store) -> i32 {
    match store.get_capacity(None) {
        0 => store.get_capacity(Some(ResourceType::Energy)) as i32,
        capacity => capacity as i32,
    }
}

fn store_at(v: &JsValue) -> Store {
    if absent(v) {
        Store::empty()
    } else {
        store_of(v.unchecked_ref())
    }
}

fn part_of(part: screeps::Part) -> BodyPart {
    match part {
        screeps::Part::Work => BodyPart::Work,
        screeps::Part::Carry => BodyPart::Carry,
        screeps::Part::Move => BodyPart::Move,
        screeps::Part::Attack => BodyPart::Attack,
        screeps::Part::RangedAttack => BodyPart::RangedAttack,
        screeps::Part::Heal => BodyPart::Heal,
        screeps::Part::Claim => BodyPart::Claim,
        screeps::Part::Tough => BodyPart::Tough,
        _ => BodyPart::Tough,
    }
}

fn boost_of(kind: BodyPart, boost: Option<ResourceType>) -> Boost {
    match boost {
        Some(ty) => vocabulary::boost::of_compound(kind, &ty.to_string()),
        None => Boost::Unboosted,
    }
}

fn owner_of(owner: Option<objects::Owner>) -> Owner {
    match owner {
        Some(owner) => Owner::Theirs(owner.username()),
        None => Owner::Nobody,
    }
}

fn creep_of(creep: &objects::Creep) -> Creep {
    Creep {
        id: String::from(creep.try_js_raw_id().unwrap_or_default()),
        name: creep.name(),
        owner: if creep.my() {
            Owner::Ours
        } else {
            Owner::Theirs(creep.owner().username())
        },
        pos: pos_of(&creep.js_pos()),
        body: creep
            .body()
            .into_iter()
            .map(|part| {
                let kind = part_of(part.part());
                Part {
                    kind,
                    hits: part.hits() as i32,
                    boost: boost_of(kind, part.boost()),
                }
            })
            .collect(),
        hits: creep.hits() as i32,
        hits_max: creep.hits_max() as i32,
        ticks_to_live: creep.ticks_to_live().unwrap_or(0) as i32,
        fatigue: creep.fatigue() as i32,
        store: store_of(&creep.store()),
    }
}

/// The engine's fallen creep keeps no hits or fatigue.
fn fallen_of(creep: &JsValue) -> Creep {
    let mut creep = fallen_creep(creep);
    creep.hits = 0;
    creep.fatigue = 0;
    creep
}

/// A tombstone's `creep` is a bare object the crate does not type.
fn fallen_creep(creep: &JsValue) -> Creep {
    let body = match field(creep, "body").dyn_into::<Array>() {
        Ok(array) => (0..array.length())
            .map(|i| {
                let part = array.get(i);
                let kind = field(&part, "type")
                    .as_string()
                    .as_deref()
                    .and_then(vocabulary::part_of_name)
                    .unwrap_or(BodyPart::Tough);
                let boost = field(&part, "boost");
                Part {
                    kind,
                    hits: int_of(&field(&part, "hits")),
                    boost: if absent(&boost) {
                        Boost::Unboosted
                    } else {
                        vocabulary::boost::of_compound(kind, &boost.as_string().unwrap_or_default())
                    },
                }
            })
            .collect(),
        Err(_) => Vec::new(),
    };
    let pos = field(creep, "pos");
    Creep {
        id: field(creep, "id").as_string().unwrap_or_default(),
        name: field(creep, "name").as_string().unwrap_or_default(),
        owner: match field(creep, "my").as_bool() {
            Some(true) => Owner::Ours,
            _ => {
                let owner = field(creep, "owner");
                if absent(&owner) {
                    Owner::Nobody
                } else {
                    Owner::Theirs(field(&owner, "username").as_string().unwrap_or_default())
                }
            }
        },
        pos: Pos::new(
            field(&pos, "roomName").as_string().unwrap_or_default(),
            int_of(&field(&pos, "x")),
            int_of(&field(&pos, "y")),
        ),
        body,
        hits: int_of(&field(creep, "hits")),
        hits_max: int_of(&field(creep, "hitsMax")),
        ticks_to_live: int_of(&field(creep, "ticksToLive")),
        fatigue: int_of(&field(creep, "fatigue")),
        store: store_at(&field(creep, "store")),
    }
}

fn ownable(kind: &Kind) -> bool {
    !matches!(
        kind,
        Kind::Road | Kind::Wall | Kind::Portal | Kind::PowerBank | Kind::Other(_)
    )
}

fn structure_of(s: &JsValue) -> Structure {
    let base: &objects::Structure = s.unchecked_ref();
    let kind = Kind::of_name(&field(s, "structureType").as_string().unwrap_or_default());
    let owner = if ownable(&kind) {
        let owned: &objects::OwnedStructure = s.unchecked_ref();
        if owned.my() {
            Owner::Ours
        } else {
            owner_of(owned.owner())
        }
    } else {
        Owner::Nobody
    };
    Structure {
        id: String::from(base.try_js_raw_id().unwrap_or_default()),
        pos: pos_of(&base.js_pos()),
        owner: owner.clone(),
        hits: base.hits() as i32,
        hits_max: base.hits_max() as i32,
        store: store_at(&field(s, "store")),
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

fn collapse_of(s: &JsValue, kind: &Kind) -> i32 {
    if !kind.is_invader_core() {
        return 0;
    }
    let effects = field(s, "effects");
    let found = match effects.dyn_into::<Array>() {
        Ok(array) => (0..array.length())
            .map(|i| array.get(i))
            .find(|e| int_of(&field(e, "effect")) == 1002),
        Err(_) => None,
    };
    match found {
        Some(e) => int_of(&field(&e, "ticksRemaining")),
        None => int_of(&field(s, "ticksToDeploy")) + engine::STRONGHOLD_LONGEST_COLLAPSE,
    }
}

fn site_of(s: &JsValue) -> Site {
    let base: &objects::ConstructionSite = s.unchecked_ref();
    Site {
        id: String::from(base.try_js_raw_id().unwrap_or_default()),
        kind: Kind::of_name(&field(s, "structureType").as_string().unwrap_or_default()),
        pos: pos_of(&base.js_pos()),
        owner: if base.my() {
            Owner::Ours
        } else {
            let owner = base.owner();
            if jsval(&owner).is_null() {
                Owner::Nobody
            } else {
                Owner::Theirs(owner.username())
            }
        },
        progress: base.progress() as i32,
        progress_total: base.progress_total() as i32,
    }
}

fn controller_of(c: &objects::StructureController) -> Controller {
    Controller {
        id: String::from(c.try_js_raw_id().unwrap_or_default()),
        pos: pos_of(&c.js_pos()),
        owner: if c.my() {
            Owner::Ours
        } else {
            owner_of(c.owner())
        },
        level: c.level() as i32,
        progress: c.progress().unwrap_or(0) as i32,
        progress_total: c.progress_total().unwrap_or(0) as i32,
        ticks_to_downgrade: c.ticks_to_downgrade().unwrap_or(0) as i32,
        reservation: c
            .reservation()
            .map(|r| (r.username(), r.ticks_to_end() as i32)),
        safe_mode: c.safe_mode().unwrap_or(0) as i32,
        safe_mode_available: c.safe_mode_available() as i32,
        safe_mode_cooldown: c.safe_mode_cooldown().unwrap_or(0) as i32,
        upgrade_blocked: c.upgrade_blocked().unwrap_or(0) as i32,
    }
}

fn source_of(s: &objects::Source) -> Source {
    Source {
        id: String::from(s.try_js_raw_id().unwrap_or_default()),
        pos: pos_of(&s.js_pos()),
        energy: s.energy() as i32,
        energy_capacity: s.energy_capacity() as i32,
        ticks_to_regeneration: s.ticks_to_regeneration().unwrap_or(0) as i32,
    }
}

fn mineral_of(m: &objects::Mineral) -> Mineral {
    Mineral {
        id: String::from(m.try_js_raw_id().unwrap_or_default()),
        pos: pos_of(&m.js_pos()),
        resource: resource_of(m.mineral_type()),
        amount: m.mineral_amount() as i32,
    }
}

fn pile_of(p: &objects::Resource) -> Pile {
    Pile {
        id: String::from(p.try_js_raw_id().unwrap_or_default()),
        pos: pos_of(&p.js_pos()),
        resource: resource_of(p.resource_type()),
        amount: p.amount() as i32,
    }
}

fn remains_of_js(r: &JsValue) -> Remains {
    let creep = field(r, "creep");
    Remains {
        id: field(r, "id").as_string().unwrap_or_default(),
        pos: Pos::new(
            field(&field(r, "pos"), "roomName")
                .as_string()
                .unwrap_or_default(),
            int_of(&field(&field(r, "pos"), "x")),
            int_of(&field(&field(r, "pos"), "y")),
        ),
        store: store_at(&field(r, "store")),
        fallen: if absent(&creep) {
            None
        } else {
            Some(Fallen {
                creep: fallen_of(&creep),
                died: int_of(&field(r, "deathTime")),
            })
        },
    }
}

/// `Game.map.getRoomTerrain(name).getRawBuffer()`: the map serves rooms the
/// bot cannot see, and reading it needs no `Room` object.
fn terrain_of(name: &str) -> Vec<u8> {
    let game = match Reflect::get(&js_sys::global(), &JsValue::from_str("Game")) {
        Ok(game) => game,
        Err(_) => return Vec::new(),
    };
    let map = field(&game, "map");
    let get_terrain = field(&map, "getRoomTerrain");
    let arguments = Array::new();
    arguments.push(&JsValue::from_str(name));
    let terrain = match Reflect::apply(
        get_terrain.unchecked_ref::<js_sys::Function>(),
        &map,
        &arguments,
    ) {
        Ok(terrain) => terrain,
        Err(_) => return Vec::new(),
    };
    let get_raw = field(&terrain, "getRawBuffer");
    match Reflect::apply(
        get_raw.unchecked_ref::<js_sys::Function>(),
        &terrain,
        &Array::new(),
    ) {
        Ok(raw) => js_sys::Uint8Array::new(&raw).to_vec(),
        Err(_) => Vec::new(),
    }
}

fn room_of(room: &objects::Room) -> Room {
    let name = room.name().to_string();
    let controller = room.controller().as_ref().map(controller_of);

    let mut remains: Vec<Remains> = Vec::new();
    for ty in [find::RoomObject::Tombstones, find::RoomObject::Ruins] {
        for object in room.find(ty, None) {
            remains.push(remains_of_js(&jsval(&object)));
        }
    }

    Room {
        name,
        terrain: terrain_of(&room.name().to_string()),
        controller,
        energy_available: room.energy_available() as i32,
        energy_capacity: room.energy_capacity_available() as i32,
        sources: room
            .find(find::SOURCES, None)
            .iter()
            .map(source_of)
            .collect(),
        minerals: room
            .find(find::MINERALS, None)
            .iter()
            .map(mineral_of)
            .collect(),
        structures: room
            .find(find::RoomObject::Structures, None)
            .iter()
            .map(|s| structure_of(&jsval(s)))
            .collect(),
        sites: room
            .find(find::RoomObject::ConstructionSites, None)
            .iter()
            .map(|s| site_of(&jsval(s)))
            .collect(),
        piles: room
            .find(find::DROPPED_RESOURCES, None)
            .iter()
            .map(pile_of)
            .collect(),
        remains,
        hostiles: room
            .find(find::HOSTILE_CREEPS, None)
            .iter()
            .map(creep_of)
            .collect(),
    }
}

fn spawn_of(spawn: &objects::StructureSpawn, creeps: &JsValue) -> Spawn {
    Spawn {
        id: String::from(spawn.try_js_raw_id().unwrap_or_default()),
        name: spawn.name(),
        pos: pos_of(&spawn.js_pos()),
        spawning: spawn.spawning().map(|s| {
            let name: String = s.name().into();
            let creep = field(creeps, &name);
            Oven {
                remaining: s.remaining_time() as i32,
                body: if absent(&creep) {
                    Vec::new()
                } else {
                    match field(&creep, "body").dyn_into::<Array>() {
                        Ok(array) => (0..array.length())
                            .map(|i| {
                                let part = array.get(i);
                                let kind = field(&part, "type")
                                    .as_string()
                                    .as_deref()
                                    .and_then(vocabulary::part_of_name)
                                    .unwrap_or(BodyPart::Tough);
                                let boost = field(&part, "boost");
                                Part {
                                    kind,
                                    hits: int_of(&field(&part, "hits")),
                                    boost: if absent(&boost) {
                                        Boost::Unboosted
                                    } else {
                                        vocabulary::boost::of_compound(
                                            kind,
                                            &boost.as_string().unwrap_or_default(),
                                        )
                                    },
                                }
                            })
                            .collect(),
                        Err(_) => Vec::new(),
                    }
                },
                name,
            }
        }),
    }
}

fn game_creeps() -> JsValue {
    match Reflect::get(&js_sys::global(), &JsValue::from_str("Game")) {
        Ok(game) => field(&game, "creeps"),
        Err(_) => JsValue::UNDEFINED,
    }
}

pub fn of_game() -> World {
    let time = game::time() as i32;
    let cpu_limit = game::cpu::limit() as f64;
    let bucket = game::cpu::bucket();

    let mut room_entries: Vec<(String, objects::Room)> = game::rooms()
        .entries()
        .map(|(name, room)| (name.to_string(), room))
        .collect();
    room_entries.sort_by(|a, b| a.0.cmp(&b.0));

    let mut creep_entries: Vec<(String, objects::Creep)> = game::creeps().entries().collect();
    creep_entries.sort_by(|a, b| a.0.cmp(&b.0));

    let mut spawn_entries: Vec<(String, objects::StructureSpawn)> =
        game::spawns().entries().collect();
    spawn_entries.sort_by(|a, b| a.0.cmp(&b.0));

    let creeps_js = game_creeps();
    let rooms = room_entries
        .iter()
        .map(|(_, r)| room_of(r))
        .collect::<Vec<_>>();
    let spawns = spawn_entries
        .iter()
        .map(|(_, s)| spawn_of(s, &creeps_js))
        .collect::<Vec<_>>();
    let creeps = creep_entries
        .iter()
        .filter(|(_, c)| !c.spawning())
        .map(|(_, c)| creep_of(c))
        .collect::<Vec<_>>();

    let me = match (spawn_entries.first(), creep_entries.first()) {
        (Some((_, spawn)), _) => spawn
            .owner()
            .map(|owner| owner.username())
            .unwrap_or_default(),
        (None, Some((_, creep))) => creep.owner().username(),
        (None, None) => room_entries
            .iter()
            .find_map(|(_, room)| {
                room.controller()
                    .filter(|c| c.my())
                    .and_then(|c| c.owner())
                    .map(|owner| owner.username())
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
