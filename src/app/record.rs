//! The recorder's ring: the last five ticks' `World` in the shape the F#
//! bundle's `JSON.stringify` gives it, so `observe capture` and `smoke` read
//! either tree's segments. Fable prints a union as its case name alone when it
//! carries nothing and as `[case, ...fields]` when it does; records keep field
//! order, and an option is `null` or its payload.

use js_sys::{Array, Object, Reflect};
use screeps::raw_memory;
use wasm_bindgen::JsValue;

use crate::core::vocabulary::{BodyPart, Boost, Compound, Ore, Resource};
use crate::core::world::{
    Controller, Creep, Fallen, Kind, Mineral, Oven, Part, Pile, Pos, Remains, Room, Site, Source,
    Spawn, Store, Structure, World,
};

const SLOTS: i32 = 5;
const FIRST: u8 = 10;
const LIMIT: usize = 100 * 1024;

fn object(entries: Vec<(&str, JsValue)>) -> JsValue {
    let object = Object::new();
    for (name, value) in entries {
        if !value.is_undefined() {
            let _ = Reflect::set(&object, &JsValue::from_str(name), &value);
        }
    }
    object.into()
}

fn array(values: Vec<JsValue>) -> JsValue {
    Array::from(&values.into_iter().collect::<Array>()).into()
}

fn case(name: &str) -> JsValue {
    JsValue::from_str(name)
}

fn tag(name: &str, fields: Vec<JsValue>) -> JsValue {
    let mut values = vec![case(name)];
    values.extend(fields);
    array(values)
}

fn n(value: i32) -> JsValue {
    JsValue::from(value)
}

fn f(value: f64) -> JsValue {
    JsValue::from_f64(value)
}

fn text(value: &str) -> JsValue {
    JsValue::from_str(value)
}

/// Fable prints `None` as `undefined`, which `JSON.stringify` drops.
fn none() -> JsValue {
    JsValue::UNDEFINED
}

fn some(value: JsValue) -> JsValue {
    value
}

fn pos(value: &Pos) -> JsValue {
    object(vec![
        ("Room", text(&value.room)),
        ("X", n(value.x)),
        ("Y", n(value.y)),
    ])
}

fn owner(value: &crate::core::world::Owner) -> JsValue {
    match value {
        crate::core::world::Owner::Ours => case("Ours"),
        crate::core::world::Owner::Theirs(username) => tag("Theirs", vec![text(username)]),
        crate::core::world::Owner::Nobody => case("Nobody"),
    }
}

fn resource(value: &Resource) -> JsValue {
    match value {
        Resource::Energy => case("Energy"),
        Resource::Ore(Ore::Hydrogen) => tag("Ore", vec![case("Hydrogen")]),
        Resource::Ore(Ore::Oxygen) => tag("Ore", vec![case("Oxygen")]),
        Resource::Ore(Ore::Utrium) => tag("Ore", vec![case("Utrium")]),
        Resource::Ore(Ore::Keanium) => tag("Ore", vec![case("Keanium")]),
        Resource::Ore(Ore::Lemergium) => tag("Ore", vec![case("Lemergium")]),
        Resource::Ore(Ore::Zynthium) => tag("Ore", vec![case("Zynthium")]),
        Resource::Ore(Ore::Catalyst) => tag("Ore", vec![case("Catalyst")]),
        Resource::Compound(Compound::Hydroxide) => tag("Compound", vec![case("Hydroxide")]),
        Resource::Compound(Compound::UtriumHydride) => tag("Compound", vec![case("UtriumHydride")]),
        Resource::Compound(Compound::UtriumAcid) => tag("Compound", vec![case("UtriumAcid")]),
        Resource::Other(name) => tag("Other", vec![text(name)]),
    }
}

fn part_kind(value: BodyPart) -> JsValue {
    case(match value {
        BodyPart::Work => "Work",
        BodyPart::Carry => "Carry",
        BodyPart::Move => "Move",
        BodyPart::Attack => "Attack",
        BodyPart::RangedAttack => "RangedAttack",
        BodyPart::Heal => "Heal",
        BodyPart::Claim => "Claim",
        BodyPart::Tough => "Tough",
    })
}

fn boost(value: Boost) -> JsValue {
    case(match value {
        Boost::Unboosted => "Unboosted",
        Boost::Tier1 => "Tier1",
        Boost::Tier2 => "Tier2",
        Boost::Tier3 => "Tier3",
    })
}

fn kind(value: &Kind) -> JsValue {
    match value {
        Kind::Spawn => case("Spawn"),
        Kind::Extension => case("Extension"),
        Kind::Tower => case("Tower"),
        Kind::Road => case("Road"),
        Kind::Container => case("Container"),
        Kind::Storage => case("Storage"),
        Kind::Link => case("Link"),
        Kind::Rampart => case("Rampart"),
        Kind::Wall => case("Wall"),
        Kind::Extractor => case("Extractor"),
        Kind::Terminal => case("Terminal"),
        Kind::Observer => case("Observer"),
        Kind::Lab => case("Lab"),
        Kind::Factory => case("Factory"),
        Kind::PowerSpawn => case("PowerSpawn"),
        Kind::Nuker => case("Nuker"),
        Kind::KeeperLair => case("KeeperLair"),
        Kind::InvaderCore => case("InvaderCore"),
        Kind::PowerBank => case("PowerBank"),
        Kind::Portal => case("Portal"),
        Kind::Other(name) => tag("Other", vec![text(name)]),
    }
}

fn store(value: &Store) -> JsValue {
    let held: Vec<JsValue> = value
        .held
        .iter()
        .map(|(each, amount)| array(vec![resource(each), n(*amount)]))
        .collect();
    object(vec![("Held", array(held)), ("Capacity", n(value.capacity))])
}

fn part(value: &Part) -> JsValue {
    object(vec![
        ("Kind", part_kind(value.kind)),
        ("Hits", n(value.hits)),
        ("Boost", boost(value.boost)),
    ])
}

fn body(value: &[Part]) -> JsValue {
    array(value.iter().map(part).collect())
}

fn creep(value: &Creep) -> JsValue {
    object(vec![
        ("Id", text(&value.id)),
        ("Name", text(&value.name)),
        ("Owner", owner(&value.owner)),
        ("Pos", pos(&value.pos)),
        ("Body", body(&value.body)),
        ("Hits", n(value.hits)),
        ("HitsMax", n(value.hits_max)),
        ("TicksToLive", n(value.ticks_to_live)),
        ("Fatigue", n(value.fatigue)),
        ("Store", store(&value.store)),
    ])
}

fn fallen(value: &Fallen) -> JsValue {
    object(vec![
        ("Creep", creep(&value.creep)),
        ("Died", n(value.died)),
    ])
}

fn remains(value: &Remains) -> JsValue {
    object(vec![
        ("Id", text(&value.id)),
        ("Pos", pos(&value.pos)),
        ("Store", store(&value.store)),
        (
            "Fallen",
            match &value.fallen {
                Some(each) => some(fallen(each)),
                None => none(),
            },
        ),
    ])
}

fn structure(value: &Structure) -> JsValue {
    object(vec![
        ("Id", text(&value.id)),
        ("Kind", kind(&value.kind)),
        ("Pos", pos(&value.pos)),
        ("Owner", owner(&value.owner)),
        ("Hits", n(value.hits)),
        ("HitsMax", n(value.hits_max)),
        ("Store", store(&value.store)),
        ("Cooldown", n(value.cooldown)),
        ("Collapse", n(value.collapse)),
        ("Level", n(value.level)),
        ("Walkable", JsValue::from_bool(value.walkable)),
    ])
}

fn site(value: &Site) -> JsValue {
    object(vec![
        ("Id", text(&value.id)),
        ("Kind", kind(&value.kind)),
        ("Pos", pos(&value.pos)),
        ("Owner", owner(&value.owner)),
        ("Progress", n(value.progress)),
        ("ProgressTotal", n(value.progress_total)),
    ])
}

fn source(value: &Source) -> JsValue {
    object(vec![
        ("Id", text(&value.id)),
        ("Pos", pos(&value.pos)),
        ("Energy", n(value.energy)),
        ("EnergyCapacity", n(value.energy_capacity)),
        ("TicksToRegeneration", n(value.ticks_to_regeneration)),
    ])
}

fn mineral(value: &Mineral) -> JsValue {
    object(vec![
        ("Id", text(&value.id)),
        ("Pos", pos(&value.pos)),
        ("Resource", resource(&value.resource)),
        ("Amount", n(value.amount)),
    ])
}

fn pile(value: &Pile) -> JsValue {
    object(vec![
        ("Id", text(&value.id)),
        ("Pos", pos(&value.pos)),
        ("Resource", resource(&value.resource)),
        ("Amount", n(value.amount)),
    ])
}

fn controller(value: &Controller) -> JsValue {
    object(vec![
        ("Id", text(&value.id)),
        ("Pos", pos(&value.pos)),
        ("Owner", owner(&value.owner)),
        ("Level", n(value.level)),
        ("Progress", n(value.progress)),
        ("ProgressTotal", n(value.progress_total)),
        ("TicksToDowngrade", n(value.ticks_to_downgrade)),
        (
            "Reservation",
            match &value.reservation {
                Some((username, ticks)) => some(array(vec![text(username), n(*ticks)])),
                None => none(),
            },
        ),
        ("SafeMode", n(value.safe_mode)),
        ("SafeModeAvailable", n(value.safe_mode_available)),
        ("SafeModeCooldown", n(value.safe_mode_cooldown)),
        ("UpgradeBlocked", n(value.upgrade_blocked)),
    ])
}

fn terrain(value: &[u8]) -> JsValue {
    text(
        &value
            .iter()
            .map(|tile| char::from(b'0' + tile))
            .collect::<String>(),
    )
}

fn room(value: &Room) -> JsValue {
    let creeps = |list: &[Creep]| array(list.iter().map(creep).collect());

    object(vec![
        ("Name", text(&value.name)),
        ("Terrain", terrain(&value.terrain)),
        (
            "Controller",
            match &value.controller {
                Some(each) => some(controller(each)),
                None => none(),
            },
        ),
        ("EnergyAvailable", n(value.energy_available)),
        ("EnergyCapacity", n(value.energy_capacity)),
        ("Sources", array(value.sources.iter().map(source).collect())),
        (
            "Minerals",
            array(value.minerals.iter().map(mineral).collect()),
        ),
        (
            "Structures",
            array(value.structures.iter().map(structure).collect()),
        ),
        ("Sites", array(value.sites.iter().map(site).collect())),
        ("Piles", array(value.piles.iter().map(pile).collect())),
        (
            "Remains",
            array(value.remains.iter().map(remains).collect()),
        ),
        ("Hostiles", creeps(&value.hostiles)),
    ])
}

fn oven(value: &Oven) -> JsValue {
    object(vec![
        ("Name", text(&value.name)),
        ("Remaining", n(value.remaining)),
        ("Body", body(&value.body)),
    ])
}

fn spawn(value: &Spawn) -> JsValue {
    object(vec![
        ("Id", text(&value.id)),
        ("Name", text(&value.name)),
        ("Pos", pos(&value.pos)),
        (
            "Spawning",
            match &value.spawning {
                Some(each) => some(oven(each)),
                None => none(),
            },
        ),
    ])
}

fn world(value: &World) -> JsValue {
    object(vec![
        ("Time", n(value.time)),
        ("Me", text(&value.me)),
        ("CpuLimit", f(value.cpu_limit)),
        ("Bucket", n(value.bucket)),
        ("Rooms", array(value.rooms.iter().map(room).collect())),
        ("Spawns", array(value.spawns.iter().map(spawn).collect())),
        ("Creeps", array(value.creeps.iter().map(creep).collect())),
        ("OurSites", n(value.our_sites)),
    ])
}

/// A JS string's length, which is what the F# measures the segment against.
fn length(value: &str) -> usize {
    value.chars().map(char::len_utf16).sum()
}

pub fn record(time: i32, world: &World) -> Option<String> {
    let memory = raw_memory::get();
    let memory_bytes = memory.as_string().map_or(0, |each| length(&each));

    let recorded = object(vec![
        ("tick", n(time)),
        ("memoryBytes", n(memory_bytes as i32)),
        ("world", self::world(world)),
    ]);
    let text = js_sys::JSON::stringify(&recorded)
        .ok()
        .and_then(|text| text.as_string())
        .unwrap_or_default();

    if length(&text) > LIMIT {
        return Some(format!(
            "recorder: tick {} is {} bytes, over a segment",
            time,
            length(&text)
        ));
    }

    raw_memory::segments().set((i32::from(FIRST) + time % SLOTS) as u8, text);
    None
}
