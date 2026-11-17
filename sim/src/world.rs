//! A `worlds/`-shaped JSON (`scripts/sim/export.mjs`) stood as a krebs world,
//! our first spawn on it as `soak.mjs`'s respawn stands it.
use std::sync::Arc;

use fabot_core::layout;
use fabot_core::world::{Pos, RoomName, Rules};
use krebs::mapgen::{self, RoomKind};
use krebs::room_name::RoomXY;
use krebs::scenario::GlobalScenario;
use krebs::world::{EngineConfig, RoomSnapshot, World};
use krebs_util::json::Json;
use screeps::{SAFE_MODE_DURATION, SPAWN_ENERGY_CAPACITY, SPAWN_ENERGY_START, SPAWN_HITS};
use serde_json::{Value, json};

pub const US: &str = "u1";
pub const USERNAME: &str = "fabot";
pub const INVADER: &str = "2";
pub const KEEPER: &str = "3";

pub struct Map {
    pub rooms: Vec<(String, String)>,
    objects: Vec<Value>,
    time: i64,
}

/// Each room's terrain as 2500 digits, row by row; rows or one string.
pub fn read(path: &str) -> Map {
    let text = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("{path}: {e}"));
    let world: Value = serde_json::from_str(&text).expect("the world is JSON");
    let mut map = Map {
        rooms: Vec::new(),
        objects: Vec::new(),
        time: world["time"].as_i64().unwrap_or(0),
    };
    for room in world["rooms"].as_array().expect("rooms") {
        let name = room["name"].as_str().expect("a room's name").to_string();
        let terrain = match &room["terrain"] {
            Value::Array(rows) => rows.iter().filter_map(Value::as_str).collect(),
            t => t.as_str().unwrap_or_default().to_string(),
        };
        assert_eq!(terrain.len(), 2500, "{name}: terrain");
        map.objects
            .extend(room["objects"].as_array().into_iter().flatten().cloned());
        map.rooms.push((name, terrain));
    }
    map
}

impl Map {
    pub fn terrain(&self, room: &str) -> &[u8] {
        let at = self.rooms.iter().find(|(n, _)| n == room);
        at.unwrap_or_else(|| panic!("{room}: not in the world"))
            .1
            .as_bytes()
    }

    fn in_room(&self, room: &str) -> impl Iterator<Item = &Value> {
        self.objects.iter().filter(move |o| o["room"] == room)
    }

    /// The tile Layout's plan puts the first spawn on, as `soak.mjs` seats a respawn.
    pub fn seat(&self, room: &str) -> (i64, i64) {
        let name = RoomName::parse(room).expect("a room name");
        let at = |o: &Value| {
            let xy = |k: &str| o[k].as_i64().expect("a tile") as i32;
            Pos::new(name, xy("x"), xy("y"))
        };
        let of = |kind: &str| -> Vec<Pos> {
            self.in_room(room)
                .filter(|o| o["type"] == kind)
                .map(at)
                .collect()
        };
        let controller = *of("controller").first().expect("a controller");
        let terrain = self.terrain(room).iter().map(|b| b - b'0').collect();
        let bare = layout::bare(
            name,
            terrain,
            (controller, &of("source"), &of("mineral")),
            1,
        );
        let (_, tile) = layout::first_spawn(&Rules::vanilla(), &bare).expect("a plan");
        let pos = fabot_core::tiles::to_pos(name, tile);
        (i64::from(pos.x), i64::from(pos.y))
    }

    /// Our user, the room's controller ours at level 1, in safe mode unless `ended`, and Spawn1 on `(x, y)`.
    pub fn stand(&self, room: &str, (x, y): (i64, i64), seed: u64, ended: bool) -> World {
        let t = self.time;
        let mut objects: Vec<Value> = Vec::new();
        for o in &self.objects {
            let mut o = o.clone();
            match o["type"].as_str() {
                Some("controller") if o["room"] == room => {
                    let ours = json!({ "level": 1, "user": US, "progress": 0, "downgradeTime": null,
                        "safeMode": if ended { 0 } else { t + i64::from(SAFE_MODE_DURATION) }, "safeModeAvailable": 0 });
                    merge(&mut o, ours);
                }
                Some("controller") => merge(&mut o, json!({ "level": 0 })),
                Some("source") => merge(&mut o, json!({ "invaderHarvested": 0 })),
                _ => {}
            }
            objects.push(o);
        }
        // A harness fixture: any id serves, and Spawn1 is a respawn's first spawn's name.
        objects.push(json!({ "_id": "f0000000000000000000spwn", "type": "spawn", "room": room, "x": x, "y": y,
            "name": "Spawn1", "user": US, "store": { "energy": SPAWN_ENERGY_START }, "storeCapacityResource": { "energy": SPAWN_ENERGY_CAPACITY },
            "hits": SPAWN_HITS, "hitsMax": SPAWN_HITS, "spawning": null, "notifyWhenAttacked": true }));
        let user = |id: &str, name: &str| {
            json!({ "_id": id, "username": name, "usernameLower": name.to_lowercase(),
            "cpu": 100, "gcl": 0, "cpuAvailable": 10000, "active": 10000, "rooms": [], "money": 0, "power": 0 })
        };
        let mut us = user(US, USERNAME);
        us["rooms"] = json!([room]);
        let db = json!({
            "rooms": self.rooms.iter().map(|(n, _)| room_doc(n)).collect::<Vec<_>>(),
            "rooms.terrain": self.rooms.iter().map(|(n, t)| json!({ "room": n, "terrain": t })).collect::<Vec<_>>(),
            "users": [us, user(INVADER, "Invader"), user(KEEPER, "Source Keeper")],
            "rooms.objects": objects,
        });
        let scenario = json!({ "kind": "global", "worldSeed": format!("0x{seed:x}"),
            "clock": { "baseMs": 0, "baseTick": 0, "nominalTickMs": crate::TICK_MS }, "gameTime": t, "worldSize": 0,
            "compat": "private", "db": db });
        let text = Json::parse(&scenario.to_string()).expect("krebs reads JSON");
        let mut w = GlobalScenario::from_json(&text)
            .and_then(|s| s.world())
            .expect("krebs stands the world");
        w.config = Arc::new(EngineConfig {
            history: false,
            ..(*w.config).clone()
        });
        // An import derives no presence: until a room is processed nobody sees it.
        for r in 0..w.rooms.len() {
            let s = &w.rooms[r];
            let (info, objects, flags) = (s.info.clone(), s.objects.to_vec(), s.flags.clone());
            let seen = RoomSnapshot::from_docs(
                info,
                objects,
                flags,
                s.event_log.clone(),
                s.map_view.clone(),
                s.processed_at,
                &w.user_ids,
            );
            w.rooms[r] = Arc::new(seen);
            w.active.insert(r as u32);
        }
        w
    }
}

/// A `rooms` doc as the official world generator lays it out: a highway a
/// bus with its sector's deposit type, a keeper room marked so.
fn room_doc(name: &str) -> Value {
    let mut doc = json!({ "_id": name, "status": "normal" });
    let Some(xy) = RoomXY::parse_engine(name) else {
        return doc;
    };
    match mapgen::room_kind(xy) {
        RoomKind::Highway => merge(
            &mut doc,
            json!({ "bus": true, "depositType": mapgen::deposit_type(xy) }),
        ),
        kind => merge(
            &mut doc,
            json!({ "sourceKeepers": kind == RoomKind::Keeper }),
        ),
    }
    doc
}

fn merge(o: &mut Value, with: Value) {
    if let (Value::Object(o), Value::Object(with)) = (o, with) {
        o.extend(with);
    }
}
