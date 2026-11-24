//! A `worlds/`-shaped JSON (`scripts/scenario/worlds/`) stood as a krebs world,
//! our first spawn on it as `soak.mjs`'s respawn stands it.
use std::sync::Arc;

use fabot_base::tiles::{self, Tile};
use fabot_base::world::{Pos, RoomName, Rules};
use fabot_layout as layout;
use krebs::scenario::GlobalScenario;
use krebs::world::{EngineConfig, RoomSnapshot, World};
use krebs_util::json::Json;
use screeps::{SAFE_MODE_DURATION, SPAWN_ENERGY_CAPACITY, SPAWN_ENERGY_START, SPAWN_HITS};
use serde_json::{Value, json};

use crate::seat::Seat;

pub const US: &str = "u1";
pub const USERNAME: &str = "fabot";
pub const INVADER: &str = "2";
pub const RIVAL: (&str, &str) = ("u2", "rival");
pub const KEEPER: &str = "3";

#[derive(Default)]
pub struct Map {
    pub rooms: Vec<(String, String)>,
    pub objects: Vec<Value>,
    time: i64,
    /// Memory's text at the start: what `scripts/sim/run.mjs` stood as `simMemory`.
    pub memory: String,
    /// A capture's intel rows (segment 2).
    pub intel: String,
    /// The other players, by id, and our GCL: a capture's.
    users: Vec<(String, String)>,
    gcl: i64,
    /// The rooms whose status is not our colonies' (a capture's `closed`): the engine lets no creep in.
    pub closed: Vec<String>,
    /// A generated world's seed, which sets its buses' power banks due.
    pub banks: Option<u64>,
}

/// Each room's terrain as 2500 digits, row by row; rows or one string.
pub fn read(path: &str) -> Map {
    let text = std::fs::read_to_string(path).unwrap_or_else(|e| panic!("{path}: {e}"));
    let world: Value = serde_json::from_str(&text).expect("the world is JSON");
    let mut map = Map {
        time: world["time"].as_i64().unwrap_or(0),
        memory: world
            .get("simMemory")
            .map_or_else(String::new, Value::to_string),
        intel: (world["segments"]["2"].as_str()).unwrap_or_default().into(),
        users: (world["users"].as_object().into_iter().flatten())
            .filter(|(id, _)| *id != US && *id != INVADER && *id != KEEPER)
            .map(|(id, name)| (id.clone(), name.as_str().unwrap_or(id).to_string()))
            .collect(),
        gcl: world["gcl"].as_i64().unwrap_or(0),
        closed: (world["closed"].as_array().into_iter().flatten())
            .filter_map(|r| r.as_str().map(str::to_string))
            .collect(),
        ..Map::default()
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

    pub fn terrains(&self) -> Vec<(RoomName, screeps::LocalRoomTerrain)> {
        let digits = |t: &str| t.bytes().map(|b| b - b'0').collect::<Vec<_>>();
        (self.rooms.iter())
            .filter_map(|(name, t)| Some((name.parse().ok()?, digits(t).try_into().ok()?)))
            .map(|(name, bits)| (name, screeps::LocalRoomTerrain::new_from_bits(bits)))
            .collect()
    }

    /// The tile Layout's plan puts the first spawn on, as `soak.mjs` seats a respawn.
    pub fn seat(&self, room: &str) -> Option<Tile> {
        layout::first_spawn(&Rules::vanilla(), &self.bare(room)).map(|(_, tile, _)| tile)
    }

    pub fn bare(&self, room: &str) -> fabot_base::world::Room<'static> {
        let name = room.parse::<RoomName>().expect("a room name");
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
        let rocks = (controller, &of("source")[..], &of("mineral")[..]);
        layout::bare(name, terrain, rocks, 1)
    }

    /// Each seat's user, its room's controller its own (at level 1 unless the world's already is), in safe
    /// mode unless `ended` or the world says, and Spawn1 on Layout's tile unless a spawn stands.
    pub fn stand(&self, seats: &[Seat], seed: u64, ended: bool, badge: Option<Value>) -> World {
        let t = self.time;
        let mut objects: Vec<Value> = Vec::new();
        for o in &self.objects {
            let mut o = o.clone();
            let seat = seats.iter().find(|s| o["room"] == s.room);
            match (o["type"].as_str(), seat) {
                (Some("controller"), Some(s)) => {
                    if o["user"] != s.uid {
                        let ours = json!({ "level": 1, "user": s.uid, "progress": 0, "downgradeTime": null, "safeModeAvailable": 0 });
                        merge(&mut o, ours);
                    }
                    if o.get("safeMode").is_none() {
                        let safe = if ended {
                            0
                        } else {
                            t + i64::from(SAFE_MODE_DURATION)
                        };
                        merge(&mut o, json!({ "safeMode": safe }));
                    }
                }
                (Some("controller"), _) if o["user"].is_null() => {
                    merge(&mut o, json!({ "level": 0 }))
                }
                (Some("source"), _) => merge(&mut o, json!({ "invaderHarvested": 0 })),
                _ => {}
            }
            objects.push(o);
        }
        // A harness fixture: any id serves, and Spawn1 is a respawn's first spawn's name.
        for (i, Seat { uid, room, .. }) in seats.iter().enumerate() {
            if self.in_room(room).any(|o| o["type"] == "spawn") {
                continue;
            }
            let tile = self.seat(room).expect("a plan");
            let (x, y) = (tiles::x_of(tile), tiles::y_of(tile));
            eprintln!("spawn at {x},{y}");
            objects.push(json!({ "_id": format!("f0000000000000000000{:04x}", 0xffff - i), "type": "spawn", "room": room, "x": x, "y": y,
            "name": "Spawn1", "user": uid, "store": { "energy": SPAWN_ENERGY_START }, "storeCapacityResource": { "energy": SPAWN_ENERGY_CAPACITY },
            "hits": SPAWN_HITS, "hitsMax": SPAWN_HITS, "spawning": null, "notifyWhenAttacked": true }));
        }
        let mut players = seats.iter().map(|s| {
            let mut u = user(&s.uid, &s.name);
            let homes =
                (objects.iter()).filter(|o| o["type"] == "controller" && o["user"] == s.uid);
            u["rooms"] = homes.map(|o| o["room"].clone()).collect();
            u["gcl"] = json!(self.gcl);
            u
        });
        let us = crate::badge::ours(players.next().expect("a seat"), badge);
        let npcs = [us, user(INVADER, "Invader"), user(KEEPER, "Source Keeper")];
        let others = self.users.iter().map(|(id, name)| user(id, name));
        let users: Vec<_> = npcs.into_iter().chain(players).chain(others).collect();
        let db = json!({
            "rooms": self.rooms.iter().map(|(n, _)| crate::generated::room_doc(n, self.closed.contains(n), self.banks.map(|s| (s, t)))).collect::<Vec<_>>(),
            "rooms.terrain": self.rooms.iter().map(|(n, t)| json!({ "room": n, "terrain": t })).collect::<Vec<_>>(),
            "users": users,
            "rooms.objects": objects,
        });
        krebs(db, seed, t)
    }
}

pub fn user(id: &str, name: &str) -> Value {
    crate::badge::put(
        json!({ "_id": id, "username": name, "usernameLower": name.to_lowercase(),
    "cpu": 100, "gcl": 0, "cpuAvailable": 10000, "active": 10000, "rooms": [], "money": 0, "power": 0 }),
    )
}

pub fn krebs(db: Value, seed: u64, t: i64) -> World {
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
        w.rooms[r] = rebuilt(&w, r, w.rooms[r].objects.to_vec());
        w.active.insert(r as u32);
    }
    w
}

pub fn rebuilt(k: &World, r: usize, objects: Vec<krebs::doc::DocRef>) -> Arc<RoomSnapshot> {
    let s = &k.rooms[r];
    Arc::new(RoomSnapshot::from_docs(
        s.info.clone(),
        objects,
        s.flags.clone(),
        s.event_log.clone(),
        s.map_view.clone(),
        s.processed_at,
        &k.user_ids,
    ))
}

fn merge(o: &mut Value, with: Value) {
    if let (Value::Object(o), Value::Object(with)) = (o, with) {
        o.extend(with);
    }
}
