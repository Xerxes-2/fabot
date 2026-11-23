//! `--capture a-b[,c-d]`: those ticks (b excluded) to `capture.jsonl` for
//! `scripts/bench.mjs`, in `scripts/capture-read.mjs`'s shape: the World
//! `decide` reads, as `read.js` reads it off `Game`, so the bundle replays what
//! sim decided on. A tick writes only the objects that changed since the last.
use std::collections::BTreeMap;
use std::fs::File;
use std::io::{BufWriter, Write};

use fabot_base::declarations::Declarations;
use fabot_base::intel::{self, Intel};
use fabot_base::world::{Built, Creep, Id, Owner, Pos, Room, Store, World};
use screeps::StructureType as Kind;
use serde_json::{Map, Value, json};

use crate::view::{EVENT_ATTACK, EVENT_OBJECT_DESTROYED, View};
use crate::world;

const SOURCES: &str = "105";
const MINERALS: &str = "116";
const STRUCTURES: &str = "107";
const SITES: &str = "111";
const PILES: &str = "106";
const TOMBSTONES: &str = "118";
const RUINS: &str = "123";
const HOSTILES: &str = "103";
/// The collapse effect an Invader core runs once deployed.
const COLLAPSE_TIMER: i32 = 1002;

pub struct Capture {
    windows: Vec<(u64, u64)>,
    out: BufWriter<File>,
    last: Option<BTreeMap<String, String>>,
    /// Every room's terrain, written once: the bot reads rooms it never sees.
    terrain: Value,
}

impl Capture {
    pub fn open(windows: &str, out: &str, map: &world::Map) -> Capture {
        let window = |w: &str| {
            let (a, b) = w.split_once('-').expect("--capture: a-b[,c-d]");
            (
                a.parse().expect("--capture: a"),
                b.parse().expect("--capture: b"),
            )
        };
        let terrain: Map<String, Value> = (map.rooms.iter())
            .map(|(n, t)| (n.clone(), json!(t)))
            .collect();
        Capture {
            windows: windows
                .split(',')
                .filter(|w| !w.is_empty())
                .map(window)
                .collect(),
            out: BufWriter::new(
                File::create(format!("{out}/capture.jsonl")).expect("capture.jsonl"),
            ),
            last: None,
            terrain: json!(terrain),
        }
    }

    /// Tick `t` as the bot reads it before it decides, with Memory's text and intel's segment.
    pub fn tick(&mut self, t: u64, v: &View, memory: &str, intel: &Intel) {
        if !self.windows.iter().any(|(a, b)| (*a..*b).contains(&t)) {
            return;
        }
        let world = crate::read::world(v, &Declarations::default());
        let mut game = Game {
            objects: Map::new(),
            me: &world.me,
        };
        // `read.js` reads the logs of a room we hold; every strike stands in the first one.
        let held = |r: &&Room| r.controller.as_ref().is_some_and(|c| c.owner.is_ours());
        let logged = world.rooms.iter().find(held).map(|r| r.name);
        let rooms: Vec<Value> = world
            .rooms
            .iter()
            .map(|r| game.room(&world, r, logged == Some(r.name)))
            .collect();
        let spawns: Map<String, Value> = (world.spawns.iter())
            .map(|s| (s.name.clone(), json!(s.id.text().to_string())))
            .collect();
        for s in &world.spawns {
            game.objects[&*s.id.text()]["name"] = json!(s.name);
        }
        let creeps = game.ours(&world);
        let written: BTreeMap<String, String> = (game.objects.iter())
            .map(|(k, o)| (k.clone(), o.to_string()))
            .collect();
        let last = self.last.take();
        let changed: Map<String, Value> = (game.objects.into_iter())
            .filter(|(k, _)| last.as_ref().is_none_or(|l| l.get(k) != written.get(k)))
            .collect();
        let gone: Vec<&String> = last
            .iter()
            .flat_map(|l| l.keys())
            .filter(|k| !written.contains_key(*k))
            .collect();
        let mut line = json!({ "time": world.time, "cpu": { "limit": world.cpu_limit, "bucket": world.bucket },
            "gcl": { "level": world.rooms_allowed }, "memory": memory, "intel": intel::to_text(intel), "rooms": rooms,
            "creeps": creeps, "spawns": spawns, "objects": changed, "gone": gone,
            "constructionSites": (0..world.our_sites).map(|i| format!("site:{i}")).collect::<Vec<_>>() });
        if last.is_none() {
            line["full"] = json!(true);
            line["terrain"] = self.terrain.take();
        }
        writeln!(self.out, "{line}").expect("capture.jsonl");
        self.last = Some(written);
    }
}

struct Game<'a> {
    objects: Map<String, Value>,
    me: &'a str,
}

fn store(s: &Store) -> Value {
    let held: Map<String, Value> = (s.held.iter())
        .map(|(r, n)| (json!(r).as_str().unwrap_or("").to_string(), json!(n)))
        .collect();
    json!({ "held": held, "capacity": s.capacity })
}

fn pos(p: Pos) -> Value {
    json!({ "x": p.x, "y": p.y, "roomName": p.room.to_string() })
}

impl Game<'_> {
    fn username(&self, owner: &Owner) -> Option<String> {
        match owner {
            Owner::Ours => Some(self.me.to_string()),
            Owner::Ally(n) | Owner::Enemy(n) | Owner::Stranger(n) => Some(n.clone()),
            Owner::Invader => Some(screeps::INVADER_USERNAME.to_string()),
            Owner::Keeper => Some(screeps::SOURCE_KEEPER_USERNAME.to_string()),
            Owner::Nobody => None,
        }
    }

    /// An object by its id, `fields` beside its position and owner; the key back.
    fn put(&mut self, id: Id, at: Pos, owner: Option<&Owner>, mut fields: Value) -> String {
        fields["pos"] = pos(at);
        if let Some(owner) = owner {
            fields["my"] = json!(owner.is_ours());
            if let Some(name) = self.username(owner) {
                fields["owner"] = json!({ "username": name });
            }
        }
        let key = id.text().to_string();
        self.objects.insert(key.clone(), fields);
        key
    }

    fn creep(&mut self, c: &Creep) -> String {
        let body: Vec<Value> = (c.body.iter())
            .map(|p| json!({ "type": p.kind, "hits": p.hits, "boost": p.boost }))
            .collect();
        let fields = json!({ "name": c.name, "hits": c.hits, "hitsMax": c.hits_max, "body": body,
            "ticksToLive": c.ticks_to_live, "fatigue": c.fatigue, "store": store(&c.store) });
        self.put(c.id, c.pos, Some(&c.owner), fields)
    }

    /// Our creeps by name, one still in an oven too: `read.js` reads its body off it.
    fn ours(&mut self, world: &World) -> Map<String, Value> {
        let mut creeps: Map<String, Value> = (world.creeps.iter())
            .map(|c| (c.name.clone(), json!(self.creep(c))))
            .collect();
        for s in &world.spawns {
            let Some(oven) = &s.spawning else { continue };
            let key = format!("creep:{}", oven.name);
            let body: Vec<Value> = (oven.body.iter())
                .map(|p| json!({ "type": p.kind, "hits": p.hits, "boost": p.boost }))
                .collect();
            let creep = json!({ "name": oven.name, "spawning": true, "my": true, "body": body, "owner": { "username": self.me }, "pos": pos(s.pos) });
            self.objects.insert(key.clone(), creep);
            creeps.insert(oven.name.clone(), json!(key));
            self.objects[&*s.id.text()]["spawning"] =
                json!({ "name": oven.name, "remainingTime": oven.remaining });
        }
        creeps
    }

    fn structure(&mut self, r: &Room, s: &fabot_base::world::Structure) -> String {
        let mut fields = json!({ "structureType": s.kind, "hits": s.hits, "hitsMax": s.hits_max });
        if s.kind.has_store() {
            fields["store"] = store(&s.store);
        }
        if s.kind.has_cooldown() {
            fields["cooldown"] = json!(s.cooldown);
        }
        match s.kind {
            Kind::Controller => {
                let c = r.controller.as_ref().expect("the room's controller");
                fields = json!({ "structureType": s.kind, "level": c.level, "progress": c.progress,
                    "progressTotal": c.progress_total, "ticksToDowngrade": c.ticks_to_downgrade, "safeMode": c.safe_mode,
                    "safeModeAvailable": c.safe_mode_available, "safeModeCooldown": c.safe_mode_cooldown,
                    "upgradeBlocked": c.upgrade_blocked });
                if let Some((owner, left)) = &c.reservation {
                    fields["reservation"] =
                        json!({ "username": self.username(owner), "ticksToEnd": left });
                }
                if let Some((owner, text)) = &c.sign {
                    fields["sign"] = json!({ "username": self.username(owner), "text": text });
                }
            }
            Kind::KeeperLair => fields["ticksToSpawn"] = json!(s.spawn),
            Kind::InvaderCore if s.deploy == 0 => {
                fields["effects"] =
                    json!([{ "effect": COLLAPSE_TIMER, "ticksRemaining": s.collapse }]);
            }
            Kind::InvaderCore => fields["ticksToDeploy"] = json!(s.deploy),
            Kind::Rampart => fields["isPublic"] = json!(s.walkable && !s.owner.is_ours()),
            _ => {}
        }
        if s.kind == Kind::InvaderCore {
            fields["level"] = json!(s.level);
        }
        self.put(s.id, s.pos, Some(&s.owner), fields)
    }

    fn room(&mut self, world: &World, r: &Room, logged: bool) -> Value {
        let mut find = Map::new();
        let sources = (r.sources.iter()).map(|s| {
            let fields = json!({ "energy": s.energy, "energyCapacity": s.energy_capacity, "ticksToRegeneration": s.ticks_to_regeneration });
            self.put(s.id, s.pos, None, fields)
        });
        find.insert(SOURCES.into(), json!(sources.collect::<Vec<_>>()));
        let minerals = (r.minerals.iter()).map(|m| {
            self.put(
                m.id,
                m.pos,
                None,
                json!({ "mineralType": m.resource, "mineralAmount": m.amount }),
            )
        });
        find.insert(MINERALS.into(), json!(minerals.collect::<Vec<_>>()));
        let mut structures: Vec<String> =
            r.structures.iter().map(|s| self.structure(r, s)).collect();
        for (i, p) in r.blocked.iter().enumerate() {
            let key = format!("blocked:{}:{i}", p.room);
            self.objects
                .insert(key.clone(), json!({ "structureType": "", "pos": pos(*p) }));
            structures.push(key);
        }
        find.insert(STRUCTURES.into(), json!(structures));
        let sites = (r.sites.iter()).map(|s| {
            let fields = json!({ "structureType": s.kind, "progress": s.progress, "progressTotal": s.progress_total });
            self.put(s.id, s.pos, Some(&s.owner), fields)
        });
        find.insert(SITES.into(), json!(sites.collect::<Vec<_>>()));
        let piles = (r.piles.iter()).map(|p| {
            self.put(
                p.id,
                p.pos,
                None,
                json!({ "resourceType": p.resource, "amount": p.amount }),
            )
        });
        find.insert(PILES.into(), json!(piles.collect::<Vec<_>>()));
        let (mut tombstones, mut ruins) = (Vec::new(), Vec::new());
        for x in &r.remains {
            let creep = match (&x.fallen, x.theirs) {
                (Some(f), _) => {
                    json!({ "my": true, "body": f.body.iter().map(|p| json!({ "type": p })).collect::<Vec<_>>(), "ticksToLive": f.ticks_to_live })
                }
                (None, Some(id)) => json!({ "my": false, "id": id.text().to_string() }),
                (None, None) => Value::Null,
            };
            let died = x.fallen.as_ref().map(|f| f.died);
            let tombstone = !creep.is_null();
            let key = self.put(
                x.id,
                x.pos,
                None,
                json!({ "store": store(&x.store), "creep": creep, "deathTime": died }),
            );
            if tombstone {
                tombstones.push(key)
            } else {
                ruins.push(key)
            }
        }
        find.insert(TOMBSTONES.into(), json!(tombstones));
        find.insert(RUINS.into(), json!(ruins));
        let hostiles: Vec<String> = r.others.iter().map(|c| self.creep(c)).collect();
        find.insert(HOSTILES.into(), json!(hostiles));
        let mut room = json!({ "name": r.name.to_string(), "energyAvailable": r.energy_available,
            "energyCapacityAvailable": r.energy_capacity, "find": find });
        if let Some(c) = &r.controller {
            room["controller"] = json!(c.id.text().to_string());
        }
        if logged {
            let attacks = (world.attacks.iter()).map(|(by, at)| json!({ "event": EVENT_ATTACK, "objectId": by.text().to_string(), "data": { "targetId": at.text().to_string() } }));
            let destroyed = (world.destroyed.iter()).map(|(_, id, _)| json!({ "event": EVENT_OBJECT_DESTROYED, "objectId": id.text().to_string(), "data": { "type": "" } }));
            room["events"] = json!(attacks.chain(destroyed).collect::<Vec<_>>());
        }
        room
    }
}
