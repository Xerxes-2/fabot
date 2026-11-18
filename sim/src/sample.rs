//! `soak-step.mjs`'s ledger and samples off krebs's documents, field for
//! field where `measures.mjs` and `soak-many.mjs` read them.
use std::collections::{BTreeMap, BTreeSet};

use krebs::atom;
use krebs::constants::{CREEP_CORPSE_RATE, CREEP_PART_MAX_ENERGY};
use krebs::doc::{Atom, Obj, Val};
use krebs::intents::RoomIntents;
use screeps::{
    CONSTRUCTION_COST_ROAD_SWAMP_RATIO, CONSTRUCTION_COST_ROAD_WALL_RATIO, CREEP_CLAIM_LIFE_TIME,
    CREEP_LIFE_TIME, ENERGY_DECAY, LINK_LOSS_RATIO, Part, REPAIR_COST, SPAWN_ENERGY_CAPACITY,
    StructureType, TOWER_ENERGY_COST, controller_levels,
};
use serde_json::{Map, Value, json};

use crate::read::build_cost;
use crate::view::{
    EVENT_ATTACK, EVENT_BUILD, EVENT_HARVEST, EVENT_HEAL, EVENT_REPAIR, EVENT_TRANSFER,
    EVENT_UPGRADE_CONTROLLER, Room, View, num, obj, ours, store, text,
};
use crate::world::Map as World;

const PAYING: [&str; 6] = [
    "spawn",
    "extension",
    "container",
    "storage",
    "link",
    "tower",
];

/// The level that first allows the kind: a structure's worth is scaled by it.
fn unlock(kind: StructureType) -> f64 {
    f64::from(
        (1..=8)
            .find(|l| kind.controller_structures(*l) > 0)
            .unwrap_or(1),
    )
}

/// `soak-step.mjs` `reserveOf`: every tower firing through a creep's life, twice over.
fn reserve(level: u32) -> f64 {
    let towers = StructureType::Tower.controller_structures(level);
    if level >= 4 {
        f64::from(towers) * f64::from(TOWER_ENERGY_COST) * f64::from(CREEP_LIFE_TIME) * 2.0
    } else {
        0.0
    }
}

/// A creep's body, or a tombstone's.
fn parts(o: &Obj) -> Vec<&str> {
    match o
        .get(Atom::intern("body"))
        .or(o.get(Atom::intern("creepBody")))
    {
        Some(Val::Arr(parts)) => (parts.iter())
            .filter_map(|p| p.as_str().or(p.as_obj().map(|p| text(p, atom!("type")))))
            .collect(),
        _ => Vec::new(),
    }
}

fn life(body: &[&str]) -> f64 {
    let claim = body.contains(&"claim");
    f64::from(if claim {
        CREEP_CLAIM_LIFE_TIME
    } else {
        CREEP_LIFE_TIME
    })
}

fn near(a: &Obj, b: &Obj, r: f64) -> bool {
    (num(a, atom!("x")) - num(b, atom!("x")))
        .abs()
        .max((num(a, atom!("y")) - num(b, atom!("y"))).abs())
        <= r
}

#[derive(Default, Clone)]
struct Line {
    dug: f64,
    decayed: f64,
    build: f64,
    upgrade: f64,
    repair: f64,
    bodies: f64,
    reserved: f64,
    hostile: f64,
    least_downgrade: Option<f64>,
}

#[derive(Default)]
struct Ledger {
    dug: f64,
    regen: f64,
    /// What our bodies leave beyond what they carried: the engine's corpse rate.
    corpses: f64,
    spawn: f64,
    build: f64,
    upgrade: f64,
    repair: f64,
    tower: f64,
    decayed: f64,
    linked: f64,
    stock0: Option<f64>,
    stock: f64,
}

#[derive(Default)]
pub struct Sampler {
    rooms: Vec<String>,
    lines: BTreeMap<String, Line>,
    ledger: Ledger,
    regen_next: f64,
    born: BTreeSet<String>,
    /// Each pile's room and energy at the last tick's end.
    piles: BTreeMap<String, (String, f64)>,
    /// The piles our bodies pick up and the bodies our spawns recycle this tick.
    taken: BTreeSet<String>,
    graves: BTreeSet<String>,
    placed: BTreeMap<String, i64>,
}

/// Piles, tombstones (an Invader's corpse is ours to take), and stores nobody's or ours.
fn stock(room: &Room) -> f64 {
    (room.objects.iter())
        .map(|(kind, o)| match *kind {
            "energy" => num(o, atom!("energy")),
            "tombstone" => store(o, atom!("energy")),
            _ if ours(o) || text(o, atom!("user")).is_empty() => store(o, atom!("energy")),
            _ => 0.0,
        })
        .sum()
}

impl Sampler {
    pub fn new(home: &str) -> Sampler {
        let mut s = Sampler::default();
        s.follow(home);
        s
    }

    fn follow(&mut self, room: &str) {
        self.rooms.push(room.to_string());
        self.lines.insert(room.to_string(), Line::default());
    }

    /// Rooms we own, reserve or work in, or carry energy through, join from the tick they first do,
    /// so no energy of ours is ever outside the ledger.
    fn follow_worked(&mut self, v: &View) -> Vec<String> {
        let mut joined = Vec::new();
        for room in &v.rooms {
            let mine = room.of("creep").filter(|o| ours(o));
            let carried: f64 = mine.map(|o| store(o, atom!("energy"))).sum();
            if self.lines.contains_key(room.name) || room.events.is_null() && carried == 0.0 {
                continue;
            }
            let held = room.of("controller").any(|o| {
                ours(o)
                    || obj(o, atom!("reservation"))
                        .is_some_and(|r| text(r, atom!("user")) == crate::world::US)
            });
            let creep = |id: &str| {
                room.of("creep")
                    .any(|o| text(o, atom!("_id")) == id && ours(o))
            };
            let source = |id: &str| room.of("source").any(|o| text(o, atom!("_id")) == id);
            let worked = room.events().any(|e| {
                let on = e.data["targetId"].as_str().unwrap_or_default();
                let works = matches!(e.kind, EVENT_BUILD | EVENT_REPAIR);
                (works || e.kind == EVENT_HARVEST && source(on)) && creep(e.by)
            });
            if held || worked || carried > 0.0 {
                self.follow(room.name);
                joined.push(room.name.to_string());
                if let Some(stock0) = &mut self.ledger.stock0 {
                    // What our creeps carry in was in the room they came from.
                    *stock0 += stock(room) - carried;
                }
            }
        }
        joined
    }

    /// One tick of the ledger, off the followed rooms as the tick left them.
    pub fn account(&mut self, v: &View) {
        let joined = self.follow_worked(v);
        let time = v.k.time as f64;
        let mut piles = BTreeMap::new();
        self.ledger.regen += std::mem::take(&mut self.regen_next);
        let mut total = 0.0;
        for name in &self.rooms {
            let room = v.room(name);
            total += stock(room);
            let line = self.lines.get_mut(name).expect("a line per followed room");
            // Its stock at joining is counted after this tick's decay.
            let booked = self.ledger.stock0.is_some() && !joined.contains(name);
            let ids = |pred: &dyn Fn(&str, &Obj) -> bool| -> BTreeSet<&str> {
                (room.objects.iter())
                    .filter(|(kind, o)| pred(kind, o))
                    .map(|(_, o)| text(o, atom!("_id")))
                    .collect()
            };
            let sources = ids(&|kind, _| kind == "source");
            let towers = ids(&|kind, o| kind == "tower" && ours(o));
            let links = ids(&|kind, o| kind == "link" && ours(o));
            let strangers = ids(&|_, o| !ours(o) && !text(o, atom!("user")).is_empty());
            for e in room.events().filter(|e| !strangers.contains(e.by)) {
                let target = e.data["targetId"].as_str().unwrap_or_default();
                let amount = e.data["amount"].as_f64().unwrap_or(0.0);
                let spent = e.data["energySpent"].as_f64();
                let (ledger, by_tower) = (&mut self.ledger, towers.contains(e.by));
                match e.kind {
                    EVENT_HARVEST if sources.contains(target) => {
                        ledger.dug += amount;
                        line.dug += amount;
                    }
                    EVENT_BUILD => {
                        ledger.build += spent.unwrap_or(amount);
                        line.build += spent.unwrap_or(amount);
                    }
                    EVENT_UPGRADE_CONTROLLER => {
                        ledger.upgrade += spent.unwrap_or(0.0);
                        line.upgrade += spent.unwrap_or(0.0);
                    }
                    EVENT_REPAIR if by_tower => {
                        ledger.tower += spent.unwrap_or(f64::from(TOWER_ENERGY_COST))
                    }
                    EVENT_REPAIR => {
                        ledger.repair += spent.unwrap_or(0.0);
                        line.repair += spent.unwrap_or(0.0);
                    }
                    EVENT_TRANSFER if links.contains(e.by) && links.contains(target) => {
                        let kept = (amount * f64::from(1.0 - LINK_LOSS_RATIO)).floor();
                        ledger.linked += amount - kept;
                    }
                    EVENT_ATTACK | EVENT_HEAL if by_tower => {
                        ledger.tower += f64::from(TOWER_ENERGY_COST)
                    }
                    _ => {}
                }
            }
            let (mut bank, mut hostile, mut low_spawns) = (0.0, false, 0.0);
            for &(kind, o) in &room.objects {
                match kind {
                    "controller" if ours(o) => {
                        let left = num(o, atom!("downgradeTime")) - time;
                        line.least_downgrade =
                            Some(line.least_downgrade.map_or(left, |l| l.min(left)));
                    }
                    "controller" => {
                        let reservation = obj(o, atom!("reservation"));
                        let held = |r: &Obj| {
                            text(r, atom!("user")) == crate::world::US
                                && num(r, atom!("endTime")) > time
                        };
                        line.reserved += f64::from(u8::from(reservation.is_some_and(held)));
                    }
                    "energy" if text(o, atom!("resourceType")) == "energy" => {
                        let now = num(o, atom!("energy"));
                        piles.insert(text(o, atom!("_id")).to_string(), (name.clone(), now));
                        // The engine decays what a pile holds after this tick's drops, x, to
                        // now = x - ceil(x / 1000); x = 1000k and 1000k + 1 both leave 999k.
                        let lost = (now / f64::from(ENERGY_DECAY - 1)).ceil();
                        self.ledger.decayed += lost * f64::from(u8::from(booked));
                        line.decayed += lost * f64::from(u8::from(booked));
                    }
                    "creep" if !ours(o) => hostile = true,
                    "tombstone" if self.graves.insert(text(o, atom!("_id")).into()) => {
                        let body = parts(o);
                        let recycled = self.taken.contains(text(o, atom!("creepId")));
                        let ttl = num(o, Atom::intern("creepTicksToLive")) / life(&body);
                        let rate = if recycled { 1.0 } else { CREEP_CORPSE_RATE } * ttl;
                        let max = f64::from(CREEP_PART_MAX_ENERGY);
                        let part = |p: Part| (f64::from(p.cost()) * rate).min(max);
                        let parsed = body.iter().filter_map(|p| p.parse().ok());
                        let left = parsed.map(part).sum::<f64>().floor();
                        self.ledger.corpses += left * f64::from(u8::from(booked));
                    }
                    "creep" => {
                        let body = parts(o);
                        let cost = (body.iter().filter_map(|p| p.parse::<Part>().ok()))
                            .map(|p| f64::from(p.cost()))
                            .sum::<f64>();
                        if self.born.insert(text(o, atom!("name")).to_string())
                            && self.ledger.stock0.is_some()
                        {
                            self.ledger.spawn += cost;
                        }
                        line.bodies += cost / life(&body);
                    }
                    "spawn" | "extension" if ours(o) => {
                        bank += store(o, atom!("energy"));
                        low_spawns += f64::from(u8::from(
                            kind == "spawn"
                                && store(o, atom!("energy")) < f64::from(SPAWN_ENERGY_CAPACITY),
                        ));
                    }
                    _ => {}
                }
            }
            // A pile of 1 nobody picked up decays to nothing.
            let gone = (self.piles.iter())
                .filter(|(id, (r, e))| {
                    r == name && *e <= 1.0 && !piles.contains_key(*id) && !self.taken.contains(*id)
                })
                .count() as f64;
            self.ledger.decayed += gone;
            line.decayed += gone;
            line.hostile += f64::from(u8::from(hostile));
            if bank < f64::from(SPAWN_ENERGY_CAPACITY) {
                self.regen_next += low_spawns;
            }
        }
        self.piles = piles;
        self.ledger.stock0.get_or_insert(total);
        self.ledger.stock = total;
    }

    /// The piles our bodies pick up and the bodies our spawns recycle, off this tick's intents.
    pub fn intend(&mut self, intents: &[(usize, RoomIntents)]) {
        let objects = intents
            .iter()
            .flat_map(|(_, i)| &i.users)
            .flat_map(|u| &u.objects);
        let target =
            |o: &Obj, act| obj(o, Atom::intern(act)).map(|t| text(t, atom!("id")).to_string());
        let acts = objects.flat_map(|(_, o)| ["pickup", "recycleCreep"].map(|act| target(o, act)));
        self.taken = acts.flatten().collect();
    }

    /// The home's level, 0 once it is not ours.
    pub fn level(&self, v: &View) -> f64 {
        let home = v.room(&self.rooms[0]);
        home.of("controller")
            .find(|o| ours(o))
            .map_or(0.0, |o| num(o, atom!("level")))
    }

    pub fn ledger(&self) -> Value {
        let l = &self.ledger;
        let spent = l.spawn + l.build + l.upgrade + l.repair + l.tower;
        let moved = l.stock - l.stock0.unwrap_or(0.0);
        let residual = l.dug + l.regen + l.corpses - spent - l.decayed - l.linked - moved;
        json!({ "dug": l.dug, "regen": l.regen, "corpses": l.corpses, "spawn": l.spawn, "build": l.build, "upgrade": l.upgrade,
            "repair": l.repair, "tower": l.tower, "decayed": l.decayed, "linked": l.linked, "stock": l.stock,
            "residual": residual })
    }

    /// One sample of the home, and the colony's worth, score and room lines.
    pub fn sample(&mut self, v: &View, t: u64, map: &World) -> Value {
        let time = v.k.time as i64;
        let home = v.room(&self.rooms[0]);
        let (mut creeps, mut body_parts, mut spawns, mut hostiles) = (0, 0, 0, 0);
        let (mut sources, mut stored, mut dropped, mut cash, mut capacity) =
            (0.0, 0.0, 0.0, 0.0, 0.0);
        let (mut sites, mut oldest, mut level, mut progress, mut safe) = (0, 0, 0.0, 0.0, false);
        let mut built: BTreeMap<String, i64> = BTreeMap::new();
        let mut queued: BTreeMap<String, i64> = BTreeMap::new();
        let mut mineral = Value::Null;
        for &(kind, o) in &home.objects {
            let mine = ours(o);
            let st = text(o, atom!("structureType"));
            match kind {
                "constructionSite" if mine => {
                    sites += 1;
                    *queued.entry(st.to_string()).or_default() += 1;
                    if PAYING.contains(&st) {
                        let at = *self
                            .placed
                            .entry(text(o, atom!("_id")).to_string())
                            .or_insert(t as i64);
                        oldest = oldest.max(t as i64 - at);
                    }
                }
                "controller" => {
                    progress = num(o, atom!("progress"));
                    if mine {
                        level = num(o, atom!("level"));
                        safe = num(o, atom!("safeMode")) > time as f64;
                    }
                }
                "source" => sources += num(o, atom!("energy")),
                "mineral" => mineral = json!(num(o, atom!("mineralAmount"))),
                "energy" if text(o, atom!("resourceType")) == "energy" => {
                    dropped += num(o, atom!("energy"))
                }
                "creep" if mine => {
                    creeps += 1;
                    body_parts += parts(o).len();
                }
                "creep" => hostiles += 1,
                _ => {}
            }
            let structure = kind
                .parse::<StructureType>()
                .is_ok_and(|k| k != StructureType::Controller);
            if structure && (mine || matches!(kind, "road" | "container")) {
                *built.entry(kind.to_string()).or_default() += 1;
            }
            if matches!(kind, "spawn" | "extension") && mine {
                spawns += i32::from(kind == "spawn");
                cash += store(o, atom!("energy"));
                capacity +=
                    obj(o, atom!("storeCapacityResource")).map_or(0.0, |c| num(c, atom!("energy")));
            }
            if matches!(kind, "container" | "storage") {
                stored += store(o, atom!("energy"));
            }
        }
        let rocks: Vec<&Obj> = home.of("source").collect();
        let source_containers: Vec<&Obj> = (home.of("container"))
            .filter(|c| rocks.iter().any(|s| near(s, c, 1.0)))
            .collect();
        let roads: Vec<&Obj> = home.of("road").collect();
        let solid: Vec<&Obj> = (home.objects.iter())
            .filter(|(kind, o)| {
                let kind = kind.parse::<StructureType>().ok();
                let open = [
                    StructureType::Road,
                    StructureType::Container,
                    StructureType::Rampart,
                    StructureType::Controller,
                ];
                ours(o) && kind.is_some_and(|k| !open.contains(&k))
            })
            .map(|(_, o)| *o)
            .collect();
        let same = |a: &Obj, b: &Obj| near(a, b, 0.0);
        let buried = roads
            .iter()
            .filter(|r| solid.iter().any(|s| same(r, s)))
            .count();
        let unroaded = (solid.iter())
            .filter(|&&o| text(o, atom!("type")) == "extension")
            .filter(|&&o| !source_containers.iter().any(|c| near(c, o, 1.0)))
            .filter(|&&o| {
                !roads
                    .iter()
                    .any(|r| near(r, o, 1.0) && !solid.iter().any(|s| same(s, r)))
            })
            .count();
        let posted = (home.of("link"))
            .filter(|l| source_containers.iter().any(|c| near(c, l, 1.0)))
            .count();
        let (worth, score) = self.worth(v, map);
        json!({ "t": t, "time": time, "rcl": level, "safeMode": safe, "progress": progress, "creeps": creeps,
            "parts": body_parts, "spawns": spawns, "hostiles": hostiles, "sources": sources, "energy": cash,
            "capacity": capacity, "stored": stored, "dropped": dropped, "sites": sites, "queued": queued,
            "oldestPaying": oldest, "built": built, "sourceContainers": source_containers.len(), "buried": buried,
            "unroaded": unroaded, "postedLinks": posted, "mineralAmount": mineral, "ledger": self.ledger(),
            "worth": worth, "score": score, "rooms": self.lines(v), "others": {}, "bodies": [], "foes": [] })
    }

    fn worth(&self, v: &View, map: &World) -> (Value, Value) {
        let (mut controller, mut structures, mut scored) = (0.0, 0.0, 0.0);
        let (mut fortification, mut sites, mut level) = (0.0, 0.0, 0_u32);
        for name in &self.rooms {
            for &(kind, o) in &v.room(name).objects {
                let mine = ours(o);
                let structure = kind.parse::<StructureType>().ok();
                match (kind, structure) {
                    ("constructionSite", _) if mine => sites += num(o, atom!("progress")),
                    (_, Some(StructureType::Controller)) if mine => {
                        let at = num(o, atom!("level")) as u32;
                        level = level.max(at);
                        controller += (1..at)
                            .filter_map(controller_levels)
                            .map(f64::from)
                            .sum::<f64>();
                        controller += num(o, atom!("progress"));
                    }
                    (_, Some(StructureType::Rampart | StructureType::Wall)) => {
                        if mine || kind == "constructedWall" {
                            fortification +=
                                (num(o, atom!("hits")) * f64::from(REPAIR_COST)).floor();
                        }
                    }
                    (_, Some(k))
                        if (mine || matches!(kind, "road" | "container"))
                            && k.construction_cost().is_some() =>
                    {
                        let cost = if k == StructureType::Road {
                            let at = (num(o, atom!("y")) * 50.0 + num(o, atom!("x"))) as usize;
                            let ratio = match map.terrain(name)[at] {
                                b'1' | b'3' => CONSTRUCTION_COST_ROAD_WALL_RATIO,
                                b'2' => CONSTRUCTION_COST_ROAD_SWAMP_RATIO,
                                _ => 1,
                            };
                            f64::from(build_cost(kind)) * f64::from(ratio)
                        } else {
                            f64::from(build_cost(kind))
                        };
                        structures += cost;
                        scored += cost * (1.0 + unlock(k).ln());
                    }
                    _ => {}
                }
            }
        }
        let stock = self.ledger.stock;
        let reserve = reserve(level);
        let credit = if reserve == 0.0 {
            stock
        } else {
            reserve * (stock / reserve).tanh()
        };
        let worth = json!({ "controller": controller, "structures": structures, "fortification": fortification,
            "sites": sites, "stock": stock });
        let score = json!({ "controller": controller, "structures": scored, "fortification": fortification,
            "stock": credit });
        (worth, score)
    }

    fn lines(&self, v: &View) -> Value {
        let mut out = Map::new();
        for name in &self.rooms {
            let line = &self.lines[name];
            let (mut creeps, mut hostiles, mut roads, mut containers, mut cores) = (0, 0, 0, 0, 0);
            let (mut held, mut ores) = (0.0, 0.0);
            for &(kind, o) in &v.room(name).objects {
                match kind {
                    "creep" if ours(o) => creeps += 1,
                    "creep" => hostiles += 1,
                    "road" => roads += 1,
                    "container" => containers += 1,
                    "invaderCore" => cores += 1,
                    "storage" | "terminal" | "lab" | "factory" | "powerSpawn" if ours(o) => {
                        for (r, amount) in obj(o, atom!("store")).into_iter().flat_map(Obj::iter) {
                            let amount = amount.as_f64().unwrap_or(0.0);
                            held += amount;
                            if r.as_str() != "energy" {
                                ores += amount;
                            }
                        }
                    }
                    _ => {}
                }
            }
            let bodies = line.bodies.round();
            out.insert(name.clone(), json!({ "dug": line.dug, "decayed": line.decayed, "build": line.build,
                "upgrade": line.upgrade, "repair": line.repair, "bodies": bodies, "reserved": line.reserved,
                "hostile": line.hostile, "waves": 0, "carriedOut": 0, "leastDowngrade": line.least_downgrade,
                "net": line.dug - line.decayed - line.repair - bodies, "creeps": creeps, "hostiles": hostiles,
                "roads": roads, "containers": containers, "cores": cores, "held": held, "ores": ores }));
        }
        Value::Object(out)
    }
}
