//! Each body's life over the followed rooms and their neighbours (`bodies.jsonl`), the
//! longest a body of ours stood loaded and idle or kept a mineral (a carrier, or any body
//! taking energy), and its border bounces.
use std::collections::{BTreeMap, BTreeSet};

use fabot_base::{engine, vocabulary};
use krebs::atom;
use krebs::doc::{Obj, Val};
use serde_json::{Value, json};

use crate::sample::{boosts, made, parts};
use crate::view::{EVENT_ATTACK, EVENT_OBJECT_DESTROYED, Room, View, num, obj, store, text, used};

#[derive(Default)]
pub struct Lives {
    open: BTreeMap<String, Value>,
    /// A body's key by id: the strike that kills it lands the tick it is gone from the room.
    keys: BTreeMap<String, String>,
    pub closed: Vec<Value>,
    /// Bodies the engine destroyed the tick a strike landed, and what struck each last
    /// as `[kind, user]`.
    pub deaths: Vec<Value>,
    by: BTreeMap<String, Value>,
    pub still_loaded: u64,
    stills: BTreeMap<String, (String, f64, u64)>,
    pub mineral_aside: u64,
    pub mineral_held: u64,
    pub bounced: u64,
    last_two: BTreeMap<String, [Option<Step>; 2]>,
}

/// Where a body of ours stood a tick: its room, whether on an edge, the energy and mineral it
/// carried, the ticks that mineral has not fallen and whether energy came aboard in them.
struct Step {
    room: String,
    edge: bool,
    energy: f64,
    ore: f64,
    aside: u64,
    took: bool,
}

pub fn letters(body: &[&str]) -> String {
    let letter = |p: &str| {
        p.parse()
            .map_or('t', |p| vocabulary::part_letter(p).to_ascii_lowercase())
    };
    body.iter().map(|p| letter(p)).collect()
}

fn array<'a>(v: &'a mut Value, key: &str) -> &'a mut Vec<Value> {
    if v[key].is_null() {
        v[key] = json!([]);
    }
    v[key].as_array_mut().unwrap()
}

fn edge(o: &Obj) -> bool {
    let (x, y) = (num(o, atom!("x")), num(o, atom!("y")));
    x == 0.0 || y == 0.0 || x == 49.0 || y == 49.0
}

/// What the body did this tick: the engine's action log holds this tick's acts, the rest null.
pub fn acts(o: &Obj) -> Vec<&str> {
    let log = obj(o, atom!("actionLog"));
    log.into_iter()
        .flat_map(Obj::iter)
        .filter(|(_, v)| !matches!(v, Val::Null))
        .map(|(k, _)| k.as_str())
        .filter(|k| *k != "say")
        .collect()
}

impl Lives {
    /// One tick as the bot reads it: `around` the followed rooms and their neighbours, `followed`
    /// the first of them; what bodies of ours carried out of each home, by home.
    pub fn observe(
        &mut self,
        around: &[&Room],
        followed: usize,
        homes: &[String],
        v: &View,
    ) -> BTreeMap<String, f64> {
        let time = v.k.time;
        let mut carried_out = BTreeMap::new();
        let mut destroyed = BTreeSet::new();
        for (i, room) in around.iter().enumerate() {
            let creeps = room.events().filter(|e| e.kind == EVENT_OBJECT_DESTROYED);
            destroyed.extend(creeps.filter(|e| e.data["type"] == "creep").map(|e| e.by));
            let mut struck: BTreeMap<&str, (f64, Value)> = BTreeMap::new();
            for e in room.events().filter(|e| e.kind == EVENT_ATTACK) {
                let target = e.data["targetId"].as_str().unwrap_or_default();
                let by = (room.objects.iter()).find(|(_, o)| text(o, atom!("_id")) == e.by);
                let at = struck.entry(target).or_default();
                at.0 += e.data["damage"].as_f64().unwrap_or(0.0);
                at.1 = json!(by.map(|(k, o)| [*k, text(o, atom!("user"))]));
            }
            for o in room.of("creep") {
                let name = text(o, atom!("name"));
                let key = format!("{} {name}", text(o, atom!("user")));
                let body = parts(o);
                let typed: Vec<_> = body.iter().filter_map(|p| p.parse().ok()).collect();
                let id = text(o, atom!("_id"));
                if !self.keys.contains_key(id) {
                    self.keys.insert(id.to_string(), key.clone());
                }
                let life = self.open.entry(key).or_insert_with(|| {
                    json!({ "name": name, "ours": v.ours(o), "body": letters(&body),
                        "cost": engine::body_cost(&typed), "first": time, "rooms": {}, "acts": {} })
                });
                let at = format!(
                    "{} {},{}",
                    room.name,
                    num(o, atom!("x")),
                    num(o, atom!("y"))
                );
                if life["at"] == at.as_str() && life["last"] == time - 1 {
                    life["still"] = json!(life["still"].as_u64().unwrap_or(0) + 1);
                }
                if life.get("at").is_some()
                    && life["at"] != at.as_str()
                    && life.get("left").is_none()
                {
                    life["left"] = json!(time);
                }
                if !life["at"]
                    .as_str()
                    .and_then(|a| a.strip_prefix(room.name))
                    .is_some_and(|a| a.starts_with(' '))
                {
                    if let Some([home]) = life["entered"].as_array().map(Vec::as_slice) {
                        let at = (around.iter()).find(|r| home[0] == r.name);
                        life["heldAtLeave"] = json!(at.map(|h| made(h, v)));
                    }
                    array(life, "entered").push(json!([room.name, time]));
                }
                life["at"] = json!(at);
                life["last"] = json!(time);
                life["ttl"] = json!(num(o, atom!("ageTime")) - time as f64);
                life["hits"] = json!(num(o, atom!("hits")));
                life["boosts"] = json!(boosts(o).into_iter().collect::<BTreeSet<_>>());
                let ticks = &mut life["rooms"][room.name];
                *ticks = json!(ticks.as_u64().unwrap_or(0) + 1);
                for act in acts(o) {
                    let n = &mut life["acts"][act];
                    *n = json!(n.as_u64().unwrap_or(0) + 1);
                    if matches!(act, "harvest" | "reserveController") {
                        let list = array(&mut life["tiles"], act);
                        if !list.iter().any(|t| t == at.as_str()) {
                            list.push(json!(at));
                        }
                    }
                }
                if !v.ours(o) {
                    continue;
                }
                let energy = store(o, atom!("energy"));
                if i < followed {
                    let was = self.stills.get(name);
                    let idle = energy > 0.0
                        && was.is_some_and(|w| w.0 == at && w.1 == energy)
                        && acts(o).is_empty();
                    let ticks = if idle { was.map_or(0, |w| w.2) + 1 } else { 0 };
                    self.stills
                        .insert(name.to_string(), (at.clone(), energy, ticks));
                    self.still_loaded = self.still_loaded.max(ticks);
                }
                let [a, b] = self.last_two.remove(name).unwrap_or_default();
                let ore = used(o) - energy;
                let kept = b.as_ref().filter(|b| ore > 0.0 && ore >= b.ore);
                let now = Step {
                    room: room.name.to_string(),
                    edge: edge(o),
                    energy,
                    ore,
                    aside: kept.map_or(0, |b| b.aside + 1),
                    took: kept.is_some_and(|b| b.took || energy > b.energy),
                };
                if !body.contains(&"work") {
                    self.mineral_held = self.mineral_held.max(now.aside);
                }
                if now.took {
                    self.mineral_aside = self.mineral_aside.max(now.aside);
                }
                if let (Some(a), Some(b)) = (&a, &b)
                    && a.edge
                    && b.edge
                    && now.edge
                    && a.room == now.room
                    && b.room != now.room
                {
                    self.bounced += 1;
                }
                if let Some(b) = &b
                    && b.room != now.room
                    && homes.contains(&b.room)
                {
                    *carried_out.entry(b.room.clone()).or_default() += now.energy;
                }
                self.last_two.insert(name.to_string(), [b, Some(now)]);
            }
            for (id, (d, by)) in struck {
                let Some(k) = self.keys.get(id) else { continue };
                if let Some(life) = self.open.get_mut(k) {
                    life["struck"] = json!(life["struck"].as_f64().unwrap_or(0.0) + d);
                    life["struckAt"] = json!(time);
                    self.by.insert(k.clone(), by);
                }
            }
        }
        let gone: Vec<String> = (self.open.iter())
            .filter(|(_, l)| l["last"] != time)
            .map(|(k, _)| k.clone())
            .collect();
        let dead: BTreeSet<&String> = destroyed
            .iter()
            .filter_map(|id| self.keys.get(*id))
            .collect();
        for key in gone {
            let life = self.open.remove(&key).expect("an open life");
            let by = self.by.remove(&key);
            if life["struckAt"] == time && dead.contains(&key) {
                self.deaths.push(
                    json!({ "t": time, "name": life["name"], "ours": life["ours"],
                    "cost": life["cost"], "body": life["body"], "user": key.split(' ').next(),
                    "by": by }),
                );
            }
            self.closed.push(life);
        }
        carried_out
    }

    /// The bodies still seen at the run's end, closed after the others.
    pub fn close(&mut self) -> Vec<Value> {
        let open = std::mem::take(&mut self.open);
        std::mem::take(&mut self.closed)
            .into_iter()
            .chain(open.into_values())
            .collect()
    }
}
