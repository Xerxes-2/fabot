//! `Executor` against krebs: each intent as the official game API
//! (`engine/src/game/*.js`) would store it, after the API's own checks in its
//! order where they are cheap and decide the stored amount, refused with its code.
use std::collections::BTreeMap;

use fabot_base::HashMap;
use fabot_base::intents::{
    Action, ControllerAct, CreepAct, LabAct, LinkAct, Refusal, RoomAct, SpawnAct, TerminalAct,
    TowerAct,
};
use fabot_base::vocabulary::{self, Part, Resource};
use fabot_base::world;
use krebs::atom;
use krebs::doc::{Atom, Id, Obj, Val};
use krebs::intents::{RoomIntents, UserRoomIntents};
use krebs::world::UserIdx;
use krebs_util::json::Json;
use screeps::ErrorCode::{self, *};
use serde_json::{Value, json};

use crate::view::{View, num, obj, on, ours, store, text, used};

type Found<'a> = (usize, &'a Obj);

/// Every object by id, through krebs's object index, and our creeps by name, this tick.
struct Index<'a> {
    v: &'a View<'a>,
    creeps: HashMap<&'a str, Found<'a>>,
}

impl<'a> Index<'a> {
    fn get(&self, id: world::Id) -> Option<Found<'a>> {
        let at = self.v.k.index.objects.get(&Id::new(&id.text())?)?;
        let docs = &self.v.k.rooms[at.room as usize].objects;
        let i = docs.binary_search_by_key(&at.seq, |d| d.seq).ok()?;
        Some((at.room as usize, &docs[i].obj))
    }
}

fn index<'a>(v: &'a View<'a>) -> Index<'a> {
    let us = crate::read::our_idx(v);
    let seen = |r: &usize| (v.k.rooms[*r].presence.iter()).any(|(u, p)| *u == us && p.objects > 0);
    let creeps = (0..v.rooms.len()).filter(seen).flat_map(|r| {
        let creeps = v.rooms[r].of("creep").filter(|o| ours(o));
        creeps.map(move |o| (text(o, atom!("name")), (r, o)))
    });
    Index {
        v,
        creeps: creeps.collect(),
    }
}

/// `utils.capacityForResource`.
fn capacity(o: &Obj, resource: &str) -> f64 {
    let by = obj(o, atom!("storeCapacityResource"));
    let own = by
        .and_then(|c| c.get(Atom::intern(resource)))
        .and_then(Val::as_f64);
    let rest = || {
        let reserved: f64 = by.map_or(0.0, |c| c.iter().filter_map(|(_, v)| v.as_f64()).sum());
        (num(o, atom!("storeCapacity")) - reserved).max(0.0)
    };
    own.filter(|c| *c != 0.0).unwrap_or_else(rest)
}

fn range(a: Found, b: Found) -> f64 {
    if a.0 != b.0 {
        return f64::INFINITY;
    }
    let d = |k| (num(a.1, k) - num(b.1, k)).abs();
    d(atom!("x")).max(d(atom!("y")))
}

fn has_active(o: &Obj, part: &str) -> bool {
    match o.get(Atom::intern("body")) {
        Some(Val::Arr(parts)) => (parts.iter().filter_map(Val::as_obj))
            .any(|p| text(p, atom!("type")) == part && num(p, atom!("hits")) > 0.0),
        _ => false,
    }
}

fn check(fails: bool, code: ErrorCode) -> Result<(), ErrorCode> {
    if fails { Err(code) } else { Ok(()) }
}

/// Each object's intents by name: a later one of a name replaces the earlier.
type Named = serde_json::Map<String, Value>;

/// Intents by room, then by object in the order first given one.
#[derive(Default)]
pub struct Stored {
    rooms: BTreeMap<usize, Vec<(String, Named)>>,
}

impl Stored {
    fn named(&mut self, room: usize, object: &str) -> &mut Named {
        let objects = self.rooms.entry(room).or_default();
        let i = (objects.iter().position(|(o, _)| o == object)).unwrap_or_else(|| {
            objects.push((object.to_string(), Named::new()));
            objects.len() - 1
        });
        &mut objects[i].1
    }

    fn put(&mut self, room: usize, object: &str, name: &str, value: Value) {
        self.named(room, object).insert(name.to_string(), value);
    }

    /// The room's list intents (`createConstructionSite`, `destroyStructure`) under `room`.
    fn push(&mut self, room: usize, name: &str, value: Value) {
        let list =
            (self.named(room, "room").entry(name)).or_insert_with(|| Value::Array(Vec::new()));
        if let Value::Array(list) = list {
            list.push(value);
        }
    }

    /// Each room's intents sanitized as the system isolate does, for the processor.
    pub fn rooms(self, us: UserIdx) -> Vec<(usize, RoomIntents)> {
        let sanitize = |named: Named| {
            let text = Json::parse(&Value::Object(named).to_string()).expect("intents are JSON");
            krebs::system::sanitize_user_intents(&text, &[]).expect("intents sanitize")
        };
        (self.rooms.into_iter())
            .map(|(room, objects)| {
                let objects = (objects.into_iter())
                    .map(|(id, named)| (id.into_boxed_str(), sanitize(named)))
                    .collect();
                let user = UserRoomIntents {
                    user: us,
                    objects,
                    manual: Vec::new(),
                };
                (room, RoomIntents { users: vec![user] })
            })
            .collect()
    }
}

struct Run<'a> {
    v: &'a View<'a>,
    at: Index<'a>,
    stored: Stored,
    /// `createdCreepNames`: a name asked this tick is taken.
    created: Vec<String>,
    sites: u32,
}

impl Run<'_> {
    fn target(&self, id: world::Id) -> Result<Found<'_>, ErrorCode> {
        self.at.get(id).ok_or(NotFound)
    }

    fn creep(&mut self, name: &str, act: &CreepAct) -> Result<(), ErrorCode> {
        let &creep = self.at.creeps.get(name).ok_or(NotFound)?;
        check(
            creep.1.get(Atom::intern("spawning")) == Some(&Val::Bool(true)),
            Busy,
        )?;
        let id = text(creep.1, atom!("_id")).to_string();
        let near = |t: Found| check(range(creep, t) > 1.0, NotInRange);
        let energy = store(creep.1, atom!("energy"));
        let (name, value) = match act {
            CreepAct::Say(_) => return Ok(()),
            CreepAct::Move(direction) => {
                check(num(creep.1, atom!("fatigue")) > 0.0, Tired)?;
                check(!has_active(creep.1, "move"), NoBodypart)?;
                ("move", json!({ "direction": direction }))
            }
            CreepAct::Drop(r) => {
                let r = vocabulary::resource_name(r);
                let amount = store(creep.1, Atom::intern(&r));
                check(amount <= 0.0, NotEnough)?;
                ("drop", json!({ "amount": amount, "resourceType": r }))
            }
            CreepAct::RangedMassAttack => ("rangedMassAttack", json!({})),
            CreepAct::Transfer(t, r) => {
                let t = self.target(*t)?;
                let r = vocabulary::resource_name(r);
                if r == "energy" && text(t.1, atom!("type")) == "controller" {
                    let id = world::Id::parse(text(t.1, atom!("_id"))).ok_or(InvalidTarget)?;
                    return self.creep(name, &CreepAct::Upgrade(id));
                }
                let room = capacity(t.1, &r);
                check(room == 0.0, InvalidTarget)?;
                near(t)?;
                check(store(creep.1, Atom::intern(&r)) <= 0.0, NotEnough)?;
                let stored = if obj(t.1, atom!("storeCapacityResource")).is_some() {
                    store(t.1, Atom::intern(&r))
                } else {
                    used(t.1)
                };
                check(stored >= room, Full)?;
                let amount = store(creep.1, Atom::intern(&r)).min(room - stored);
                (
                    "transfer",
                    json!({ "id": text(t.1, atom!("_id")), "amount": amount, "resourceType": r }),
                )
            }
            CreepAct::Withdraw(t, r) => {
                let t = self.target(*t)?;
                let r = vocabulary::resource_name(r);
                near(t)?;
                let empty = num(creep.1, atom!("storeCapacity")) - used(creep.1);
                check(empty <= 0.0, Full)?;
                let amount = empty.min(store(t.1, Atom::intern(&r)));
                check(amount <= 0.0, NotEnough)?;
                (
                    "withdraw",
                    json!({ "id": text(t.1, atom!("_id")), "amount": amount, "resourceType": r }),
                )
            }
            CreepAct::Harvest(t) => {
                check(!has_active(creep.1, "work"), NoBodypart)?;
                let t = self.target(*t)?;
                let left = if text(t.1, atom!("type")) == "source" {
                    atom!("energy")
                } else {
                    atom!("mineralAmount")
                };
                check(num(t.1, left) <= 0.0, NotEnough)?;
                near(t)?;
                ("harvest", json!({ "id": text(t.1, atom!("_id")) }))
            }
            CreepAct::Pickup(t) => {
                let t = self.target(*t)?;
                check(used(creep.1) >= num(creep.1, atom!("storeCapacity")), Full)?;
                near(t)?;
                ("pickup", json!({ "id": text(t.1, atom!("_id")) }))
            }
            CreepAct::Build(t) | CreepAct::Repair(t) | CreepAct::Upgrade(t) => {
                check(!has_active(creep.1, "work"), NoBodypart)?;
                check(energy <= 0.0, NotEnough)?;
                let t = self.target(*t)?;
                check(range(creep, t) > 3.0, NotInRange)?;
                (stored(act), json!({ "id": text(t.1, atom!("_id")) }))
            }
            CreepAct::RangedAttackCreep(t) | CreepAct::RangedHealCreep(t) => {
                let t = self.target(*t)?;
                check(range(creep, t) > 3.0, NotInRange)?;
                (stored(act), json!({ "id": text(t.1, atom!("_id")) }))
            }
            CreepAct::AttackCreep(t) | CreepAct::HealCreep(t) => {
                let t = self.target(*t)?;
                near(t)?;
                let (x, y) = (num(t.1, atom!("x")), num(t.1, atom!("y")));
                (
                    stored(act),
                    json!({ "id": text(t.1, atom!("_id")), "x": x, "y": y }),
                )
            }
            CreepAct::Reserve(t)
            | CreepAct::Unreserve(t)
            | CreepAct::ClaimController(t)
            | CreepAct::Dismantle(t) => {
                let t = self.target(*t)?;
                near(t)?;
                (stored(act), json!({ "id": text(t.1, atom!("_id")) }))
            }
        };
        self.stored.put(creep.0, &id, name, value);
        Ok(())
    }

    /// An intent on the object `id` once each of `targets` is found.
    fn on(
        &mut self,
        id: world::Id,
        targets: &[world::Id],
        name: &str,
        value: Value,
    ) -> Result<(), ErrorCode> {
        let (room, _) = self.at.get(id).ok_or(NotFound)?;
        for t in targets {
            self.target(*t)?;
        }
        self.stored.put(room, &id.text(), name, value);
        Ok(())
    }

    fn spawn(
        &mut self,
        id: world::Id,
        (body, name, drawn): (&[Part], &str, &[world::Id]),
    ) -> Result<(), ErrorCode> {
        let (room, spawn) = self.at.get(id).ok_or(NotFound)?;
        check(
            self.at.creeps.contains_key(name) || self.created.iter().any(|n| n == name),
            NameExists,
        )?;
        check(obj(spawn, atom!("spawning")).is_some(), Busy)?;
        check(!on(spawn), RclNotEnough)?;
        let bank: f64 = if drawn.is_empty() {
            let banks = self.v.rooms[room].objects.iter();
            let banks =
                banks.filter(|(k, o)| matches!(*k, "spawn" | "extension") && ours(o) && on(o));
            banks.map(|(_, o)| store(o, atom!("energy"))).sum()
        } else {
            let drawn = drawn.iter().filter_map(|d| self.at.get(*d));
            drawn.map(|(_, o)| store(o, atom!("energy"))).sum()
        };
        check(
            bank < f64::from(body.iter().map(|p| p.cost()).sum::<u32>()),
            NotEnough,
        )?;
        self.created.push(name.to_string());
        let parts: Vec<&str> = body.iter().map(|p| part_name(*p)).collect();
        let mut value = json!({ "name": name, "body": parts });
        if !drawn.is_empty() {
            value["energyStructures"] = json!(drawn);
        }
        self.stored.put(room, &id.text(), "createCreep", value);
        Ok(())
    }

    fn act(&mut self, intent: &Action) -> (&'static str, Result<(), ErrorCode>) {
        let resource = |r: &Resource| vocabulary::resource_name(r).to_string();
        match intent {
            Action::Creep(name, act) => (verb(act), self.creep(name, act)),
            Action::Room(room, RoomAct::CreateSite(x, y, kind)) => {
                let Some(r) = self.v.index(&room.to_string()) else {
                    return ("site", Err(NotFound));
                };
                self.sites += 1;
                let value = json!({ "roomName": room.to_string(), "x": x, "y": y,
                    "structureType": kind.name(), "name": format!("Spawn{}", self.sites) });
                self.stored.push(r, "createConstructionSite", value);
                ("site", Ok(()))
            }
            Action::Room(room, RoomAct::Destroy(id)) => match self.target(*id) {
                Ok((r, _)) => {
                    let value = json!({ "id": id, "roomName": room.to_string() });
                    self.stored.push(r, "destroyStructure", value);
                    ("destroy", Ok(()))
                }
                Err(e) => ("destroy", Err(e)),
            },
            Action::Tower(id, act) => {
                let (name, t) = match act {
                    TowerAct::Attack(t) => ("attack", t),
                    TowerAct::Heal(t) => ("heal", t),
                    TowerAct::Repair(t) => ("repair", t),
                };
                (name, self.on(*id, &[*t], name, json!({ "id": t })))
            }
            Action::Controller(id, ControllerAct::ActivateSafeMode) => {
                ("safeMode", self.on(*id, &[], "activateSafeMode", json!({})))
            }
            Action::Controller(id, ControllerAct::Unclaim) => {
                ("unclaim", self.on(*id, &[], "unclaim", json!({})))
            }
            Action::Link(id, LinkAct::TransferEnergy(t, amount)) => {
                let value = json!({ "id": t, "amount": amount, "resourceType": "energy" });
                ("link", self.on(*id, &[*t], "transfer", value))
            }
            Action::Lab(id, LabAct::RunReaction(a, b)) => {
                let value = json!({ "lab1": a, "lab2": b });
                ("runReaction", self.on(*id, &[*a, *b], "runReaction", value))
            }
            Action::Lab(id, LabAct::BoostCreep(c)) => (
                "boostCreep",
                self.on(*id, &[*c], "boostCreep", json!({ "id": c })),
            ),
            Action::Terminal(id, TerminalAct::Send(r, amount, room)) => {
                let value = json!({ "resourceType": resource(r), "amount": amount,
                    "targetRoomName": room.to_string(), "description": "" });
                ("send", self.on(*id, &[], "send", value))
            }
            Action::Observe(id, room) => {
                let value = json!({ "roomName": room.to_string() });
                ("observeRoom", self.on(*id, &[], "observeRoom", value))
            }
            Action::Spawn(id, SpawnAct::RecycleCreep(c)) => (
                "recycle",
                self.on(*id, &[*c], "recycleCreep", json!({ "id": c })),
            ),
            Action::Spawn(id, SpawnAct::SpawnCreep(body, name, drawn)) => {
                ("spawn", self.spawn(*id, (body, name, drawn)))
            }
        }
    }
}

/// The API method's intent name, where it is not the executor's verb.
fn stored(act: &CreepAct) -> &'static str {
    match act {
        CreepAct::Upgrade(_) => "upgradeController",
        CreepAct::Reserve(_) => "reserveController",
        CreepAct::Unreserve(_) => "attackController",
        CreepAct::ClaimController(_) => "claimController",
        act => verb(act),
    }
}

fn part_name(part: Part) -> &'static str {
    match part {
        Part::Work => "work",
        Part::Carry => "carry",
        Part::Move => "move",
        Part::Attack => "attack",
        Part::RangedAttack => "ranged_attack",
        Part::Heal => "heal",
        Part::Claim => "claim",
        _ => "tough",
    }
}

/// `src/app/executor.rs`'s verb, which a refusal is counted under.
fn verb(act: &CreepAct) -> &'static str {
    match act {
        CreepAct::Move(_) => "move",
        CreepAct::Say(_) => "say",
        CreepAct::Harvest(_) => "harvest",
        CreepAct::Transfer(..) => "transfer",
        CreepAct::Withdraw(..) => "withdraw",
        CreepAct::Pickup(_) => "pickup",
        CreepAct::Drop(_) => "drop",
        CreepAct::Build(_) => "build",
        CreepAct::Repair(_) => "repair",
        CreepAct::Upgrade(_) => "upgrade",
        CreepAct::Reserve(_) => "reserve",
        CreepAct::Unreserve(_) => "unreserve",
        CreepAct::ClaimController(_) => "claim",
        CreepAct::AttackCreep(_) => "attack",
        CreepAct::RangedAttackCreep(_) => "rangedAttack",
        CreepAct::RangedMassAttack => "rangedMassAttack",
        CreepAct::HealCreep(_) => "heal",
        CreepAct::RangedHealCreep(_) => "rangedHeal",
        CreepAct::Dismantle(_) => "dismantle",
    }
}

/// One bad intent costs itself, never the rest of the tick's.
pub fn run(v: &View, intents: &[Action]) -> (Stored, Vec<Refusal>) {
    let mut run = Run {
        v,
        at: index(v),
        stored: Stored::default(),
        created: Vec::new(),
        sites: v.k.time as u32,
    };
    let mut refusals = Vec::new();
    for intent in intents {
        if let (act, Err(code)) = run.act(intent) {
            refusals.push(Refusal {
                actor: intent.actor(),
                act: act.to_string(),
                code: code as i32,
            });
        }
    }
    (run.stored, refusals)
}
