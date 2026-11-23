//! krebs's world as one tick's reader sees it: each room's objects by kind,
//! its event log parsed once (empty unless the room ran last tick), and
//! lookups by room name, object id and our creeps' names.
use krebs::atom;
use krebs::doc::{Atom, Obj, Val};
use krebs::world::World as Krebs;
use serde_json::Value;

use crate::world::US;

pub use krebs::constants::{
    EVENT_ATTACK, EVENT_BUILD, EVENT_HARVEST, EVENT_HEAL, EVENT_OBJECT_DESTROYED, EVENT_REPAIR,
    EVENT_TRANSFER, EVENT_UPGRADE_CONTROLLER,
};

pub fn num(o: &Obj, k: Atom) -> f64 {
    o.get(k).and_then(Val::as_f64).unwrap_or(0.0)
}

pub fn text(o: &Obj, k: Atom) -> &str {
    o.get(k).and_then(Val::as_str).unwrap_or("")
}

pub fn obj(o: &Obj, k: Atom) -> Option<&Obj> {
    o.get(k).and_then(Val::as_obj)
}

/// Of the first seat, the one the samples follow.
pub fn ours(o: &Obj) -> bool {
    text(o, atom!("user")) == US
}

/// The game API's `isActive` for a spawn or extension: the engine marks one off.
pub fn on(o: &Obj) -> bool {
    !matches!(o.get(atom!("off")), Some(Val::Bool(true)))
}

pub fn store(o: &Obj, resource: Atom) -> f64 {
    obj(o, atom!("store")).map_or(0.0, |s| num(s, resource))
}

pub fn used(o: &Obj) -> f64 {
    obj(o, atom!("store")).map_or(0.0, |s| s.iter().filter_map(|(_, v)| v.as_f64()).sum())
}

pub fn xy(o: &Obj) -> (i64, i64) {
    (num(o, atom!("x")) as i64, num(o, atom!("y")) as i64)
}

pub fn ms(at: std::time::Instant) -> f64 {
    at.elapsed().as_secs_f64() * 1000.0
}

pub struct Event<'a> {
    pub kind: u32,
    pub by: &'a str,
    pub data: &'a Value,
}

pub struct Room<'a> {
    pub name: &'a str,
    pub objects: Vec<(&'a str, &'a Obj)>,
    pub events: Value,
}

impl Room<'_> {
    pub fn events(&self) -> impl Iterator<Item = Event<'_>> {
        (self.events.as_array().into_iter().flatten()).map(|e| Event {
            kind: e["event"].as_u64().unwrap_or(0) as u32,
            by: e["objectId"].as_str().unwrap_or_default(),
            data: &e["data"],
        })
    }

    pub fn of(&self, kind: &str) -> impl Iterator<Item = &Obj> {
        self.objects
            .iter()
            .filter(move |(k, _)| *k == kind)
            .map(|(_, o)| *o)
    }
}

pub struct View<'a> {
    pub k: &'a Krebs,
    pub rooms: Vec<Room<'a>>,
    pub us: &'a str,
}

impl<'a> View<'a> {
    pub fn of(k: &'a Krebs) -> View<'a> {
        let rooms = (k.rooms.iter().enumerate())
            .map(|(r, s)| Room {
                name: &k.meta.rooms[r].name,
                objects: s.objects.iter().map(|d| (d.kind(), &d.obj)).collect(),
                events: if s.processed_at + 1 == k.time && s.event_log.len() > 2 {
                    serde_json::from_str(&s.event_log).unwrap_or(Value::Null)
                } else {
                    Value::Null
                },
            })
            .collect();
        View { k, rooms, us: US }
    }

    pub fn ours(&self, o: &Obj) -> bool {
        text(o, atom!("user")) == self.us
    }

    pub fn index(&self, room: &str) -> Option<usize> {
        self.rooms.iter().position(|r| r.name == room)
    }

    pub fn room(&self, room: &str) -> &Room<'a> {
        &self.rooms[self.index(room).expect("a room of the world")]
    }
}
