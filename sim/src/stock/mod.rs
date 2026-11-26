use std::sync::Arc;

use krebs::{
    doc::{Doc, Val},
    world::World,
};
use serde_json::{Value, json};

use crate::npc;

/// `amount` of `resource` in each of `room`'s containers, as a season stocks a stronghold's loot.
pub fn loot(k: &mut World, room: &str, resource: &str, amount: f64) {
    let r = npc::index(k, room);
    let (held, kept): (Vec<Arc<Doc>>, Vec<Arc<Doc>>) =
        (k.rooms[r].objects.iter().cloned()).partition(|d| d.kind() == "container");
    let next = crate::world::rebuilt(k, r, kept);
    k.set_room(r as u32, next);
    let json = |d: &Arc<Doc>| Val::Obj(d.obj.clone()).to_json().stringify();
    let docs = (held.iter()).filter_map(|d| serde_json::from_str::<Value>(&json(d)).ok());
    let stocked = docs.map(|mut doc| {
        doc["store"][resource] = json!(amount);
        doc
    });
    npc::insert(k, r, stocked.collect());
}
