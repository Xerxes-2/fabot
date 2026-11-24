//! A tick as `engine/engaged.mjs` reads it, a line of `fight.jsonl` while an
//! engagement is open and on the tick after: Fight's Memory, the bodies, cores
//! and tombstones standing, and who struck or healed whom.
use krebs::atom;
use serde_json::{Value, json};

use crate::view::{EVENT_ATTACK, EVENT_HEAL, Room, View, num, text};

pub fn row(v: &View, memory: &str, t: u64, open: &mut bool) -> Option<Value> {
    let m: Value = (memory.contains("\"engagements\":[{"))
        .then(|| serde_json::from_str(memory).ok())
        .flatten()
        .unwrap_or_default();
    let engaged = m["v2"]["fight"]["engagements"]
        .as_array()
        .is_some_and(|e| !e.is_empty());
    let was = std::mem::replace(open, engaged);
    if !was && !*open {
        return None;
    }
    let objects: Vec<Value> = (v.rooms.iter())
        .flat_map(|r| r.objects.iter().map(move |(k, o)| (r.name, *k, *o)))
        .filter(|(_, k, _)| matches!(*k, "creep" | "invaderCore" | "tombstone"))
        .map(|(room, kind, o)| {
            let name = o.get(atom!("name")).or(o.get(atom!("creepName")));
            json!({ "id": text(o, atom!("_id")), "name": name.and_then(|n| n.as_str()), "room": room,
                "x": num(o, atom!("x")), "y": num(o, atom!("y")), "type": kind, "ours": v.ours(o),
                "spent": crate::read::spent(o) })
        })
        .collect();
    let struck: Vec<Value> = (v.rooms.iter().flat_map(Room::events))
        .filter(|e| matches!(e.kind, EVENT_ATTACK | EVENT_HEAL))
        .map(|e| json!([e.by, e.data["targetId"]]))
        .collect();
    Some(json!({ "t": t, "fight": m["v2"]["fight"], "objects": objects, "struck": struck }))
}
