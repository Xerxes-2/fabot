//! A scenario's Invaders, level-0 cores and strongholds as the official engine stands them; krebs plays them.
use std::sync::Arc;

use fabot_base::intel::Intel;
use krebs::atom;
use krebs::constants::{INVADER_CORE_HITS, OBSTACLE_OBJECT_TYPES};
use krebs::cron::CronCtx;
use krebs::cron::strongholds::{SpawnOpts, spawn_stronghold};
use krebs::doc::{Doc, Obj, Val};
use krebs::world::World;
use krebs_util::json::Json;
use serde_json::{Value, json};

use crate::view::{View, num, store, text, used, xy};
use crate::world::INVADER;

/// On a tick, in a room: a wave's bodies, a stronghold's [level, deploy], [resource, amount] to
/// stock its containers with, or (null) a level-0 core.
pub type Due = (u64, String, Option<Npc>);

#[derive(serde::Deserialize)]
#[serde(untagged)]
pub enum Npc {
    Wave(Vec<Vec<String>>),
    Stronghold(u32, u32),
    Loot(String, f64),
}

pub fn index(k: &World, room: &str) -> usize {
    let at = k.meta.rooms.iter().position(|m| &*m.name == room);
    at.expect(room)
}

pub fn insert(k: &mut World, r: usize, docs: Vec<Value>) {
    let mut objects = k.rooms[r].objects.to_vec();
    for mut doc in docs {
        doc["_id"] = json!(k.ids.next().as_str());
        if let Val::Obj(o) = Val::from_json(&Json::parse(&doc.to_string()).expect("JSON")) {
            objects.push(Doc::new(k.next_seq, o));
            k.next_seq += 1;
        }
    }
    let next = crate::world::rebuilt(k, r, objects);
    k.set_room(r as u32, next);
    k.active.insert(r as u32);
}

fn wall(k: &World, r: usize, (x, y): (i64, i64)) -> bool {
    k.terrain.raw(r as u32)[(y * 50 + x) as usize] & 1 != 0
}

/// A wave of `bodies` in `room`, placed as genInvaders places a raid: one exit side at random,
/// the first body on a random tile of it and each next on the free tile nearest the last. A side
/// toward a room someone owns or reserves stays open: the schedule, not genInvaders, decides
/// that a wave lands.
pub fn wave(k: &mut World, room: &str, bodies: &[Vec<String>], mut random: impl FnMut() -> f64) {
    let r = index(k, room);
    let sides: [fn(i64) -> (i64, i64); 4] = [|i| (i, 0), |i| (i, 49), |i| (0, i), |i| (49, i)];
    let open: Vec<Vec<(i64, i64)>> = (sides.iter())
        .map(|at| (0..49).map(at).filter(|&p| !wall(k, r, p)).collect())
        .filter(|tiles: &Vec<_>| !tiles.is_empty())
        .collect();
    let mut exits = open[(random() * open.len() as f64) as usize].clone();
    let mut last: Option<(i64, i64)> = None;
    let mut docs = Vec::new();
    for body in bodies {
        let square = match last {
            Some((a, b)) => {
                (exits.iter().copied()).min_by_key(|&(x, y)| (x - a).abs().max((y - b).abs()))
            }
            None => exits.get((random() * exits.len() as f64) as usize).copied(),
        };
        let Some((x, y)) = square else { break };
        exits.retain(|&e| e != (x, y));
        last = square;
        let parts: Vec<Value> = body
            .iter()
            .map(|p| json!({ "type": p, "hits": 100 }))
            .collect();
        let name = format!("invader_{room}_{}", (random() * 1000.0) as u32);
        docs.push(
            json!({ "type": "creep", "user": INVADER, "body": parts, "hits": body.len() * 100,
            "hitsMax": body.len() * 100, "ticksToLive": 1500, "x": x, "y": y, "room": room,
            "fatigue": 0, "store": {}, "storeCapacity": 0, "name": name }),
        );
    }
    insert(k, r, docs);
}

/// A level-0 core in `room` as a stronghold's expansion stands one: on a free tile within 5 of
/// the controller, no wall or obstacle beside it. No parent lends it a collapse timer.
pub fn core(k: &mut World, room: &str, mut random: impl FnMut() -> f64) {
    let r = index(k, room);
    let objects: Vec<Arc<Doc>> = k.rooms[r].objects.to_vec();
    let controller = objects.iter().find(|d| d.kind() == "controller");
    let (cx, cy) = xy(&controller.expect("a controller").obj);
    let blocks = |(x, y): (i64, i64)| {
        let near = |(a, b): (i64, i64)| (a - x).abs() <= 1 && (b - y).abs() <= 1;
        let obstacle = |d: &Arc<Doc>| {
            OBSTACLE_OBJECT_TYPES.contains(&d.kind()) || matches!(d.kind(), "rampart" | "portal")
        };
        (-1..=1).any(|dx| (-1..=1).any(|dy| wall(k, r, (x + dx, y + dy))))
            || objects.iter().any(|d| near(xy(&d.obj)) && obstacle(d))
    };
    let mut pick = |c: i64| {
        let (lo, hi) = ((c - 5).max(1), (c + 5).min(48));
        lo + (random() * (hi - lo) as f64) as i64
    };
    let tile = (0..500).map(|_| (pick(cx), pick(cy))).find(|&p| !blocks(p));
    let Some((x, y)) = tile else { return };
    let doc = json!({ "type": "invaderCore", "level": 0, "room": room, "x": x, "y": y,
        "user": INVADER, "hits": INVADER_CORE_HITS, "hitsMax": INVADER_CORE_HITS,
        "strongholdId": null, "effects": null });
    insert(k, r, vec![doc]);
}

/// `bunker{level}` as the `strongholds.spawn` CLI stands it, deploying in `deploy` ticks.
pub fn stronghold(k: &mut World, room: &str, level: u32, deploy: u32) {
    let r = index(k, room);
    let opts = SpawnOpts {
        template_name: Some(format!("bunker{level}")),
        deploy_time: Some(f64::from(deploy)),
        ..SpawnOpts::default()
    };
    let mut cx = CronCtx::new(k.seed, k.time, 0, "spawnStronghold");
    if let Err(e) = spawn_stronghold(k, &mut cx, r as u32, &opts) {
        panic!("a stronghold in {room}: {e}");
    }
}

/// Each stronghold by room, beside intel's `[level, deploys_at, collapses_at, seen]`; fallen, level 0.
pub fn strongholds(v: &View, intel: &Intel) -> Value {
    let rooms = v.rooms.iter().filter_map(|room| {
        let Some(core) = (room.of("invaderCore")).find(|o| num(o, atom!("level")) > 0.0) else {
            let loot_of = |kinds: &[&str]| -> f64 {
                let held = kinds.iter().flat_map(|k| room.of(k));
                held.map(|o| used(o) - store(o, atom!("energy"))).sum()
            };
            let (loot, ruin) = (loot_of(&["ruin", "container", "tombstone"]), loot_of(&["ruin"]));
            let fallen = room.of("rampart").any(|o| text(o, atom!("user")) == INVADER);
            return fallen.then(|| (room.name.to_string(), json!({ "level": 0, "loot": loot, "ruin": ruin })));
        };
        let row = room.name.parse().ok().and_then(|r| intel.get(&r));
        let read = row.and_then(|r| r.stronghold.map(|s| json!([s.level, s.deploys_at, s.collapses_at, r.seen])));
        let defenders = (room.of("creep")).filter(|c| !text(c, atom!("strongholdId")).is_empty());
        Some((room.name.to_string(), json!({ "level": num(core, atom!("level")), "deployTime": num(core, atom!("deployTime")),
            "collapse": collapse(core).unwrap_or(0.0), "towers": room.of("tower").count(), "defenders": defenders.count(), "intel": read })))
    });
    Value::Object(rooms.collect())
}

pub fn collapse(o: &Obj) -> Option<f64> {
    let Some(Val::Arr(effects)) = o.get(atom!("effects")) else {
        return None;
    };
    let timer = f64::from(krebs::constants::EFFECT_COLLAPSE_TIMER);
    (effects.iter().filter_map(Val::as_obj))
        .find(|e| num(e, atom!("effect")) == timer)
        .map(|e| num(e, atom!("endTime")))
}
