//! `sim duel`: the generated set's duels (`npm run exchange`), one world a
//! duel. Reads `exchange`'s samples on stdin plus each one's `ground` and
//! `owner`; both sides run the simplest Close/Kite emitter, not the bot's
//! tactics, an Invader the engine's own AI. Writes `{ id: { won, ticks, dead,
//! hits } }`: `won` null where both stand, ticks from first contact (two
//! bodies within three, else the first hit) to the end, our bodies dead and
//! our hits lost (a dead body's whole).
//!
//! Usage: sim duel [--ticks N] [--seed N]
use std::collections::BTreeMap;
use std::io::Read;
use std::sync::atomic::{AtomicUsize, Ordering};

use krebs::atom;
use krebs::cron::JOBS;
use krebs::doc::Val;
use krebs::hooks::ProcessorHooks;
use krebs::intents::RoomIntents;
use krebs::room_name::RoomXY;
use krebs::world::World;
use krebs_pathfinder::{Goal, PathFinder, Pos, RoomCost, SearchOpts};
use screeps::{INVADER_USERNAME, Part};
use serde::Deserialize;
use serde_json::{Map, Value, json};

use crate::act::Stored;
use crate::view::{num, text, xy};
use crate::world::{self, INVADER, RIVAL, US, USERNAME};

#[derive(Deserialize, Clone, Copy, PartialEq)]
#[serde(rename_all = "lowercase")]
enum Side {
    Ours,
    Theirs,
}

#[derive(Deserialize)]
struct At {
    x: i32,
    y: i32,
}

#[derive(Deserialize)]
struct Body {
    parts: Vec<String>,
    boosts: Vec<String>,
    hits: u32,
    x: i32,
    y: i32,
    #[serde(default)]
    invader: bool,
}

#[derive(Deserialize)]
struct Tower {
    x: i32,
    y: i32,
    energy: u32,
}

#[derive(Deserialize)]
struct Rampart {
    x: i32,
    y: i32,
    hits: u32,
}

#[derive(Deserialize)]
struct Army {
    bodies: Vec<Body>,
    towers: Vec<Tower>,
    ramparts: Vec<Rampart>,
}

#[derive(Deserialize)]
struct Sample {
    id: String,
    room: String,
    owner: Side,
    ground: Vec<String>,
    controller: At,
    ours: Army,
    theirs: Army,
}

#[derive(PartialEq)]
enum Kind {
    Creep,
    Tower,
    Rampart,
}

struct Thing {
    id: String,
    kind: Kind,
    user: String,
    x: i32,
    y: i32,
    hits: f64,
    hits_max: f64,
    fatigue: f64,
    active: Vec<Part>,
}

impl Thing {
    fn has(&self, part: Part) -> bool {
        self.active.contains(&part)
    }
    fn hurt(&self) -> bool {
        self.hits < self.hits_max
    }
    fn range(&self, o: &Thing) -> i32 {
        (self.x - o.x).abs().max((self.y - o.y).abs())
    }
    fn closest<'a>(&self, xs: &[&'a Thing]) -> Option<&'a Thing> {
        xs.iter().copied().min_by_key(|o| self.range(o))
    }
    fn side(&self) -> Side {
        if self.user == US {
            Side::Ours
        } else {
            Side::Theirs
        }
    }
}

/// One side's creeps' hits by id.
fn hits(all: &[Thing], side: Side) -> BTreeMap<String, f64> {
    let creeps = all
        .iter()
        .filter(|c| c.kind == Kind::Creep && c.side() == side);
    creeps.map(|c| (c.id.clone(), c.hits)).collect()
}

fn things(world: &World) -> Vec<Thing> {
    let docs = world.rooms[0].objects.iter();
    docs.filter_map(|d| {
        let kind = match d.kind() {
            "creep" => Kind::Creep,
            "tower" => Kind::Tower,
            "rampart" => Kind::Rampart,
            _ => return None,
        };
        let o = &d.obj;
        let parts = match o.get(atom!("body")) {
            Some(Val::Arr(parts)) => parts.iter().filter_map(Val::as_obj).collect(),
            _ => Vec::new(),
        };
        Some(Thing {
            id: text(o, atom!("_id")).to_string(),
            kind,
            user: text(o, atom!("user")).to_string(),
            x: xy(o).0 as i32,
            y: xy(o).1 as i32,
            hits: num(o, atom!("hits")),
            hits_max: num(o, atom!("hitsMax")),
            fatigue: num(o, atom!("fatigue")),
            active: (parts.into_iter().filter(|p| num(p, atom!("hits")) > 0.0))
                .filter_map(|p| text(p, atom!("type")).parse().ok())
                .collect(),
        })
    })
    .collect()
}

/// One side's intents this tick, as `PathFinder.search` (terrain only, one room) and the game API store them.
struct Orders<'a> {
    pf: &'a mut PathFinder,
    room: krebs_pathfinder::RoomXY,
    terrain: &'a [u8; 2500],
    stored: Stored,
}

impl Orders<'_> {
    fn put(&mut self, o: &Thing, name: &str, value: Value) {
        self.stored.put(0, &o.id, name, value);
    }

    fn at(o: &Thing) -> Value {
        json!({ "id": o.id, "x": o.x, "y": o.y })
    }

    fn step(&mut self, c: &Thing, goals: &[(&Thing, u32)], flee: bool) {
        let pos = |x: i32, y: i32| Pos {
            room: self.room,
            x: x as u8,
            y: y as u8,
        };
        let goals: Vec<Goal> = (goals.iter())
            .map(|(o, range)| Goal {
                pos: pos(o.x, o.y),
                range: *range,
            })
            .collect();
        let opts = SearchOpts::from_js(0, 0, None, 0, 0, 1, flee);
        let terrain = self.terrain;
        let found = (self.pf).search(
            pos(c.x, c.y),
            &goals,
            &opts,
            |_| Some(terrain),
            |_| RoomCost::Terrain,
        );
        let Some(next) = found.expect("one room's terrain").path.first().copied() else {
            return;
        };
        let (dx, dy) = (i32::from(next.x) - c.x, i32::from(next.y) - c.y);
        let compass = [
            (0, -1),
            (1, -1),
            (1, 0),
            (1, 1),
            (0, 1),
            (-1, 1),
            (-1, 0),
            (-1, -1),
        ];
        let direction = 1 + compass.iter().position(|&d| d == (dx, dy)).expect("a step");
        if c.fatigue == 0.0 && c.has(Part::Move) {
            self.put(c, "move", json!({ "direction": direction }));
        }
    }

    /// The most hurt ally in reach healed; its range, or None when no ally is hurt.
    fn mend(&mut self, c: &Thing, allies: &[&Thing]) -> Option<i32> {
        let ratio = |o: &&Thing| o.hits / o.hits_max;
        let patient =
            (allies.iter().filter(|a| a.hurt())).min_by(|a, b| ratio(a).total_cmp(&ratio(b)))?;
        let range = c.range(patient);
        if range <= 1 {
            self.put(c, "heal", Self::at(patient));
        } else if range <= 3 {
            self.put(c, "rangedHeal", json!({ "id": patient.id }));
        }
        Some(range)
    }

    /// A swing closes on the nearest foe; a gun holds three off it and flees a swing inside two; a
    /// healer keeps to its most hurt ally; a WORK body dismantles the nearest tower or rampart.
    fn act(&mut self, c: &Thing, allies: &[&Thing], foes: &[&Thing], their_structures: &[&Thing]) {
        let target = c.closest(foes);
        if c.has(Part::Attack) {
            let Some(target) = target else { return };
            if c.range(target) <= 1 {
                self.put(c, "attack", Self::at(target));
            } else if c.has(Part::Heal) && c.hurt() {
                self.put(c, "heal", Self::at(c));
            }
            self.step(c, &[(target, u32::from(c.range(target) > 1))], false);
        } else if c.has(Part::RangedAttack) {
            if c.has(Part::Heal) {
                if c.hurt() {
                    self.put(c, "heal", Self::at(c));
                } else {
                    self.mend(c, allies);
                }
            }
            let Some(target) = target else { return };
            if foes.iter().filter(|f| c.range(f) <= 1).count() >= 2 {
                self.put(c, "rangedMassAttack", json!({}));
            } else if c.range(target) <= 3 {
                self.put(c, "rangedAttack", json!({ "id": target.id }));
            }
            let swings: Vec<_> = (foes.iter())
                .filter(|f| f.has(Part::Attack) && c.range(f) <= 2)
                .map(|f| (*f, 3))
                .collect();
            if !swings.is_empty() {
                self.step(c, &swings, true);
            } else if c.range(target) > 3 {
                self.step(c, &[(target, 3)], false);
            }
        } else if c.has(Part::Heal) {
            let others: Vec<&Thing> = allies.iter().copied().filter(|a| a.id != c.id).collect();
            let range = self.mend(c, allies);
            let hurt = (others.iter().copied().filter(|a| a.hurt()))
                .min_by(|a, b| a.hits.total_cmp(&b.hits));
            match hurt.or_else(|| c.closest(&others)) {
                Some(patient) if range != Some(0) => self.step(c, &[(patient, 1)], false),
                None if target.is_some() => {
                    let away: Vec<_> = foes.iter().map(|f| (*f, 4)).collect();
                    self.step(c, &away, true);
                }
                _ => {}
            }
        } else if c.has(Part::Work) {
            let Some(goal) = c.closest(their_structures) else {
                return;
            };
            if c.range(goal) <= 1 {
                self.put(c, "dismantle", json!({ "id": goal.id }));
            } else {
                self.step(c, &[(goal, 1)], false);
            }
        }
    }
}

/// The sample's room: its owner's controller at RCL8 (so the owner's towers fire), each side's
/// towers, ramparts and bodies.
fn stand(s: &Sample, seed: u64) -> World {
    let room = &s.room;
    let owner = if s.owner == Side::Ours { US } else { RIVAL.0 };
    let mut objects = vec![
        json!({ "type": "controller", "room": room, "x": s.controller.x, "y": s.controller.y,
        "user": owner, "level": 8, "progress": 0, "downgradeTime": 1_000_000, "safeModeAvailable": 0, "safeMode": 0 }),
    ];
    for (army, user, side) in [(&s.ours, US, "ours"), (&s.theirs, RIVAL.0, "theirs")] {
        for t in &army.towers {
            objects.push(json!({ "type": "tower", "room": room, "x": t.x, "y": t.y, "user": user, "hits": 3000,
                "hitsMax": 3000, "store": { "energy": t.energy }, "storeCapacityResource": { "energy": 1000 } }));
        }
        for r in &army.ramparts {
            objects.push(json!({ "type": "rampart", "room": room, "x": r.x, "y": r.y, "user": user, "hits": r.hits,
                "hitsMax": 300_000_000, "nextDecayTime": 101 }));
        }
        for (i, b) in army.bodies.iter().enumerate() {
            let body: Vec<Value> = (b.parts.iter().zip(&b.boosts))
                .map(|(p, k)| match k.as_str() {
                    "" => json!({ "type": p, "hits": 100 }),
                    k => json!({ "type": p, "hits": 100, "boost": k }),
                })
                .collect();
            let user = if b.invader { INVADER } else { user };
            objects.push(json!({ "type": "creep", "room": room, "x": b.x, "y": b.y, "user": user, "body": body,
                "name": format!("{}-{side}-{i}", s.id), "hits": b.hits, "hitsMax": b.hits,
                "fatigue": 0, "store": {}, "storeCapacity": 0, "notifyWhenAttacked": true }));
        }
    }
    for (i, o) in objects.iter_mut().enumerate() {
        o["_id"] = json!(format!("{:024x}", i + 1));
    }
    let mut users = [
        world::user(US, USERNAME),
        world::user(RIVAL.0, RIVAL.1),
        world::user(INVADER, INVADER_USERNAME),
    ];
    users[usize::from(s.owner == Side::Theirs)]["rooms"] = json!([room]);
    let terrain: String = s.ground.concat();
    let db = json!({ "rooms": [world::room_doc(room, false)], "rooms.terrain": [{ "room": room, "terrain": terrain }],
        "users": users, "rooms.objects": objects });
    world::krebs(db, seed, 1)
}

fn duel(s: &Sample, ticks: u64, seed: u64, pf: &mut PathFinder) -> Value {
    let mut world = stand(s, seed);
    // No cron in a duel: each job last ran at the end of time.
    let mut engine = crate::Engine {
        hooks: ProcessorHooks::default(),
        cron_last: vec![i64::MAX; JOBS.len()],
        draw: seed,
        processed: 0,
    };
    let xy = RoomXY::parse_engine(&s.room).expect("a room name");
    let room = krebs_pathfinder::RoomXY { x: xy.x, y: xy.y };
    let terrain = *world.terrain.raw(0);
    let user_idx = |id: &str| {
        let ids = world.users.iter().map(|u| text(&u.doc, atom!("_id")));
        ids.into_iter().position(|u| u == id).expect("a user") as u32
    };
    let (us, them) = (user_idx(US), user_idx(RIVAL.0));
    let begun = things(&world);
    let first = [hits(&begun, Side::Ours), hits(&begun, Side::Theirs)];
    let (mut last, mut contact, mut struck, mut end, mut won) =
        (BTreeMap::new(), None, None, None, None);
    for t in 0..ticks {
        let all = things(&world);
        let creeps: Vec<&Thing> = all.iter().filter(|o| o.kind == Kind::Creep).collect();
        let now = [hits(&all, Side::Ours), hits(&all, Side::Theirs)];
        if (first.iter().zip(&now)).any(|(f, n)| {
            f.iter()
                .any(|(id, h)| n.get(id).copied().unwrap_or(0.0) < *h)
        }) {
            struck = struck.or(Some(t));
        }
        let near = (creeps.iter().filter(|c| c.side() == Side::Ours)).any(|a| {
            creeps
                .iter()
                .any(|b| b.side() == Side::Theirs && a.range(b) <= 3)
        });
        if near {
            contact = contact.or(Some(t));
        }
        let [ours, theirs] = now;
        last = ours;
        if last.is_empty() || theirs.is_empty() {
            (end, won) = (Some(t), Some(!last.is_empty()));
            break;
        }
        let mut users = Vec::new();
        for (user, idx) in [(US, us), (RIVAL.0, them)] {
            let mut orders = Orders {
                pf,
                room,
                terrain: &terrain,
                stored: Stored::default(),
            };
            let (allies, foes): (Vec<&Thing>, Vec<&Thing>) =
                creeps.iter().partition(|c| c.user == user);
            let their_structures: Vec<&Thing> = (all.iter())
                .filter(|o| o.kind != Kind::Creep && o.user != user)
                .collect();
            for tower in all
                .iter()
                .filter(|o| o.kind == Kind::Tower && o.user == user)
            {
                if let Some(target) = tower.closest(&foes) {
                    orders.put(tower, "attack", json!({ "id": target.id }));
                }
            }
            for c in &allies {
                orders.act(c, &allies, &foes, &their_structures);
            }
            users.extend(
                orders
                    .stored
                    .rooms(idx)
                    .into_iter()
                    .flat_map(|(_, r)| r.users),
            );
        }
        engine.step(&mut world, vec![(0, RoomIntents { users })]);
    }
    let (mut dead, mut lost) = (0, 0.0);
    for (id, h) in &first[0] {
        dead += u32::from(!last.contains_key(id));
        lost += h - last.get(id).copied().unwrap_or(0.0);
    }
    json!({ "won": won, "ticks": end.zip(contact.or(struck)).map(|(e, s)| e - s), "dead": dead, "hits": lost })
}

pub fn main() {
    let mut flags = BTreeMap::from([("--ticks".to_string(), 400), ("--seed".to_string(), 1)]);
    let mut argv = std::env::args().skip(2);
    while let (Some(flag), Some(value)) = (argv.next(), argv.next()) {
        let slot = flags
            .get_mut(&flag)
            .expect("usage: sim duel [--ticks N] [--seed N]");
        *slot = value.parse().expect("a number");
    }
    let (ticks, seed) = (flags["--ticks"], flags["--seed"]);
    let mut text = String::new();
    std::io::stdin()
        .read_to_string(&mut text)
        .expect("samples on stdin");
    let samples: Vec<Sample> = serde_json::from_str(&text).expect("exchange's samples");
    let next = AtomicUsize::new(0);
    let workers = std::thread::available_parallelism().map_or(1, usize::from);
    let mut results: Vec<(usize, Value)> = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..workers)
            .map(|_| {
                scope.spawn(|| {
                    let mut pf = PathFinder::new();
                    let mut done = Vec::new();
                    loop {
                        let i = next.fetch_add(1, Ordering::Relaxed);
                        let Some(s) = samples.get(i) else { break done };
                        done.push((i, duel(s, ticks, seed, &mut pf)));
                    }
                })
            })
            .collect();
        handles
            .into_iter()
            .flat_map(|h| h.join().expect("a duel thread"))
            .collect()
    });
    results.sort_by_key(|(i, _)| *i);
    let results_by_id: Map<String, Value> = (results.into_iter())
        .map(|(i, r)| (samples[i].id.clone(), r))
        .collect();
    println!("{}", Value::Object(results_by_id));
}
