//! `sim duel`: the generated set's duels (`npm run exchange`), one world a
//! duel. Reads `exchange`'s samples on stdin plus each one's `ground` and
//! `owner`; both sides run the simplest Close/Kite emitter, or with `--real 1`
//! ours the bot's tactics (`real`); an Invader runs the engine's own AI. Writes
//! `{ id: { won, ticks, dead, hits } }`: `won` null where both stand, ticks from
//! first contact (two bodies within three, else the first hit) to the end, our
//! bodies dead and our hits lost (a dead body's whole).
//!
//! Usage: sim duel [--ticks N] [--seed N] [--real 1] [`pgs::FLAGS`]
mod pgs;
mod real;

use std::collections::BTreeMap;
use std::io::Read;
use std::sync::atomic::{AtomicUsize, Ordering};

use fabot_base::engine;
use fabot_tactics::Tactic;
use krebs::atom;
use krebs::cron::JOBS;
use krebs::doc::{Id, Val};
use krebs::hooks::ProcessorHooks;
use krebs::intents::RoomIntents;
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
    /// The tactic our side fields.
    tactic: Tactic,
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
        let direction = crate::rival::direction(pos(c.x, c.y), next).expect("a step");
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
    fn act(&mut self, c: &Thing, v: &View) {
        let (allies, foes, theirs) = (&v.allies, &v.foes, &v.theirs);
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
            let Some(goal) = c.closest(theirs) else {
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
            let body: Vec<Value> = (b.parts.iter().zip(&b.boosts).enumerate())
                .map(|(i, (p, k))| {
                    let hits = fabot_base::engine::part_hits_at(b.parts.len(), b.hits as i32, i);
                    match k.as_str() {
                        "" => json!({ "type": p, "hits": hits }),
                        k => json!({ "type": p, "hits": hits, "boost": k }),
                    }
                })
                .collect();
            let user = if b.invader { INVADER } else { user };
            objects.push(json!({ "type": "creep", "room": room, "x": b.x, "y": b.y, "user": user, "body": body,
                "name": format!("{}-{side}-{i}", s.id), "hits": b.hits, "hitsMax": engine::PART_HITS * b.parts.len() as i32,
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
    let db = json!({ "rooms": [crate::generated::room_doc(room, false, None)], "rooms.terrain": [{ "room": room, "terrain": terrain }],
        "users": users, "rooms.objects": objects });
    world::krebs(db, seed, 1)
}

/// What a side's body acts on: its side's creeps, the other side's, and the other side's structures.
struct View<'a> {
    allies: Vec<&'a Thing>,
    foes: Vec<&'a Thing>,
    theirs: Vec<&'a Thing>,
}

/// A duel under way; `now` is each side's hits as last read.
struct Duel {
    world: World,
    t: u64,
    first: [BTreeMap<String, f64>; 2],
    now: [BTreeMap<String, f64>; 2],
    contact: Option<u64>,
    struck: Option<u64>,
    end: Option<u64>,
    won: Option<bool>,
    real: Option<real::Real>,
}

/// How a side's bodies act in a tick.
type Act<'p> = &'p dyn Fn(&mut Orders, &Thing, &View);

impl Duel {
    fn new(s: &Sample, seed: u64, real: bool) -> Self {
        let world = stand(s, seed);
        let begun = things(&world);
        let first = [hits(&begun, Side::Ours), hits(&begun, Side::Theirs)];
        Duel {
            world,
            t: 0,
            now: first.clone(),
            first,
            contact: None,
            struck: None,
            end: None,
            won: None,
            real: real.then(|| real::Real::new(s.tactic)),
        }
    }

    // No cron in a duel: each job last ran at the end of time.
    fn engine(&self) -> crate::Engine {
        crate::Engine {
            hooks: ProcessorHooks::default(),
            cron_last: vec![i64::MAX; JOBS.len()],
            draw: self.world.seed,
            processed: 0,
        }
    }

    fn read(&mut self) -> Vec<Thing> {
        let all = things(&self.world);
        let creeps: Vec<&Thing> = all.iter().filter(|o| o.kind == Kind::Creep).collect();
        self.now = [hits(&all, Side::Ours), hits(&all, Side::Theirs)];
        if (self.first.iter().zip(&self.now)).any(|(f, n)| {
            f.iter()
                .any(|(id, h)| n.get(id).copied().unwrap_or(0.0) < *h)
        }) {
            self.struck = self.struck.or(Some(self.t));
        }
        let near = (creeps.iter().filter(|c| c.side() == Side::Ours)).any(|a| {
            creeps
                .iter()
                .any(|b| b.side() == Side::Theirs && a.range(b) <= 3)
        });
        if near {
            self.contact = self.contact.or(Some(self.t));
        }
        if self.now.iter().any(BTreeMap::is_empty) {
            (self.end, self.won) = (Some(self.t), Some(!self.now[0].is_empty()));
        }
        all
    }

    fn tick(&mut self, engine: &mut crate::Engine, pf: &mut PathFinder, acts: [Act; 2]) -> bool {
        let all = self.read();
        if self.won.is_some() {
            return false;
        }
        let creeps: Vec<&Thing> = all.iter().filter(|o| o.kind == Kind::Creep).collect();
        let xy = self.world.meta.rooms[0].xy;
        let mut users = Vec::new();
        for (user, act) in [US, RIVAL.0].into_iter().zip(acts) {
            let idx = self.world.user_ids[&Id::new(user).expect("a user id")];
            let mut orders = Orders {
                pf,
                room: krebs_pathfinder::RoomXY { x: xy.x, y: xy.y },
                terrain: self.world.terrain.raw(0),
                stored: Stored::default(),
            };
            let real = (self.real.as_mut()).filter(|_| user == US);
            let ours = real.is_some();
            if let Some(real) = real {
                orders.stored = real.orders(&self.world);
            }
            let (allies, foes) = creeps.iter().partition(|c| c.user == user);
            let theirs = (all.iter())
                .filter(|o| o.kind != Kind::Creep && o.user != user)
                .collect();
            let view = View {
                allies,
                foes,
                theirs,
            };
            for tower in all
                .iter()
                .filter(|o| o.kind == Kind::Tower && o.user == user)
            {
                if let Some(target) = tower.closest(&view.foes) {
                    orders.put(tower, "attack", json!({ "id": target.id }));
                }
            }
            for c in view.allies.iter().filter(|_| !ours) {
                act(&mut orders, c, &view);
            }
            users.extend(
                orders
                    .stored
                    .rooms(idx)
                    .into_iter()
                    .flat_map(|(_, r)| r.users),
            );
        }
        engine.step(&mut self.world, vec![(0, RoomIntents { users })]);
        self.t += 1;
        true
    }

    fn result(&self) -> Value {
        let (mut dead, mut lost) = (0, 0.0);
        for (id, h) in &self.first[0] {
            dead += u32::from(!self.now[0].contains_key(id));
            lost += h - self.now[0].get(id).copied().unwrap_or(0.0);
        }
        let ticks = self
            .end
            .zip(self.contact.or(self.struck))
            .map(|(e, s)| e - s);
        json!({ "won": self.won, "ticks": ticks, "dead": dead, "hits": lost })
    }
}

fn duel(s: &Sample, ticks: u64, seed: u64, real: bool, pf: &mut PathFinder) -> Value {
    let mut d = Duel::new(s, seed, real);
    let mut engine = d.engine();
    let emit = |o: &mut Orders, c: &Thing, v: &View| o.act(c, v);
    while d.t < ticks && d.tick(&mut engine, pf, [&emit, &emit]) {}
    d.result()
}

pub fn main() {
    let flags = [("--ticks", 400), ("--seed", 1), ("--real", 0)].map(|(f, v)| (f.to_string(), v));
    let mut flags = BTreeMap::from(flags);
    flags.extend(pgs::FLAGS.map(|(f, v)| (f.to_string(), v)));
    let usage: String = flags.keys().map(|f| format!(" [{f} N]")).collect();
    let mut argv = std::env::args().skip(2);
    while let (Some(flag), Some(value)) = (argv.next(), argv.next()) {
        let slot = (flags.get_mut(&flag)).unwrap_or_else(|| panic!("usage: sim duel{usage}"));
        *slot = value.parse().expect("a number");
    }
    let (ticks, seed, real) = (flags["--ticks"], flags["--seed"], flags["--real"] > 0);
    let search = (flags["--rollout"] > 0).then(|| pgs::Search {
        rollout: flags["--rollout"],
        replan: flags["--replan"],
        rounds: flags["--rounds"],
        theirs: flags["--theirs"] > 0,
    });
    let run = |s: &Sample, pf: &mut PathFinder| match &search {
        Some(search) => pgs::duel(s, (ticks, seed, real), pf, search),
        None if flags["--check-fork"] > 0 => pgs::check_fork(s, (ticks, seed, real), pf),
        None => duel(s, ticks, seed, real, pf),
    };
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
                        done.push((i, run(s, &mut pf)));
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
