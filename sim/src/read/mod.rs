//! `World.ofGame` off krebs's documents: what the game API derives from a
//! document (ticks to live, to regenerate, to downgrade) is derived here as
//! the API does, and the rooms are those our objects give vision of or our
//! observers looked at last tick.
use std::collections::BTreeSet;

use fabot_base::{
    declarations::Declarations,
    vocabulary::{self, Part},
    world::*,
};
use krebs::{
    atom,
    doc::{Atom, Obj, Val},
    world::{UserIdx, World as Krebs},
};
use screeps::{
    CPU_BUCKET_MAX, GCL_MULTIPLY, GCL_POW, StructureType, controller_downgrade, controller_levels,
};

use crate::view::{self, EVENT_ATTACK, EVENT_OBJECT_DESTROYED, View, num, obj, text};

fn username<'a>(k: &'a Krebs, user: &'a str) -> &'a str {
    let at = k.users.iter().find(|u| text(&u.doc, atom!("_id")) == user);
    at.map_or(user, |u| text(&u.doc, atom!("username")))
}

pub fn our_idx(v: &View) -> UserIdx {
    let at = v.k.users.iter().position(|u| text(&u.doc, atom!("_id")) == v.us);
    at.expect("our user") as UserIdx
}

/// `Game.gcl.level` off our points.
fn rooms_allowed(v: &View) -> i32 {
    let points = num(&v.k.users[our_idx(v) as usize].doc, atom!("gcl"));
    (points / f64::from(GCL_MULTIPLY)).powf(1.0 / GCL_POW).floor() as i32 + 1
}

/// The cost a destroyed structure's log line is credited with.
pub fn build_cost(kind: &str) -> i32 {
    let kind = kind.parse::<StructureType>().ok();
    kind.and_then(StructureType::construction_cost).unwrap_or(0) as i32
}

pub fn spent(o: &Obj) -> bool {
    fabot_base::world::spent(&Reader::body(o))
}

struct Reader<'a> {
    time: i32,
    declarations: &'a Declarations,
    k: &'a Krebs,
    me: &'a str,
}

impl Reader<'_> {
    fn left(&self, o: &Obj, k: Atom) -> i32 {
        (num(o, k) as i32 - self.time).max(0)
    }

    fn owner(&self, o: &Obj) -> Owner {
        match text(o, atom!("user")) {
            "" => Owner::Nobody,
            user => Owner::of_user(username(self.k, user).into(), self.me, self.declarations),
        }
    }

    fn pos(&self, room: RoomName, o: &Obj) -> Pos {
        Pos::new(room, num(o, atom!("x")) as i32, num(o, atom!("y")) as i32)
    }

    /// `store.getCapacity()`, else its energy's: the one capacity `read.js` takes.
    fn store(o: &Obj) -> Store {
        let held = (obj(o, atom!("store")).into_iter())
            .flat_map(Obj::iter)
            .filter_map(|(k, v)| Some((k.as_str(), v.as_f64()? as i32)))
            .filter(|(_, amount)| *amount > 0)
            .filter_map(|(name, amount)| Some((name.parse().ok()?, amount)))
            .collect();
        let by_resource =
            obj(o, atom!("storeCapacityResource")).map_or(0.0, |c| num(c, atom!("energy")));
        let whole = num(o, atom!("storeCapacity"));
        Store { held, capacity: (if whole > 0.0 { whole } else { by_resource }) as i32 }
    }

    fn body(o: &Obj) -> Vec<BodyPart> {
        let parts = match o.get(atom!("body")) {
            Some(Val::Arr(parts)) => parts.as_slice(),
            _ => &[],
        };
        (parts.iter())
            .filter_map(Val::as_obj)
            .map(|p| BodyPart {
                kind: text(p, atom!("type")).parse().unwrap_or(Part::Tough),
                hits: num(p, atom!("hits")) as i32,
                boost: text(p, atom!("boost")).parse().ok(),
            })
            .collect()
    }

    fn creep(&self, room: RoomName, o: &Obj) -> Creep {
        Creep {
            id: Id::of_engine(text(o, atom!("_id"))),
            name: text(o, atom!("name")).to_string(),
            owner: self.owner(o),
            pos: self.pos(room, o),
            body: Reader::body(o),
            hits: num(o, atom!("hits")) as i32,
            hits_max: num(o, atom!("hitsMax")) as i32,
            ticks_to_live: num(o, atom!("ageTime")) as i32 - self.time,
            fatigue: num(o, atom!("fatigue")) as i32,
            store: Reader::store(o),
        }
        .clocked()
    }

    fn controller(&self, room: RoomName, o: &Obj) -> Controller {
        let level = num(o, atom!("level")) as i32;
        let downgrade = match o.get(atom!("downgradeTime")) {
            Some(Val::Num(t)) => *t as i32 - self.time,
            _ => controller_downgrade(level as u8).unwrap_or(0) as i32,
        };
        Controller {
            id: Id::of_engine(text(o, atom!("_id"))),
            pos: self.pos(room, o),
            owner: self.owner(o),
            level,
            progress: num(o, atom!("progress")) as i32,
            progress_total: controller_levels(level as u32).unwrap_or(0) as i32,
            ticks_to_downgrade: downgrade,
            reservation: obj(o, atom!("reservation"))
                .map(|r| (self.owner(r), self.left(r, atom!("endTime")))),
            sign: obj(o, atom!("sign")).map(|s| (self.owner(s), text(s, atom!("text")).into())),
            safe_mode: self.left(o, atom!("safeMode")),
            safe_mode_available: num(o, atom!("safeModeAvailable")) as i32,
            safe_mode_cooldown: self.left(o, atom!("safeModeCooldown")),
            upgrade_blocked: self.left(o, atom!("upgradeBlocked")),
        }
    }

    fn structure(&self, room: RoomName, kind: StructureType, o: &Obj) -> Structure {
        let owner = self.owner(o);
        let cooldown = match o.get(atom!("cooldown")) {
            Some(Val::Num(c)) => *c as i32,
            _ => self.left(o, atom!("cooldownTime")),
        };
        let public = matches!(o.get(atom!("isPublic")), Some(Val::Bool(true)));
        let core = kind == StructureType::InvaderCore;
        let deploy = (num(o, atom!("deployTime")) as i32 - self.time).max(0);
        let collapse = crate::npc::collapse(o).map(|end| end as i32 - self.time);
        let (collapse, deploy) = fabot_base::engine::core_clock(collapse, deploy);
        Structure {
            id: Id::of_engine(text(o, atom!("_id"))),
            pos: self.pos(room, o),
            hits: num(o, atom!("hits")) as i32,
            hits_max: num(o, atom!("hitsMax")) as i32,
            store: if kind.has_store() { Reader::store(o) } else { Store::default() },
            cooldown: if kind.has_cooldown() { cooldown } else { 0 },
            collapse: if core { collapse } else { 0 },
            deploy: if core { deploy } else { 0 },
            level: if core { num(o, atom!("level")) as i32 } else { 0 },
            spawn: (kind == StructureType::KeeperLair)
                .then(|| num(o, atom!("nextSpawnTime")) as i32 - self.time)
                .filter(|t| *t >= 0),
            walkable: fabot_base::world::walkable(kind, &owner, public),
            owner,
            kind,
        }
    }

    /// A tombstone keeps our creep's body and life left, another's its id; a ruin none.
    fn remains(&self, room: RoomName, o: &Obj, tombstone: bool) -> Remains {
        let fallen = (tombstone && self.owner(o).is_ours()).then(|| Fallen {
            body: match o.get(atom!("creepBody")) {
                Some(Val::Arr(parts)) => (parts.iter().filter_map(Val::as_str))
                    .map(|t| t.parse().unwrap_or(Part::Tough))
                    .collect(),
                _ => Vec::new(),
            },
            ticks_to_live: num(o, atom!("creepTicksToLive")) as i32,
            died: num(o, atom!("deathTime")) as i32,
        });
        Remains {
            id: Id::of_engine(text(o, atom!("_id"))),
            pos: self.pos(room, o),
            store: Reader::store(o),
            theirs: (tombstone && fallen.is_none())
                .then(|| Id::of_engine(text(o, atom!("creepId")))),
            fallen,
        }
    }
}

/// The World `decide` reads, as `read.js` and `app/world.rs` hand it over.
pub fn world(v: &View, declarations: &Declarations) -> World<'static> {
    let k = v.k;
    let time = k.time as i32;
    let me = username(k, v.us);
    let r = Reader { time, declarations, k, me };
    let us = our_idx(v);
    let mut world = World {
        time,
        me: me.to_string(),
        cpu_limit: 100.0,
        bucket: CPU_BUCKET_MAX as i32,
        rooms: Vec::new(),
        spawns: Vec::new(),
        creeps: Vec::new(),
        our_sites: 0,
        rooms_allowed: rooms_allowed(v),
        closed: BTreeSet::new(),
        attacks: Vec::new(),
        destroyed: Vec::new(),
        rules: Rules::vanilla(),
    };
    for room in &v.rooms {
        let sites = room.of("constructionSite");
        world.our_sites += sites.filter(|o| v.ours(o)).count() as i32;
    }
    let observed: BTreeSet<&str> = (k.rooms.iter().flat_map(|r| r.presence.iter()))
        .filter(|(u, _)| *u == us)
        .filter_map(|(_, p)| Some(&*p.observe.as_ref()?.1))
        .collect();
    let sees = |i: &usize| {
        (k.rooms[*i].presence.iter()).any(|(u, p)| *u == us && p.vision > 0)
            || observed.contains(v.rooms[*i].name)
    };
    let mut seen: Vec<usize> = (0..v.rooms.len()).filter(sees).collect();
    seen.sort_by_key(|i| v.rooms[*i].name);
    for i in seen {
        let at = &v.rooms[i];
        let name = at.name.parse().unwrap_or(RoomName::SIM);
        let mut room = Room::empty(name);
        let mut ovens: Vec<(&Obj, Spawn)> = Vec::new();
        for &(kind, o) in &at.objects {
            match kind {
                "controller" => {
                    room.controller = Some(r.controller(name, o));
                    room.structures.push(r.structure(name, StructureType::Controller, o));
                }
                "source" => room.sources.push(Source {
                    id: Id::of_engine(text(o, atom!("_id"))),
                    pos: r.pos(name, o),
                    energy: num(o, atom!("energy")) as i32,
                    energy_capacity: num(o, atom!("energyCapacity")) as i32,
                    ticks_to_regeneration: r.left(o, atom!("nextRegenerationTime")),
                }),
                "mineral" => room.minerals.push(Mineral {
                    id: Id::of_engine(text(o, atom!("_id"))),
                    pos: r.pos(name, o),
                    resource: text(o, atom!("mineralType")).parse().ok(),
                    amount: num(o, atom!("mineralAmount")) as i32,
                }),
                "constructionSite" => {
                    room.sites.extend(text(o, atom!("structureType")).parse().map(|kind| Site {
                        kind,
                        id: Id::of_engine(text(o, atom!("_id"))),
                        pos: r.pos(name, o),
                        owner: r.owner(o),
                        progress: num(o, atom!("progress")) as i32,
                        progress_total: num(o, atom!("progressTotal")) as i32,
                    }))
                }
                "energy" => {
                    room.piles.extend(text(o, atom!("resourceType")).parse().map(|resource| Pile {
                        id: Id::of_engine(text(o, atom!("_id"))),
                        pos: r.pos(name, o),
                        resource,
                        amount: num(o, Atom::intern(text(o, atom!("resourceType")))) as i32,
                    }))
                }
                "tombstone" => room.remains.push(r.remains(name, o, true)),
                "ruin" => room.remains.push(r.remains(name, o, false)),
                "creep" if o.get(atom!("spawning")) == Some(&Val::Bool(true)) => {}
                "creep" if v.ours(o) => world.creeps.push(r.creep(name, o)),
                "creep" => room.others.push(r.creep(name, o)),
                "flag" | "powerCreep" | "nuke" => {}
                kind => {
                    if v.ours(o) && matches!(kind, "spawn" | "extension") && view::on(o) {
                        let store = Reader::store(o);
                        room.energy_available += store.amount_of(vocabulary::ResourceType::Energy);
                        room.energy_capacity += store.capacity;
                    }
                    if v.ours(o) && kind == "spawn" {
                        let spawn = Spawn {
                            id: Id::of_engine(text(o, atom!("_id"))),
                            name: text(o, atom!("name")).to_string(),
                            pos: r.pos(name, o),
                            spawning: None,
                        };
                        ovens.push((o, spawn));
                    }
                    match kind.parse() {
                        Ok(kind) => room.structures.push(r.structure(name, kind, o)),
                        Err(_) => room.blocked.push(r.pos(name, o)),
                    }
                }
            }
        }
        for (o, mut spawn) in ovens {
            if let Some(oven) = obj(o, atom!("spawning")) {
                let named = text(oven, atom!("name"));
                let creep = at.of("creep").find(|c| text(c, atom!("name")) == named && v.ours(c));
                spawn.spawning = Some(Oven {
                    name: named.to_string(),
                    remaining: r.left(oven, atom!("spawnTime")),
                    body: creep.map_or_else(Vec::new, Reader::body),
                });
            }
            world.spawns.push(spawn);
        }
        let held = (room.controller.as_ref()).is_some_and(|c| {
            c.owner.is_ours() || c.reservation.as_ref().is_some_and(|r| r.0.is_ours())
        });
        let logged = held || world.creeps.iter().any(|c| c.pos.room == name);
        for e in at.events().filter(|_| logged) {
            if e.kind == EVENT_ATTACK {
                let target = e.data["targetId"].as_str().unwrap_or_default();
                world.attacks.push((Id::of_engine(e.by), Id::of_engine(target)));
            } else if e.kind == EVENT_OBJECT_DESTROYED {
                let kind = e.data["type"].as_str().unwrap_or_default();
                if kind != "creep" {
                    world.destroyed.push((name, Id::of_engine(e.by), build_cost(kind)));
                }
            }
        }
        world.rooms.push(room);
    }
    world.spawns.sort_by(|a, b| a.name.cmp(&b.name));
    world.creeps.sort_by(|a, b| a.name.cmp(&b.name));
    world
}
