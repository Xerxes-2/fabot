//! The scenarios' scripted rivals (`ARCHITECTURE.md` §10.2), ported from the xxscreeps rival's
//! `roles.js` (removed with the port): `harass` kites an outpost, `raid` fights (or with CLAIM
//! alone attacks the controller) where `stand` put its bodies, and `keep` does nothing, since
//! its level-1 room holds no tower and never the 600 energy its repairer cost.
use std::cell::Cell;

use fabot_base::engine;
use fabot_base::tiles;
use krebs::atom;
use krebs::constants::OBSTACLE_OBJECT_TYPES;
use krebs::doc::{Id, Obj, Val};
use krebs::intents::RoomIntents;
use krebs::world::World as Krebs;
use krebs_pathfinder::{Goal, PathFinder, Pos, RoomCost, RoomXY, SearchOpts};
use serde::Deserialize;
use serde_json::{Value, json};

use crate::act::{Stored, has_active};
use crate::view::{View, num, obj, store, text, xy};

/// A player beside ours: what its bodies do on the run's tick `t`, put through `o`.
pub trait Player {
    fn play(&self, v: &View, t: u64, o: &mut Orders);
}

#[derive(Deserialize, Clone, Copy)]
#[serde(rename_all = "lowercase")]
enum Role {
    Keep,
    Raid,
    Harass,
}

/// `scripts/sim/run.mjs`'s seat: the user it stood, its room, a scenario opponent's config and raid.
#[derive(Deserialize, Clone)]
#[serde(deny_unknown_fields)]
pub struct Rival {
    id: String,
    room: String,
    role: Role,
    target: Option<String>,
    #[serde(default)]
    first: u64,
    count: Option<usize>,
    body: Option<Vec<String>>,
    #[serde(default)]
    cross: bool,
    raid: Option<Raid>,
}

/// Bodies stood on the run's tick `first` (and each `every` after) in `room`, each part as
/// `{ type, hits, boost? }`.
#[derive(Deserialize, Clone)]
#[serde(deny_unknown_fields)]
struct Raid {
    first: u64,
    every: Option<u64>,
    room: Option<String>,
    #[serde(default)]
    inside: bool,
    bodies: Vec<Vec<Value>>,
}

const HOVER: i32 = 2;

#[derive(Clone, Copy)]
struct It<'a> {
    kind: &'a str,
    o: &'a Obj,
    room: usize,
}

impl It<'_> {
    fn id(&self) -> &str {
        text(self.o, atom!("_id"))
    }
    fn user(&self) -> &str {
        text(self.o, atom!("user"))
    }
    fn at(&self) -> (i32, i32) {
        (xy(self.o).0 as i32, xy(self.o).1 as i32)
    }
    fn range(&self, b: &It) -> i32 {
        let (a, b, room) = (self.at(), b.at(), self.room == b.room);
        if room {
            (a.0 - b.0).abs().max((a.1 - b.1).abs())
        } else {
            i32::MAX
        }
    }
    fn has(&self, part: &str) -> bool {
        has_active(self.o, part)
    }
    fn hurt(&self) -> bool {
        num(self.o, atom!("hits")) < num(self.o, atom!("hitsMax"))
    }
    fn armed(&self) -> bool {
        self.has("attack") || self.has("ranged_attack")
    }
}

fn all<'a>(v: &View<'a>, r: usize) -> impl Iterator<Item = It<'a>> {
    (v.rooms[r].objects.iter()).map(move |&(kind, o)| It { kind, o, room: r })
}

fn creeps<'a>(v: &View<'a>, r: usize) -> impl Iterator<Item = It<'a>> {
    all(v, r).filter(|o| o.kind == "creep")
}

fn closest<'a>(c: &It, xs: impl Iterator<Item = It<'a>>) -> Option<It<'a>> {
    xs.min_by_key(|o| c.range(o))
}

fn pos(v: &View, r: usize, (x, y): (i32, i32)) -> Pos {
    let room = RoomXY {
        x: v.k.meta.rooms[r].xy.x,
        y: v.k.meta.rooms[r].xy.y,
    };
    let (x, y) = (x as u8, y as u8);
    Pos { room, x, y }
}

/// `getDirectionTo` a tile beside `from`, across a border too.
pub fn direction(from: Pos, to: Pos) -> Option<usize> {
    let world = |p: Pos| {
        (
            p.room.x * 50 + i32::from(p.x),
            p.room.y * 50 + i32::from(p.y),
        )
    };
    let d = (world(to).0 - world(from).0, world(to).1 - world(from).1);
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
    compass.iter().position(|&x| x == d).map(|i| i + 1)
}

/// Where a path may lead: `closed`, never over a border; `crossed`, into the next room too;
/// `built`, within the room around the structures that block.
#[derive(PartialEq)]
enum Edges {
    Open,
    Crossed,
    Closed,
    Built,
}

/// Each room's costs where a search reads more than terrain, `x * 50 + y`.
type Costs = Vec<(RoomXY, Vec<Cell<u8>>)>;

/// One player's intents this tick, as the game API stores them.
pub struct Orders<'p> {
    me: &'p str,
    pf: &'p mut PathFinder,
    stored: Stored,
}

impl Orders<'_> {
    fn put(&mut self, o: &It, name: &str, value: Value) {
        self.stored.put(o.room, o.id(), name, value);
    }

    fn at(o: &It) -> Value {
        json!({ "id": o.id(), "x": o.at().0, "y": o.at().1 })
    }

    fn walk(&mut self, v: &View, c: &It, goals: &[Goal], (opts, costs): (SearchOpts, &Costs)) {
        let from = pos(v, c.room, c.at());
        let terrain = |at: RoomXY| {
            let r = (v.k.meta.rooms.iter()).position(|m| m.xy.x == at.x && m.xy.y == at.y)?;
            Some(v.k.terrain.raw(r as u32))
        };
        let cost = |at| match costs.iter().find(|(r, _)| *r == at) {
            Some((_, m)) => RoomCost::Matrix(m),
            None => RoomCost::Terrain,
        };
        let Ok(found) = self.pf.search(from, goals, &opts, terrain, cost) else {
            return;
        };
        let Some(direction) = found.path.first().and_then(|&next| direction(from, next)) else {
            return;
        };
        if num(c.o, atom!("fatigue")) == 0.0 && c.has("move") {
            self.put(c, "move", json!({ "direction": direction }));
        }
    }

    /// Terrain only, as roles.js searched, within the room unless `edges` says otherwise.
    fn step(&mut self, v: &View, c: &It, goals: &[(It, i32)], flee: bool, edges: Edges) {
        let goal = |(o, range): &(It, i32)| Goal {
            pos: pos(v, o.room, o.at()),
            range: *range as u32,
        };
        let goals: Vec<Goal> = goals.iter().map(goal).collect();
        let rooms = if edges == Edges::Crossed { 2 } else { 1 };
        let mut costs = Costs::new();
        if edges == Edges::Closed {
            let border = vec![Cell::new(0); 2500];
            for i in 0..50 {
                for (x, y) in [(i, 0), (i, 49), (0, i), (49, i)] {
                    border[x * 50 + y].set(255);
                }
            }
            costs.push((pos(v, c.room, (0, 0)).room, border));
        }
        if edges == Edges::Built {
            costs.push((pos(v, c.room, (0, 0)).room, self.blocked(v, c.room, false)));
        }
        let opts = SearchOpts::from_js(0, 0, None, 0, 0, rooms, flee);
        self.walk(v, c, &goals, (opts, &costs));
    }

    fn blocked(&self, v: &View, r: usize, creeps: bool) -> Vec<Cell<u8>> {
        let m = vec![Cell::new(0); 2500];
        for o in all(v, r) {
            let cell = &m[(o.at().0 * 50 + o.at().1) as usize];
            let foreign = o.kind == "rampart" && o.user() != self.me;
            if (creeps && o.kind == "creep") || OBSTACLE_OBJECT_TYPES.contains(&o.kind) || foreign {
                cell.set(255);
            } else if o.kind == "road" && cell.get() == 0 {
                cell.set(1);
            }
        }
        m
    }

    /// `creep.moveTo(new RoomPosition(25, 25, room))`: roads at 1, plain 2, swamp 10, and where the
    /// player sees, creeps and the structures that block impassable.
    fn toward(&mut self, v: &View, c: &It, room: &str) {
        let Some(to) = v.index(room) else { return };
        let seen = (0..v.rooms.len()).filter(|&r| all(v, r).any(|o| o.user() == self.me));
        let costs: Costs = seen
            .map(|r| (pos(v, r, (0, 0)).room, self.blocked(v, r, true)))
            .collect();
        let goal = Goal {
            pos: pos(v, to, (25, 25)),
            range: 0,
        };
        let opts = SearchOpts::from_js(2, 10, None, 0, 0, 16, false);
        self.walk(v, c, &[goal], (opts, &costs));
    }
}

/// The nearest foe, else the nearest hostile spawn, else any hostile structure but a controller.
fn target_of<'a>(v: &View<'a>, c: &It, foes: &[It<'a>], me: &str) -> Option<It<'a>> {
    let unbuilt = [
        "creep",
        "constructionSite",
        "tombstone",
        "ruin",
        "controller",
        "powerCreep",
    ];
    let theirs = |spawn: bool| {
        let owned = all(v, c.room).filter(|s| theirs(s, me));
        closest(
            c,
            owned.filter(|s| !unbuilt.contains(&s.kind) && (!spawn || s.kind == "spawn")),
        )
    };
    (closest(c, foes.iter().copied()).or_else(|| theirs(true))).or_else(|| theirs(false))
}

/// A gun kites a swing inside 2 and holds 3 off its target, closing around our structures: a
/// terrain-only approach walked into them and stalled.
fn gun(v: &View, o: &mut Orders, c: &It, foes: &[It], target: Option<It>) {
    if c.has("heal") && c.hurt() {
        o.put(c, "heal", Orders::at(c));
    }
    let Some(target) = target else { return };
    let range = c.range(&target);
    if foes.iter().filter(|f| c.range(f) <= 1).count() >= 2 {
        o.put(c, "rangedMassAttack", json!({}));
    } else if range <= 3 {
        o.put(c, "rangedAttack", json!({ "id": target.id() }));
    }
    let swings: Vec<_> = (foes.iter().filter(|f| f.has("attack") && c.range(f) <= 2))
        .map(|f| (*f, 3))
        .collect();
    if !swings.is_empty() {
        o.step(v, c, &swings, true, Edges::Open);
    } else if range > 3 {
        o.step(v, c, &[(target, 3)], false, Edges::Built);
    }
}

fn swing(v: &View, o: &mut Orders, c: &It, target: Option<It>) {
    match target {
        Some(t) if c.range(&t) <= 1 => o.put(c, "attack", Orders::at(&t)),
        Some(t) => o.step(v, c, &[(t, 1)], false, Edges::Open),
        None => {}
    }
}

fn healer(v: &View, o: &mut Orders, c: &It, allies: &[It]) {
    let ratio = |a: &&It| num(a.o, atom!("hits")) / num(a.o, atom!("hitsMax"));
    let patient = (allies.iter().filter(|a| a.hurt())).min_by(|a, b| ratio(a).total_cmp(&ratio(b)));
    match patient.map(|p| c.range(p)) {
        Some(r) if r <= 1 => o.put(c, "heal", Orders::at(patient.unwrap())),
        Some(r) if r <= 3 => o.put(c, "rangedHeal", json!({ "id": patient.unwrap().id() })),
        _ => {}
    }
    let armed = allies
        .iter()
        .copied()
        .filter(|a| a.id() != c.id() && a.armed());
    let lead = (patient.copied().filter(|p| p.id() != c.id())).or_else(|| closest(c, armed));
    if let Some(lead) = lead.filter(|l| c.range(l) > 1) {
        o.step(v, c, &[(lead, 1)], false, Edges::Open);
    }
}

fn theirs(s: &It, me: &str) -> bool {
    !s.user().is_empty() && s.user() != me
}

/// CLAIM strikes our controller each time its upgrade block lapses: the block also
/// refuses safe mode, which is how S11's W2N24 fell to bodies with no weapon.
fn unclaim(v: &View, o: &mut Orders, c: &It) {
    let reach = engine::MELEE_RANGE;
    match all(v, c.room).find(|s| s.kind == "controller" && theirs(s, o.me)) {
        Some(t) if c.range(&t) <= reach => o.put(c, "attackController", json!({ "id": t.id() })),
        Some(t) => o.step(v, c, &[(t, reach)], false, Edges::Built),
        None => {}
    }
}

impl Rival {
    /// With nothing to strike where it stands, a raid walks on to `target`.
    fn raid(&self, v: &View, o: &mut Orders) {
        for r in 0..v.rooms.len() {
            let (allies, foes): (Vec<It>, Vec<It>) = creeps(v, r).partition(|c| c.user() == o.me);
            for c in &allies {
                let target = target_of(v, c, &foes, o.me);
                match &self.target {
                    Some(t) if v.rooms[r].name != t && target.is_none() => o.toward(v, c, t),
                    _ if c.has("attack") => swing(v, o, c, target),
                    _ if c.has("ranged_attack") => gun(v, o, c, &foes, target),
                    _ if c.has("heal") => healer(v, o, c, &allies),
                    _ if c.has("claim") => unclaim(v, o, c),
                    _ => {}
                }
            }
        }
    }

    /// `count` bodies of `body` cast from `first` on and recast as they die.
    fn harass(&self, v: &View, t: u64, o: &mut Orders) {
        let rooms = || (0..v.rooms.len()).flat_map(|r| all(v, r));
        let mine: Vec<It> = rooms()
            .filter(|c| c.kind == "creep" && c.user() == o.me)
            .collect();
        let idle =
            |s: &It| s.kind == "spawn" && s.user() == o.me && obj(s.o, atom!("spawning")).is_none();
        let body = (self.body.clone()).unwrap_or_else(|| vec!["attack".into(), "move".into()]);
        let cost: u32 = (body.iter().filter_map(|p| p.parse::<screeps::Part>().ok()))
            .map(screeps::Part::cost)
            .sum();
        if let Some(s) = rooms().find(idle)
            && t >= self.first
            && mine.len() < self.count.unwrap_or(1)
        {
            let banks = all(v, s.room)
                .filter(|e| matches!(e.kind, "spawn" | "extension") && e.user() == o.me);
            if banks.map(|e| store(e.o, atom!("energy"))).sum::<f64>() >= f64::from(cost) {
                let name = format!("harass-{}", v.k.time);
                o.put(&s, "createCreep", json!({ "name": name, "body": body }));
            }
        }
        let born = |c: &&It| c.o.get(atom!("spawning")) != Some(&Val::Bool(true));
        for c in mine.iter().filter(born) {
            self.harasser(v, o, c);
        }
    }

    /// Hovers within HOVER of `target`'s source farthest from our armed bodies, strikes what stands
    /// beside it, and keeps out of any armed body's reach, giving up the source rather than the room
    /// unless `cross`.
    fn harasser(&self, v: &View, o: &mut Orders, c: &It) {
        let foes: Vec<It> = creeps(v, c.room).filter(|f| f.user() != o.me).collect();
        // Its weapon's range and a step each.
        let reach = |f: &It| if f.has("ranged_attack") { 3 } else { 1 } + 2;
        match closest(c, foes.iter().copied()) {
            Some(p) if c.range(&p) <= 1 && c.has("attack") => o.put(c, "attack", Orders::at(&p)),
            Some(p) if c.range(&p) <= 3 && c.has("ranged_attack") => {
                o.put(c, "rangedAttack", json!({ "id": p.id() }));
            }
            _ => {}
        }
        let near: Vec<_> = (foes.iter().filter(|f| f.armed() && c.range(f) <= reach(f)))
            .map(|f| (*f, reach(f) + 1))
            .collect();
        if !near.is_empty() {
            let edges = if self.cross {
                Edges::Crossed
            } else {
                Edges::Closed
            };
            return o.step(v, c, &near, true, edges);
        }
        let Some(target) = &self.target else { return };
        if v.rooms[c.room].name != target {
            return o.toward(v, c, target);
        }
        let safest =
            |s: &It| (foes.iter().filter(|f| f.armed())).fold(50, |m, f| m.min(s.range(f)));
        let mut sources: Vec<It> = all(v, c.room).filter(|s| s.kind == "source").collect();
        sources.sort_by_key(|s| (-safest(s), c.range(s)));
        let Some(&source) = sources.first() else {
            return;
        };
        let unarmed: Vec<_> = (foes
            .iter()
            .filter(|f| !f.armed() && f.range(&source) <= HOVER + 1))
        .map(|f| (*f, 1))
        .collect();
        if !unarmed.is_empty() {
            o.step(v, c, &unarmed, false, Edges::Open);
        } else if c.range(&source) > HOVER {
            o.step(v, c, &[(source, HOVER)], false, Edges::Open);
        }
    }
}

impl Player for Rival {
    fn play(&self, v: &View, t: u64, o: &mut Orders) {
        match self.role {
            Role::Keep => {}
            Role::Raid => self.raid(v, o),
            Role::Harass => self.harass(v, t, o),
        }
    }
}

/// The run's rivals and the search they share.
pub struct Rivals {
    players: Vec<Rival>,
    pf: PathFinder,
}

impl Rivals {
    pub fn new(players: Vec<Rival>) -> Self {
        let pf = PathFinder::new();
        Rivals { players, pf }
    }

    pub fn intents(&mut self, v: &View, t: u64) -> Vec<(usize, RoomIntents)> {
        let mut rooms = Vec::new();
        for p in &self.players {
            let (me, pf) = (p.id.as_str(), &mut self.pf);
            let mut o = Orders {
                me,
                pf,
                stored: Stored::default(),
            };
            p.play(v, t, &mut o);
            rooms.extend(
                o.stored
                    .rooms(v.k.user_ids[&Id::new(me).expect("a user id")]),
            );
        }
        rooms
    }

    /// Each body of a raid due at `t` on an exit tile of its room (else
    /// `home`) toward the rival's (every side when none leads there), the first on a random one and
    /// the next nearest it; one tile in when `inside`, as a body on an exit crosses.
    pub fn stand(&self, k: &mut Krebs, t: u64, home: &str, mut random: impl FnMut() -> f64) {
        let due = |r: &&Raid| {
            t == r.first
                || r.every
                    .is_some_and(|e| t > r.first && (t - r.first).is_multiple_of(e))
        };
        for p in &self.players {
            let Some(raid) = p.raid.as_ref().filter(due) else {
                continue;
            };
            let at = |name: &str| {
                (k.meta.rooms.iter())
                    .position(|m| &*m.name == name)
                    .expect(name)
            };
            let (r, theirs) = (at(raid.room.as_deref().unwrap_or(home)), at(&p.room));
            let (a, b) = (k.meta.rooms[r].xy, k.meta.rooms[theirs].xy);
            let open = |(x, y): (i32, i32)| k.terrain.raw(r as u32)[(y * 50 + x) as usize] & 1 == 0;
            let edge = |exit: i32| {
                (tiles::exit_tiles(exit).iter()).map(|t| (tiles::x_of(*t), tiles::y_of(*t)))
            };
            let led: Vec<i32> = (EXITS.into_iter())
                .filter(|e| edge(*e).any(open))
                .filter(|e| (a.x + tiles::dx(*e), a.y + tiles::dy(*e)) == (b.x, b.y))
                .collect();
            let sides = if led.is_empty() { EXITS.to_vec() } else { led };
            let mut tiles: Vec<(i32, i32)> = sides
                .into_iter()
                .flat_map(edge)
                .filter(|&p| open(p))
                .collect();
            let Some(&(ox, oy)) = tiles.get((random() * tiles.len() as f64) as usize) else {
                continue;
            };
            tiles.sort_by_key(|&(x, y)| (x - ox).abs().max((y - oy).abs()));
            let room = &*k.meta.rooms[r].name;
            let docs = (raid.bodies.iter().zip(tiles).enumerate()).map(|(i, (parts, (x, y)))| {
                let inner = (x.clamp(1, 48), y.clamp(1, 48));
                let (x, y) = if raid.inside && open(inner) { inner } else { (x, y) };
                let hits = parts.len() as i32 * engine::PART_HITS;
                json!({ "type": "creep", "user": p.id, "body": parts, "name": format!("raid-{}-{i}", k.time),
                    "hits": hits, "hitsMax": hits, "ageTime": k.time + engine::CREEP_LIFETIME as u64, "x": x, "y": y, "room": room,
                    "fatigue": 0, "store": {}, "storeCapacity": 0, "notifyWhenAttacked": true })
            });
            let docs: Vec<Value> = docs.collect();
            crate::npc::insert(k, r, docs);
        }
    }
}

/// As FIND_EXIT lists them.
const EXITS: [i32; 4] = [tiles::TOP, tiles::RIGHT, tiles::BOTTOM, tiles::LEFT];
