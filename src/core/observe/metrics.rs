//! What the bot measures of itself (`ARCHITECTURE.md` §10.5), one definition
//! for live and the soak. Write-only: no decision reads it.
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fmt::Write;

use crate::core::HashMap;
use crate::core::economy::bodies::{self, Pattern};
use crate::core::engine;
use crate::core::fight::shape::Engagement;
use crate::core::intents::{CreepAct, Intent, Refusal, SpawnAct, TowerAct};
use crate::core::tick;
use crate::core::vocabulary::{BodyPart, Resource};
use crate::core::world::{Creep, Room, RoomName, World};

pub const WINDOW: i32 = 100;
const RING: usize = 150;
/// The engine's limit on one segment, less a margin.
const BYTES: usize = 99 * 1024;

const STOCK: &str = "stock";
const FLOOR: &str = "floor";
const BUILD: &str = "spend.build";
const REPAIR: &str = "spend.repair";
const UPGRADE: &str = "spend.upgrade";
const SPAWN: &str = "spend.spawn";
const TOWER: &str = "spend.tower";
const PROGRESS: &str = "progress";
const STALE: &str = "site.stale";
const SPAWN_TICKS: &str = "spawn.ticks";
const SPAWNING: &str = "spawn.util";
const IDLE_WANT: &str = "spawn.idleWant";
const SHORT: &str = "short.";
const FLIPS: &str = "fight.flips";
const BODIES: &str = "bodies";
const BODIES_OF: &str = "bodies.";
const IDLE_OF: &str = "idle.";
const CHURN: &str = "churn.task";
const REFUSED: &str = "refuse.";
const CPU: &str = "cpu.";
const BUCKET: &str = "bucket";

#[derive(Clone, Copy)]
enum Fold {
    Sum,
    Last,
    Mean,
    Share(&'static str),
    /// 100 × the sum over the window's mean economy bodies.
    PerBody,
}

struct Metric {
    name: &'static str,
    fold: Fold,
    means: &'static str,
}

const fn metric(name: &'static str, fold: Fold, means: &'static str) -> Metric {
    Metric { name, fold, means }
}

/// A row is a room's, or the world's for the bodies: the economy is one pool
/// (§5), so a body is no colony's. A name ending in `.` is a family.
const METRICS: &[Metric] = &[
    metric(
        STOCK,
        Fold::Last,
        "energy in storages, terminals, containers and our bodies, in a room we own or reserve",
    ),
    metric(
        FLOOR,
        Fold::Last,
        "any resource in piles, tombstones and ruins, in a room we own or reserve",
    ),
    metric(
        BUILD,
        Fold::Sum,
        "accepted build intents: WORK × 5, capped by the site's remainder and the energy held or fed",
    ),
    metric(
        REPAIR,
        Fold::Sum,
        "accepted repair intents: WORK × 1, capped by hits missing / 100 and the energy held or fed",
    ),
    metric(
        UPGRADE,
        Fold::Sum,
        "accepted upgrade intents: WORK × 1, capped by the energy held or fed",
    ),
    metric(SPAWN, Fold::Sum, "accepted spawn intents: the body's cost"),
    metric(TOWER, Fold::Sum, "accepted tower intents: 10 each"),
    metric(
        PROGRESS,
        Fold::Sum,
        "controller progress gained between window ends; none at RCL8",
    ),
    metric(
        STALE,
        Fold::Last,
        "ticks since the stalest site last progressed, read at window ends",
    ),
    metric(SPAWN_TICKS, Fold::Sum, "spawn-ticks"),
    metric(
        SPAWNING,
        Fold::Share(SPAWN_TICKS),
        "% of spawn-ticks spawning",
    ),
    metric(
        IDLE_WANT,
        Fold::Sum,
        "spawn-ticks with no spawn intent, the bank full and the colony owed a body",
    ),
    metric(
        SHORT,
        Fold::Mean,
        "the colony's seats of the pattern the quota left uncast",
    ),
    metric(
        FLIPS,
        Fold::Sum,
        "engagements opened in a room within a window of one closing there",
    ),
    metric(BODIES, Fold::Mean, "economy bodies"),
    metric(
        BODIES_OF,
        Fold::Mean,
        "economy bodies of the pattern its name casts",
    ),
    metric(
        IDLE_OF,
        Fold::Share(BODIES_OF),
        "% of the pattern's body-ticks with no task or no intent",
    ),
    metric(
        CHURN,
        Fold::PerBody,
        "task changes on a tick the body's load held, per 100 economy bodies",
    ),
    metric(
        REFUSED,
        Fold::Sum,
        "the executor's refusals, by act and code",
    ),
    metric(CPU, Fold::Mean, "a phase's CPU per tick, in hundredths"),
    metric(BUCKET, Fold::Last, "the CPU bucket"),
];

#[derive(Clone, Copy)]
enum Bound {
    Max(i32),
    Min(i32),
}

const THRESHOLDS: &[(&str, Bound)] = &[
    ("idle.harvester", Bound::Max(75)),
    ("idle.hauler", Bound::Max(50)),
    ("idle.worker", Bound::Max(50)),
    ("idle.upgrader", Bound::Max(30)),
    (CHURN, Bound::Max(100)),
    (IDLE_WANT, Bound::Max(50)),
    (STALE, Bound::Max(3000)),
    (FLIPS, Bound::Max(1)),
    (REFUSED, Bound::Max(50)),
    (BUCKET, Bound::Min(1000)),
];

fn names(family: &str, name: &str) -> bool {
    family == name || (family.ends_with('.') && name.starts_with(family))
}

fn rank(name: &str) -> Option<usize> {
    METRICS.iter().position(|m| names(m.name, name))
}

type Row = Option<RoomName>;
type Sums = BTreeMap<Row, BTreeMap<String, f64>>;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Window {
    /// On a multiple of `WINDOW`; fewer `ticks` than it is a window the bot
    /// did not see whole.
    pub start: i32,
    pub ticks: i32,
    pub rows: BTreeMap<Row, BTreeMap<String, i32>>,
}

#[derive(Clone, Copy, Debug, Default)]
struct Pace {
    bodies: f64,
    idle: f64,
}

#[derive(Clone, Debug)]
struct Held {
    task: String,
    load: i32,
}

#[derive(Clone, Copy, Debug)]
struct Stale {
    progress: i32,
    since: i32,
}

#[derive(Clone, Copy, Debug)]
struct Upgraded {
    level: i32,
    progress: i32,
    total: i32,
}

/// `ARCHITECTURE.md` §4's `metrics`.
#[derive(Clone, Debug, Default)]
pub struct Metrics {
    start: i32,
    ticks: i32,
    sums: Sums,
    /// Kept apart from `sums` as they are added every tick, merged at the close.
    paces: BTreeMap<Pattern, Pace>,
    cpu: Vec<(String, f64)>,
    pub ring: VecDeque<Window>,
    tasks: HashMap<String, Held>,
    sites: BTreeMap<String, Stale>,
    controllers: BTreeMap<RoomName, Upgraded>,
    engaged: BTreeMap<i32, RoomName>,
    closed: BTreeMap<RoomName, i32>,
}

pub struct Seen<'a> {
    pub world: &'a World,
    pub assignments: &'a BTreeMap<String, String>,
    pub engagements: &'a [Engagement],
    pub intents: &'a [Intent],
    pub refusals: &'a [Refusal],
    pub phases: &'a [(String, f64)],
}

fn add(sums: &mut Sums, row: Row, name: &str, v: f64) {
    let names = sums.entry(row).or_default();
    match names.get_mut(name) {
        Some(sum) => *sum += v,
        None => {
            names.insert(name.to_string(), v);
        }
    }
}

fn ours(room: &Room) -> bool {
    room.controller.as_ref().is_some_and(|c| {
        c.owner.is_ours() || c.reservation.as_ref().is_some_and(|(o, _)| o.is_ours())
    })
}

fn energy(creep: &Creep) -> i32 {
    creep.store.amount_of(&Resource::Energy)
}

/// The energy each working body is fed this tick: the engine applies a
/// transfer, withdraw or pickup before the work, so it is spent too.
fn fed<'a>(creeps: &HashMap<&str, &'a Creep>, intents: &'a [Intent]) -> HashMap<&'a str, i32> {
    let mut by_id: HashMap<&str, &Creep> = HashMap::default();
    for intent in intents {
        if let Intent::Creep(name, CreepAct::Build(_) | CreepAct::Repair(_) | CreepAct::Upgrade(_)) =
            intent
            && let Some(c) = creeps.get(name.as_str())
        {
            by_id.insert(c.id.as_str(), c);
        }
    }
    let mut out: HashMap<&str, i32> = HashMap::default();
    if by_id.is_empty() {
        return out;
    }
    for intent in intents {
        let Intent::Creep(name, act) = intent else {
            continue;
        };
        let Some(c) = creeps.get(name.as_str()) else {
            continue;
        };
        match act {
            CreepAct::Transfer(id, Resource::Energy) => {
                if let Some(to) = by_id.get(id.as_str()) {
                    *out.entry(to.name.as_str()).or_default() += energy(c).min(to.store.free());
                }
            }
            CreepAct::Withdraw(_, Resource::Energy) | CreepAct::Pickup(_) => {
                *out.entry(name.as_str()).or_default() += c.store.free();
            }
            _ => (),
        }
    }
    out
}

/// What one accepted act spends and where. A body builds and repairs within
/// its room, so its target is looked for there alone.
fn spent(
    world: &World,
    creeps: &HashMap<&str, &Creep>,
    fed: &HashMap<&str, i32>,
    intent: &Intent,
) -> Option<(RoomName, &'static str, i32)> {
    match intent {
        Intent::Creep(name, act) => {
            let c = creeps.get(name.as_str())?;
            let works = c.live(BodyPart::Work);
            let room = || world.room(c.pos.room);
            let (what, most) = match act {
                CreepAct::Build(id) => {
                    let s = room()?.sites.iter().find(|s| &s.id == id)?;
                    (
                        BUILD,
                        (works * engine::BUILD_POWER).min(s.progress_total - s.progress),
                    )
                }
                CreepAct::Repair(id) => {
                    let s = room()?.structures.iter().find(|s| &s.id == id)?;
                    let per = engine::REPAIR_HITS_PER_ENERGY;
                    (REPAIR, works.min((s.hits_max - s.hits + per - 1) / per))
                }
                CreepAct::Upgrade(_) => (UPGRADE, works),
                _ => return None,
            };
            let held = energy(c) + fed.get(name.as_str()).copied().unwrap_or(0);
            Some((c.pos.room, what, most.min(held)))
        }
        Intent::Spawn(id, SpawnAct::SpawnCreep(body, _)) => {
            let s = world.spawns.iter().find(|s| &s.id == id)?;
            Some((s.pos.room, SPAWN, engine::body_cost(body)))
        }
        Intent::Tower(id, TowerAct::Attack(_) | TowerAct::Heal(_)) => {
            let r = (world.rooms.iter()).find(|r| r.structures.iter().any(|s| &s.id == id))?;
            Some((r.name, TOWER, engine::TOWER_ENERGY_COST))
        }
        _ => None,
    }
}

/// The actor and the executor's verb for an act that spends, as a refusal names them.
fn verb(intent: &Intent) -> Option<(&str, &'static str)> {
    Some(match intent {
        Intent::Creep(name, CreepAct::Build(_)) => (name, "build"),
        Intent::Creep(name, CreepAct::Repair(_)) => (name, "repair"),
        Intent::Creep(name, CreepAct::Upgrade(_)) => (name, "upgrade"),
        Intent::Spawn(id, _) => (id, "spawn"),
        Intent::Tower(id, TowerAct::Attack(_)) => (id, "attack"),
        Intent::Tower(id, TowerAct::Heal(_)) => (id, "heal"),
        _ => return None,
    })
}

pub fn step(mut m: Metrics, seen: &Seen) -> Metrics {
    let world = seen.world;
    let start = world.time - world.time.rem_euclid(WINDOW);
    if m.ticks > 0 && start != m.start {
        m.close(world);
    }
    if m.ticks == 0 {
        m.start = start;
    }
    m.ticks += 1;
    let sums = &mut m.sums;

    let refused: BTreeSet<(&str, &str)> = (seen.refusals.iter())
        .map(|r| (r.actor.as_str(), r.act.as_str()))
        .collect();
    let creeps: HashMap<&str, &Creep> = (world.creeps.iter())
        .map(|c| (c.name.as_str(), c))
        .collect();
    let fed = fed(&creeps, seen.intents);
    let mut acted: Vec<&str> = Vec::new();
    let mut spawned: BTreeSet<&str> = BTreeSet::new();
    let mut owed: BTreeSet<RoomName> = BTreeSet::new();
    for intent in seen.intents {
        match intent {
            Intent::Creep(name, _) => acted.push(name.as_str()),
            Intent::Spawn(id, _) => {
                spawned.insert(id.as_str());
            }
            Intent::Short(room, pattern, n) => {
                let name = format!("{SHORT}{}", bodies::name_of(*pattern));
                add(sums, Some(*room), &name, f64::from(*n));
                owed.insert(*room);
            }
            _ => (),
        }
        if let Some(act) = verb(intent)
            && !refused.contains(&act)
            && let Some((room, what, n)) = spent(world, &creeps, &fed, intent)
        {
            add(sums, Some(room), what, f64::from(n));
        }
    }

    acted.sort_unstable();
    for c in world.creeps.iter().filter(|c| !tick::fights(&c.name)) {
        let Some(p) = bodies::pattern_of_name(&c.name) else {
            continue;
        };
        let pace = m.paces.entry(p).or_default();
        pace.bodies += 1.0;
        let task = seen.assignments.get(&c.name);
        if task.is_none() || acted.binary_search(&c.name.as_str()).is_err() {
            pace.idle += 1.0;
        }
        let load = c.store.used();
        match (task, m.tasks.get_mut(&c.name)) {
            (Some(now), Some(held)) => {
                if *now != held.task {
                    if held.load == load {
                        add(sums, None, CHURN, 1.0);
                    }
                    now.clone_into(&mut held.task);
                }
                held.load = load;
            }
            (Some(now), None) => {
                let held = Held {
                    task: now.clone(),
                    load,
                };
                m.tasks.insert(c.name.clone(), held);
            }
            _ => (),
        }
    }
    m.tasks
        .retain(|name, _| seen.assignments.contains_key(name));

    for s in &world.spawns {
        let row = Some(s.pos.room);
        add(sums, row, SPAWN_TICKS, 1.0);
        // Below a full bank the spawn waits on energy, which the quota casts to.
        let full = world
            .room(s.pos.room)
            .is_some_and(|r| r.energy_available >= r.energy_capacity);
        if s.spawning.is_some() {
            add(sums, row, SPAWNING, 1.0);
        } else if full && owed.contains(&s.pos.room) && !spawned.contains(s.id.as_str()) {
            add(sums, row, IDLE_WANT, 1.0);
        }
    }

    let now: BTreeMap<i32, RoomName> = (seen.engagements.iter()).map(|e| (e.id, e.room)).collect();
    for (id, room) in &m.engaged {
        if !now.contains_key(id) {
            m.closed.insert(*room, world.time);
        }
    }
    for (id, room) in &now {
        if !m.engaged.contains_key(id)
            && m.closed.get(room).is_some_and(|t| world.time - t < WINDOW)
        {
            add(sums, Some(*room), FLIPS, 1.0);
        }
    }
    m.engaged = now;
    m.closed.retain(|_, t| world.time - *t < WINDOW);

    for r in seen.refusals {
        add(sums, None, &format!("{REFUSED}{}.{}", r.act, r.code), 1.0);
    }
    for (i, (phase, ms)) in seen.phases.iter().enumerate() {
        let at = match m.cpu.get(i) {
            Some((p, _)) if p == phase => Some(i),
            _ => m.cpu.iter().position(|(p, _)| p == phase),
        };
        match at {
            Some(at) => m.cpu[at].1 += 100.0 * ms,
            None => m.cpu.push((phase.clone(), 100.0 * ms)),
        }
    }
    m
}

impl Metrics {
    /// The `Last` metrics are read off this tick's world, the first past the window.
    fn close(&mut self, world: &World) {
        let time = world.time;
        let mut sums = std::mem::take(&mut self.sums);
        add(&mut sums, None, BUCKET, f64::from(world.bucket));
        for (p, pace) in std::mem::take(&mut self.paces) {
            add(&mut sums, None, BODIES, pace.bodies);
            add(
                &mut sums,
                None,
                &format!("{BODIES_OF}{}", bodies::name_of(p)),
                pace.bodies,
            );
            add(
                &mut sums,
                None,
                &format!("{IDLE_OF}{}", bodies::name_of(p)),
                pace.idle,
            );
        }
        for (phase, sum) in std::mem::take(&mut self.cpu) {
            add(&mut sums, None, &format!("{CPU}{phase}"), sum);
        }

        self.sites
            .retain(|id, _| (world.rooms.iter()).any(|r| r.sites.iter().any(|s| &s.id == id)));
        for r in &world.rooms {
            let row = Some(r.name);
            for s in r.sites.iter().filter(|s| s.owner.is_ours()) {
                let fresh = Stale {
                    progress: s.progress,
                    since: time,
                };
                let stale = self.sites.entry(s.id.clone()).or_insert(fresh);
                if stale.progress != s.progress {
                    *stale = fresh;
                }
                let ticks = f64::from(time - stale.since);
                let names = sums.entry(row).or_default();
                let at = names.entry(STALE.to_string()).or_insert(0.0);
                *at = at.max(ticks);
            }
            if let Some(c) = r.controller.as_ref().filter(|c| c.owner.is_ours()) {
                let now = Upgraded {
                    level: c.level,
                    progress: c.progress,
                    total: c.progress_total,
                };
                if let Some(was) = self.controllers.insert(r.name, now) {
                    let gained = if was.level == c.level {
                        c.progress - was.progress
                    } else {
                        was.total - was.progress + c.progress
                    };
                    add(&mut sums, row, PROGRESS, f64::from(gained.max(0)));
                }
            }
            if !ours(r) {
                continue;
            }
            let stored = (r.structures.iter())
                .filter(|s| s.kind.is_storage() || s.kind.is_terminal() || s.kind.is_container())
                .map(|s| s.store.amount_of(&Resource::Energy));
            let carried = (world.creeps.iter())
                .filter(|c| c.pos.room == r.name)
                .map(energy);
            add(
                &mut sums,
                row,
                STOCK,
                f64::from(stored.chain(carried).sum::<i32>()),
            );
            let piles = r.piles.iter().map(|p| p.amount);
            let remains = r.remains.iter().map(|x| x.store.used());
            add(
                &mut sums,
                row,
                FLOOR,
                f64::from(piles.chain(remains).sum::<i32>()),
            );
        }

        let ticks = f64::from(self.ticks);
        let window = Window {
            start: self.start,
            ticks: self.ticks,
            rows: (sums.iter())
                .map(|(row, names)| {
                    let fold = |name: &str, v: f64| match rank(name).map(|i| METRICS[i].fold) {
                        Some(Fold::Mean) => v / ticks,
                        Some(Fold::Share(of)) => {
                            let under = if of.ends_with('.') {
                                format!("{of}{}", &name[name.find('.').unwrap_or(0) + 1..])
                            } else {
                                of.to_string()
                            };
                            100.0 * v / names.get(&under).copied().unwrap_or(f64::INFINITY)
                        }
                        Some(Fold::PerBody) => {
                            let bodies = sums.get(&None).and_then(|w| w.get(BODIES));
                            100.0 * v * ticks / bodies.copied().unwrap_or(f64::INFINITY)
                        }
                        _ => v,
                    };
                    let values = (names.iter())
                        .map(|(name, v)| (name.clone(), fold(name, *v).round() as i32))
                        .collect();
                    (*row, values)
                })
                .collect(),
        };
        self.ring.push_back(window);
        if self.ring.len() > RING {
            self.ring.pop_front();
        }
        self.ticks = 0;
    }

    pub fn alerts(&self) -> Vec<String> {
        let Some(w) = self.ring.back().filter(|w| w.ticks == WINDOW) else {
            return Vec::new();
        };
        let mut out = Vec::new();
        for (row, values) in &w.rows {
            for (name, v) in values {
                let bound = THRESHOLDS.iter().find(|(t, _)| names(t, name));
                let past = match bound.map(|(_, b)| *b) {
                    Some(Bound::Max(most)) if *v > most => format!("> {most}"),
                    Some(Bound::Min(least)) if *v < least => format!("< {least}"),
                    _ => continue,
                };
                let at = row.map_or(String::new(), |r| format!(" in {r}"));
                let means = rank(name).map_or("", |i| METRICS[i].means);
                out.push(format!("{name}{at} {v} {past} (t{}): {means}", w.start));
            }
        }
        out
    }

    /// The names once, then each window's `@start ticks` and a line a row
    /// (a room, `*` the world's) of values in the names' order, 0 left empty;
    /// the oldest windows dropped past a segment's size.
    pub fn text(&mut self) -> String {
        loop {
            let text = to_text(&self.ring);
            if text.len() <= BYTES || self.ring.len() <= 1 {
                return text;
            }
            self.ring.pop_front();
        }
    }

    pub fn of_text(text: &str) -> Metrics {
        let mut lines = text.lines();
        let names: Vec<&str> = match lines.next().and_then(|l| l.strip_prefix("names ")) {
            Some(l) => l.split(' ').collect(),
            None => return Metrics::default(),
        };
        let mut ring: VecDeque<Window> = VecDeque::new();
        for line in lines {
            let (head, cells) = line.split_once(' ').unwrap_or((line, ""));
            if let Some(start) = head.strip_prefix('@') {
                if let (Ok(start), Ok(ticks)) = (start.parse(), cells.parse()) {
                    let rows = BTreeMap::new();
                    ring.push_back(Window { start, ticks, rows });
                }
                continue;
            }
            let values = (cells.split(',').zip(&names))
                .filter_map(|(v, name)| Some((name.to_string(), v.parse().ok()?)))
                .collect();
            if let Some(w) = ring.back_mut() {
                w.rows.insert(RoomName::parse(head), values);
            }
        }
        Metrics {
            ring,
            ..Metrics::default()
        }
    }
}

/// Names in the table's order, so a room's row, whose metrics come first,
/// ends early.
fn to_text(ring: &VecDeque<Window>) -> String {
    let mut names: Vec<&str> = (ring.iter())
        .flat_map(|w| w.rows.values().flat_map(|r| r.keys().map(String::as_str)))
        .collect::<BTreeSet<&str>>()
        .into_iter()
        .collect();
    names.sort_by_key(|n| (rank(n), *n));
    let mut out = format!("names {}\n", names.join(" "));
    for w in ring {
        let _ = writeln!(out, "@{} {}", w.start, w.ticks);
        for (row, values) in &w.rows {
            let cells: Vec<String> = (names.iter())
                .map(|n| {
                    (values.get(*n).filter(|v| **v != 0)).map_or(String::new(), ToString::to_string)
                })
                .collect();
            let row = row.map_or("*".to_string(), |r| r.to_string());
            let _ = writeln!(out, "{row} {}", cells.join(",").trim_end_matches(','));
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::tests::Dice;

    #[test]
    fn a_ring_reads_back_as_written() {
        let mut dice = Dice::new(3);
        let rooms = [
            None,
            Some(RoomName::named("W1N1")),
            Some(RoomName::named("E2S3")),
        ];
        let names = [
            STOCK,
            BUILD,
            "idle.hauler",
            "refuse.build.-9",
            "cpu.economy",
        ];
        let mut ring: VecDeque<Window> = VecDeque::new();
        for i in 0..20 {
            let mut w = Window {
                start: i * WINDOW,
                ticks: dice.between(1, WINDOW),
                rows: BTreeMap::new(),
            };
            for row in rooms {
                let mut values = BTreeMap::new();
                for name in names {
                    let v = dice.between(-5, 5000);
                    if dice.chance(2) && v != 0 {
                        values.insert(name.to_string(), v);
                    }
                }
                if dice.chance(2) {
                    w.rows.insert(row, values);
                }
            }
            if !w.rows.is_empty() {
                ring.push_back(w);
            }
        }
        let mut m = Metrics {
            ring: ring.clone(),
            ..Metrics::default()
        };
        assert_eq!(Metrics::of_text(&m.text()).ring, ring);
    }

    #[test]
    fn every_threshold_names_a_metric() {
        for (name, _) in THRESHOLDS {
            assert!(rank(name).is_some(), "{name}");
        }
    }
}
