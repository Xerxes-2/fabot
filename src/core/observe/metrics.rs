//! What the bot measures of itself (`ARCHITECTURE.md` §10.5), one definition
//! for live and the soak. Write-only: no decision reads it.
use std::collections::{BTreeMap, BTreeSet, VecDeque};
use std::fmt::Write;

use crate::core::HashMap;
use crate::core::economy::bodies::{self, Pattern};
use crate::core::economy::slots;
use crate::core::engine;
use crate::core::fight::kinds::defend;
use crate::core::fight::shape::{Cast, Engagement, Phase};
use crate::core::intents::{CreepAct, Intent, Refusal, SpawnAct, TowerAct};
use crate::core::tick;
use crate::core::tick::{Declarations, Derived};
use crate::core::vocabulary::{BodyPart, Resource};
use crate::core::world::{Creep, Kind, Owner, Pos, Room, RoomName, World, hostiles};

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
const DEATH: &str = "death.";
const LOST: &str = "lost.structure";
const DWELL: &str = "hostile.dwell";
const EXPOSURE: &str = "exposure";
const ROOMS_HELD: &str = "held";
const LOSS: &str = "loss.";
const BODIES: &str = "bodies";
const BODIES_OF: &str = "bodies.";
const IDLE_OF: &str = "idle.";
const CHURN: &str = "churn.task";
const RETURNED: &str = "carry.returned";
const RING_FED: &str = "carry.ring";
const HELD: &str = "move.held";
const WEDGED: &str = "move.wedged";
const EDGE: &str = "move.edgeFlips";
const SWING: &str = "move.swings";
const IN_RANGE: &str = "shot.inRange";
const MISSED: &str = "shot.missed";
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
        "accepted build intents: WORK × 5, capped by what the tick's earlier builders left of the site and the energy held or fed",
    ),
    metric(
        REPAIR,
        Fold::Sum,
        "accepted repair intents: WORK × 1, capped by hits missing / 100 after the tick's earlier repairs and the energy held or fed",
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
        "ticks since the room's freshest site was first seen or last progressed: its building stalled, not queued; read at window ends",
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
        "spawn-ticks with no spawn of its room casting, the bank full and the colony owed a body: the bank is read before the tick's casts",
    ),
    metric(
        SHORT,
        Fold::Mean,
        "the colony's open slots of the pattern left uncast",
    ),
    metric(
        FLIPS,
        Fold::Sum,
        "engagements opened in a room within a window of one closing there",
    ),
    metric(
        DEATH,
        Fold::Sum,
        "our bodies dead with more than 5 ticks to live, by what last struck them within 5 ticks: a player's creep (player), a player's tower (tower), an Invader's or keeper's creep or tower (npc), or none seen while wounded or out of sight (unknown)",
    ),
    metric(
        LOST,
        Fold::Sum,
        "structures destroyed in a room we own or reserve on a tick a creep not ours struck them",
    ),
    metric(
        DWELL,
        Fold::Sum,
        "ticks an armed creep, neither an ally's nor a keeper, stood in a room we own or declare an outpost with no tower of ours able to fire and no engagement answering (none open, or one disengaging or left to towers and ramparts with no body of its own queued or spawning); its defenders' walk out is not dwell",
    ),
    metric(
        EXPOSURE,
        Fold::Sum,
        "colony-ticks with an armed creep, neither an ally's nor a keeper, in the colony or in a declared outpost no other colony is nearer, while the colony is below RCL3, has no tower holding a shot's energy, or has no safe mode on or able to fire",
    ),
    metric(
        ROOMS_HELD,
        Fold::Sum,
        "ticks a room is a colony of ours or an outpost we work",
    ),
    metric(
        LOSS,
        Fold::Sum,
        "energy lost: the body cost of the deaths death.* counts, by the same cause, and the build cost of the structures lost.structure counts (structure)",
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
        "% of the pattern's body-ticks with no task or no intent, a held step counting as one",
    ),
    metric(
        CHURN,
        Fold::PerBody,
        "task changes on a tick the body's load held, per 100 economy bodies",
    ),
    metric(
        RETURNED,
        Fold::Sum,
        "energy a body transferred into the store it last withdrew energy from, no transfer, build, repair or upgrade between: at most what it drew",
    ),
    metric(
        RING_FED,
        Fold::Sum,
        "energy carriers transferred into a spawn or an extension",
    ),
    metric(
        HELD,
        Fold::Share(BODIES),
        "% of economy body-ticks whose step the arbitration denied",
    ),
    metric(
        WEDGED,
        Fold::Sum,
        "economy bodies kept 50 ticks in a row, a tired tick neither breaking nor adding to the run",
    ),
    metric(
        EDGE,
        Fold::PerBody,
        "an economy body's returns to the room it left 5 ticks before or less, per 100 economy bodies",
    ),
    metric(
        SWING,
        Fold::PerBody,
        "an economy body back on the tile of two ticks before, the tile between in the same room, per 100 economy bodies a window: the A-B-A swing the scenarios' oscillation counts per 100 ticks of one fight body",
    ),
    metric(
        IN_RANGE,
        Fold::Sum,
        "armed body-ticks of ours with a creep, neither an ally's, a stranger's nor a keeper, within its longest weapon's range, not healing a wounded one of ours",
    ),
    metric(
        MISSED,
        Fold::Share(IN_RANGE),
        "% of those with no attack, ranged attack or mass attack asked",
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
    ("idle.harvester", Bound::Max(95)),
    ("idle.hauler", Bound::Max(50)),
    ("idle.worker", Bound::Max(85)),
    ("idle.upgrader", Bound::Max(30)),
    (CHURN, Bound::Max(50)),
    (RETURNED, Bound::Max(1800)),
    (IDLE_WANT, Bound::Max(0)),
    (STALE, Bound::Max(3000)),
    (FLIPS, Bound::Max(1)),
    (DEATH, Bound::Max(2)),
    (LOST, Bound::Max(0)),
    (DWELL, Bound::Max(20)),
    (EXPOSURE, Bound::Max(0)),
    (HELD, Bound::Max(3)),
    (WEDGED, Bound::Max(0)),
    (EDGE, Bound::Max(5)),
    (SWING, Bound::Max(25)),
    (MISSED, Bound::Max(20)),
    (REFUSED, Bound::Max(0)),
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
enum Cause {
    Player,
    Tower,
    Npc,
    Unknown,
}

impl Cause {
    fn name(self) -> &'static str {
        match self {
            Cause::Player => "player",
            Cause::Tower => "tower",
            Cause::Npc => "npc",
            Cause::Unknown => "unknown",
        }
    }
}

#[derive(Clone, Debug)]
struct Track {
    id: String,
    seen: i32,
    at: Pos,
    before: Option<Pos>,
    still: i32,
    left_at: Option<(RoomName, i32)>,
    ttl: i32,
    wounded: bool,
    struck: Option<(Cause, i32)>,
    cost: i32,
}

#[derive(Clone, Copy, Debug, Default)]
struct Asked {
    held: bool,
    strike: bool,
    heal: bool,
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
    tracks: HashMap<String, Track>,
    /// A body's last energy draw not yet delivered: the store and the amount.
    drawn: HashMap<String, (String, i32)>,
}

pub struct Seen<'a> {
    pub world: &'a World,
    pub assignments: &'a BTreeMap<String, String>,
    pub engagements: &'a [Engagement],
    pub casts: &'a [Cast],
    pub intents: &'a [Intent],
    pub refusals: &'a [Refusal],
    pub phases: &'a [(String, f64)],
    pub declarations: &'a Declarations,
    pub derived: &'a Derived,
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
/// its room, so its target is looked for there alone. The engine applies one
/// body's act before the next's, so bodies on one target share what is `undone`.
fn spent<'w>(
    world: &'w World,
    creeps: &HashMap<&str, &Creep>,
    fed: &HashMap<&str, i32>,
    undone: &mut HashMap<&'w str, i32>,
    intent: &Intent,
) -> Option<(RoomName, &'static str, i32)> {
    match intent {
        Intent::Creep(name, act) => {
            let c = creeps.get(name.as_str())?;
            let works = c.live(BodyPart::Work);
            let room = || world.room(c.pos.room);
            let held = energy(c) + fed.get(name.as_str()).copied().unwrap_or(0);
            let (what, (id, total), most) = match act {
                CreepAct::Build(id) => {
                    let s = room()?.sites.iter().find(|s| &s.id == id)?;
                    let target = (s.id.as_str(), s.progress_total - s.progress);
                    (BUILD, target, works * engine::BUILD_POWER)
                }
                CreepAct::Repair(id) => {
                    let s = room()?.structures.iter().find(|s| &s.id == id)?;
                    let per = engine::REPAIR_HITS_PER_ENERGY;
                    let target = (s.id.as_str(), (s.hits_max - s.hits + per - 1) / per);
                    (REPAIR, target, works)
                }
                CreepAct::Upgrade(_) => return Some((c.pos.room, UPGRADE, works.min(held))),
                _ => return None,
            };
            let left = undone.entry(id).or_insert(total);
            let n = most.min(held).min(*left);
            *left -= n;
            Some((c.pos.room, what, n))
        }
        Intent::Spawn(id, SpawnAct::SpawnCreep(body, ..)) => {
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
        Intent::Spawn(id, SpawnAct::SpawnCreep(..)) => (id, "spawn"),
        Intent::Tower(id, TowerAct::Attack(_)) => (id, "attack"),
        Intent::Tower(id, TowerAct::Heal(_)) => (id, "heal"),
        _ => return None,
    })
}

fn returned(
    sums: &mut Sums,
    drawn: &mut HashMap<String, (String, i32)>,
    creeps: &HashMap<&str, &Creep>,
    (intents, world): (&[Intent], &World),
    refused: &BTreeSet<(&str, &str)>,
) {
    for intent in intents {
        let Intent::Creep(name, act) = intent else {
            continue;
        };
        let Some(c) = creeps.get(name.as_str()) else {
            continue;
        };
        if let CreepAct::Transfer(id, Resource::Energy) = act
            && let Some(s) = (world.rooms.iter().flat_map(|r| r.structures.iter()))
                .find(|s| &s.id == id && (s.kind.is_spawn() || s.kind == Kind::Extension))
            && !refused.contains(&(name, "transfer"))
            && bodies::pattern_of_name(name) == Some(Pattern::Hauler)
        {
            add(
                sums,
                None,
                RING_FED,
                f64::from(energy(c).min(s.store.free())),
            );
        }
        match act {
            CreepAct::Withdraw(id, Resource::Energy) if !refused.contains(&(name, "withdraw")) => {
                drawn.insert(name.clone(), (id.clone(), c.store.free()));
            }
            CreepAct::Transfer(id, Resource::Energy) if !refused.contains(&(name, "transfer")) => {
                if let Some((from, n)) = drawn.remove(name.as_str())
                    && &from == id
                {
                    add(sums, None, RETURNED, f64::from(n.min(energy(c))));
                }
            }
            CreepAct::Transfer(_, Resource::Energy) => (),
            CreepAct::Transfer(..)
            | CreepAct::Build(_)
            | CreepAct::Repair(_)
            | CreepAct::Upgrade(_) => {
                drawn.remove(name.as_str());
            }
            _ => (),
        }
    }
    drawn.retain(|name, _| creeps.contains_key(name.as_str()));
}

fn task_of<'a>(assignments: &'a BTreeMap<String, String>, name: &str) -> Option<&'a str> {
    assignments.get(name).and_then(|held| slots::task_of(held))
}

/// Fewer ticks to live than this at its last sight, a body died of age.
const AGED: i32 = 5;
/// A body struck this many ticks or fewer before it died died of that strike.
const STRUCK_LATELY: i32 = 5;
const WEDGED_TICKS: i32 = 50;
const FLIP_TICKS: i32 = 5;

impl Track {
    fn of(c: &Creep, time: i32) -> Track {
        Track {
            id: c.id.clone(),
            seen: time,
            at: c.pos,
            before: None,
            still: 0,
            left_at: None,
            ttl: c.ticks_to_live,
            wounded: c.hits < c.hits_max,
            struck: None,
            cost: engine::body_cost(&c.body.iter().map(|p| p.kind).collect::<Vec<_>>()),
        }
    }

    fn moved(&mut self, now: Pos, time: i32, held: bool, tired: bool) -> Vec<&'static str> {
        let mut out = Vec::new();
        if self.seen != time - 1 {
            self.before = None;
            self.still = 0;
        }
        if held {
            out.push(HELD);
            self.still += 1;
            if self.still == WEDGED_TICKS {
                out.push(WEDGED);
            }
        } else if !tired {
            self.still = 0;
        }
        if now.room != self.at.room {
            if (self.left_at).is_some_and(|(room, at)| room == now.room && time - at <= FLIP_TICKS)
            {
                out.push(EDGE);
            }
            self.left_at = Some((self.at.room, time));
        }
        if self.before == Some(now) && self.at != now && self.at.room == now.room {
            out.push(SWING);
        }
        self.before = (self.seen == time - 1).then_some(self.at);
        self.at = now;
        self.seen = time;
        out
    }
}

/// An attacker gone the same tick, or out of sight, is unknown.
fn cause_of(room: Option<&Room>, by: &str) -> Cause {
    let npc = |o: &Owner| matches!(o, Owner::Invader | Owner::Keeper);
    let Some(room) = room else {
        return Cause::Unknown;
    };
    if let Some(c) = room.others.iter().find(|c| c.id == by) {
        return if npc(&c.owner) {
            Cause::Npc
        } else {
            Cause::Player
        };
    }
    match room
        .structures
        .iter()
        .find(|s| s.id == by && s.kind.is_tower())
    {
        Some(s) if npc(&s.owner) => Cause::Npc,
        Some(_) => Cause::Tower,
        None => Cause::Unknown,
    }
}

/// Fight bodies' steps are left to the scenarios (`oscillation`): a kite's step back is a swing that pays.
fn steps(
    sums: &mut Sums,
    tracks: &mut HashMap<String, Track>,
    world: &World,
    asked: &HashMap<&str, Asked>,
) {
    let time = world.time;
    for c in &world.creeps {
        let a = asked.get(c.name.as_str()).copied().unwrap_or_default();
        let Some(t) = tracks.get_mut(&c.name) else {
            tracks.insert(c.name.clone(), Track::of(c, time));
            continue;
        };
        let steps = t.moved(c.pos, time, a.held, c.fatigue > 0);
        if !tick::fights(&c.name) && bodies::pattern_of_name(&c.name).is_some() {
            for name in steps {
                add(sums, None, name, 1.0);
            }
        }
        t.ttl = c.ticks_to_live;
        t.wounded = c.hits < c.hits_max;
    }
}

fn shots(sums: &mut Sums, world: &World, asked: &HashMap<&str, Asked>) {
    for c in &world.creeps {
        let a = asked.get(c.name.as_str()).copied().unwrap_or_default();
        let reach = if c.live(BodyPart::RangedAttack) > 0 {
            engine::RANGED_RANGE
        } else if c.live(BodyPart::Attack) > 0 {
            engine::MELEE_RANGE
        } else {
            continue;
        };
        let foe = |h: &Creep| {
            !matches!(h.owner, Owner::Stranger(_) | Owner::Keeper) && c.pos.within(reach, &h.pos)
        };
        if !a.heal
            && world
                .room(c.pos.room)
                .is_some_and(|r| r.hostiles().any(foe))
        {
            add(sums, None, IN_RANGE, 1.0);
            if !a.strike {
                add(sums, None, MISSED, 1.0);
            }
        }
    }
}

/// The strike that killed a body is logged the tick after, in a room that
/// may have gone out of sight with it: the last strike seen stands for it.
fn deaths(sums: &mut Sums, tracks: &mut HashMap<String, Track>, world: &World) {
    let time = world.time;
    if !world.attacks.is_empty() {
        let ids: HashMap<&str, (&str, RoomName)> = (tracks.iter())
            .map(|(name, t)| (t.id.as_str(), (name.as_str(), t.at.room)))
            .collect();
        let struck: Vec<(String, Cause)> = (world.attacks.iter())
            .filter_map(|(by, at)| {
                let (name, room) = ids.get(at.as_str())?;
                Some((name.to_string(), cause_of(world.room(*room), by)))
            })
            .collect();
        for (name, cause) in struck {
            if let Some(t) = tracks.get_mut(&name) {
                let known = (t.struck).is_some_and(|(c, at)| {
                    !matches!(c, Cause::Unknown) && time - at <= STRUCK_LATELY
                });
                if !(known && matches!(cause, Cause::Unknown)) {
                    t.struck = Some((cause, time));
                }
            }
        }
    }

    tracks.retain(|_, t| {
        if t.seen == time {
            return true;
        }
        // Out of sight it may have died unwounded; we never recycle a body.
        let unseen = world.room(t.at.room).is_none();
        let cause = match t.struck {
            Some((cause, at)) if time - at <= STRUCK_LATELY => Some(cause),
            _ => (t.wounded || unseen).then_some(Cause::Unknown),
        };
        if t.ttl > AGED
            && let Some(cause) = cause
        {
            let row = Some(t.at.room);
            add(sums, row, &format!("{DEATH}{}", cause.name()), 1.0);
            add(
                sums,
                row,
                &format!("{LOSS}{}", cause.name()),
                f64::from(t.cost),
            );
        }
        false
    });
}

/// A body of ours that dismantles is no loss's cause.
fn lost(sums: &mut Sums, world: &World) {
    for (room, id, cost) in &world.destroyed {
        let struck = (world.attacks.iter())
            .any(|(by, at)| at == id && !world.creeps.iter().any(|c| &c.id == by));
        if struck && world.room(*room).is_some_and(ours) {
            add(sums, Some(*room), LOST, 1.0);
            add(
                sums,
                Some(*room),
                &format!("{LOSS}structure"),
                f64::from(*cost),
            );
        }
    }
}

fn armed(r: &Room) -> bool {
    (r.hostiles()).any(|h| h.owner != Owner::Keeper && hostiles::strikes(h))
}

fn towers(r: &Room) -> bool {
    (r.structures.iter()).any(|s| {
        s.kind.is_tower()
            && s.owner.is_ours()
            && s.store.amount_of(&Resource::Energy) >= engine::TOWER_ENERGY_COST
    })
}

fn dwell(sums: &mut Sums, seen: &Seen) {
    for r in &seen.world.rooms {
        let worked = r.controller.as_ref().is_some_and(|c| c.owner.is_ours())
            || (seen.declarations.outposts.iter()).any(|o| o.room == r.name);
        let (armed, towers) = (armed(r), towers(r));
        let answered = seen.engagements.iter().any(|e| {
            e.room == r.name
                && e.phase != Phase::Disengaging
                && (!e.unanswered || seen.casts.iter().any(|c| c.engagement == e.id))
        });
        if worked && armed && !towers && !answered {
            add(sums, Some(r.name), DWELL, 1.0);
        }
    }
}

fn reach(a: RoomName, b: RoomName) -> i32 {
    let ((ax, ay), (bx, by)) = (a.coords(), b.coords());
    (ax - bx).abs().max((ay - by).abs())
}

/// No colony owns an outpost, so exposure counts one against the nearest;
/// by room distance, as metrics has no Atlas to walk.
fn exposure(sums: &mut Sums, seen: &Seen) {
    let colonies = &seen.derived.colonies;
    let nearest = |room: RoomName| colonies.iter().copied().min_by_key(|c| reach(*c, room));
    for colony in colonies {
        let Some(r) = seen.world.room(*colony) else {
            continue;
        };
        let Some(c) = r.controller.as_ref() else {
            continue;
        };
        let safe = c.safe_mode > 0 || defend::fireable(c);
        if c.level >= 3 && towers(r) && safe {
            continue;
        }
        let mut outposts = (seen.declarations.outposts.iter())
            .filter(|o| nearest(o.room) == Some(*colony))
            .filter_map(|o| seen.world.room(o.room));
        if armed(r) || outposts.any(armed) {
            add(sums, Some(*colony), EXPOSURE, 1.0);
        }
    }
}

fn held(sums: &mut Sums, seen: &Seen) {
    let colonies = &seen.derived.colonies;
    let worked = (seen.derived.worked.iter()).map(|o| o.room);
    for room in colonies.iter().copied().chain(worked) {
        add(sums, Some(room), ROOMS_HELD, 1.0);
    }
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
    let mut undone = HashMap::default();
    let mut asked: HashMap<&str, Asked> = HashMap::default();
    let mut spawned: BTreeSet<RoomName> = BTreeSet::new();
    let mut owed: BTreeSet<RoomName> = BTreeSet::new();
    for intent in seen.intents {
        match intent {
            Intent::Creep(name, act) => {
                let a = asked.entry(name.as_str()).or_default();
                match act {
                    CreepAct::AttackCreep(_)
                    | CreepAct::RangedAttackCreep(_)
                    | CreepAct::RangedMassAttack => a.strike = true,
                    CreepAct::HealCreep(id) | CreepAct::RangedHealCreep(id) => {
                        a.heal |= (world.creeps.iter()).any(|c| &c.id == id && c.hits < c.hits_max);
                    }
                    _ => (),
                }
            }
            Intent::Spawn(id, SpawnAct::SpawnCreep(..))
                if !refused.contains(&(id.as_str(), "spawn")) =>
            {
                spawned.extend(
                    world
                        .spawns
                        .iter()
                        .filter(|s| &s.id == id)
                        .map(|s| s.pos.room),
                );
            }
            Intent::Held(name) => asked.entry(name.as_str()).or_default().held = true,
            Intent::Short(room, pattern, n) => {
                let name = format!("{SHORT}{}", bodies::name_of(*pattern));
                add(sums, Some(*room), &name, f64::from(*n));
                owed.insert(*room);
            }
            _ => (),
        }
        if let Some(act) = verb(intent)
            && !refused.contains(&act)
            && let Some((room, what, n)) = spent(world, &creeps, &fed, &mut undone, intent)
        {
            add(sums, Some(room), what, f64::from(n));
        }
    }

    for c in world.creeps.iter().filter(|c| !tick::fights(&c.name)) {
        let Some(p) = bodies::pattern_of_name(&c.name) else {
            continue;
        };
        let pace = m.paces.entry(p).or_default();
        pace.bodies += 1.0;
        let task = task_of(seen.assignments, &c.name);
        if task.is_none() || !asked.contains_key(c.name.as_str()) {
            pace.idle += 1.0;
        }
        let load = c.store.used();
        match (task, m.tasks.get_mut(&c.name)) {
            (Some(now), Some(held)) => {
                if now != held.task {
                    if held.load == load {
                        add(sums, None, CHURN, 1.0);
                    }
                    now.clone_into(&mut held.task);
                }
                held.load = load;
            }
            (Some(now), None) => {
                let held = Held {
                    task: now.to_string(),
                    load,
                };
                m.tasks.insert(c.name.clone(), held);
            }
            _ => (),
        }
    }
    m.tasks
        .retain(|name, _| task_of(seen.assignments, name).is_some());
    returned(sums, &mut m.drawn, &creeps, (seen.intents, world), &refused);

    for s in &world.spawns {
        let row = Some(s.pos.room);
        add(sums, row, SPAWN_TICKS, 1.0);
        // Below a full bank the spawn waits on energy, which the casts wait for.
        let full = world
            .room(s.pos.room)
            .is_some_and(|r| r.energy_available >= r.energy_capacity);
        if s.spawning.is_some() {
            add(sums, row, SPAWNING, 1.0);
        } else if full && owed.contains(&s.pos.room) && !spawned.contains(&s.pos.room) {
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

    steps(sums, &mut m.tracks, world, &asked);
    shots(sums, world, &asked);
    deaths(sums, &mut m.tracks, world);
    lost(sums, world);
    dwell(sums, seen);
    exposure(sums, seen);
    held(sums, seen);

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
                (names.entry(STALE.to_string()))
                    .and_modify(|at| *at = at.min(ticks))
                    .or_insert(ticks);
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
    fn a_body_counts_its_stuck_steps_flips_and_swings() {
        let (x, y) = (RoomName::named("W1N1"), RoomName::named("W2N1"));
        let mut t = Track {
            id: String::new(),
            seen: 0,
            at: Pos::new(x, 10, 10),
            before: None,
            still: 0,
            left_at: None,
            ttl: 1500,
            wounded: false,
            struck: None,
            cost: 0,
        };
        let mut stuck = Vec::new();
        for time in 1..=62 {
            stuck.extend(t.moved(Pos::new(x, 10, 10), time, time % 5 != 0, time % 5 == 0));
        }
        let wedged = stuck.iter().filter(|n| **n == WEDGED).count();
        assert_eq!((stuck.len() - wedged, wedged), (50, 1));
        assert!(t.moved(Pos::new(x, 11, 10), 63, false, false).is_empty());
        assert_eq!(t.moved(Pos::new(x, 10, 10), 64, false, false), [SWING]);
        assert!(t.moved(Pos::new(y, 49, 10), 65, false, false).is_empty());
        assert!(t.moved(Pos::new(x, 0, 10), 71, false, false).is_empty());
        assert_eq!(t.moved(Pos::new(y, 49, 10), 72, false, false), [EDGE]);
    }

    #[test]
    fn every_threshold_names_a_metric() {
        for (name, _) in THRESHOLDS {
            assert!(rank(name).is_some(), "{name}");
        }
    }
}
