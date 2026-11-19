use std::collections::{BTreeMap, BTreeSet};

use crate::atlas::{self, Atlas, Context, Mover, Target};
use crate::declarations;
use crate::engine;
use crate::grid::{self, Footprint};
use crate::intel::Scout;
use crate::intents::{
    Action, Ask, CreepAct, LabAct, LinkAct, ModuleOutput, Rank, SpawnAct, Telemetry, TerminalAct,
};
use crate::mv::{Wish, plan};
use crate::tiles::{self, TileSet};
use crate::vocabulary::{Part, Resource};
use crate::world::{Creep, Id, Kind, Pos, RoomName};

use super::bodies::{self, Pattern};
use super::derived::{self, Body, Derived, Ground, LinkFact, RefillFact};
use super::jobs::Jobs;
use super::slots::{self, Job, Slot, distance};
use super::task::{self, Assignment, Task};

/// Re-chosen only on arrival off the exit band, so a walk is never turned
/// mid-way nor on a border; a scout whose open rooms lie past `REACH_HOPS`
/// shares a held one rather than idle.
fn scouting(
    atlas: &Atlas,
    derived: &Derived,
    held: &BTreeMap<String, Assignment>,
) -> BTreeMap<String, RoomName> {
    let mut scouts: Vec<&Body> = (derived.bodies.iter())
        .filter(|b| b.pattern == Pattern::Scout)
        .collect();
    scouts.sort_by(|a, b| a.name.cmp(&b.name));
    let kept = |b: &Body| match held.get(&b.name).and_then(|a| a.task.as_ref()) {
        Some(Task::Scout(room)) => (derived.scouts.iter())
            .find(|(at, _, _)| at.room == *room)
            .filter(|(at, _, _)| !b.pos.within(Task::Scout(*room).range_of(), at))
            .map(|_| *room),
        _ => None,
    };
    let mut out: BTreeMap<String, RoomName> = (scouts.iter())
        .filter_map(|b| Some((b.name.clone(), kept(b)?)))
        .collect();
    for b in scouts {
        let walks = |open: &dyn Fn(RoomName) -> bool| {
            let rooms = (derived.scouts.iter()).filter(|(at, _, _)| open(at.room));
            // A radius room is three apart but its route may run past the
            // reach the trees hold.
            let far = |at: &Pos| {
                let rooms = atlas.route(
                    &BTreeSet::new(),
                    declarations::SIEGE_HOPS,
                    b.pos.room,
                    at.room,
                )?;
                Some(Atlas::CROSSING_TICKS * (rooms.len() as i32 - 1))
            };
            atlas::onward(rooms.filter_map(|(at, age, _)| {
                let walk = distance(atlas, derived, &b.pos, at).or_else(|| far(at))?;
                Some((at.room, *age, walk))
            }))
        };
        if !out.contains_key(&b.name)
            && let Some(room) =
                walks(&|room| !out.values().any(|r| *r == room)).or_else(|| walks(&|_| true))
        {
            out.insert(b.name.clone(), room);
        }
    }
    out
}

fn ring_members<'a>(derived: &'a Derived, task: &Task, at: &Pos) -> Vec<&'a RefillFact> {
    let Task::Refill(task::Target::Ring(room), _) = task else {
        return Vec::new();
    };
    let g = (derived.grounds.iter()).find(|g| g.room == *room);
    let mut members: Vec<&RefillFact> = (g.iter().flat_map(|g| &g.refills))
        .filter(|f| f.kind != Kind::Tower && f.free > 0)
        .collect();
    members.sort_by_key(|f| (at.range(&f.pos).unwrap_or(i32::MAX), &f.id));
    members
}

fn spot_of(derived: &Derived, task: &Task) -> Option<Pos> {
    match task {
        Task::Scout(room) => Some(Pos::new(*room, 25, 25)),
        _ => derived.spots.get(&task.target()?).copied(),
    }
}

/// What a store, link, lab or container holds of `r`: the engine refuses a
/// draw from nothing.
fn holds(derived: &Derived, id: task::Target, r: &Resource) -> i32 {
    let object = id.object();
    derived
        .grounds
        .iter()
        .map(|g| {
            let stored = (g.stores.iter())
                .filter(|s| s.id == id && r.is_energy())
                .map(|s| s.energy)
                .sum::<i32>();
            let linked = (g.links.iter())
                .filter(|l| Some(l.id) == object && r.is_energy())
                .map(|l| l.energy)
                .sum::<i32>();
            let ores = (g.ores.iter())
                .filter(|o| Some(o.0) == object && &o.2 == r)
                .map(|o| o.3)
                .sum::<i32>();
            let labs = (g.labs.iter())
                .filter(|l| Some(l.id) == object)
                .filter_map(|l| l.held.as_ref().filter(|h| &h.0 == r).map(|h| h.1))
                .sum::<i32>();
            let mined = (g.minerals.iter())
                .filter(|m| m.container.is_some() && m.container == object && &m.resource == r)
                .map(|m| m.held)
                .sum::<i32>();
            let terminal = (g.terminal.iter())
                .filter(|t| Some(t.id) == object)
                .map(|t| t.store.amount_of(r))
                .sum::<i32>();
            stored.max(terminal)
                + linked
                + ores.max(0) * i32::from(stored == 0 && terminal == 0)
                + labs
                + mined
        })
        .sum()
}

fn piled(derived: &Derived, id: task::Target) -> bool {
    (derived.grounds.iter()).any(|g| g.stores.iter().any(|s| s.kind.is_pile() && s.id == id))
}

fn act_of(derived: &Derived, task: &Task) -> Option<CreepAct> {
    let held = |id: task::Target| {
        derived.grounds.iter().any(|g| {
            g.controller
                .as_ref()
                .is_some_and(|c| id == c.id.into() && derived::invader_held(c))
        })
    };

    let at = |id: &task::Target| id.object();
    Some(match task {
        Task::Harvest(id) => CreepAct::Harvest(at(id)?),
        Task::Withdraw(id, _) if piled(derived, *id) => CreepAct::Pickup(at(id)?),
        Task::Refill(id, r) if piled(derived, *id) => CreepAct::Drop(r.clone()),
        Task::Withdraw(id, r) => CreepAct::Withdraw(at(id)?, r.clone()),
        Task::Pickup(id, _) => CreepAct::Pickup(at(id)?),
        Task::Refill(id, r) => CreepAct::Transfer(at(id)?, r.clone()),
        Task::Build(id) => CreepAct::Build(at(id)?),
        Task::Repair(id) => CreepAct::Repair(at(id)?),
        Task::Upgrade(id) => CreepAct::Upgrade(at(id)?),
        Task::Reserve(id) if held(*id) => CreepAct::Unreserve(at(id)?),
        Task::Reserve(id) => CreepAct::Reserve(at(id)?),
        Task::Claim(id) => CreepAct::ClaimController(at(id)?),
        Task::Scout(_) | Task::Meet(_) => return None,
    })
}

/// Every body with room and no other resource aboard picks up dropped energy
/// beside it, beside its Task's own act: the engine's pickup conflicts with no
/// other, and a pile no Task reaches decays. Not a harvester, whose dig falls
/// where it stands. `taken`: the bodies whose act already fills them.
fn pickups(derived: &Derived, taken: &BTreeSet<String>) -> Vec<Action> {
    let mut picks = Vec::new();

    for b in &derived.bodies {
        if b.free > 0
            && b.pattern != Pattern::Harvester
            && !taken.contains(&b.name)
            && b.store.held.iter().all(|(r, _)| r.is_energy())
        {
            let pile = (derived.grounds.iter()).find_map(|g| {
                (g.piles.iter()).find(|p| p.resource.is_energy() && b.pos.within(1, &p.pos))
            });

            if let Some(p) = pile {
                picks.push(Action::Creep(b.name.clone(), CreepAct::Pickup(p.id)));
            }
        }
    }

    picks
}

/// Hivemind's `MIN_ENERGY_TRANSFER`, a quarter of a link.
const LINK_MIN_SEND: i32 = 200;

/// Hivemind's `process/rooms/owned/links.ts`.
fn link_send(g: &Ground) -> Option<Action> {
    let hubs = || g.links.iter().filter(|l| l.role.is_hub());
    type Side<'a> = Option<(&'a LinkFact, i32)>;
    fn pick<'a>(best: Side<'a>, hub: Side<'a>) -> Side<'a> {
        best.filter(|(_, n)| *n >= LINK_MIN_SEND).or(hub).or(best)
    }
    let over = (g.links.iter())
        .filter(|l| l.role.is_source() && l.energy > 0 && l.cooldown == 0)
        .min_by_key(|l| -l.energy)
        .map(|l| (l, l.energy));
    let hub = (hubs())
        .filter(|l| l.cooldown == 0 && l.energy > 0)
        .min_by_key(|l| -l.energy);
    let (from, out) = pick(over, hub.map(|l| (l, l.energy)))?;
    let under = (g.links.iter())
        .filter(|l| l.role.is_controller() && l.free > 0)
        .min_by_key(|l| -l.free)
        .map(|l| (l, l.free));
    let hub = (hubs().filter(|l| l.free > 0))
        .min_by_key(|l| l.energy)
        .map(|l| (l, l.free));
    let (to, room) = pick(under, hub)?;
    let amount = out.min(room);
    (from.id != to.id && amount >= LINK_MIN_SEND).then_some(Action::Link(
        from.id,
        LinkAct::TransferEnergy(to.id, amount),
    ))
}

/// One send a tick off cooldown: what the terminal holds of the first
/// resource still owed, as much as its energy pays for and the receiver has
/// room for, never under the engine's least. Energy sent leaves the fee of
/// what is still owed.
fn terminal_send(g: &Ground) -> Option<Action> {
    let (t, (home, free)) = (g.terminal.as_ref()?, g.ship_to.as_ref()?);
    if t.cooldown > 0 {
        return None;
    }
    let apart = tiles::rooms_apart(g.room, *home);
    let energy = t.store.amount_of(&Resource::Energy);
    let fees: i32 = g
        .shipment
        .iter()
        .filter(|(r, _)| !r.is_energy())
        .map(|(_, n)| engine::send_cost(*n, apart))
        .sum();
    let kept = if fees > 0 { fees + 100 } else { 0 };
    g.shipment.iter().find_map(|(ore, owed)| {
        let (pays, sent) = if ore.is_energy() {
            (energy - kept, 1)
        } else {
            (energy, 0)
        };
        let mut amount = t.store.amount_of(ore).min(*owed).min(*free);
        while amount > 0 && engine::send_cost(amount, apart) + sent * amount > pays {
            amount = amount * 9 / 10;
        }
        (amount >= engine::TERMINAL_MIN_SEND.min(*owed))
            .then(|| Action::Terminal(t.id, TerminalAct::Send(ore.clone(), amount, *home)))
    })
}

fn wish_in(groups: &mut Vec<(RoomName, Vec<Wish>)>, room: RoomName, wish: Wish) {
    match groups.iter_mut().find(|(each, _)| *each == room) {
        Some((_, list)) => list.push(wish),
        None => groups.push((room, vec![wish])),
    }
}

/// A dug-out source's holders stay seated until it regenerates, and an
/// extractor's until it cools or its container has room for a pull (v1's
/// heldByFullContainer): a pull past it drops ore nothing picks up.
fn dug_out(derived: &Derived) -> BTreeSet<Id> {
    derived
        .grounds
        .iter()
        .flat_map(|g| {
            let sources = (g.sources.iter()).filter(|s| s.energy == 0).map(|s| s.id);
            let full = |m: &&derived::MineralFact| {
                m.cooldown > 0
                    || engine::CONTAINER_CAPACITY - m.held
                        < bodies::MINER_WORK * engine::MINERAL_HARVEST_PER_WORK
            };
            sources.chain(g.minerals.iter().filter(full).map(|m| m.id))
        })
        .collect()
}

fn sends(derived: &Derived) -> Vec<Action> {
    let mut sends: Vec<Action> = Vec::new();
    for g in &derived.grounds {
        sends.extend(link_send(g));
        sends.extend(terminal_send(g));
        if let Some(plan) = &g.lab_plan {
            let lab = |id: Id| g.labs.iter().find(|l| l.id == id);
            let held = |id: Id| lab(id).and_then(|l| l.held.clone());
            let cooldown = |id: Id| lab(id).map_or(0, |l| l.cooldown);
            for (output, a, b) in super::labs::ready_runs(plan, &held, &cooldown) {
                sends.push(Action::Lab(output, LabAct::RunReaction(a, b)));
            }
        }
    }

    sends
}

/// Arbitration's weight: the dig and its carriers walk first.
fn weight_of(job: &Job) -> i32 {
    match job {
        Job::Dig(_) | Job::Reserve(_) | Job::Claim(_) => 41,
        Job::Fill => 31,
        Job::Work(_) => 21,
        Job::Upgrade => 11,
        Job::Scout(_) => 1,
    }
}

/// The seats beside a source no post holds, for a worker's dig.
fn dig_seats(atlas: &Atlas, derived: &Derived, id: task::Target, at: &Pos) -> Vec<Pos> {
    let post = (derived.grounds.iter())
        .flat_map(|g| g.sources.iter())
        .find(|s| id == s.id.into())
        .and_then(|s| s.post);
    let seats = atlas.seats(Footprint::One, 1, at);
    seats.into_iter().filter(|p| Some(*p) != post).collect()
}

/// A reserver keeps off every post in the room: its controller can share a
/// seat with a source, and its harvester stands nowhere else. None when no
/// seat is a post, or every one is: any tile in range serves.
fn reserve_seats(atlas: &Atlas, derived: &Derived, at: &Pos) -> Option<Vec<Pos>> {
    let posts: Vec<Pos> = (derived.grounds.iter())
        .filter(|g| g.room == at.room)
        .flat_map(|g| {
            (g.sources.iter().filter_map(|s| s.post))
                .chain(g.minerals.iter().filter_map(|m| m.post))
                .chain(g.buffer_seats.iter().copied())
        })
        .collect();
    let seats = atlas.seats(Footprint::One, 1, at);
    let open: Vec<Pos> = (seats.iter())
        .filter(|p| !posts.contains(p))
        .copied()
        .collect();
    (!open.is_empty() && open.len() < seats.len()).then_some(open)
}

/// A seated task walks to a seat no other body stands on, so one body on the
/// nearest seat does not hold the rest short of theirs.
fn target_of(
    atlas: &Atlas,
    derived: &Derived,
    (slot, task, at): (&Slot, &Task, Pos),
    stood: &BTreeSet<Pos>,
    creep: &Creep,
) -> (Target, Option<Vec<Pos>>) {
    let seats: Option<Vec<Pos>> = match (&slot.job, task) {
        (Job::Dig(_) | Job::Upgrade, _) if slot.seat != at => Some(vec![slot.seat]),
        (_, Task::Harvest(id)) => {
            Some(dig_seats(atlas, derived, *id, &at)).filter(|s| !s.is_empty())
        }
        (_, Task::Reserve(_) | Task::Claim(_)) => reserve_seats(atlas, derived, &at),
        // A drop lands where the body stands.
        (_, Task::Refill(id, _)) if piled(derived, *id) => Some(vec![at]),
        // A worker upgrades off the upgraders' seats.
        (Job::Work(_), Task::Upgrade(_)) => {
            let buffer = (derived.grounds.iter())
                .find(|g| g.room == at.room)
                .map_or(&[][..], |g| &g.buffer_seats[..]);
            let seats = atlas.seats(Footprint::One, task.range_of(), &at);
            Some(
                seats
                    .into_iter()
                    .filter(|p| !buffer.contains(p))
                    .collect::<Vec<_>>(),
            )
            .filter(|s| !s.is_empty())
        }
        _ => None,
    };
    let target = match &seats {
        Some(seats) => {
            let open: Vec<Pos> = (seats.iter())
                .filter(|p| **p == creep.pos || !stood.contains(*p))
                .copied()
                .collect();
            let tiles = if open.is_empty() { seats } else { &open };
            Target {
                room: at.room,
                tiles: tiles.iter().map(tiles::of_pos).collect(),
                range: 0,
                avoid: BTreeSet::new(),
                max_hops: declarations::REACH_HOPS,
            }
        }
        // A scout walks as far as its choice reached (`scouting`).
        None if matches!(task, Task::Scout(_)) => Target {
            max_hops: declarations::SIEGE_HOPS,
            ..atlas::target_at(&at, task.range_of())
        },
        None => atlas::target_at(&at, task.range_of()),
    };
    (target, seats)
}

/// An assigned body's acts this tick, and its walk: to its seat, else in its
/// task's range.
fn work(
    atlas: &mut Atlas,
    derived: &Derived,
    (creep, body, mover, danger): (&Creep, &Body, Mover, Option<&TileSet>),
    (slot, task): (&Slot, &Task),
    stood: &BTreeSet<Pos>,
    ready: &dyn Fn(&Task) -> bool,
    acts: &mut Vec<Action>,
) -> Wish {
    let members = ring_members(derived, task, &creep.pos);
    let ring = match (task, members.first()) {
        (Task::Refill(_, r), Some(f)) => Some(Task::Refill(f.id.into(), r.clone())),
        _ => None,
    };
    let task = ring.as_ref().unwrap_or(task);
    let Some(at) = spot_of(derived, task) else {
        return plan::stay(&*atlas, creep, mover.factor, 1);
    };
    let (mut target, seats) = target_of(&*atlas, derived, (slot, task, at), stood, creep);
    // v1's refill cluster.
    if !members.is_empty() {
        target.tiles = members.iter().map(|f| tiles::of_pos(&f.pos)).collect();
    }
    let buffer = (derived.grounds.iter())
        .filter(|g| g.room == creep.pos.room)
        .find_map(|g| g.buffer.as_ref());
    let beside = buffer.filter(|b| b.energy > 0 && creep.pos.within(1, &b.pos));
    // The engine withdraws before it upgrades, so the draw tops up the spend.
    if let Some(b) = beside
        && let Some(id) = b.id.object()
        && matches!(task, Task::Upgrade(_))
        && body.free > 0
        && body.energy < body.work * engine::UPGRADE_DRAIN_PER_WORK
    {
        acts.push(Action::Creep(
            creep.name.clone(),
            CreepAct::Withdraw(id, Resource::Energy),
        ));
    }
    let digs = matches!(slot.job, Job::Dig(_));
    if let Some(act) = raising(derived, body, slot) {
        acts.push(Action::Creep(creep.name.clone(), act));
    } else {
        let arrived = creep.pos.within(task.range_of(), &at)
            && seats
                .as_ref()
                .is_none_or(|seats| seats.contains(&creep.pos))
            && ready(task);
        // The API refuses a spend from an empty store before the draw lands.
        let can = match task {
            Task::Upgrade(_) | Task::Build(_) | Task::Repair(_) => body.energy > 0 && body.work > 0,
            Task::Harvest(_) => body.work > 0,
            Task::Reserve(_) | Task::Claim(_) => body.claim > 0,
            Task::Withdraw(id, r) => holds(derived, *id, r) > 0 && body.free > 0,
            Task::Pickup(..) => body.free > 0,
            Task::Refill(_, r) => body.store.amount_of(r) > 0,
            _ => true,
        };
        if arrived
            && can
            && let Some(act) = act_of(derived, task)
        {
            acts.push(Action::Creep(creep.name.clone(), act));
        }
    }
    if digs && let Some(g) = derived.grounds.iter().find(|g| g.room == creep.pos.room) {
        let beside = |p: &Pos| creep.pos.within(1, p);
        let bay = (g.bays.iter()).find(|b| beside(&b.pos)).map(|b| b.id);
        let link = (g.links.iter())
            .find(|l| g.receiver.is_some() && l.role.is_source() && l.free > 0 && beside(&l.pos))
            .map(|l| l.id);
        if let Some(to) = bay.or(link) {
            if body.energy > 0 {
                acts.push(Action::Creep(
                    creep.name.clone(),
                    CreepAct::Transfer(to, Resource::Energy),
                ));
            }
            let under = (g.stores.iter()).find(|s| s.pos == creep.pos && s.energy > 0);
            if let Some(c) = under.filter(|_| body.free > 0)
                && let Some(id) = c.id.object()
            {
                acts.push(Action::Creep(
                    creep.name.clone(),
                    CreepAct::Withdraw(id, Resource::Energy),
                ));
            }
        }
    }
    plan::toward_round(atlas, creep, mover, &target, weight_of(&slot.job), danger)
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct EconomyState {
    pub assignments: BTreeMap<String, Assignment>,
    pub spare: BTreeMap<RoomName, f64>,
}

pub fn decide(
    context: &mut Context,
    state: &mut EconomyState,
    scouts: &[Scout],
) -> Result<ModuleOutput, String> {
    // `{ World with Creeps = filtered }`: a creep a fight holds is not in the
    // economy, and only the creep list differs.
    let seen: Vec<&Creep> = (context.world.creeps.iter())
        .filter(|c| !declarations::fights(&c.name))
        .collect();

    let derived = derived::of_world(
        context.world,
        &seen,
        context.atlas,
        context.declarations,
        context.derived,
        context.intel,
        scouts,
    );

    let movers: BTreeMap<&str, Mover> = (seen.iter())
        .map(|c| {
            let mover = Mover {
                footprint: Footprint::One,
                factor: grid::factor_of(c),
            };
            (c.name.as_str(), mover)
        })
        .collect();

    let owners = slots::owners(context.atlas, &derived);
    let slots = slots::plan(context.atlas, &derived, &owners, &state.assignments);
    state.spare = slots::spare(&derived, &slots);
    let (assigned, holders, retired) =
        slots::assign(&*context.atlas, &derived, &slots, &state.assignments);
    let (casts, short) = slots::casts(&derived, &slots, &holders);

    let asks = (casts.iter())
        .map(|cast| {
            let rank = if cast.floor {
                Rank::Floor
            } else {
                Rank::Economy
            };
            (
                cast.colony,
                Ask {
                    body: cast.body.clone(),
                    name: cast.name.clone(),
                    rank,
                },
            )
        })
        .collect();
    let telemetry = Telemetry {
        counts: vec![("parkedCarriers", slots::parked(&slots, &holders))],
        short: (short.into_iter())
            .map(|(room, p, n)| (room, bodies::name_of(p), n))
            .collect(),
        traces: Vec::new(),
    };

    let scouted = scouting(&*context.atlas, &derived, &state.assignments);
    let dug = dug_out(&derived);
    let ready = |task: &Task| match task {
        Task::Harvest(id) => id.object().is_none_or(|id| !dug.contains(&id)),
        _ => true,
    };
    let stood: BTreeSet<Pos> = seen.iter().map(|c| c.pos).collect();
    let bodies: BTreeMap<&str, &Body> = (derived.bodies.iter())
        .map(|b| (b.name.as_str(), b))
        .collect();

    // A body holding a task books its load before another picks.
    let kept = |name: &str| state.assignments.get(name).and_then(|a| a.task.clone());
    let held: Vec<(usize, &Body, Option<Task>)> = (assigned.iter())
        .filter_map(|(name, si)| Some((*si, *bodies.get(name.as_str())?, kept(name))))
        .collect();
    let mut jobs = Jobs::new(&*context.atlas, &derived, (&slots, &owners), &held);
    let mut order: Vec<&Creep> = seen;
    // The mover settles a tie by the order it is asked.
    order.sort_by_cached_key(|&c| (kept(&c.name).is_none(), c.name.as_str()));

    // A scout walks where `scouting` sends it, with a slot to count it or
    // without, as where no colony casts one.
    let strays: BTreeMap<&str, Slot> = (derived.bodies.iter())
        .filter(|b| b.pattern == Pattern::Scout && !assigned.contains_key(&b.name))
        .filter_map(|b| {
            let room = *scouted.get(&b.name)?;
            let slot = Slot {
                id: String::new(),
                rank: 0,
                ..Slot::new(
                    0,
                    false,
                    Job::Scout(room),
                    Pos::new(room, 25, 25),
                    "",
                    0,
                    Vec::new(),
                )
            };
            Some((b.name.as_str(), slot))
        })
        .collect();
    type Held<'a> = Option<(&'a Slot, &'a Body, Option<Task>)>;
    let tasks: Vec<(&Creep, Held)> = (order.into_iter())
        .map(|creep| {
            let at = (assigned.get(&creep.name))
                .map(|si| &slots[*si])
                .or_else(|| strays.get(creep.name.as_str()))
                .zip(bodies.get(creep.name.as_str()).copied());
            let at = at.map(|(slot, body)| {
                let task = match &slot.job {
                    Job::Scout(_) => scouted.get(&creep.name).map(|room| Task::Scout(*room)),
                    _ => jobs.task(slot, body, kept(&creep.name).as_ref()),
                };
                (slot, body, task)
            });
            (creep, at)
        })
        .collect();

    let mut next = BTreeMap::new();
    let mut assign = |name: &str, slot: &str, task: Option<Task>| {
        let slot = slot.to_string();
        next.insert(name.to_string(), Assignment { slot, task });
    };
    let mut acts: Vec<Action> = Vec::new();
    let mut wishes: Vec<(RoomName, Vec<Wish>)> = Vec::new();
    for (creep, at) in tasks {
        let mover = movers[creep.name.as_str()];
        let danger = context.derived.danger(creep.pos.room);
        let Some((slot, body, task)) = at else {
            // A load held idle is lost to every sink.
            let loaded = creep.store.amount_of(&Resource::Energy) > 0;
            let works = creep.live(Part::Work) > 0;
            let sink = (derived.grounds.iter())
                .filter(|g| g.room == creep.pos.room)
                .find_map(|g| match (works, &g.controller, g.bank()) {
                    (true, Some(c), _)
                        if c.owner.is_ours() && !slots::fed(c) && !g.sites.is_empty() =>
                    {
                        (g.sites.iter())
                            .min_by_key(|s| creep.pos.range(&s.pos))
                            .map(|s| (s.pos, task::RANGE_UPGRADE, CreepAct::Build(s.id)))
                    }
                    (true, Some(c), _) if c.owner.is_ours() && slots::fed(c) => {
                        Some((c.pos, task::RANGE_UPGRADE, CreepAct::Upgrade(c.id)))
                    }
                    (_, _, Some(s)) if s.owned => (s.id.object())
                        .map(|id| (s.pos, 1, CreepAct::Transfer(id, Resource::Energy))),
                    (false, _, Some(s)) => Some((s.pos, 0, CreepAct::Drop(Resource::Energy))),
                    _ => None,
                });
            if let Some((to, reach, act)) = sink.filter(|_| loaded) {
                if creep.pos.within(reach, &to) {
                    acts.push(Action::Creep(creep.name.clone(), act));
                } else {
                    let wish = plan::toward_round(
                        context.atlas,
                        creep,
                        mover,
                        &atlas::target_at(&to, reach),
                        reach,
                        danger,
                    );
                    wish_in(&mut wishes, creep.pos.room, wish);
                    continue;
                }
            }
            // A retired body has no seat to come back to.
            if let Some(s) = (derived.colonies.iter())
                .find(|c| retired.contains(&creep.name) && c.room == creep.pos.room)
                .map(|c| &c.spawns[0])
            {
                if creep.pos.within(1, &s.pos) {
                    let recycle = SpawnAct::RecycleCreep(creep.id);
                    acts.push(Action::Spawn(s.id, recycle));
                }
                let to = atlas::target_at(&s.pos, 1);
                let wish = plan::toward_round(context.atlas, creep, mover, &to, 1, danger);
                wish_in(&mut wishes, creep.pos.room, wish);
                continue;
            }
            let wish = home(context.atlas, &derived, creep, mover, danger);
            wish_in(&mut wishes, creep.pos.room, wish);
            continue;
        };
        if slot.closed {
            assign(&creep.name, &slot.id, None);
            let wish = home(context.atlas, &derived, creep, mover, danger);
            wish_in(&mut wishes, creep.pos.room, wish);
            continue;
        }
        assign(&creep.name, &slot.id, task.clone());

        let wish = match &task {
            Some(task) => work(
                context.atlas,
                &derived,
                (creep, body, mover, danger),
                (slot, task),
                &stood,
                &ready,
                &mut acts,
            ),
            None => plan::stay(&*context.atlas, creep, mover.factor, 1),
        };
        wish_in(&mut wishes, creep.pos.room, wish);
    }
    for b in &derived.ovens {
        if let Some(si) = assigned.get(&b.name) {
            assign(&b.name, &slots[*si].id, None);
        }
    }
    for c in &casts {
        assign(&c.name, &c.slot, None);
    }

    let taken: BTreeSet<String> = (acts.iter())
        .filter_map(|act| match act {
            Action::Creep(
                name,
                CreepAct::Pickup(_) | CreepAct::Withdraw(..) | CreepAct::Harvest(_),
            ) => Some(name.clone()),
            _ => None,
        })
        .collect();
    let mut actions = acts;
    actions.extend(pickups(&derived, &taken));
    actions.extend(sends(&derived));
    let given_up = (context.world.rooms.iter()).filter(|r| context.declarations.abandoning(r.name));
    actions.extend(given_up.flat_map(super::abandon::raze));

    state.assignments = next;
    Ok(ModuleOutput {
        actions,
        asks,
        wishes: (wishes.into_iter())
            .flat_map(|(room, list)| list.into_iter().map(move |w| (room, w)))
            .collect(),
        telemetry,
    })
}

/// A body with no slot walks home, where one may open, the last of equal
/// walks; at home it stands.
fn home(
    atlas: &mut Atlas,
    derived: &Derived,
    creep: &Creep,
    mover: Mover,
    danger: Option<&TileSet>,
) -> Wish {
    if derived.colonies.is_empty() || derived.colonies.iter().any(|c| c.room == creep.pos.room) {
        return plan::stay(&*atlas, creep, mover.factor, 1);
    }
    let walk = |c: &&derived::Colony| {
        distance(&*atlas, derived, &creep.pos, &c.spawns[0].pos).unwrap_or(i32::MAX)
    };
    let home = (derived.colonies.iter().rev())
        .min_by_key(walk)
        .map_or(creep.pos, |c| c.spawns[0].pos);
    plan::toward_round(atlas, creep, mover, &atlas::target_at(&home, 1), 1, danger)
}

/// A harvester full on its post raises or keeps the container under it, so
/// its dig never piles on the site; an upgrader keeps its buffer, which no
/// worker below the reserve has the energy to mend.
fn raising(derived: &Derived, body: &Body, slot: &Slot) -> Option<CreepAct> {
    let at = &body.pos;
    if !(*at == slot.seat && body.work > 0 && body.energy > 0) {
        return None;
    }
    let g = derived.grounds.iter().find(|g| g.room == at.room)?;
    let kept = match slot.job {
        Job::Dig(_) if body.free == 0 => {
            if let Some(site) = slots::raised_by(g, body) {
                return Some(CreepAct::Build(site.id));
            }
            (g.repairs.iter()).find(|r| r.pos == *at && r.kind == Kind::Container)
        }
        Job::Upgrade => {
            (g.repairs.iter()).find(|r| g.buffer.as_ref().is_some_and(|b| b.id == r.id.into()))
        }
        _ => None,
    };
    kept.filter(|r| 2 * r.hits < r.hits_max)
        .map(|r| CreepAct::Repair(r.id))
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::*;
    use crate::economy::dice::{self, Dice};

    #[test]
    fn decide_never_faults_and_holds_a_slot_and_a_task_it_can_read() {
        let mut dice = Dice::new(51);

        for _ in 0..100 {
            let g = dice::world(&mut dice);
            let mut atlas = Atlas::of_world(&g.world);
            let sightings = BTreeMap::new();
            let mut declarations = declarations::undeclared();
            declarations.outposts = g.worked.clone();
            declarations.claims = g.claims.clone();
            let mut derived = atlas::derive(&g.world, &mut atlas);
            derived.worked = atlas::worked(&g.world, &sightings, &declarations, &derived);

            let mut context = Context {
                world: &g.world,
                atlas: &mut atlas,
                declarations: &declarations,
                sightings: &sightings,
                intel: &BTreeMap::new(),
                derived: &derived,
                trace: false,
            };

            let mut state = EconomyState::default();
            decide(&mut context, &mut state, &[]).expect("a world decides");
            let out = decide(&mut context, &mut state, &[]).expect("a world decides again");

            for (name, held) in &state.assignments {
                assert!(!held.slot.is_empty(), "{name} {held:?}");
            }

            for (_, ask) in &out.asks {
                assert!(ask.body.len() <= crate::engine::MAX_BODY_PARTS);
            }

            let wished: BTreeSet<&str> =
                (out.wishes.iter()).map(|(_, w)| w.creep.as_str()).collect();

            let fought = (g.world.creeps.iter())
                .filter(|c| declarations::fights(&c.name))
                .count();

            assert_eq!(
                wished.len() + fought,
                g.world.creeps.len(),
                "every body not in a fight wishes once"
            );
        }
    }
}
