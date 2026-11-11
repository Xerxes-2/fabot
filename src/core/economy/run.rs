use std::collections::{BTreeMap, BTreeSet};

use crate::core::atlas::{self, Atlas, Mover, Target};
use crate::core::engine;
use crate::core::grid::{self, Footprint};
use crate::core::intents::{Ask, CreepAct, Intent, LabAct, LinkAct, Rank, TerminalAct};
use crate::core::mv::{Wish, plan};
use crate::core::state::State;
use crate::core::tick::{self, Context};
use crate::core::tiles::{self, TileSet};
use crate::core::vocabulary::{BodyPart, Resource};
use crate::core::world::{Creep, Pos, RoomName};

use super::bodies;
use super::derived::{self, Body, Derived, Ground, LinkFact};
use super::jobs::Jobs;
use super::slots::{self, Job, Slot, distance};
use super::task::{self, Task};

/// Creep name → `slot|task`.
type Assignments = BTreeMap<String, String>;

/// Re-chosen only on arrival off the exit band, so a walk is never turned
/// mid-way nor on a border; a scout whose open rooms lie past `REACH_HOPS`
/// shares a held one rather than idle.
fn scouting(atlas: &Atlas, derived: &Derived, held: &Assignments) -> Assignments {
    let mut scouts: Vec<&Body> = (derived.bodies.iter())
        .filter(|b| b.pattern.is_scout())
        .collect();
    scouts.sort_by(|a, b| a.name.cmp(&b.name));
    let kept = |b: &Body| match (held.get(&b.name))
        .and_then(|v| slots::task_of(v))
        .and_then(task::of_task_id)
    {
        Some(Task::Scout(room)) => (derived.scouts.iter())
            .find(|(at, _, _)| at.room == room)
            .filter(|(at, _, _)| !b.pos.within(Task::Scout(room).range_of(), at))
            .map(|_| room),
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
                let rooms = atlas.route(&BTreeSet::new(), tick::SIEGE_HOPS, b.pos.room, at.room)?;
                Some(Atlas::crossing_ticks() * (rooms.len() as i32 - 1))
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
    out.into_iter()
        .map(|(name, room)| (name, task::task_id(&Task::Scout(room))))
        .collect()
}

fn ring_member(derived: &Derived, task: &Task, at: &Pos) -> Option<Task> {
    let Task::Refill(id, r) = task else {
        return None;
    };
    let g = (derived.grounds.iter()).find(|g| &super::jobs::ring_id(g.room) == id)?;
    (g.refills.iter())
        .filter(|f| !f.kind.is_tower() && f.free > 0)
        .min_by_key(|f| (at.range(&f.pos).unwrap_or(i32::MAX), f.id.clone()))
        .map(|f| Task::Refill(f.id.clone(), r.clone()))
}

fn spot_of(derived: &Derived, task: &Task) -> Option<Pos> {
    let id = match task {
        Task::Scout(room) => return Some(Pos::new(*room, 25, 25)),
        Task::Harvest(id)
        | Task::Withdraw(id, _)
        | Task::Pickup(id, _)
        | Task::Refill(id, _)
        | Task::Build(id)
        | Task::Repair(id)
        | Task::Upgrade(id)
        | Task::Reserve(id)
        | Task::Claim(id) => id,
    };
    let claim = derived.claims.iter().filter(|c| &c.id == id).map(|c| c.pos);
    derived
        .grounds
        .iter()
        .find_map(|g| {
            let ctrl = g.controller.iter().filter(|c| &c.id == id).map(|c| c.pos);
            (g.sources.iter().filter(|x| &x.id == id).map(|x| x.pos))
                .chain(g.minerals.iter().filter(|x| &x.id == id).map(|x| x.pos))
                .chain(g.stores.iter().filter(|x| &x.id == id).map(|x| x.pos))
                .chain(g.refills.iter().filter(|x| &x.id == id).map(|x| x.pos))
                .chain(g.sites.iter().filter(|x| &x.id == id).map(|x| x.pos))
                .chain(g.repairs.iter().filter(|x| &x.id == id).map(|x| x.pos))
                .chain(g.piles.iter().filter(|x| &x.id == id).map(|x| x.pos))
                .chain(g.labs.iter().filter(|x| &x.id == id).map(|x| x.pos))
                .chain(g.links.iter().filter(|x| &x.id == id).map(|x| x.pos))
                .chain(g.terminal.iter().filter(|x| &x.id == id).map(|x| x.pos))
                .chain(g.boost_lab.iter().filter(|x| &x.id == id).map(|x| x.pos))
                .chain(g.ores.iter().filter(|x| &x.0 == id).map(|x| x.1))
                .chain(ctrl)
                .next()
        })
        .or_else(|| claim.clone().next())
}

/// What a store, link, lab or container holds of `r`: the engine refuses a
/// draw from nothing.
fn holds(derived: &Derived, id: &str, r: &Resource) -> i32 {
    derived
        .grounds
        .iter()
        .map(|g| {
            let stored = (g.stores.iter())
                .filter(|s| s.id == id && r.is_energy())
                .map(|s| s.energy)
                .sum::<i32>();
            let linked = (g.links.iter())
                .filter(|l| l.id == id && r.is_energy())
                .map(|l| l.energy)
                .sum::<i32>();
            let ores = (g.ores.iter())
                .filter(|o| o.0 == id && &o.2 == r)
                .map(|o| o.3)
                .sum::<i32>();
            let labs = (g.labs.iter())
                .filter(|l| l.id == id)
                .filter_map(|l| l.held.as_ref().filter(|h| &h.0 == r).map(|h| h.1))
                .sum::<i32>();
            let mined = (g.minerals.iter())
                .filter(|m| m.container.as_deref() == Some(id) && &m.resource == r)
                .map(|m| m.held)
                .sum::<i32>();
            let terminal = (g.terminal.iter())
                .filter(|t| t.id == id)
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

fn act_of(derived: &Derived, task: &Task) -> Option<CreepAct> {
    let held = |id: &str| {
        derived.grounds.iter().any(|g| {
            g.controller
                .as_ref()
                .is_some_and(|c| c.id == id && derived::invader_held(c))
        })
    };

    Some(match task {
        Task::Harvest(id) => CreepAct::Harvest(id.clone()),
        Task::Withdraw(id, r) => CreepAct::Withdraw(id.clone(), r.clone()),
        Task::Pickup(id, _) => CreepAct::Pickup(id.clone()),
        Task::Refill(id, r) => CreepAct::Transfer(id.clone(), r.clone()),
        Task::Build(id) => CreepAct::Build(id.clone()),
        Task::Repair(id) => CreepAct::Repair(id.clone()),
        Task::Upgrade(id) => CreepAct::Upgrade(id.clone()),
        Task::Reserve(id) if held(id) => CreepAct::Unreserve(id.clone()),
        Task::Reserve(id) => CreepAct::Reserve(id.clone()),
        Task::Claim(id) => CreepAct::ClaimController(id.clone()),
        Task::Scout(_) => return None,
    })
}

/// Every body with room and no other resource aboard picks up dropped energy
/// beside it, beside its Task's own act: the engine's pickup conflicts with no
/// other, and a pile no Task reaches decays. Not a harvester, whose dig falls
/// where it stands. `taken`: the bodies whose act already fills them.
fn pickups(derived: &Derived, taken: &BTreeSet<String>) -> Vec<Intent> {
    let mut picks = Vec::new();

    for b in &derived.bodies {
        if b.free > 0
            && !b.pattern.is_harvester()
            && !taken.contains(&b.name)
            && b.store.held.iter().all(|(r, _)| r.is_energy())
        {
            let pile = derived.grounds.iter().enumerate().find_map(|(gi, g)| {
                let colony = (gi < derived.colonies.len()).then_some(gi);
                let _ = colony;
                g.piles
                    .iter()
                    .find(|p| p.resource.is_energy() && b.pos.within(1, &p.pos))
            });

            if let Some(p) = pile {
                picks.push(Intent::Creep(
                    b.name.clone(),
                    CreepAct::Pickup(p.id.clone()),
                ));
            }
        }
    }

    picks
}

/// The smallest send: the engine credits `floor(amount × 0.97)`, so a send in
/// whole hundreds loses the 3% and nothing to the rounding.
const LINK_BATCH: i32 = 100;

/// The sources' links first, so the hub's never takes their room. A send need
/// not wait to grow: the loss is the same share of any batch.
fn link_sends(sends: &mut Vec<Intent>, ground: &Ground) {
    let Some(receiver) = &ground.receiver else {
        return;
    };

    let mut room = receiver.free;

    let mut links: Vec<&LinkFact> = ground.links.iter().collect();
    links.sort_by_key(|l| !l.role.is_source());
    for l in links {
        let amount = room.min(l.energy) / LINK_BATCH * LINK_BATCH;

        if l.id != receiver.id && l.cooldown == 0 && amount > 0 {
            room -= amount;
            sends.push(Intent::Link(
                l.id.clone(),
                LinkAct::TransferEnergy(receiver.id.clone(), amount),
            ));
        }
    }
}

/// One send a tick off cooldown: what the terminal holds of the first
/// resource still owed, as much as its energy pays for and the receiver has
/// room for, never under the engine's least. Energy sent leaves the fee of
/// what is still owed.
fn terminal_send(g: &Ground) -> Option<Intent> {
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
            .then(|| Intent::Terminal(t.id.clone(), TerminalAct::Send(ore.clone(), amount, *home)))
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
fn dug_out(derived: &Derived) -> BTreeSet<String> {
    derived
        .grounds
        .iter()
        .flat_map(|g| {
            let sources = (g.sources.iter())
                .filter(|s| s.energy == 0)
                .map(|s| s.id.clone());
            let full = |m: &&derived::MineralFact| {
                m.cooldown > 0
                    || engine::CONTAINER_CAPACITY - m.held
                        < bodies::MINER_WORK * engine::MINERAL_HARVEST_PER_WORK
            };
            sources.chain(g.minerals.iter().filter(full).map(|m| m.id.clone()))
        })
        .collect()
}

fn sends(derived: &Derived) -> Vec<Intent> {
    let mut sends: Vec<Intent> = Vec::new();
    for g in &derived.grounds {
        link_sends(&mut sends, g);
        if let Some(send) = terminal_send(g) {
            sends.push(send);
        }
        if let Some(plan) = &g.lab_plan {
            let lab = |id: &str| g.labs.iter().find(|l| l.id == id);
            let held = |id: &str| lab(id).and_then(|l| l.held.clone());
            let cooldown = |id: &str| lab(id).map_or(0, |l| l.cooldown);
            for (output, a, b) in super::labs::ready_runs(plan, &held, &cooldown) {
                sends.push(Intent::Lab(output, LabAct::RunReaction(a, b)));
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
fn dig_seats(atlas: &Atlas, derived: &Derived, id: &str, at: &Pos) -> Vec<Pos> {
    let post = (derived.grounds.iter())
        .flat_map(|g| g.sources.iter())
        .find(|s| s.id == id)
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
            Some(dig_seats(atlas, derived, id, &at)).filter(|s| !s.is_empty())
        }
        (_, Task::Reserve(_) | Task::Claim(_)) => reserve_seats(atlas, derived, &at),
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
            let tiles = if open.is_empty() { seats.clone() } else { open };
            Target {
                room: at.room,
                tiles: tiles.iter().map(tiles::of_pos).collect(),
                range: 0,
                avoid: BTreeSet::new(),
                max_hops: tick::REACH_HOPS,
            }
        }
        // A scout walks as far as its choice reached (`scouting`).
        None if matches!(task, Task::Scout(_)) => Target {
            max_hops: tick::SIEGE_HOPS,
            ..tick::target_at(&at, task.range_of())
        },
        None => tick::target_at(&at, task.range_of()),
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
    acts: &mut Vec<Intent>,
) -> Wish {
    let ring = ring_member(derived, task, &creep.pos);
    let task = ring.as_ref().unwrap_or(task);
    let Some(at) = spot_of(derived, task) else {
        return plan::stay(&*atlas, creep, mover.factor, 1);
    };
    let (target, seats) = target_of(&*atlas, derived, (slot, task, at), stood, creep);
    let buffer = (derived.grounds.iter())
        .filter(|g| g.room == creep.pos.room)
        .find_map(|g| g.buffer.as_ref());
    let beside = buffer.filter(|b| b.energy > 0 && creep.pos.within(1, &b.pos));
    // The engine withdraws before it upgrades, so the draw tops up the spend.
    if let Some(b) = beside
        && task.is_upgrade()
        && body.free > 0
        && body.energy < body.work * engine::UPGRADE_DRAIN_PER_WORK
    {
        acts.push(Intent::Creep(
            creep.name.clone(),
            CreepAct::Withdraw(b.id.clone(), Resource::Energy),
        ));
    }
    let digs = matches!(slot.job, Job::Dig(_));
    if let Some(act) = raising(derived, body, slot) {
        acts.push(Intent::Creep(creep.name.clone(), act));
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
            Task::Withdraw(id, r) => holds(derived, id, r) > 0 && body.free > 0,
            Task::Pickup(..) => body.free > 0,
            Task::Refill(_, r) => body.store.amount_of(r) > 0,
            _ => true,
        };
        if arrived
            && can
            && let Some(act) = act_of(derived, task)
        {
            acts.push(Intent::Creep(creep.name.clone(), act));
        }
    }
    if digs && let Some(g) = derived.grounds.iter().find(|g| g.room == creep.pos.room) {
        let link = (g.links.iter()).find(|l| {
            g.receiver.is_some() && l.role.is_source() && l.free > 0 && creep.pos.within(1, &l.pos)
        });
        if let Some(l) = link {
            if body.energy > 0 {
                acts.push(Intent::Creep(
                    creep.name.clone(),
                    CreepAct::Transfer(l.id.clone(), Resource::Energy),
                ));
            }
            let under = (g.stores.iter()).find(|s| s.pos == creep.pos && s.energy > 0);
            if let Some(c) = under.filter(|_| body.free > 0) {
                acts.push(Intent::Creep(
                    creep.name.clone(),
                    CreepAct::Withdraw(c.id.clone(), Resource::Energy),
                ));
            }
        }
    }
    plan::toward_round(atlas, creep, mover, &target, weight_of(&slot.job), danger)
}

pub fn decide(context: &mut Context, state: &mut State) -> Result<Vec<Intent>, String> {
    // `{ World with Creeps = filtered }`: a creep a fight holds is not in the
    // economy, and only the creep list differs.
    let seen: Vec<&Creep> = (context.world.creeps.iter())
        .filter(|c| !tick::fights(&c.name))
        .collect();

    let derived = derived::of_world(
        context.world,
        &seen,
        context.atlas,
        context.declarations,
        context.derived,
        context.intel,
        &state.scouts,
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
    let slots = slots::plan(context.atlas, &derived, &owners);
    let (assigned, holders) = slots::assign(&*context.atlas, &derived, &slots, &state.assignments);
    let (casts, short) = slots::casts(&*context.atlas, &derived, &slots, &holders);

    let mut intents: Vec<Intent> = (casts.iter())
        .map(|cast| {
            let rank = if cast.floor {
                Rank::Floor
            } else {
                Rank::Economy
            };
            Intent::Queue(
                cast.colony,
                Ask {
                    body: cast.body.clone(),
                    name: cast.name.clone(),
                    rank,
                },
            )
        })
        .collect();
    intents.extend(
        short
            .into_iter()
            .map(|(room, p, n)| Intent::Short(room, p, n)),
    );

    let scouted = scouting(&*context.atlas, &derived, &state.assignments);
    let dug = dug_out(&derived);
    let ready = |task: &Task| match task {
        Task::Harvest(id) => !dug.contains(id),
        _ => true,
    };
    let stood: BTreeSet<Pos> = seen.iter().map(|c| c.pos).collect();
    let bodies: BTreeMap<&str, &Body> = (derived.bodies.iter())
        .map(|b| (b.name.as_str(), b))
        .collect();

    // A body holding a task books its load before another picks.
    let kept = |name: &str| {
        (state.assignments.get(name))
            .and_then(|v| slots::task_of(v))
            .and_then(task::of_task_id)
    };
    let held: Vec<(usize, &Body, Option<Task>)> = (assigned.iter())
        .filter_map(|(name, si)| Some((*si, *bodies.get(name.as_str())?, kept(name))))
        .collect();
    let mut jobs = Jobs::new(&*context.atlas, &derived, (&slots, &owners), &held);
    let first = |c: &&Creep| {
        let task = kept(&c.name);
        let slot = assigned.get(&c.name).map(|si| &slots[*si]);
        let counted = (slot.zip(task.as_ref())).is_some_and(|(s, t)| jobs.counted(s, &c.pos, t));
        (task.is_none(), !counted, c.name.clone())
    };
    let mut order: Vec<&Creep> = seen.clone();
    order.sort_by_key(first);

    // A scout walks where `scouting` sends it, with a slot to count it or
    // without, as where no colony casts one.
    let strays: BTreeMap<&str, Slot> = (derived.bodies.iter())
        .filter(|b| b.pattern.is_scout() && !assigned.contains_key(&b.name))
        .filter_map(|b| {
            let Task::Scout(room) = task::of_task_id(scouted.get(&b.name)?)? else {
                return None;
            };
            let slot = Slot {
                id: String::new(),
                colony: 0,
                room,
                job: Job::Scout(room),
                seat: Pos::new(room, 25, 25),
                body: Vec::new(),
                fed_by: None,
                spends: false,
                rank: 0,
                closed: false,
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
                    Job::Scout(_) => scouted.get(&creep.name).and_then(|id| task::of_task_id(id)),
                    _ => jobs.task(slot, body, kept(&creep.name)),
                };
                (slot, body, task)
            });
            (creep, at)
        })
        .collect();
    // The booking order is not the wishes' order: the mover settles a tie
    // by the order it is asked.
    let mut tasks = tasks;
    tasks.sort_by_key(|(c, _)| (kept(&c.name).is_none(), c.name.clone()));

    let mut next = Assignments::new();
    let mut acts: Vec<Intent> = Vec::new();
    let mut wishes: Vec<(RoomName, Vec<Wish>)> = Vec::new();
    for (creep, at) in tasks {
        let mover = movers[creep.name.as_str()];
        let danger = context.derived.danger(creep.pos.room);
        let Some((slot, body, task)) = at else {
            // What a body without a slot holds is spent where it stands.
            let controller = (derived.grounds.iter())
                .filter_map(|g| g.controller.as_ref())
                .find(|c| c.owner.is_ours() && creep.pos.within(task::RANGE_UPGRADE, &c.pos));
            if let Some(c) = controller.filter(|_| creep.store.amount_of(&Resource::Energy) > 0)
                && creep
                    .body
                    .iter()
                    .any(|p| p.kind == BodyPart::Work && p.hits > 0)
            {
                acts.push(Intent::Creep(
                    creep.name.clone(),
                    CreepAct::Upgrade(c.id.clone()),
                ));
            }
            let wish = home(context.atlas, &derived, creep, mover, danger);
            wish_in(&mut wishes, creep.pos.room, wish);
            continue;
        };
        if slot.closed {
            next.insert(creep.name.clone(), format!("{}|", slot.id));
            let wish = home(context.atlas, &derived, creep, mover, danger);
            wish_in(&mut wishes, creep.pos.room, wish);
            continue;
        }
        let held = task.as_ref().map_or(String::new(), task::task_id);
        next.insert(creep.name.clone(), format!("{}|{held}", slot.id));

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
            next.insert(b.name.clone(), format!("{}|", slots[*si].id));
        }
    }
    for c in &casts {
        next.insert(c.name.clone(), format!("{}|", c.slot));
    }

    let moves = (wishes.into_iter())
        .flat_map(|(room, list)| list.into_iter().map(move |w| Intent::Wish(room, w)));
    let taken: BTreeSet<String> = (acts.iter())
        .filter_map(|act| match act {
            Intent::Creep(
                name,
                CreepAct::Pickup(_) | CreepAct::Withdraw(..) | CreepAct::Harvest(_),
            ) => Some(name.clone()),
            _ => None,
        })
        .collect();
    intents.extend(acts);
    intents.extend(pickups(&derived, &taken));
    intents.extend(sends(&derived));
    intents.extend(moves);

    state.assignments = next;
    Ok(intents)
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
    plan::toward_round(atlas, creep, mover, &tick::target_at(&home, 1), 1, danger)
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
            if let Some(site) = g.sites.iter().find(|s| s.pos == *at) {
                return Some(CreepAct::Build(site.id.clone()));
            }
            (g.repairs.iter()).find(|r| r.pos == *at && r.kind.is_container())
        }
        Job::Upgrade => {
            (g.repairs.iter()).find(|r| g.buffer.as_ref().is_some_and(|b| b.id == r.id))
        }
        _ => None,
    };
    kept.filter(|r| 2 * r.hits < r.hits_max)
        .map(|r| CreepAct::Repair(r.id.clone()))
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::*;
    use crate::core::economy::dice::{self, Dice};
    use crate::core::state::State;

    #[test]
    fn decide_never_faults_and_holds_a_slot_and_a_task_it_can_read() {
        let mut dice = Dice::new(51);

        for _ in 0..100 {
            let g = dice::world(&mut dice);
            let mut atlas = Atlas::of_world(&g.world);
            let sightings = BTreeMap::new();
            let mut declarations = tick::undeclared();
            declarations.outposts = g.worked.clone();
            declarations.claims = g.claims.clone();
            let derived = tick::derive(&g.world, &mut atlas, &declarations, &sightings);

            let mut context = Context {
                world: &g.world,
                atlas: &mut atlas,
                declarations: &declarations,
                sightings: &sightings,
                intel: &BTreeMap::new(),
                derived: &derived,
                trace: false,
            };

            let mut state = State::empty();
            decide(&mut context, &mut state).expect("a world decides");
            let intents = decide(&mut context, &mut state).expect("a world decides again");

            for (name, held) in &state.assignments {
                assert!(!slots::slot_of(held).is_empty(), "{name} {held}");
                assert!(
                    slots::task_of(held).is_none_or(|t| task::of_task_id(t).is_some()),
                    "{name} {held}"
                );
            }

            for intent in &intents {
                if let Intent::Queue(_, ask) = intent {
                    assert!(ask.body.len() <= crate::core::engine::MAX_BODY_PARTS);
                }
            }

            let wished: BTreeSet<String> = intents
                .iter()
                .filter(|i| i.is_wish())
                .map(crate::core::intents::Intent::actor)
                .collect();

            let fought: Vec<String> = g
                .world
                .creeps
                .iter()
                .filter(|c| tick::fights(&c.name))
                .map(|c| c.name.clone())
                .collect();

            assert_eq!(
                wished.len() + fought.len(),
                g.world.creeps.len(),
                "every body not in a fight wishes once"
            );
        }
    }
}
