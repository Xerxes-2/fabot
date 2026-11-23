/// What a body does in its slot this tick: the one load it fetches or
/// delivers, the site it builds. A task held last tick is kept while it can
/// still be done, so a walk is not turned mid-way.
use std::collections::BTreeMap;

use fabot_atlas::Atlas;
use fabot_base::HashMap;
use fabot_base::engine;
use fabot_base::vocabulary::ResourceType::{self, Energy};
use fabot_base::world::{Built, Id, Pos, StructureType};

use super::bodies::Pattern;
use super::derived::{Body, Derived, Ground, StoreKind, TerminalFact};
use super::slots::{self, Job, Slot};
use super::task::{Target, Task};

/// Hivemind's task providers (`dispatcher/resource-destination/*`,
/// `dispatcher/resource-source/*`) that `rank` prices.
#[derive(Clone, Copy, Debug, PartialEq)]
enum Of {
    Ring { spawn: bool },
    Tower,
    Lab,
    Reagent,
    Terminal { fee: bool },
    Feed { drink: i32, capacity: i32 },
    Hub,
    Worker { builds: bool },
    Bank,
    Container,
    Drop,
    Loot,
    Stock { pile: bool },
    Ore,
    Link,
    Mineral,
    Drain,
    Received,
}

#[derive(Clone, Debug)]
struct Spot {
    task: Task,
    pos: Pos,
    amount: i32,
    of: Of,
}

pub struct Jobs<'a> {
    atlas: &'a Atlas,
    derived: &'a Derived<'a>,
    sinks: Vec<Vec<Spot>>,
    takes: Vec<Vec<Spot>>,
    booked_load: HashMap<Task, i32>,
    digging: HashMap<Id, i32>,
    /// Per colony: a carrier stands, so a worker refills the ring only
    /// without one.
    carried: Vec<bool>,
    /// Worker id to the carrier holding a load for it.
    booked: BTreeMap<Id, Id>,
    hauls: Vec<i32>,
    owners: &'a [Option<usize>],
}

pub fn fee_short(ground: &Ground) -> i32 {
    (ground.terminal.as_ref()).map_or(0, |t| 0.max(ground.shipped() - t.store.amount_of(Energy)))
}

enum Supply {
    Own,
    Draw(Task),
    Meet(Id),
    Wait(Option<Task>),
}

/// Each worked room but a raised claim, and the colony whose builders serve it.
pub fn served<'d>(
    derived: &'d Derived,
    owners: &'d [Option<usize>],
) -> impl Iterator<Item = (&'d Ground, usize)> {
    let worked = (derived.grounds.iter().zip(owners)).skip(derived.colonies.len());
    (worked.filter(|(g, _)| g.controller.as_ref().is_none_or(|c| !c.owner.is_ours())))
        .filter_map(|(g, o)| Some((g, (*o)?)))
}

/// A home worker its colony's carriers bring loads to, by its id: one that
/// banks on a pile, as Hivemind's do until a storage; a crew digs its own.
fn brought(derived: &Derived, slot: &Slot, body: &Body) -> Option<Id> {
    let g = &derived.grounds[slot.colony];
    let home = slot.room == g.room && body.pos.room == g.room;
    let worker = matches!(slot.job, Job::Work(_)) && !slot.digs();
    body.id.filter(|_| home && worker && g.banks_on_pile())
}

/// What the source links send the controller's link a tick, read off the
/// structures, not the harvesters, whose walks and successions would flip the
/// feed's sink.
fn inflow(g: &Ground) -> i32 {
    match &g.receiver {
        Some(r) if r.role.is_controller() => (g.sources.iter())
            .filter(|s| s.link.is_some())
            .map(|s| s.dug)
            .sum(),
        _ => 0,
    }
}

fn drink(derived: &Derived, g: &Ground) -> i32 {
    let level = g.controller.as_ref().map_or(0, |c| c.level);
    (derived.bodies.iter())
        .filter(|b| b.pos.room == g.room && b.pattern == Pattern::Upgrader)
        .map(|b| b.work * engine::UPGRADE_DRAIN_PER_WORK)
        .sum::<i32>()
        .min(super::bodies::upgradable_at(level))
}

fn hub_fed(g: &Ground) -> bool {
    (g.feed.as_ref()).is_some_and(|f| {
        g.links
            .iter()
            .any(|l| f.id == l.id.into() && l.role.is_hub())
    })
}

/// While carriers bring the feed, the controller's buffer is the upgraders'
/// alone (§5): a worker's draw from it would be carried back by hand from the
/// stock.
fn hand_fed(derived: &Derived, g: &Ground) -> bool {
    if hub_fed(g) {
        let net = g.link_net();
        return net.energy < net.least;
    }
    g.feed.as_ref().is_some_and(|f| {
        let linked = g.receiver.as_ref().is_some_and(|r| f.id == r.id.into());
        drink(derived, g) > if linked { inflow(g) } else { 0 }
    })
}

fn sinks_of(derived: &Derived, ci: usize) -> Vec<Spot> {
    use StructureType::*;
    let g = &derived.grounds[ci];
    let mut out = Vec::new();
    let mut add = |id: Target, pos: Pos, r: ResourceType, amount: i32, of: Of| {
        if amount > 0 {
            out.push(Spot {
                task: Task::Refill(id, r),
                pos,
                amount,
                of,
            });
        }
    };
    // The spawn and its extensions are one sink, so a carrier walks the ring
    // and never re-chooses within it.
    let (towers, ring): (Vec<_>, Vec<_>) = g.refills.iter().partition(|r| r.kind == Tower);
    if let Some(spawn) = derived.colonies.get(ci).and_then(|c| c.spawns.first()) {
        let open: Vec<_> = ring.iter().filter(|r| r.free > 0).collect();
        let of = Of::Ring {
            spawn: open.iter().any(|r| r.kind == Spawn),
        };
        let free = open.iter().map(|r| r.free).sum();
        add(Target::Ring(g.room), spawn.pos, Energy, free, of);
    }
    for r in towers {
        add(r.id.into(), r.pos, Energy, r.free, Of::Tower);
    }
    if let Some(b) = &g.boost_lab {
        add(b.id.into(), b.pos, Energy, b.short.energy, Of::Lab);
        add(
            b.id.into(),
            b.pos,
            b.short.compound,
            b.short.mineral,
            Of::Lab,
        );
    }
    if let Some(t) = &g.terminal {
        let fee = Of::Terminal { fee: true };
        add(
            t.id.into(),
            t.pos,
            Energy,
            fee_short(g).min(t.store.free()),
            fee,
        );
        for (ore, n) in g
            .shipment
            .iter()
            .filter(|(r, _)| *r != Energy || g.abandoned)
        {
            let n = (n - t.store.amount_of(*ore)).min(t.store.free());
            add(t.id.into(), t.pos, *ore, n, Of::Terminal { fee: false });
        }
    }
    if hand_fed(derived, g)
        && let Some(f) = &g.feed
    {
        let of = if hub_fed(g) {
            Of::Hub
        } else {
            Of::Feed {
                drink: drink(derived, g),
                capacity: f.energy + f.free,
            }
        };
        add(f.id, f.pos, Energy, f.free, of);
    }
    if let Some(plan) = &g.lab_plan {
        for (lab, reagent, room) in &plan.fills {
            if let Some(l) = g.labs.iter().find(|l| &l.id == lab) {
                add((*lab).into(), l.pos, *reagent, *room, Of::Reagent);
            }
        }
    }
    if !g.abandoned
        && let Some(s) = g.deposit()
    {
        add(s.id, s.pos, Energy, s.free, Of::Bank);
        // A pile takes a mineral too, so no carrier keeps one it cannot place.
        for r in ores_aboard(derived) {
            add(s.id, s.pos, r, s.free, Of::Bank);
        }
    }
    out
}

fn ores_aboard(derived: &Derived) -> Vec<ResourceType> {
    let held = derived.bodies.iter().flat_map(|b| &b.store.held);
    super::derived::once(held.map(|(r, _)| *r).filter(|r| *r != Energy))
}

/// What a source's container keeps for the bay its harvester refills (main's):
/// the carriers take only the rest.
fn bay(g: &Ground, id: Target) -> i32 {
    (g.sources.iter())
        .filter(|s| s.container.is_some() && s.container == id.object())
        .map(|s| s.bay)
        .sum()
}

fn takes_of(derived: &Derived, ci: usize, outposts: &[&Ground], short: i32) -> Vec<Spot> {
    let g = &derived.grounds[ci];
    let mut out = Vec::new();
    let mut add = |task: Task, pos: Pos, amount: i32, of: Of| {
        if amount > 0 {
            out.push(Spot {
                task,
                pos,
                amount,
                of,
            });
        }
    };
    let energy = |id: Target| Task::Withdraw(id, Energy);
    for o in outposts {
        for st in o.stores.iter().filter(|s| s.source_dug > 0) {
            add(energy(st.id), st.pos, st.energy, Of::Container);
        }
        let posted = |p: &Pos| {
            o.sources
                .iter()
                .any(|s| p.within(1, &s.post.unwrap_or(s.pos)))
        };
        for p in o
            .piles
            .iter()
            .filter(|p| p.resource == Energy && posted(&p.pos))
        {
            let task = Task::Pickup(p.id.into(), p.resource);
            add(task, p.pos, p.amount, Of::Drop);
        }
    }
    let linked = |id: Target| g.links.iter().any(|l| id == l.id.into());
    for st in (g.stores.iter()).filter(|s| s.source_dug > 0 && !s.kind.is_buffer() && !linked(s.id))
    {
        add(
            energy(st.id),
            st.pos,
            st.energy - bay(g, st.id),
            Of::Container,
        );
    }
    if let Some(s) = g.bank() {
        let of = Of::Stock {
            pile: s.kind.is_pile(),
        };
        add(energy(s.id), s.pos, s.energy.min(short), of);
    }
    // Sent energy is banked where the stock is drawn, out of the minerals' room.
    let sent = |o: &Ground| {
        o.ship_to.is_some_and(|(to, _)| to == g.room)
            && o.shipment.iter().any(|(r, _)| *r == Energy)
    };
    let banks_here = |t: &&TerminalFact| g.deposit().is_some_and(|d| d.id == t.id.into());
    if let Some(t) = (g.terminal.as_ref())
        .filter(|t| g.ship_to.is_none() && derived.grounds.iter().any(sent) && !banks_here(t))
    {
        let n = t.store.amount_of(Energy);
        add(energy(t.id.into()), t.pos, n, Of::Received);
    }
    let net = g.link_net();
    for l in g
        .links
        .iter()
        .filter(|l| l.role.is_hub() && net.energy > net.most)
    {
        add(energy(l.id.into()), l.pos, l.energy, Of::Link);
    }
    for (store, pos, r, n) in &g.ores {
        add(Task::Withdraw((*store).into(), *r), *pos, *n, Of::Ore);
    }
    if let Some(plan) = &g.lab_plan {
        for (lab, mineral) in &plan.drains {
            if let Some(l) = g.labs.iter().find(|l| &l.id == lab) {
                let held = l.held.as_ref().map_or(0, |h| h.1);
                add(
                    Task::Withdraw((*lab).into(), *mineral),
                    l.pos,
                    held,
                    Of::Drain,
                );
            }
        }
    }
    for m in &g.minerals {
        if let Some(id) = &m.container {
            let task = Task::Withdraw((*id).into(), m.resource);
            add(task, m.post.unwrap_or(m.pos), m.held, Of::Mineral);
        }
    }
    for p in g.piles.iter().filter(|p| p.resource == Energy) {
        let task = Task::Pickup(p.id.into(), p.resource);
        add(task, p.pos, p.amount, Of::Drop);
    }
    for s in g.stores.iter().filter(|s| s.kind.is_remains()) {
        add(energy(s.id), s.pos, s.energy, Of::Drop);
    }
    // Hivemind's `drop` and `grave` sources; a raid stands in for its danger matrix.
    let free = g
        .deposit()
        .filter(|b| !b.kind.is_pile())
        .map_or(0, |b| b.free);
    for o in std::iter::once(g)
        .chain(outposts.iter().copied())
        .filter(|o| !o.raided)
    {
        let piles = (o.piles.iter())
            .filter(|p| p.resource != Energy)
            .map(|p| (Task::Pickup(p.id.into(), p.resource), p.pos, p.amount));
        let graves =
            (o.loot.iter()).map(|(id, pos, r, n)| (Task::Withdraw((*id).into(), *r), *pos, *n));
        for (task, pos, n) in piles
            .chain(graves)
            .filter(|(_, _, n)| (10..=free).contains(n))
        {
            add(task, pos, n, Of::Loot);
        }
    }
    out
}

impl<'a> Jobs<'a> {
    pub fn new(
        atlas: &'a Atlas,
        derived: &'a Derived,
        (slots, owners): (&[Slot], &'a [Option<usize>]),
        held: &[(usize, &Body, Option<Task>)],
    ) -> Jobs<'a> {
        let mut sinks: Vec<Vec<Spot>> = (0..derived.colonies.len())
            .map(|ci| sinks_of(derived, ci))
            .collect();
        for (si, body, _) in held {
            let s = &slots[*si];
            if let Some(id) = brought(derived, s, body)
                && 3 * body.free >= body.store.capacity
            {
                sinks[s.colony].push(Spot {
                    task: Task::Refill(id.into(), Energy),
                    pos: body.pos,
                    amount: body.free,
                    of: Of::Worker {
                        builds: s.job == Job::Work(true),
                    },
                });
            }
        }
        let wants = |s: &Spot| {
            !matches!(s.of, Of::Bank) && matches!(&s.task, Task::Refill(_, r) if *r == Energy)
        };
        let mut short: Vec<i32> = (sinks.iter())
            .map(|s| s.iter().filter(|s| wants(s)).map(|s| s.amount).sum())
            .collect();
        for (si, body, task) in held {
            let (s, home) = (&slots[*si], derived.colonies[slots[*si].colony].room);
            let bank = derived.grounds[s.colony].bank().map(|b| b.id);
            let bound = task.as_ref().is_some_and(|t| match t {
                Task::Withdraw(id, r) => *r == Energy && Some(*id) == bank,
                t => sinks[s.colony].iter().any(|x| &x.task == t && wants(x)),
            });
            if s.job == Job::Fill && body.pos.room == home && bound {
                short[s.colony] -= body.energy;
            }
        }
        let takes = (0..derived.colonies.len())
            .map(|ci| {
                let outposts: Vec<&Ground> = (derived.grounds.iter().enumerate())
                    .skip(derived.colonies.len())
                    .filter(|(gi, _)| owners.get(*gi) == Some(&Some(ci)))
                    .map(|(_, g)| g)
                    .collect();
                takes_of(derived, ci, &outposts, short[ci])
            })
            .collect();
        let mut carried = vec![false; derived.colonies.len()];
        let mut digging: HashMap<Id, i32> = HashMap::default();
        let mut booked = BTreeMap::new();
        for (si, body, task) in held {
            let s = &slots[*si];
            match &s.job {
                Job::Fill => {
                    carried[s.colony] = true;
                    if let (Some(Task::Refill(to, _)), Some(id)) = (task, body.id)
                        && let Some(to) = to.object()
                    {
                        booked.insert(to, id);
                    }
                }
                Job::Dig(id) => *digging.entry(*id).or_default() += 1,
                _ => {}
            }
        }
        Jobs {
            atlas,
            derived,
            sinks,
            takes,
            booked_load: HashMap::default(),
            digging,
            carried,
            booked,
            hauls: (0..derived.grounds.len())
                .map(|gi| derived.haul(owners, gi))
                .collect(),
            owners,
        }
    }

    fn nearest<R: Ord, T>(&self, from: &Pos, ends: impl Iterator<Item = (R, Pos, T)>) -> Option<T> {
        slots::nearest(from, ends, |p| slots::distance(self.atlas, from, p)).map(|(end, _)| end)
    }

    /// Hivemind's (`dispatcher/resource-*`) priority and weight; `None` where
    /// its `isValid` fails, and a negative priority is never picked.
    fn rank(&self, ci: usize, s: &Spot, body: &Body, carrier: bool) -> Option<(i32, i32)> {
        let load = self.booked_load.get(&s.task).copied().unwrap_or_default();
        // What the structure lacks, before the loads booked against it.
        let lacks = s.amount + load;
        let range = s.pos.range(&body.pos).unwrap_or(50);
        let cap = body.store.capacity.max(1);
        let g = &self.derived.grounds[ci];
        let ring = self.ring_open(ci);
        Some(match s.of {
            Of::Ring { .. } if !carrier && load >= s.amount => return None,
            Of::Ring { spawn } => (
                4 + i32::from(spawn) - 3 * i32::from(load >= s.amount),
                100 * body.energy.min(s.amount) / cap + 100 - range,
            ),
            Of::Tower if 5 * lacks <= engine::TOWER_CAPACITY => (-1, 0),
            Of::Tower => (
                3 + i32::from(g.raided) + i32::from(5 * lacks > 4 * engine::TOWER_CAPACITY),
                lacks,
            ),
            Of::Lab => (3, s.amount / 30),
            Of::Reagent => {
                let held = engine::LAB_MINERAL_CAPACITY - lacks;
                let low = i32::from(held <= engine::LAB_REACTION_AMOUNT);
                let high = i32::from(5 * held > engine::LAB_MINERAL_CAPACITY);
                (3 + low - high, 100 * lacks / engine::LAB_MINERAL_CAPACITY)
            }
            Of::Terminal { fee } => (4 - i32::from(fee), s.amount),
            // A booking from afar holds the hub through the walk home; an outpost
            // load banks in the storage beside it.
            Of::Hub if s.pos.room != body.pos.room => return None,
            Of::Feed { drink, capacity } => {
                let need = s.amount + drink * range;
                if need <= 0 {
                    return None;
                }
                let short = i32::from(4 * need < 3 * capacity) + i32::from(2 * need < capacity);
                (4 - short, need)
            }
            Of::Hub => (5 - i32::from(range > 10), s.amount),
            Of::Worker { builds } => (
                2,
                if builds { 150 } else { 50 } + (100 * s.amount / body.energy.max(1)).min(100),
            ),
            Of::Bank => (0, body.store.capacity - body.free),
            Of::Container if 10 * s.amount <= cap => (-1, s.amount),
            Of::Container => {
                let busy = carrier && ring && g.has_storage();
                let full = s.amount >= body.free && !busy;
                (2 + 2 * i32::from(full), s.amount)
            }
            Of::Drop => {
                let small = i32::from(s.amount < 100) + i32::from(s.amount < 200);
                let reserve = self.derived.colonies[ci].reserve;
                let busy = carrier && ring && g.stock().is_some_and(|n| n > reserve);
                (4 - small - 2 * i32::from(busy), s.amount)
            }
            // Under the ring's open refill, so a few units of mineral never
            // keep the spawn waiting; Hivemind's priority else, its weight by 30.
            Of::Loot => (4 - 2 * i32::from(ring), 10 * s.amount / 3),
            Of::Stock { pile: false } => (2, s.amount),
            Of::Stock { pile: true } if carrier && ring => (5, 0),
            Of::Stock { pile: true } => (if carrier { 0 } else { 4 }, 0),
            Of::Ore => (0, 0),
            Of::Link => (5 - i32::from(range > 10), s.amount),
            Of::Mineral if 2 * s.amount < engine::CONTAINER_CAPACITY => (-1, 0),
            Of::Mineral => (3, 5 * s.amount),
            Of::Drain => (3, 0),
            Of::Received => (1, 0),
        })
    }

    /// The ring has room past the loads bound for it.
    fn ring_open(&self, ci: usize) -> bool {
        (self.sinks[ci].iter()).any(|s| {
            matches!(s.of, Of::Ring { .. })
                && self.booked_load.get(&s.task).copied().unwrap_or_default() < s.amount
        })
    }

    fn pick(
        &self,
        ci: usize,
        spots: &[Spot],
        (body, carrier): (&Body, bool),
        fits: &dyn Fn(&Spot) -> bool,
    ) -> Option<usize> {
        let ends = (spots.iter().enumerate())
            .filter(|(_, s)| s.amount > 0 && fits(s))
            .filter_map(|(i, s)| Some((i, self.rank(ci, s, body, carrier)?)))
            .filter(|(_, (p, _))| *p >= 0)
            .map(|(i, (p, w))| ((-p, -w), spots[i].pos, i));
        self.nearest(&body.pos, ends)
    }

    fn valid(&self, ci: usize, spots: &[Spot], task: &Task, asker: (&Body, bool)) -> Option<usize> {
        (spots.iter()).position(|s| {
            &s.task == task && s.amount > 0 && self.rank(ci, s, asker.0, asker.1).is_some()
        })
    }

    fn count(booked_load: &mut HashMap<Task, i32>, task: &Task, n: i32) {
        *booked_load.entry(task.clone()).or_default() += n;
    }

    /// The ring is held by the loads bound for it, Hivemind's way; any other
    /// spot is booked down, main's.
    fn book(booked_load: &mut HashMap<Task, i32>, spot: &mut Spot, n: i32) {
        if !matches!(spot.of, Of::Ring { .. }) {
            spot.amount -= n;
        }
        Jobs::count(booked_load, &spot.task, n);
    }

    fn deliver(
        &mut self,
        ci: usize,
        body: &Body,
        kept: Option<&Task>,
        carrier: bool,
    ) -> Option<Task> {
        let held: Vec<ResourceType> = body
            .store
            .held
            .iter()
            .filter(|(_, n)| *n > 0)
            .map(|(r, _)| *r)
            .collect();
        let kept = kept.filter(|t| matches!(t, Task::Refill(_, r) if held.contains(r)));
        let fits = |s: &Spot| {
            matches!(&s.task, Task::Refill(_, r) if held.contains(r)
                && (carrier || (!matches!(s.of, Of::Bank) || *r != Energy) && !matches!(s.of, Of::Worker { .. })))
        };
        let i = (kept.and_then(|t| self.valid(ci, &self.sinks[ci], t, (body, carrier))))
            .or_else(|| self.pick(ci, &self.sinks[ci], (body, carrier), &fits))?;
        let spot = &mut self.sinks[ci][i];
        let Task::Refill(_, r) = &spot.task else {
            return None;
        };
        Jobs::book(&mut self.booked_load, spot, body.store.amount_of(*r));
        Some(spot.task.clone())
    }

    /// Hivemind's `calculateSource`: a sink that outranks the best source is
    /// fetched for, and only so is the stock drawn.
    fn fetch(
        &mut self,
        ci: usize,
        takes: &mut [Spot],
        body: &Body,
        kept: Option<&Task>,
        fits: &dyn Fn(&Spot) -> bool,
    ) -> Option<Task> {
        let me = (body, true);
        // Hivemind drops a source's order once it withdraws: a body beside
        // its source with a load aboard has taken it.
        let taken = |s: &Spot| body.free < body.store.capacity && s.pos.within(1, &body.pos);
        let kept = kept.filter(|t| {
            t.is_intake() && (takes.iter()).any(|s| &s.task == *t && fits(s) && !taken(s))
        });
        let mut i = kept.and_then(|t| self.valid(ci, takes, t, me));
        if i.is_none() {
            let ring = self.ring_open(ci);
            let on_demand = |s: &Spot| match s.of {
                Of::Stock { pile } => !(pile && ring),
                Of::Ore => true,
                _ => false,
            };
            let of = |t: &Task| match t {
                Task::Withdraw(id, r) | Task::Pickup(id, r) => Some((*id, *r)),
                _ => None,
            };
            i = self.pick(ci, takes, me, &|s| fits(s) && !on_demand(s));
            let stocked = |r: ResourceType| {
                takes
                    .iter()
                    .any(|s| fits(s) && of(&s.task).is_some_and(|(_, x)| x == r))
            };
            let sinks = &self.sinks[ci];
            let wanted = |s: &Spot| {
                !matches!(s.of, Of::Bank) && matches!(&s.task, Task::Refill(_, r) if stocked(*r))
            };
            if let Some(d) = self.pick(ci, sinks, me, &wanted)
                && i.is_none_or(|i| {
                    self.rank(ci, &sinks[d], body, true) > self.rank(ci, &takes[i], body, true)
                })
                && let Task::Refill(to, r) = &sinks[d].task
            {
                let source =
                    |s: &Spot| fits(s) && of(&s.task).is_some_and(|(id, x)| &x == r && id != *to);
                i = self.pick(ci, takes, me, &source).or(i);
            }
        }
        let spot = &mut takes[i?];
        Jobs::book(&mut self.booked_load, spot, body.free);
        Some(spot.task.clone())
    }

    /// Hivemind's `delivering`, read off the held task.
    fn fetching(body: &Body, kept: Option<&Task>) -> bool {
        let cap = body.store.capacity;
        let used = cap - body.free;
        let ore = (body.store.held.iter()).any(|(r, n)| *r != Energy && *n > 0);
        let bringing = matches!(kept, Some(Task::Refill(_, r)) if body.store.amount_of(*r) > 0);
        !bringing
            && (10 * used <= cap && !ore || 10 * used < 9 * cap && kept.is_none_or(Task::is_intake))
    }

    pub fn task(&mut self, slot: &Slot, body: &Body, kept: Option<&Task>) -> Option<Task> {
        let ci = slot.colony;
        match &slot.job {
            Job::Dig(id) => Some(Task::Harvest((*id).into())),
            Job::Upgrade => {
                let c = self.derived.grounds[ci].controller.as_ref()?;
                Some(Task::Upgrade(c.id.into()))
            }
            Job::Reserve(id) => Some(Task::Reserve((*id).into())),
            Job::Claim(id) => Some(Task::Claim((*id).into())),
            Job::Scout(room) => Some(Task::Scout(*room)),
            Job::Fill => {
                // Energy is fetched only for a sink with room: with none, the
                // dig waits in its container for the workers, not aboard.
                let room: i32 = (self.sinks[ci].iter())
                    .filter(|s| matches!(&s.task, Task::Refill(_, r) if *r == Energy))
                    .map(|s| s.amount.max(0))
                    .sum();
                // The home's carriers are cast for its own flows and stay on
                // them, so the ring never waits on a body gone to an outpost.
                let home = self.derived.colonies[ci].room;
                let far = slots::far(slot);
                // Topped up from a store, a mineral would go back to the store
                // it came from.
                let aboard = |r: ResourceType| r != Energy && body.store.amount_of(r) > 0;
                let ore = body.store.held.iter().any(|(r, _)| aboard(*r));
                let fits = |s: &Spot| {
                    let draws = |of: &dyn Fn(ResourceType) -> bool| matches!(&s.task, Task::Withdraw(_, r) | Task::Pickup(_, r) if of(*r));
                    (far || s.pos.room == home)
                        && (room > 0 || !draws(&|r| r == Energy))
                        && (!ore || matches!(s.of, Of::Drain) && draws(&aboard))
                };
                let mut takes = std::mem::take(&mut self.takes[ci]);
                let used = body.store.capacity - body.free;
                let task = if Jobs::fetching(body, kept) {
                    (self.fetch(ci, &mut takes, body, kept, &fits))
                        .or_else(|| (used > 0).then(|| self.deliver(ci, body, kept, true))?)
                } else {
                    (self.deliver(ci, body, kept, true)).or_else(|| {
                        (body.free > 0).then(|| self.fetch(ci, &mut takes, body, kept, &fits))?
                    })
                };
                self.takes[ci] = takes;
                task
            }
            Job::Work(builds) => self.work(slot, *builds, body, kept),
        }
    }

    fn work(
        &mut self,
        slot: &Slot,
        builds: bool,
        body: &Body,
        kept: Option<&Task>,
    ) -> Option<Task> {
        let fetching = body.energy == 0
            || body.free > 0 && kept.is_some_and(|t| t.is_intake() && self.can_take(slot, t));
        if !fetching {
            return self.spend(slot, builds, body, kept);
        }
        match self.supply(slot, builds, body, kept) {
            Supply::Own => self.fuel(slot, body, kept),
            Supply::Meet(carrier) => Some(Task::Meet(carrier.into())),
            Supply::Draw(t) => Some(t),
            Supply::Wait(work) => work,
        }
    }

    /// Hivemind's delivery rule (`role/upgrader.ts`, `role/builder.ts`): a
    /// brought worker short of a load draws what stands by the controller if
    /// it upgrades, meets the carrier booked to it, waits beside a buffer
    /// carriers keep, and fetches its own with neither.
    fn supply(&mut self, slot: &Slot, builds: bool, body: &Body, kept: Option<&Task>) -> Supply {
        let Some(id) = brought(self.derived, slot, body) else {
            return Supply::Own;
        };
        let work = self.spend(slot, builds, body, kept.filter(|t| !t.is_intake()));
        let upgrades = matches!(work, Some(Task::Upgrade(_)));
        if let Some(t) = self.by_controller(slot, body, kept).filter(|_| upgrades) {
            return Supply::Draw(t);
        }
        if let Some(carrier) = self.booked.get(&id) {
            return Supply::Meet(*carrier);
        }
        let g = &self.derived.grounds[slot.colony];
        if upgrades && g.feed.is_some() {
            Supply::Wait(work)
        } else {
            Supply::Own
        }
    }

    fn by_controller(&self, slot: &Slot, body: &Body, kept: Option<&Task>) -> Option<Task> {
        let g = self.derived.grounds.iter().find(|g| g.room == slot.room)?;
        let c = g.controller.as_ref()?;
        let reach = if c.level < slots::STORAGE_LEVEL {
            10
        } else {
            3
        };
        let stores = (g.stores.iter())
            .filter(|s| match s.kind {
                StoreKind::Buffer => s.energy > 50,
                StoreKind::Pile => s.energy >= body.store.capacity && s.pos.within(reach, &c.pos),
                StoreKind::Container => {
                    s.energy > engine::CONTAINER_CAPACITY / 4 && s.pos.within(reach, &c.pos)
                }
                StoreKind::Storage | StoreKind::Remains => false,
            })
            .map(|s| (Task::Withdraw(s.id, Energy), s.pos, s.kind.is_buffer()));
        let piles = (g.piles.iter())
            .filter(|p| p.resource == Energy && p.amount >= body.store.capacity)
            .filter(|p| p.pos.within(reach, &c.pos))
            .map(|p| (Task::Pickup(p.id.into(), p.resource), p.pos, false));
        let near: Vec<(Task, Pos, bool)> = stores.chain(piles).collect();
        if let Some((t, _, _)) = near.iter().find(|(t, _, _)| Some(t) == kept) {
            return Some(t.clone());
        }
        let ends = near.into_iter().map(|(t, p, buffer)| (!buffer, p, t));
        self.nearest(&body.pos, ends)
    }

    fn spend(
        &mut self,
        slot: &Slot,
        builds: bool,
        body: &Body,
        kept: Option<&Task>,
    ) -> Option<Task> {
        use StructureType::*;
        let ci = slot.colony;
        let derived = self.derived;
        let ground = derived.grounds.iter().find(|g| g.room == slot.room)?;
        let own = derived
            .colonies
            .get(ci)
            .is_some_and(|c| c.room == slot.room);
        let controller =
            (ground.controller.as_ref()).filter(|c| c.owner.is_ours() && slots::fed(c));
        if let Some(c) = controller.filter(|c| slots::inside_deadline(c)) {
            return Some(Task::Upgrade(c.id.into()));
        }
        if own
            && !self.carried[ci]
            && let Some(t) = self.deliver(
                ci,
                body,
                kept.filter(|t| matches!(t, Task::Refill(..))),
                false,
            )
        {
            return Some(t);
        }
        let grounds: Vec<&Ground> = if own {
            let served = (served(derived, self.owners).filter(|(_, c)| *c == ci)).map(|(g, _)| g);
            std::iter::once(ground).chain(served).collect()
        } else {
            vec![ground]
        };
        // A held task spends what is aboard; below the reserve only a mend, so it ends whole, not one tick past risk.
        if let Some(t @ (Task::Build(_) | Task::Repair(_) | Task::Upgrade(_))) = kept
            && (!slot.draws_reserve() || matches!(t, Task::Repair(_)))
            && grounds.iter().any(|g| match t {
                Task::Build(id) => g.sites.iter().any(|s| *id == s.id.into()),
                Task::Repair(id) => g.repairs.iter().any(|r| *id == r.id.into()),
                Task::Upgrade(id) => controller.is_some_and(|c| *id == c.id.into()),
                _ => false,
            })
        {
            return Some(t.clone());
        }
        // A home's worker mends an outpost road at its timer: its wear is the outpost's mender's.
        let haul = |g: &Ground| {
            let gi = derived.grounds.iter().position(|x| x.room == g.room);
            gi.filter(|_| g.room == slot.room)
                .map_or(0, |gi| self.hauls[gi])
        };
        // Home first: an outpost has its own mender.
        let repair = |fits: &dyn Fn(&super::derived::RepairFact, i32) -> bool| {
            let ends = (grounds.iter())
                .flat_map(|g| {
                    let haul = haul(g);
                    g.repairs
                        .iter()
                        .filter(move |r| fits(r, haul))
                        .map(move |r| (r, haul))
                })
                .map(|(r, haul)| {
                    let rank = (r.pos.room != slot.room, r.urgency(haul));
                    (rank, r.pos, Task::Repair(r.id.into()))
                });
            self.nearest(&body.pos, ends)
        };
        if let Some(t) = repair(&|r, haul| r.urgent(haul)) {
            return Some(t);
        }
        if slot.draws_reserve() {
            return repair(&|r, haul| r.at_risk(haul));
        }
        if let Some(c) = controller.filter(|_| !builds) {
            return Some(Task::Upgrade(c.id.into()));
        }
        if let Some(t) = repair(&|r, haul| r.at_risk(haul)) {
            return Some(t);
        }
        // The links feed the controller: they go up with the extensions. The
        // buffer goes first, or a bay's sites beside it hold its upgraders off.
        // A crowded source's crews spill their dig on its post until it stands.
        let buffer = |p: &Pos| grounds.iter().any(|g| g.buffer_tile.as_ref() == Some(p));
        let crowded = |p: &Pos| {
            (grounds.iter())
                .any(|g| (g.sources.iter()).any(|s| s.post == Some(*p) && s.seats.unwrap_or(0) > 1))
        };
        let first = |s: &super::derived::SiteFact| match s.kind {
            // A rampart's site costs 1: built in a tick, it never waits on a level.
            Spawn | Rampart => 0,
            Container if buffer(&s.pos) => 0,
            Tower | Extension | Link => 1,
            Container if crowded(&s.pos) => 1,
            Container => 2,
            _ if s.kind.pays_back() => 3,
            _ => 4,
        };
        let sites = (grounds.iter()).flat_map(|g| g.sites.iter());
        let ends = sites.map(|s| ((first(s), s.pos.room != slot.room), s.pos, s.id));
        if let Some(id) = self.nearest(&body.pos, ends) {
            return Some(Task::Build(id.into()));
        }
        // A rampart to its target, anything else once it is half gone, by the
        // bodies the surplus hires for it: a floor body's pace is unpriced.
        if let Some(t) =
            repair(&|r, _| slot.spends && (r.kind == Rampart || 2 * r.hits < r.hits_max))
        {
            return Some(t);
        }
        controller.map(|c| Task::Upgrade(c.id.into()))
    }

    fn can_take(&self, slot: &Slot, t: &Task) -> bool {
        let caster = self.derived.grounds.get(slot.colony).map(|g| g.room);
        (self.derived.grounds.iter())
            .filter(|g| g.room == slot.room || Some(g.room) == caster)
            .any(|g| match t {
                Task::Withdraw(id, _) => g.stores.iter().any(|s| s.id == *id && s.energy > 0),
                Task::Pickup(id, _) => g.piles.iter().any(|p| *id == p.id.into()),
                Task::Harvest(id) => g.sources.iter().any(|s| *id == s.id.into() && s.energy > 0),
                _ => false,
            })
    }

    fn fuel(&mut self, slot: &Slot, body: &Body, kept: Option<&Task>) -> Option<Task> {
        let g = self.derived.grounds.iter().find(|g| g.room == slot.room)?;
        let seated = |id: Id| {
            (g.sources.iter()).any(|s| {
                s.id == id && self.digging.get(&s.id).copied().unwrap_or(0) < s.seats.unwrap_or(0)
            })
        };
        if let Some(t) = kept.filter(|t| t.is_intake() && self.can_take(slot, t))
            && !matches!(t, Task::Harvest(id) if !id.object().is_some_and(seated))
        {
            if let Task::Harvest(id) = t
                && let Some(id) = id.object()
            {
                *self.digging.entry(id).or_default() += 1;
            }
            Jobs::count(&mut self.booked_load, t, body.free);
            return Some(t.clone());
        }
        let ci = slot.colony;
        let reserve = self
            .derived
            .colonies
            .get(ci)
            .filter(|c| c.room == g.room && !slot.draws_reserve())
            .map_or(0, |c| match slot.job {
                Job::Work(true) => g.kept_from_builders(c.reserve),
                _ => c.reserve,
            });
        // The reserve is the towers' and the spawn's, a payback site's builder
        // aside; never half a load past it: a draw takes all it has room for.
        // Under it, what stands in a link or container is the reserve's too.
        let short = g.stock().is_some_and(|n| n < reserve);
        let fed = hand_fed(self.derived, g);
        let mut loads: Vec<(Task, Pos, i32)> = (g.stores.iter())
            .filter(|s| (!short || s.kind.is_remains()) && !(fed && s.kind.is_buffer()))
            .map(|s| {
                let spare = g.spare(s, reserve);
                let n = if g.banks(s) && spare < body.free {
                    0
                } else {
                    spare
                };
                (Task::Withdraw(s.id, Energy), s.pos, n)
            })
            .chain(
                (g.piles.iter().filter(|p| p.resource == Energy))
                    .map(|p| (Task::Pickup(p.id.into(), p.resource), p.pos, p.amount)),
            )
            .collect();
        // A pioneer brings its colony's stock past the reserve when the room
        // it raises has none at hand.
        if let (Some(home), Some(c)) = (self.derived.grounds.get(ci), self.derived.colonies.get(ci))
            && home.room != g.room
            && let Some(st) = home.stores.iter().find(|s| s.owned)
        {
            let n = Some(home.spare(st, c.reserve)).filter(|n| *n >= body.free);
            loads.push((Task::Withdraw(st.id, Energy), st.pos, n.unwrap_or(0)));
        }
        // Not Hivemind's rank: ranked, a worker waits idle on a drop another holds.
        let loads = (loads.iter())
            .filter(|(_, _, n)| *n > 0 && *n >= body.free.min(100))
            .map(|(t, p, n)| ((p.room != g.room, *n < body.free), *p, t));
        if let Some(t) = self.nearest(&body.pos, loads) {
            Jobs::count(&mut self.booked_load, t, body.free);
            return Some(t.clone());
        }
        // Beside a storage the dig is the carriers' to bank.
        if g.has_storage() {
            return None;
        }
        let open = (g.sources.iter())
            .filter(|s| s.energy > 0)
            .filter(|s| seated(s.id))
            .map(|s| ((), s.pos, s));
        let open = self.nearest(&body.pos, open)?;
        *self.digging.entry(open.id).or_default() += 1;
        Some(Task::Harvest(open.id.into()))
    }
}
