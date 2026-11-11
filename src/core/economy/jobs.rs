/// What a body does in its slot this tick: the one load it fetches or
/// delivers, the site it builds. A task held last tick is kept while it can
/// still be done, so a walk is not turned mid-way.
use crate::core::HashMap;
use crate::core::atlas::Atlas;
use crate::core::engine;
use crate::core::vocabulary::Resource;
use crate::core::world::{Kind, Pos, RoomName};

use super::derived::{Body, Derived, Ground, RAMPART_SAFE};
use super::slots::{self, Job, Slot};
use super::task::Task;

#[derive(Clone, Debug)]
struct Spot {
    task: Task,
    pos: Pos,
    amount: i32,
    tier: i32,
}

pub struct Jobs<'a> {
    atlas: &'a Atlas,
    derived: &'a Derived,
    sinks: Vec<Vec<Spot>>,
    takes: Vec<Vec<Spot>>,
    digging: HashMap<String, i32>,
    /// Per colony: a carrier stands, so a worker refills the ring only
    /// without one.
    carried: Vec<bool>,
}

/// The energy the terminal keeps for its shipment's fee, a hundred over as
/// the fee rounds up per send, and the energy it ships.
pub fn fee_short(ground: &Ground) -> i32 {
    let (Some(t), Some((home, _))) = (&ground.terminal, &ground.ship_to) else {
        return 0;
    };
    let apart = crate::core::tiles::rooms_apart(ground.room, *home);
    let fee: i32 = (ground.shipment.iter())
        .map(|(r, n)| engine::send_cost(*n, apart) + if r.is_energy() { *n } else { 0 })
        .sum();
    let kept = if fee > 0 { fee + 100 } else { 0 };
    0.max(kept - t.store.amount_of(&Resource::Energy))
}

/// The task id of a colony's spawn ring: `run` delivers it to the nearest of
/// its spawn and extensions with room.
pub fn ring_id(room: RoomName) -> String {
    format!("ring-{room}")
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
        .filter(|b| b.pos.room == g.room && b.pattern.is_upgrader())
        .map(|b| b.work * engine::UPGRADE_DRAIN_PER_WORK)
        .sum::<i32>()
        .min(super::bodies::upgradable_at(level))
}

/// While carriers bring the feed, the controller's buffer is the upgraders'
/// alone (§5): a worker's draw from it would be carried back by hand from the
/// stock.
fn hand_fed(derived: &Derived, g: &Ground) -> bool {
    g.feed.as_ref().is_some_and(|f| {
        let linked = g.receiver.as_ref().is_some_and(|r| r.id == f.id);
        drink(derived, g) > if linked { inflow(g) } else { 0 }
    })
}

fn sinks_of(derived: &Derived, ci: usize) -> Vec<Spot> {
    let g = &derived.grounds[ci];
    let mut out = Vec::new();
    let mut add = |id: &str, pos: Pos, r: Resource, amount: i32, tier: i32| {
        if amount > 0 {
            out.push(Spot {
                task: Task::Refill(id.to_string(), r),
                pos,
                amount,
                tier,
            });
        }
    };
    // The spawn and its extensions are one sink, so a carrier walks the ring
    // and never re-chooses within it.
    let (towers, ring): (Vec<_>, Vec<_>) = g.refills.iter().partition(|r| r.kind.is_tower());
    if let Some(spawn) = derived.colonies.get(ci).and_then(|c| c.spawns.first()) {
        let free = ring.iter().map(|r| r.free).sum();
        add(&ring_id(g.room), spawn.pos, Resource::Energy, free, 0);
    }
    for r in towers {
        add(&r.id, r.pos, Resource::Energy, r.free, 1);
    }
    if let Some(b) = &g.boost_lab {
        add(&b.id, b.pos, Resource::Energy, b.energy, 1);
        add(&b.id, b.pos, b.compound.clone(), b.mineral, 1);
    }
    if let Some(t) = &g.terminal {
        add(
            &t.id,
            t.pos,
            Resource::Energy,
            fee_short(g).min(t.store.free()),
            2,
        );
        for (ore, n) in g
            .shipment
            .iter()
            .filter(|(r, _)| !r.is_energy() || g.abandoned)
        {
            add(
                &t.id,
                t.pos,
                ore.clone(),
                (n - t.store.amount_of(ore)).min(t.store.free()),
                2,
            );
        }
    }
    if hand_fed(derived, g)
        && let Some(f) = &g.feed
    {
        add(&f.id, f.pos, Resource::Energy, f.free, 2);
    }
    if let Some(plan) = &g.lab_plan {
        for (lab, reagent, room) in &plan.fills {
            if let Some(l) = g.labs.iter().find(|l| &l.id == lab) {
                add(lab, l.pos, reagent.clone(), *room, 2);
            }
        }
    }
    if !g.abandoned
        && let Some(s) = g.bank()
    {
        add(&s.id, s.pos, Resource::Energy, s.free, 3);
        if s.owned {
            for r in ores_aboard(derived) {
                add(&s.id, s.pos, r, s.free, 3);
            }
        }
    }
    out
}

fn ores_aboard(derived: &Derived) -> Vec<Resource> {
    let mut out: Vec<Resource> = Vec::new();
    for b in &derived.bodies {
        for (r, _) in &b.store.held {
            if !r.is_energy() && !out.contains(r) {
                out.push(r.clone());
            }
        }
    }
    out
}

fn takes_of(
    derived: &Derived,
    ci: usize,
    (sinks, aboard): (&[Spot], i32),
    outposts: &[&Ground],
) -> Vec<Spot> {
    let g = &derived.grounds[ci];
    let mut out = Vec::new();
    let mut add = |task: Task, pos: Pos, amount: i32, tier: i32| {
        if amount > 0 {
            out.push(Spot {
                task,
                pos,
                amount,
                tier,
            });
        }
    };
    // With a stock to draw the ring is filled from it first, and the dig
    // banked after.
    let dig = i32::from(g.bank().is_some());
    for o in outposts {
        for st in o.stores.iter().filter(|s| s.source_dug > 0) {
            add(
                Task::Withdraw(st.id.clone(), Resource::Energy),
                st.pos,
                st.energy,
                dig,
            );
        }
        let posted = |p: &Pos| {
            o.sources
                .iter()
                .any(|s| s.post.is_some_and(|post| p.within(1, &post)))
        };
        for p in o
            .piles
            .iter()
            .filter(|p| p.resource.is_energy() && posted(&p.pos))
        {
            add(
                Task::Pickup(p.id.clone(), p.resource.clone()),
                p.pos,
                p.amount,
                dig,
            );
        }
    }
    let short: i32 = (sinks.iter())
        .filter(|s| s.tier < 3 && matches!(&s.task, Task::Refill(_, r) if r.is_energy()))
        .map(|s| s.amount)
        .sum::<i32>()
        - aboard;
    let feed = g.feed.as_ref().map(|f| f.id.as_str());
    let linked = |id: &str| g.links.iter().any(|l| l.id == id);
    for st in
        (g.stores.iter()).filter(|s| s.source_dug > 0 && !s.kind.is_buffer() && !linked(&s.id))
    {
        add(
            Task::Withdraw(st.id.clone(), Resource::Energy),
            st.pos,
            st.energy,
            dig,
        );
    }
    if let Some(s) = g.bank() {
        add(
            Task::Withdraw(s.id.clone(), Resource::Energy),
            s.pos,
            s.energy.min(short),
            0,
        );
    }
    for l in g
        .links
        .iter()
        .filter(|l| l.role.is_hub() && Some(l.id.as_str()) != feed)
    {
        add(
            Task::Withdraw(l.id.clone(), Resource::Energy),
            l.pos,
            l.energy,
            0,
        );
    }
    // Never drawn for a sink it is itself, or the load pours back in.
    let wanted = |r: &Resource, from: &str| {
        (sinks.iter())
            .filter(|s| s.tier < 3)
            .filter(|s| matches!(&s.task, Task::Refill(to, x) if x == r && to != from))
            .map(|s| s.amount)
            .sum::<i32>()
    };
    for (store, pos, r, n) in &g.ores {
        add(
            Task::Withdraw(store.clone(), r.clone()),
            *pos,
            (*n).min(wanted(r, store)),
            1,
        );
    }
    if let Some(plan) = &g.lab_plan {
        for (lab, mineral) in &plan.drains {
            if let Some(l) = g.labs.iter().find(|l| &l.id == lab) {
                let held = l.held.as_ref().map_or(0, |h| h.1);
                add(Task::Withdraw(lab.clone(), mineral.clone()), l.pos, held, 1);
            }
        }
    }
    for m in &g.minerals {
        if let Some(id) = &m.container {
            add(
                Task::Withdraw(id.clone(), m.resource.clone()),
                m.post.unwrap_or(m.pos),
                m.held,
                2,
            );
        }
    }
    for p in g.piles.iter().filter(|p| p.resource.is_energy()) {
        add(
            Task::Pickup(p.id.clone(), p.resource.clone()),
            p.pos,
            p.amount,
            2,
        );
    }
    for s in g.stores.iter().filter(|s| s.kind.is_remains()) {
        add(
            Task::Withdraw(s.id.clone(), Resource::Energy),
            s.pos,
            s.energy,
            2,
        );
    }
    out
}

/// What the stock's draw counts as already bound for a sink: it books its
/// sink first, or the drawn load finds the room taken and goes back in. A
/// load still walking in from an outpost keeps no sink waiting, and a
/// worker's draw is its own spending.
fn counted(derived: &Derived, sinks: &[Spot], slot: &Slot, at: &Pos, task: &Task) -> bool {
    let ci = slot.colony;
    let drawn = matches!(task, Task::Withdraw(id, r) if r.is_energy()
        && derived.grounds[ci].bank().is_some_and(|s| &s.id == id));
    slot.job == Job::Fill
        && at.room == derived.colonies[ci].room
        && (drawn || sinks.iter().any(|s| s.tier < 3 && &s.task == task))
}

fn carried(body: &Body, r: &Resource) -> i32 {
    body.store.amount_of(r)
}

impl<'a> Jobs<'a> {
    pub fn new(
        atlas: &'a Atlas,
        derived: &'a Derived,
        (slots, owners): (&[Slot], &[Option<usize>]),
        held: &[(usize, &Body, Option<Task>)],
    ) -> Jobs<'a> {
        let sinks: Vec<Vec<Spot>> = (0..derived.colonies.len())
            .map(|ci| sinks_of(derived, ci))
            .collect();
        let mut aboard = vec![0; derived.colonies.len()];
        for (si, body, task) in held {
            let ci = slots[*si].colony;
            if task
                .as_ref()
                .is_some_and(|t| counted(derived, &sinks[ci], &slots[*si], &body.pos, t))
            {
                aboard[ci] += body.energy;
            }
        }
        let takes = (0..derived.colonies.len())
            .map(|ci| {
                let outposts: Vec<&Ground> = (derived.grounds.iter().enumerate())
                    .skip(derived.colonies.len())
                    .filter(|(gi, _)| owners.get(*gi) == Some(&Some(ci)))
                    .map(|(_, g)| g)
                    .collect();
                takes_of(derived, ci, (&sinks[ci], aboard[ci]), &outposts)
            })
            .collect();
        let mut carried = vec![false; derived.colonies.len()];
        for (si, _, _) in held {
            if slots[*si].job == Job::Fill {
                carried[slots[*si].colony] = true;
            }
        }
        Jobs {
            atlas,
            derived,
            sinks,
            takes,
            digging: HashMap::default(),
            carried,
        }
    }

    pub fn counted(&self, slot: &Slot, at: &Pos, task: &Task) -> bool {
        counted(self.derived, &self.sinks[slot.colony], slot, at, task)
    }

    fn distance(&self, a: &Pos, b: &Pos) -> i32 {
        slots::distance(self.atlas, self.derived, a, b).unwrap_or(i32::MAX / 2)
    }

    fn pick(
        &self,
        spots: &[Spot],
        at: &Pos,
        (load, fetch): (i32, bool),
        fits: &dyn Fn(&Spot) -> bool,
    ) -> Option<usize> {
        (spots.iter().enumerate())
            .filter(|(_, s)| s.amount > 0 && fits(s))
            .min_by_key(|(_, s)| {
                // A carrier fetches where most loads wait, so the far dig
                // never overflows while a near one trickles.
                let loads = if fetch { s.amount / load.max(1) } else { 0 };
                (s.tier, s.amount < load, -loads, self.distance(at, &s.pos))
            })
            .map(|(i, _)| i)
    }

    fn book(spots: &mut [Spot], task: &Task, n: i32) -> bool {
        match spots.iter_mut().find(|s| &s.task == task && s.amount > 0) {
            Some(s) => {
                s.amount -= n;
                true
            }
            None => false,
        }
    }

    fn deliver(
        &mut self,
        ci: usize,
        body: &Body,
        kept: Option<&Task>,
        storage: bool,
    ) -> Option<Task> {
        let held: Vec<Resource> = body
            .store
            .held
            .iter()
            .filter(|(_, n)| *n > 0)
            .map(|(r, _)| r.clone())
            .collect();
        if let Some(t @ Task::Refill(_, r)) = kept
            && carried(body, r) > 0
            && Jobs::book(&mut self.sinks[ci], t, carried(body, r))
        {
            return Some(t.clone());
        }
        let fits = |s: &Spot| matches!(&s.task, Task::Refill(_, r) if held.contains(r) && (storage || s.tier < 3 || !r.is_energy()));
        let load = body.store.capacity - body.free;
        let i = self.pick(&self.sinks[ci], &body.pos, (load, false), &fits)?;
        let task = self.sinks[ci][i].task.clone();
        if let Task::Refill(_, r) = &task {
            self.sinks[ci][i].amount -= carried(body, r);
        }
        Some(task)
    }

    fn fetch(
        &self,
        spots: &mut [Spot],
        body: &Body,
        kept: Option<&Task>,
        fits: &dyn Fn(&Spot) -> bool,
    ) -> Option<Task> {
        if let Some(t) = kept.filter(|t| t.is_intake())
            && spots.iter().any(|s| &s.task == t && fits(s))
            && Jobs::book(spots, t, body.free)
        {
            return Some(t.clone());
        }
        let i = self.pick(spots, &body.pos, (body.free, true), fits)?;
        spots[i].amount -= body.free;
        Some(spots[i].task.clone())
    }

    fn fetching(body: &Body, kept: Option<&Task>, more: bool) -> bool {
        let used = body.store.capacity - body.free;
        used == 0
            || body.free > 0 && more && !kept.is_some_and(|t| matches!(t, Task::Refill(..)))
            || body.free > 0 && !more && used * 2 < body.store.capacity
    }

    pub fn task(&mut self, slot: &Slot, body: &Body, kept: Option<Task>) -> Option<Task> {
        let ci = slot.colony;
        let kept = kept.as_ref();
        match &slot.job {
            Job::Dig(id) => Some(Task::Harvest(id.clone())),
            Job::Upgrade => {
                let c = self.derived.grounds[ci].controller.as_ref()?;
                Some(Task::Upgrade(c.id.clone()))
            }
            Job::Reserve(id) => Some(Task::Reserve(id.clone())),
            Job::Claim(id) => Some(Task::Claim(id.clone())),
            Job::Scout(room) => Some(Task::Scout(*room)),
            Job::Fill => {
                // Energy is fetched only for a sink with room: with none, the
                // dig waits in its container for the workers, not aboard.
                let room: i32 = (self.sinks[ci].iter())
                    .filter(|s| matches!(&s.task, Task::Refill(_, r) if r.is_energy()))
                    .map(|s| s.amount.max(0))
                    .sum();
                // The home's carriers are cast for its own flows and stay on
                // them, so the ring never waits on a body gone to an outpost.
                let home = self.derived.colonies[ci].room;
                let far = slots::far(slot);
                let fits = |s: &Spot| {
                    (far || s.pos.room == home)
                        && (room > 0
                            || !matches!(&s.task, Task::Withdraw(_, r) | Task::Pickup(_, r) if r.is_energy()))
                };
                let mut takes = std::mem::take(&mut self.takes[ci]);
                let more = takes.iter().any(|s| s.amount > 0 && fits(s));
                // A fight's cast waits on the ring, never on a source's trickle.
                let ring =
                    (self.sinks[ci].iter()).any(|s| s.tier == 0 && s.amount >= body.store.capacity);
                let trickle = self.derived.grounds[ci].bank().is_some()
                    && body.free < body.store.capacity
                    && (takes.iter()).any(|s| {
                        Some(&s.task) == kept && s.pos.room == home && s.amount < body.free
                    });
                let task = if Jobs::fetching(body, kept, more) && !(ring && trickle) {
                    self.fetch(&mut takes, body, kept, &fits).or_else(|| {
                        (body.free < body.store.capacity)
                            .then(|| self.deliver(ci, body, kept, true))
                            .flatten()
                    })
                } else {
                    self.deliver(ci, body, kept, true)
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
        let ci = slot.colony;
        let derived = self.derived;
        let ground = derived.grounds.iter().find(|g| g.room == slot.room)?;
        if body.energy == 0
            || body.free > 0 && kept.is_some_and(|t| t.is_intake() && self.can_take(slot, t))
        {
            return self.fuel(slot, body, kept);
        }
        let own = derived
            .colonies
            .get(ci)
            .is_some_and(|c| c.room == slot.room);
        let controller = ground.controller.as_ref().filter(|c| c.owner.is_ours());
        if let Some(c) = controller.filter(|c| slots::inside_deadline(c)) {
            return Some(Task::Upgrade(c.id.clone()));
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
            let owners = &derived.grounds;
            std::iter::once(ground)
                .chain(owners.iter().skip(derived.colonies.len()).filter(|g| {
                    g.controller.as_ref().is_none_or(|c| !c.owner.is_ours()) && self.serves(ci, g)
                }))
                .collect()
        } else {
            vec![ground]
        };
        // What is aboard was drawn past the reserve: a held task spends it.
        if let Some(t @ (Task::Build(_) | Task::Repair(_) | Task::Upgrade(_))) = kept
            && grounds.iter().any(|g| match t {
                Task::Build(id) => g.sites.iter().any(|s| &s.id == id),
                Task::Repair(id) => g.repairs.iter().any(|r| &r.id == id),
                Task::Upgrade(id) => controller.is_some_and(|c| &c.id == id),
                _ => false,
            })
        {
            return Some(t.clone());
        }
        if let Some(c) = controller.filter(|_| !builds) {
            return Some(Task::Upgrade(c.id.clone()));
        }
        let life = f64::from(engine::CREEP_LIFETIME);
        let at_risk = |r: &&super::derived::RepairFact| {
            f64::from(r.hits) < r.decay * life || r.kind == Kind::Rampart && r.hits <= RAMPART_SAFE
        };
        let near = |pos: &Pos| (pos.room != slot.room, self.distance(&body.pos, pos));
        let repair = |fits: &dyn Fn(&&super::derived::RepairFact) -> bool| {
            (grounds.iter())
                .flat_map(|g| g.repairs.iter())
                .filter(fits)
                .min_by_key(|r| near(&r.pos))
                .map(|r| Task::Repair(r.id.clone()))
        };
        if let Some(t) = repair(&at_risk) {
            return Some(t);
        }
        // The links and the buffer feed the controller: they go up with the
        // extensions.
        // A crowded source's crews spill their dig on its post until it stands.
        let crowded = |p: &Pos| {
            grounds.iter().any(|g| {
                g.buffer_tile.as_ref() == Some(p)
                    || (g.sources.iter()).any(|s| s.post == Some(*p) && s.seats.unwrap_or(0) > 1)
            })
        };
        let first = |s: &super::derived::SiteFact| match s.kind {
            Kind::Spawn => 0,
            Kind::Extension | Kind::Link => 1,
            Kind::Container if crowded(&s.pos) => 1,
            Kind::Container | Kind::Tower => 2,
            _ => 3,
        };
        if let Some(s) = (grounds.iter())
            .flat_map(|g| g.sites.iter())
            .min_by_key(|s| (first(s), near(&s.pos)))
        {
            return Some(Task::Build(s.id.clone()));
        }
        // A rampart to its target, anything else once it is half gone, by the
        // bodies the surplus hires for it: a floor body's pace is unpriced.
        if let Some(t) =
            repair(&|r| slot.spends && (r.kind == Kind::Rampart || 2 * r.hits < r.hits_max))
        {
            return Some(t);
        }
        controller.map(|c| Task::Upgrade(c.id.clone()))
    }

    fn serves(&self, ci: usize, g: &Ground) -> bool {
        let at = g
            .controller
            .as_ref()
            .map(|c| c.pos)
            .or_else(|| g.sources.first().map(|s| s.pos));
        let homes = &self.derived.colonies;
        at.is_some_and(|at| {
            (0..homes.len())
                .filter(|i| self.derived.grounds[*i].bank().is_some())
                .min_by_key(|i| self.distance(&homes[*i].spawns[0].pos, &at))
                == Some(ci)
        })
    }

    fn can_take(&self, slot: &Slot, t: &Task) -> bool {
        let caster = self.derived.grounds.get(slot.colony).map(|g| g.room);
        (self.derived.grounds.iter())
            .filter(|g| g.room == slot.room || Some(g.room) == caster)
            .any(|g| match t {
                Task::Withdraw(id, _) => g.stores.iter().any(|s| &s.id == id && s.energy > 0),
                Task::Pickup(id, _) => g.piles.iter().any(|p| &p.id == id),
                Task::Harvest(id) => g.sources.iter().any(|s| &s.id == id && s.energy > 0),
                _ => false,
            })
    }

    fn fuel(&mut self, slot: &Slot, body: &Body, kept: Option<&Task>) -> Option<Task> {
        let g = self.derived.grounds.iter().find(|g| g.room == slot.room)?;
        if let Some(t) = kept.filter(|t| t.is_intake() && self.can_take(slot, t)) {
            if let Task::Harvest(id) = t {
                *self.digging.entry(id.clone()).or_default() += 1;
            }
            return Some(t.clone());
        }
        let ci = slot.colony;
        let reserve = self
            .derived
            .colonies
            .get(ci)
            .filter(|c| c.room == g.room && !slot.draws_reserve())
            .map_or(0, |c| c.reserve);
        let mut spots: Vec<Spot> = Vec::new();
        // A worker never draws the reserve, that is the towers' and the
        // spawn's, nor half a load past it: a draw takes all it has room for.
        // Under it, what stands in a link or container is the reserve's too.
        let short = g.stock().is_some_and(|n| n < reserve);
        let fed = hand_fed(self.derived, g);
        for s in (g.stores.iter())
            .filter(|s| (!short || s.kind.is_remains()) && !(fed && s.kind.is_buffer()))
        {
            let spare = s.spare(reserve);
            spots.push(Spot {
                task: Task::Withdraw(s.id.clone(), Resource::Energy),
                pos: s.pos,
                amount: if s.owned && spare < body.free {
                    0
                } else {
                    spare
                },
                tier: 0,
            });
        }
        for p in g.piles.iter().filter(|p| p.resource.is_energy()) {
            spots.push(Spot {
                task: Task::Pickup(p.id.clone(), p.resource.clone()),
                pos: p.pos,
                amount: p.amount,
                tier: 0,
            });
        }
        // A pioneer brings its colony's stock past the reserve when the room
        // it raises has none at hand.
        if let (Some(home), Some(c)) = (self.derived.grounds.get(ci), self.derived.colonies.get(ci))
            && home.room != g.room
            && let Some(st) = home.stores.iter().find(|s| s.owned)
        {
            spots.push(Spot {
                task: Task::Withdraw(st.id.clone(), Resource::Energy),
                pos: st.pos,
                amount: Some(st.spare(c.reserve))
                    .filter(|n| *n >= body.free)
                    .unwrap_or(0),
                tier: 1,
            });
        }
        let i = self.pick(&spots, &body.pos, (body.free, false), &|s| {
            s.amount >= body.free.min(100)
        });
        if let Some(i) = i {
            return Some(spots[i].task.clone());
        }
        // Beside a storage the dig is the carriers' to bank.
        if g.has_storage() {
            return None;
        }
        let open = (g.sources.iter())
            .filter(|s| s.energy > 0)
            .filter(|s| {
                let seats = s.seats.unwrap_or(0) - i32::from(s.post.is_some());
                self.digging.get(&s.id).copied().unwrap_or(0) < seats
            })
            .min_by_key(|s| self.distance(&body.pos, &s.pos))?;
        *self.digging.entry(open.id.clone()).or_default() += 1;
        Some(Task::Harvest(open.id.clone()))
    }
}
