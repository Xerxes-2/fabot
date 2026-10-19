/// Answers "how many" and "who casts", never "who does what": that is the
/// matcher's.
use std::cell::Cell;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::rc::Rc;

use crate::core::HashMap;
use crate::core::engine;
use crate::core::vocabulary::{BodyPart, part_count_in};
use crate::core::world::Pos;

use super::bodies::{self, Pattern};
use super::derived::{Body, Colony, Derived, Ground, SourceFact, key_parts_in};
use super::price;

#[derive(Clone, Debug)]
pub struct Seat {
    pub pattern: Pattern,
    pub at: Pos,
    pub parts: i32,
    pub colony: Option<usize>,
    pub body: Vec<BodyPart>,
    pub ret: Option<f64>,
}

#[derive(Clone, Debug)]
pub struct Want {
    pub pattern: Pattern,
    pub wanted: i32,
    pub standing: i32,
}

#[derive(Clone, Debug)]
pub struct Cast {
    pub colony: String,
    pub spawn: String,
    pub body: Vec<BodyPart>,
    pub name: String,
    pub floor: bool,
}

#[derive(Clone, Debug)]
pub struct Plan {
    pub wants: Vec<Want>,
    pub seats: Vec<Seat>,
    pub casts: Vec<Cast>,
}

/// What a plan is priced with: the ticks between two tiles, the colony nearest
/// one, one loaded walk and a successor's walk. `Run` reads them off the Atlas;
/// a test hands in its own.
pub trait Walks {
    fn distance(&mut self, a: &Pos, b: &Pos) -> Option<i32>;
    /// Ticks a body of the parts walks loaded from a tile to within a range of
    /// another.
    fn leg(&mut self, body: &[BodyPart], from: &Pos, onto: &Pos, range: i32) -> Option<i32>;
    fn near(&mut self, can: &dyn Fn(usize) -> bool, at: &Pos) -> Option<usize>;
    fn walk(&mut self, fresh: bool, body: &Body, from: &Pos) -> Option<i32>;
}

pub fn lead(body: &Body, walk: i32) -> i32 {
    body.size * engine::SPAWN_TICKS_PER_PART + walk.max(0)
}

/// Out of the count inside its successor's lead, so the successor is cast
/// while it still works and arrives as it dies.
pub fn expiring(body: &Body, walk: i32) -> bool {
    body.ticks_to_live <= lead(body, walk)
}

fn ceil_div(n: i32, d: i32) -> i32 {
    (n + d - 1) / d
}

/// The engine drops what a part adds at the cap; one claim life short of it
/// outlasts the successor's walk.
pub fn claim_parts_for(held: i32) -> i32 {
    let short = 0.max(engine::RESERVATION_CAP - held - engine::CLAIM_LIFETIME);

    1 + ceil_div(
        ceil_div(short, engine::CLAIM_LIFETIME),
        engine::RESERVE_PER_CLAIM,
    )
}

fn life_of(pattern: Pattern) -> i32 {
    if pattern.is_claimer() {
        engine::CLAIM_LIFETIME
    } else {
        engine::CREEP_LIFETIME
    }
}

fn parts_of(pattern: Pattern, body: &[BodyPart]) -> i32 {
    part_count_in(body, bodies::key_part_of(pattern))
}

/// What a body of parts is worth to a demand over a creep's life; `None` where
/// nothing is priced.
type Value = Box<dyn Fn(&[BodyPart]) -> Option<f64>>;

/// A dig a hauler moves: its source's rate, the parts it wanted, and what of
/// them no harvester has taken.
type Dig = (i32, i32, Rc<Cell<i32>>);

struct Demand {
    pattern: Pattern,
    at: Pos,
    wanted: i32,
    seats: i32,
    spill: f64,
    value: Value,
    /// `Rc` because a hauler's worth is read off the dig's remainder after the
    /// bodies have taken their shares.
    left: Rc<Cell<i32>>,
    seats_left: Rc<Cell<i32>>,
}

fn seated(pattern: Pattern, at: &Pos, wanted: i32, seats: i32, value: Value) -> Demand {
    Demand {
        pattern,
        at: at.clone(),
        wanted: wanted.max(0),
        seats: seats.max(0),
        spill: 0.0,
        value,
        left: Rc::new(Cell::new(wanted.max(0))),
        seats_left: Rc::new(Cell::new(seats.max(0))),
    }
}

fn demand(pattern: Pattern, at: &Pos, wanted: i32, value: Value) -> Demand {
    seated(pattern, at, wanted, i32::MAX, value)
}

fn body_for(colony: &Colony, d: &Demand) -> Option<Vec<BodyPart>> {
    if d.pattern.is_harvester() && d.at.room == colony.room {
        bodies::home_harvester(d.wanted, colony.bank_capacity)
    } else {
        bodies::sized(d.pattern, d.wanted, colony.bank_capacity)
    }
}

fn home_of(colony: &Colony) -> Pos {
    colony.spawns[0].pos.clone()
}

/// The walk there loaded and back empty: an empty Carry weighs nothing, so
/// each leg is its own flood toward `work`.
fn round_trip(
    walks: &mut dyn Walks,
    body: &[BodyPart],
    fetch: &Pos,
    work: &Pos,
    range: i32,
) -> Option<i32> {
    let empty: Vec<BodyPart> = body.iter().copied().filter(|p| !p.is_carry()).collect();

    match (
        walks.leg(body, fetch, work, range),
        walks.leg(&empty, fetch, work, range),
    ) {
        (Some(there), Some(back)) => Some(2.max(there + back)),
        _ => None,
    }
}

/// `lifetimes` is the owner's number (`ARCHITECTURE.md` §5).
pub fn reserve_target(lifetimes: f64, level: i32) -> i32 {
    (f64::from(
        engine::controller_structures("tower", level)
            * engine::TOWER_ENERGY_COST
            * engine::CREEP_LIFETIME,
    ) * lifetimes) as i32
}

/// Without a storage a remote's haul has no sink but the spawn ring.
fn banks_remote(derived: &Derived, ci: usize) -> bool {
    let ground = &derived.grounds[ci];
    !ground.abandoned && ground.stores.iter().any(|s| s.owned)
}

pub type Near<'a> = dyn FnMut(&dyn Fn(usize) -> bool, &Pos) -> Option<usize> + 'a;

pub fn owners(near: &mut Near, derived: &Derived) -> BTreeMap<String, usize> {
    let mut owners = BTreeMap::new();
    for (gi, ground) in derived.grounds.iter().enumerate() {
        let at = || {
            let controller = ground.controller.as_ref().map(|c| &c.pos);
            controller.or_else(|| ground.sources.first().map(|s| &s.pos))
        };
        let owner = if gi < derived.colonies.len() {
            Some(gi)
        } else {
            at().and_then(|at| near(&|ci| banks_remote(derived, ci), at))
        };
        if let Some(ci) = owner {
            owners.insert(ground.room.clone(), ci);
        }
    }
    owners
}

/// The backlog is spread over a creep's life, so a hauler cast now clears it
/// before it dies. A dig that falls on the floor is the workers' to fetch.
fn haul_flows(ground: &Ground, s: &SourceFact, post: &Pos, carried: f64) -> Vec<(Pos, f64)> {
    match &s.container {
        None => Vec::new(),
        Some(id) => {
            let stored: i32 = ground
                .stores
                .iter()
                .map(|st| if &st.id == id { st.energy } else { 0 })
                .sum();

            let piled: i32 = ground
                .piles
                .iter()
                .map(|p| {
                    if p.resource.is_energy() && p.pos.within(1, &s.pos) {
                        p.amount
                    } else {
                        0
                    }
                })
                .sum();

            let left = f64::from(s.dug) - carried
                + f64::from(stored + piled) / f64::from(engine::CREEP_LIFETIME);

            match &ground.receiver {
                Some(r) if r.role.is_hub() && carried > 0.0 => {
                    vec![(post.clone(), left), (r.pos.clone(), carried)]
                }
                _ => vec![(post.clone(), left)],
            }
        }
    }
}

/// A hub receiver drains without limit: the carriers empty it into the storage.
fn link_drain(derived: &Derived, ground: &Ground) -> f64 {
    match (&ground.receiver, &ground.controller) {
        (Some(r), Some(c)) if r.role.is_controller() => {
            f64::from(super::pool::upgrade_drain(derived, &ground.room, c.level))
        }
        (Some(_), _) => f64::INFINITY,
        (None, _) => 0.0,
    }
}

/// A haul trip is to the dearest sink, not the nearest: a ring fills in a
/// trip, and the flow that goes on all day is to the far sink.
fn ground_demands(
    owners: &BTreeMap<String, usize>,
    walks: &mut dyn Walks,
    derived: &Derived,
) -> Vec<Demand> {
    let mut demands: Vec<Demand> = Vec::new();
    let life = f64::from(engine::CREEP_LIFETIME);
    let colonies = &derived.colonies;
    let n = colonies.len();
    let mut carries = vec![0.0f64; n];
    let mut remote = vec![0.0f64; n];
    let mut flows = vec![0.0f64; n];
    let mut digs: Vec<Vec<Dig>> = vec![Vec::new(); n];

    let sinks_of = |ci: usize| -> Vec<Pos> {
        let ground = &derived.grounds[ci];
        let mut sinks = vec![home_of(&colonies[ci])];
        sinks.extend(ground.feed.iter().map(|f| f.pos.clone()));
        sinks.extend(
            ground
                .stores
                .iter()
                .filter(|st| st.owned)
                .map(|st| st.pos.clone()),
        );
        sinks
    };

    for (gi, ground) in derived.grounds.iter().enumerate() {
        let owner = owners.get(&ground.room).copied();
        let mut drain = link_drain(derived, ground);

        for s in &ground.sources {
            let Some(post) = &s.post else { continue };

            // A post with no container yet piles its dig, and a pile decays.
            let spill = if s.container.is_some() {
                0.0
            } else {
                price::pile_decay(1)
            };

            let rate = s.rate;
            let mut dig =
                seated(
                    Pattern::Harvester,
                    &s.pos,
                    super::derived::dig_work(rate),
                    1,
                    Box::new(move |body| {
                        Some(
                            (f64::from(rate.min(
                                part_count_in(body, BodyPart::Work) * engine::HARVEST_PER_WORK,
                            )) - spill)
                                * life,
                        )
                    }),
                );
            dig.spill = spill;

            let dig_left = Rc::clone(&dig.left);
            let dig_wanted = dig.wanted;
            demands.push(dig);

            match owner {
                Some(ci) if s.container.is_some() => {
                    let carried = if s.link.is_some() {
                        f64::from(s.dug).min(drain)
                    } else {
                        0.0
                    };

                    drain -= carried;
                    let mut hauled = false;

                    for (fetch, flow) in haul_flows(ground, s, post, carried) {
                        let trip = bodies::sized(
                            Pattern::Hauler,
                            engine::MAX_BODY_PARTS as i32,
                            colonies[ci].bank_capacity,
                        )
                        .map_or(0, |body| {
                            sinks_of(ci)
                                .iter()
                                .filter_map(|sink| round_trip(walks, &body, &fetch, sink, 1))
                                .fold(0, i32::max)
                        });

                        if trip > 0 {
                            hauled = true;
                            carries[ci] += flow * f64::from(trip);

                            if gi >= colonies.len() {
                                remote[ci] += flow * f64::from(trip);
                            }

                            flows[ci] += flow;
                        }
                    }

                    if hauled {
                        digs[ci].push((s.rate, dig_wanted, dig_left));
                    }
                }
                _ => (),
            }
        }

        // A mineral is worth an energy a unit: with no market, its sink is
        // the boosts, which nothing yet prices.
        for m in &ground.minerals {
            let Some(post) = m
                .post
                .as_ref()
                .filter(|_| m.container.is_some() && m.amount > 0)
            else {
                continue;
            };
            let amount = f64::from(m.amount);
            let per_tick = |work: i32| {
                f64::from(work * engine::MINERAL_HARVEST_PER_WORK)
                    / f64::from(engine::MINERAL_HARVEST_CYCLE)
            };
            demands.push(seated(
                Pattern::Harvester,
                &m.pos,
                bodies::MINER_WORK,
                1,
                Box::new(move |body| {
                    Some((per_tick(part_count_in(body, BodyPart::Work)) * life).min(amount))
                }),
            ));

            if let (Some(ci), Some(_)) = (owner, &m.container) {
                let trip = bodies::sized(
                    Pattern::Hauler,
                    engine::MAX_BODY_PARTS as i32,
                    colonies[ci].bank_capacity,
                )
                .map_or(0, |body| {
                    sinks_of(ci)
                        .iter()
                        .filter_map(|sink| round_trip(walks, &body, post, sink, 1))
                        .fold(0, i32::max)
                });
                let flow = per_tick(bodies::MINER_WORK);
                carries[ci] += flow * f64::from(trip);
                flows[ci] += flow;
            }
        }

        match &ground.controller {
            Some(c) if ground.reservable && gi >= colonies.len() => {
                let gain = f64::from(super::derived::reserve_gain(ground) * engine::CLAIM_LIFETIME);

                demands.push(seated(
                    Pattern::Claimer,
                    &c.pos,
                    claim_parts_for(ground.our_reservation),
                    1,
                    Box::new(move |_| Some(gain)),
                ));
            }
            _ => (),
        }
    }

    for ci in 0..colonies.len() {
        if carries[ci] > 0.0 {
            let per_body = bodies::sized(
                Pattern::Hauler,
                engine::MAX_BODY_PARTS as i32,
                colonies[ci].bank_capacity,
            )
            .map_or(0, |body| part_count_in(&body, BodyPart::Carry));

            // One body leaves a remote container filling for its whole round
            // trip a room out.
            let wanted = {
                let flowing = (carries[ci] / f64::from(engine::CARRY_PART_CAPACITY)).ceil() as i32;

                if remote[ci] * 2.0 >= f64::from(per_body * engine::CARRY_PART_CAPACITY) {
                    flowing.max(2 * per_body)
                } else {
                    flowing
                }
            };

            let flow = flows[ci];
            let digs = digs[ci].clone();

            demands.push(demand(
                Pattern::Hauler,
                &home_of(&colonies[ci]),
                wanted,
                Box::new(move |body| {
                    let moved =
                        f64::from(part_count_in(body, BodyPart::Carry)) * flow / f64::from(wanted);

                    // A haul moves only what a dig leaves, so the harvesters
                    // are bought first.
                    let dug: i32 = digs
                        .iter()
                        .map(|(rate, dig_wanted, left)| {
                            (*rate).min((dig_wanted - left.get()) * engine::HARVEST_PER_WORK)
                        })
                        .sum();

                    Some(f64::from(dug).min(moved) * life)
                }),
            ));
        }
    }

    for c in &derived.claims {
        demands.push(demand(Pattern::Claimer, &c.pos, 1, Box::new(|_| None)));
    }

    for (at, _, _) in derived.scouts.iter().filter(|(_, _, cast)| *cast) {
        demands.push(demand(Pattern::Scout, at, 1, Box::new(|_| None)));
    }

    demands
}

/// A colony given up casts only its floors.
fn castable(derived: &Derived, d: &Demand, ci: usize) -> bool {
    let colonies = &derived.colonies;
    !derived.grounds[ci].abandoned
        && engine::body_cost(bodies::block_of(d.pattern)) <= colonies[ci].bank_capacity
        && (!d.pattern.is_upgrader() || colonies[ci].room == d.at.room)
}

fn spend_demands(
    owners: &BTreeMap<String, usize>,
    walks: &mut dyn Walks,
    derived: &Derived,
    others: &[Demand],
) -> Vec<Demand> {
    let colonies = &derived.colonies;
    let n = colonies.len();
    let life = f64::from(engine::CREEP_LIFETIME);
    let mut income = vec![0.0f64; n];
    let mut owed = vec![0i32; n];
    let mut upkeep = vec![0.0f64; n];

    for ground in &derived.grounds {
        if let Some(ci) = owners.get(&ground.room) {
            income[*ci] += f64::from(ground.posted);
            owed[*ci] += ground.sites.iter().map(|s| s.left).sum::<i32>();
        }
    }

    for d in others {
        let caster = owners
            .get(&d.at.room)
            .copied()
            .filter(|ci| castable(derived, d, *ci))
            .or_else(|| walks.near(&|ci| castable(derived, d, ci), &d.at));

        if let Some(ci) = caster
            && let Some(body) = body_for(&colonies[ci], d)
        {
            let parts = parts_of(d.pattern, &body);

            if parts > 0 {
                let bodies = d.seats.min(ceil_div(d.wanted, parts));

                upkeep[ci] += f64::from(bodies)
                    * (f64::from(engine::body_cost(&body)) / f64::from(life_of(d.pattern))
                        + d.spill);
            }
        }
    }

    let mut out: Vec<Demand> = Vec::new();

    for (ci, colony) in colonies.iter().enumerate() {
        let ground = &derived.grounds[ci];
        let at = &colony.controller.pos;

        // Only the downgrade deadline is upgraded, so the terminal stays
        // active while it ships.
        if ground.abandoned {
            if super::pool::inside_deadline(&colony.controller) {
                out.push(demand(Pattern::Worker, at, 1, Box::new(|_| None)));
            }
            continue;
        }
        let storage = ground.stores.iter().find(|s| s.owned);
        let (reserve, stock) = match storage {
            Some(_) => (
                reserve_target(derived.reserve_lifetimes, colony.controller.level),
                ground
                    .stores
                    .iter()
                    .map(|s| if s.kind.is_storage() { s.energy } else { 0 })
                    .sum::<i32>(),
            ),
            None => (0, 0),
        };
        let spendable = if stock < reserve {
            0.0
        } else {
            income[ci] - upkeep[ci]
        };

        let upgradable = bodies::upgradable_at(colony.controller.level);

        let upgrader =
            bodies::sized(Pattern::Upgrader, upgradable, colony.bank_capacity).filter(|body| {
                ground.buffer.is_some()
                    && bodies::standing(
                        part_count_in(body, BodyPart::Work),
                        part_count_in(body, BodyPart::Carry),
                    )
            });

        // No more upgraders than the buffer seats or the Work the controller
        // takes: one past them draws nothing.
        let (upgraders, upgrader_work, upgraders_drain) = match &upgrader {
            Some(body) => {
                let work = part_count_in(body, BodyPart::Work);
                let seats = f64::from((ground.buffer_seats.len() as i32).min(upgradable / work));

                let life_cost = bodies::upgrader_hire(upgradable, 1, body) * life;

                // The sites are paid from the stock: a level's plan keeps its
                // window of sites standing, so a charge on income idles the seat.
                let on_income = (spendable * life / life_cost).max(0.0).floor().min(seats);

                let on_stock = (f64::from(stock - reserve - owed[ci]) / life_cost)
                    .max(0.0)
                    .floor()
                    .min(seats - on_income);

                (
                    (on_income + on_stock) as i32,
                    work,
                    on_income * life_cost / life,
                )
            }
            None => (0, 0, 0.0),
        };

        let rest = spendable - upgraders_drain;

        // A worker drains the controller while its load lasts, then walks to
        // its energy and back, to each dig as often as it yields.
        let range = super::task::RANGE_UPGRADE;

        let fetches: Vec<(Pos, i32)> = match (&ground.buffer, storage) {
            (Some(buffer), _) => vec![(buffer.pos.clone(), 1)],
            (None, Some(storage)) => vec![(storage.pos.clone(), 1)],
            (None, None) => ground
                .sources
                .iter()
                .map(|s| (s.post.clone().unwrap_or_else(|| s.pos.clone()), s.dug))
                .collect(),
        };

        let cast = bodies::sized(
            Pattern::Worker,
            engine::MAX_BODY_PARTS as i32,
            colony.bank_capacity,
        );

        let spend = match &cast {
            Some(body) if parts_of(Pattern::Worker, body) > 0 => {
                let work = parts_of(Pattern::Worker, body);

                let mut weighted = 0;
                let mut weights = 0;

                for (fetch, weight) in &fetches {
                    if let Some(trip) = round_trip(walks, body, fetch, at, range) {
                        weighted += *weight * trip;
                        weights += *weight;
                    }
                }

                let upgrading =
                    f64::from(part_count_in(body, BodyPart::Carry) * engine::CARRY_PART_CAPACITY)
                        / f64::from(work * engine::UPGRADE_DRAIN_PER_WORK);

                if weights == 0 {
                    0
                } else {
                    let drain = f64::from(engine::UPGRADE_DRAIN_PER_WORK) * upgrading
                        / (upgrading + f64::from(weighted) / f64::from(weights));

                    let upkeep = f64::from(engine::body_cost(body))
                        / f64::from(work * life_of(Pattern::Worker));

                    (rest.max(0.0) / (drain + upkeep)).ceil() as i32
                }
            }
            _ => 0,
        };

        let body_work = cast.as_ref().map_or(1, |b| parts_of(Pattern::Worker, b));

        let floor_bodies = worker_floor(ground);

        // A crew digs a source whose container does not stand, and builds it.
        // Not knowing a source's seats is no crew.
        let crews: i32 = ground
            .sources
            .iter()
            .map(|s| match s.seats {
                Some(seats) if s.container.is_none() => seats - i32::from(s.post.is_some()),
                _ => 0,
            })
            .sum();

        let seats = match ground.upgrade_seats {
            Some(seats) => floor_bodies.max(seats) + crews,
            None => i32::MAX,
        };

        out.push(seated(
            Pattern::Worker,
            at,
            spend.max(floor_bodies * body_work) + crews * body_work,
            seats,
            Box::new(move |body| Some(f64::from(parts_of(Pattern::Worker, body)) * life)),
        ));
        out.push(seated(
            Pattern::Upgrader,
            at,
            upgraders * upgrader_work,
            upgraders,
            Box::new(move |body| Some(f64::from(parts_of(Pattern::Upgrader, body)) * life)),
        ));
    }

    out
}

fn worker_floor(ground: &Ground) -> i32 {
    if !ground.sites.is_empty() || !ground.repairs.is_empty() {
        2
    } else {
        1
    }
}

/// The upgraders last: they spend what every other seat leaves. Another
/// colony's seat ranks with the caster's workers past its floor, so a young
/// colony's spawn grows it before it hauls or reserves a neighbour's remotes.
/// A tie keeps the demands' order (`sort_by` is stable).
fn cast_order(
    derived: &Derived,
    owners: &BTreeMap<String, usize>,
    d: &Demand,
    caster: Option<usize>,
    ret: Option<f64>,
    filled: i32,
) -> (i32, f64) {
    let own = caster.is_some() && owners.get(&d.at.room) == caster.as_ref();
    let floor = caster.is_some_and(|ci| {
        derived.colonies[ci].room == d.at.room && filled < worker_floor(&derived.grounds[ci])
    });
    match (ret, d.pattern) {
        (None, _) => (0, 0.0),
        (Some(_), Pattern::Upgrader) => (4, 0.0),
        (Some(r), p) if own && p != Pattern::Worker => (1, -r),
        (Some(r), _) if own && floor => (2, -r),
        (Some(r), _) => (3, -r),
    }
}

/// The ticks a body's successor walks from a spawn to its task, read the way
/// the F# read it: a fresh body walks at the creep's own body, empty.
fn expiring_now(
    walks: &mut dyn Walks,
    derived: &Derived,
    owners: &BTreeMap<String, usize>,
    body: &Body,
) -> bool {
    let spawn_time = lead(body, 0);

    if body.ticks_to_live <= spawn_time {
        true
    } else {
        match owners
            .get(&body.pos.room)
            .copied()
            .or_else(|| walks.near(&|_| true, &body.pos))
        {
            Some(ci) => {
                let home = home_of(&derived.colonies[ci]);

                let walked = |walks: &mut dyn Walks, fresh: bool| -> i32 {
                    walks
                        .walk(fresh, body, &home)
                        .or_else(|| walks.distance(&home, &body.pos))
                        .unwrap_or(0)
                };

                expiring(body, walked(walks, false)) && expiring(body, walked(walks, true))
            }
            None => false,
        }
    }
}

/// The ledger and the idle spawns a cast spends, so a cast names only what it
/// makes.
struct Foundry<'a> {
    casts: &'a mut Vec<Cast>,
    idle: &'a mut Vec<Vec<usize>>,
    bank: &'a mut [i32],
    colonies: &'a [Colony],
    time: i32,
}

impl Foundry<'_> {
    fn at(&mut self, ci: usize, pattern: Pattern, body: Vec<BodyPart>, floor: bool) {
        let index = self.idle[ci].remove(0);
        let spawn = &self.colonies[ci].spawns[index];
        self.bank[ci] -= engine::body_cost(&body);

        self.casts.push(Cast {
            colony: self.colonies[ci].room.clone(),
            spawn: spawn.id.clone(),
            name: bodies::cast_name(pattern, self.time, &spawn.name),
            body,
            floor,
        });
    }
}

/// A seat its bank cannot pay yields to the next it can: an order, never a
/// veto. Never to an upgrader.
pub fn plan(
    walks: &mut dyn Walks,
    derived: &Derived,
    owners: &BTreeMap<String, usize>,
    pool: &[super::pool::Offer],
    assignments: &BTreeMap<String, String>,
) -> Plan {
    let colonies = &derived.colonies;

    // A colony that feeds outposts cannot wait for a bank that holds its whole
    // hauler. A lone room can, and regresses when it casts to the bank instead.
    let mut served = vec![0i32; colonies.len()];

    for ci in owners.values() {
        served[*ci] += 1;
    }

    let others = ground_demands(owners, walks, derived);

    let spent = spend_demands(owners, walks, derived, &others);

    let mut all = others;
    all.extend(spent);

    let mut standing: BTreeMap<Pattern, i32> = BTreeMap::new();
    let targets: HashMap<&str, &Pos> = pool
        .iter()
        .map(|offer| (offer.id.as_str(), &offer.target))
        .collect();

    let work_at = |body: &Body| -> Pos {
        match assignments
            .get(&body.name)
            .and_then(|id| targets.get(id.as_str()))
        {
            Some(pos) => (*pos).clone(),
            None => body.pos.clone(),
        }
    };

    for body in derived.bodies.iter().chain(derived.ovens.iter()) {
        if !expiring_now(walks, derived, owners, body) {
            let pattern = body.pattern;
            let parts = key_parts_in(pattern, body);

            *standing.entry(pattern).or_insert(0) += parts;

            // The nearest open demand of the pattern: a body out of a seat's
            // reach never fills it, or a far colony's surplus sits in it.
            let mut pick: Option<usize> = None;
            let mut least = i32::MAX;
            let at = work_at(body);
            // The nearest seat by distance can be a young colony's that a
            // neighbour's remote crew never reaches; a dig or claim seat is
            // its object's already.
            let serves = owners.get(&at.room).filter(|ci| {
                colonies[**ci].room != at.room
                    && matches!(pattern, Pattern::Worker | Pattern::Hauler)
            });

            for (i, d) in all.iter().enumerate() {
                // One cast for another room's seat upgrades whichever
                // controller it reaches first.
                if d.pattern == pattern
                    && d.left.get() > 0
                    && d.seats_left.get() > 0
                    && (!d.pattern.is_upgrader() || d.at.room == at.room)
                    && serves.is_none_or(|ci| owners.get(&d.at.room) == Some(ci))
                {
                    let Some(ticks) = walks.distance(&at, &d.at) else {
                        continue;
                    };

                    if pick.is_none() || ticks < least {
                        pick = Some(i);
                        least = ticks;
                    }
                }
            }

            if let Some(i) = pick {
                let d = &all[i];
                d.left.set(0.max(d.left.get() - parts));
                d.seats_left.set(0.max(d.seats_left.get() - 1));
            }
        }
    }

    let wants: Vec<Want> = bodies::ALL_PATTERNS
        .iter()
        .map(|pattern| Want {
            pattern: *pattern,
            wanted: all
                .iter()
                .filter(|d| d.pattern == *pattern)
                .map(|d| d.wanted)
                .sum(),
            standing: standing.get(pattern).copied().unwrap_or(0),
        })
        .collect();

    let mut bank: Vec<i32> = colonies.iter().map(|c| c.bank).collect();

    let mut idle: Vec<Vec<usize>> = colonies
        .iter()
        .map(|c| {
            c.spawns
                .iter()
                .enumerate()
                .filter(|(_, s)| s.spawning.is_none())
                .map(|(i, _)| i)
                .collect()
        })
        .collect();

    let mut casts: Vec<Cast> = Vec::new();
    let mut foundry = Foundry {
        casts: &mut casts,
        idle: &mut idle,
        bank: &mut bank,
        colonies,
        time: derived.time,
    };

    // The anchor floor: with no harvester, nothing digs what refills the bank.
    for (ci, colony) in colonies.iter().enumerate() {
        let ground = &derived.grounds[ci];

        let digging = derived
            .bodies
            .iter()
            .chain(derived.ovens.iter())
            .any(|b| b.pattern.is_harvester() && b.pos.room == colony.room);

        let Some(s) = ground.sources.iter().find(|s| s.post.is_some()) else {
            continue;
        };

        if foundry.idle[ci].is_empty() || digging || colony.bank >= colony.bank_capacity {
            continue;
        }

        if let Some(body) =
            bodies::home_harvester(super::derived::dig_work(s.rate), foundry.bank[ci])
            && engine::body_cost(&body) <= foundry.bank[ci]
        {
            foundry.at(ci, Pattern::Harvester, body, true);
        }
    }

    // The supply floor: with nobody to refill it, the bank never fills again.
    // A worker builds and upgrades between loads; a stock is carried by a hauler.
    for ci in 0..colonies.len() {
        let ground = &derived.grounds[ci];
        if ground.abandoned && ground.shipment.is_empty() {
            continue;
        }
        let pattern = if ground.abandoned || ground.stores.iter().any(|s| s.owned && s.energy > 0) {
            Pattern::Hauler
        } else {
            Pattern::Worker
        };

        let serves = |pattern: Pattern, body: &Body| -> bool {
            (body.pattern == pattern || body.pattern.is_hauler())
                && [work_at(body).room, body.pos.room.clone()]
                    .iter()
                    .any(|room| owners.get(room) == Some(&ci))
        };

        if foundry.idle[ci].is_empty()
            || derived
                .bodies
                .iter()
                .chain(derived.ovens.iter())
                .any(|b| serves(pattern, b))
        {
            continue;
        }

        if let Some(body) = bodies::sized(pattern, engine::MAX_BODY_PARTS as i32, foundry.bank[ci])
        {
            foundry.at(ci, pattern, body, true);
        }
    }

    let mut richest: Option<&Colony> = None;
    for colony in colonies {
        let better = match richest {
            Some(r) => colony.bank_capacity > r.bank_capacity,
            None => true,
        };
        if better {
            richest = Some(colony);
        }
    }

    let tick_worth = |ci: usize| -> f64 {
        f64::from(derived.grounds[ci].income) / colonies[ci].spawns.len() as f64
    };

    let mut seats: Vec<((i32, f64), Seat)> = Vec::new();

    for d in &all {
        let caster = match owners.get(&d.at.room).copied() {
            Some(ci) if !foundry.idle[ci].is_empty() && castable(derived, d, ci) => Some(ci),
            _ => walks.near(
                &|ci| !foundry.idle[ci].is_empty() && castable(derived, d, ci),
                &d.at,
            ),
        };

        let body = caster
            .map(|ci| &colonies[ci])
            .or(richest)
            .and_then(|colony| body_for(colony, d));

        let mut left = d.left.get();
        let mut seats_left = d.seats_left.get();
        let mut go = true;

        while go && left > 0 && seats_left > 0 {
            match &body {
                Some(body) if parts_of(d.pattern, body) > 0 => {
                    let ret = (d.value)(body).map(|v| match caster {
                        Some(ci) => price::body_return(
                            v,
                            engine::body_cost(body),
                            engine::SPAWN_TICKS_PER_PART * body.len() as i32,
                            tick_worth(ci),
                        ),
                        None => 0.0,
                    });

                    let order = cast_order(derived, owners, d, caster, ret, d.seats - seats_left);
                    seats.push((
                        order,
                        Seat {
                            pattern: d.pattern,
                            at: d.at.clone(),
                            parts: left,
                            colony: caster,
                            body: body.clone(),
                            ret,
                        },
                    ));

                    left -= parts_of(d.pattern, body);
                    seats_left -= 1;
                }
                _ => go = false,
            }
        }
    }

    seats.sort_by(|((ka, va), _), ((kb, vb), _)| {
        ka.cmp(kb)
            .then(va.partial_cmp(vb).unwrap_or(Ordering::Equal))
    });

    let mut waiting = vec![false; colonies.len()];

    let mut seated: Vec<Seat> = Vec::with_capacity(seats.len());

    for (_, mut seat) in seats {
        if let Some(ci) = seat.colony
            && !foundry.idle[ci].is_empty()
            && !(seat.pattern.is_upgrader() && waiting[ci])
        {
            let body = if seat.pattern.is_hauler() && served[ci] > 1 {
                bodies::sized(
                    seat.pattern,
                    parts_of(seat.pattern, &seat.body),
                    foundry.bank[ci],
                )
            } else {
                Some(seat.body.clone())
            };

            match body {
                Some(fit) if engine::body_cost(&fit) <= foundry.bank[ci] => {
                    foundry.at(ci, seat.pattern, fit.clone(), false);
                    seat.body = fit;
                }
                _ => waiting[ci] = true,
            }
        }

        seated.push(seat);
    }

    Plan {
        wants,
        seats: seated,
        casts,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use super::*;
    use crate::core::atlas::Atlas;
    use crate::core::economy::derived::StoreKind;
    use crate::core::economy::dice::{self, Dice};
    use crate::core::economy::matcher::Assignments;
    use crate::core::economy::pool::{self, Capacity};
    use crate::core::economy::task::Task;
    use crate::core::tick;
    use crate::core::vocabulary::Resource;
    use crate::core::world::{Controller, Creep};

    #[test]
    fn a_reservers_claim_leaves_the_reservation_within_a_claim_life_of_the_cap() {
        for held in 0..=engine::RESERVATION_CAP {
            let n = claim_parts_for(held);

            let gain =
                |parts: i32| (parts * engine::RESERVE_PER_CLAIM - 1) * engine::CLAIM_LIFETIME;

            let short = engine::RESERVATION_CAP - held - engine::CLAIM_LIFETIME;

            assert!(
                n >= 1 && gain(n) >= short && (n == 1 || gain(n - 1) < short),
                "{}",
                held
            );
        }
    }

    fn derive(g: &dice::Generated) -> Derived {
        let atlas = Atlas::of_world(&g.world);
        let all: Vec<&Creep> = g.world.creeps.iter().collect();
        crate::core::economy::derived::of_world(
            &g.world,
            &all,
            &atlas,
            &tick::Declarations {
                claims: g.claims.clone(),
                ..tick::undeclared()
            },
            &g.worked,
            &std::collections::BTreeMap::new(),
            &[],
        )
    }

    /// Chebyshev in a room, a room's side between rooms.
    struct Flat {
        homes: Vec<Pos>,
    }

    impl Flat {
        fn new(derived: &Derived) -> Flat {
            Flat {
                homes: derived
                    .colonies
                    .iter()
                    .map(|c| c.spawns[0].pos.clone())
                    .collect(),
            }
        }

        fn chebyshev(a: &Pos, b: &Pos) -> i32 {
            if a.room == b.room {
                (a.x - b.x).abs().max((a.y - b.y).abs())
            } else {
                engine::ROOM_SIDE
            }
        }
    }

    impl Walks for Flat {
        fn distance(&mut self, a: &Pos, b: &Pos) -> Option<i32> {
            Some(Flat::chebyshev(a, b))
        }

        fn leg(&mut self, _body: &[BodyPart], from: &Pos, onto: &Pos, _range: i32) -> Option<i32> {
            Some(Flat::chebyshev(from, onto))
        }

        fn near(&mut self, can: &dyn Fn(usize) -> bool, at: &Pos) -> Option<usize> {
            let mut best: Option<(i32, usize)> = None;

            for (i, home) in self.homes.iter().enumerate() {
                if can(i) {
                    let d = Flat::chebyshev(home, at);
                    if best.is_none_or(|(least, _)| d < least) {
                        best = Some((d, i));
                    }
                }
            }

            best.map(|(_, i)| i)
        }

        fn walk(&mut self, _fresh: bool, _body: &Body, _from: &Pos) -> Option<i32> {
            Some(engine::ROOM_SIDE)
        }
    }

    /// Every third body assigned to an offer, picked by its index.
    fn assigned_in(derived: &Derived, pool: &[pool::Offer]) -> BTreeMap<String, String> {
        if pool.is_empty() {
            return BTreeMap::new();
        }

        derived
            .bodies
            .iter()
            .enumerate()
            .filter(|(i, _)| i % 3 == 0)
            .map(|(i, b)| (b.name.clone(), pool[(i * 7) % pool.len()].id.clone()))
            .collect()
    }

    fn planned(derived: &Derived) -> (Vec<pool::Offer>, Plan) {
        let mut flat = Flat::new(derived);
        let owners = owners(&mut |c, at| flat.near(c, at), derived);
        let offers = pool::pool(derived, &owners, &Assignments::new());
        let assignments = assigned_in(derived, &offers);

        (
            offers.clone(),
            plan(&mut flat, derived, &owners, &offers, &assignments),
        )
    }

    fn worlds(seed: u64) -> Vec<dice::Generated> {
        let mut dice = Dice::new(seed);
        (0..300).map(|_| dice::world(&mut dice)).collect()
    }

    fn controllers(g: &dice::Generated) -> Vec<&Controller> {
        g.world
            .rooms
            .iter()
            .filter_map(|r| r.controller.as_ref())
            .collect()
    }

    #[test]
    fn the_pool_offers_each_task_once_with_no_negative_capacity() {
        for g in worlds(102) {
            let offers = pool::pool(&derive(&g), &BTreeMap::new(), &Assignments::new());
            let ids: Vec<&String> = offers.iter().map(|o| &o.id).collect();
            let distinct: BTreeSet<&String> = ids.iter().copied().collect();

            assert_eq!(distinct.len(), ids.len());

            for o in &offers {
                match o.capacity {
                    Capacity::Unbounded => (),
                    Capacity::Bodies(n) | Capacity::Work(n) | Capacity::Load(n) => {
                        assert!(n >= 0, "{}", o.id)
                    }
                    Capacity::Seated { seats, posts, work } => {
                        assert!(
                            (0..=1).contains(&posts) && posts <= seats && work >= 0,
                            "{}",
                            o.id
                        )
                    }
                }
            }
        }
    }

    #[test]
    fn no_reservation_or_claim_is_offered_on_another_players_reservation() {
        for g in worlds(103) {
            for o in pool::pool(&derive(&g), &BTreeMap::new(), &Assignments::new()) {
                let (Task::Reserve(id) | Task::Claim(id)) = &o.task else {
                    continue;
                };

                assert!(controllers(&g).iter().any(|c| {
                    &c.id == id && c.reservation.as_ref().is_none_or(|(by, _)| by.is_ours())
                }));
            }
        }
    }

    #[test]
    fn a_storage_draw_is_worth_no_more_than_the_dearest_refill_it_serves() {
        for g in worlds(104) {
            let derived = derive(&g);
            let offers = pool::pool(&derived, &BTreeMap::new(), &Assignments::new());

            let rank_of = |task: &Task| -> Option<i32> {
                offers
                    .iter()
                    .find(|o| o.id == crate::core::economy::task::task_id(task))
                    .map(|o| o.priority)
            };

            for ground in &derived.grounds {
                let mut dearest = price::BOTTOM_RANK;

                for r in &ground.refills {
                    let rank = rank_of(&Task::Refill(r.id.clone(), Resource::Energy));
                    if let Some(rank) = rank {
                        dearest = dearest.min(rank);
                    }
                }

                for s in &ground.stores {
                    if s.kind != StoreKind::Storage {
                        continue;
                    }

                    if let Some(rank) = rank_of(&Task::Withdraw(s.id.clone(), Resource::Energy)) {
                        assert!(rank >= dearest, "{} {}", s.id, rank);
                    }
                }
            }
        }
    }

    #[test]
    fn the_quota_never_wants_or_counts_a_negative_number() {
        for g in worlds(105) {
            let (_, p) = planned(&derive(&g));

            for w in &p.wants {
                assert!(w.wanted >= 0 && w.standing >= 0);
            }
        }
    }

    #[test]
    fn no_seat_asks_for_nothing() {
        for g in worlds(106) {
            let (_, p) = planned(&derive(&g));

            for s in &p.seats {
                assert!(s.parts > 0);
            }
        }
    }

    #[test]
    fn a_source_is_cast_no_more_harvesters_than_its_seats() {
        for g in worlds(107) {
            let derived = derive(&g);
            let (_, p) = planned(&derived);

            for ground in &derived.grounds {
                for s in &ground.sources {
                    let cast = p
                        .seats
                        .iter()
                        .filter(|seat| seat.pattern == Pattern::Harvester && seat.at == s.pos)
                        .count() as i32;

                    assert!(s.seats.is_none_or(|seats| cast <= seats), "{}", s.id);
                }
            }
        }
    }

    #[test]
    fn a_body_counts_only_while_its_life_outlasts_its_successors_spawn_and_walk() {
        for g in worlds(108) {
            let derived = derive(&g);
            let (_, p) = planned(&derived);

            let walked = if derived.colonies.is_empty() {
                0
            } else {
                engine::ROOM_SIDE
            };

            for pattern in bodies::ALL_PATTERNS {
                let counted: i32 = derived
                    .bodies
                    .iter()
                    .chain(derived.ovens.iter())
                    .filter(|b| {
                        b.pattern == pattern
                            && b.ticks_to_live > b.size * engine::SPAWN_TICKS_PER_PART + walked
                    })
                    .map(|b| key_parts_in(pattern, b))
                    .sum();

                for w in &p.wants {
                    assert!(w.pattern != pattern || w.standing == counted, "{pattern:?}");
                }
            }
        }
    }

    #[test]
    fn no_spawn_idles_while_its_colony_can_pay_a_seat_it_has_not_cast() {
        for g in worlds(109) {
            let derived = derive(&g);
            let (_, p) = planned(&derived);

            for (ci, colony) in derived.colonies.iter().enumerate() {
                let own: Vec<&Cast> = p.casts.iter().filter(|c| c.colony == colony.room).collect();
                let seats: Vec<&Seat> = p.seats.iter().filter(|s| s.colony == Some(ci)).collect();

                let idle = colony
                    .spawns
                    .iter()
                    .filter(|s| s.spawning.is_none())
                    .count() as i32
                    - own.len() as i32;

                let left =
                    colony.bank - own.iter().map(|c| engine::body_cost(&c.body)).sum::<i32>();

                let paid = own.iter().filter(|c| !c.floor).count() == seats.len();

                assert!(
                    idle == 0 || paid || seats.iter().any(|s| engine::body_cost(&s.body) > left),
                    "{}",
                    colony.room
                );
            }
        }
    }

    #[test]
    fn a_body_in_an_oven_counts_and_opens_no_seat_of_its_pattern() {
        for g in worlds(110) {
            let derived = derive(&g);
            let (_, baked) = planned(&derived);
            let (_, bare) = planned(&Derived {
                ovens: Vec::new(),
                ..derived.clone()
            });

            let count = |p: &Plan, pattern: Pattern| -> usize {
                p.seats.iter().filter(|s| s.pattern == pattern).count()
            };

            let standing = |p: &Plan, pattern: Pattern| -> i32 {
                p.wants
                    .iter()
                    .filter(|w| w.pattern == pattern)
                    .map(|w| w.standing)
                    .sum()
            };

            let floors = |p: &Plan| -> Vec<String> {
                p.casts
                    .iter()
                    .filter(|c| c.floor)
                    .map(|c| c.spawn.clone())
                    .collect()
            };

            for pattern in bodies::ALL_PATTERNS {
                let oven: i32 = derived
                    .ovens
                    .iter()
                    .filter(|b| b.pattern == pattern)
                    .map(|b| key_parts_in(pattern, b))
                    .sum();

                assert!(
                    floors(&baked) != floors(&bare)
                        || count(&baked, pattern) <= count(&bare, pattern)
                );
                assert_eq!(
                    standing(&baked, pattern),
                    standing(&bare, pattern) + oven,
                    "{pattern:?}"
                );
            }
        }
    }

    #[test]
    fn a_seat_its_bank_cannot_pay_yields_to_the_next_never_to_an_upgrader() {
        for g in worlds(111) {
            let derived = derive(&g);
            let (_, p) = planned(&derived);

            for (ci, colony) in derived.colonies.iter().enumerate() {
                let own: Vec<&Cast> = p.casts.iter().filter(|c| c.colony == colony.room).collect();
                let floors: i32 = own
                    .iter()
                    .filter(|c| c.floor)
                    .map(|c| engine::body_cost(&c.body))
                    .sum();
                let mut left: Vec<&Cast> = own.iter().copied().filter(|c| !c.floor).collect();

                let mut bank = colony.bank - floors;
                let mut ok = true;
                let mut waiting = false;

                for seat in p.seats.iter().filter(|s| s.colony == Some(ci)) {
                    let upgrader = seat.pattern == Pattern::Upgrader;

                    match left.first() {
                        Some(next) if next.body == seat.body => {
                            ok = ok && !(upgrader && waiting);
                            bank -= engine::body_cost(&seat.body);
                            left.remove(0);
                        }
                        Some(_) => {
                            let short = engine::body_cost(&seat.body) > bank;
                            ok = ok && (short || upgrader && waiting);
                            waiting = waiting || short;
                        }
                        None => (),
                    }
                }

                assert!(ok && left.is_empty(), "{}", colony.room);
            }
        }
    }

    #[test]
    fn a_seats_cast_is_the_body_its_colony_makes() {
        for g in worlds(112) {
            let derived = derive(&g);
            let (_, p) = planned(&derived);

            for c in &p.casts {
                assert!(
                    c.floor
                        || p.seats.iter().any(|s| s.body == c.body
                            && s.colony
                                .is_some_and(|ci| derived.colonies[ci].room == c.colony)),
                    "{}",
                    c.name
                );
            }
        }
    }

    #[test]
    fn the_supply_floor_casts_only_where_nothing_of_its_kind_or_a_hauler_works_in_the_home() {
        for g in worlds(113) {
            let derived = derive(&g);
            let mut flat = Flat::new(&derived);
            let owners = owners(&mut |c, at| flat.near(c, at), &derived);
            let offers = pool::pool(&derived, &owners, &Assignments::new());
            let assignments = assigned_in(&derived, &offers);
            let p = plan(&mut flat, &derived, &owners, &offers, &assignments);

            let serves = |room: &str, kind: Option<Pattern>, b: &Body| -> bool {
                let at = assignments
                    .get(&b.name)
                    .and_then(|id| offers.iter().find(|o| &o.id == id))
                    .map_or_else(|| b.pos.room.clone(), |o| o.target.room.clone());

                (Some(b.pattern) == kind || b.pattern == Pattern::Hauler) && at == room
            };

            for c in &p.casts {
                let kind = bodies::pattern_of_name(&c.name);

                assert!(
                    !c.floor
                        || kind == Some(Pattern::Harvester)
                        || !derived.bodies.iter().any(|b| serves(&c.colony, kind, b)),
                    "{}",
                    c.name
                );
            }
        }
    }

    #[test]
    fn a_colonys_casts_are_paid_out_of_its_bank_one_an_idle_spawn() {
        for g in worlds(114) {
            let derived = derive(&g);
            let (_, p) = planned(&derived);

            for colony in &derived.colonies {
                let own: Vec<&Cast> = p.casts.iter().filter(|c| c.colony == colony.room).collect();
                let spawns: Vec<&String> = own.iter().map(|c| &c.spawn).collect();

                assert!(own.iter().map(|c| engine::body_cost(&c.body)).sum::<i32>() <= colony.bank);

                let distinct: BTreeSet<&String> = spawns.iter().copied().collect();
                assert_eq!(distinct.len(), spawns.len());

                for id in &spawns {
                    assert!(
                        colony
                            .spawns
                            .iter()
                            .any(|s| &s.id == *id && s.spawning.is_none())
                    );
                }

                for c in &own {
                    assert!(c.body.len() <= engine::MAX_BODY_PARTS);
                }
            }

            let names: BTreeSet<&String> = p.casts.iter().map(|c| &c.name).collect();
            assert_eq!(names.len(), p.casts.len());
        }
    }
}
