use std::borrow::Cow;
/// One pool for every colony: a body takes the work it fits and reaches,
/// whoever cast it (`ARCHITECTURE.md` §5).
use std::collections::BTreeSet;

use crate::core::engine;
use crate::core::tiles;
use crate::core::vocabulary::Resource;
use crate::core::world::{Kind, Pos};

use super::bodies::{self, Pattern};
use super::derived::{Body, Derived, RepairFact, StoreKind};
use super::matcher::Assignments;
use super::price;
use super::task::{self, Task};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Capacity {
    Unbounded,
    Bodies(i32),
    Work(i32),
    /// `posts` are the garrisons' seats, so a light body never holds the seat
    /// a harvester digs into a container from.
    Seated {
        seats: i32,
        posts: i32,
        work: i32,
    },
    Load(i32),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Draw {
    Open,
    /// The stock while no other sink is hungry, which a carrier would only
    /// pour back.
    Spenders,
    /// The buffer is drawn nowhere else, so what it holds goes to the
    /// controller or to construction, never back to a refill.
    Buffer,
}

impl Draw {
    pub fn is_open(self) -> bool {
        self == Draw::Open
    }
    pub fn is_buffer(self) -> bool {
        self == Draw::Buffer
    }
}

#[derive(Clone, Debug)]
pub struct Offer {
    pub task: Task,
    pub id: String,
    pub target: Pos,
    pub priority: i32,
    pub capacity: Capacity,
    pub draw: Draw,
    /// The tile a seated offer's garrison digs from.
    pub post: Option<Pos>,
    /// Rooms a body may not take it from: another colony's, whose own stock
    /// fills it.
    pub shut: BTreeSet<String>,
}

fn carried(body: &Body, resource: &Resource) -> i32 {
    body.store.amount_of(resource)
}

pub fn garrisons(body: &Body, offer: &Offer) -> bool {
    match offer.capacity {
        Capacity::Seated { posts, .. } => posts > 0 && body.pattern.is_harvester(),
        _ => false,
    }
}

pub fn upgrades(body: &Body) -> bool {
    !body.pattern.is_harvester() && body.work > 0 && body.carry > 0
}

/// The engine withdraws before it upgrades, so an Upgrade's draw tops up what
/// the upgrade spends.
pub fn draws_buffer(body: &Body, offer: &Offer) -> bool {
    offer.draw.is_buffer()
        && body.free > 0
        && match offer.task {
            Task::Upgrade(_) => body.energy < body.work * engine::UPGRADE_DRAIN_PER_WORK,
            _ => body.energy == 0,
        }
}

/// One rule for a fresh match and a kept one. A harvester digs whatever it
/// holds, the engine dropping the overflow; an upgrader's delivery is a
/// commute for its WORK.
pub fn fits(body: &Body, offer: &Offer) -> bool {
    let half_empty = body.free > 0 && body.free * 2 >= body.store.capacity;

    match &offer.task {
        // An upgrader is cast by, and counts for, the room it stands in, so it
        // waits out a dry buffer there rather than walk to another's controller.
        Task::Upgrade(_) if body.pattern.is_upgrader() => {
            body.pos.room == offer.target.room && upgrades(body)
        }
        Task::Upgrade(_) => upgrades(body) && (body.energy > 0 || offer.draw.is_buffer()),
        _ if body.pattern.is_upgrader() => false,
        Task::Harvest(_) => {
            body.work > 0
                && (garrisons(body, offer)
                    || !body.pattern.is_harvester()
                        && (half_empty
                            || body.free > 0
                                && body.pos.within(offer.task.range_of(), &offer.target)))
        }
        _ if body.pattern.is_harvester() => false,
        Task::Withdraw(_, _) if !offer.draw.is_open() && body.work == 0 => false,
        // An ore rides alone to the store, never with a worker's energy.
        Task::Withdraw(_, r) if !r.is_energy() => {
            body.work == 0 && body.carry > 0 && body.store.held.is_empty()
        }
        Task::Withdraw(_, _) | Task::Pickup(_, _) => body.carry > 0 && half_empty,
        Task::Refill(_, resource) => {
            body.carry > 0 && carried(body, resource) > 0 && !offer.shut.contains(&body.pos.room)
        }
        Task::Build(_) | Task::Repair(_) => {
            body.work > 0 && body.carry > 0 && (body.energy > 0 || offer.draw.is_buffer())
        }
        Task::Reserve(_) | Task::Claim(_) => body.claim > 0,
    }
}

/// The walk back is read as the walk there: a load's sink is not known when it
/// is matched.
pub fn outlives(body: &Body, offer: &Offer, walk: i32) -> bool {
    let legs = match offer.task {
        Task::Withdraw(_, _) | Task::Pickup(_, _) => 2,
        _ => 1,
    };

    legs * walk < body.ticks_to_live
}

/// Only a post's garrison counts WORK, so the other seats stay open while it
/// digs short of the work.
pub fn share(body: &Body, offer: &Offer) -> i32 {
    match offer.capacity {
        Capacity::Unbounded => 0,
        Capacity::Bodies(_) => 1,
        Capacity::Seated { posts, .. } if posts > 0 && !garrisons(body, offer) => 0,
        Capacity::Work(_) | Capacity::Seated { .. } => body.work,
        Capacity::Load(_) => match &offer.task {
            Task::Refill(_, resource) => carried(body, resource),
            task if task.is_intake() => body.free,
            _ if draws_buffer(body, offer) => body.free,
            _ => body.energy,
        },
    }
}

pub fn admits(offer: &Offer, garrison: bool, held: i32, seated_alike: i32) -> bool {
    match offer.capacity {
        Capacity::Unbounded => true,
        Capacity::Bodies(n) | Capacity::Work(n) | Capacity::Load(n) => held < n,
        Capacity::Seated { seats, posts, work } => {
            held < work && seated_alike < if garrison { posts } else { seats - posts }
        }
    }
}

/// A dig counts what the body already dug, so a digger part-way through its
/// load is worth no less than an empty one.
pub fn seat_value(body: &Body, offer: &Offer, walk: i32) -> i32 {
    match offer.capacity {
        Capacity::Seated { .. } => {
            let stay = 0.max(body.ticks_to_live - walk);

            match &offer.task {
                Task::Harvest(_) => {
                    let dig = body.work * engine::HARVEST_PER_WORK * stay;

                    if body.pattern.is_harvester() {
                        dig
                    } else {
                        (body.free + body.energy).min(dig)
                    }
                }
                Task::Upgrade(_) => {
                    let upgrade = body.work * engine::UPGRADE_DRAIN_PER_WORK * stay;

                    if offer.draw.is_buffer() {
                        upgrade
                    } else {
                        body.energy.min(upgrade)
                    }
                }
                _ => body.work * stay,
            }
        }
        _ => 0,
    }
}

fn offer(task: Task, target: &Pos, value: f64, capacity: Capacity) -> Offer {
    Offer {
        id: task::task_id(&task),
        task,
        target: target.clone(),
        priority: price::economy_rank(value),
        capacity,
        draw: Draw::Open,
        post: None,
        shut: BTreeSet::new(),
    }
}

/// A lab's fill drawn from the stores that hold its reagent, no more than the
/// fills have room for, and its drain carried off to the storage like any ore.
fn lab_offers(offers: &mut Vec<Offer>, ground: &super::derived::Ground, plan: &super::labs::Plan) {
    let value = price::value_of_rank(price::SURPLUS_RANK);
    let at = |id: &str| ground.labs.iter().find(|l| l.id == id);

    for (lab, reagent, room) in &plan.fills {
        if let Some(l) = at(lab) {
            offers.push(offer(
                Task::Refill(lab.clone(), reagent.clone()),
                &l.pos,
                value,
                Capacity::Load(*room),
            ));
        }
    }

    let mut wanted: Vec<(&Resource, i32)> = Vec::new();
    for (_, reagent, room) in &plan.fills {
        match wanted.iter_mut().find(|(r, _)| *r == reagent) {
            Some((_, n)) => *n += room,
            None => wanted.push((reagent, *room)),
        }
    }
    for (store, pos, resource, amount) in &ground.ores {
        if let Some((_, room)) = wanted.iter().find(|(r, _)| *r == resource) {
            offers.push(offer(
                Task::Withdraw(store.clone(), resource.clone()),
                pos,
                value,
                Capacity::Load((*amount).min(*room)),
            ));
        }
    }

    for (lab, mineral) in &plan.drains {
        if let Some(l) = at(lab) {
            let held = l.held.as_ref().map_or(0, |h| h.1);
            offers.push(offer(
                Task::Withdraw(lab.clone(), mineral.clone()),
                &l.pos,
                value,
                Capacity::Load(held),
            ));
        }
    }
}

/// The energy the terminal lacks for the fee of the whole shipment.
pub fn fee_short(ground: &super::derived::Ground) -> i32 {
    match (&ground.terminal, &ground.ship_to) {
        (Some(t), Some(home)) => {
            let fee: i32 = ground
                .shipment
                .iter()
                .map(|(_, n)| engine::send_cost(*n, tiles::rooms_apart(&ground.room, home)))
                .sum();
            0.max(fee - t.store.amount_of(&Resource::Energy))
        }
        _ => 0,
    }
}

/// A shipment's ore carried from the storage into the terminal, and the fee's
/// energy beside it, so the terminal sends it whole.
fn shipment_offers(offers: &mut Vec<Offer>, ground: &super::derived::Ground) {
    let Some(t) = &ground.terminal else {
        return;
    };
    let value = price::value_of_rank(price::SURPLUS_RANK);
    for (ore, n) in &ground.shipment {
        let need = (n - t.store.amount_of(ore)).min(t.store.free());
        if need <= 0 {
            continue;
        }
        offers.push(offer(
            Task::Refill(t.id.clone(), ore.clone()),
            &t.pos,
            value,
            Capacity::Load(need),
        ));
        for (store, pos, resource, amount) in &ground.ores {
            if store != &t.id && resource == ore {
                offers.push(offer(
                    Task::Withdraw(store.clone(), ore.clone()),
                    pos,
                    value,
                    Capacity::Load((*amount).min(need)),
                ));
            }
        }
    }
    let fee = fee_short(ground);
    if fee > 0 {
        offers.push(offer(
            Task::Refill(t.id.clone(), Resource::Energy),
            &t.pos,
            value,
            Capacity::Load(fee.min(t.store.free())),
        ));
    }
}

fn inside_deadline(c: &crate::core::world::Controller) -> bool {
    c.level > 0 && c.ticks_to_downgrade < engine::safe_mode_downgrade_line(c.level)
}

/// A room short of its own feet: its first spawn, then its upgrade to the
/// level a tower stands at, the tower, and a tower's worth of fire (owner,
/// 2026-10-18: the tower charged first, the colony feeding itself second).
/// At the deadline's rank, so any body in reach takes it, and past the first
/// spawn only while another colony on its feet has bodies to send: a lone
/// room's bank goes to its first bodies (soak:many, RCL3 at t20090 against
/// t8727; a towerless crafted world lost its fight to the tower).
const NURSERY_RANK: i32 = price::DEADLINE_RANK;
const NURSERY_LEVEL: i32 = 3;

fn fed(derived: &Derived, room: &str) -> bool {
    derived
        .colonies
        .iter()
        .any(|c| c.room != room && c.controller.level >= NURSERY_LEVEL)
}

/// Every site alike, their order Layout's plan order, but a room's first
/// spawn and first tower.
pub fn site_rank(derived: &Derived, site: &super::derived::SiteFact) -> i32 {
    let first_spawn =
        site.kind == Kind::Spawn && derived.colonies.iter().all(|c| c.room != site.pos.room);
    let first_tower = site.kind.is_tower()
        && fed(derived, &site.pos.room)
        && derived
            .grounds
            .iter()
            .any(|g| g.room == site.pos.room && g.towers == 0);
    if first_spawn || first_tower {
        NURSERY_RANK
    } else {
        price::SURPLUS_RANK
    }
}

/// A rampart under `rampart_safe` is priced as the flow, so a fresh one is not
/// lost. A structure that outlives a creep's life loses nothing while it waits
/// for the next, so it waits behind the sites.
pub fn repair_value(r: &RepairFact) -> f64 {
    match r.kind {
        Kind::Rampart if r.hits <= super::derived::RAMPART_SAFE => price::flow_draw(0.0),
        Kind::Rampart => price::value_of_rank(price::SURPLUS_RANK + 1),
        _ if f64::from(r.hits) < r.decay * f64::from(engine::CREEP_LIFETIME) => {
            price::rebuild_at_risk(r.rebuild, r.hits, r.decay)
        }
        _ => price::value_of_rank(price::SURPLUS_RANK + 1),
    }
}

/// `held`: last tick's assignments, whose Refills are what is already aboard.
pub fn pool(derived: &Derived, held: &Assignments) -> Vec<Offer> {
    let mut offers: Vec<Offer> = Vec::new();

    // The storage is offered for every ore a body carries, so no load is
    // stranded.
    let ores: Vec<Resource> = {
        let mut seen: BTreeSet<Cow<'static, str>> = BTreeSet::new();
        let mut ores = Vec::new();
        for body in &derived.bodies {
            for (resource, _) in &body.store.held {
                let name = crate::core::vocabulary::resource_name(resource);
                if !resource.is_energy() && seen.insert(name) {
                    ores.push(resource.clone());
                }
            }
        }
        ores
    };

    // A tick a spawn waits is a tick of the income bodies earn, and every
    // colony's alike: travel picks the ring, so a richer room's never draws a
    // body past its own.
    let ring_worth: f64 = {
        let n = derived.colonies.len();
        let income: i32 = derived.grounds.iter().take(n).map(|g| g.income).sum();
        f64::from(income) / n.max(1) as f64
    };

    let colony_rooms: BTreeSet<String> = derived.colonies.iter().map(|c| c.room.clone()).collect();

    for (i, ground) in derived.grounds.iter().enumerate() {
        let colony = derived.colonies.get(i);

        // Offered through a regeneration gap too, so its holders keep their
        // seats. Priced at the rate, the engine's cap on what its seats dig.
        // Away from a colony only at a post: a worker never walks out to dig.
        for s in &ground.sources {
            if colony.is_some() || s.post.is_some() {
                let mut harvest = offer(
                    Task::Harvest(s.id.clone()),
                    &s.pos,
                    f64::from(s.rate),
                    Capacity::Seated {
                        seats: s.seats.unwrap_or(i32::MAX),
                        posts: i32::from(s.post.is_some()),
                        work: (s.rate + engine::HARVEST_PER_WORK - 1) / engine::HARVEST_PER_WORK,
                    },
                );
                harvest.post = s.post.clone();
                offers.push(harvest);
            }
        }

        // Dug only from its container's tile, whose holder is the garrison.
        for m in &ground.minerals {
            let Some(post) = m
                .post
                .as_ref()
                .filter(|_| colony.is_some() && m.container.is_some() && m.amount > 0)
            else {
                continue;
            };
            let rate = bodies::MINER_WORK * engine::MINERAL_HARVEST_PER_WORK
                / engine::MINERAL_HARVEST_CYCLE;
            let mut harvest = offer(
                Task::Harvest(m.id.clone()),
                &m.pos,
                f64::from(rate),
                Capacity::Seated {
                    seats: 1,
                    posts: 1,
                    work: bodies::MINER_WORK,
                },
            );
            harvest.post = Some(post.clone());
            offers.push(harvest);
        }

        for m in &ground.minerals {
            if let Some(id) = m.container.as_ref().filter(|_| m.held > 0) {
                let fill = f64::from(m.held) / f64::from(engine::CONTAINER_CAPACITY);
                let rate = bodies::MINER_WORK * engine::MINERAL_HARVEST_PER_WORK
                    / engine::MINERAL_HARVEST_CYCLE;
                offers.push(offer(
                    Task::Withdraw(id.clone(), m.resource.clone()),
                    m.post.as_ref().unwrap_or(&m.pos),
                    price::flow_draw(price::overflow_loss(rate, fill)),
                    Capacity::Load(m.held),
                ));
            }
        }

        if let Some(plan) = &ground.lab_plan {
            lab_offers(&mut offers, ground, plan);
        }
        shipment_offers(&mut offers, ground);

        let mut hungriest = 0.0f64;

        let ring_free: i32 = ground.refills.iter().map(|r| r.free).sum();

        let shut_to = |free: i32| -> BTreeSet<String> {
            if colony.is_some() && ground.stores.iter().any(|s| s.owned && s.energy >= free) {
                let mut shut = colony_rooms.clone();
                shut.remove(&ground.room);
                shut
            } else {
                BTreeSet::new()
            }
        };

        for r in &ground.refills {
            let value = if r.kind.is_tower() {
                price::tower_short(price::HOME_RAID_PRIOR, r.free)
            } else {
                ring_worth
            };

            hungriest = hungriest.max(value);

            let mut refill = offer(
                Task::Refill(r.id.clone(), Resource::Energy),
                &r.pos,
                value,
                Capacity::Load(r.free),
            );
            if r.kind.is_tower()
                && ground.fire < engine::TOWER_CAPACITY
                && fed(derived, &ground.room)
            {
                refill.priority = NURSERY_RANK;
            }
            refill.shut = shut_to(ring_free);
            offers.push(refill);
        }

        let draw = if ground.buffer.as_ref().is_some_and(|b| b.energy > 0) {
            Draw::Buffer
        } else {
            Draw::Open
        };

        for s in &ground.sites {
            let mut build = offer(
                Task::Build(s.id.clone()),
                &s.pos,
                price::value_of_rank(site_rank(derived, s)),
                Capacity::Load(s.left),
            );
            build.draw = draw;
            offers.push(build);
        }

        for r in &ground.repairs {
            let missing = (r.hits_max - r.hits + engine::REPAIR_HITS_PER_ENERGY - 1)
                / engine::REPAIR_HITS_PER_ENERGY;

            let mut repair = offer(
                Task::Repair(r.id.clone()),
                &r.pos,
                repair_value(r),
                Capacity::Load(missing),
            );
            repair.draw = draw;
            offers.push(repair);
        }

        match &ground.controller {
            Some(c) if c.owner.is_ours() => {
                let capacity = Capacity::Seated {
                    seats: ground.upgrade_seats.unwrap_or(i32::MAX),
                    posts: 0,
                    work: bodies::upgradable_at(c.level),
                };

                let mut upgrade = offer(
                    Task::Upgrade(c.id.clone()),
                    &c.pos,
                    price::value_of_rank(price::UPGRADE_RANK),
                    capacity,
                );
                upgrade.draw = draw;

                if inside_deadline(c) {
                    upgrade.priority = price::DEADLINE_RANK;
                } else if colony.is_some() && c.level < NURSERY_LEVEL && fed(derived, &ground.room)
                {
                    upgrade.priority = NURSERY_RANK;
                }
                offers.push(upgrade);
            }
            Some(c) if ground.reservable && colony.is_none() => {
                offers.push(offer(
                    Task::Reserve(c.id.clone()),
                    &c.pos,
                    f64::from(super::derived::reserve_gain(ground)),
                    Capacity::Bodies(1),
                ));
            }
            _ => (),
        }

        let starved = colony.is_some_and(|c| c.bank < c.bank_capacity);

        let drinks = derived
            .bodies
            .iter()
            .chain(derived.ovens.iter())
            .any(|b| upgrades(b) && b.pos.room == ground.room);

        let feed = ground.feed.as_ref().filter(|f| f.free > 0 && drinks);

        // An upgrader waiting on its buffer loses a tick of what the row hired
        // it at.
        let feed_worth = match (feed, &ground.buffer, colony) {
            (Some(_), Some(buffer), Some(c)) => {
                let upgradable = bodies::upgradable_at(c.controller.level);

                let upgraders: i32 = derived
                    .bodies
                    .iter()
                    .map(|b| i32::from(b.pattern.is_upgrader() && b.pos.room == ground.room))
                    .sum();

                bodies::sized(Pattern::Upgrader, upgradable, c.bank_capacity).map_or(0.0, |body| {
                    bodies::upgrader_hire(upgradable, upgraders, &body) * (1.0 - buffer.fill)
                })
            }
            _ => 0.0,
        };

        hungriest = hungriest.max(feed_worth);

        // A loaded body is its sink's next delivery, so a draw past the room
        // the sinks have left pours back into the store it came from.
        let sinks: BTreeSet<String> = ground
            .refills
            .iter()
            .map(|r| &r.id)
            .chain(feed.map(|f| &f.id))
            .map(|id| task::task_id(&Task::Refill(id.clone(), Resource::Energy)))
            .collect();
        let aboard: i32 = derived
            .bodies
            .iter()
            .filter(|b| held.get(&b.name).is_some_and(|id| sinks.contains(id)))
            .map(|b| b.energy)
            .sum();
        let room = ring_free + feed.map_or(0, |f| f.free) + fee_short(ground);
        // What the upgraders drain from the feed while a load walks out to it.
        let drain = colony.map_or(0, |c| {
            let work: i32 = derived
                .bodies
                .iter()
                .chain(derived.ovens.iter())
                .filter(|b| upgrades(b) && b.pos.room == ground.room)
                .map(|b| b.work * engine::UPGRADE_DRAIN_PER_WORK)
                .sum();
            work.min(bodies::upgradable_at(c.controller.level))
        });
        let walked = |from: &Pos| {
            feed.and_then(|f| from.range(&f.pos))
                .map_or(0, |r| drain * r)
        };

        // A terminal with a shipment keeps its energy for the fee.
        let sending = |id: &str| {
            ground.ship_to.is_some() && ground.terminal.as_ref().is_some_and(|t| t.id == id)
        };

        for s in &ground.stores {
            if s.energy > 0 && !s.kind.is_buffer() && !sending(&s.id) {
                let value = if s.kind.is_remains() {
                    price::flow_draw(price::transient_loss(s.energy))
                } else if s.kind == StoreKind::Container && s.source_dug > 0 {
                    price::flow_draw(price::overflow_loss(s.source_dug, s.fill))
                } else {
                    price::stock_draw(starved, hungriest)
                };

                let (draw, load) = if s.kind.is_storage() {
                    let left = room + walked(&s.pos) - aboard;
                    if left > 0 {
                        (Draw::Open, s.energy.min(left))
                    } else {
                        (Draw::Spenders, s.energy)
                    }
                } else {
                    (Draw::Open, s.energy)
                };

                let mut withdraw = offer(
                    Task::Withdraw(s.id.clone(), Resource::Energy),
                    &s.pos,
                    value,
                    Capacity::Load(load),
                );
                withdraw.draw = draw;
                offers.push(withdraw);
            }

            if s.owned && s.free > 0 {
                for r in std::iter::once(Resource::Energy).chain(ores.iter().cloned()) {
                    offers.push(offer(
                        Task::Refill(s.id.clone(), r),
                        &s.pos,
                        price::value_of_rank(price::BOTTOM_RANK),
                        Capacity::Load(s.free),
                    ));
                }
            }
        }

        if let Some(f) = feed {
            let mut refill = offer(
                Task::Refill(f.id.clone(), Resource::Energy),
                &f.pos,
                price::value_of_rank(price::BUFFER_RANK).max(feed_worth),
                Capacity::Load(f.free),
            );
            refill.shut = shut_to(room);
            offers.push(refill);
        }

        for p in &ground.piles {
            if p.resource.is_energy() {
                offers.push(offer(
                    Task::Pickup(p.id.clone(), p.resource.clone()),
                    &p.pos,
                    price::flow_draw(price::pile_decay(p.amount)),
                    Capacity::Load(p.amount),
                ));
            }
        }
    }

    for c in &derived.claims {
        offers.push(offer(
            Task::Claim(c.id.clone()),
            &c.pos,
            price::HELD_RATE,
            Capacity::Bodies(1),
        ));
    }

    offers
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use super::*;
    use crate::core::economy::bodies::Pattern;
    use crate::core::economy::dice::{self, Dice};

    fn post_offer() -> Offer {
        Offer {
            id: "h:s".to_string(),
            task: Task::Harvest("s".to_string()),
            target: dice::at(),
            priority: 0,
            capacity: Capacity::Seated {
                seats: 1,
                posts: 1,
                work: 1,
            },
            draw: Draw::Open,
            post: None,
            shut: BTreeSet::new(),
        }
    }

    #[test]
    fn a_harvester_fits_its_post_whatever_its_store_holds() {
        let mut dice = Dice::new(21);

        for i in 0..300 {
            let body = dice::body(&mut dice, i);
            let harvester = Body {
                pattern: Pattern::Harvester,
                free: 0,
                ..body
            };

            assert!(harvester.work == 0 || fits(&harvester, &post_offer()));
        }
    }
}
