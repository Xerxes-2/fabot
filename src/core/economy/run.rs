/// The pool, the match, the quota, and the acts and walks they imply.
use std::collections::{BTreeMap, BTreeSet};

use crate::core::HashMap;
use crate::core::atlas::{self, Atlas, Mover, Target};
use crate::core::engine;
use crate::core::grid::{self, Footprint};
use crate::core::intents::{Ask, CreepAct, Intent, LabAct, LinkAct, Rank, TerminalAct};
use crate::core::mv::{Wish, plan};
use crate::core::state::State;
use crate::core::tick::{self, Context};
use crate::core::tiles::{self};
use crate::core::vocabulary::{BodyPart, Resource};
use crate::core::world::{Creep, Pos, RoomName};

use super::bodies;
use super::derived::{self, Body, Derived, Ground, LinkFact};
use super::matcher::{self, Assignments};
use super::pool::{self, Offer};
use super::price;
use super::quota::{self, Walks};
use super::task::{self, Task};

fn weight_of(priority: i32) -> i32 {
    (price::BOTTOM_RANK - priority + 1).max(1)
}

fn target_of(offer: &Offer) -> Target {
    Target {
        room: offer.target.room,
        tiles: vec![tiles::of_pos(&offer.target)],
        range: offer.task.range_of(),
        avoid: BTreeSet::new(),
        max_hops: tick::REACH_HOPS,
    }
}

/// The ticks from `a` to `b`: in one room its range, else its crossings off
/// a tree `Derived` grew from either end, a search only without one.
fn distance(atlas: &Atlas, derived: &Derived, a: &Pos, b: &Pos) -> Option<i32> {
    if let Some(r) = a.range(b) {
        return Some(r);
    }
    let tree = (derived.routes.get(&b.room)).or_else(|| derived.routes.get(&a.room));
    let crossed = match tree {
        Some(tree) => tree.crossings(a.room, b.room),
        None => (atlas.route(&BTreeSet::new(), tick::REACH_HOPS, a.room, b.room))
            .map(|r| r.len() as i32 - 1),
    };
    crossed.map(|n| Atlas::crossing_ticks() * n)
}

/// Re-chosen only on arrival off the exit band, so a walk is never turned
/// mid-way nor on a border; a scout whose open rooms lie past `REACH_HOPS`
/// shares a held one rather than idle.
fn scouting(atlas: &Atlas, derived: &Derived, held: &Assignments) -> Assignments {
    let mut scouts: Vec<&Body> = (derived.bodies.iter())
        .filter(|b| b.pattern.is_scout())
        .collect();
    scouts.sort_by(|a, b| a.name.cmp(&b.name));
    let kept = |b: &Body| match held.get(&b.name).and_then(|id| task::of_task_id(id)) {
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
            atlas::onward(rooms.filter_map(|(at, age, _)| {
                Some((at.room, *age, distance(atlas, derived, &b.pos, at)?))
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

fn near(
    atlas: &mut Atlas,
    derived: &Derived,
    homes: &[Pos],
    can: &dyn Fn(usize) -> bool,
    at: &Pos,
) -> Option<usize> {
    tick::nearest(atlas, &|room| derived.routes.get(&room), homes, can, at)
}

fn ticks_to(
    atlas: &Atlas,
    derived: &Derived,
    mover: Mover,
    target: &Target,
    from: &Pos,
) -> Option<i32> {
    match derived.routes.get(&target.room) {
        Some(tree)
            if from.room != target.room
                && target.avoid.is_empty()
                && target.max_hops == tick::REACH_HOPS =>
        {
            atlas.ticks_on(tree, mover, target, from)
        }
        _ => atlas.ticks_to(mover, target, from),
    }
}

pub struct Walked<'a> {
    pub atlas: &'a mut Atlas,
    pub derived: &'a Derived,
    pub homes: &'a [Pos],
    pub creeps: &'a BTreeMap<&'a str, &'a Creep>,
    pub movers: &'a BTreeMap<&'a str, Mover>,
    pub offers: &'a [Offer],
    pub by_id: &'a HashMap<&'a str, usize>,
    pub assignments: &'a Assignments,
}

impl Walks for Walked<'_> {
    fn distance(&mut self, a: &Pos, b: &Pos) -> Option<i32> {
        distance(&*self.atlas, self.derived, a, b)
    }

    fn leg(&mut self, body: &[BodyPart], from: &Pos, onto: &Pos, range: i32) -> Option<i32> {
        let mover = Mover {
            footprint: Footprint::One,
            factor: grid::loaded_factor(body),
        };
        ticks_to(
            self.atlas,
            self.derived,
            mover,
            &tick::target_at(onto, range),
            from,
        )
    }

    fn near(&mut self, can: &dyn Fn(usize) -> bool, at: &Pos) -> Option<usize> {
        near(&mut *self.atlas, self.derived, self.homes, can, at)
    }

    fn walk(&mut self, fresh: bool, body: &Body, from: &Pos) -> Option<i32> {
        let creep = *self.creeps.get(body.name.as_str())?;
        let id = self.assignments.get(&body.name)?;
        let oi = *self.by_id.get(id.as_str())?;
        let offer = &self.offers[oi];

        let mover = if fresh {
            let kinds: Vec<BodyPart> = creep.body.iter().map(|p| p.kind).collect();
            Mover {
                footprint: Footprint::One,
                factor: grid::empty_factor(&kinds),
            }
        } else {
            *self.movers.get(body.name.as_str())?
        };

        ticks_to(self.atlas, self.derived, mover, &target_of(offer), from)
    }
}

/// A body digs from nowhere else, so a harvester's dig falls into the container
/// and a light body keeps off the post.
fn seats_of(atlas: &Atlas, derived: &Derived, body: &Body, offer: &Offer) -> Option<Vec<Pos>> {
    match &offer.task {
        Task::Harvest(id) => {
            let seats = atlas.seats(Footprint::One, offer.task.range_of(), &offer.target);

            match (seats.is_empty(), derived.posts.get(id)) {
                (true, _) => None,
                (false, Some(post)) if pool::garrisons(body, offer) => Some(vec![*post]),
                (false, Some(post)) => Some(seats.into_iter().filter(|p| p != post).collect()),
                (false, None) => Some(seats),
            }
        }
        Task::Upgrade(_) if pool::draws_buffer(body, offer) => derived
            .grounds
            .iter()
            .find(|g| g.room == offer.target.room)
            .and_then(|g| {
                if g.buffer_seats.is_empty() {
                    None
                } else {
                    Some(g.buffer_seats.clone())
                }
            }),
        Task::Build(_) | Task::Repair(_) if pool::draws_buffer(body, offer) => derived
            .grounds
            .iter()
            .find_map(|g| {
                if g.room == offer.target.room {
                    g.buffer.as_ref()
                } else {
                    None
                }
            })
            .map(|b| atlas.seats(Footprint::One, 1, &b.pos))
            .filter(|seats| !seats.is_empty()),
        _ => None,
    }
}

/// A seat no other body stands on, so one body on the nearest seat does not
/// hold the crew short of the rest.
fn seat_of(stood: &BTreeSet<Pos>, creep: &Creep, offer: &Offer, seats: Option<&[Pos]>) -> Target {
    let free = |p: &Pos| p == &creep.pos || !stood.contains(p);

    let tiles: Option<Vec<Pos>> = match seats {
        Some(seats) => {
            let open: Vec<Pos> = seats.iter().filter(|p| free(p)).copied().collect();
            if open.is_empty() {
                Some(seats.to_vec())
            } else {
                Some(open)
            }
        }
        None => None,
    };

    match tiles {
        Some(tiles) if !tiles.is_empty() => Target {
            room: offer.target.room,
            tiles: tiles.iter().map(tiles::of_pos).collect(),
            range: 0,
            avoid: BTreeSet::new(),
            max_hops: tick::REACH_HOPS,
        },
        _ => target_of(offer),
    }
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
/// where it stands. `taken`: the bodies whose act is already a pickup.
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
                g.piles
                    .iter()
                    .find(|p| p.resource.is_energy() && b.pos.within(1, &p.pos) && b.serves(colony))
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

/// An assigned body's acts this tick, and its walk: to a free seat of its
/// Task, else in its range.
fn work(
    atlas: &mut Atlas,
    derived: &Derived,
    (creep, body, mover): (&Creep, &Body, Mover),
    offer: &Offer,
    stood: &BTreeSet<Pos>,
    ready: &dyn Fn(&Task) -> bool,
    acts: &mut Vec<Intent>,
) -> Wish {
    let seats = seats_of(&*atlas, derived, body, offer);

    let buffer = derived
        .grounds
        .iter()
        .filter(|g| g.room == creep.pos.room)
        .find_map(|g| g.buffer.as_ref());

    // An empty buffer has nothing to draw: the engine refuses it.
    if let Some(b) = buffer
        && b.energy > 0
        && pool::draws_buffer(body, offer)
        && creep.pos.within(1, &b.pos)
    {
        acts.push(Intent::Creep(
            creep.name.clone(),
            CreepAct::Withdraw(b.id.clone(), Resource::Energy),
        ));
    }

    if let Some(act) = raising(derived, body, offer) {
        acts.push(Intent::Creep(creep.name.clone(), act))
    } else {
        let arrived = creep.pos.within(offer.task.range_of(), &offer.target)
            && seats
                .as_ref()
                .is_none_or(|seats| seats.iter().any(|p| p == &creep.pos))
            && ready(&offer.task);

        // The API refuses an upgrade from an empty store before
        // the draw lands.
        let can_act =
            body.energy > 0 || !(pool::draws_buffer(body, offer) || offer.task.is_upgrade());

        if arrived
            && can_act
            && let Some(act) = act_of(derived, &offer.task)
        {
            acts.push(Intent::Creep(creep.name.clone(), act));
        }
    }

    if pool::garrisons(body, offer) && body.energy > 0 {
        let link = derived.grounds.iter().find_map(|g| {
            g.links.iter().find(|l| {
                g.receiver.is_some()
                    && l.role.is_source()
                    && l.free > 0
                    && creep.pos.within(1, &l.pos)
            })
        });

        if let Some(l) = link {
            acts.push(Intent::Creep(
                creep.name.clone(),
                CreepAct::Transfer(l.id.clone(), Resource::Energy),
            ));
        }
    }

    let target = seat_of(stood, creep, offer, seats.as_deref());
    plan::toward(atlas, creep, mover, &target, weight_of(offer.priority))
}

pub fn decide(context: &mut Context, state: &State) -> Result<(Vec<Intent>, State), String> {
    // `{ World with Creeps = filtered }`: a creep a fight holds is not in the
    // pool, and only the creep list differs.
    let seen: Vec<&Creep> = context
        .world
        .creeps
        .iter()
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

    let creeps: BTreeMap<&str, &Creep> = seen.iter().map(|c| (c.name.as_str(), *c)).collect();

    let movers: BTreeMap<&str, Mover> = seen
        .iter()
        .map(|c| {
            let mover = Mover {
                footprint: Footprint::One,
                factor: grid::factor_of(c),
            };
            (c.name.as_str(), mover)
        })
        .collect();

    let homes: Vec<Pos> = derived.colonies.iter().map(|c| c.spawns[0].pos).collect();
    let owners = quota::owners(
        &mut |can, at| near(&mut *context.atlas, &derived, &homes, can, at),
        &derived,
    );
    let offers = pool::pool(&derived, &owners, &state.assignments);

    let by_id: HashMap<&str, usize> = offers
        .iter()
        .enumerate()
        .map(|(i, offer)| (offer.id.as_str(), i))
        .collect();

    let mut hired = state.assignments.clone();
    hired.extend(scouting(&*context.atlas, &derived, &state.assignments));
    let assignments = {
        let mut travel = |body: &Body, offer: &Offer| -> Option<i32> {
            let mover = *movers.get(body.name.as_str())?;
            ticks_to(context.atlas, &derived, mover, &target_of(offer), &body.pos)
        };
        // Each step moves a body at most a tile, in a tick at least.
        let floor = |body: &Body, offer: &Offer| {
            body.pos
                .range(&offer.target)
                .map_or(0, |r| r - offer.task.range_of())
        };
        matcher::match_all(&mut travel, &floor, &offers, &derived.bodies, &hired)
    };

    let plan = {
        let mut walked = Walked {
            atlas: &mut *context.atlas,
            derived: &derived,
            homes: &homes,
            creeps: &creeps,
            movers: &movers,
            offers: &offers,
            by_id: &by_id,
            assignments: &assignments,
        };
        quota::plan(&mut walked, &derived, &owners, &offers, &assignments)
    };

    let mut intents: Vec<Intent> = plan
        .casts
        .iter()
        .map(|cast| {
            Intent::Queue(
                cast.colony,
                Ask {
                    body: cast.body.clone(),
                    name: cast.name.clone(),
                    rank: if cast.floor {
                        Rank::Floor
                    } else {
                        Rank::Economy
                    },
                },
            )
        })
        .collect();
    let mut owed: BTreeMap<(RoomName, bodies::Pattern), i32> = BTreeMap::new();
    for seat in &plan.seats {
        if let Some(ci) = seat.colony {
            *owed
                .entry((derived.colonies[ci].room, seat.pattern))
                .or_default() += 1;
        }
    }
    for cast in plan.casts.iter().filter(|c| !c.floor) {
        if let Some(pattern) = bodies::pattern_of_name(&cast.name) {
            *owed.entry((cast.colony, pattern)).or_default() -= 1;
        }
    }
    intents.extend(
        (owed.into_iter())
            .filter(|(_, n)| *n > 0)
            .map(|((room, p), n)| Intent::Short(room, p, n)),
    );

    let dug = dug_out(&derived);

    let ready = |task: &Task| -> bool {
        match task {
            Task::Harvest(id) => !dug.contains(id),
            _ => true,
        }
    };

    let stood: BTreeSet<Pos> = seen.iter().map(|c| c.pos).collect();

    let bodies: BTreeMap<&str, &Body> = derived
        .bodies
        .iter()
        .map(|b| (b.name.as_str(), b))
        .collect();

    let mut acts: Vec<Intent> = Vec::new();
    let mut wishes: Vec<(RoomName, Vec<Wish>)> = Vec::new();

    for creep in &seen {
        let mover = movers[creep.name.as_str()];

        let assigned = assignments
            .get(&creep.name)
            .and_then(|id| by_id.get(id.as_str()))
            .map(|oi| &offers[*oi]);

        match assigned {
            Some(offer) => {
                let body = bodies[creep.name.as_str()];
                let wish = work(
                    context.atlas,
                    &derived,
                    (creep, body, mover),
                    offer,
                    &stood,
                    &ready,
                    &mut acts,
                );
                wish_in(&mut wishes, creep.pos.room, wish);
            }
            None if !derived.colonies.is_empty()
                && derived.colonies.iter().all(|c| c.room != creep.pos.room) =>
            {
                // Home, where it can be matched: the last of equal walks.
                let walk = |c: &&derived::Colony| {
                    distance(&*context.atlas, &derived, &creep.pos, &c.spawns[0].pos)
                        .unwrap_or(i32::MAX)
                };
                let home = derived
                    .colonies
                    .iter()
                    .rev()
                    .min_by_key(walk)
                    .map_or(creep.pos, |c| c.spawns[0].pos);

                wish_in(
                    &mut wishes,
                    creep.pos.room,
                    plan::toward(context.atlas, creep, mover, &tick::target_at(&home, 1), 1),
                );
            }
            None => wish_in(
                &mut wishes,
                creep.pos.room,
                plan::stay(&*context.atlas, creep, mover.factor, 1),
            ),
        }
    }

    let moves = (wishes.into_iter())
        .flat_map(|(room, list)| list.into_iter().map(move |w| Intent::Wish(room, w)));

    let taken: BTreeSet<String> = (acts.iter())
        .filter_map(|act| match act {
            Intent::Creep(name, CreepAct::Pickup(_)) => Some(name.clone()),
            _ => None,
        })
        .collect();

    intents.extend(acts);
    intents.extend(pickups(&derived, &taken));
    intents.extend(sends(&derived));
    intents.extend(moves);

    let mut next = state.clone();
    next.assignments = assignments;

    Ok((intents, next))
}

/// A garrison full on its post raises or keeps its container; away from a
/// colony no crew raises one nor stays to keep it.
fn raising(derived: &Derived, body: &Body, offer: &Offer) -> Option<CreepAct> {
    let at = &body.pos;
    if !(pool::garrisons(body, offer)
        && body.energy > 0
        && body.free == 0
        && offer.post.as_ref() == Some(at))
    {
        return None;
    }
    (derived.grounds.iter().skip(derived.colonies.len())).find_map(|g| {
        match g.sites.iter().find(|s| s.pos == *at) {
            Some(site) => Some(CreepAct::Build(site.id.clone())),
            None => (g.repairs.iter())
                .find(|r| r.pos == *at && r.kind.is_container() && 2 * r.hits < r.hits_max)
                .map(|r| CreepAct::Repair(r.id.clone())),
        }
    })
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::*;
    use crate::core::economy::dice::{self, Dice};
    use crate::core::state::State;

    #[test]
    fn decide_never_faults_and_assigns_only_offered_tasks() {
        let mut dice = Dice::new(51);

        for _ in 0..100 {
            let g = dice::world(&mut dice);
            let mut atlas = Atlas::of_world(&g.world);
            let sightings = BTreeMap::new();
            let declarations = tick::undeclared();
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

            let (intents, state) = decide(&mut context, &State::empty()).expect("a world decides");

            for (name, id) in &state.assignments {
                assert!(
                    crate::core::economy::task::of_task_id(id).is_some(),
                    "{name} {id}"
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
