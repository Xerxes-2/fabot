/// The pool, the match, the quota, and the acts and walks they imply.
use std::collections::{BTreeMap, BTreeSet};

use crate::core::HashMap;
use crate::core::atlas::{Atlas, Mover, Target};
use crate::core::grid::{self, Footprint};
use crate::core::intents::{Ask, CreepAct, Intent, LinkAct, Rank};
use crate::core::mv::{Wish, plan};
use crate::core::state::State;
use crate::core::tick::{self, Context};
use crate::core::tiles::{self};
use crate::core::vocabulary::{BodyPart, Resource};
use crate::core::world::{Creep, Pos};

use super::derived::{self, Body, Derived, Ground, LinkFact};
use super::matcher::{self, Assignments};
use super::pool::{self, Offer};
use super::price;
use super::quota::{self, Walks};
use super::task::Task;

fn weight_of(priority: i32) -> i32 {
    (price::BOTTOM_RANK - priority + 1).max(1)
}

fn target_of(offer: &Offer) -> Target {
    Target {
        room: offer.target.room.clone(),
        tiles: vec![tiles::of_pos(&offer.target)],
        range: offer.task.range_of(),
        avoid: BTreeSet::new(),
        max_hops: tick::REACH_HOPS,
    }
}

/// `Atlas::route` without a search from a room `Derived` holds the tree of.
fn route_of(atlas: &Atlas, derived: &Derived, target: &Target, from: &str) -> Option<Vec<String>> {
    if target.avoid.is_empty()
        && target.max_hops == tick::REACH_HOPS
        && let Some(tree) = derived.routes.get(from)
    {
        return tree.route(from, &target.room);
    }
    atlas.route(&target.avoid, target.max_hops, from, &target.room)
}

/// `Atlas::ticks_to`, whose flood needs the route; `route_of` reads the trees,
/// so its answer is taken before the Atlas is borrowed mutably.
fn ticks_to(
    atlas: &mut Atlas,
    derived: &Derived,
    mover: Mover,
    target: &Target,
    from: &Pos,
) -> Option<i32> {
    if from.room == target.room {
        atlas.travel_ticks(mover, target, from)
    } else {
        let rooms = route_of(atlas, derived, target, &from.room);
        atlas.ticks_to(
            move |_: &Target, _: &str| rooms.clone(),
            mover,
            target,
            from,
        )
    }
}

fn distance(atlas: &Atlas, derived: &Derived, a: &Pos, b: &Pos) -> Option<i32> {
    match a.range(b) {
        Some(r) => Some(r),
        None => route_of(atlas, derived, &tick::target_at(b, 0), &a.room)
            .map(|rooms| Atlas::crossing_ticks() * (rooms.len() as i32 - 1)),
    }
}

/// `Tick::nearest` over the routes it will ask for, read before its floods.
fn near(
    atlas: &mut Atlas,
    derived: &Derived,
    homes: &[Pos],
    can: &dyn Fn(usize) -> bool,
    at: &Pos,
) -> Option<usize> {
    // (target room, from room) to the route `Tick::nearest` asks for.
    type Route = ((String, String), Option<Vec<String>>);
    let mut routes: Vec<Route> = Vec::new();

    for home in homes {
        routes.push((
            (home.room.clone(), at.room.clone()),
            route_of(atlas, derived, &tick::target_at(home, 1), &at.room),
        ));
        routes.push((
            (at.room.clone(), home.room.clone()),
            route_of(atlas, derived, &tick::target_at(at, 0), &home.room),
        ));
    }

    let route = move |target: &Target, from: &str| -> Option<Vec<String>> {
        routes
            .iter()
            .find(|((there, start), _)| there == &target.room && start == from)
            .and_then(|(_, rooms)| rooms.clone())
    };

    tick::nearest(atlas, route, homes, can, at)
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
            &mut *self.atlas,
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

        ticks_to(
            &mut *self.atlas,
            self.derived,
            mover,
            &target_of(offer),
            from,
        )
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
                (false, Some(post)) if pool::garrisons(body, offer) => Some(vec![post.clone()]),
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
            let open: Vec<Pos> = seats.iter().filter(|p| free(p)).cloned().collect();
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
            room: offer.target.room.clone(),
            tiles: tiles.iter().map(tiles::of_pos).collect(),
            range: 0,
            avoid: BTreeSet::new(),
            max_hops: tick::REACH_HOPS,
        },
        _ => target_of(offer),
    }
}

fn act_of(derived: &Derived, task: &Task) -> CreepAct {
    let held = |id: &str| {
        derived.grounds.iter().any(|g| {
            g.controller
                .as_ref()
                .is_some_and(|c| c.id == id && derived::invader_held(c))
        })
    };

    match task {
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
    }
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
            let pile = derived.grounds.iter().find_map(|g| {
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

fn wish_in(groups: &mut Vec<(String, Vec<Wish>)>, room: &str, wish: Wish) {
    match groups.iter_mut().find(|(each, _)| each == room) {
        Some((_, list)) => list.push(wish),
        None => groups.push((room.to_string(), vec![wish])),
    }
}

pub fn decide(context: &mut Context, state: &State) -> Result<(Vec<Intent>, State), String> {
    let worked = tick::worked(context);

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
        &worked,
        &[],
    );

    let offers = pool::pool(&derived, &state.assignments);

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

    let by_id: HashMap<&str, usize> = offers
        .iter()
        .enumerate()
        .map(|(i, offer)| (offer.id.as_str(), i))
        .collect();

    let homes: Vec<Pos> = derived
        .colonies
        .iter()
        .map(|c| c.spawns[0].pos.clone())
        .collect();

    let assignments = {
        let mut travel = |body: &Body, offer: &Offer| -> Option<i32> {
            let mover = *movers.get(body.name.as_str())?;
            ticks_to(
                &mut *context.atlas,
                &derived,
                mover,
                &target_of(offer),
                &body.pos,
            )
        };
        matcher::match_all(&mut travel, &offers, &derived.bodies, &state.assignments)
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
        quota::plan(&mut walked, &derived, &offers, &assignments)
    };

    let mut intents: Vec<Intent> = plan
        .casts
        .iter()
        .map(|cast| {
            Intent::Queue(
                cast.colony.clone(),
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

    // A dug-out source's holders stay seated until it regenerates.
    let dug_out: BTreeSet<String> = derived
        .grounds
        .iter()
        .flat_map(|g| g.sources.iter())
        .filter(|s| s.energy == 0)
        .map(|s| s.id.clone())
        .collect();

    let ready = |task: &Task| -> bool {
        match task {
            Task::Harvest(id) => !dug_out.contains(id),
            _ => true,
        }
    };

    let stood: BTreeSet<Pos> = seen.iter().map(|c| c.pos.clone()).collect();

    let bodies: BTreeMap<&str, &Body> = derived
        .bodies
        .iter()
        .map(|b| (b.name.as_str(), b))
        .collect();

    let mut acts: Vec<Intent> = Vec::new();
    let mut wishes: Vec<(String, Vec<Wish>)> = Vec::new();

    for creep in &seen {
        let mover = movers[creep.name.as_str()];

        let assigned = assignments
            .get(&creep.name)
            .and_then(|id| by_id.get(id.as_str()))
            .map(|oi| &offers[*oi]);

        match assigned {
            Some(offer) => {
                let body = bodies[creep.name.as_str()];
                let seats = seats_of(&*context.atlas, &derived, body, offer);

                let buffer = derived
                    .grounds
                    .iter()
                    .filter(|g| g.room == creep.pos.room)
                    .find_map(|g| g.buffer.as_ref());

                if let Some(b) = buffer
                    && pool::draws_buffer(body, offer)
                    && creep.pos.within(1, &b.pos)
                {
                    acts.push(Intent::Creep(
                        creep.name.clone(),
                        CreepAct::Withdraw(b.id.clone(), Resource::Energy),
                    ));
                }

                // Away from a colony no crew raises a container nor stays to
                // keep it.
                let raising = if pool::garrisons(body, offer)
                    && body.energy > 0
                    && body.free == 0
                    && offer.post.as_ref() == Some(&creep.pos)
                {
                    derived
                        .grounds
                        .iter()
                        .skip(derived.colonies.len())
                        .find_map(|g| match g.sites.iter().find(|s| s.pos == creep.pos) {
                            Some(site) => Some(CreepAct::Build(site.id.clone())),
                            None => g
                                .repairs
                                .iter()
                                .find(|r| {
                                    r.pos == creep.pos
                                        && r.kind.is_container()
                                        && 2 * r.hits < r.hits_max
                                })
                                .map(|r| CreepAct::Repair(r.id.clone())),
                        })
                } else {
                    None
                };

                if let Some(act) = &raising {
                    acts.push(Intent::Creep(creep.name.clone(), act.clone()))
                } else {
                    let arrived = creep.pos.within(offer.task.range_of(), &offer.target)
                        && seats
                            .as_ref()
                            .is_none_or(|seats| seats.iter().any(|p| p == &creep.pos))
                        && ready(&offer.task);

                    // The API refuses an upgrade from an empty store before
                    // the draw lands.
                    let can_act = body.energy > 0
                        || !(pool::draws_buffer(body, offer) || offer.task.is_upgrade());

                    if arrived && can_act {
                        acts.push(Intent::Creep(
                            creep.name.clone(),
                            act_of(&derived, &offer.task),
                        ));
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

                let target = seat_of(&stood, creep, offer, seats.as_deref());
                wish_in(
                    &mut wishes,
                    &creep.pos.room,
                    plan::toward(
                        context.atlas,
                        creep,
                        mover,
                        &target,
                        weight_of(offer.priority),
                    ),
                );
            }
            None if !derived.colonies.is_empty()
                && derived.colonies.iter().all(|c| c.room != creep.pos.room) =>
            {
                // Home, where it can be matched.
                let mut home = derived.colonies[0].spawns[0].pos.clone();
                let mut best =
                    distance(&*context.atlas, &derived, &creep.pos, &home).unwrap_or(i32::MAX);

                for colony in &derived.colonies[1..] {
                    let at = &colony.spawns[0].pos;
                    let d = distance(&*context.atlas, &derived, &creep.pos, at).unwrap_or(i32::MAX);

                    // `Array.minBy`: the last of equal keys.
                    if d <= best {
                        best = d;
                        home = at.clone();
                    }
                }

                wish_in(
                    &mut wishes,
                    &creep.pos.room,
                    plan::toward(context.atlas, creep, mover, &tick::target_at(&home, 1), 1),
                );
            }
            None => wish_in(
                &mut wishes,
                &creep.pos.room,
                plan::stay(&*context.atlas, creep, mover.factor, 1),
            ),
        }
    }

    let mut moves: Vec<Intent> = Vec::new();
    for (room, list) in wishes {
        for wish in list {
            moves.push(Intent::Wish(room.clone(), wish));
        }
    }

    let mut sends: Vec<Intent> = Vec::new();
    for g in &derived.grounds {
        link_sends(&mut sends, g);
    }

    let mut taken: BTreeSet<String> = BTreeSet::new();
    for act in &acts {
        if let Intent::Creep(name, CreepAct::Pickup(_)) = act {
            taken.insert(name.clone());
        }
    }

    intents.extend(acts);
    intents.extend(pickups(&derived, &taken));
    intents.extend(sends);
    intents.extend(moves);

    let mut next = state.clone();
    next.assignments = assignments;

    Ok((intents, next))
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

            let mut context = Context {
                world: &g.world,
                atlas: &mut atlas,
                declarations: &declarations,
                sightings: &sightings,
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
