/// What each committed body does this tick (§7): its walk, its strike and its heal.
use std::collections::{BTreeMap, BTreeSet};

use super::casting;
use super::kinds::defend::{self, Home};
use super::kinds::{raid, siege};
use super::scene::{Hunting, Scene, middle, picket};
use super::shape::{Engagement, Objective, Phase};
use crate::atlas::Atlas;
use crate::exchange;
use crate::intents::{self, ControllerAct, CreepAct, Intent};
use crate::mv::Wish;
use crate::tactics::{self, Tactic};
use crate::tick;
use crate::tiles::Tile;
use crate::vocabulary::Part;
use crate::world::{self, Creep, Id, Owner, Pos, RoomName, Structure};
use crate::{engine, tiles};

pub(super) fn act(
    scene: &Scene,
    atlas: &mut Atlas,
    trace: bool,
    decided: &[Engagement],
    gates: &BTreeMap<i32, tactics::Gate>,
    given_up: &BTreeSet<i32>,
    grounds: &BTreeMap<RoomName, Hunting>,
) -> Vec<Intent> {
    let world = scene.world;
    let mut sight = tactics::Sight {
        atlas,
        hostiles: &scene.foes,
        struck: scene
            .foes
            .values()
            .flatten()
            .filter(|h| scene.struck(h))
            .map(|h| h.id)
            .collect(),
        spared: world
            .rooms
            .iter()
            .map(|r| {
                let creeps = r.others.iter().filter(|c| !scene.struck(c));
                let allied = r.structures.iter().filter(|s| s.owner.is_ally());
                let at = creeps.map(|c| c.pos);
                (r.name, at.chain(allied.map(|s| s.pos)).collect())
            })
            .collect(),
        ramparts: world
            .rooms
            .iter()
            .map(|r| {
                let ours = r
                    .structures
                    .iter()
                    .filter(|s| s.kind.is_rampart() && s.owner.is_ours());
                (r.name, ours.map(|s| s.pos).collect())
            })
            .collect(),
        musters: scene.musters.clone(),
        avoid: scene.avoid,
    };

    let engaged_in: BTreeMap<&str, &Engagement> = decided
        .iter()
        .flat_map(|e| e.ours.iter().map(move |n| (n.as_str(), e)))
        .collect();

    let fighters: Vec<&Creep> = world
        .creeps
        .iter()
        .filter(|c| tick::fights(&c.name))
        .collect();

    let swings = |c: &Creep| c.body.iter().any(|p| p.kind == Part::Attack);

    // A group lost from sight is sought in its room, which brings it back in
    // sight; a siege's room out of sight is entered anywhere, as its middle
    // may lie behind its walls.
    let centre_of = |e: &Engagement| -> (Pos, i32) {
        match (
            &e.objective,
            scene.by_id.get(&e.group),
            scene.next_struck(e),
        ) {
            // His towers are spent at the edge before the set breaches: it
            // waits in the room beside his, off the tiles a pair works.
            (Objective::Siege(_), _, _) if gates.contains_key(&e.id) => {
                (middle(gates[&e.id].from), engine::ROOM_SIDE / 2)
            }
            (Objective::Siege(_), _, Some(b)) => (b.pos, engine::RANGED_RANGE),
            (Objective::Defend, Some(g), _) => (g.centre, engine::RANGED_RANGE),
            (Objective::Siege(_), _, None) => (middle(e.room), engine::ROOM_SIDE / 2),
            _ => (middle(e.room), engine::RANGED_RANGE),
        }
    };

    let mut walking: BTreeMap<String, Wish> = BTreeMap::new();
    // A siege's room is priced under its towers while out of sight.
    let fortified: BTreeSet<RoomName> = world
        .rooms
        .iter()
        .filter(|r| !raid::charged(r).is_empty())
        .map(|r| r.name)
        .chain(
            decided
                .iter()
                .filter(|e| matches!(e.objective, Objective::Siege(_)))
                .map(|e| e.room),
        )
        .collect();

    for e in decided {
        // A set that walks in as one holds its formation into the fight.
        let formation = matches!(e.plan.tactic, Tactic::BlobClose | Tactic::BlobKite)
            && !e.unanswered
            && e.phase == Phase::Approaching;
        if !formation {
            continue;
        }

        let members: Vec<&Creep> = scene
            .members_of(e)
            .into_iter()
            .filter(|c| raid::raids(e) || !swings(c))
            .collect();

        if !members.is_empty() {
            let (centre, range) = centre_of(e);

            for wish in tactics::blob(&mut sight, &centre, range, &members, &fortified) {
                walking.insert(wish.creep.clone(), wish);
            }
        }
    }

    let fronts: BTreeMap<i32, tactics::Front> = (decided.iter())
        .filter_map(|e| Some((e.id, front_of(scene, sight.atlas, e)?)))
        .collect();

    let taps: BTreeMap<i32, &world::Controller> = (decided.iter())
        .filter(|e| e.phase != Phase::Disengaging)
        .filter_map(|e| match &e.objective {
            Objective::Siege(target) => {
                Some((e.id, siege::tapped(scene.in_sight(e.room)?, *target)?))
            }
            _ => None,
        })
        .collect();

    let hunts: BTreeMap<i32, (&Hunting, bool)> = decided
        .iter()
        .filter(|e| e.objective == Objective::Sweep)
        .filter_map(|e| {
            let ground = grounds.get(&e.room)?;
            // The whole set scores its tiles at once, or none does (ibex).
            let contact = scene.members_of(e).iter().any(|m| {
                m.pos.room == e.room
                    && (ground.threat[tiles::of_pos(&m.pos)] > 0
                        || ground.marks.iter().any(|t| m.pos.within(4, &t.pos)))
            });
            Some((e.id, (ground, contact)))
        })
        .collect();

    let mut traced: Vec<Intent> = Vec::new();
    let mut lookups = 0;
    let mut step = |sight: &mut tactics::Sight, c: &Creep| -> (Option<CreepAct>, Wish) {
        let Some(&e) = engaged_in.get(c.name.as_str()) else {
            return (None, tactics::retreat(sight, c));
        };

        if let Some(controller) = taps.get(&e.id).filter(|_| tactics::claims(c)) {
            let tap = controller.upgrade_blocked == 0
                && c.pos.within(engine::MELEE_RANGE, &controller.pos);
            let act = tap.then_some(CreepAct::Unreserve(controller.id));
            return (
                act,
                tactics::toward(sight, c, &controller.pos, engine::MELEE_RANGE),
            );
        }

        // A picket walks with the set and takes its post once the set fights.
        if e.phase == Phase::Fighting
            && picket(e, c)
            && let Some(front) = fronts.get(&e.id)
            && !front.posts.is_empty()
        {
            return (None, tactics::picket(sight, front, &scene.members_of(e), c));
        }

        if e.phase == Phase::Fighting && fronts.contains_key(&e.id) && !raid::raids(e) {
            let members: Vec<&Creep> = scene
                .members_of(e)
                .into_iter()
                .filter(|m| !tactics::picket_of(m))
                .collect();
            return tactics::siege(sight, &fronts[&e.id], &members, c);
        }

        let (centre, _) = centre_of(e);
        let order = &e.plan.order;

        if e.unanswered {
            return (None, tactics::rampart(sight, c));
        }

        let wish = match e.phase {
            // The towers' room is the only one a drain presses into.
            // A drain that gave up stands off in the room beside his, casting
            // nothing more (§6.2).
            Phase::Draining => match gates.get(&e.id) {
                Some(gate) if given_up.contains(&e.id) => {
                    tactics::toward(sight, c, &middle(gate.from), engine::ROOM_SIDE / 2)
                }
                Some(gate) => tactics::drain(sight, gate, c),
                None => tactics::hold(sight, c),
            },
            Phase::Forming => tactics::hold(sight, c),
            Phase::Pursuing => tactics::close(sight, order, &centre, c),
            Phase::Disengaging => tactics::retreat(sight, c),
            Phase::Approaching | Phase::Fighting => {
                if let Some(wish) = walking.get(&c.name) {
                    return (None, wish.clone());
                }

                if raid::raids(e) {
                    return (
                        None,
                        match scene.in_sight(e.room).filter(|r| r.name == c.pos.room) {
                            Some(in_sight) => raid::dance(sight, in_sight, &scene.members_of(e), c),
                            None => tactics::toward(sight, c, &centre, engine::RANGED_RANGE),
                        },
                    );
                }

                // A transit room's hostiles are not the sweep's.
                if e.objective == Objective::Sweep && c.pos.room != e.room {
                    return (
                        None,
                        tactics::toward(sight, c, &centre, engine::RANGED_RANGE),
                    );
                }

                if let Some((ground, contact)) = hunts.get(&e.id) {
                    let mates: Vec<&Creep> = scene
                        .members_of(e)
                        .into_iter()
                        .filter(|m| m.name != c.name && m.pos.room == c.pos.room)
                        .collect();
                    let (strike, wish, scored) = tactics::hunt(
                        sight,
                        &ground.threat,
                        &ground.marks,
                        &mates,
                        (&ground.at, *contact),
                        c,
                    );
                    lookups += scored.len() as i32;
                    let at = |(t, s): &(Tile, i64)| (tiles::to_pos(c.pos.room, *t), *s);
                    if trace && !scored.is_empty() {
                        traced.push(Intent::Trace(
                            c.name.clone(),
                            scored.iter().map(at).collect(),
                        ));
                    }
                    return (strike, wish);
                }

                let tactic = &e.plan.tactic;

                if matches!(tactic, Tactic::BlobClose)
                    && tactics::exposed(sight, centre.room, &scene.members_of(e), c)
                {
                    tactics::step_back(sight, centre.room, &scene.members_of(e), c)
                } else if matches!(tactic, Tactic::Kite | Tactic::BlobKite) && !swings(c) {
                    let anchor = raid::raids(e).then_some(&centre);
                    tactics::kite(sight, anchor, order, &centre, c)
                } else if matches!(tactic, Tactic::Hold) {
                    tactics::rampart(sight, c)
                } else {
                    tactics::close(sight, order, &centre, c)
                }
            }
        };
        (None, wish)
    };

    let running = world.rooms.iter().any(|r| {
        r.controller
            .as_ref()
            .is_some_and(|c| c.owner.is_ours() && c.safe_mode > 0)
    });

    let mut acts: Vec<Intent> = Vec::new();

    if let Some(id) = defend::safe_mode(running, &homes_of(scene, decided)) {
        acts.push(Intent::Controller(id, ControllerAct::ActivateSafeMode));
    }

    let incoming: BTreeMap<String, i32> = decided
        .iter()
        .filter_map(|e| {
            let front = fronts.get(&e.id)?;
            let members: Vec<&Creep> = scene
                .members_of(e)
                .into_iter()
                .filter(|m| m.pos.room == front.breach.room)
                .collect();

            tactics::focus_of(front, &members)
                .map(|focus| (focus.name.clone(), front.breach_strike))
        })
        .collect();

    let mut chosen: BTreeMap<String, Vec<CreepAct>> = BTreeMap::new();

    for c in &fighters {
        let (own, wish) = step(&mut sight, c);
        let engaged = engaged_in.get(c.name.as_str()).copied();
        let order = engaged.map_or(&[][..], |e| e.plan.order.as_slice());
        let mut wanted: Vec<CreepAct> = own.into_iter().collect();
        // A hunt's scored shot stands for the reflex.
        let raid = engaged.filter(|e| raid::raids(e));
        if wanted.iter().all(|a| a.slots() & intents::RANGED == 0) {
            let razed = raid
                .and_then(|e| scene.in_sight(e.room))
                .filter(|r| r.name == c.pos.room)
                .and_then(|room| raid::volley(&sight, room, c));

            match (raid.is_some(), razed) {
                (true, Some(act)) => wanted.push(act),
                (armed, _) => wanted.extend(tactics::strike(&sight, order, c, armed)),
            }
        }
        chosen.insert(c.name.clone(), wanted);
        acts.push(Intent::Wish(c.pos.room, wish));
    }

    acts.extend(traced);
    acts.push(Intent::Count("threatLookups", lookups));
    let busy = |slot: u8| -> BTreeSet<String> {
        let taken = chosen
            .iter()
            .filter(|(_, a)| a.iter().any(|a| a.slots() & slot != 0));
        taken.map(|(n, _)| n.clone()).collect()
    };
    let (swinging, shooting) = (busy(intents::MELEE), busy(intents::RANGED));
    for (healer, act) in tactics::heals(&fighters, &world.creeps, &incoming, &swinging, &shooting) {
        chosen.entry(healer).or_default().push(act);
    }

    acts.extend(chosen.into_iter().flat_map(|(name, wanted)| {
        let done = intents::performed(&wanted);
        done.into_iter()
            .map(move |a| Intent::Creep(name.clone(), a))
    }));
    acts
}

/// A siege's wall and what holds it: his line, his towers' strike on it, and
/// the posts a picket denies him from.
fn front_of(scene: &Scene, atlas: &Atlas, e: &Engagement) -> Option<tactics::Front> {
    let (Objective::Siege(_), Some(room), Some(wall)) =
        (&e.objective, scene.in_sight(e.room), scene.next_struck(e))
    else {
        return None;
    };
    let towers = raid::charged(room);
    let fired: BTreeSet<&Id> = scene.towers_in(room).iter().map(|t| &t.id).collect();
    let struck = (scene.world.attacks.iter())
        .find(|(by, _)| fired.contains(by))
        .map(|(_, at)| *at);
    let breach_strike = (towers.iter())
        .map(|t| t.range(&wall.pos).map_or(0, engine::tower_attack_at))
        .sum();
    let posts = if scene.members_of(e).iter().any(|c| tactics::picket_of(c)) {
        siege::exposed(atlas, room)
            .into_iter()
            .map(|post| {
                let strike = exchange::tower_strike(&towers, &post);
                let worth = siege::denied(room, &post);
                (post, casting::squad(e.capacity, strike, worth).len())
            })
            .filter(|(_, n)| *n > 0)
            .collect()
    } else {
        Vec::new()
    };
    Some(tactics::Front {
        breach: wall.pos,
        wall: (!room.controller.as_ref().is_some_and(|c| c.id == wall.id)).then_some(wall.id),
        line: (room.structures.iter().filter(|s| Scene::rival(s)))
            .map(|s| s.pos)
            .collect(),
        posts,
        towers,
        struck,
        breach_strike,
    })
}

/// Each home in sight with a hostile in it as safe mode reads it: its raid,
/// what stands and what fell. One with none never spends it.
fn homes_of(scene: &Scene, decided: &[Engagement]) -> Vec<Home> {
    let world = scene.world;
    let mut homes_lost: Vec<Home> = Vec::new();

    for name in &scene.homes {
        let raided = |r: &&world::Room| r.hostiles().next().is_some();
        let Some(room) = scene.in_sight(*name).filter(raided) else {
            continue;
        };

        let own: Vec<Structure> = room
            .structures
            .iter()
            .filter(|s| s.owner.is_ours())
            .cloned()
            .collect();

        let raiding: Vec<&Engagement> = decided
            .iter()
            .filter(|e| e.unanswered && e.room == *name && scene.by_id.contains_key(&e.group))
            .collect();

        let raid: BTreeSet<Id> = raiding
            .iter()
            .flat_map(|e| scene.by_id[&e.group].members.iter().copied())
            .collect();

        if let Some(controller) = &room.controller {
            let opened = raiding
                .iter()
                .filter(|e| {
                    room.hostiles().any(|h| {
                        h.owner != Owner::Invader && scene.by_id[&e.group].members.contains(&h.id)
                    })
                })
                .fold(world.time, |first, e| first.min(e.opened));

            homes_lost.push(Home {
                controller: controller.clone(),
                towered: own.iter().any(|s| s.kind.is_tower()),
                raid: room
                    .hostiles()
                    .filter(|h| raid.contains(&h.id))
                    .cloned()
                    .collect(),
                hostiles: room.hostiles().cloned().collect(),
                ours: own,
                bodies: world
                    .creeps
                    .iter()
                    .filter(|c| c.pos.room == *name)
                    .cloned()
                    .collect(),
                time: world.time,
                opened,
                fallen: room
                    .remains
                    .iter()
                    .filter_map(|r| r.fallen.clone())
                    .collect(),
            });
        }
    }

    homes_lost
}
