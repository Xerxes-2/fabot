/// The fight's tick (`ARCHITECTURE.md` §6): who is out there, what we committed,
/// what each committed body does, and what it asks for.
use std::collections::{BTreeMap, BTreeSet};

use super::kinds::{defend, raid, siege, sweep};
use super::scene::{Asked, Hunting, Scene, fresh};
use super::shape::{Cast, Engagement, FightState, Group, Objective, Phase};
use super::{casting, tracker};
use fabot_atlas::{self as atlas, Atlas, Context};
use fabot_base::declarations::{self, OpKind};
use fabot_base::engine;
use fabot_base::intents::{Ask, ModuleOutput, Rank};
use fabot_base::world::{Creep, Id, Pos, RoomName, StructureType, hostiles, sightings};
use fabot_tactics::{self as tactics, Tactic};

/// A merge keeps the older group's id; two engagements on one group are one,
/// the older surviving with both's bodies, losses and budget. Also each
/// merged id's survivor.
fn merged(
    events: &[tracker::Event],
    engagements: &[Engagement],
) -> (Vec<Engagement>, BTreeMap<i32, i32>) {
    let mut ranked = engagements.to_vec();
    for e in &mut ranked {
        for event in events {
            if let tracker::Event::Merged { into, from } = event
                && e.group == *from
            {
                e.group = *into;
            }
        }
    }
    ranked.sort_by_key(|e| e.id);

    let mut kept: Vec<Engagement> = Vec::new();
    let mut into: BTreeMap<i32, i32> = BTreeMap::new();

    for e in ranked {
        let older = kept.iter().position(|k| {
            matches!(k.objective, Objective::Defend)
                && matches!(e.objective, Objective::Defend)
                && k.group == e.group
        });

        match older {
            Some(at) => {
                kept[at].ours = kept[at].ours.union(&e.ours).cloned().collect();
                kept[at].lost += e.lost;
                kept[at].budget += e.budget;
                into.insert(e.id, kept[at].id);
            }
            None => kept.push(e),
        }
    }

    (kept, into)
}

fn changed(e: &Engagement, events: &[tracker::Event], by_id: &BTreeMap<i32, Group>) -> bool {
    events.iter().any(|event| match event {
        tracker::Event::Split { from, into } => *from == e.group || *into == e.group,
        tracker::Event::Merged { into, .. } => *into == e.group,
        tracker::Event::Left(id) => *id == e.group,
        tracker::Event::Newcomer(id) => by_id.get(id).is_some_and(|g| g.room() == e.room),
    })
}

pub fn decide(context: &mut Context, state: &mut FightState) -> ModuleOutput {
    let world = context.world;
    let declarations = context.declarations;
    let preyed: &[RoomName] = &declarations.rooms(OpKind::Sweep).collect::<Vec<_>>();
    let sightings = context.sightings;
    let intel = context.intel;
    let age = |room: RoomName| {
        intel
            .get(&room)
            .map(|row| (world.time - row.seen, row.level > 0))
    };
    let atlas: &mut Atlas = &mut *context.atlas;

    let mut foes: BTreeMap<RoomName, Vec<Creep>> = BTreeMap::new();
    let mut cores: BTreeSet<Id> = BTreeSet::new();

    for r in &world.rooms {
        let mut all: Vec<Creep> = r.hostiles().cloned().collect();
        let seen = all.len();
        let razed = r
            .structures
            .iter()
            .filter(|s| s.kind == StructureType::InvaderCore && s.level == 0);
        all.extend(razed.map(casting::core_as_hostile));
        cores.extend(all[seen..].iter().map(|c| c.id));
        foes.insert(r.name, all);
    }

    let hostiles: Vec<Creep> = world
        .rooms
        .iter()
        .flat_map(|r| foes[&r.name].clone())
        .collect();

    let owned = &context.derived.owned;
    let held = (state.engagements.iter()).fold(0, |most, e| most.max(e.group));

    let (groups, events) = tracker::step(
        tracker::LOST_GROUP_TICKS,
        held,
        owned,
        &state.groups,
        &hostiles,
        world.time,
    );

    let homes: BTreeSet<RoomName> = context.derived.colonies.iter().copied().collect();
    let worked: BTreeSet<RoomName> = (declarations.outposts.iter())
        .chain(&declarations.claims)
        .map(|o| o.room)
        .collect();

    let by_id: BTreeMap<i32, Group> = groups.iter().map(|g| (g.id, g.clone())).collect();
    let hostile_at: BTreeMap<Id, Pos> = hostiles.iter().map(|h| (h.id, h.pos)).collect();
    let ours: BTreeMap<&str, &Creep> = world.creeps.iter().map(|c| (c.name.as_str(), c)).collect();
    let standing: BTreeSet<String> = world.creeps.iter().map(|c| c.name.clone()).collect();
    let able: BTreeSet<String> = (world.creeps.iter())
        .filter(|c| !c.spent())
        .map(|c| c.name.clone())
        .collect();
    let aging_out: BTreeSet<String> = world
        .creeps
        .iter()
        .filter(|c| c.ticks_to_live <= 1)
        .map(|c| c.name.clone())
        .collect();

    let (mut engagements, into) = merged(&events, &state.engagements);

    // An outpost has no towers to answer a body that works it (§7.2).
    let outpost = |r: &RoomName| worked.contains(r) && !homes.contains(r);
    let works = |h: &Creep| outpost(&h.pos.room) && hostiles::works(h);
    let armed: BTreeSet<Id> = (hostiles.iter())
        .filter(|h| hostiles::foe(h))
        .filter(|h| cores.contains(&h.id) || hostiles::armed(owned, &hostiles, h) || works(h))
        .map(|h| h.id)
        .collect();

    // A Defend left open holds its room: a newcomer inside it is its target
    // now, with a fresh budget, where its own group is not there (a home's,
    // so a second raid is not left to towers) or was lost from sight.
    for e in &mut engagements {
        if !matches!(e.objective, Objective::Defend) || e.phase == Phase::Disengaging {
            continue;
        }

        let in_room =
            |id| (by_id.get(&id)).is_some_and(|g| g.room() == e.room && g.last_seen == world.time);

        let gone = if homes.contains(&e.room) {
            !in_room(e.group)
        } else {
            !by_id.contains_key(&e.group)
        };

        let newcomer = events.iter().find_map(|event| match event {
            tracker::Event::Newcomer(id)
                if gone && in_room(*id) && by_id[id].members.iter().any(|m| armed.contains(m)) =>
            {
                Some(*id)
            }
            _ => None,
        });

        if let Some(id) = newcomer {
            e.group = id;
            e.opened = world.time;
            e.budget = e.lost;
            e.unanswered = true;
            e.predicted = None;
        }
    }

    let alive: BTreeSet<i32> = engagements.iter().map(|e| e.id).collect();

    let (landed, spawning): (Vec<Cast>, Vec<Cast>) = state
        .casts
        .iter()
        .map(|c| match into.get(&c.engagement) {
            Some(&survivor) => Cast {
                engagement: survivor,
                ..c.clone()
            },
            None => c.clone(),
        })
        .filter(|c| {
            alive.contains(&c.engagement)
                && world
                    .rooms
                    .iter()
                    .any(|r| r.name == c.room && r.energy_capacity >= engine::body_cost(&c.body))
        })
        .partition(|c| standing.contains(&c.name));

    for c in &landed {
        if let Some(e) = engagements.iter_mut().find(|e| e.id == c.engagement) {
            e.ours.insert(c.name.clone());
        }
    }

    let walks = (engagements.iter())
        .map(|e| (e.room, Scene::hops(e)))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .map(|(room, hops)| {
            let tree = atlas.tree(&declarations.avoid, hops, room);
            ((room, hops), tree)
        })
        .collect();
    let muster = |r: RoomName| declarations.colonies.iter().find(|c| c.room == r)?.muster;
    let spawn = |r: RoomName| world.spawns.iter().find(|s| s.pos.room == r).map(|s| s.pos);
    let musters = (homes.iter())
        .filter_map(|r| muster(*r).or_else(|| spawn(*r)))
        .collect();
    let scene = Scene {
        world,
        sightings,
        remembered: sightings::remembered(sightings, world),
        avoid: &declarations.avoid,
        rules: &declarations.tactics,
        walks,
        homes,
        defended: declarations.rooms(OpKind::Defend).collect(),
        musters,
        worked,
        foes,
        cores,
        by_id,
        hostile_at,
        ours,
        armed,
        landed,
        spawning,
        boost: casting::cluster_boost(world, declarations),
    };

    // A planned barrier fallen advances the plan, never re-prices it; a
    // target fallen re-plans toward the next razed.
    let mut retargeted: BTreeSet<i32> = BTreeSet::new();

    for e in &mut engagements {
        if let Objective::Siege(target) = &e.objective
            && let Some(room) = scene.in_sight(e.room)
        {
            let barriers = siege::barrier_at(room);
            e.plan.order.retain(|id| barriers.contains_key(id));

            let members = scene.members_of(e);
            let next = if raid::raids(e) {
                raid::target(room, &members)
            } else {
                siege::next_target(room, members.first().map(|c| &c.pos))
            };

            if (!siege::stands(room, *target) || siege::tapped(room, *target).is_some())
                && let Some(next) = next
                && next != *target
            {
                e.objective = Objective::Siege(next);
                retargeted.insert(e.id);
            }
        }
    }

    let stirred: BTreeSet<RoomName> = groups
        .iter()
        .filter(|g| g.last_seen == world.time)
        .filter(|g| {
            let was = state.groups.iter().find(|p| p.id == g.id);
            was.is_none_or(|p| p.last_seen < world.time - 1 || p.members != g.members)
        })
        .map(Group::room)
        .collect();
    let sweeps = || {
        engagements
            .iter()
            .filter(|e| e.objective == Objective::Sweep)
    };
    let grounds: BTreeMap<RoomName, Hunting> = (preyed.iter())
        .filter(|room| stirred.contains(*room))
        .chain(sweeps().map(|e| &e.room))
        .filter_map(|room| {
            Some((
                *room,
                scene.ground(atlas, scene.in_sight(*room)?, &state.groups),
            ))
        })
        .collect();
    let cornered: BTreeSet<i32> = sweeps()
        .filter(|e| {
            let members = scene.members_of(e);
            let mates = |c: &Creep| {
                (members.iter().copied())
                    .filter(|m| m.name != c.name)
                    .collect::<Vec<_>>()
            };
            (grounds.get(&e.room)).is_some_and(|g| {
                (members.iter().filter(|c| c.pos.room == e.room))
                    .any(|c| tactics::cornered(atlas, &g.threat, &mates(c), c))
            })
        })
        .map(|e| e.id)
        .collect();
    let candidates = scene.candidates(atlas, &grounds, &stirred);

    // The edge each siege would drain at: his border tile his towers reach,
    // nearest them, and the room across it our medics wait in. The gate is the
    // pair's: a tile his volley leaves the pairs the bank buys healing over.
    // A room with no such tile is not drained: the set breaches by its plan,
    // or stands off a safe mode as it did before the drain (§6.2).
    let gates: BTreeMap<i32, tactics::Gate> = (engagements.iter())
        .filter(|e| matches!(e.objective, Objective::Siege(_)) && !raid::raids(e))
        .filter_map(|e| {
            let room = scene.in_sight(e.room)?;
            let towers = raid::charged(room);
            let drains = !towers.is_empty()
                && matches!(e.phase, Phase::Approaching | Phase::Draining)
                && (Scene::safe(room) || scene.outstruck(e, &towers));
            if !drains {
                return None;
            }
            let gate = siege::gate(&*atlas, e.room, scene.enter_room(&*atlas, e)?, &towers)?;
            siege::drain_pays(e.room, e.capacity, &towers, &gate).then_some((e.id, gate))
        })
        .collect();
    let given_up: BTreeSet<i32> = (engagements.iter())
        .filter(|e| matches!(e.objective, Objective::Siege(_)) && siege::gave_up(e))
        .map(|e| e.id)
        .collect();

    let stepped: Vec<(Engagement, bool)> = engagements
        .iter()
        .filter_map(|e| {
            let next = match &e.objective {
                Objective::Defend => {
                    let home = scene.homes.contains(&e.room);
                    let counted = if home { &standing } else { &able };
                    defend::step(&scene.facts_of(e, counted, &aging_out), e)
                }
                Objective::Siege(_) => {
                    let declared = declarations.priority(OpKind::Siege, e.room).is_some();
                    let f = scene.siege_facts(e, declared, &standing, &aging_out, gates.get(&e.id));
                    siege::step(&f, e)
                }
                Objective::Sweep => sweep::step(
                    &sweep::Facts {
                        time: world.time,
                        standing: standing.clone(),
                        aging_out: aging_out.clone(),
                        asked: scene.spawning.iter().any(|c| c.engagement == e.id),
                        arrived: scene.members_of(e).iter().all(|c| c.pos.room == e.room),
                        worth: grounds
                            .get(&e.room)
                            .map(|g| g.worth > 0 || !g.escort.is_empty()),
                        safe: scene.in_sight(e.room).is_some_and(Scene::safe),
                        elsewhere: scene.elsewhere(atlas, e, &candidates, &grounds),
                        patrol: (grounds.get(&e.room))
                            .filter(|g| g.worth == 0 && g.escort.is_empty())
                            .and_then(|_| scene.patrol(atlas, e, preyed, age)),
                    },
                    e,
                ),
            }?;

            Some((
                next.clone(),
                next.ours.len() < e.ours.len() || retargeted.contains(&e.id),
            ))
        })
        .collect();

    let targeted: BTreeSet<RoomName> = stepped
        .iter()
        .flat_map(|(e, _)| [Some(e.room), scene.by_id.get(&e.group).map(Group::room)])
        .flatten()
        .collect();

    // Past every id held last tick, so none closed this tick is reused by one opened.
    let first_id = 1 + state.engagements.iter().fold(0, |most, e| most.max(e.id));

    let opened: Vec<(Engagement, bool)> = defend::opens(
        |room| declarations.priority(OpKind::Defend, room),
        |id| scene.armed.contains(id),
        world.time,
        &targeted,
        &groups,
    )
    .into_iter()
    .enumerate()
    .map(|(i, (g, priority))| {
        let id = first_id + i as i32;
        let mut e = fresh(id, Objective::Defend, g.room(), world.time, Tactic::Close);
        e.group = g.id;
        e.priority = priority;
        (e, true)
    })
    .collect();

    let besieging: BTreeSet<RoomName> = stepped
        .iter()
        .filter(|(e, _)| matches!(e.objective, Objective::Siege(_)))
        .map(|(e, _)| e.room)
        .collect();

    let declared: Vec<(Engagement, bool)> = declarations
        .operations
        .iter()
        .filter(|o| o.kind == OpKind::Siege && !besieging.contains(&o.room))
        // A safe mode refuses the breach but not the edge: his towers are
        // drawn there while it runs, and the siege opens on a drain only where
        // the pairs it casts out-heal his volley on the gate.
        .filter_map(|op| {
            let room = scene.in_sight(op.room)?;
            let target = siege::next_target(room, None)?;
            let towers = raid::charged(room);
            if !Scene::safe(room) {
                return Some((op, target));
            }
            if towers.is_empty() {
                return None;
            }
            let mut watch = fresh(
                first_id,
                Objective::Siege(target),
                room.name,
                world.time,
                Tactic::BlobClose,
            );
            watch.capacity = scene.capacity_of(&*atlas, &watch);
            // A safe mode's room opens on the drain that prices: the least his
            // towers strike a border tile is what a gate can draw at.
            siege::drainable(room.name, watch.capacity, &towers).then_some((op, target))
        })
        .enumerate()
        .map(|(i, (op, target))| {
            let id = first_id + (opened.len() + i) as i32;
            let tactic = Tactic::BlobClose;
            let e = fresh(id, Objective::Siege(target), op.room, world.time, tactic);
            let priority = op.priority;
            (Engagement { priority, ..e }, false)
        })
        .collect();

    let sweeping = stepped.iter().any(|(e, _)| e.objective == Objective::Sweep);
    let won = || candidates.iter().filter(|c| c.3);
    let swept = atlas::onward(won().map(|(r, w, k, _)| (*r, *w, *k)))
        .or_else(|| atlas::onward(candidates.iter().map(|(r, w, k, _)| (*r, *w, *k))))
        .filter(|_| !sweeping)
        .map(|room| {
            let id = first_id + (opened.len() + declared.len()) as i32;
            let e = fresh(id, Objective::Sweep, room, world.time, Tactic::Kite);
            let priority = declarations
                .priority(OpKind::Sweep, room)
                .unwrap_or_default();
            (Engagement { priority, ..e }, false)
        });

    let committed: BTreeSet<&String> = stepped.iter().flat_map(|(e, _)| &e.ours).collect();

    let mut free: Vec<String> = world
        .creeps
        .iter()
        .filter(|c| {
            declarations::fights(&c.name)
                && able.contains(&c.name)
                && !(committed.contains(&c.name) || aging_out.contains(&c.name))
        })
        .map(|c| c.name.clone())
        .collect();

    let mut asked = Asked {
        casts: scene.spawning.clone(),
        ..Asked::default()
    };

    for c in &scene.spawning {
        asked.queue(c.room, &c.body);
    }

    let mut ranked: Vec<(Engagement, bool)> = stepped;
    ranked.extend(opened);
    ranked.extend(declared);
    ranked.extend(swept);
    ranked.sort_by(|(a, _), (b, _)| {
        b.priority
            .partial_cmp(&a.priority)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then(a.id.cmp(&b.id))
    });

    let mut decided: Vec<Engagement> = Vec::new();

    for (e, left) in ranked {
        let grew = e.unanswered && scene.capacity_of(atlas, &e) > e.capacity;

        match &e.objective {
            Objective::Siege(target) => {
                if let Some(next) = scene.besieged(atlas, &mut asked, &e, *target, left, grew) {
                    decided.push(next);
                }
            }
            Objective::Sweep
                if e.since == world.time && matches!(e.phase, Phase::Forming | Phase::Fighting)
                    || grew
                    || left
                    || scene.landed.iter().any(|c| c.engagement == e.id)
                    || cornered.contains(&e.id)
                    || changed(&e, &events, &scene.by_id) =>
            {
                let onward = || {
                    (scene.elsewhere(atlas, &e, &candidates, &grounds))
                        .or_else(|| scene.patrol(atlas, &e, preyed, age))
                };
                decided.extend(scene.swept(atlas, &mut asked, (&e, onward), (&mut free, &grounds)));
            }
            Objective::Defend
                if left
                    || grew
                    || changed(&e, &events, &scene.by_id)
                    || e.since == world.time && e.phase == Phase::Forming =>
            {
                decided.push(scene.priced(atlas, &mut asked, &e, &mut free, declarations));
            }
            Objective::Defend | Objective::Sweep => decided.push(e),
        }
    }

    let kept: BTreeSet<i32> = decided
        .iter()
        .filter(|e| !(e.phase == Phase::Disengaging || scene.spent(e)))
        .map(|e| e.id)
        .collect();

    let mut casts = asked.casts;
    casts.retain(|c| kept.contains(&c.engagement));

    let ovens: BTreeSet<&String> = (world.spawns.iter())
        .filter_map(|s| s.spawning.as_ref().map(|o| &o.name))
        .collect();

    let asks = (casts.iter())
        .filter(|c| !ovens.contains(&c.name))
        .map(|c| {
            (
                c.room,
                Ask {
                    body: c.body.clone(),
                    name: c.name.clone(),
                    rank: Rank::Fight,
                },
            )
        })
        .collect();

    let mut out = super::act::act(
        &scene,
        atlas,
        context.trace,
        &decided,
        &gates,
        &given_up,
        &grounds,
    );
    out.asks = asks;

    state.groups = groups;
    state.engagements = decided;
    state.engagements.sort_by_key(|e| e.id);
    state.casts = casts;
    state.due = asked.due;
    out
}
