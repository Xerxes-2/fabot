/// The fight's tick (`ARCHITECTURE.md` §6): who is out there, what we committed,
/// what each committed body does, and what it asks for.
use std::collections::{BTreeMap, BTreeSet};

use super::kinds::{defend, stronghold, sweep};
use super::scene::{Hunting, Scene, fresh};
use super::shape::{Cast, Engagement, FightState, Group, Objective, Phase};
use super::{casting, open, tracker};
use fabot_atlas::{Atlas, Context};
use fabot_base::declarations::OpKind;
use fabot_base::engine;
use fabot_base::intents::{Ask, ModuleOutput, Rank};
use fabot_base::world::{self, Creep, Id, Pos, RoomName, StructureType, sightings};
use fabot_tactics::{self as tactics, Tactic};

/// A merge keeps the older group's id; two engagements on one group are one,
/// the older surviving with both's bodies, casts, losses and budget.
fn merged(
    events: &[tracker::Event],
    engagements: &[Engagement],
    casts: &[Cast],
) -> (Vec<Engagement>, Vec<Cast>) {
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
    let mut casts = casts.to_vec();

    for e in ranked {
        let older = kept.iter().position(|k| {
            matches!(k.objective, Objective::Defend)
                && matches!(e.objective, Objective::Defend)
                && k.group == e.group
        });

        match older {
            Some(at) => {
                let mut free = kept[at].free(&casts);
                let k = &mut kept[at];
                for (c, s) in casts
                    .iter_mut()
                    .filter(|c| c.engagement == e.id)
                    .zip(&mut free)
                {
                    (c.engagement, c.slot) = (k.id, s);
                }
                k.ours.extend(e.ours.into_keys().zip(free));
                k.lost += e.lost;
                k.budget += e.budget;
            }
            None => kept.push(e),
        }
    }

    (kept, casts)
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
    let atlas: &mut Atlas = &mut *context.atlas;

    let mut foes: BTreeMap<RoomName, Vec<Creep>> = BTreeMap::new();
    let mut cores: BTreeSet<Id> = BTreeSet::new();
    let mut hostiles: Vec<Creep> = Vec::new();

    for r in &world.rooms {
        let mut all: Vec<Creep> = r.hostiles().cloned().collect();
        let seen = all.len();
        let razed = r
            .structures
            .iter()
            .filter(|s| s.kind == StructureType::InvaderCore && s.level == 0);
        all.extend(razed.map(casting::core_as_hostile));
        cores.extend(all[seen..].iter().map(|c| c.id));
        hostiles.extend(all.iter().cloned());
        foes.insert(r.name, all);
    }

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
        .filter(|c| !world::spent(&c.body))
        .map(|c| c.name.clone())
        .collect();
    let aging_out: BTreeSet<String> = world
        .creeps
        .iter()
        .filter(|c| c.ticks_to_live <= engine::AGED_OUT_TICKS_TO_LIVE)
        .map(|c| c.name.clone())
        .collect();

    let (mut engagements, casts) = merged(&events, &state.engagements, &state.casts);

    let (armed, opens) = open::armed_and_opening(owned, (&worked, &homes), &hostiles, &cores);
    open::newcomers(
        &mut engagements,
        &events,
        &by_id,
        &homes,
        &opens,
        world.time,
    );

    let alive: BTreeSet<i32> = engagements.iter().map(|e| e.id).collect();

    let (landed, spawning): (Vec<Cast>, Vec<Cast>) = (casts.into_iter())
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
            e.ours.insert(c.name.clone(), c.slot);
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
    let musters = homes.iter().filter_map(|r| muster(*r)).collect();
    let scene = Scene {
        world,
        sightings,
        intel,
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
        boosts: casting::cluster_boosts(&context.derived.boosts, &spawning, &engagements),
        boosting: &context.derived.boosts,
        landed,
        spawning,
    };

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
    let mut claimed: BTreeSet<RoomName> = sweeps().map(|e| e.room).collect();
    let candidates = scene.candidates(atlas, &grounds, &stirred);

    let stepped: Vec<(Engagement, bool)> = engagements
        .iter()
        .filter_map(|e| {
            let next = match &e.objective {
                Objective::Defend => {
                    let home = scene.homes.contains(&e.room);
                    let counted = if home { &standing } else { &able };
                    defend::step(&scene.facts_of(e, counted, &aging_out, &state.groups), e)
                }
                Objective::Sweep => {
                    let members = scene.members_of(e);
                    let worth = sweep::worth_held(&scene, e, &grounds);
                    let at = (preyed, &declarations.enemies, &claimed);
                    let next = sweep::step(
                        &sweep::Facts {
                            time: world.time,
                            standing: able.clone(),
                            aging_out: aging_out.clone(),
                            asked: scene.spawning.iter().any(|c| c.engagement == e.id),
                            arrived: members.iter().all(|c| c.pos.room == e.room),
                            worth,
                            elsewhere: scene.elsewhere(atlas, e, (&candidates, &claimed), &grounds),
                            patrol: (worth == Some(false))
                                .then(|| sweep::patrol(&scene, atlas, e, at))
                                .flatten(),
                        },
                        e,
                    );
                    claimed.extend(next.as_ref().map(|n| n.room));
                    next
                }
                Objective::Stronghold => {
                    stronghold::step(&scene, declarations, &standing, &aging_out, e)
                }
            }?;

            Some((next.clone(), next.ours.len() < e.ours.len()))
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
        |id| opens.contains(id),
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

    let first = first_id + opened.len() as i32;
    let at = (&engagements[..], first, declarations);
    let swept = sweep::opened(&scene, (&candidates, &mut claimed), at);

    let committed: BTreeSet<&String> = stepped.iter().flat_map(|(e, _)| e.ours.keys()).collect();

    let mut free: Vec<String> = world
        .creeps
        .iter()
        .filter(|c| defend::free(c) && !committed.contains(&c.name))
        .map(|c| c.name.clone())
        .collect();

    let mut asked = scene.asked();

    let mut ranked: Vec<(Engagement, bool)> = stepped;
    ranked.extend(opened);
    ranked.extend(swept);
    let raided = stronghold::opens(&scene, declarations, &ranked, first_id);
    ranked.extend(raided);
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
            Objective::Defend | Objective::Sweep if defend::overdue(&scene, &e) => {
                let stands = sweep::holds(&scene, &e, (e.room, &grounds));
                decided.extend(defend::late(&mut asked, (world.time, stands), &e))
            }
            Objective::Sweep
                if e.since == world.time && matches!(e.phase, Phase::Forming | Phase::Fighting)
                    || grew
                    || left
                    || scene.landed.iter().any(|c| c.engagement == e.id)
                    || cornered.contains(&e.id)
                    || changed(&e, &events, &scene.by_id) =>
            {
                let at = (preyed, &declarations.enemies, &claimed);
                let onward = || {
                    (scene.elsewhere(atlas, &e, (&candidates, &claimed), &grounds))
                        .or_else(|| sweep::patrol(&scene, atlas, &e, at))
                };
                let next = scene.swept(atlas, &mut asked, (&e, onward), (&mut free, &grounds));
                claimed.extend(next.as_ref().map(|n| n.room));
                decided.extend(next);
            }
            Objective::Defend
                if left
                    || grew
                    || changed(&e, &events, &scene.by_id)
                    || e.since == world.time && e.phase == Phase::Forming
                    || defend::unvisited(&scene, &e)
                    || e.unanswered && scene.homes.contains(&e.room) =>
            {
                decided.push(scene.priced(atlas, &mut asked, &e, &mut free, declarations));
            }
            Objective::Stronghold => {
                decided.push(stronghold::priced(&scene, atlas, &mut asked, &e))
            }
            Objective::Defend | Objective::Sweep => decided.push(e),
        }
    }

    scene.boost_hold(&engagements, &mut decided);
    let facts = (&scene, &state.groups[..], &world.fallen());
    super::price::account(facts, &state.engagements, &mut decided);
    let closed = super::price::closed(facts, &engagements, &decided);
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

    let rank = |c: &Cast| match decided.iter().find(|e| e.id == c.engagement) {
        Some(e) if e.objective != Objective::Defend => Rank::Fight,
        Some(e) if scene.homes.contains(&e.room) => Rank::Defend,
        _ => Rank::DefendOutpost,
    };
    let asks = (casts.iter())
        .filter(|c| !ovens.contains(&c.name))
        .map(|c| {
            (
                c.room,
                Ask {
                    body: c.body.clone(),
                    name: c.name.clone(),
                    rank: rank(c),
                },
            )
        })
        .collect();

    let mut out = super::act::act(&scene, atlas, context.trace, (&decided, &casts, &grounds));
    out.asks = asks;

    state.groups = groups;
    state.engagements = decided;
    state.engagements.sort_by_key(|e| e.id);
    state.casts = casts;
    state.closed = closed;
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use fabot_base::vocabulary::Part;

    #[test]
    fn a_merged_defend_holds_each_body_and_cast_in_its_own_slot() {
        let defend = |id: i32, ours: &[(&str, u16)]| Engagement {
            group: id,
            ours: ours.iter().map(|(n, s)| (n.to_string(), *s)).collect(),
            ..fresh(id, Objective::Defend, RoomName::W1N1, 1, Tactic::Close)
        };
        let cast = |engagement: i32, name: &str, slot| Cast {
            engagement,
            name: name.into(),
            slot,
            room: RoomName::W1N1,
            body: vec![Part::Attack, Part::Move],
        };
        let engagements = [defend(1, &[("a", 0)]), defend(2, &[("b", 0), ("c", 1)])];
        let casts = [cast(1, "d", 1), cast(2, "e", 2), cast(2, "f", 0)];
        let events = [tracker::Event::Merged { into: 1, from: 2 }];

        let (kept, casts) = merged(&events, &engagements, &casts);

        assert_eq!(kept.len(), 1);
        let slots: Vec<u16> = (kept[0].ours.values().copied())
            .chain(casts.iter().filter(|c| c.engagement == 1).map(|c| c.slot))
            .collect();
        assert_eq!(slots.len(), 6);
        assert_eq!(slots.iter().collect::<BTreeSet<_>>().len(), 6);
    }
}
