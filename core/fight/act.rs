/// What each committed body does this tick (§7): its walk, its strike and its heal.
use std::collections::{BTreeMap, BTreeSet};

use super::kinds::defend::{self, Home};
use super::kinds::stronghold;
use super::scene::{Hunting, Scene};
use super::shape::{Engagement, Objective, Phase};
use super::tracker::REACH;
use fabot_atlas::Atlas;
use fabot_base::declarations;
use fabot_base::intents::{self, Action, ControllerAct, CreepAct, ModuleOutput};
use fabot_base::tiles::Tile;
use fabot_base::wish::{Idle, Wish};
use fabot_base::world::{self, Creep, Id, Owner, Pos, RoomName, Structure, StructureType};
use fabot_base::{engine, tiles};
use fabot_tactics::{self as tactics, Tactic};

pub(super) fn act(
    scene: &Scene,
    atlas: &mut Atlas,
    trace: bool,
    fights: defend::Fights,
) -> ModuleOutput {
    let (world, (decided, _, grounds)) = (scene.world, fights);
    let rooms: BTreeSet<RoomName> = decided.iter().map(|e| e.room).collect();
    let middle = |r: RoomName| {
        grounds
            .get(&r)
            .map_or_else(|| atlas.reached_middle(r), |g| g.middle)
    };
    let middles: BTreeMap<RoomName, Pos> = rooms.into_iter().map(|r| (r, middle(r))).collect();
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
                    .filter(|s| s.kind == StructureType::Rampart && s.owner.is_ours());
                (r.name, ours.map(|s| s.pos).collect())
            })
            .collect(),
        musters: scene.musters.clone(),
        avoid: scene.avoid,
        rules: &world.rules,
    };

    let engaged_in: BTreeMap<&str, &Engagement> = decided
        .iter()
        .flat_map(|e| e.ours.keys().map(move |n| (n.as_str(), e)))
        .collect();

    let fighters: Vec<&Creep> = world
        .creeps
        .iter()
        .filter(|c| declarations::fights(&c.name))
        .collect();

    // A group lost from sight is sought in its room, which brings it back in sight.
    let centre_of = |e: &Engagement| -> (Pos, i32) {
        match (&e.objective, scene.by_id.get(&e.group)) {
            (Objective::Defend, Some(g)) => (g.centre, engine::RANGED_RANGE),
            (Objective::Defend, None) => (
                scene.raid_at(e, e.room).unwrap_or(middles[&e.room]),
                engine::RANGED_RANGE,
            ),
            _ => (middles[&e.room], engine::RANGED_RANGE),
        }
    };

    let fortified = fortified(world);
    let mut walking: BTreeMap<String, Wish> = BTreeMap::new();
    for e in decided {
        let on = (e.plan.tactic, &e.plan.order[..]);
        let defends = e.objective == Objective::Defend;
        let walk = (e.phase, e.unanswered, defends);
        let members = scene.members_of(e);
        let blob = formation(&mut sight, on, walk, members, centre_of(e), &fortified);
        let held = stronghold::walks(&mut sight, scene, e);
        walking.extend(blob.into_iter().chain(held).map(|w| (w.creep.clone(), w)));
    }

    let hunts: BTreeMap<i32, (&Hunting, bool)> = decided
        .iter()
        .filter(|e| e.objective == Objective::Sweep)
        .filter_map(|e| {
            let ground = grounds.get(&e.room)?;
            // The whole set scores its tiles at once, or none does (ibex).
            let contact = scene.members_of(e).iter().any(|m| {
                m.pos.room == e.room
                    && (ground.threat[tiles::of_pos(&m.pos)] > 0
                        || (ground.marks.iter()).any(|t| m.pos.within(REACH, &t.pos)))
            });
            Some((e.id, (ground, contact)))
        })
        .collect();

    let mut traced = Vec::new();
    let mut lookups = 0;
    let mut step = |sight: &mut tactics::Sight, c: &Creep| -> (Option<CreepAct>, Wish) {
        let Some(&e) = engaged_in.get(c.name.as_str()) else {
            return (None, defend::idle(sight, scene, fights, c));
        };

        if let Some(acted) = stronghold::act(sight, scene, (e, &middles, &walking), c) {
            return acted;
        }

        let (centre, _) = centre_of(e);
        let enter = e.objective == Objective::Defend;
        let gathers = enter && matches!(e.plan.tactic, Tactic::BlobClose | Tactic::BlobKite);
        let mut spawns = (scene.world.working_spawns()).filter(|s| s.pos.room == c.pos.room);
        let door = spawns.any(|s| engine::spawn_door(tiles::of_pos(&s.pos), tiles::of_pos(&c.pos)));

        if e.unanswered {
            return (None, tactics::rampart(sight, &fighters, c));
        }

        if let Some(wish) = walking.get(&c.name) {
            return (None, wish.clone());
        }

        let wish = match e.phase {
            // Held on a spawn's door a forming body bars it to the casts still to come.
            Phase::Forming if !scene.in_reach(e) && door => tactics::retreat(sight, c),
            Phase::Forming if !gathers || !scene.in_reach(e) => tactics::hold(sight, c),
            Phase::Forming | Phase::Disengaging => tactics::retreat(sight, c),
            Phase::Pursuing => tactics::close(sight, &e.plan.order, &centre, enter, c),
            Phase::Approaching | Phase::Fighting => {
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
                        traced.push((c.name.clone(), scored.iter().map(at).collect()));
                    }
                    return (strike, wish);
                }

                let members = scene.members_of(e);
                tactics::engage(
                    sight,
                    (e.plan.tactic, &e.plan.order),
                    (&centre, enter),
                    (&members, &fighters),
                    c,
                )
            }
        };
        let why = (e.phase == Phase::Forming).then_some(Idle::Forming);
        (None, wish.idling(why))
    };

    let mut out = ModuleOutput::default();

    if let Some(id) = defend::safe_mode(defend::running(world), &homes_of(scene, decided)) {
        out.actions
            .push(Action::Controller(id, ControllerAct::ActivateSafeMode));
    }

    let incoming: BTreeMap<String, i32> = stronghold::incoming(scene, decided).collect();

    let mut chosen: BTreeMap<String, Vec<CreepAct>> = BTreeMap::new();

    for c in &fighters {
        let (own, wish) = step(&mut sight, c);
        let engaged = engaged_in.get(c.name.as_str()).copied();
        let order = engaged.map_or(&[][..], |e| e.plan.order.as_slice());
        let mut wanted: Vec<CreepAct> = own.into_iter().collect();
        // A hunt's scored shot stands for the reflex.
        if wanted.iter().all(|a| a.slots() & intents::RANGED == 0) {
            wanted.extend(tactics::strike(&sight, order, c));
        }
        chosen.insert(c.name.clone(), wanted);
        out.wishes.push((c.pos.room, wish));
    }

    out.telemetry.traces = traced;
    out.telemetry.struck = sight.struck.clone();
    out.telemetry.counts.push(("threatLookups", lookups));
    let patients = (&fighters[..], &world.creeps[..]);
    out.actions.extend(reflexes(chosen, patients, &incoming));
    out.actions
        .extend(defend::recycles(scene, sight.atlas, fights));
    out
}

/// The rooms a blob walks into under towers.
pub fn fortified(world: &world::World) -> BTreeSet<RoomName> {
    (world.rooms.iter())
        .filter(|r| !world.rules.charged_towers(r).is_empty())
        .map(|r| r.name)
        .collect()
}

/// A blob's wishes while it walks as one: a Defend's holds it while it fights in its mark's room,
/// as its together arm was priced, into its reach (else a cripple left's), else onto `centre`.
pub fn formation(
    sight: &mut tactics::Sight,
    (tactic, order): (Tactic, &[Id]),
    (phase, unanswered, defends): (Phase, bool, bool),
    members: Vec<&Creep>,
    (centre, range): (Pos, i32),
    fortified: &BTreeSet<RoomName>,
) -> Vec<Wish> {
    let foes = sight.hostiles;
    let marked = (order.iter()).find_map(|id| foes.values().flatten().find(|h| h.id == *id));
    let mark = defends
        .then(|| marked.or_else(|| foes.get(&centre.room)?.first()))
        .flatten();
    let walks = matches!(tactic, Tactic::BlobClose | Tactic::BlobKite)
        && !unanswered
        && (phase == Phase::Approaching || defends && phase == Phase::Fighting);
    let reach = match tactic {
        Tactic::BlobKite => engine::RANGED_RANGE,
        _ => engine::MELEE_RANGE,
    };
    let (centre, range) = mark.map_or((centre, range), |h| (h.pos, reach));
    let present = |c: &&Creep| phase != Phase::Fighting || c.pos.room == centre.room;
    let members: Vec<&Creep> = (members.into_iter())
        .filter(|c| (defends || !c.swings()) && present(c))
        .collect();
    if !walks || members.is_empty() {
        return Vec::new();
    }
    let raid = mark.map_or(&[][..], |h| foes[&h.pos.room].as_slice());
    tactics::blob(sight, (&centre, range), &members, (fortified, raid))
}

/// Each body's acts and the heals its swing and shot leave it, as the engine performs them.
pub fn reflexes(
    mut chosen: BTreeMap<String, Vec<CreepAct>>,
    (healers, patients): (&[&Creep], &[Creep]),
    incoming: &BTreeMap<String, i32>,
) -> impl Iterator<Item = Action> {
    let busy = |slot: u8| -> BTreeSet<String> {
        let taken = chosen
            .iter()
            .filter(|(_, a)| a.iter().any(|a| a.slots() & slot != 0));
        taken.map(|(n, _)| n.clone()).collect()
    };
    let (swinging, shooting) = (busy(intents::MELEE), busy(intents::RANGED));
    for (healer, act) in tactics::heals(healers, patients, incoming, &swinging, &shooting) {
        chosen.entry(healer).or_default().push(act);
    }
    chosen.into_iter().flat_map(|(name, wanted)| {
        let done = intents::performed(&wanted);
        done.into_iter()
            .map(move |a| Action::Creep(name.clone(), a))
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
                towered: (scene.world.rules.towers(room)).any(|s| s.owner.is_ours()),
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
                    .filter_map(|r| Some((r.pos, r.fallen.clone()?)))
                    .collect(),
            });
        }
    }

    homes_lost
}
