pub mod expand;
pub mod intel;
pub mod outposts;

use super::intents::Intent;
use super::state::State;
use std::collections::{BTreeMap, BTreeSet};

use super::engine;
use super::fight::shape::{Cast, Engagement};
use super::layout::Standing;
use super::tick::{ColonyPlan, Context, Declarations, Derived, Outpost, Stage};
use super::tiles;
use super::vocabulary::{BodyPart, Boost, boost};
use super::world::{RoomName, Rules, World, sightings, terrain::Terrain};
use expand::Expansion;
use intel::Intel;

pub fn decide(context: &mut Context, state: &mut State) -> Result<Vec<Intent>, String> {
    let mut preyed = intel::preyed(context.intel, context.declarations, context.world.time);
    preyed.retain(|room| context.atlas.has_terrain(*room));
    // A scout only dies where armed bodies stand, and draws them off; a
    // siege's sight is the fight's.
    let unarmed = |room| sightings::unarmed(context.sightings, context.world.time, room);
    let mut radius = intel::threat_radius(context.world, context.atlas, &context.derived.owned);
    radius.retain(|room, _| unarmed(*room));
    let (observed, mut scouts) = intel::sight(
        context.world,
        context.atlas,
        context.declarations,
        context.intel,
        (&preyed, &radius),
    );
    scouts.retain(|s| context.declarations.sieges.contains(&s.room) || unarmed(s.room));
    state.scouts = scouts;
    state.preyed = preyed;
    Ok(observed)
}

/// A rival's room this near raids within a creep life.
const NEAR: i32 = 3;

/// The longest a rampart holds, as v1's floor priced it: the ticks a colony
/// takes to cast its answer twice, the first lost.
const HOLD: i32 = 2 * engine::MAX_BODY_PARTS as i32 * engine::SPAWN_TICKS_PER_PART;

/// A quad, as TooAngel and most bots send: one dismantler, three healers.
const HEALERS: i32 = 3;

/// The threat in, the hits a rampart is kept at out, and whether the ring
/// pays (§5): only where the quad out-heals the towers.
pub fn wall(rules: &Rules, rival: i32, level: i32, threatened: bool) -> (i32, bool) {
    let capacity =
        engine::SPAWN_ENERGY + rules.allowed("extension", rival) * engine::extension_energy(rival);
    let pairs = |part: BodyPart| {
        (capacity / engine::body_cost(&[part, BodyPart::Move]))
            .min(engine::MAX_BODY_PARTS as i32 / 2)
    };
    let boost = |part: BodyPart| {
        if rules.allowed("lab", rival) > 0 {
            boost::multiple(part, Boost::Tier3)
        } else {
            1
        }
    };
    let work = pairs(BodyPart::Work);
    let heal = HEALERS * pairs(BodyPart::Heal) * engine::HEAL_POWER * boost(BodyPart::Heal);
    let fire = rules.allowed("tower", level) * engine::tower_attack_at(engine::TOWER_FALLOFF_RANGE);
    let hits = 2 * work * engine::PART_HITS;
    let held = if fire > heal {
        HOLD.min(hits / (fire - heal) + 1)
    } else {
        HOLD
    };
    let full = work * engine::DISMANTLE_POWER * boost(BodyPart::Work) * held;
    let floor = 20 * engine::RAMPART_DECAY_AMOUNT;
    let kept = floor.max(if threatened { full } else { full / 3 });
    (kept, fire <= heal)
}

/// The threat in, a reserve out, as a priced choice would be (§5).
pub fn reserve(rules: &Rules, lifetimes: f64, level: i32, threatened: bool) -> i32 {
    let fire = rules.allowed("tower", level) * engine::TOWER_ENERGY_COST * engine::CREEP_LIFETIME;
    let full = (f64::from(fire) * lifetimes) as i32;
    if threatened { full } else { full / 3 }
}

pub fn plan(
    world: &World,
    derived: &Derived,
    declarations: &Declarations,
    intel: &Intel,
    (engagements, casts, due): (&[Engagement], &[Cast], &BTreeSet<RoomName>),
    layout: &BTreeMap<RoomName, Standing>,
    (terrain, expansion): (&dyn Fn(RoomName) -> Option<Terrain>, &mut Option<Expansion>),
) -> (Vec<ColonyPlan>, Vec<Outpost>, Option<Outpost>) {
    let rival = |name: &str| name != world.me && !declarations.allies.contains(name);
    let rivals: Vec<(RoomName, i32)> = (intel.iter())
        .filter(|(_, row)| row.level > 0 && row.owner.as_deref().is_some_and(rival))
        .map(|(room, row)| (*room, row.level))
        .collect();
    let fought = |room: RoomName| engagements.iter().any(|e| e.room == room);
    let plans: Vec<ColonyPlan> = (derived.owned.iter().filter_map(|r| world.room(*r)))
        .filter_map(|room| {
            let level = room.controller.as_ref()?.level;
            let threat = (rivals.iter())
                .filter(|(r, _)| tiles::rooms_apart(*r, room.name) <= NEAR)
                .map(|(_, l)| (*l).min(level))
                .max();
            let near = threat.is_some();
            // A fight's committed casts are paid before any surplus.
            let asked: i32 = (casts.iter())
                .filter(|c| c.room == room.name)
                .map(|c| engine::body_cost(&c.body))
                .sum();
            let threatened = near || fought(room.name) || asked > 0;
            let kept = reserve(
                &world.rules,
                declarations.reserve_lifetimes,
                level,
                threatened,
            );
            let (rampart_hits, outheals) =
                wall(&world.rules, threat.unwrap_or(level), level, threatened);
            let stored = (room.structures.iter()).any(|s| s.kind.is_storage() && s.owner.is_ours());
            let pile = (level >= 2 && !stored)
                .then(|| layout.get(&room.name)?.plan.as_ref()?.pile())
                .flatten();
            Some(ColonyPlan {
                room: room.name,
                stage: Stage::Exploit,
                target: level,
                reserve: kept + asked,
                rampart_hits,
                ring: near && outheals,
                owes: asked > 0 || due.contains(&room.name),
                pile,
            })
        })
        .collect();
    // An expansion declared this tick holds the outposts from the next.
    let claimed = expand::claim(intel, derived, expansion.as_ref()).map(|c| c.room);
    let preyed = intel::preyed(intel, declarations, world.time);
    let busy = outposts::busy(
        &plans,
        (declarations, claimed),
        (engagements, &preyed, derived),
    );
    let candidates = outposts::survey(world, derived, (declarations, &plans, claimed), intel);
    let chosen = outposts::chosen(world, candidates, &busy);
    let held = expansion.take();
    *expansion = expand::step(
        world,
        derived,
        (declarations, &plans),
        (&rivals, &chosen, &busy),
        intel,
        terrain,
        held,
    );
    let claim = expand::claim(intel, derived, expansion.as_ref());
    (plans, chosen, claim)
}
