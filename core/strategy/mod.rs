pub mod expand;
pub mod intel;
pub mod outposts;

use fabot_base::intel::Scout;
use fabot_base::intents::ModuleOutput;
use std::collections::{BTreeMap, BTreeSet};

use super::fight::shape::{Cast, Engagement};
use super::layout::{RING_LEVEL, Standing};
use crate::atlas::{Context, Derived};
use expand::Expansions;
use fabot_base::declarations::{ColonyPlan, Declarations, Outpost, Stage};
use fabot_base::engine;
use fabot_base::intel::Intel;
use fabot_base::tiles;
use fabot_base::vocabulary::{Part, ResourceType, multiple};
use fabot_base::world::{Kind, RoomName, Rules, World, sightings};
use screeps::LocalRoomTerrain;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct StrategyState {
    /// `Scout room` declarations, rebuilt each tick before Economy reads them.
    pub scouts: Vec<Scout>,
    /// Sweep candidates this tick, the stalest seen first.
    pub preyed: Vec<RoomName>,
    pub expansion: Expansions,
}

pub fn decide(context: &mut Context, state: &mut StrategyState) -> ModuleOutput {
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
    observed.into()
}

/// A rival's room this near raids within a creep life.
const NEAR: i32 = 3;

/// The longest a rampart holds, as v1's floor priced it: the ticks a colony
/// takes to cast its answer twice, the first lost.
const HOLD: i32 = 2 * engine::MAX_BODY_PARTS as i32 * engine::SPAWN_TICKS_PER_PART;

/// Hivemind's quiet rampart hits.
const FLOOR: i32 = 10_000;

/// A quad, as TooAngel and most bots send: one dismantler, three healers.
const HEALERS: i32 = 3;

/// The threat in, the hits a rampart is kept at out, and whether the quad
/// out-heals the towers, which rings a colony below `RING_LEVEL` (§5).
pub fn wall(rules: &Rules, rival: i32, level: i32, threatened: bool) -> (i32, bool) {
    let capacity =
        engine::SPAWN_ENERGY + rules.allowed("extension", rival) * engine::extension_energy(rival);
    let pairs = |part: Part| {
        (capacity / engine::body_cost(&[part, Part::Move])).min(engine::MAX_BODY_PARTS as i32 / 2)
    };
    let boost = |part: Part, catalyzed: ResourceType| {
        if rules.allowed("lab", rival) > 0 {
            multiple(part, Some(catalyzed))
        } else {
            1
        }
    };
    let work = pairs(Part::Work);
    let heal = HEALERS
        * pairs(Part::Heal)
        * engine::HEAL_POWER
        * boost(Part::Heal, ResourceType::CatalyzedLemergiumAlkalide);
    let fire = rules.allowed("tower", level) * engine::tower_attack_at(engine::TOWER_FALLOFF_RANGE);
    let hits = 2 * work * engine::PART_HITS;
    let held = if fire > heal {
        HOLD.min(hits / (fire - heal) + 1)
    } else {
        HOLD
    };
    let full = work
        * engine::DISMANTLE_POWER
        * boost(Part::Work, ResourceType::CatalyzedZynthiumAcid)
        * held;
    let kept = match () {
        _ if threatened => full.max(FLOOR),
        // The controller there takes no surplus, so a quiet wall thickens instead.
        _ if engine::topped_out(level) => (full / 3).max(FLOOR),
        _ => FLOOR,
    };
    (kept, fire <= heal)
}

/// The threat in, a reserve out, as a priced choice would be (§5).
pub fn reserve(rules: &Rules, lifetimes: f64, level: i32, threatened: bool) -> i32 {
    let fire = rules.allowed("tower", level) * engine::TOWER_ENERGY_COST * engine::CREEP_LIFETIME;
    let full = (f64::from(fire) * lifetimes) as i32;
    if threatened { full } else { full / 3 }
}

/// Switched off, the choices are the operator's alone, but each colony keeps
/// its plan: Economy reads its reserve and Layout drops a room without one.
pub fn plan<'t>(
    (on, world): (bool, &World),
    derived: &Derived,
    declarations: &Declarations,
    intel: &Intel,
    (engagements, casts, due): (&[Engagement], &[Cast], &BTreeSet<RoomName>),
    (layout, spare): (&BTreeMap<RoomName, Standing>, &BTreeMap<RoomName, f64>),
    (terrain, expansion): (
        &dyn Fn(RoomName) -> Option<&'t LocalRoomTerrain>,
        &mut Expansions,
    ),
) -> (Vec<ColonyPlan>, Vec<Outpost>, Option<Outpost>) {
    let rival = |name: &str| name != world.me && !declarations.allies.contains(name);
    let rivals: Vec<(RoomName, i32)> = (intel.iter())
        .filter(|(_, row)| row.level > 0 && row.owner.as_deref().is_some_and(rival))
        .map(|(room, row)| (*room, row.level))
        .collect();
    let fought = |room: RoomName| engagements.iter().any(|e| e.room == room);
    let mut plans: Vec<ColonyPlan> = (derived.owned.iter().filter_map(|r| world.room(*r)))
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
            let stored =
                (room.structures.iter()).any(|s| s.kind == Kind::Storage && s.owner.is_ours());
            let plan = layout.get(&room.name).and_then(|s| s.plan.as_ref());
            let pile = (level >= 2 && !stored).then(|| plan?.pile()).flatten();
            let muster = plan.and_then(|p| p.muster);
            Some(ColonyPlan {
                room: room.name,
                stage: if on && expansion.abandoning(room.name) {
                    Stage::Abandon
                } else {
                    Stage::Exploit
                },
                target: level,
                reserve: kept + asked,
                rampart_hits,
                ring: level >= RING_LEVEL || (near && outheals),
                owes: asked > 0 || due.contains(&room.name),
                pile,
                muster,
            })
        })
        .collect();
    if !on {
        return (plans, Vec::new(), None);
    }
    // An expansion declared this tick holds the outposts from the next.
    let claimed = expand::claim(intel, derived, expansion.in_flight.as_ref()).map(|c| c.room);
    let preyed = intel::preyed(intel, declarations, world.time);
    let busy = outposts::busy(
        &plans,
        (declarations, claimed),
        (engagements, &preyed, derived),
    );
    let candidates = outposts::survey(world, derived, (declarations, &plans, claimed), intel);
    let chosen = outposts::chosen(world, candidates, (&busy, spare));
    let fought: BTreeSet<RoomName> = engagements.iter().map(|e| e.room).collect();
    let around = expand::Around {
        rivals: &rivals,
        outposts: &chosen,
        busy: &busy,
        fought: &fought,
    };
    expand::step(
        world,
        derived,
        (declarations, &plans),
        &around,
        intel,
        terrain,
        expansion,
    );
    expand::book(world, derived, &mut plans, &busy);
    let claim = expand::claim(intel, derived, expansion.in_flight.as_ref());
    (plans, chosen, claim)
}
