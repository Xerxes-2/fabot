pub mod expand;
pub mod intel;
pub mod outposts;
mod season;

use fabot_base::intel::Scout;
use fabot_base::intents::ModuleOutput;
use std::collections::{BTreeMap, BTreeSet};

use expand::Expansions;
use fabot_atlas::{self as atlas, Atlas, Context, Derived};
use fabot_base::declarations::{
    ColonyPlan, Declarations, OpKind, Operation, Outpost, Planned, Stage,
};
use fabot_base::engine;
use fabot_base::intel::Intel;
use fabot_base::tiles;
use fabot_base::vocabulary::{Part, ResourceType, multiple};
use fabot_base::world::{RoomName, Rules, StructureType, World, sightings};
use fabot_fight::shape::{Cast, Engagement};
use fabot_layout::{RING_LEVEL, Standing};
use screeps::LocalRoomTerrain;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct StrategyState {
    /// `Scout room` declarations, rebuilt each tick before Economy reads them.
    pub scouts: Vec<Scout>,
    pub expansion: Expansions,
}

pub fn decide(context: &mut Context, state: &mut StrategyState) -> ModuleOutput {
    let preyed: Vec<RoomName> = context.declarations.rooms(OpKind::Sweep).collect();
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
    let capacity = screeps::SPAWN_ENERGY_CAPACITY as i32
        + rules.allowed(StructureType::Extension, rival) * engine::extension_energy(rival);
    let pairs = |part: Part| {
        (capacity / engine::body_cost(&[part, Part::Move])).min(engine::MAX_BODY_PARTS as i32 / 2)
    };
    let boost = |part: Part, catalyzed: ResourceType| {
        if rules.allowed(StructureType::Lab, rival) > 0 {
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
    let fire = rules.allowed(StructureType::Tower, level)
        * engine::tower_attack_at(engine::TOWER_FALLOFF_RANGE);
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
    let fire = rules.allowed(StructureType::Tower, level)
        * engine::TOWER_ENERGY_COST
        * engine::CREEP_LIFETIME;
    let full = (f64::from(fire) * lifetimes) as i32;
    if threatened { full } else { full / 3 }
}

/// Switched off, the choices are the operator's alone, but each colony keeps
/// its plan: Economy reads its reserve and Layout drops a room without one.
pub fn plan<'t>(
    (on, world, atlas): (bool, &World, &mut Atlas),
    derived: &Derived,
    declarations: &Declarations,
    intel: &Intel,
    (engagements, casts, due): (&[Engagement], &[Cast], &BTreeSet<RoomName>),
    (layout, spare): (&BTreeMap<RoomName, Standing>, &BTreeMap<RoomName, f64>),
    (terrain, expansion): (
        &dyn Fn(RoomName) -> Option<&'t LocalRoomTerrain>,
        &mut Expansions,
    ),
) -> Planned {
    let rival = |name: &str| name != world.me && !declarations.allies.contains(name);
    let mut rivals: Vec<(RoomName, i32)> = Vec::new();
    let mut theirs: BTreeSet<RoomName> = BTreeSet::new();
    for (room, row) in intel {
        let owner = row.owner.as_deref().filter(|_| row.level > 0);
        let reserver = (row.reservation.as_ref())
            .filter(|(user, lapse)| *lapse > world.time && user != screeps::INVADER_USERNAME)
            .map(|(user, _)| user.as_str());
        if owner.is_some_and(rival) {
            rivals.push((*room, row.level));
        }
        if owner.is_some_and(|o| o != world.me) || reserver.is_some_and(rival) {
            theirs.insert(*room);
        }
    }
    let fought = |room: RoomName| engagements.iter().any(|e| e.room == room);
    let fighting: BTreeSet<RoomName> = (engagements.iter())
        .flat_map(|e| outposts::casters(atlas, derived, &declarations.avoid, e.room))
        .collect();
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
            let owes = asked > 0 || due.contains(&room.name);
            let kept = reserve(
                &world.rules,
                declarations.reserve_lifetimes,
                level,
                threatened,
            );
            let (rampart_hits, outheals) =
                wall(&world.rules, threat.unwrap_or(level), level, threatened);
            let stored = (room.structures.iter())
                .any(|s| s.kind == StructureType::Storage && s.owner.is_ours());
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
                owes,
                fights: owes || fighting.contains(&room.name),
                pile,
                muster,
            })
        })
        .collect();
    if !on {
        let mut operations = operations(derived, declarations, (&[], None), &[]);
        let planned = (declarations, &plans[..]);
        operations.extend(crossed((atlas, derived), planned, (&[], None), &theirs));
        return (plans, Vec::new(), None, operations);
    }
    // An expansion declared this tick holds the outposts from the next.
    let claiming = expand::claim(intel, derived, expansion.in_flight.as_ref());
    let claimed = claiming.as_ref().map(|c| c.room);
    let mut preyed = intel::preyed(intel, declarations, world.time);
    preyed.retain(|room| atlas.has_terrain(*room));
    let declared = operations(derived, declarations, (&[], claiming.as_ref()), &preyed);
    let busy = outposts::busy(
        atlas,
        (&plans, derived),
        (declarations, claiming.as_ref()),
        (&declared, engagements),
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
    let may_start = expand::may_start(derived, declarations, expansion);
    expand::book(world, derived, &mut plans, &busy, may_start);
    let claim = expand::claim(intel, derived, expansion.in_flight.as_ref());
    let mut operations = operations(derived, declarations, (&chosen, claim.as_ref()), &preyed);
    let (planned, worked) = ((declarations, &plans[..]), (&chosen[..], claim.as_ref()));
    operations.extend(crossed((atlas, derived), planned, worked, &theirs));
    (plans, chosen, claim, operations)
}

/// A home's raid is answered before an outpost's; a siege or sweep takes
/// only the bodies no defence asks for.
const HOME: f64 = 2.0;
const OUTPOST: f64 = 1.0;

fn operations(
    derived: &Derived,
    declarations: &Declarations,
    (chosen, claim): (&[Outpost], Option<&Outpost>),
    preyed: &[RoomName],
) -> Vec<Operation> {
    let homes: BTreeSet<RoomName> = derived.colonies.iter().copied().collect();
    let worked: BTreeSet<RoomName> = (worked(derived, declarations, (chosen, claim)))
        .map(|(o, _)| o.room)
        .collect();
    (homes.iter().map(row(OpKind::Defend, HOME)))
        .chain(worked.iter().map(row(OpKind::Defend, OUTPOST)))
        .chain(declarations.sieges.iter().map(row(OpKind::Siege, 0.0)))
        .chain(preyed.iter().map(row(OpKind::Sweep, 0.0)))
        .collect()
}

fn worked<'a>(
    derived: &'a Derived,
    declarations: &'a Declarations,
    (chosen, claim): (&'a [Outpost], Option<&'a Outpost>),
) -> impl Iterator<Item = (&'a Outpost, bool)> {
    let outposts = (declarations.outposts.iter().chain(chosen)).map(|o| (o, false));
    (outposts.chain(expand::raising(derived, declarations, claim).map(|o| (o, true))))
        .filter(|(o, _)| !derived.colonies.contains(&o.room))
}

/// Hivemind's remote is under attack while any room its haul walks is: each
/// room on the route from the colony that serves a worked room is defended.
fn crossed(
    (atlas, derived): (&mut Atlas, &Derived),
    (declarations, plans): (&Declarations, &[ColonyPlan]),
    worked: (&[Outpost], Option<&Outpost>),
    theirs: &BTreeSet<RoomName>,
) -> Vec<Operation> {
    let planned = |room| (declarations.colonies.iter().chain(plans)).find(|p| p.room == room);
    let worked: Vec<(&Outpost, bool)> = self::worked(derived, declarations, worked).collect();
    let mut crossed = BTreeSet::new();
    for (o, claim) in &worked {
        if let Some(ci) = atlas::server(atlas, derived, &planned, (&o.controller.pos, *claim)) {
            let (home, tree) = &derived.trees[ci];
            crossed.extend(tree.route(o.room, *home).into_iter().flatten());
        }
    }
    // A Defend where another player holds fights his guards on his ground:
    // a stance the owner's enemies list sets, not a haul's route.
    crossed.retain(|r| {
        !derived.colonies.contains(r)
            && !theirs.contains(r)
            && !worked.iter().any(|(o, _)| o.room == *r)
    });
    crossed.iter().map(row(OpKind::Defend, OUTPOST)).collect()
}

fn row(kind: OpKind, priority: f64) -> impl Fn(&RoomName) -> Operation {
    move |room| Operation {
        kind,
        room: *room,
        priority,
    }
}
