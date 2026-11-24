pub mod expand;
pub mod intel;
pub mod outposts;
mod season;
pub mod stronghold;

use fabot_base::intel::Scout;
use fabot_base::intents::ModuleOutput;
use std::collections::{BTreeMap, BTreeSet};

use expand::Expansions;
use fabot_atlas::{self as atlas, Atlas, BANK_LEVEL, Context, Derived};
use fabot_base::declarations::{
    ColonyPlan, Declarations, OpKind, Operation, Outpost, Planned, Stage,
};
use fabot_base::engine;
use fabot_base::intel::{Intel, Row};
use fabot_base::tiles;
use fabot_base::tuning::Tuning;
use fabot_base::vocabulary::{Part, ResourceType, multiple};
use fabot_base::world::{Pos, RoomName, Rules, StructureType, World, sightings};
use fabot_economy::EconomyState;
use fabot_fight::shape::{Cast, Engagement};
use fabot_layout::{RING_LEVEL, Standing};
use screeps::LocalRoomTerrain;

/// `ARCHITECTURE.md` §4 `piles`: a colony's hub as its plan last drew it.
#[derive(Clone, Copy, Debug, PartialEq, Eq, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum Hub {
    Pile(Pos),
    /// A structure takes the hub's centre (`Plan::pile`).
    Built,
}

impl Hub {
    fn of(pile: Option<Pos>) -> Hub {
        pile.map_or(Hub::Built, Hub::Pile)
    }

    fn pile(self) -> Option<Pos> {
        match self {
            Hub::Pile(at) => Some(at),
            Hub::Built => None,
        }
    }
}

/// A heap's first plan apart from the hub kept before it: the cold ticks
/// banked where no plan does.
pub type Misfiled = Vec<RoomName>;

#[derive(Clone, Debug, Default, PartialEq)]
pub struct StrategyState {
    /// `Scout room` declarations, rebuilt each tick before Economy reads them.
    pub scouts: Vec<Scout>,
    pub expansion: Expansions,
    pub piles: BTreeMap<RoomName, Hub>,
}

pub fn decide(context: &mut Context, state: &mut StrategyState) -> ModuleOutput {
    let preyed: Vec<RoomName> = context.declarations.rooms(OpKind::Sweep).collect();
    // A scout only dies where armed bodies stand, and draws them off; a
    // siege's sight is the fight's.
    let unarmed = |room| sightings::unarmed(context.sightings, context.world.time, room);
    let mut radius = intel::threat_radius(
        (context.world, context.atlas),
        (context.intel, context.declarations),
        &context.derived.owned,
    );
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

/// A quad, as TooAngel and most bots send: one dismantler, three healers.
const HEALERS: i32 = 3;

/// The threat in, the hits a rampart is kept at out, and whether the quad
/// out-heals the towers, which rings a colony below `RING_LEVEL` (§5).
pub fn wall(
    rules: &Rules,
    t: &Tuning,
    (rival, level): (i32, i32),
    threatened: bool,
) -> (i32, bool) {
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
    let heal = engine::heal_at(
        HEALERS * pairs(Part::Heal) * boost(Part::Heal, ResourceType::CatalyzedLemergiumAlkalide),
        engine::MELEE_RANGE,
    );
    let fire =
        rules.allowed(StructureType::Tower, level) * engine::tower_attack_at(engine::ROOM_SIDE);
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
    // The floor is Hivemind's quiet rampart hits.
    let kept = match () {
        _ if threatened => full.max(t.rampart_floor),
        // The controller there takes no surplus, so a quiet wall thickens instead.
        _ if engine::topped_out(level) => {
            ((f64::from(full) / t.quiet_wall_divisor) as i32).max(t.rampart_floor)
        }
        _ => t.rampart_floor,
    };
    (kept, fire <= heal)
}

/// The threat in, a reserve out, as a priced choice would be (§5).
pub fn reserve(rules: &Rules, t: &Tuning, level: i32, threatened: bool) -> i32 {
    let fire = rules.allowed(StructureType::Tower, level)
        * engine::TOWER_ENERGY_COST
        * engine::CREEP_LIFETIME;
    let full = (f64::from(fire) * t.reserve_lifetimes) as i32;
    if threatened {
        full
    } else {
        (f64::from(full) / t.quiet_reserve_divisor) as i32
    }
}

fn rival(world: &World, declarations: &Declarations, name: &str) -> bool {
    name != world.me && !declarations.allies.contains(name)
}

pub fn rivals(world: &World, declarations: &Declarations, intel: &Intel) -> Vec<(RoomName, i32)> {
    let theirs = |row: &&Row| (row.owner.as_deref()).is_some_and(|o| rival(world, declarations, o));
    (intel.iter().filter(|(_, row)| row.level > 0 && theirs(row)))
        .map(|(room, row)| (*room, row.level))
        .collect()
}

/// Switched off, the choices are the operator's alone, but each colony keeps
/// its plan: Economy reads its reserve and Layout drops a room without one.
pub fn plan<'t>(
    (on, world, atlas): (bool, &World, &mut Atlas),
    derived: &Derived,
    declarations: &Declarations,
    intel: &Intel,
    (engagements, casts, due): (&[Engagement], &[Cast], &BTreeSet<RoomName>),
    (layout, economy): (&BTreeMap<RoomName, Standing>, &mut EconomyState),
    (terrain, state): (
        &dyn Fn(RoomName) -> Option<&'t LocalRoomTerrain>,
        &mut StrategyState,
    ),
) -> (Planned, Misfiled) {
    let StrategyState {
        expansion, piles, ..
    } = state;
    let mut misfiled = Vec::new();
    let rival = |name: &str| rival(world, declarations, name);
    let rivals = rivals(world, declarations, intel);
    let mut theirs: BTreeSet<RoomName> = BTreeSet::new();
    for (room, row) in intel {
        let owner = row.owner.as_deref().filter(|_| row.level > 0);
        let reserver = (row.reservation.as_ref())
            .filter(|(user, lapse)| *lapse > world.time && user != screeps::INVADER_USERNAME)
            .map(|(user, _)| user.as_str());
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
            let tuning = &declarations.tuning;
            let kept = reserve(&world.rules, tuning, level, threatened);
            let rival = threat.unwrap_or(level);
            let (rampart_hits, outheals) = wall(&world.rules, tuning, (rival, level), threatened);
            let stored = (room.structures.iter())
                .any(|s| s.kind == StructureType::Storage && s.owner.is_ours());
            let standing = layout.get(&room.name);
            let plan = standing.and_then(|s| s.plan.as_ref());
            let hub = match (plan, standing) {
                (Some(plan), Some(standing)) => {
                    let now = Hub::of(plan.pile());
                    let kept = piles.insert(room.name, now);
                    if standing.cold && kept.is_some_and(|k| k != now) {
                        misfiled.push(room.name);
                    }
                    Some(now)
                }
                _ => piles.get(&room.name).copied(),
            };
            let pile = hub
                .filter(|_| level >= BANK_LEVEL && !stored)
                .and_then(Hub::pile);
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
    piles.retain(|room, _| plans.iter().any(|p| p.room == *room));
    if !on {
        let mut operations = operations(derived, declarations, (&[], None), &[]);
        let planned = (declarations, &plans[..]);
        operations.extend(crossed((atlas, derived), planned, (&[], None), &theirs));
        return ((plans, Vec::new(), None, operations), misfiled);
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
    let candidates = outposts::survey(
        (world, atlas),
        derived,
        (declarations, &plans, claimed),
        intel,
    );
    let homes =
        (declarations.clone()).planned((plans.clone(), Vec::new(), claiming.clone(), Vec::new()));
    let spare = fabot_economy::spare((world, atlas), (derived, &homes), intel, economy);
    let chosen = outposts::chosen(world, candidates, (&busy, &spare), &declarations.tuning);
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
    let margin = declarations.tuning.sponsor_margin;
    expand::book(world, derived, &mut plans, (&busy, margin), may_start);
    let claim = expand::claim(intel, derived, expansion.in_flight.as_ref());
    let mut operations = operations(derived, declarations, (&chosen, claim.as_ref()), &preyed);
    let (planned, worked) = ((declarations, &plans[..]), (&chosen[..], claim.as_ref()));
    operations.extend(crossed((atlas, derived), planned, worked, &theirs));
    let fights = (engagements, casts);
    let held = (derived, &declarations.avoid);
    operations.extend(stronghold::rows(world, intel, held, fights, atlas));
    ((plans, chosen, claim, operations), misfiled)
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
    let planned = |room| atlas::plan_of(declarations, plans, room);
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
