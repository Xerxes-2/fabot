/// The clock is passed in to keep `decide` deterministic.
use std::collections::BTreeSet;

use fabot_atlas::{Atlas, Context, Derived, Mover, derive, target_at, worked};
use fabot_base::{
    HashMap, boost,
    declarations::{Declarations, Refused},
    engine,
    grid::{self, Footprint},
    intel::Intel,
    intents::{Action, Ask, CreepAct, LabAct, ModuleOutput, Rank, Settled, SpawnAct, Telemetry},
    tiles::{self, Tile},
    wish::Wish,
    world::{self, Id, Pos, Room, RoomName, Rules, Sightings, Structure, StructureType, World},
};
use fabot_layout::shape::LayoutState;
use fabot_mv as mv;
use fabot_strategy::{self as strategy, intel};
use fabot_tactics as tactics;
use screeps::LocalRoomTerrain;

use super::state::State;

pub type ModuleRun = fn(&mut Context, &mut State) -> ModuleOutput;

pub struct Module {
    pub name: &'static str,
    pub run: ModuleRun,
}

#[derive(Clone, Debug)]
pub struct Report {
    pub module: &'static str,
    pub cpu: f64,
}

pub struct TickResult {
    pub settled: Settled,
    pub telemetry: Telemetry,
    pub state: State,
    pub reports: Vec<Report>,
}

fn by_room<T>(pairs: &[(RoomName, T)]) -> Vec<(RoomName, Vec<&T>)> {
    let mut index: HashMap<RoomName, usize> = HashMap::default();
    let mut groups: Vec<(RoomName, Vec<&T>)> = Vec::new();
    for (room, value) in pairs {
        let i = *index.entry(*room).or_insert_with(|| {
            groups.push((*room, Vec::new()));
            groups.len() - 1
        });
        groups[i].1.push(value);
    }
    groups
}

/// Hivemind's order: the bays, which their harvesters refill, first. None
/// without a bay, so the engine keeps its own.
fn drawn(rules: &Rules, r: &Room, spawn: &Pos) -> Vec<Id> {
    let in_bay = Atlas::in_bay(r);
    let bay = |s: &&Structure| in_bay(s.kind, &s.pos);
    let ours = rules.working(r).filter(|s| s.owner.is_ours());
    let mut energy: Vec<&Structure> = ours
        .filter(|s| matches!(s.kind, StructureType::Spawn | StructureType::Extension))
        .collect();
    if !energy.iter().any(bay) {
        return Vec::new();
    }
    energy.sort_by_key(|s| (!bay(s), s.kind != StructureType::Spawn, spawn.range(&s.pos)));
    energy.into_iter().map(|s| s.id).collect()
}

/// Each room's spawn queue, in `Rank` order on its idle spawns while the bank
/// pays: a Defend the bank cannot pay yet holds what ranks behind it, any
/// other ask nothing.
fn queue(context: &Context, layout: &LayoutState, asks: &[(RoomName, Ask)]) -> Vec<Action> {
    let world = context.world;
    let mut out = Vec::new();
    for (room, mut asks) in by_room(asks) {
        asks.sort_by_key(|a| a.rank);
        let Some(r) = world.room(room) else {
            continue;
        };
        let open = |t| context.atlas.ground_at(room, t).is_some();
        let mut idle =
            world.working_spawns().filter(|s| s.pos.room == room && s.spawning.is_none());
        let mut bank = r.energy_available;
        for a in &asks {
            let cost = engine::body_cost(&a.body);
            let defends = matches!(a.rank, Rank::Defend | Rank::DefendOutpost);
            if cost > bank && defends && cost <= r.energy_capacity {
                break;
            }
            if cost > bank {
                continue;
            }
            let Some(spawn) = idle.next() else {
                break;
            };
            out.push(Action::Spawn(
                spawn.id,
                SpawnAct::SpawnCreep(
                    a.body.clone(),
                    a.name.clone(),
                    drawn(&world.rules, r, &spawn.pos),
                    (layout.get(&room).and_then(|s| s.plan.as_ref()))
                        .map_or_else(Vec::new, |p| p.doors(tiles::of_pos(&spawn.pos), open)),
                ),
            ));
            bank -= cost;
        }
    }
    out
}

/// One arbitration per room over every module's wishes (§3's `Move`); a creep
/// of ours that wishes nothing holds its tile.
pub fn arbitrate(context: &Context, wishes: &[(RoomName, Wish)]) -> (Vec<Action>, Vec<String>) {
    let groups = by_room(wishes);
    let wished: BTreeSet<&str> =
        (groups.iter()).flat_map(|(_, ws)| ws.iter().map(|w| w.creep.as_str())).collect();
    let mut scratch = mv::arbitrate::scratch();
    let mut out = Vec::new();
    let mut stood = Vec::new();

    for (room, ws) in groups {
        // A wish onto a hostile's tile follows it: the engine moves the body
        // there only once the hostile steps off, so it holds its tile.
        let theirs: BTreeSet<Tile> = (context.world.room(room).into_iter())
            .flat_map(|r| r.hostiles().map(|c| tiles::of_pos(&c.pos)))
            .collect();
        let (glued, ws): (Vec<&Wish>, Vec<&Wish>) =
            (ws.into_iter()).partition(|w| w.head != w.at && theirs.contains(&w.head));
        out.extend(glued.iter().map(|w| {
            Action::Creep(w.creep.clone(), CreepAct::Move(tiles::direction(w.at, w.head)))
        }));
        let mut held: Vec<Tile> = context
            .world
            .creeps
            .iter()
            .filter(|c| c.pos.room == room && !wished.contains(c.name.as_str()))
            .map(|c| tiles::of_pos(&c.pos))
            .collect();
        held.extend(glued.iter().map(|w| w.at));
        let steps = mv::arbitrate::arbitrate_with(
            &mut scratch,
            |tile| context.atlas.passable(room, tile) || context.atlas.is_struck(room, tile),
            &held,
            &ws,
        );
        let stepped: BTreeSet<&str> = steps.iter().map(|(name, _)| name.as_str()).collect();
        stood.extend(
            (ws.iter())
                .filter(|w| w.moves && !w.stays() && !stepped.contains(w.creep.as_str()))
                .map(|w| w.creep.clone()),
        );
        out.extend(
            steps
                .into_iter()
                .map(|(name, direction)| Action::Creep(name, CreepAct::Move(direction))),
        );
    }
    (out, stood)
}

/// A body `boost::boosting` stands at a lab is held there and boosted: its
/// module's acts and wish are dropped for the walk, and the boost itself when it
/// stands beside it.
fn boost_stop(
    context: &mut Context,
    actions: &mut Vec<Action>,
    wishes: &[(RoomName, Wish)],
) -> Option<Vec<(RoomName, Wish)>> {
    let world = context.world;
    let stalls: Vec<(&world::Creep, &boost::Wait)> = (context.derived.boosts.iter())
        .flat_map(|(_, b)| b.waits.iter().filter(|w| w.stands))
        .filter_map(|w| Some((world.creeps.iter().find(|c| c.id == w.creep)?, w)))
        .collect();

    if stalls.is_empty() {
        return None;
    }

    let stalling: BTreeSet<&str> = stalls.iter().map(|(c, _)| c.name.as_str()).collect();
    actions.retain(|a| !matches!(a, Action::Creep(name, _) if stalling.contains(name.as_str())));
    let mut walks: Vec<(RoomName, Wish)> =
        (wishes.iter()).filter(|(_, w)| !stalling.contains(w.creep.as_str())).cloned().collect();

    let mut seated: Vec<Pos> = Vec::new();
    for (creep, w) in &stalls {
        let at = &w.at;
        // So it stands in no other lab's feed a carrier must reach.
        let labs: Vec<Pos> = boost::labs_in(world, at.room).map(|l| l.pos).collect();
        let feeds = |p: &Pos| labs.iter().filter(|l| *l != at && p.within(1, l)).count();
        let seat = (context.atlas.seats(Footprint::One, 1, at).into_iter())
            .filter(|p| !seated.contains(p))
            .min_by_key(|p| (feeds(p), p.range(&creep.pos), tiles::of_pos(p)))
            .unwrap_or(*at);
        seated.push(seat);
        let lab = (world.rooms.iter()).flat_map(|r| &r.structures).find(|s| s.id == w.lab);
        if creep.pos.within(1, at) && lab.is_some_and(|l| boost::can_boost(l, w.load.compound)) {
            actions.push(Action::Lab(w.lab, LabAct::BoostCreep(creep.id)));
        }
        let mover = Mover::one(grid::factor_of(creep));
        // Beside its lab it gives way to the carrier filling it; out of reach
        // it shoves through, or a carrier parked on the lab's one free side
        // holds it off until its wait runs out and it leaves short.
        let weight = if creep.pos.within(1, at) { 1 } else { tactics::WEIGHT };
        let goal = target_at(&seat, i32::from(seat == *at));
        let wish = mv::plan::toward(context.atlas, creep, mover, &goal, weight);
        walks.push((creep.pos.room, wish));
    }

    Some(walks)
}

fn settle(context: &mut Context, layout: &LayoutState, asked: &ModuleOutput) -> Settled {
    let mut out = queue(context, layout, &asked.asks);
    out.extend(asked.actions.iter().cloned());
    let walks = boost_stop(context, &mut out, &asked.wishes);
    let (moves, held) = arbitrate(context, walks.as_deref().unwrap_or(&asked.wishes));
    out.extend(moves);
    out.extend(super::sign::reflex(context.world, context.declarations));
    if context.declarations.says_idle {
        out.extend(asked.telemetry.says());
    }
    Settled { actions: out, held }
}

/// A room the engine never lets a creep into serves nothing, and a tactic
/// rule no name answers to never applies; both dropped, for the operator.
fn refuse(declarations: &mut Declarations, closed: &BTreeSet<RoomName>) -> Refused {
    let rooms = declarations.refuse_rooms(closed);
    let (kept, rules) = tactics::checked(std::mem::take(&mut declarations.tactics));
    declarations.tactics = kept;
    Refused { rooms, rules }
}

pub fn run(
    clock: impl Fn() -> f64,
    modules: &[Module],
    context: &mut Context,
    mut state: State,
) -> TickResult {
    let mut asked = ModuleOutput::default();
    let mut reports: Vec<Report> = Vec::new();

    for m in modules {
        if state.off.contains(m.name) {
            reports.push(Report { module: m.name, cpu: 0.0 });
            continue;
        }
        let start = clock();
        let produced = (m.run)(context, &mut state);
        reports.push(Report { module: m.name, cpu: clock() - start });
        asked.extend(produced);
    }

    let start = clock();
    let settled = settle(context, &state.layout, &asked);
    reports.push(Report { module: "settle", cpu: clock() - start });

    TickResult { settled, telemetry: asked.telemetry, state, reports }
}

/// A tick decided: what the driver executes and writes, and what it keeps.
pub struct Decided {
    pub result: TickResult,
    pub declarations: Declarations,
    pub derived: Derived,
    pub refused: Refused,
    pub sightings: Sightings,
    pub atlas: Atlas,
    pub misfiled: fabot_strategy::Misfiled,
}

/// From the world read to the intents settled. `began` is when the `world`
/// phase began, after the driver read it.
pub fn decide<'t>(
    (clock, began): (impl Fn() -> f64, f64),
    phases: &mut Vec<(String, f64)>,
    (world, trace): (&World, bool),
    (mut declarations, state): (Declarations, State),
    (sightings, atlas): (Sightings, Atlas),
    intel: &'t mut Intel,
    (terrain, terrains): (
        &dyn Fn(RoomName) -> Option<&'t LocalRoomTerrain>,
        impl IntoIterator<Item = (RoomName, &'t [u8; tiles::COUNT])>,
    ),
) -> Decided {
    let mut state = state;
    let refused = refuse(&mut declarations, &world.closed);
    let sightings = fabot_base::world::sightings::step(world, sightings);
    let kept = !atlas.check || fabot_base::world::sightings::kept(world, &sightings);
    assert!(kept, "a sighting differs from what stands now");
    intel::step(world, &atlas, intel);
    phases.push(("world".to_string(), clock() - began));

    let start = clock();
    let seen = (intel.iter()).map(|(room, row)| {
        (
            *room,
            row.lairs.as_deref(),
            row.stronghold.is_some_and(|s| s.collapses_at > world.time)
                || row.guarded(&world.me, &declarations),
        )
    });
    let mut atlas = Atlas::following(atlas, world, seen, terrains);
    phases.push(("atlas".to_string(), clock() - start));

    let start = clock();
    let mut derived = derive(world, &mut atlas);
    phases.push(("derive".to_string(), clock() - start));
    // Before Strategy: Economy's home pass stocks the labs these waits keep.
    let boosted: Vec<_> = fabot_fight::boosted(&state.fight.engagements).collect();
    derived.boosts = (boost::clusters(world, &declarations).into_iter())
        .map(|r| {
            let here: Vec<&str> =
                (boosted.iter()).filter(|b| b.colony == r).map(|b| b.name.as_str()).collect();
            (r, boost::boosting(world, r, &here))
        })
        .collect();
    let (plans, misfiled) = strategy::plan(
        (!state.off.contains(STRATEGY), world, &mut atlas),
        &derived,
        &declarations,
        intel,
        (&state.fight.engagements, &state.fight.casts),
        (&state.layout, &mut state.economy),
        (terrain, &mut state.strategy),
    );
    let declarations = declarations.planned(plans);
    derived.worked = worked(world, &sightings, &declarations, &derived);

    let mut context = Context {
        world,
        atlas: &mut atlas,
        declarations: &declarations,
        sightings: &sightings,
        intel,
        derived: &derived,
        trace,
    };
    let result = run(clock, &MODULES, &mut context, state);
    for report in &result.reports {
        phases.push((report.module.to_string(), report.cpu));
    }
    Decided { result, declarations, derived, refused, sightings, atlas, misfiled }
}

const STRATEGY: &str = "strategy";

/// The modules `Main` runs, in this order.
pub const MODULES: [Module; 4] = [
    Module { name: STRATEGY, run: |c, s| fabot_strategy::decide(c, &mut s.strategy) },
    Module { name: "fight", run: |c, s| fabot_fight::decide(c, &mut s.fight) },
    Module {
        name: "economy",
        run: |c, s| fabot_economy::decide(c, &mut s.economy, &s.strategy.scouts),
    },
    Module { name: "layout", run: |c, s| fabot_layout::decide(c, &mut s.layout) },
];
