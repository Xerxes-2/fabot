//! A driver's tick between its read and its writes (`ARCHITECTURE.md` §1):
//! the heap threaded through `decide`, the intents executed, Memory written,
//! the metrics measured. The wasm driver and `sim/` call it alike.
use std::collections::BTreeMap;

use fabot_atlas::Atlas;
use fabot_base::{
    declarations::Declarations,
    intel::Intel,
    intents::{Action, Refusal, Telemetry},
    tiles,
    world::{RoomName, Sightings, World},
};
use fabot_layout::shape::Standing;
use fabot_observe::{
    engagements,
    metrics::{self, Metrics},
};
use fabot_tactics::Offer;
use screeps::LocalRoomTerrain;

use crate::{memory::Memory, tick};

/// `ARCHITECTURE.md` §4's heap rows `decide` reads and hands back; empty after a reset.
#[derive(Default)]
pub struct Heap {
    pub standing: BTreeMap<RoomName, Standing>,
    pub sightings: Sightings,
    pub atlas: Atlas,
}

/// What the driver reports and writes after the tick.
pub struct Ran<'t> {
    pub world: World<'t>,
    pub refusals: Vec<Refusal>,
    pub faults: Vec<(&'static str, String)>,
    pub counts: Vec<(String, i32)>,
    pub off: Vec<String>,
    pub offers: Vec<(i32, Vec<Offer>)>,
    /// `engagements.log`'s rows of the engagements closed this tick.
    pub closed: Vec<String>,
    pub telemetry: Telemetry,
}

/// The terrain a driver has read: a room's, and every room's for the Atlas.
pub trait Terrains {
    fn terrain(&self, room: RoomName) -> Option<&LocalRoomTerrain>;
    fn terrains(&self) -> Box<dyn Iterator<Item = (RoomName, &[u8; tiles::COUNT])> + '_>;
}

/// The driver's side of a tick, in the order they run: the world read for
/// the declarations, with its terrain and what else the driver keeps of the
/// read; the intents executed; Memory's text (when it changed) written with the world, that read and
/// intel beside it, each pushing its own phases.
pub type Phases = Vec<(String, f64)>;
/// Named per-tick counts (`ARCHITECTURE.md` §9.3).
pub type Counts = Vec<(String, i32)>;
/// When the read began, and where it adds its phases and counts.
pub type Clocked<'a> = (f64, &'a mut Phases, &'a mut Counts);

pub struct Seam<R, E, W> {
    pub read: R,
    pub execute: E,
    pub write: W,
}

pub fn run<'t, P, R, E, W>(
    heap: &mut Heap,
    (memory, intel, metrics): (&mut Memory, &mut Intel, Option<&mut Metrics>),
    (clock, phases, trace): (&dyn Fn() -> f64, &mut Vec<(String, f64)>, bool),
    seam: Seam<R, E, W>,
) -> Ran<'t>
where
    R: FnOnce(&Declarations, Clocked<'_>) -> (World<'t>, &'t dyn Terrains, P, f64),
    E: FnOnce(&[Action]) -> Vec<Refusal>,
    W: FnOnce(&World, &P, &Intel, Option<String>, &mut Vec<(String, f64)>),
{
    let start = clock();
    let mut state = memory.state();
    let (declarations, untuned) = memory.declarations();
    state.layout = std::mem::take(&mut heap.standing);
    let read = clock();
    phases.push(("hydrate".to_string(), read - start));

    let mut walked = Counts::new();
    let (mut world, maps, pass, start) = (seam.read)(&declarations, (read, phases, &mut walked));
    (world.rooms.iter_mut()).for_each(|r| r.terrain = maps.terrain(r.name));
    let misread = world.drop_misread();
    let sightings = std::mem::take(&mut heap.sightings);
    let atlas = std::mem::take(&mut heap.atlas);
    let decided = tick::decide(
        (clock, start),
        phases,
        (&world, trace),
        (declarations, state),
        (sightings, atlas),
        &mut *intel,
        (&|room| maps.terrain(room), maps.terrains()),
    );

    let closed = engagements::rows(&decided.result.state.fight.closed, world.time);
    let mut faults: Vec<_> = untuned.into_iter().map(|e| ("tuning", e)).collect();
    if misread > 0 {
        faults.push(("world", format!("{misread} objects' ids not hex: dropped")));
    }
    for room in &decided.misfiled {
        faults.push(("strategy", format!("{room}: kept pile is not its plan's")));
    }

    let start = clock();
    let refusals = (seam.execute)(&decided.result.settled.actions);
    phases.push(("execute".to_string(), clock() - start));
    let start = clock();
    let text = memory.written(&decided.result.state);
    phases.push(("serialize".to_string(), clock() - start));
    (seam.write)(&world, &pass, intel, text, phases);

    let mut counts = metrics::counts(
        &world,
        (&decided.atlas, &decided.derived),
        &decided.declarations,
        &decided.refused,
        &decided.result.telemetry.counts,
    );
    if let Some(m) = metrics {
        let start = clock();
        let state = &decided.result.state;
        let seen = metrics::Seen {
            world: &world,
            assignments: &state.economy.assignments,
            engagements: &state.fight.engagements,
            casts: &state.fight.casts,
            intents: &decided.result.settled.actions,
            held: &decided.result.settled.held,
            telemetry: &decided.result.telemetry,
            refusals: &refusals,
            phases,
            walked: &walked,
            declarations: &decided.declarations,
            derived: &decided.derived,
        };
        *m = metrics::step(std::mem::take(m), &seen);
        phases.push(("metrics".to_string(), clock() - start));
    }

    counts.extend(walked);
    let tick::Decided { mut result, sightings, atlas, .. } = decided;
    heap.standing = std::mem::take(&mut result.state.layout);
    heap.sightings = sightings;
    heap.atlas = atlas;
    let engagements = result.state.fight.engagements.iter();
    Ran {
        world,
        refusals,
        faults,
        counts,
        off: result.state.off.iter().cloned().collect(),
        offers: engagements
            .filter(|e| !e.plan.offers.is_empty())
            .map(|e| (e.id, e.plan.offers.clone()))
            .collect(),
        closed,
        telemetry: result.telemetry,
    }
}
