//! A driver's tick between its read and its writes (`ARCHITECTURE.md` §1):
//! the heap threaded through `decide`, the intents executed, Memory written,
//! the metrics measured. The wasm driver and `sim/` call it alike.
use std::collections::BTreeMap;

use screeps::LocalRoomTerrain;

use crate::atlas::{Atlas, tiles};
use crate::declarations::Declarations;
use crate::fight::shape::Phase;
use crate::intel::Intel;
use crate::intents::{Action, Refusal};
use crate::layout::shape::Standing;
use crate::memory::Memory;
use crate::observe::metrics::{self, Metrics};
use crate::tactics::Offer;
use crate::tick::{self, Outcome};
use crate::world::Pos;
use crate::world::{RoomName, Sightings, World};

/// `ARCHITECTURE.md` §4's heap rows `decide` reads and hands back; empty after a reset.
#[derive(Default)]
pub struct Heap {
    pub standing: BTreeMap<RoomName, Standing>,
    pub spare: BTreeMap<RoomName, f64>,
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
    pub traces: Vec<(String, Vec<(Pos, i64)>)>,
}

/// The terrain a driver has read: a room's, and every room's for the Atlas.
pub trait Terrains {
    fn terrain(&self, room: RoomName) -> Option<&LocalRoomTerrain>;
    fn terrains(&self) -> Box<dyn Iterator<Item = (RoomName, &[u8; tiles::COUNT])> + '_>;
}

/// The driver's side of a tick, in the order they run: the world read for
/// the declarations and the room a drain stands in, with the terrain it
/// holds and what else the driver keeps of the read; the intents executed;
/// Memory's text (when it changed) written with the world, that read and
/// intel beside it, each pushing its own phases.
pub struct Seam<R, E, W> {
    pub read: R,
    pub execute: E,
    pub write: W,
}

/// A declared siege's owner, as intel last saw it, is an enemy (owner's call).
fn besieging(mut declarations: Declarations, intel: &Intel) -> Declarations {
    let owners: Vec<String> = (declarations.sieges.iter())
        .filter_map(|room| intel.get(room)?.owner.clone())
        .collect();
    declarations.enemies.extend(owners);
    declarations
}

pub fn run<'t, P, R, E, W>(
    heap: &mut Heap,
    (memory, intel, metrics): (&mut Memory, &mut Intel, Option<&mut Metrics>),
    (clock, phases, trace): (&dyn Fn() -> f64, &mut Vec<(String, f64)>, bool),
    seam: Seam<R, E, W>,
) -> Ran<'t>
where
    R: FnOnce(&Declarations, Option<RoomName>) -> (World<'t>, &'t dyn Terrains, P),
    E: FnOnce(&[Action]) -> Vec<Refusal>,
    W: FnOnce(&World, &P, &Intel, Option<String>, &mut Vec<(String, f64)>),
{
    let mut state = memory.state();
    state.layout = std::mem::take(&mut heap.standing);
    state.economy.spare = std::mem::take(&mut heap.spare);

    let declarations: Declarations = besieging(memory.declarations(), intel);
    let start = clock();
    let drain = (state.fight.engagements.iter())
        .find(|e| e.phase == Phase::Draining)
        .map(|e| e.room);
    let (mut world, maps, pass) = (seam.read)(&declarations, drain);
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

    let mut faults: Vec<_> = (decided.result.reports.iter())
        .filter_map(|r| match &r.outcome {
            Outcome::Faulted(message) => Some((r.module, message.clone())),
            _ => None,
        })
        .collect();
    if misread > 0 {
        faults.push(("world", format!("{misread} objects' ids not hex: dropped")));
    }

    let start = clock();
    let refusals = (seam.execute)(&decided.result.settled.actions);
    phases.push(("execute".to_string(), clock() - start));
    (seam.write)(
        &world,
        &pass,
        intel,
        memory.written(&decided.result.state),
        phases,
    );

    let counts = metrics::counts(
        &world,
        &decided.atlas,
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
            short: &decided.result.telemetry.short,
            refusals: &refusals,
            phases,
            declarations: &decided.declarations,
            derived: &decided.derived,
        };
        *m = metrics::step(std::mem::take(m), &seen);
        phases.push(("metrics".to_string(), clock() - start));
    }

    let tick::Decided {
        mut result,
        sightings,
        atlas,
        ..
    } = decided;
    heap.standing = std::mem::take(&mut result.state.layout);
    heap.spare = std::mem::take(&mut result.state.economy.spare);
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
        traces: result.telemetry.traces,
    }
}
