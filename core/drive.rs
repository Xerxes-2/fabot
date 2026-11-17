//! A driver's tick between its read and its writes (`ARCHITECTURE.md` §1):
//! the heap threaded through `decide`, the intents executed, Memory written,
//! the metrics measured. The wasm driver and `sim/` call it alike.
use std::collections::BTreeMap;

use screeps::LocalRoomTerrain;

use crate::atlas::{Atlas, tiles};
use crate::fight::shape::Phase;
use crate::intents::{Intent, Refusal};
use crate::layout::shape::Standing;
use crate::memory::Memory;
use crate::observe::metrics::{self, Metrics};
use crate::strategy::intel::Intel;
use crate::tactics::Offer;
use crate::tick::{self, Declarations, Outcome};
use crate::world::{RoomName, Sightings, World};

/// `ARCHITECTURE.md` §4's heap rows `decide` reads and hands back; empty after a reset.
pub struct Heap {
    pub standing: BTreeMap<RoomName, Standing>,
    pub spare: BTreeMap<RoomName, f64>,
    pub sightings: Sightings,
    pub atlas: Atlas,
}

impl Heap {
    pub fn new() -> Heap {
        Heap {
            standing: BTreeMap::new(),
            spare: BTreeMap::new(),
            sightings: Sightings::new(),
            atlas: Atlas::empty(),
        }
    }
}

impl Default for Heap {
    fn default() -> Heap {
        Heap::new()
    }
}

/// What the driver reports and writes after the tick.
pub struct Ran<'t> {
    pub world: World<'t>,
    pub refusals: Vec<Refusal>,
    pub faults: Vec<(&'static str, String)>,
    pub counts: Vec<(String, i32)>,
    pub off: Vec<String>,
    pub offers: Vec<(i32, Vec<Offer>)>,
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

pub fn run<'t, P, R, E, W>(
    heap: &mut Heap,
    (memory, intel, metrics): (&mut Memory, &mut Intel, Option<&mut Metrics>),
    (clock, phases, trace): (&dyn Fn() -> f64, &mut Vec<(String, f64)>, bool),
    seam: Seam<R, E, W>,
) -> Ran<'t>
where
    R: FnOnce(&Declarations, Option<RoomName>) -> (World<'t>, &'t dyn Terrains, P),
    E: FnOnce(&[Intent]) -> Vec<Refusal>,
    W: FnOnce(&World, &P, &Intel, Option<String>, &mut Vec<(String, f64)>),
{
    let mut state = memory.state();
    state.layout = std::mem::take(&mut heap.standing);
    state.spare = std::mem::take(&mut heap.spare);

    let declarations: Declarations = memory.declarations().besieging(intel);
    let start = clock();
    let drain = (state.engagements.iter())
        .find(|e| e.phase == Phase::Draining)
        .map(|e| e.room);
    let (world, maps, pass) = (seam.read)(&declarations, drain);
    let sightings = std::mem::take(&mut heap.sightings);
    let atlas = std::mem::replace(&mut heap.atlas, Atlas::empty());
    let decided = tick::decide(
        (clock, start),
        phases,
        (&world, trace),
        (declarations, state),
        (sightings, atlas),
        &mut *intel,
        (&|room| maps.terrain(room), maps.terrains()),
    );

    let faults = (decided.result.reports.iter())
        .filter_map(|r| match &r.outcome {
            Outcome::Faulted(message) => Some((r.module, message.clone())),
            _ => None,
        })
        .collect();

    let start = clock();
    let refusals = (seam.execute)(&decided.result.intents);
    phases.push(("execute".to_string(), clock() - start));
    (seam.write)(
        &world,
        &pass,
        intel,
        memory.written(&decided.result.state),
        phases,
    );

    let counts = metrics::counts(&world, &decided);
    if let Some(m) = metrics {
        let start = clock();
        *m = metrics::measure(std::mem::take(m), &world, &decided, &refusals, phases);
        phases.push(("metrics".to_string(), clock() - start));
    }

    let tick::Decided {
        mut result,
        sightings,
        atlas,
        ..
    } = decided;
    heap.standing = std::mem::take(&mut result.state.layout);
    heap.spare = std::mem::take(&mut result.state.spare);
    heap.sightings = sightings;
    heap.atlas = atlas;
    let engagements = result.state.engagements.iter();
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
    }
}
