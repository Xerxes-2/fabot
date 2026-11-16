//! The tick driver: read, decide, execute, write.
use std::cell::RefCell;
use std::collections::BTreeMap;

use wasm_bindgen::prelude::*;

use crate::app::wire;
use crate::app::world;
use fabot_core::atlas::Atlas;
use fabot_core::intents::Refusal;
use fabot_core::layout::shape::Standing;
use fabot_core::observe::metrics::{self, Metrics};
use fabot_core::strategy::intel::Intel;
use fabot_core::tick::{self, Declarations};
use fabot_core::world::{RoomName, Sightings};

const FAULTS_KEPT: usize = 20;
/// A reset loses at most this many ticks of sight.
const INTEL_WRITTEN_EVERY: i32 = 100;

struct Heap {
    /// A module named here is skipped and its last output not replayed.
    faults: Vec<wire::Fault>,
    /// Heap only, so empty after a reset, when each module re-derives it.
    standing: BTreeMap<RoomName, Standing>,
    spare: BTreeMap<RoomName, f64>,
    /// Heap only: after a reset every outpost reads clear, and the first body
    /// back in one that is not sees what stands there and stands it down again.
    sightings: Sightings,
    /// Heap only: after a reset every room floods afresh.
    atlas: Atlas,
    maps: world::Maps,
    /// `None` until its segment is read after a reset.
    intel: Option<Intel>,
    intel_asked: bool,
    /// `None` until its segment is read, with intel's.
    metrics: Option<Metrics>,
}

thread_local! {
    static HOOKED: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
    static HEAP: RefCell<Heap> = RefCell::new(Heap {
        faults: Vec::new(),
        standing: BTreeMap::new(),
        spare: BTreeMap::new(),
        sightings: Sightings::new(),
        atlas: Atlas::empty(),
        maps: world::Maps::default(),
        intel: None,
        intel_asked: false,
        metrics: None,
    });
}

fn record_fault(faults: &mut Vec<wire::Fault>, where_: &str, message: &str) {
    let fault = wire::Fault {
        tick: game_cpu::time(),
        where_: where_.to_string(),
        message: message.to_string(),
    };
    faults.insert(0, fault);
    faults.truncate(FAULTS_KEPT);
}

fn run_tick() -> Result<(), String> {
    let clock = || game_cpu::get_used();
    let mut phases: Vec<(String, f64)> = Vec::new();

    let (sightings, atlas, mut faults, (standing, spare), mut maps, held, asked, metrics) = HEAP
        .with(|heap| {
            let mut heap = heap.borrow_mut();
            (
                std::mem::take(&mut heap.sightings),
                std::mem::replace(&mut heap.atlas, Atlas::empty()),
                std::mem::take(&mut heap.faults),
                (
                    std::mem::take(&mut heap.standing),
                    std::mem::take(&mut heap.spare),
                ),
                std::mem::take(&mut heap.maps),
                heap.intel.take(),
                std::mem::replace(&mut heap.intel_asked, true),
                heap.metrics.take(),
            )
        });
    let mut intel = held.or_else(|| wire::read_intel(asked));
    let metrics = metrics.or_else(|| asked.then(wire::read_metrics));

    let start = clock();
    let mut memory = wire::read_memory();
    let mut state = memory.state();
    state.layout = standing;
    state.spare = spare;
    phases.push(("memory".to_string(), clock() - start));

    let declarations: Declarations = memory
        .declarations()
        .besieging(intel.as_ref().unwrap_or(&Intel::new()));
    let start = clock();
    let drain = (state.engagements.iter())
        .find(|e| e.phase == fabot_core::fight::shape::Phase::Draining)
        .map(|e| e.room);
    let (mut world, pass) = world::of_game(&mut maps, &declarations, drain);
    (world.rooms.iter_mut()).for_each(|r| r.terrain = maps.terrain(r.name));
    let mut unread = Intel::new();
    let rows = intel.as_mut().unwrap_or(&mut unread);
    let decided = tick::decide(
        (clock, start),
        &mut phases,
        (&world, wire::tracing()),
        (declarations, state),
        (sightings, atlas),
        rows,
        (&|room| maps.terrain(room), maps.terrains()),
    );

    for report in &decided.result.reports {
        if let tick::Outcome::Faulted(message) = &report.outcome {
            record_fault(&mut faults, report.module, message);
        }
    }

    let start = clock();
    let refusals: Vec<Refusal> = executor::run(&decided.result.intents);
    let execute_cpu = clock() - start;

    let start = clock();
    wire::write_state(&mut memory, &decided.result.state);
    if let Some(rows) = &intel
        && world.time % INTEL_WRITTEN_EVERY == 0
    {
        wire::write_intel(rows);
    }
    let write_cpu = clock() - start;

    phases.push(("execute".to_string(), execute_cpu));
    phases.push(("write".to_string(), write_cpu));

    if memory.recording() {
        let start = clock();
        let rows = intel.as_ref().unwrap_or(&unread);
        if let Some(fault) = wire::record(&world, &pass, &memory.text, rows) {
            record_fault(&mut faults, "record", &fault);
        }
        phases.push(("record".to_string(), clock() - start));
    }

    let counts = metrics::counts(&world, &decided);
    let start = clock();
    let metrics = metrics.map(|m| {
        let mut m = metrics::measure(m, &world, &decided, &refusals, &phases);
        if world.time % metrics::WINDOW == 0 {
            wire::write_segment(wire::METRICS_SEGMENT, m.text());
        }
        m
    });
    phases.push(("metrics".to_string(), clock() - start));
    let tick::Decided {
        mut result,
        sightings,
        atlas,
        ..
    } = decided;

    let observed = wire::Observed {
        tick: world.time,
        version: wire::version(),
        bucket: world.bucket,
        total: clock(),
        phases,
        counts,
        off: result.state.off.iter().cloned().collect(),
        faults: faults.clone(),
        refusals,
        offers: result
            .state
            .engagements
            .iter()
            .filter(|e| !e.plan.offers.is_empty())
            .map(|e| (e.id, e.plan.offers.clone()))
            .collect(),
        alerts: metrics.as_ref().map_or_default(Metrics::alerts),
    };
    wire::write_observe(&observed);
    let standing = std::mem::take(&mut result.state.layout);

    HEAP.with(|heap| {
        let mut heap = heap.borrow_mut();
        heap.faults = faults;
        heap.standing = standing;
        heap.spare = std::mem::take(&mut result.state.spare);
        heap.sightings = sightings;
        heap.atlas = atlas;
        heap.maps = maps;
        heap.intel = intel;
        heap.metrics = metrics;
    });

    Ok(())
}

fn faulted_by(faults: &mut Vec<wire::Fault>, error: &str) {
    let tick = game_cpu::time();
    record_fault(faults, "tick", error);
    let observed = wire::Observed {
        tick,
        version: wire::version(),
        bucket: game_cpu::bucket(),
        total: game_cpu::get_used(),
        phases: Vec::new(),
        counts: Vec::new(),
        off: Vec::new(),
        faults: faults.clone(),
        refusals: Vec::new(),
        offers: Vec::new(),
        alerts: Vec::new(),
    };
    wire::write_observe(&observed);
}

/// Exported as `loop` on the bundled `main` module, which the engine calls.
#[wasm_bindgen(js_name = loop)]
pub fn tick() {
    HOOKED.with(|hooked| {
        if !hooked.replace(true) {
            super::hook_panics();
        }
    });

    if let Err(error) = run_tick() {
        HEAP.with(|heap| {
            let mut heap = heap.borrow_mut();
            faulted_by(&mut heap.faults, &error);
        });
        wasm_bindgen::throw_str(&error);
    }
}

mod game_cpu {
    pub use screeps::game::cpu::{bucket, get_used};

    pub fn time() -> i32 {
        screeps::game::time() as i32
    }
}

mod executor {
    pub use crate::app::executor::*;
}
