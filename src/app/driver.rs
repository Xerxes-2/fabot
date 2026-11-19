//! The tick driver: read, decide, execute, write.
use std::cell::RefCell;

use wasm_bindgen::prelude::*;

use crate::app::world;
use crate::app::{executor, record, wire};
use fabot_core::drive::{self, Terrains as _};
use fabot_core::observe::metrics::{self, Metrics};
use fabot_core::strategy::intel::{self, Intel};
use fabot_core::tick::Declarations;
use fabot_core::world::{World, read};

const FAULTS_KEPT: usize = 20;
/// A reset loses at most this many ticks of sight.
const INTEL_WRITTEN_EVERY: i32 = 100;

struct Heap {
    /// A module named here is skipped and its last output not replayed.
    faults: Vec<wire::Fault>,
    /// Heap only: after a reset each module re-derives its rows, every
    /// outpost reads clear and every room floods afresh.
    drive: drive::Heap,
    maps: world::Maps,
    buffers: read::Buffers,
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
        drive: drive::Heap::default(),
        maps: world::Maps::default(),
        buffers: read::Buffers::default(),
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

fn run_tick() {
    let clock = || game_cpu::get_used();
    let mut phases: Vec<(String, f64)> = Vec::new();

    let (mut kept, mut faults, mut maps, mut buffers, held, asked, metrics) = HEAP.with(|heap| {
        let mut heap = heap.borrow_mut();
        (
            std::mem::take(&mut heap.drive),
            std::mem::take(&mut heap.faults),
            std::mem::take(&mut heap.maps),
            std::mem::take(&mut heap.buffers),
            heap.intel.take(),
            std::mem::replace(&mut heap.intel_asked, true),
            heap.metrics.take(),
        )
    });
    let mut intel = held.or_else(|| wire::read_intel(asked));
    let mut metrics = metrics.or_else(|| asked.then(wire::read_metrics));

    let start = clock();
    let mut memory = wire::read_memory();
    phases.push(("memory".to_string(), clock() - start));

    let held_intel = intel.is_some();
    let recording = memory.recording().then(|| memory.text.clone());
    let mut unread = Intel::new();
    let mut recorded = None;
    let (read_maps, read_buffers) = (&mut maps, &mut buffers);
    let seam = drive::Seam {
        read: move |declarations: &Declarations, drain| {
            let read_maps = read_maps;
            let (mut world, read) = world::of_game(read_maps, read_buffers, declarations, drain);
            let maps: &world::Maps = read_maps;
            (world.rooms.iter_mut()).for_each(|r| r.terrain = maps.terrain(r.name));
            (world, maps as &dyn drive::Terrains, read)
        },
        execute: executor::run,
        write: |world: &World,
                pass: &world::Pass,
                rows: &Intel,
                text,
                phases: &mut Vec<(String, f64)>| {
            let start = clock();
            wire::write_memory(text);
            if held_intel && world.time % INTEL_WRITTEN_EVERY == 0 {
                wire::write_intel(rows);
            }
            phases.push(("write".to_string(), clock() - start));
            if let Some(text) = &recording {
                let start = clock();
                recorded = record::record(
                    world.time,
                    world.cpu_limit,
                    pass,
                    text,
                    &intel::to_text(rows),
                );
                phases.push(("record".to_string(), clock() - start));
            }
        },
    };
    kept.atlas.check = wire::checking();
    let rows = intel.as_mut().unwrap_or(&mut unread);
    let ran = drive::run(
        &mut kept,
        (&mut memory, rows, metrics.as_mut()),
        (&clock, &mut phases, wire::tracing()),
        seam,
    );

    let drive::Ran {
        world,
        refusals,
        faults: faulted,
        counts,
        off,
        offers,
        traces,
    } = ran;
    for (creep, scored) in traces {
        let tiles: Vec<String> = scored
            .iter()
            .map(|(p, score)| format!("[\"{}\",{},{},{score}]", p.room, p.x, p.y))
            .collect();
        wire::trace(&format!(
            r#"{{"creep":"{creep}","tiles":[{}]}}"#,
            tiles.join(",")
        ));
    }
    for (module, message) in &faulted {
        record_fault(&mut faults, module, message);
    }
    if let Some(fault) = recorded {
        record_fault(&mut faults, "record", &fault);
    }
    if let Some(m) = metrics.as_mut()
        && world.time % metrics::WINDOW == 0
    {
        wire::write_segment(wire::METRICS_SEGMENT, m.text());
    }

    let observed = wire::Observed {
        tick: world.time,
        version: wire::version(),
        bucket: world.bucket,
        total: clock(),
        phases,
        counts,
        off,
        faults: faults.clone(),
        refusals,
        offers,
        alerts: metrics.as_ref().map_or_default(Metrics::alerts),
    };
    wire::write_observe(&observed);
    buffers.keep(world);

    HEAP.with(|heap| {
        let mut heap = heap.borrow_mut();
        heap.faults = faults;
        heap.drive = kept;
        heap.maps = maps;
        heap.buffers = buffers;
        heap.intel = intel;
        heap.metrics = metrics;
    });
}

/// Exported as `loop` on the bundled `main` module, which the engine calls.
#[wasm_bindgen(js_name = loop)]
pub fn tick() {
    HOOKED.with(|hooked| {
        if !hooked.replace(true) {
            super::hook_panics();
        }
    });

    run_tick();
}

mod game_cpu {
    pub use screeps::game::cpu::get_used;

    pub fn time() -> i32 {
        screeps::game::time() as i32
    }
}
