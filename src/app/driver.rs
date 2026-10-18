//! The tick driver: read, decide, execute, write.
use std::cell::RefCell;
use std::collections::BTreeMap;

use wasm_bindgen::prelude::*;

use crate::app::executor::Refusal;
use crate::app::wire;
use crate::app::world;
use crate::core::atlas::Atlas;
use crate::core::layout::shape::Standing;
use crate::core::strategy::intel::{self, Intel};
use crate::core::tick::{self, Context, Declarations};
use crate::core::world::Sightings;

const FAULTS_KEPT: usize = 20;
/// A reset loses at most this many ticks of sight.
const INTEL_WRITTEN_EVERY: i32 = 100;

struct Heap {
    /// A module named here is skipped and its last output not replayed.
    faults: Vec<wire::Fault>,
    /// Heap only, so empty after a reset, when each module re-derives it.
    standing: BTreeMap<String, Standing>,
    /// Heap only: after a reset every outpost reads clear, and the first body
    /// back in one that is not sees what stands there and stands it down again.
    sightings: Sightings,
    /// Heap only: after a reset every room floods afresh.
    atlas: Atlas,
    /// A room's terrain never changes, so it is read once a room a reset.
    terrain: world::Terrains,
    /// `None` until its segment is read after a reset.
    intel: Option<Intel>,
    intel_asked: bool,
}

thread_local! {
    static HOOKED: std::cell::Cell<bool> = const { std::cell::Cell::new(false) };
    static HEAP: RefCell<Heap> = RefCell::new(Heap {
        faults: Vec::new(),
        standing: BTreeMap::new(),
        sightings: Sightings::new(),
        atlas: Atlas::empty(),
        terrain: world::Terrains::default(),
        intel: None,
        intel_asked: false,
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

    let (mut sightings, mut atlas, mut faults, standing, mut terrain, held, asked) =
        HEAP.with(|heap| {
            let mut heap = heap.borrow_mut();
            (
                std::mem::take(&mut heap.sightings),
                std::mem::replace(&mut heap.atlas, Atlas::empty()),
                std::mem::take(&mut heap.faults),
                std::mem::take(&mut heap.standing),
                std::mem::take(&mut heap.terrain),
                heap.intel.take(),
                std::mem::replace(&mut heap.intel_asked, true),
            )
        });
    let mut intel = held.or_else(|| wire::read_intel(asked));

    let start = clock();
    let mut state = wire::read_state();
    state.layout = standing;
    phases.push(("memory".to_string(), clock() - start));

    let declarations: Declarations = wire::read_declarations();
    let start = clock();
    let (world, pass) = world::of_game(&mut terrain, &declarations);
    sightings = crate::core::world::sightings::step(&world, sightings);
    let mut unread = Intel::new();
    let rows = intel.as_mut().unwrap_or(&mut unread);
    intel::step(&world, rows);
    phases.push(("world".to_string(), clock() - start));

    let start = clock();
    atlas = Atlas::following(&atlas, &world);
    phases.push(("atlas".to_string(), clock() - start));

    let mut context = Context {
        world: &world,
        atlas: &mut atlas,
        declarations: &declarations,
        sightings: &sightings,
        intel: rows,
    };

    let mut result = tick::run(clock, tick::module_list(), &mut context, state);

    for report in &result.reports {
        phases.push((report.module.to_string(), report.cpu));
        if let tick::Outcome::Faulted(message) = &report.outcome {
            record_fault(&mut faults, report.module, message);
        }
    }

    let start = clock();
    let refusals: Vec<Refusal> = executor::run(&result.intents);
    let execute_cpu = clock() - start;

    let start = clock();
    wire::write_state(&result.state);
    if let Some(rows) = &intel
        && world.time % INTEL_WRITTEN_EVERY == 0
    {
        wire::write_intel(rows);
    }
    let write_cpu = clock() - start;

    phases.push(("execute".to_string(), execute_cpu));
    phases.push(("write".to_string(), write_cpu));

    if wire::recording() {
        let start = clock();
        if let Some(fault) = wire::record(&world, &pass, intel.as_ref().unwrap_or(&unread)) {
            record_fault(&mut faults, "record", &fault);
        }
        phases.push(("record".to_string(), clock() - start));
    }

    let counts = vec![
        ("atlasHits".to_string(), atlas.hits()),
        ("atlasMisses".to_string(), atlas.misses()),
    ];
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
    };
    wire::write_observe(&observed);
    let standing = std::mem::take(&mut result.state.layout);

    HEAP.with(|heap| {
        let mut heap = heap.borrow_mut();
        heap.faults = faults;
        heap.standing = standing;
        heap.sightings = sightings;
        heap.atlas = atlas;
        heap.terrain = terrain;
        heap.intel = intel;
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
    };
    wire::write_observe(&observed);
}

/// Exported as `loop` on the bundled `main` module, which the engine calls.
#[wasm_bindgen(js_name = loop)]
pub fn tick() {
    HOOKED.with(|hooked| {
        if !hooked.replace(true) {
            super::hook_panics();
            super::js::warm(&[
                "Game",
                "act",
                "actor",
                "assignments",
                "body",
                "boost",
                "bucket",
                "budget",
                "capacity",
                "casts",
                "code",
                "constructionSites",
                "controller",
                "cooldown",
                "counts",
                "cpu",
                "creep",
                "creeps",
                "dead",
                "deathTime",
                "declarations",
                "effect",
                "effects",
                "engagement",
                "engagements",
                "fabot",
                "fatigue",
                "faults",
                "fight",
                "getObjectById",
                "getRawBuffer",
                "getRoomTerrain",
                "group",
                "groups",
                "hits",
                "hitsMax",
                "id",
                "isPublic",
                "kind",
                "level",
                "lost",
                "map",
                "members",
                "message",
                "my",
                "name",
                "off",
                "opened",
                "order",
                "ours",
                "outposts",
                "owner",
                "phase",
                "phases",
                "pos",
                "predicted",
                "priority",
                "recorder",
                "refusals",
                "reserve",
                "room",
                "roomName",
                "rooms",
                "seen",
                "since",
                "sources",
                "store",
                "structureType",
                "switches",
                "tactic",
                "target",
                "tick",
                "ticks",
                "ticksRemaining",
                "ticksToDeploy",
                "ticksToLive",
                "type",
                "unanswered",
                "username",
                "version",
                "where",
                "x",
                "y",
            ]);
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
