use js_sys::JsString;
use screeps::raw_memory;
use serde::{Serialize, Serializer};
use wasm_bindgen::{JsCast, JsValue};

use super::js::field;
use crate::core::memory::Memory;
use crate::core::observe::metrics::Metrics;
use crate::core::state::State;
use crate::core::strategy::intel::{self, Intel};
use crate::core::tactics::{self, Offer};
use crate::core::world::World;

/// Read by `npm run observe` and `scripts/capture`.
pub const OBSERVE_SEGMENT: u8 = 1;
const INTEL_SEGMENT: u8 = 2;
pub const METRICS_SEGMENT: u8 = 3;

pub fn read_memory() -> Memory {
    Memory::of_text(String::from(raw_memory::get()))
}

pub fn write_state(memory: &mut Memory, state: &State) {
    if let Some(text) = memory.written(state) {
        raw_memory::set(&JsString::from(text));
    }
}

/// Only the xxscreeps harness's wrapper sets `fabotTrace`: live, nothing is
/// traced.
pub fn tracing() -> bool {
    field(&js_sys::global(), "fabotTrace").as_bool() == Some(true)
}

pub fn trace(line: &str) {
    let console = field(&js_sys::global(), "console");
    if let Some(log) = field(&console, "log").dyn_ref::<js_sys::Function>() {
        let _ = log.call1(&console, &JsValue::from_str(&format!("#trace {line}")));
    }
}

#[derive(Clone, Debug, Serialize)]
pub struct Fault {
    pub tick: i32,
    #[serde(rename = "where")]
    pub where_: String,
    pub message: String,
}

#[derive(Clone, Debug, Serialize)]
pub struct Observed {
    pub tick: i32,
    pub version: String,
    pub bucket: i32,
    #[serde(rename = "cpu")]
    pub total: f64,
    pub phases: Vec<(String, f64)>,
    pub counts: Vec<(String, i32)>,
    pub off: Vec<String>,
    /// Each engagement priced this tick, its candidates' offers beside it (§6.3).
    #[serde(serialize_with = "offer_rows")]
    pub offers: Vec<(i32, Vec<Offer>)>,
    pub faults: Vec<Fault>,
    pub refusals: Vec<crate::core::intents::Refusal>,
    pub alerts: Vec<String>,
}

/// An offer as `npm run observe -- engagements` reads it: `[name, value, cost,
/// bodies]`, the numbers the operator compares.
fn offer_rows<S: Serializer>(offers: &[(i32, Vec<Offer>)], s: S) -> Result<S::Ok, S::Error> {
    s.collect_seq(offers.iter().map(|(id, list)| {
        let rows: Vec<_> = (list.iter())
            .map(|o| (tactics::name(o.tactic), o.value, o.cost, o.bodies))
            .collect();
        (id, rows)
    }))
}

pub fn write_observe(o: &Observed) {
    let text = serde_json::to_string(o).unwrap_or_else(|_| "{}".to_string());
    write_segment(OBSERVE_SEGMENT, text);
}

/// The engine keeps the active list across uploads and counts every loaded
/// segment against the ten saved per tick, so a cold heap replaces it.
pub fn read_intel(asked: bool) -> Option<Intel> {
    if !asked {
        raw_memory::set_active_segments(&[INTEL_SEGMENT, METRICS_SEGMENT]);
        return None;
    }
    let text = raw_memory::segments().get(INTEL_SEGMENT);
    Some(text.map_or_else(Intel::new, |text| intel::of_text(&text)))
}

/// Read once after a reset, the tick after `read_intel` asked for it.
pub fn read_metrics() -> Metrics {
    let text = raw_memory::segments().get(METRICS_SEGMENT);
    text.map_or_else(Metrics::default, |text| Metrics::of_text(&text))
}

pub fn write_intel(rows: &Intel) {
    write_segment(INTEL_SEGMENT, intel::to_text(rows));
}

pub fn write_segment(id: u8, text: impl Into<String>) {
    let segments = raw_memory::segments();
    segments.set(id, text.into());
}

pub fn record(
    world: &World,
    pass: &super::world::Pass,
    memory: &str,
    rows: &Intel,
) -> Option<String> {
    super::record::record(
        world.time,
        world.cpu_limit,
        pass,
        memory,
        &intel::to_text(rows),
    )
}

/// The bundle's banner, set by `scripts/rust.mjs` on the global.
pub fn version() -> String {
    js_sys::Reflect::get(&js_sys::global(), &JsValue::from_str("__FABOT_VERSION__"))
        .ok()
        .and_then(|v| v.as_string())
        .unwrap_or_else(|| "unbuilt".to_string())
}
