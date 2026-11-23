use js_sys::JsString;
use screeps::raw_memory;
use serde::{Serialize, Serializer};
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::{JsCast, JsValue};

use fabot_base::intel::{self, Intel};
use fabot_core::memory::Memory;
use fabot_observe::engagements::Log;
use fabot_observe::metrics::Metrics;
use fabot_tactics::Offer;

/// Read by `npm run observe` and `scripts/capture`.
pub const OBSERVE_SEGMENT: u8 = 1;
const INTEL_SEGMENT: u8 = 2;
pub const METRICS_SEGMENT: u8 = 3;
pub const ENGAGEMENTS_SEGMENT: u8 = 4;

pub fn read_memory() -> Memory {
    Memory::of_text(String::from(raw_memory::get()))
}

pub fn write_memory(text: Option<String>) {
    if let Some(text) = text {
        raw_memory::set(&JsString::from(text));
    }
}

/// Only the xxscreeps harness's wrapper sets `fabotTrace`: live, nothing is
/// traced.
pub fn tracing() -> bool {
    global().fabot_trace().as_bool() == Some(true)
}

/// `replay --check`: what is kept across ticks is computed afresh beside it.
pub fn checking() -> bool {
    global().fabot_check().as_bool() == Some(true)
}

pub fn trace(line: &str) {
    let _ = console_log(&format!("#trace {line}"));
}

#[wasm_bindgen]
extern "C" {
    type Global;
    #[wasm_bindgen(method, getter, js_name = fabotTrace)]
    fn fabot_trace(this: &Global) -> JsValue;
    #[wasm_bindgen(method, getter, js_name = __fabotCheck)]
    fn fabot_check(this: &Global) -> JsValue;
    #[wasm_bindgen(method, getter, js_name = __FABOT_VERSION__)]
    fn banner(this: &Global) -> Option<String>;

    #[wasm_bindgen(js_namespace = console, js_name = log, catch)]
    fn console_log(line: &str) -> Result<(), JsValue>;
    #[wasm_bindgen(js_namespace = console, js_name = error, catch)]
    pub fn console_error(message: &str, error: &JsValue) -> Result<(), JsValue>;
}

fn global() -> Global {
    js_sys::global().unchecked_into()
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
    pub refusals: Vec<fabot_base::intents::Refusal>,
    pub alerts: Vec<String>,
}

/// An offer as `npm run observe -- engagements` reads it: `[name, value, cost,
/// bodies]`, the numbers the operator compares.
fn offer_rows<S: Serializer>(offers: &[(i32, Vec<Offer>)], s: S) -> Result<S::Ok, S::Error> {
    s.collect_seq(offers.iter().map(|(id, list)| {
        let rows: Vec<_> = (list.iter())
            .map(|o| (o.tactic, o.value, o.cost, o.bodies))
            .collect();
        (id, rows)
    }))
}

pub fn write_observe(o: &Observed) {
    let text = serde_json::to_string(o).unwrap_or_else(|_| "{}".to_string());
    write_segment(OBSERVE_SEGMENT, text);
}

/// The engine keeps the active list on the user record across resets and
/// uploads, so a cold heap reads what it loaded and replaces the list: every
/// loaded segment counts against the ten saved per tick.
pub fn read_intel(asked: bool) -> Option<Intel> {
    if !asked {
        raw_memory::set_active_segments(&[INTEL_SEGMENT, METRICS_SEGMENT, ENGAGEMENTS_SEGMENT]);
    }
    let text = raw_memory::segments().get(INTEL_SEGMENT);
    (asked || text.is_some()).then(|| text.map_or_else(Intel::new, |text| intel::of_text(&text)))
}

/// Read once after a reset, with intel.
pub fn read_metrics() -> Metrics {
    let text = raw_memory::segments().get(METRICS_SEGMENT);
    text.map_or_else(Metrics::default, |text| Metrics::of_text(&text))
}

pub fn read_log() -> Log {
    let text = raw_memory::segments().get(ENGAGEMENTS_SEGMENT);
    text.map_or_else(Log::default, |text| Log::of_text(&text))
}

pub fn write_intel(rows: &Intel) {
    write_segment(INTEL_SEGMENT, intel::to_text(rows));
}

pub fn write_segment(id: u8, text: impl Into<String>) {
    let segments = raw_memory::segments();
    segments.set(id, text.into());
}

/// The bundle's banner, set by `scripts/rust.mjs` on the global.
pub fn version() -> String {
    global().banner().unwrap_or_else(|| "unbuilt".to_string())
}
