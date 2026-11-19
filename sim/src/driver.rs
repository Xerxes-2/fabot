//! `src/app/driver.rs` without the wasm seam: Memory as text, segments as
//! values, the heap (`ARCHITECTURE.md` §4) kept across ticks and never reset.
use std::collections::{BTreeMap, BTreeSet};
use std::io::Write;
use std::time::Instant;

use fabot_core::atlas::tiles;
use fabot_core::drive::{self, Terrains};
use fabot_core::intents::Intent;
use fabot_core::memory::Memory;
use fabot_core::observe::metrics::{self, Metrics};
use fabot_core::strategy::intel::Intel;
use fabot_core::tick::Declarations;
use fabot_core::world::{RoomName, World};
use krebs::intents::RoomIntents;
use screeps::LocalRoomTerrain;
use serde_json::{Value, json};

use crate::view::{View, ms};
use crate::{act, read};

/// Every room's terrain, read once: the world never changes it.
pub struct Maps(
    pub Vec<(RoomName, LocalRoomTerrain)>,
    pub BTreeSet<RoomName>,
);

impl Terrains for Maps {
    fn terrain(&self, room: RoomName) -> Option<&LocalRoomTerrain> {
        self.0.iter().find(|(r, _)| *r == room).map(|(_, t)| t)
    }

    fn terrains(&self) -> Box<dyn Iterator<Item = (RoomName, &[u8; tiles::COUNT])> + '_> {
        Box::new(self.0.iter().map(|(r, t)| (*r, t.get_bits())))
    }
}

pub struct Driver<W: Write> {
    maps: Maps,
    pub memory: String,
    pub heap: drive::Heap,
    intel: Intel,
    metrics: Metrics,
    pub faults: BTreeSet<(&'static str, String)>,
    pub refused: BTreeMap<String, u64>,
    windows_read: i32,
    /// faults.jsonl, cpu.jsonl, metrics.jsonl.
    pub out: [W; 3],
    pub adapter_ms: f64,
    pub decide_ms: f64,
}

impl<W: Write> Driver<W> {
    pub fn new(maps: Maps, memory: String, out: [W; 3]) -> Driver<W> {
        Driver {
            maps,
            memory,
            heap: drive::Heap::default(),
            intel: Intel::new(),
            metrics: Metrics::default(),
            faults: BTreeSet::new(),
            refused: BTreeMap::new(),
            windows_read: i32::MIN,
            out,
            adapter_ms: 0.0,
            decide_ms: 0.0,
        }
    }

    /// One tick of the bot: its intents by room.
    pub fn tick(
        &mut self,
        v: &View,
        t: u64,
        ledger: &dyn Fn() -> Value,
    ) -> Vec<(usize, RoomIntents)> {
        let began = Instant::now();
        let clock = || ms(began);
        let mut memory = Memory::of_text(std::mem::take(&mut self.memory));
        let mut written = None;
        let mut stored = None;
        let maps = &self.maps;
        let read_ms = std::cell::Cell::new(0.0);
        let seam = drive::Seam {
            read: |declarations: &Declarations, _drain| {
                let at = Instant::now();
                let mut world = read::world(v, declarations);
                world.closed = maps.1.clone();
                (world.rooms.iter_mut()).for_each(|r| r.terrain = maps.terrain(r.name));
                read_ms.set(ms(at));
                (world, maps as &dyn Terrains, ())
            },
            execute: |intents: &[Intent]| {
                let (intents, refusals) = act::run(v, intents);
                stored = Some(intents);
                refusals
            },
            write: |_: &World, _: &(), _: &Intel, text, _: &mut Vec<(String, f64)>| written = text,
        };
        let mut phases = Vec::new();
        let ran = drive::run(
            &mut self.heap,
            (&mut memory, &mut self.intel, Some(&mut self.metrics)),
            (&clock, &mut phases, false),
            seam,
        );
        self.memory = written.unwrap_or(memory.text);
        let decided = phases
            .iter()
            .filter(|(p, _)| !matches!(p.as_str(), "execute" | "metrics"));
        let decide_ms = decided.map(|(_, cpu)| cpu).sum::<f64>() - read_ms.get();
        self.decide_ms += decide_ms;
        self.adapter_ms += ms(began) - decide_ms;

        let [faults, cpu, windows] = &mut self.out;
        for (module, message) in ran.faults {
            if self.faults.insert((module, message.clone())) {
                let fault = json!({ "tick": ran.world.time, "where": module, "message": message });
                let _ = writeln!(faults, "{fault}");
            }
        }
        for r in &ran.refusals {
            *self
                .refused
                .entry(format!("{} {}", r.act, r.code))
                .or_default() += 1;
        }
        if ran.world.time % metrics::WINDOW == 0 {
            rows(&self.metrics, t, &mut self.windows_read, &ledger(), windows);
        }
        let row = json!({ "t": t, "time": ran.world.time, "cpu": decide_ms, "phases": phases, "counts": ran.counts });
        let _ = writeln!(cpu, "{row}");
        stored.map_or_else(Vec::new, |s| s.rooms(read::our_idx(v)))
    }
}

/// The metrics windows newer than `after` as `metrics.jsonl` rows (`soak-step.mjs`
/// reads them off segment 3), the newest with the ledger beside it.
fn rows(m: &Metrics, t: u64, after: &mut i32, ledger: &Value, out: &mut impl Write) {
    let fresh: Vec<_> = m.ring.iter().filter(|w| w.start > *after).collect();
    let newest = fresh.iter().map(|w| w.start).max();
    for w in fresh {
        for (row, values) in &w.rows {
            let values: serde_json::Map<String, Value> = (values.iter())
                .filter(|(_, v)| **v != 0)
                .map(|(n, v)| (n.clone(), json!(f64::from(*v))))
                .collect();
            let row = row.map_or("*".to_string(), |r| r.to_string());
            let mut line =
                json!({ "t": t, "start": w.start, "ticks": w.ticks, "row": row, "values": values });
            if Some(w.start) == newest {
                line["ledger"] = ledger.clone();
            }
            let _ = writeln!(out, "{line}");
        }
    }
    *after = newest.unwrap_or(*after).max(*after);
}

#[cfg(test)]
mod tests {
    use std::collections::{BTreeMap, VecDeque};

    use fabot_core::observe::metrics::{Metrics, Window};
    use fabot_core::world::RoomName;
    use serde_json::{Value, json};

    #[test]
    fn a_window_is_written_once_the_newest_with_the_ledger() {
        let row = |room: Option<RoomName>, values: &[(&str, i32)]| {
            let values = values.iter().map(|(n, v)| (n.to_string(), *v)).collect();
            BTreeMap::from([(room, values)])
        };
        let ring = VecDeque::from([
            Window {
                start: 0,
                ticks: 100,
                rows: row(None, &[("held", 100), ("exposure", 0)]),
            },
            Window {
                start: 100,
                ticks: 100,
                rows: row(RoomName::parse("W1N1"), &[("held", 100), ("exposure", 3)]),
            },
        ]);
        let mut m = Metrics::default();
        m.ring = ring;
        let (mut out, mut after) = (Vec::new(), i32::MIN);
        super::rows(&m, 7, &mut after, &json!({ "dug": 1 }), &mut out);
        super::rows(&m, 8, &mut after, &json!({}), &mut out);
        let rows: Vec<Value> = (String::from_utf8(out).unwrap().lines())
            .map(|l| serde_json::from_str(l).unwrap())
            .collect();
        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0]["values"], json!({ "held": 100.0 }));
        assert_eq!(rows[1]["values"], json!({ "held": 100.0, "exposure": 3.0 }));
        assert_eq!(
            (rows[0].get("ledger"), &rows[1]["ledger"]),
            (None, &json!({ "dug": 1 }))
        );
    }
}
