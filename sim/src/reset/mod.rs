//! The heap a driver keeps, lost every `--reset-every N` ticks as `src/app/driver.rs`
//! meets a global reset: Memory, world and segments 2-3 (active, so read then) kept.
use fabot_base::intel::{self, Intel};
use fabot_core::drive;
use fabot_observe::metrics::{self, Metrics};

#[derive(Default)]
pub struct Twin {
    pub heap: drive::Heap,
    pub intel: Intel,
    pub metrics: Metrics,
    every: u64,
    segments: (String, String),
}

impl Twin {
    pub fn new(every: u64, check: bool) -> Twin {
        let mut kept = Twin {
            every,
            ..Twin::default()
        };
        kept.heap.atlas.check = check;
        kept
    }

    pub fn decide(&mut self, t: u64) -> (&mut drive::Heap, &mut Intel, Option<&mut Metrics>) {
        if t > 0 && t.is_multiple_of(self.every) {
            let check = self.heap.atlas.check;
            self.heap = drive::Heap::default();
            self.heap.atlas.check = check;
            self.intel = intel::of_text(&self.segments.0);
            self.metrics = Metrics::of_text(&self.segments.1);
        }
        (&mut self.heap, &mut self.intel, Some(&mut self.metrics))
    }

    pub fn written(&mut self, time: i32) -> &Metrics {
        if self.every > 0 {
            if time % intel::WRITTEN_EVERY == 0 {
                self.segments.0 = intel::to_text(&self.intel);
            }
            if time % metrics::WINDOW == 0 {
                self.segments.1 = self.metrics.text();
            }
        }
        &self.metrics
    }
}
