//! The heap a driver keeps, lost every `--reset-every N` ticks as `src/app/driver.rs`
//! meets a global reset: Memory, segments and world kept.
use fabot_base::intel::{self, Intel};
use fabot_core::drive;
use fabot_observe::metrics::{self, Metrics};

/// `src/app/driver.rs`'s.
const INTEL_WRITTEN_EVERY: i32 = 100;

#[derive(Default, PartialEq)]
enum Since {
    #[default]
    Warm,
    Lost,
    /// The cold tick asked for the segments and ran without them; this one reads them.
    Asked,
}

#[derive(Default)]
pub struct Kept {
    pub heap: drive::Heap,
    pub intel: Intel,
    pub metrics: Metrics,
    every: u64,
    /// Segments 2 and 3 as last written.
    segments: (String, String),
    since: Since,
}

impl Kept {
    pub fn new(every: u64, check: bool) -> Kept {
        let mut kept = Kept {
            every,
            ..Kept::default()
        };
        kept.heap.atlas.check = check;
        kept
    }

    pub fn decide(&mut self, t: u64) -> (&mut drive::Heap, &mut Intel, Option<&mut Metrics>) {
        if t > 0 && t.is_multiple_of(self.every) {
            let check = self.heap.atlas.check;
            self.heap = drive::Heap::default();
            self.heap.atlas.check = check;
            (self.intel, self.metrics) = (Intel::new(), Metrics::default());
            self.since = Since::Lost;
        }
        match self.since {
            Since::Lost => self.since = Since::Asked,
            Since::Asked => {
                self.intel = intel::of_text(&self.segments.0);
                self.metrics = Metrics::of_text(&self.segments.1);
                self.since = Since::Warm;
            }
            Since::Warm => {}
        }
        let metrics = (self.since != Since::Asked).then_some(&mut self.metrics);
        (&mut self.heap, &mut self.intel, metrics)
    }

    pub fn written(&mut self, time: i32) -> &Metrics {
        if self.every > 0 && self.since != Since::Asked {
            if time % INTEL_WRITTEN_EVERY == 0 {
                self.segments.0 = intel::to_text(&self.intel);
            }
            if time % metrics::WINDOW == 0 {
                self.segments.1 = self.metrics.text();
            }
        }
        &self.metrics
    }
}
