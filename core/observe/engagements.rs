//! `engagements.log` (`ARCHITECTURE.md` §4, §10.1): each engagement as
//! it closed, a JSON row a line, as `scripts/sim/prediction.mjs` reads one.
use std::collections::{BTreeSet, VecDeque};

use serde::Serialize;

use fabot_base::engine;
use fabot_base::world::RoomName;
use fabot_exchange::Outcome;
use fabot_fight::shape::{Closed, Phase};
use fabot_tactics::Tactic;

const ROWS: usize = 200;
const BYTES: usize = engine::SEGMENT_BYTES;

#[derive(Serialize)]
#[serde(rename_all = "camelCase")]
struct Row<'a> {
    id: i32,
    kind: &'static str,
    room: RoomName,
    tactic: Tactic,
    rivals: &'a BTreeSet<String>,
    opened: i32,
    contact: Option<i32>,
    seen: Option<i32>,
    closed: i32,
    #[serde(skip_serializing_if = "Option::is_none")]
    predicted: Option<&'a Outcome>,
    dead: i32,
    energy: i32,
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    disengaged: bool,
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    gave_up: bool,
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    left: bool,
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    safe: bool,
}

/// The rows of the engagements Fight closed at `time`.
pub fn rows(closed: &[Closed], time: i32) -> Vec<String> {
    (closed.iter())
        .filter_map(|c| {
            let e = &c.engagement;
            let row = Row {
                id: e.id,
                kind: (&e.objective).into(),
                room: e.room,
                tactic: e.plan.tactic,
                rivals: &e.actual.rivals,
                opened: e.opened,
                contact: e.actual.contact,
                seen: e.actual.seen,
                closed: time,
                predicted: e.predicted.as_ref(),
                dead: e.lost + e.actual.left_dead,
                energy: e.actual.energy,
                disengaged: e.phase == Phase::Disengaging,
                gave_up: e.actual.gave_up,
                left: c.left,
                safe: c.safe,
            };
            serde_json::to_string(&row).ok()
        })
        .collect()
}

/// The newest rows, oldest first, as the segment holds them.
#[derive(Debug, Default)]
pub struct Log(VecDeque<String>);

impl Log {
    pub fn of_text(text: &str) -> Log {
        Log(text.lines().map(str::to_string).collect())
    }

    pub fn push(&mut self, rows: Vec<String>) -> String {
        self.0.extend(rows);
        while self.0.len() > ROWS || self.0.iter().map(|r| r.len() + 1).sum::<usize>() > BYTES {
            // Owner: the log is read for Exchange's error, so priced rows outlast the rest.
            let unpriced = self.0.iter().position(|r| !r.contains(r#""predicted":"#));
            self.0.remove(unpriced.unwrap_or(0));
        }
        self.0.make_contiguous().join("\n")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn the_log_keeps_the_newest_rows_within_a_segment() {
        let mut log = Log::default();
        let rows = |n: usize, len: usize| (0..n).map(|i| format!("{i:0len$}")).collect::<Vec<_>>();
        let text = log.push(rows(ROWS + 5, 10));
        assert_eq!(Log::of_text(&text).0.len(), ROWS);
        assert_eq!(text.lines().next(), Some("0000000005"));
        let text = log.push(rows(ROWS, BYTES / 50));
        assert!(text.len() <= BYTES);
    }

    #[test]
    fn a_full_log_drops_its_oldest_unpriced_row_before_any_priced_one() {
        let row = |i: usize| match i {
            1 | 3 => format!(r#"{{"id":{i}}}"#),
            _ => format!(r#"{{"id":{i},"predicted":{{}}}}"#),
        };
        let text = Log::default().push((0..ROWS + 2).map(row).collect());
        assert_eq!(
            text.lines().take(3).collect::<Vec<_>>(),
            [row(0), row(2), row(4)]
        );
    }
}
