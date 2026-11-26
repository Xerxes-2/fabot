//! The players our bot plays in one world (`--players`): each its own user,
//! `Driver` and outputs.
use std::{
    fs::{self, File},
    io::{BufWriter, Write},
};

use serde_json::{Value, json};

use crate::{
    driver::{Driver, Maps},
    engaged, reset,
    sample::Sampler,
    view::View,
    world,
};

/// A player our bot plays: its user, first room, and Memory's text at the start.
pub struct Seat {
    pub uid: String,
    pub name: String,
    pub room: String,
    pub memory: String,
}

/// `--players`' entries `{name, room?, declarations?, tuning?}`, else one in `room` with the world's Memory.
pub fn seats(
    players: Option<Vec<Value>>,
    room: &str,
    homes: impl FnOnce(&[Option<String>]) -> Option<Vec<String>>,
    memory: &str,
) -> Vec<Seat> {
    let players = players.unwrap_or_else(|| vec![json!({ "name": world::USERNAME, "room": room })]);
    let at = |p: &Value| p["room"].as_str().filter(|r| !r.is_empty()).map(Into::into);
    let wanted: Vec<_> = players.iter().map(at).collect();
    let named = |p: &Value| {
        at(p).unwrap_or_else(|| panic!("{}: a seat names its room on --world", p["name"]))
    };
    let rooms = homes(&wanted).unwrap_or_else(|| players.iter().map(named).collect());
    (players.iter().zip(rooms).enumerate())
        .map(|(i, (p, room))| Seat {
            // The first is `US`, and u2 the duel's `RIVAL`.
            uid: format!("u{}", 2 * i + 1),
            name: p["name"].as_str().expect("a seat's name").into(),
            room,
            memory: match (&p["declarations"], p.get("tuning")) {
                (Value::Null, None) if i == 0 => memory.to_string(),
                (Value::Null, Some(t)) if i == 0 => {
                    let mut m: Value = serde_json::from_str(memory).unwrap_or_default();
                    m["v2"]["tuning"] = t.clone();
                    m.to_string()
                }
                (Value::Null, None) => String::new(),
                (d, t) => json!({ "v2": { "declarations": d, "tuning": t } }).to_string(),
            },
        })
        .collect()
}

pub struct Player<W: Write> {
    pub seat: Seat,
    pub driver: Driver<W>,
    pub dir: String,
    pub sampler: Sampler,
    pub samples: W,
    fight: W,
    engaged: bool,
}

/// Each seat's player, writing to `out` for the first and `<out>/<name>` for the rest;
/// its samples follow `follow` beside the outposts and claims its Memory declares.
pub fn players(
    seats: Vec<Seat>,
    out: &str,
    follow: &[String],
    maps: impl Fn() -> Maps,
    kept: impl Fn() -> reset::Twin,
) -> Vec<Player<BufWriter<File>>> {
    let players = seats.into_iter().enumerate().map(|(i, seat)| {
        let dir = match i {
            0 => out.to_string(),
            _ => format!("{out}/{}", seat.name),
        };
        fs::create_dir_all(&dir).expect("the output directory");
        let file = |name: &str| {
            BufWriter::new(File::create(format!("{dir}/{name}")).expect("an output file"))
        };
        let files = ["faults.jsonl", "cpu.jsonl", "metrics.jsonl"].map(file);
        let mut driver = Driver::new(maps(), seat.memory.clone(), files);
        driver.kept = kept();
        let memory: Value = serde_json::from_str(&seat.memory).unwrap_or_default();
        let declared = |key: &str| -> Vec<String> {
            let sites = (memory["v2"]["declarations"][key].as_array()).into_iter().flatten();
            (sites.filter_map(|c| Some(c["room"].as_str()?.into()))).collect()
        };
        let followed = [declared("outposts"), follow.to_vec()].concat();
        Player {
            sampler: Sampler::new(&seat.room, &followed, declared("claims")),
            samples: file("samples.jsonl"),
            fight: file("fight.jsonl"),
            seat,
            driver,
            dir,
            engaged: false,
        }
    });
    players.collect()
}

impl<W: Write> Player<W> {
    pub fn engaged(&mut self, v: &View, t: u64) {
        if let Some(row) = engaged::row(v, &self.driver.memory, t, &mut self.engaged) {
            let _ = writeln!(self.fight, "{row}");
        }
    }
}
