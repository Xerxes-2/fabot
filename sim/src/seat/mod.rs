//! The players our bot plays in one world (`--players`): each its own user,
//! `Driver` and outputs.
use std::fs::{self, File};
use std::io::{BufWriter, Write};

use krebs::atom;
use krebs::doc::Obj;
use serde_json::{Value, json};

use crate::driver::{Driver, Maps};
use crate::view::{View, num, text};
use crate::{engaged, reset, world};

/// A player our bot plays: its user, first room, and Memory's text at the start.
pub struct Seat {
    pub uid: String,
    pub name: String,
    pub room: String,
    pub memory: String,
}

/// `--players`' entries `{name, room?, declarations?}`, else one in `room` with the world's Memory.
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
            memory: match &p["declarations"] {
                Value::Null if i == 0 => memory.to_string(),
                Value::Null => String::new(),
                d => json!({ "v2": { "declarations": d } }).to_string(),
            },
        })
        .collect()
}

pub struct Player<W: Write> {
    pub seat: Seat,
    pub driver: Driver<W>,
    pub dir: String,
    fight: W,
    engaged: bool,
}

/// Each seat's player, writing to `out` for the first and `<out>/<name>` for the rest.
pub fn players(
    seats: Vec<Seat>,
    out: &str,
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
        let files = [
            file("faults.jsonl"),
            file("cpu.jsonl"),
            file("metrics.jsonl"),
        ];
        let mut driver = Driver::new(maps(), seat.memory.clone(), files);
        driver.kept = kept();
        let fight = file("fight.jsonl");
        Player {
            seat,
            driver,
            dir,
            fight,
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

    /// Its controllers' levels and progress and its creeps.
    pub fn standing(&self, v: &View) -> String {
        let user = |o: &&Obj| text(o, atom!("user")) == self.seat.uid;
        let of = |kind| v.rooms.iter().flat_map(move |r| r.of(kind)).filter(user);
        let rcl = |o: &Obj| [num(o, atom!("level")), num(o, atom!("progress"))];
        let levels: Vec<_> = of("controller").map(rcl).collect();
        let creeps = of("creep").count();
        let Seat { name, room, .. } = &self.seat;
        format!("{name} in {room}: rcl, progress {levels:?} creeps {creeps}")
    }
}
