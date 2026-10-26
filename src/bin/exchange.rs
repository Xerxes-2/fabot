//! The generated set's predictions (`ARCHITECTURE.md` §10.1, `npm run exchange`).
//! Reads samples on stdin as JSON
//! `[{ id, room, terrain: { weight, runOff }, ours: side, theirs: side }]`, a
//! side `{ bodies: [{ parts, boosts, hits, x, y, invader? }], towers: [{ x, y, energy }],
//! ramparts: [{ x, y, hits }] }` (parts head first by engine name, boosts by
//! compound or ""), and writes `[{ id, kind, ticks, dead, hits }]`.
use std::io::Read;

use fabot::core::exchange::{self, Losses, Outcome, Side, Stance, Terrain, Tower, fighter};
use fabot::core::vocabulary::{boost, part_of_name};
use fabot::core::world::{Pos, RoomName};
use serde_json::{Value, json};

fn int(e: &Value, key: &str) -> i32 {
    e[key].as_i64().unwrap() as i32
}

fn each<'a>(e: &'a Value, key: &str) -> &'a Vec<Value> {
    e[key].as_array().unwrap()
}

fn predict(sample: &Value) -> Value {
    let room = RoomName::named(sample["room"].as_str().unwrap());
    let pos = |e: &Value| Pos::new(room, int(e, "x"), int(e, "y"));
    let side = |e: &Value| Side {
        bodies: each(e, "bodies")
            .iter()
            .map(|b| {
                let parts: Vec<_> = each(b, "parts")
                    .iter()
                    .map(|p| part_of_name(p.as_str().unwrap()).unwrap())
                    .collect();
                let boosts: Vec<_> = parts
                    .iter()
                    .zip(each(b, "boosts"))
                    .map(|(&part, c)| boost::of_compound(part, c.as_str().unwrap()))
                    .collect();
                let mut fighter = fighter::of_parts(&parts, &boosts, int(b, "hits"), Some(pos(b)));
                fighter.invader = b["invader"].as_bool().unwrap_or(false);
                fighter
            })
            .collect(),
        ramparts: each(e, "ramparts")
            .iter()
            .map(|r| (pos(r), int(r, "hits")))
            .collect(),
        towers: each(e, "towers")
            .iter()
            .map(|t| Tower {
                pos: pos(t),
                energy: int(t, "energy"),
            })
            .collect(),
    };
    let terrain = &sample["terrain"];
    let none = Losses { dead: 0, hits: 0 };
    let (kind, ticks, losses) = match exchange::outcome(
        Stance::Standoff,
        &side(&sample["ours"]),
        &side(&sample["theirs"]),
        Terrain {
            weight: int(terrain, "weight"),
            run_off: int(terrain, "runOff"),
        },
    ) {
        Outcome::Win(ticks, losses) => ("win", ticks, losses),
        Outcome::Kite(ticks) => ("kite", ticks, none),
        Outcome::Lose => ("lose", 0, none),
    };
    json!({ "id": sample["id"], "kind": kind, "ticks": ticks, "dead": losses.dead, "hits": losses.hits })
}

fn main() {
    let mut text = String::new();
    std::io::stdin().read_to_string(&mut text).unwrap();
    let samples: Value = serde_json::from_str(&text).unwrap();
    let out: Vec<Value> = samples.as_array().unwrap().iter().map(predict).collect();
    println!("{}", Value::Array(out));
}
