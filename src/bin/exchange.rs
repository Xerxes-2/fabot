//! The generated set's predictions (`ARCHITECTURE.md` §10.1, `npm run exchange`).
//! Reads samples on stdin as JSON
//! `[{ id, room, terrain: { weight, runOff }, ours: side, theirs: side }]`, a
//! side `{ bodies: [{ parts, boosts, hits, x, y, invader? }], towers: [{ x, y, energy }],
//! ramparts: [{ x, y, hits }] }` (parts head first by engine name, boosts by
//! compound or ""), and writes `[{ id, tactic, kind, ticks, dead, hits }]`: the
//! tactic a home's Defend fields and its arm's outcome.
// A tool, not the bot: a panic fails the run that called it.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::io::Read;

use fabot_base::engine;
use fabot_base::vocabulary::Part;
use fabot_base::world::{BodyPart, Creep, Owner, Pos, RoomName, Store};
use fabot_exchange::{self as exchange, Outcome, Side, Terrain, Tower};
use fabot_fight::{Priced, kinds::defend};
use serde_json::{Value, json};

fn int(e: &Value, key: &str) -> i32 {
    e[key].as_i64().unwrap() as i32
}

fn each<'a>(e: &'a Value, key: &str) -> &'a Vec<Value> {
    e[key].as_array().unwrap()
}

fn predict(sample: &Value) -> Value {
    let room = sample["room"]
        .as_str()
        .unwrap()
        .parse::<RoomName>()
        .unwrap();
    let pos = |e: &Value| Pos::new(room, int(e, "x"), int(e, "y"));
    let side = |e: &Value| Side {
        bodies: each(e, "bodies")
            .iter()
            .map(|b| {
                let parts: Vec<_> = each(b, "parts")
                    .iter()
                    .map(|p| p.as_str().unwrap().parse::<Part>().unwrap())
                    .collect();
                let boosts: Vec<_> = each(b, "boosts")
                    .iter()
                    .map(|c| c.as_str().unwrap().parse().ok())
                    .collect();
                // Priced as the bot prices a hostile it sees.
                let creep = Creep {
                    id: "h".into(),
                    name: "h".into(),
                    owner: if b["invader"].as_bool().unwrap_or(false) {
                        Owner::Invader
                    } else {
                        Owner::Stranger("rival".into())
                    },
                    pos: pos(b),
                    body: (parts.iter().zip(&boosts).enumerate())
                        .map(|(i, (kind, boost))| BodyPart {
                            kind: *kind,
                            hits: engine::part_hits_at(parts.len(), int(b, "hits"), i),
                            boost: *boost,
                        })
                        .collect(),
                    hits: int(b, "hits"),
                    hits_max: engine::PART_HITS * parts.len() as i32,
                    ticks_to_live: 1500,
                    fatigue: 0,
                    store: Store::empty(),
                };
                fabot_fight::casting::fighter_of(&creep)
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
        ..Side::default()
    };
    let terrain = &sample["terrain"];
    let terrain = Terrain {
        weight: int(terrain, "weight"),
        run_off: int(terrain, "runOff"),
    };
    let (ours, theirs) = (side(&sample["ours"]), side(&sample["theirs"]));
    let posted = !ours.ramparts.is_empty();
    let set: Vec<Vec<Part>> = ours.bodies.iter().map(|f| f.body.clone()).collect();
    let arms: Vec<Priced> = fabot_fight::arms(posted, &theirs)
        .map(|(stance, together)| {
            let ours = Side {
                together,
                ..ours.clone()
            };
            let outcome = exchange::outcome(stance, &ours, &theirs, terrain);
            let arm = Priced::arm(stance, (&ours, &theirs), terrain, outcome);
            Priced {
                set: set.clone(),
                cast: set.len(),
                ..arm
            }
        })
        .collect();
    let (_, chosen) = fabot_fight::picked(&arms, defend::CANDIDATES, None);
    let (tactic, arm) = chosen.expect("an arm offers");
    let outcome = arm.outcome;
    let (kind, ticks, losses) = match outcome {
        Outcome::Win(ticks, losses) => ("win", ticks, losses),
        Outcome::Kite(ticks) => ("kite", ticks, Default::default()),
        Outcome::Lose => ("lose", 0, Default::default()),
    };
    json!({ "id": sample["id"], "tactic": <&str>::from(tactic), "kind": kind, "ticks": ticks, "dead": losses.dead, "hits": losses.hits })
}

fn main() {
    let mut text = String::new();
    std::io::stdin().read_to_string(&mut text).unwrap();
    let samples: Value = serde_json::from_str(&text).unwrap();
    let out: Vec<Value> = samples.as_array().unwrap().iter().map(predict).collect();
    println!("{}", Value::Array(out));
}
