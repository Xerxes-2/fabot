//! `sim --gen <seed>`: a world the official map generator lays out (krebs's port of
//! map.js) in place of a `worlds/` JSON, our colony on a room the seed picks.
use std::collections::BTreeMap;

use krebs::doc::Val;
use krebs::hooks::EngineHooks;
use krebs::mapgen::{self, RoomKind, WorldSpec};
use krebs_util::pool::Pool;
use serde_json::{Value, json};

use crate::world::Map;

/// `--gen <seed>` and `--sectors WxH`: whole sectors from E0S0, one by default.
pub struct Spec {
    seed: u64,
    sectors: (i32, i32),
}

impl Spec {
    pub fn of(flags: &BTreeMap<String, String>) -> Option<Spec> {
        let usage = "--gen <seed> [--sectors WxH]";
        let seed = flags.get("--gen")?.parse().expect(usage);
        let sectors = flags.get("--sectors").map_or((1, 1), |s| {
            let (w, h) = s.split_once('x').expect(usage);
            (w.parse().expect(usage), h.parse().expect(usage))
        });
        Some(Spec { seed, sectors })
    }

    pub fn world(&self) -> (Map, String) {
        let spec = WorldSpec {
            sectors_x: 0..self.sectors.0,
            sectors_y: 0..self.sectors.1,
            seed: self.seed,
            wide_coords: false,
            open: true,
            portals: None,
        };
        let rooms = mapgen::generate_rooms(&spec, &Pool::new(1), &EngineHooks::default());
        let mut map = Map::default();
        for room in rooms {
            map.rooms
                .push((room.xy.to_string(), room.terrain.to_db_string()));
            for o in room.objects {
                let mut o: Value =
                    serde_json::from_str(&Val::Obj(o).to_json().stringify()).expect("a doc");
                o["_id"] = json!(format!("{:015x}", map.objects.len() + 1));
                map.objects.push(o);
            }
        }
        let room = colony(&map, self.seed);
        (map, room)
    }
}

/// The room `seed` picks among those with a controller and two sources (`colony.mjs`'s
/// rule) whose first spawn `Map::seat` can plan, the tile `Map::stand` seats it on.
fn colony(map: &Map, seed: u64) -> String {
    let sources = |room: &str| {
        map.objects
            .iter()
            .filter(|o| o["room"] == room && o["type"] == "source")
            .count()
    };
    let normal = |room: &str| {
        room.parse()
            .is_ok_and(|xy| mapgen::room_kind(xy) == RoomKind::Normal)
    };
    let mut fit: Vec<&str> = (map.rooms.iter().map(|(n, _)| n.as_str()))
        .filter(|r| normal(r) && sources(r) == 2)
        .collect();
    fit.sort_unstable();
    let first = (seed.wrapping_mul(0x9e37_79b9_7f4a_7c15) >> 32) as usize % fit.len();
    let order = fit[first..].iter().chain(&fit[..first]);
    order
        .copied()
        .find(|r| map.seat(r).is_some())
        .expect("a room with a controller, two sources and a spawn tile")
        .to_string()
}
