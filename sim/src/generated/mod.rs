//! `sim --gen <seed>`: a world the official map generator lays out (krebs's port of
//! map.js) in place of a `worlds/` JSON, our colony on a room the seed picks.
mod highway;

use std::collections::{BTreeMap, BTreeSet};

use highway::{Field, Lane};
use krebs::{
    constants::POWER_BANK_RESPAWN_TIME,
    doc::Val,
    hooks::EngineHooks,
    mapgen::{self, BOTTOM, GeneratedRoom, LEFT, RIGHT, RoomKind, TOP, WorldSpec},
    policy,
    room_name::RoomXY,
};
use krebs_util::{pool::Pool, rng::Rng};
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

    pub fn world(&self) -> Map {
        let spec = WorldSpec {
            sectors_x: 0..self.sectors.0,
            sectors_y: 0..self.sectors.1,
            seed: self.seed,
            wide_coords: false,
            open: true,
            portals: None,
        };
        let mut rooms = mapgen::generate_rooms(&spec, &Pool::new(1), &EngineHooks::default());
        self.highways(&mut rooms);
        let mut map = Map::default();
        map.banks = Some(self.seed);
        for room in rooms {
            map.rooms.push((room.xy.to_string(), room.terrain.to_db_string()));
            for o in room.objects {
                let mut o: Value =
                    serde_json::from_str(&Val::Obj(o).to_json().stringify()).expect("a doc");
                o["_id"] = json!(format!("{:015x}", map.objects.len() + 1));
                map.objects.push(o);
            }
        }
        map
    }

    /// An unnamed seat takes the seed's pick as one seat always has, kept `APART` from the others.
    pub fn homes(&self, map: &Map, wanted: &[Option<String>]) -> Vec<String> {
        let free = |r: &str| {
            let controller = |o: &&Value| o["room"] == r && o["type"] == "controller";
            map.objects.iter().filter(controller).any(|o| o["user"].is_null())
        };
        let mut taken: Vec<&str> = wanted.iter().flatten().map(String::as_str).collect();
        for (i, r) in taken.iter().enumerate() {
            let fit = free(r) && !taken[..i].contains(r) && map.seat(r).is_some();
            assert!(fit, "{r}: no free controller and first spawn's tile, or another seat's home");
        }
        let xy = |r: &str| r.parse::<RoomXY>().expect("a room");
        let apart = |a: &str, b: &str| {
            let (a, b) = (xy(a), xy(b));
            (a.x - b.x).abs().max((a.y - b.y).abs()) >= APART
        };
        let order = order(map, self.seed);
        let mut homes = Vec::new();
        for w in wanted {
            let home = w.as_deref().unwrap_or_else(|| {
                let far = |r: &&&str| taken.iter().all(|t| apart(r, t)) && map.seat(r).is_some();
                let home =
                    order.iter().find(far).expect("no plannable room APART from the other homes");
                taken.push(home);
                home
            });
            homes.push(home.to_string());
        }
        homes
    }

    /// map.js draws a highway like any room; the official ones are an open lane between the
    /// sectors' masses. A lane end two highways share is drawn once, for both.
    fn highways(&self, rooms: &mut [GeneratedRoom]) {
        let field = Field(policy::seed(self.seed, "highway", &[]) as u32);
        let highway = |xy: RoomXY| mapgen::room_kind(xy) == RoomKind::Highway;
        let ours: BTreeSet<RoomXY> = rooms.iter().map(|r| r.xy).collect();
        let next = |xy: RoomXY, side: usize| {
            let (dx, dy) = [(0, -1), (1, 0), (0, 1), (-1, 0)][side];
            RoomXY::new(xy.x + dx, xy.y + dy)
        };
        for room in rooms.iter_mut().filter(|r| highway(r.xy)) {
            let exits = [TOP, RIGHT, BOTTOM, LEFT].map(|side| {
                let other = next(room.xy, side);
                let lane = highway(other) && ours.contains(&other) && Lane::of(room.xy).ends(side);
                match (lane, side) {
                    (true, RIGHT | BOTTOM) => field.lane_exit(room.xy, Lane::of(room.xy), side),
                    (true, _) => field.lane_exit(other, Lane::of(other), (side + 2) % 4),
                    (false, _) => mapgen::facing_edge(&room.terrain, (side + 2) % 4),
                }
            });
            let name = policy::fnv1a64(room.xy.to_string().as_bytes());
            let mut rng = Rng::new(policy::seed(self.seed, "highway", &[name]));
            room.terrain = field.terrain(room.xy, &exits, &mut rng);
        }
    }
}

/// `sim gen-rooms --gen <seed> [--sectors WxH]`: the rooms `npm run plan:quality` reads.
pub fn rooms() {
    let args: Vec<String> = std::env::args().skip(2).collect();
    let flags: BTreeMap<_, _> = args.chunks(2).map(|f| (f[0].clone(), f[1].clone())).collect();
    let map = Spec::of(&flags).expect("usage: sim gen-rooms --gen <seed> [--sectors WxH]").world();
    for (name, terrain) in &map.rooms {
        let of = |kind: &'static str| {
            (map.objects.iter()).filter(move |o| o["room"] == name.as_str() && o["type"] == kind)
        };
        let xy = |o: &Value| format!("{},{}", o["x"], o["y"]);
        let sources: Vec<String> = of("source").map(xy).collect();
        let Some(controller) = of("controller").next().filter(|_| !sources.is_empty()) else {
            continue;
        };
        let (sources, mineral) = (sources.join("|"), of("mineral").next().map_or("-".into(), xy));
        println!("{}:{name};{sources};{};{mineral};{terrain}", flags["--gen"], xy(controller));
    }
}

/// A `rooms` doc as an old official world has it: a highway a bus with its sector's deposit
/// type and, in a generated world (`banks`: its seed and tick), a power bank due.
pub fn room_doc(name: &str, closed: bool, banks: Option<(u64, i64)>) -> Value {
    let status = if closed { "out of borders" } else { "normal" };
    let mut doc = json!({ "_id": name, "status": status });
    let Some(xy) = RoomXY::parse_engine(name) else {
        return doc;
    };
    match mapgen::room_kind(xy) {
        RoomKind::Highway => {
            doc["bus"] = json!(true);
            doc["depositType"] = json!(mapgen::deposit_type(xy));
            if let Some((seed, t)) = banks {
                doc["powerBankTime"] = json!(bank_due(seed, xy, t));
            }
        }
        kind => doc["sourceKeepers"] = json!(kind == RoomKind::Keeper),
    }
    doc
}

/// When a world as old as the official one has the bank on `xy` due: the time left of a respawn
/// drawn over 0.75..1.25 of its mean, so flat to 0.75 and thinning to nothing at 1.25.
fn bank_due(seed: u64, xy: RoomXY, t: i64) -> i64 {
    let name = policy::fnv1a64(xy.to_string().as_bytes());
    let u = (policy::seed(seed, "bank-due", &[name]) >> 11) as f64 / (1u64 << 53) as f64;
    let mean = f64::from(POWER_BANK_RESPAWN_TIME);
    let left = if u < 0.75 { mean * u } else { 1.25 * mean - mean * (1.0 - u).sqrt() };
    // `genPowerBanks` reads a due tick of 0 as none set.
    t + 1 + left as i64
}

/// Rooms (on the longer axis) between two homes a seat is picked for: a remote apiece between them.
const APART: i32 = 3;

/// As `npm run soak` picks its default room, from where `seed` falls among them by name.
fn order(map: &Map, seed: u64) -> Vec<&str> {
    let sources = |room: &str| {
        map.objects.iter().filter(|o| o["room"] == room && o["type"] == "source").count()
    };
    let normal =
        |room: &str| room.parse().is_ok_and(|xy| mapgen::room_kind(xy) == RoomKind::Normal);
    let mut fit: Vec<&str> = (map.rooms.iter().map(|(n, _)| n.as_str()))
        .filter(|r| normal(r) && sources(r) == 2)
        .collect();
    fit.sort_unstable();
    let first = (seed.wrapping_mul(0x9e37_79b9_7f4a_7c15) >> 32) as usize % fit.len();
    fit.rotate_left(first);
    fit
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rooms_meet_their_neighbours_and_lanes_run_through() {
        let flags = BTreeMap::from([("--gen".to_string(), "16".to_string())]);
        let map = Spec::of(&flags).expect("a spec").world();
        assert_eq!(map.rooms, Spec::of(&flags).expect("a spec").world().rooms);
        let terrain: BTreeMap<RoomXY, &[u8]> =
            (map.rooms.iter()).map(|(n, t)| (n.parse().expect("a room"), t.as_bytes())).collect();
        let open = |t: &[u8], x: usize, y: usize| t[y * 50 + x] & 1 == 0;
        for (&xy, t) in &terrain {
            type Edge = fn(usize) -> (usize, usize);
            let sides: [(usize, (i32, i32), Edge, Edge); 2] = [
                (RIGHT, (1, 0), |i| (49, i), |i| (0, i)),
                (BOTTOM, (0, 1), |i| (i, 49), |i| (i, 0)),
            ];
            for (side, (dx, dy), mine, facing) in sides {
                let Some(other) = terrain.get(&RoomXY::new(xy.x + dx, xy.y + dy)) else {
                    continue;
                };
                let ours = |i| {
                    let (x, y) = mine(i);
                    open(t, x, y)
                };
                let theirs = |i| {
                    let (x, y) = facing(i);
                    open(other, x, y)
                };
                assert!((0..50).all(|i| ours(i) == theirs(i)), "{xy}: side {side}");
                if mapgen::room_kind(xy) == RoomKind::Highway && Lane::of(xy).ends(side) {
                    let lane: Vec<usize> = (0..50).filter(|&i| ours(i)).collect();
                    let unbroken = lane.last().zip(lane.first()).map(|(z, a)| z - a + 1);
                    assert!((21..=43).contains(&lane.len()), "{xy}: lane end {side}");
                    assert_eq!(unbroken, Some(lane.len()), "{xy}: lane end {side} broken");
                }
            }
        }
    }
}
