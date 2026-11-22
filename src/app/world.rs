//! `World.ofGame`: the one reader of the game's objects. `read.js` walks them
//! in one pass and hands over its numbers and its strings whole, for
//! `fabot_base::world::read` to decode; this fetches what only the engine
//! answers. A call across the seam costs more than the field it reads, so the
//! reader makes a handful a tick.
use fabot_base::HashMap;
use std::collections::BTreeSet;

use js_sys::{Array, Float64Array, Int32Array};
use screeps::LocalRoomTerrain;
use screeps::game::{
    self,
    map::{self, RoomStatus},
};
use wasm_bindgen::JsCast;
use wasm_bindgen::prelude::wasm_bindgen;

use fabot_base::declarations::Declarations;
use fabot_base::tiles;
use fabot_base::world::RoomName;
use fabot_base::world::*;
use fabot_core::drive::{Counts, Phases};

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_name = __fabotRead)]
    fn read(drain: Option<String>) -> Array;
}

/// `ARCHITECTURE.md` §4's `terrain` row.
#[derive(Default)]
pub struct Maps {
    terrain: HashMap<RoomName, Option<LocalRoomTerrain>>,
    status: HashMap<RoomName, Option<RoomStatus>>,
}

/// The rooms out of sight whose terrain is read in one tick, nearest our
/// spawns first, out to the furthest a route goes.
const READS_A_TICK: usize = 10;

impl fabot_core::drive::Terrains for Maps {
    fn terrains(&self) -> Box<dyn Iterator<Item = (RoomName, &[u8; tiles::COUNT])> + '_> {
        Box::new(
            (self.terrain.iter()).filter_map(|(room, t)| Some((*room, t.as_ref()?.get_bits()))),
        )
    }

    fn terrain(&self, room: RoomName) -> Option<&LocalRoomTerrain> {
        self.terrain.get(&room)?.as_ref()
    }
}

impl Maps {
    /// Kept when empty too, so a room the map lacks is asked once.
    fn read_around(&mut self, homes: impl Iterator<Item = RoomName>) {
        let reach = fabot_base::declarations::SIEGE_HOPS;
        let mut unread: Vec<(i32, RoomName)> = Vec::new();
        for home in homes {
            for dx in -reach..=reach {
                let side = reach - dx.abs();
                for dy in -side..=side {
                    if let Some(room) = home.offset(dx, dy)
                        && !self.terrain.contains_key(&room)
                    {
                        unread.push((dx.abs() + dy.abs(), room));
                    }
                }
            }
        }
        unread.sort_unstable();
        unread.dedup_by(|a, b| a.1 == b.1);
        for (_, room) in unread.into_iter().take(READS_A_TICK) {
            let terrain = terrain_of(room);
            self.terrain.insert(room, terrain);
        }
    }

    fn status(&mut self, room: RoomName) -> Option<RoomStatus> {
        *self.status.entry(room).or_insert_with(|| status_of(room))
    }
}

/// What `read.js` handed over, whole: the recorder's capture of the tick.
pub struct Pass {
    pub ints: Vec<i32>,
    pub strs: String,
}

/// `read.js`'s sections in its order, each closed by its own clock.
const SECTIONS: [&str; 10] = [
    "read.rooms",
    "read.sources",
    "read.structures",
    "read.sites",
    "read.piles",
    "read.remains",
    "read.others",
    "read.events",
    "read.own",
    "read.pack",
];

pub fn of_game<'t>(
    maps: &'t mut Maps,
    buffers: &mut read::Buffers,
    (declarations, drain): (&Declarations, Option<RoomName>),
    (clock, start, phases, walked): (&dyn Fn() -> f64, f64, &mut Phases, &mut Counts),
) -> (World<'t>, &'t Maps, Pass, f64) {
    let packed = read(drain.map(|room| room.to_string()));
    let ints = packed.get(0).unchecked_into::<Int32Array>().to_vec();
    let text = packed.get(1).as_string().unwrap_or_default();
    let marks = (packed.get(2).dyn_into::<Float64Array>()).map_or_default(|m| m.to_vec());
    let decoding = clock();
    let mut last = start;
    for (name, &[at, count, parts]) in SECTIONS.iter().zip(marks.as_chunks::<3>().0) {
        phases.push((name.to_string(), at - last));
        walked.push((name.to_string(), count as i32));
        if parts > 0.0 {
            walked.push((format!("{name}.parts"), parts as i32));
        }
        last = at;
    }
    phases.push(("read.copy".to_string(), decoding - last));
    let mut world = read::world(&ints, &text, declarations, buffers);
    world.cpu_limit = f64::from(game::cpu::limit());
    let mapping = clock();
    phases.push(("decode".to_string(), mapping - decoding));
    for name in world.rooms.iter().map(|room| room.name) {
        if !matches!(maps.terrain.get(&name), Some(Some(_)))
            && let Some(terrain) = terrain_of(name)
        {
            maps.terrain.insert(name, Some(terrain));
        }
    }
    let homes = world
        .spawns
        .iter()
        .filter(|s| s.pos.room != RoomName::SIM)
        .map(|s| s.pos.room);
    maps.read_around(homes.collect::<BTreeSet<_>>().into_iter());
    world.closed = read::closed(&world, declarations, |room| maps.status(room));
    let mapped = clock();
    phases.push(("map".to_string(), mapped - mapping));
    (world, maps, Pass { ints, strs: text }, mapped)
}

/// The map serves rooms the bot cannot see, and reading it needs no `Room` object.
fn terrain_of(name: RoomName) -> Option<LocalRoomTerrain> {
    map::get_room_terrain(name.on_map()?).map(LocalRoomTerrain::from)
}

/// `None` where the map has no `getRoomStatus` (the bench), so no room reads closed.
fn status_of(name: RoomName) -> Option<RoomStatus> {
    map::get_room_status(name.on_map()?).map(|result| result.status())
}
