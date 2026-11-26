use std::collections::BTreeSet;

use screeps::LocalRoomTerrain;

use crate::{
    boost::{Act, takes},
    tiles::{self, Tile},
    vocabulary::{Part, ResourceType},
    world::{
        Controller, Id, Mineral, Owner, Pos, Room, RoomName, Rules, Source, Spawn, Store,
        Structure, StructureType, World, terrain,
    },
};

pub fn boosted(part: Part, tier: usize) -> Option<ResourceType> {
    use ResourceType::*;
    const LINES: [[ResourceType; 3]; 6] = [
        [UtriumHydride, UtriumAcid, CatalyzedUtriumAcid],
        [KeaniumOxide, KeaniumAlkalide, CatalyzedKeaniumAlkalide],
        [LemergiumOxide, LemergiumAlkalide, CatalyzedLemergiumAlkalide],
        [GhodiumOxide, GhodiumAlkalide, CatalyzedGhodiumAlkalide],
        [ZynthiumOxide, ZynthiumAlkalide, CatalyzedZynthiumAlkalide],
        [ZynthiumHydride, ZynthiumAcid, CatalyzedZynthiumAcid],
    ];
    let t = tier.checked_sub(1)?;
    LINES.iter().map(|line| line[t]).find(|c| takes(Act::Fight, part, *c))
}

/// Regressions persist beside the source file: proptest's default files them in
/// the parent of the nearest `lib.rs` (`core/`), and warns where none is above.
pub fn config(cases: u32) -> proptest::test_runner::Config {
    use proptest::test_runner::{Config, FileFailurePersistence};
    let beside = FileFailurePersistence::WithSource("proptest-regressions");
    Config { failure_persistence: Some(Box::new(beside)), ..Config::with_cases(cases) }
}

/// A Season 11 room with a source, as `scripts/real-rooms.mjs` packed it.
pub struct RealRoom {
    pub name: RoomName,
    pub terrain: LocalRoomTerrain,
    pub sources: Vec<Tile>,
    pub controller: Option<Tile>,
    pub mineral: Option<(Tile, Option<ResourceType>)>,
}

impl RealRoom {
    /// Nobody's: its controller unowned, level 0.
    pub fn room(&self) -> Room<'_> {
        let at = |t: Tile| tiles::to_pos(self.name, t);
        let mineral = self.mineral.map(|(t, resource)| (at(t), resource));
        let sources: Vec<Pos> = self.sources.iter().map(|t| at(*t)).collect();
        room(self.name, &self.terrain, &sources, self.controller.map(at), mineral)
    }
}

/// Every room of Season 11's map with a source, in name order.
pub fn real_rooms() -> impl Iterator<Item = RealRoom> {
    let packed = include_bytes!("real-rooms.bin");
    let bytes = miniz_oxide::inflate::decompress_to_vec(packed).expect("real-rooms.bin inflates");
    let mut at = 0;
    std::iter::from_fn(move || {
        let mut take = |n: usize| {
            at += n;
            bytes.get(at - n..at)
        };
        let u16_of = |b: &[u8]| usize::from(u16::from_le_bytes([b[0], b[1]]));
        let tile = |b: &[u8]| (u16_of(b) != 0xffff).then(|| Tile::of_index(u16_of(b)));
        let head = take(3)?;
        let name = RoomName::new(i32::from(head[0] as i8), i32::from(head[1] as i8));
        let sources = take(2 * usize::from(head[2]))?.chunks(2).filter_map(tile).collect();
        let tail = take(5)?;
        let resource = (tail[4] != 0).then(|| char::from(tail[4]).to_string().parse().ok());
        let bits = take(tiles::COUNT / 4)?;
        let bits = (0..tiles::COUNT).map(|i| (bits[i / 4] >> (i % 4 * 2)) & 3).collect::<Vec<u8>>();
        Some(RealRoom {
            name,
            terrain: LocalRoomTerrain::new_from_bits(bits.try_into().ok()?),
            sources,
            controller: tile(&tail[0..2]),
            mineral: tile(&tail[2..4]).zip(resource),
        })
    })
}

/// A `scripts/scenario/worlds/` JSON's rooms with their terrain, controllers,
/// sources and minerals, nobody's; its other objects are left out.
pub fn world(text: &str) -> World<'static> {
    let map: serde_json::Value = serde_json::from_str(text).expect("a worlds/ JSON");
    let mut world = World {
        time: 1,
        me: "us".into(),
        cpu_limit: 100.0,
        bucket: 10_000,
        rooms: Vec::new(),
        spawns: Vec::new(),
        creeps: Vec::new(),
        our_sites: 0,
        rooms_allowed: 1,
        closed: BTreeSet::new(),
        attacks: Vec::new(),
        destroyed: Vec::new(),
        rules: Rules::vanilla(),
    };
    for r in map["rooms"].as_array().into_iter().flatten() {
        let name: RoomName = r["name"].as_str().and_then(|n| n.parse().ok()).expect("a room name");
        // One string, or a string a row.
        let rows = r["terrain"].as_array().into_iter().flatten().chain([&r["terrain"]]);
        let bits = rows.flat_map(|row| row.as_str().unwrap_or_default().bytes());
        let terrain = terrain::leak(bits.map(|b| b - b'0').collect()).expect("2500 tiles");
        let objects = r["objects"].as_array().into_iter().flatten();
        let of = |kind: &'static str| objects.clone().filter(move |o| o["type"] == kind);
        let pos = |o: &serde_json::Value| {
            let xy = |k: &str| o[k].as_i64().unwrap_or_default() as i32;
            Pos::new(name, xy("x"), xy("y"))
        };
        let sources: Vec<Pos> = of("source").map(pos).collect();
        let mineral = of("mineral")
            .next()
            .map(|o| (pos(o), o["mineralType"].as_str().and_then(|m| m.parse().ok())));
        world.rooms.push(room(name, terrain, &sources, of("controller").next().map(pos), mineral));
    }
    world
}

/// Our room at `level`, its controller at the middle, nothing built.
pub fn ours(name: RoomName, level: i32, terrain: &'static LocalRoomTerrain) -> Room<'static> {
    let mut room = room(name, terrain, &[], Some(Pos::new(name, 25, 25)), None);
    room.controller = room.controller.map(|c| Controller { owner: Owner::Ours, level, ..c });
    room
}

pub fn built(id: &str, kind: StructureType, pos: Pos) -> Structure {
    Structure {
        id: id.into(),
        kind,
        pos,
        owner: Owner::Ours,
        hits: 1,
        hits_max: 1,
        store: Store::default(),
        cooldown: 0,
        collapse: 0,
        deploy: 0,
        level: 0,
        spawn: None,
        walkable: crate::world::walkable(kind, &Owner::Ours, false),
    }
}

/// `world`'s room `name` ours at `level`, its one spawn at `spawn`.
pub fn colonize(world: &mut World, name: RoomName, level: i32, spawn: Tile) {
    let room = world.rooms.iter_mut().find(|r| r.name == name).expect("the room");
    let controller = room.controller.as_mut().expect("a controller");
    (controller.owner, controller.level) = (Owner::Ours, level);
    room.energy_capacity = 12_900;
    let s = built("spawn", StructureType::Spawn, tiles::to_pos(name, spawn));
    let (id, pos) = (s.id, s.pos);
    room.structures.push(s);
    world.spawns.push(Spawn { id, name: id.to_string(), pos, spawning: None });
}

fn room<'a>(
    name: RoomName,
    terrain: &'a LocalRoomTerrain,
    sources: &[Pos],
    controller: Option<Pos>,
    mineral: Option<(Pos, Option<ResourceType>)>,
) -> Room<'a> {
    let id = |kind: &str, i: usize| Id::from(format!("{name}{kind}{i}").as_str());
    Room {
        terrain: Some(terrain),
        controller: controller.map(|pos| Controller {
            id: id("c", 0),
            pos,
            owner: Owner::Nobody,
            level: 0,
            progress: 0,
            progress_total: 0,
            ticks_to_downgrade: 0,
            reservation: None,
            sign: None,
            safe_mode: 0,
            safe_mode_available: 0,
            safe_mode_cooldown: 0,
            upgrade_blocked: 0,
        }),
        sources: (sources.iter().enumerate())
            .map(|(i, pos)| Source {
                id: id("s", i),
                pos: *pos,
                energy: 0,
                energy_capacity: 0,
                ticks_to_regeneration: 0,
            })
            .collect(),
        minerals: (mineral.into_iter())
            .map(|(pos, resource)| Mineral { id: id("m", 0), pos, resource, amount: 0 })
            .collect(),
        ..Room::empty(name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// The engine stands rocks and the controller on wall: a misread tile is off it.
    #[test]
    fn every_real_rooms_rock_and_controller_stands_on_wall() {
        let mut count = 0;
        for real in real_rooms() {
            let wall =
                |t: Tile| terrain::is_wall(Some(&real.terrain), tiles::x_of(t), tiles::y_of(t));
            let mut stood = real.sources.iter().chain(&real.controller).copied();
            assert!(stood.all(wall) && real.mineral.is_none_or(|(t, _)| wall(t)), "{}", real.name);
            count += 1;
        }
        assert_eq!(count, 2916);
    }
}
