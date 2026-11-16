//! A soak node's structures (`ARCHITECTURE.md` §10.2): the Layout's plan for
//! a room, built at a level the way the bot's sites would build it with every
//! site finished at once. Reads the room on stdin as JSON
//! `{ room, terrain, sources: [[x, y]], minerals: [[x, y]], controller: [x, y], level }`
//! (terrain: 2500 digits, the raw buffer's), writes `[{ kind, x, y }]` in
//! build order, the first spawn first.
use std::io::Read;

use fabot_core::layout::{ground, plan, sites};
use fabot_core::tiles::{self, Tile};
use fabot_core::vocabulary::Resource;
use fabot_core::world::{
    Controller, Kind, Mineral, Owner, Pos, Room, RoomName, Rules, Source, Store, Structure, terrain,
};
use serde_json::{Value, json};

fn main() {
    let mut text = String::new();
    std::io::stdin().read_to_string(&mut text).unwrap();
    let input: Value = serde_json::from_str(&text).unwrap();
    let name = RoomName::named(input["room"].as_str().unwrap());
    let level = input["level"].as_i64().unwrap() as i32;
    let pos = |e: &Value| {
        Pos::new(
            name,
            e[0].as_i64().unwrap() as i32,
            e[1].as_i64().unwrap() as i32,
        )
    };
    let each = |key: &str| -> Vec<Pos> { input[key].as_array().unwrap().iter().map(pos).collect() };

    let room = Room {
        name,
        terrain: terrain::leak(
            input["terrain"]
                .as_str()
                .unwrap()
                .bytes()
                .map(|c| c - b'0')
                .collect(),
        ),
        controller: Some(Controller {
            id: "controller".into(),
            pos: pos(&input["controller"]),
            owner: Owner::Ours,
            level,
            progress: 0,
            progress_total: 0,
            ticks_to_downgrade: 0,
            reservation: None,
            safe_mode: 0,
            safe_mode_available: 0,
            safe_mode_cooldown: 0,
            upgrade_blocked: 0,
        }),
        energy_available: 0,
        energy_capacity: 0,
        sources: each("sources")
            .into_iter()
            .enumerate()
            .map(|(i, pos)| Source {
                id: format!("source{i}"),
                pos,
                energy: 0,
                energy_capacity: 0,
                ticks_to_regeneration: 0,
            })
            .collect(),
        minerals: each("minerals")
            .into_iter()
            .enumerate()
            .map(|(i, pos)| Mineral {
                id: format!("mineral{i}"),
                pos,
                resource: Resource::Energy,
                amount: 0,
            })
            .collect(),
        structures: Vec::new(),
        sites: Vec::new(),
        piles: Vec::new(),
        remains: Vec::new(),
        others: Vec::new(),
    };

    let rules = Rules::vanilla();
    let ground = ground::of_room(&room).unwrap();
    let first_spawn = plan::of_ground(&rules, &ground, &[])
        .unwrap()
        .items
        .into_iter()
        .find(|i| i.kind == Kind::Spawn)
        .unwrap();
    // The plan the bot draws once its first spawn stands.
    let plan = plan::of_ground(&rules, &ground, &[first_spawn.tile]).unwrap();

    let mut built: Vec<Structure> = Vec::new();
    let build = |built: &mut Vec<Structure>, kind: Kind, tile: Tile| {
        let walkable = kind.walkable();
        built.push(Structure {
            id: format!("s{}", built.len()),
            kind,
            pos: tiles::to_pos(name, tile),
            owner: Owner::Ours,
            hits: 1,
            hits_max: 1,
            store: Store::empty(),
            cooldown: 0,
            collapse: 0,
            spawn: None,
            level: 0,
            walkable,
        })
    };
    build(&mut built, Kind::Spawn, first_spawn.tile);
    loop {
        let census = sites::census_of(
            &Room {
                structures: built.clone(),
                ..room.clone()
            },
            &[],
        );
        let placed = sites::place(&rules, &census, level, &plan, i32::MAX);
        if placed.is_empty() {
            break;
        }
        for p in placed {
            build(&mut built, p.kind, tiles::index(p.x, p.y));
        }
    }

    let out: Vec<Value> = built
        .iter()
        .map(|s| json!({ "kind": s.kind.name(), "x": s.pos.x, "y": s.pos.y }))
        .collect();
    println!("{}", Value::Array(out));
}
