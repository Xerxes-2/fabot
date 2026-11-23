//! A soak node's structures (`ARCHITECTURE.md` §10.2): the Layout's plan for
//! a room, built at a level the way the bot's sites would build it with every
//! site finished at once (with `labs`, also the labs Layout holds back below
//! RCL8). Reads the room on stdin as JSON
//! `{ room, terrain, sources: [[x, y]], minerals: [[x, y]], controller: [x, y], level, ring?, labs? }`
//! (terrain: 2500 digits, the raw buffer's), writes `[{ kind, x, y }]` in
//! build order, the first spawn first.
// A tool, not the bot: a panic fails the run that called it.
#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::io::Read;

use fabot_base::tiles::{self, Tile};
use fabot_base::world::{
    Built, Owner, Pos, Room, RoomName, Rules, Store, Structure, StructureType,
};
use fabot_layout::{self as layout, sites};
use serde_json::{Value, json};

fn main() {
    let mut text = String::new();
    std::io::stdin().read_to_string(&mut text).unwrap();
    let input: Value = serde_json::from_str(&text).unwrap();
    let name = input["room"].as_str().unwrap().parse::<RoomName>().unwrap();
    let level = input["level"].as_i64().unwrap() as i32;
    let pos = |e: &Value| {
        Pos::new(
            name,
            e[0].as_i64().unwrap() as i32,
            e[1].as_i64().unwrap() as i32,
        )
    };
    let each = |key: &str| -> Vec<Pos> { input[key].as_array().unwrap().iter().map(pos).collect() };

    let terrain = input["terrain"]
        .as_str()
        .unwrap()
        .bytes()
        .map(|c| c - b'0')
        .collect();
    let rocks = (
        pos(&input["controller"]),
        &each("sources")[..],
        &each("minerals")[..],
    );
    let room = layout::bare(name, terrain, rocks, level);

    let rules = Rules::vanilla();
    let (plan, first_spawn) = layout::first_spawn(&rules, &room).unwrap();

    let mut built: Vec<Structure> = Vec::new();
    let build = |built: &mut Vec<Structure>, kind: StructureType, tile: Tile| {
        let walkable = kind.walkable();
        built.push(Structure {
            id: format!("s{}", built.len()).as_str().into(),
            kind,
            pos: tiles::to_pos(name, tile),
            owner: Owner::Ours,
            hits: 1,
            hits_max: 1,
            store: Store::empty(),
            cooldown: 0,
            collapse: 0,
            deploy: 0,
            spawn: None,
            level: 0,
            walkable,
        })
    };
    build(&mut built, StructureType::Spawn, first_spawn);
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

    let labs = input["labs"].as_bool().unwrap_or(false);
    for i in (plan.items.iter()).filter(|i| labs && i.level <= level && !sites::placeable(i, level))
    {
        build(&mut built, i.kind, i.tile);
    }
    // `"ring": true` stands the ring too, as a kept room's from its level.
    let ring = input["ring"].as_bool().unwrap_or(false);
    for i in plan.ring.iter().filter(|i| ring && i.level <= level) {
        build(&mut built, i.kind, i.tile);
    }
    let out: Vec<Value> = built
        .iter()
        .map(|s| json!({ "kind": s.kind, "x": s.pos.x, "y": s.pos.y }))
        .collect();
    println!("{}", Value::Array(out));
}
