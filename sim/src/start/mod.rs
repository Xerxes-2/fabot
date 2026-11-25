//! `npm run start-rooms`: each free room of a world seated as a respawn is
//! (`Map::stand`), every room in sight, ranked by what it digs at home and
//! nets from the outposts Strategy opens from it once it banks, beside
//! Strategy's claim gate and score and Layout's plan.
use std::collections::{BTreeMap, BTreeSet};
use std::sync::Arc;

use fabot_atlas::{self as atlas, Atlas, BANK_LEVEL};
use fabot_base::declarations::{ColonyPlan, Declarations, Outpost, Stage};
use fabot_base::engine;
use fabot_base::intel::{Intel, Row};
use fabot_base::tiles::{self, Tile};
use fabot_base::world::{RoomName, Rules, terrain};
use fabot_core::drive::Terrains;
use fabot_layout as layout;
use fabot_strategy::{expand, intel, outposts};
use krebs::world::Presence;

use crate::driver::Maps;
use crate::read;
use crate::seat::Seat;
use crate::view::View;
use crate::world::{self, Map};

const HEADER: [&str; 22] = [
    "rank", "room", "value", "claim", "score", "src", "near", "mineral", "ore", "season", "spawn",
    "outposts", "out-src", "out-net", "exits", "swamp%", "centre", "highway", "keepers", "rival",
    "reserved", "home",
];

pub fn main() {
    let usage = "usage: sim start-rooms <world.json> [room,…]";
    let args: Vec<String> = std::env::args().skip(2).collect();
    let map = world::read(args.first().expect(usage));
    let wanted: Vec<&str> = (args.get(1)).map_or_else(Vec::new, |a| a.split(',').collect());
    let closed = map.closed.iter().filter_map(|r| r.parse().ok()).collect();
    let maps = Maps(map.terrains(), closed);
    let free = (map.objects.iter())
        .filter(|o| o["type"] == "controller" && o["user"].is_null() && o["reservation"].is_null());
    let rooms: BTreeSet<&str> = (free.filter_map(|o| o["room"].as_str()))
        .filter(|r| wanted.is_empty() || wanted.contains(r))
        .collect();
    let mut ranked: Vec<_> = (rooms.into_iter())
        .filter_map(|r| rank(&map, &maps, r))
        .collect();
    ranked.sort_by(|(a, _), (b, _)| b.total_cmp(a));
    let mut rows = vec![HEADER.map(String::from).to_vec()];
    for (i, (_, row)) in ranked.into_iter().enumerate() {
        rows.push([vec![(i + 1).to_string()], row].concat());
    }
    let width = |c: usize| rows.iter().map(|r| r[c].len()).max().unwrap_or(0);
    let widths: Vec<usize> = (0..HEADER.len()).map(width).collect();
    for row in rows {
        let cells = row.iter().zip(&widths).map(|(c, w)| format!("{c:<w$}"));
        println!("{}", cells.collect::<Vec<_>>().join("  ").trim_end());
    }
}

/// Its value, energy a tick, and its row.
fn rank(map: &Map, maps: &Maps, room: &str) -> Option<(f64, Vec<String>)> {
    let name: RoomName = room.parse().ok()?;
    let seat = Seat {
        uid: world::US.into(),
        name: world::USERNAME.into(),
        room: room.into(),
        memory: String::new(),
    };
    let mut k = map.stand(&[seat], 1, true, None);
    // Every room in sight, as the server's map shows the owner.
    let us = read::our_idx(&View::of(&k));
    for r in &mut k.rooms {
        let r = Arc::get_mut(r).expect("a room of its own");
        let sight = Presence {
            vision: 1,
            ..Presence::default()
        };
        r.presence = (r.presence.iter().cloned()).chain([(us, sight)]).collect();
    }
    let declarations = Declarations::default();
    let mut world = read::world(&View::of(&k), &declarations, None);
    world.closed = maps.1.clone();
    (world.rooms.iter_mut()).for_each(|r| r.terrain = maps.terrain(r.name));
    let mut intel = Intel::default();
    intel::step(&world, &Atlas::default(), &mut intel);
    let seen = (intel.iter()).map(|(r, row)| (*r, row.lairs.as_deref(), false));
    let mut atlas = Atlas::following(Atlas::default(), &world, seen, maps.terrains());
    let derived = atlas::derive(&world, &mut atlas);

    // Its row before the seat: what the claim rule reads of a room not yet ours.
    let row = Row {
        owner: None,
        level: 0,
        ..intel.get(&name)?.clone()
    };
    let none = BTreeSet::new();
    let rivals = fabot_strategy::rivals(&world, &declarations, &intel);
    let facts = expand::facts(&world, (&none, &|_| false), &intel, (name, &row), (0, 0.0));
    let outpost = Outpost {
        reserved: true,
        room: name,
        controller: row.controller.clone()?,
        sources: row.sources.clone()?,
    };
    let fits = expand::fits(&world, &outpost, &intel, maps.terrain(name)?);
    let claimable =
        fits && expand::claimable(&world, (&none, &rivals), &declarations, (name, &row));

    let (plan, spawn) = layout::first_spawn(&Rules::vanilla(), &map.bare(room))?;
    // Banking on its pile at RCL2, when Strategy first opens outposts (§7.2).
    let colony = ColonyPlan {
        room: name,
        stage: Stage::Exploit,
        target: BANK_LEVEL,
        reserve: 0,
        rampart_hits: 0,
        ring: false,
        owes: false,
        fights: false,
        pile: plan.pile(),
        muster: None,
    };
    let survey = (&declarations, &[colony][..], None);
    let candidates = outposts::survey((&world, &mut atlas), &derived, survey, &intel);
    let nets: BTreeMap<RoomName, f64> = (candidates.iter())
        .map(|c| (c.outpost.room, outposts::net(&world, c)))
        .collect();
    let spare = (&none, &BTreeMap::new());
    let chosen = outposts::chosen(&world, candidates, spare, (&declarations.tuning, &[]));
    let net: f64 = chosen.iter().map(|o| nets[&o.room]).sum();
    // The claim rule's own price of a home source.
    let home = f64::from(engine::HELD_OUTPUT_PER_TICK) * facts.sources as f64;

    let wall = |t: &Tile| terrain::is_wall(maps.terrain(name), tiles::x_of(*t), tiles::y_of(*t));
    let swamp =
        |t: &&Tile| terrain::is_swamp(maps.terrain(name), tiles::x_of(**t), tiles::y_of(**t));
    let sides = [(1, "T"), (3, "R"), (5, "B"), (7, "L")];
    let exits = sides.map(|(e, c)| {
        if tiles::exit_tiles(e).iter().all(wall) {
            "-"
        } else {
            c
        }
    });
    let open: Vec<Tile> = tiles::all().filter(|t| !wall(t)).collect();
    let grid = name.on_grid()?;
    let keepers = (-5..=5)
        .flat_map(|dx| (-5..=5).filter_map(move |dy| name.offset(dx, dy)))
        .filter(|r| r.on_grid().is_some_and(tiles::is_keeper_room))
        .map(|r| tiles::rooms_apart(r, name))
        .min();
    let claim = match () {
        _ if claimable => "yes",
        _ if fits => "no",
        _ => "unfit",
    };
    let cells = [
        room.to_string(),
        format!("{:.1}", home + net),
        claim.to_string(),
        format!("{:.1}", expand::score(&facts)),
        facts.sources.to_string(),
        facts.free_near.to_string(),
        row.mineral.map_or("-".to_string(), |(m, _)| m.to_string()),
        format!("{:.1}", facts.mineral),
        format!("{:.1}", facts.worth),
        format!("{},{}", tiles::x_of(spawn), tiles::y_of(spawn)),
        (chosen.iter().map(|o| o.room.to_string()))
            .collect::<Vec<_>>()
            .join(" "),
        chosen
            .iter()
            .map(|o| o.sources.len())
            .sum::<usize>()
            .to_string(),
        format!("{net:.1}"),
        exits.concat(),
        (100 * open.iter().filter(swamp).count() / open.len().max(1)).to_string(),
        tiles::from_centre(grid).to_string(),
        tiles::from_highway(grid).to_string(),
        keepers.map_or("-".into(), |d| d.to_string()),
        (rivals
            .iter()
            .map(|(r, _)| tiles::rooms_apart(*r, name))
            .min())
        .map_or("-".into(), |d| d.to_string()),
        (intel.iter())
            .filter(|(r, w)| w.reservation.is_some() && tiles::rooms_apart(**r, name) <= 2)
            .count()
            .to_string(),
        format!("{home:.1}"),
    ];
    Some((home + net, cells.to_vec()))
}
