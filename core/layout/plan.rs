//! Hivemind's room plan (MIT, © David Franke): built around the first
//! centre that fits one, each structure given the level its room manager
//! builds it at.
use std::cmp::Reverse;

use fabot_atlas::Atlas;
use fabot_base::{
    engine,
    tiles::{self, Tile},
    world::{Built as _, Rules, StructureType},
};

use super::{
    build::{self, Built},
    ground::{Ground, cut_exits},
    path::Search,
    place::{self, Labels, SWAMP},
    shape::{self, Item, Plan},
};

/// Below it a room's energy goes to its controller alone.
pub fn haul_level(rules: &Rules) -> i32 {
    rules.level_of(StructureType::Extension, 0)
}

/// Not links, containers or roads: they are seated by what they serve.
pub fn sized() -> Vec<StructureType> {
    use StructureType::*;
    vec![Spawn, Extension, Tower, Storage, Terminal, Factory, PowerSpawn, Nuker, Observer, Lab]
}

/// Sharing one of the container's seats, so a body there draws from either.
pub fn controller_link(
    walkable: impl Fn(Tile) -> bool,
    linkable: impl Fn(Tile) -> bool,
    standable: impl Fn(Tile) -> bool,
    controller: Tile,
    container: Tile,
) -> Option<Tile> {
    tiles::within(engine::UPGRADE_RANGE, controller)
        .into_iter()
        .filter(|t| tiles::range(*t, container) > 1 && linkable(*t))
        .map(|t| (t, Atlas::upgrader_seats(&walkable, controller, t)))
        .filter(|(_, seats)| {
            seats.iter().any(|n| tiles::range(*n, container) <= 1)
                && seats.iter().all(|n| standable(*n))
        })
        .min_by_key(|(t, seats)| (-(seats.len() as i32), tiles::range(*t, container), *t))
        .map(|(t, _)| t)
}

/// Every colony walls its core from here (owner, 2026-11-14), as Hivemind does.
pub const RING_LEVEL: i32 = 4;

/// The room manager's order a kind is built in at a level.
fn order(rules: &Rules, plan: &Labels, kind: &str, level: i32, walk: &[u32]) -> Vec<Tile> {
    let both = |a: &str, b: &str| -> Vec<Tile> {
        (plan.of(a).into_iter()).filter(|t| plan.has(b, *t)).collect()
    };
    let numbered = |prefix: &str| -> Vec<Tile> {
        (0..10).flat_map(|i| plan.of(&format!("{prefix}.{i}"))).collect()
    };
    let mut out: Vec<Tile> = match kind {
        "spawn" | "tower" => numbered(kind),
        "extension" => {
            let mut v =
                if level >= 3 { both("extension.harvester", "extension") } else { Vec::new() };
            for i in 0..10 {
                v.extend(both(&format!("extension.bay.{i}"), "extension"));
            }
            v.extend(both("extension.bay", "extension"));
            v
        }
        "link" => {
            let mut v = plan.of("link.controller");
            let mut sources = plan.of("link.source");
            // Hivemind ranks by range, but a link saves the walk.
            sources.sort_by_key(|t| Reverse(walk.get(t.index()).copied()));
            v.extend(sources);
            v.extend(plan.of("link.storage"));
            v
        }
        "container" if !fabot_atlas::builds_away(rules, level) => return Vec::new(),
        "container" => {
            let mut v = plan.of("container.source");
            v.extend(plan.of("container.controller"));
            if level >= rules.level_of(StructureType::Extractor, 0) {
                v.extend(plan.of("container.mineral"));
            }
            return v;
        }
        _ => Vec::new(),
    };
    out.extend(plan.of(kind));
    out
}

/// Each kind's tiles with the level the room manager first builds them at,
/// in the order it builds them.
fn leveled(rules: &Rules, plan: &Labels, kind: StructureType, ground: &Ground) -> Vec<(Tile, i32)> {
    let mut built: Vec<(Tile, i32)> = Vec::new();
    let link = ground.controller.filter(|_| kind == StructureType::Link);
    let walk = link.map_or(Vec::new(), |c| walk_from(ground, c));
    for level in 1..=engine::CONTROLLER_MAX_LEVEL {
        let allowed = rules.allowed(kind, level).max(0) as usize;
        for t in order(rules, plan, &kind.to_string(), level, &walk) {
            if built.len() >= allowed {
                break;
            }
            if !built.iter().any(|(b, _)| *b == t) {
                built.push((t, level));
            }
        }
    }
    built
}

/// Over the terrain alone; an unreached tile ranks last.
fn walk_from(ground: &Ground, rock: Tile) -> Vec<u32> {
    let (plain, swamp) = (engine::PLAIN_WEIGHT as u32, engine::SWAMP_WEIGHT as u32);
    let search = Search { terrain: &ground.terrain, matrix: &[0; tiles::COUNT], plain, swamp };
    search.from(rock).iter().map(|d| d % u32::MAX).collect()
}

/// Where the room manager's `manageStructures` reaches a kind, but a source
/// container other seats ring goes before the extensions: its harvester digs
/// beside the crews (main's planner), and the nuker, which pays only loaded
/// and fired, goes last.
fn priority(kind: StructureType, level: i32, mined: bool, crowded: bool) -> i32 {
    use StructureType::*;
    match kind {
        Tower => 0,
        Spawn => 1,
        Container if !mined && crowded => 2,
        Extension => 3,
        Container if !mined => 4,
        Storage => 5,
        Terminal => 6,
        Road if level < 4 => 7,
        Link => 8,
        Extractor => 9,
        Container => 10,
        Rampart => 10,
        Road => 11,
        Lab => 13,
        PowerSpawn => 14,
        Observer => 15,
        Nuker => 16,
        _ => 17,
    }
}

fn to_plan(rules: &Rules, ground: &Ground, built: &Built) -> Plan {
    drawn(rules, ground, built).0
}

fn drawn(rules: &Rules, ground: &Ground, built: &Built) -> (Plan, Vec<Item>) {
    use StructureType::*;
    let plan = &built.plan;
    let fixed: Vec<Tile> = ground.rocks().collect();
    let mut keyed: Vec<(i32, i32, usize, Item)> = Vec::new();
    let mined = rules.level_of(Extractor, 0);
    let mut push = |kind: StructureType, tile: Tile, level: i32| {
        let filled = keyed.iter().any(|k| k.3.tile == tile && !k.3.kind.walkable());
        if (!fixed.contains(&tile) || kind == Extractor) && !(kind == Road && filled) {
            let seq = keyed.len();
            let rock = |r: &Tile| tiles::range(*r, tile) == 1;
            let seat = |n: &&Tile| ground.terrain[**n] != place::WALL && !fixed.contains(n);
            let crowded = (ground.sources.iter())
                .any(|r| rock(r) && tiles::around(*r).iter().filter(seat).count() > 1);
            keyed.push((
                priority(kind, level, level >= mined, crowded),
                level,
                seq,
                Item { kind, tile, level },
            ));
        }
    };
    for kind in [
        Tower, Spawn, Extension, Container, Storage, Terminal, Link, Extractor, Lab, Nuker,
        PowerSpawn, Observer, Factory,
    ] {
        for (t, level) in leveled(rules, plan, kind, ground) {
            push(kind, t, level);
        }
    }
    let early: Vec<Tile> = (plan.of("road.source").into_iter())
        .chain(plan.of("road.controller"))
        .filter(|t| ground.terrain[t.index()] == SWAMP)
        .collect();
    // Main's: each spawn, tower, storage and terminal has its own rampart
    // from the ring's level, which a ring the roads hold back would leave bare.
    for kind in [Spawn, Tower, Storage, Terminal] {
        for (t, level) in leveled(rules, plan, kind, ground) {
            push(Rampart, t, level.max(RING_LEVEL));
        }
    }
    // Our sites stack nothing on a container; Hivemind paves the controller's.
    // `push` lays none under a structure already keyed, whose build razes it.
    for t in plan.of("road").into_iter().filter(|t| !plan.has("container", *t)) {
        push(Road, t, if early.contains(&t) { 2 } else { 4 });
    }
    keyed.sort_by_key(|(p, l, s, _)| (*p, *l, *s));
    // Hivemind plans its core over a controller link now and then; the
    // tile is built for what its room manager reaches first.
    let mut items: Vec<Item> = Vec::new();
    for (_, _, _, i) in keyed {
        if !items.iter().any(|o| {
            o.tile == i.tile
                && (o.kind == Road, o.kind == Rampart) == (i.kind == Road, i.kind == Rampart)
        }) {
            items.push(i);
        }
    }

    let ramp = |t: &Tile| plan.has("rampart.ramp", *t);
    let (main, ramps): (Vec<Tile>, Vec<Tile>) =
        plan.of("rampart").into_iter().partition(|t| !ramp(t));
    let item = |kind: StructureType, level: i32| move |tile: Tile| Item { kind, tile, level };
    let mut ring: Vec<Item> = main.into_iter().chain(ramps).map(item(Rampart, 3)).collect();
    for (label, level) in [("wall.deco", 4), ("wall.quad", 6)] {
        ring.extend(plan.of(label).into_iter().map(item(Wall, level)));
    }
    ring.sort_by_key(|i| i.level);
    let mut dropped: Vec<Item> = Vec::new();
    // An outpost or another colony may lie only beyond an exit.
    let free = reached(ground, built.center, (false, true), items.iter());
    let seen = loop {
        let walk = |held| reached(ground, built.center, (false, held), items.iter().chain(&ring));
        let (seen, through) = (walk(true), walk(false));
        let beside = |t: Tile| tiles::within(1, t).into_iter().any(|n| seen[n]);
        let pocket = (items.iter())
            .filter(|i| !matches!(i.kind, Road | Rampart) && !beside(i.tile))
            .map(|i| i.tile)
            .chain(tiles::all().filter(|t| through[*t] && !seen[*t]))
            .chain(cut_exits(|t| free[t], |t| seen[t]));
        let wall = |t: Tile| ring.iter().any(|r| r.tile == t && r.kind == Wall);
        let inside =
            tiles::reach(pocket, |n| !seen[n] && ground.terrain[n] != place::WALL && !wall(n));
        let near =
            |t: Tile| (tiles::within(1, t).into_iter()).any(|n| inside[n] != tiles::UNREACHED);
        let given: Vec<Item> = ring.extract_if(.., |r| r.kind == Wall && near(r.tile)).collect();
        if given.is_empty() {
            break through;
        }
        dropped.extend(given);
    };
    // A pocket of roads and ramparts alone needs none, as its walls already
    // keep a body off it.
    let beside = |seen: &[bool], i: &Item| tiles::within(1, i.tile).into_iter().any(|n| seen[n]);
    dropped.extend(items.extract_if(.., |i| !beside(&seen, i)));
    let planned = items.iter().chain(&ring);
    let seen = reached(ground, built.center, (true, false), planned);
    dropped.extend(ring.extract_if(.., |i| !beside(&seen, i)));

    let unfit = (sized().into_iter())
        .filter(|kind| {
            (items.iter().filter(|i| i.kind == *kind).count() as i32)
                < rules.allowed(*kind, engine::CONTROLLER_MAX_LEVEL)
        })
        .collect();

    let planned: Vec<&Item> = items.iter().chain(&ring).collect();
    let open = |t: Tile| ground.terrain[t] != place::WALL;
    let muster = shape::muster(ground.room, built.center, &planned, open);
    let plan = Plan { room: ground.room, anchor: built.center, items, ring, unfit, muster };
    (plan, dropped)
}

/// The tiles a body reaches from the core, or in by an exit too; one beside
/// a structure serves it (a link, a lab, a spawn's door) as well as builds it.
/// `held`: an ore's post (any container beside an ore, a shared source post
/// too) is reached but never walked through: its digger yields only to a
/// chain two weights heavier.
fn reached<'a>(
    ground: &Ground,
    core: Tile,
    (exits, held): (bool, bool),
    planned: impl Iterator<Item = &'a Item>,
) -> Vec<bool> {
    let mut blocked: Vec<bool> = (ground.terrain.iter()).map(|t| *t == place::WALL).collect();
    ground.rocks().for_each(|t| blocked[t] = true);
    let mut post = vec![false; tiles::COUNT];
    for i in planned {
        post[i.tile] |= held
            && i.kind == StructureType::Container
            && (ground.minerals.iter()).any(|m| tiles::range(*m, i.tile) == 1);
        blocked[i.tile] = !i.kind.walkable() || (blocked[i.tile] && i.kind != StructureType::Road);
    }
    let seeds =
        (tiles::all()).filter(|t| exits && tiles::on_edge(*t) && !blocked[*t]).chain([core]);
    let walked = tiles::reach(seeds, |t| !blocked[t] && !post[t]);
    let walked = |t: Tile| walked[t] != tiles::UNREACHED;
    (tiles::all())
        .map(|t| {
            walked(t) || (post[t] && !blocked[t] && tiles::around(t).iter().any(|n| walked(*n)))
        })
        .collect()
}

/// As many centres as Hivemind's planner tries: its weighted one and one per
/// ninth of the room.
const TRIES: usize = 10;

/// The search, a few centres a tick as Hivemind's planner spreads them.
#[derive(Clone, Debug, PartialEq)]
pub struct Drawing {
    walls: Vec<u8>,
    exits: Vec<u8>,
    next: usize,
    held: Option<Plan>,
}

pub enum Drawn {
    Drawing,
    /// None where no centre builds.
    Done(Option<Plan>),
}

impl Drawing {
    pub fn of(ground: &Ground) -> Drawing {
        let (walls, exits) = place::distances(&ground.terrain);
        Drawing { walls, exits, next: 0, held: None }
    }

    /// Builds the centres in rank order while `budget` lasts, taking the
    /// first that fits a whole plan and leaves its centre to the pile a
    /// colony banks on before storage, else the first built.
    pub fn advance(&mut self, rules: &Rules, ground: &Ground, budget: &mut usize) -> Drawn {
        let dist = (&self.walls[..], &self.exits[..]);
        let centers = build::centers(ground, dist.0, dist.1);
        while let Some(center) = centers.get(self.next).filter(|_| self.next < TRIES) {
            if *budget == 0 {
                return Drawn::Drawing;
            }
            *budget -= 1;
            self.next += 1;
            if let Some(b) = build::build(rules, ground, dist, *center) {
                let plan = to_plan(rules, ground, &b);
                if plan.unfit.is_empty() && plan.pile().is_some() {
                    return Drawn::Done(Some(plan));
                }
                self.held.get_or_insert(plan);
            }
        }
        Drawn::Done(self.held.take())
    }
}

pub fn of_ground(rules: &Rules, ground: &Ground) -> Option<Plan> {
    let mut budget = usize::MAX;
    match Drawing::of(ground).advance(rules, ground, &mut budget) {
        Drawn::Done(plan) => plan,
        Drawn::Drawing => None,
    }
}

/// The first centre alone: whether a room fits a whole plan, at a
/// fraction of the search.
pub fn first_of(rules: &Rules, ground: &Ground) -> Option<Plan> {
    first_built(rules, ground).map(|b| to_plan(rules, ground, &b))
}

fn first_built(rules: &Rules, ground: &Ground) -> Option<Built> {
    let (walls, exits) = place::distances(&ground.terrain);
    let center = *build::centers(ground, &walls, &exits).first()?;
    build::build(rules, ground, (&walls, &exits), center)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{fixtures as fx, ground};

    #[test]
    fn the_allowance_never_falls_as_the_level_rises() {
        for row in &Rules::vanilla().structures {
            assert!(row.windows(2).all(|w| w[0] <= w[1]));
        }
    }

    #[test]
    fn no_level_seats_a_kind_past_its_allowance_nor_two_structures_on_a_tile() {
        let rules = Rules::vanilla();
        for colony in fx::samples(1).take(fx::MAX_TEST) {
            let Some((ground, plan)) = fx::plan_of(&colony) else {
                continue;
            };
            let rocks: Vec<Tile> = ground.rocks().collect();
            for level in 0..=engine::CONTROLLER_MAX_LEVEL {
                for kind in
                    sized().into_iter().chain([StructureType::Link, StructureType::Container])
                {
                    let seated = (plan.items.iter())
                        .filter(|i| i.kind == kind && i.level <= level)
                        .count() as i32;
                    assert!(seated <= rules.allowed(kind, level), "{kind:?} at {level}");
                }
            }
            let solid: Vec<Tile> = (plan.items.iter())
                .filter(|i| i.kind != StructureType::Road && i.kind != StructureType::Rampart)
                .map(|i| i.tile)
                .collect();
            assert_eq!(
                fx::distinct(&solid),
                solid,
                "{:?}",
                plan.items
                    .iter()
                    .filter(|i| solid.iter().filter(|t| **t == i.tile).count() > 1)
                    .collect::<Vec<_>>()
            );
            for i in plan.items.iter().filter(|i| !i.kind.walkable()) {
                let road = |o: &Item| o.tile == i.tile && o.kind == StructureType::Road;
                assert!(!plan.items.iter().any(road), "a road under {i:?}");
            }
            for i in plan.items.iter().chain(&plan.ring) {
                assert!(!tiles::on_edge(i.tile), "{i:?} on the edge");
                assert!(
                    (i.kind == StructureType::Extractor) == rocks.contains(&i.tile),
                    "{i:?} on rock"
                );
                assert!(
                    ground.terrain[i.tile] != 1
                        || i.kind == StructureType::Road
                        || i.kind == StructureType::Extractor,
                    "{i:?} on a wall"
                );
            }
        }
    }

    fn source_posts(ground: &Ground, plan: &Plan) -> Vec<Tile> {
        (plan.items.iter())
            .filter(|i| i.kind == StructureType::Container)
            .filter(|i| ground.sources.iter().any(|s| tiles::range(*s, i.tile) == 1))
            .map(|i| i.tile)
            .collect()
    }

    #[test]
    fn no_walk_from_an_exit_past_the_ring_reaches_the_base_but_beside_an_exit() {
        for colony in fx::samples(17).take(fx::MAX_TEST) {
            let Some((ground, plan)) = fx::plan_of(&colony) else {
                continue;
            };
            let open = |t: Tile| ground.terrain[t] != place::WALL;
            // The engine refuses a rampart beside an exit, so the ring cannot
            // seal those tiles; nothing else is exempt.
            let beside_exit =
                |t: Tile| (tiles::within(1, t).into_iter()).any(|n| tiles::on_edge(n) && open(n));
            let ring: Vec<Tile> = plan.ring.iter().map(|i| i.tile).collect();
            let posts = source_posts(&ground, &plan);
            // A harvest bay, its link too, stands outside the ring, beside its
            // post (owner).
            let bay = |i: &Item| {
                matches!(i.kind, StructureType::Extension | StructureType::Link)
                    && posts.iter().any(|p| tiles::range(*p, i.tile) == 1)
            };
            let shot = [
                StructureType::Spawn,
                StructureType::Extension,
                StructureType::Tower,
                StructureType::Storage,
                StructureType::Terminal,
                StructureType::Factory,
                StructureType::PowerSpawn,
                StructureType::Nuker,
                StructureType::Observer,
                StructureType::Lab,
            ];
            let core = (plan.items.iter())
                .filter(|i| !bay(i))
                .filter_map(|i| match &i.kind {
                    StructureType::Link => Some((i.tile, 0)),
                    k if shot.contains(k) => Some((i.tile, engine::RANGED_RANGE)),
                    _ => None,
                })
                .chain(ground.controller.map(|c| (c, 1)));

            let mut reached = vec![false; tiles::COUNT];
            let mut queue: Vec<Tile> =
                (tiles::all()).filter(|t| tiles::on_edge(*t) && open(*t)).collect();
            for t in &queue {
                reached[*t] = true;
            }
            while let Some(t) = queue.pop() {
                for &n in tiles::around(t) {
                    if open(n) && !reached[n] && !ring.contains(&n) {
                        reached[n] = true;
                        queue.push(n);
                    }
                }
            }
            for (c, reach) in core {
                for t in tiles::within(reach, c) {
                    assert!(!reached[t] || beside_exit(t), "{t} in reach of {c} outside the ring");
                }
            }
        }
    }

    #[test]
    fn the_first_source_link_has_the_longest_walk_to_the_controller() {
        let rules = Rules::vanilla();
        let mut linked = 0;
        for ground in fabot_base::testing::real_rooms().filter_map(|r| ground::of_room(&r.room())) {
            let Some(b) = first_built(&rules, &ground) else { continue };
            let sources = b.plan.of("link.source");
            let Some(c) = ground.controller.filter(|_| sources.len() > 1) else {
                continue;
            };
            linked += 1;
            let walk = walk_from(&ground, c);
            let links = leveled(&rules, &b.plan, StructureType::Link, &ground).into_iter();
            let first = links.map(|(t, _)| t).find(|t| sources.contains(t));
            let longest = sources.iter().map(|t| walk[*t]).max();
            assert_eq!(first.map(|t| walk[t]), longest, "{}", ground.room);
        }
        assert!(linked > 1000, "{linked}");
    }

    #[test]
    fn every_extension_beside_a_source_container_ranks_with_the_bays() {
        let rules = Rules::vanilla();
        let early = rules.allowed(StructureType::Extension, haul_level(&rules));
        for colony in fx::samples(15).take(fx::MAX_TEST) {
            let Some((ground, plan)) = fx::plan_of(&colony) else {
                continue;
            };
            let posts = source_posts(&ground, &plan);
            let bays: Vec<&Item> = (plan.items.iter())
                .filter(|i| i.kind == StructureType::Extension)
                .filter(|i| posts.iter().any(|p| tiles::range(*p, i.tile) == 1))
                .collect();
            let last = rules.level_of(StructureType::Extension, early + bays.len() as i32 - 1);
            for b in bays {
                assert!(b.level <= last, "{} at level {}", b.tile, b.level);
            }
        }
    }

    #[test]
    fn every_plan_musters_off_its_spawns_doors() {
        for colony in fx::samples(4).take(fx::MAX_TEST) {
            let Some((_, plan)) = fx::plan_of(&colony) else {
                continue;
            };
            let at = tiles::of_pos(&plan.muster.expect("a muster"));
            for i in plan.items.iter().chain(&plan.ring) {
                let reach = tiles::range(i.tile, at);
                assert!(
                    !(i.kind == StructureType::Spawn
                        && reach <= fabot_base::declarations::MUSTER_RANGE + 1),
                    "spawn at {}",
                    i.tile
                );
            }
        }
    }

    #[test]
    fn every_open_tile_beside_a_harvest_container_holds_a_structure_or_the_road_in() {
        for colony in fx::samples(3).take(fx::MAX_TEST) {
            let Some((ground, plan)) = fx::plan_of(&colony) else {
                continue;
            };
            let at = |t: Tile| plan.items.iter().filter(move |i| i.tile == t);
            for s in &ground.sources {
                let Some(c) = (plan.items.iter())
                    .find(|i| i.kind == StructureType::Container && tiles::range(i.tile, *s) == 1)
                else {
                    continue;
                };
                let exit = |t: Tile| {
                    (tiles::within(2, t).into_iter())
                        .any(|n| tiles::on_edge(n) && ground.terrain[n] != 1)
                };
                let bare: Vec<Tile> = (tiles::around(c.tile).iter().copied())
                    .filter(|t| ground.terrain[*t] != 1 && !exit(*t) && at(*t).next().is_none())
                    .collect();
                let roads = (tiles::around(c.tile).iter())
                    .filter(|t| at(**t).any(|i| i.kind == StructureType::Road));
                assert!(bare.is_empty() && roads.count() >= 1, "{bare:?} bare beside {c:?}");
            }
        }
    }

    /// The tiles a body stands on once the plan is built to `level`.
    fn open_of(ground: &Ground, plan: &Plan, level: i32) -> Vec<bool> {
        let mut open: Vec<bool> = ground.terrain.iter().map(|t| *t != place::WALL).collect();
        let planned = || plan.items.iter().chain(&plan.ring).filter(|i| i.level <= level);
        planned().for_each(|i| open[i.tile] |= i.kind == StructureType::Road);
        planned().for_each(|i| open[i.tile] &= i.kind.walkable());
        ground.rocks().for_each(|r| open[r] = false);
        open
    }

    /// The tiles a body walks to from `starts`, past every planned structure.
    fn walked(ground: &Ground, plan: &Plan, starts: &[Tile]) -> Vec<bool> {
        let open = open_of(ground, plan, engine::CONTROLLER_MAX_LEVEL);
        let steps = tiles::reach(starts.iter().copied(), |t| open[t]);
        steps.iter().map(|s| *s != tiles::UNREACHED).collect()
    }

    /// A digger holds its post while it works (`reached`'s `held`): every
    /// rock's post is walked to, from the level it is built at on, and no
    /// exit or anything planned is reached past a post alone.
    fn crafted() -> impl Iterator<Item = Ground> {
        let crafted = [
            ("W1N1", include_str!("../../scripts/scenario/worlds/corridor-post.json")),
            ("E9S5", include_str!("../../scripts/scenario/worlds/source-across-a-wall.json")),
            ("E9S5", include_str!("../../scripts/scenario/worlds/ore-post-e9s5.json")),
            ("E18S11", include_str!("../../scripts/scenario/worlds/source-pocket.json")),
            ("E13S19", include_str!("../../scripts/scenario/worlds/exit-pocket-post.json")),
        ];
        crafted.into_iter().filter_map(|(name, text)| {
            let world = fabot_base::testing::world(text);
            ground::of_room(world.rooms.iter().find(|r| r.name.to_string() == name)?)
        })
    }

    #[test]
    fn every_post_is_walked_to_from_its_level_on_and_never_the_only_way_on() {
        let rules = Rules::vanilla();
        let real = fabot_base::testing::real_rooms().step_by(5);
        let (mut posts, mut plans) = (0, 0);
        for ground in crafted().chain(real.filter_map(|r| ground::of_room(&r.room()))) {
            let Some(plan) = of_ground(&rules, &ground) else {
                continue;
            };
            plans += 1;
            let planned = plan.items.iter().chain(&plan.ring).map(|i| i.tile);
            let ends: Vec<Tile> = planned.chain(ground.rocks()).collect();
            let open = open_of(&ground, &plan, engine::CONTROLLER_MAX_LEVEL);
            let alone = |seat: Tile| {
                let from = |shut| tiles::reach([plan.anchor], |t| open[t] && t != shut);
                let (all, held) = (from(Tile::of_index(tiles::COUNT)), from(seat));
                let at = |r: &tiles::Table<u16>, t: Tile| t == seat || r[t] != tiles::UNREACHED;
                let near = |r: &tiles::Table<u16>, e: Tile| {
                    tiles::within(1, e).into_iter().any(|n| at(r, n))
                };
                tiles::all().any(|t| tiles::on_edge(t) && at(&all, t) && !at(&held, t))
                    || ends.iter().any(|e| near(&all, *e) && !near(&held, *e))
            };
            for rock in ground.sources.iter().chain(&ground.minerals) {
                let beside = |i: &&Item| {
                    i.kind == StructureType::Container && tiles::range(i.tile, *rock) == 1
                };
                let held: Vec<Tile> = plan.items.iter().filter(beside).map(|i| i.tile).collect();
                posts += held.len();
                let unseated = held.is_empty();
                let at = tiles::to_pos(ground.room, *rock);
                assert!(!unseated && !held.iter().any(|p| alone(*p)), "{at:?}'s post {held:?}");
            }
            for c in plan.items.iter().filter(|i| i.kind == StructureType::Container) {
                for level in c.level..=engine::CONTROLLER_MAX_LEVEL {
                    let open = open_of(&ground, &plan, level);
                    let walked = tiles::reach([plan.anchor], |t| open[t])[c.tile];
                    let at = tiles::to_pos(ground.room, c.tile);
                    assert!(walked != tiles::UNREACHED, "{at:?} unwalked at level {level}");
                }
            }
        }
        assert!(posts > 1000 && plans >= 524, "{posts} {plans}");
    }

    /// The whole plan built and every holder at work: its spawns, sources,
    /// controller, exits and road ends still walk to each other.
    #[test]
    #[ignore = "a report of the rooms that break it, until the owner picks the fixes"]
    fn every_end_is_walked_to_with_the_plan_built_and_its_holders_at_work() {
        use std::collections::{BTreeMap, BTreeSet};

        use StructureType::{Container, Road, Spawn};
        use fabot_base::world::RoomName;
        use fx::{End, Holder};
        let rules = Rules::vanilla();
        let real = fabot_base::testing::real_rooms();
        let mut cut: BTreeMap<(End, fx::Cause), BTreeSet<RoomName>> = BTreeMap::new();
        let mut plans = 0;
        for ground in crafted().chain(real.filter_map(|r| ground::of_room(&r.room()))) {
            let Some(plan) = of_ground(&rules, &ground) else {
                continue;
            };
            plans += 1;
            let open = open_of(&ground, &plan, engine::CONTROLLER_MAX_LEVEL);
            let planned = || plan.items.iter().chain(&plan.ring);
            let of = |k: StructureType| planned().filter(move |i| i.kind == k).map(|i| i.tile);
            let beside = |rock: Tile| of(Container).filter(move |c| tiles::range(*c, rock) == 1);
            let mut holders: Vec<(Holder, Tile)> = Vec::new();
            let mut ends: fx::Ends = of(Spawn).map(|s| (End::Spawn, vec![s], 1)).collect();
            for s in &ground.sources {
                let posts: Vec<Tile> = beside(*s).collect();
                holders.extend(posts.iter().map(|p| (Holder::Harvester, *p)));
                ends.push((End::Source, if posts.is_empty() { vec![*s] } else { posts }, 1));
            }
            holders.extend(
                ground.minerals.iter().flat_map(|m| beside(*m)).map(|p| (Holder::Digger, p)),
            );
            let mut pads: Vec<Tile> = Vec::new();
            if let Some(c) = ground.controller {
                let posted = |t: &Tile| holders.iter().any(|(_, h)| h == t);
                let buffer = of(Container)
                    .find(|b| !posted(b) && tiles::range(*b, c) <= engine::UPGRADE_RANGE);
                let seats = buffer.map(|b| Atlas::upgrader_seats(|t| open[t] && !posted(&t), c, b));
                // At RCL8 the economy casts one upgrader, its WORK the controller's whole
                // 15 a tick (`bodies::upgradable_at`), on the buffer's first seat.
                pads = seats.into_iter().flatten().collect();
                holders.extend(pads.iter().take(1).map(|t| (Holder::Upgrader, *t)));
                ends.push((End::Controller, vec![c], engine::UPGRADE_RANGE));
            }
            ends.extend(fx::exit_groups(&open).into_iter().map(|g| (End::Exit, g, 0)));
            let roads: Vec<Tile> = of(Road).collect();
            let held = |t: &Tile| holders.iter().any(|(_, h)| h == t);
            // A road on an upgrader's seat is its pad, not a way anywhere.
            let road_ends =
                fx::road_ends(&roads).into_iter().filter(|t| !held(t) && !pads.contains(t));
            ends.extend(road_ends.map(|t| (End::Road, vec![t], 0)));
            if ends.first().is_none_or(|e| e.0 != End::Spawn) {
                continue;
            }
            let mut bare = open_of(&ground, &plan, -1);
            of(Road).for_each(|t| bare[t] = true);
            for (end, _, cause) in fx::cut_off((&bare, &open), &ends, &holders) {
                cut.entry((end, cause)).or_default().insert(ground.room);
            }
        }
        let rooms: BTreeSet<&RoomName> = cut.values().flatten().collect();
        for ((end, cause), at) in &cut {
            let some: Vec<String> = at.iter().take(10).map(RoomName::to_string).collect();
            eprintln!("{end:?} cut by {cause:?}: {} rooms, {}", at.len(), some.join(" "));
        }
        assert!(rooms.is_empty(), "{} of {plans} plans cut", rooms.len());
    }

    #[test]
    fn every_structure_and_exit_is_walked_to_from_the_core_and_the_ring_from_it_or_an_exit() {
        let rules = Rules::vanilla();
        let walled = fabot_base::testing::world(include_str!(
            "../../scripts/scenario/worlds/walled-exit-e9s9.json"
        ));
        let real = fabot_base::testing::real_rooms().step_by(50);
        let grounds = [fx::pocketed()].into_iter().chain(ground::of_room(&walled.rooms[0]));
        let grounds = grounds.chain(real.filter_map(|r| ground::of_room(&r.room())));
        let drawn = (grounds.filter_map(|g| Some((of_ground(&rules, &g)?, g))))
            .map(|(p, g)| (g, p))
            .chain(fx::samples(4).take(fx::MAX_TEST).filter_map(|c| fx::plan_of(&c)));
        for (ground, plan) in drawn {
            let exits: Vec<Tile> = (tiles::all())
                .filter(|t| tiles::on_edge(*t) && ground.terrain[*t] != place::WALL)
                .chain([plan.anchor])
                .collect();
            let beside =
                |seen: &[bool], i: &Item| tiles::within(1, i.tile).into_iter().any(|n| seen[n]);
            let from_core = walked(&ground, &plan, &[plan.anchor]);
            for i in &plan.items {
                assert!(beside(&from_core, i), "{i:?} unreached from the core");
            }
            let unwalled = Plan { ring: Vec::new(), ..plan.clone() };
            let bare = walked(&ground, &unwalled, &[plan.anchor]);
            for t in exits.iter().filter(|t| bare[**t]) {
                assert!(from_core[*t], "exit {t:?} walled off");
            }
            let from_any = walked(&ground, &plan, &exits);
            for i in &plan.ring {
                assert!(beside(&from_any, i), "{i:?} unreached");
            }
        }
    }

    #[test]
    fn a_pocket_walled_off_loses_only_its_roads_ramparts_and_walls() {
        let rules = Rules::vanilla();
        let rooms = fx::samples(4).take(fx::MAX_TEST);
        let grounds = rooms.filter_map(|c| ground::of_room(&c.room));
        for ground in grounds.chain([fx::nook()]) {
            let (walls, exits) = place::distances(&ground.terrain);
            let Some(center) = build::centers(&ground, &walls, &exits).first().copied() else {
                continue;
            };
            let Some(b) = build::build(&rules, &ground, (&walls, &exits), center) else {
                continue;
            };
            use StructureType::{Rampart, Road, Wall};
            for i in drawn(&rules, &ground, &b).1 {
                assert!(matches!(i.kind, Road | Rampart | Wall), "{i:?} walled off");
            }
        }
    }

    #[test]
    fn the_plan_is_the_same_whatever_order_the_room_lists_things_in_and_however_spread() {
        let rules = Rules::vanilla();
        let mut rng = fx::Rng::new(7);
        for _ in 0..6 {
            let colony = fx::colony(&mut rng);
            let seed = rng.choose(1, 1_000_000) as u64;
            let shuffled = fx::shuffled(&colony, seed);
            let (Some(a), Some(b)) =
                (ground::of_room(&colony.room), ground::of_room(&shuffled.room))
            else {
                continue;
            };
            let whole = of_ground(&rules, &a);
            let mut drawing = Drawing::of(&b);
            let spread = loop {
                if let Drawn::Done(done) = drawing.advance(&rules, &b, &mut 1) {
                    break done;
                }
            };
            match (whole, spread) {
                (Some(x), Some(y)) => assert!(fx::same(&x, &y) && x.ring == y.ring),
                (None, None) => (),
                _ => panic!("a plan one way, none the other"),
            }
        }
    }
}
