//! Hivemind's room plan (MIT, © David Franke): every variation built and
//! scored as its planner does, the best one taken, and each structure given
//! the level its room manager builds it at.
use fabot_atlas::Atlas;
use fabot_base::engine;
use fabot_base::tiles::{self, Tile};
use fabot_base::world::{Rules, StructureType};

use super::build::{self, Built};
use super::ground::Ground;
use super::place::{self, Labels, SWAMP};
use super::score;
use super::shape::{self, Item, Plan};

pub fn level_of(rules: &Rules, kind: StructureType, rank: i32) -> i32 {
    (0..=engine::CONTROLLER_MAX_LEVEL)
        .find(|level| rules.allowed(kind, *level) > rank)
        .unwrap_or(engine::CONTROLLER_MAX_LEVEL + 1)
}

/// At the first level a room's energy goes to its controller alone.
pub fn haul_level(rules: &Rules) -> i32 {
    level_of(rules, StructureType::Extension, 0)
}

/// Not links, containers or roads: they are seated by what they serve.
pub fn sized() -> Vec<StructureType> {
    use StructureType::*;
    vec![
        Spawn, Extension, Tower, Storage, Terminal, Factory, PowerSpawn, Nuker, Observer, Lab,
    ]
}

/// Sharing one of the container's seats, so a body there draws from either.
pub fn controller_link(
    walkable: impl Fn(Tile) -> bool,
    linkable: impl Fn(Tile) -> bool,
    standable: impl Fn(Tile) -> bool,
    controller: Tile,
    container: Tile,
) -> Option<Tile> {
    let seated: Vec<(Tile, Vec<Tile>)> = tiles::within(engine::UPGRADE_RANGE, controller)
        .into_iter()
        .filter(|t| tiles::range(*t, container) > 1 && linkable(*t))
        .map(|t| (t, Atlas::upgrader_seats(&walkable, controller, t)))
        .filter(|(_, seats)| {
            seats.iter().any(|n| tiles::range(*n, container) <= 1)
                && seats.iter().all(|n| standable(*n))
        })
        .collect();

    (seated.into_iter())
        .min_by_key(|(t, seats)| (-(seats.len() as i32), tiles::range(*t, container), *t))
        .map(|(t, _)| t)
}

/// Every colony walls its core from here (owner, 2026-11-14), as Hivemind does.
pub const RING_LEVEL: i32 = 4;

/// The room manager's order a kind is built in at a level.
fn order(plan: &Labels, kind: &str, level: i32, controller: Option<Tile>) -> Vec<Tile> {
    let both = |a: &str, b: &str| -> Vec<Tile> {
        (plan.of(a).into_iter())
            .filter(|t| plan.has(b, *t))
            .collect()
    };
    let numbered = |prefix: &str| -> Vec<Tile> {
        (0..10)
            .flat_map(|i| plan.of(&format!("{prefix}.{i}")))
            .collect()
    };
    let mut out: Vec<Tile> = match kind {
        // Our pioneers dig from any tile beside a source, so a harvest bay's
        // spawn is built after the extension bays' (Hivemind builds its first).
        "spawn" => {
            let harvest = plan.of("harvester");
            let (bay, dug): (Vec<Tile>, Vec<Tile>) = (numbered(kind).into_iter())
                .partition(|t| harvest.iter().all(|h| tiles::range(*h, *t) > 1));
            bay.into_iter().chain(dug).collect()
        }
        "tower" => numbered(kind),
        "extension" => {
            let mut v = if level >= 3 {
                both("extension.harvester", "extension")
            } else {
                Vec::new()
            };
            for i in 0..10 {
                v.extend(both(&format!("extension.bay.{i}"), "extension"));
            }
            v.extend(both("extension.bay", "extension"));
            v
        }
        "link" => {
            let mut v = plan.of("link.controller");
            let mut sources = plan.of("link.source");
            if let Some(c) = controller {
                sources.sort_by_key(|t| -tiles::range(*t, c));
            }
            v.extend(sources);
            v.extend(plan.of("link.storage"));
            v
        }
        "container" if level < 2 => return Vec::new(),
        "container" => {
            let mut v = plan.of("container.source");
            v.extend(plan.of("container.controller"));
            if level >= 6 {
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
fn leveled(
    rules: &Rules,
    plan: &Labels,
    kind: StructureType,
    controller: Option<Tile>,
) -> Vec<(Tile, i32)> {
    let mut built: Vec<(Tile, i32)> = Vec::new();
    for level in 1..=engine::CONTROLLER_MAX_LEVEL {
        let allowed = rules.allowed(kind, level).max(0) as usize;
        for t in order(plan, &kind.to_string(), level, controller) {
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

/// Where the room manager's `manageStructures` reaches a kind, but a source
/// container other seats ring goes before the extensions: its harvester digs
/// beside the crews (main's planner), and the nuker, which pays only loaded
/// and fired, goes last.
fn priority(kind: StructureType, level: i32, crowded: bool) -> i32 {
    use StructureType::*;
    match kind {
        Tower => 0,
        Spawn => 1,
        Container if level < 6 && crowded => 2,
        Extension => 3,
        Container if level < 6 => 4,
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
    use StructureType::*;
    let plan = &built.plan;
    let fixed: Vec<Tile> = (ground.sources.iter().chain(&ground.minerals))
        .chain(&ground.controller)
        .copied()
        .collect();
    let mut keyed: Vec<(i32, i32, usize, Item)> = Vec::new();
    let mut push = |kind: StructureType, tile: Tile, level: i32| {
        if !fixed.contains(&tile) || kind == Extractor {
            let seq = keyed.len();
            let rock = |r: &Tile| tiles::range(*r, tile) == 1;
            let seat = |n: &&Tile| ground.terrain[**n] != place::WALL && !fixed.contains(n);
            let crowded = (ground.sources.iter())
                .any(|r| rock(r) && tiles::around(*r).iter().filter(seat).count() > 1);
            keyed.push((
                priority(kind, level, crowded),
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
        for (t, level) in leveled(rules, plan, kind, ground.controller) {
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
        for (t, level) in leveled(rules, plan, kind, ground.controller) {
            push(Rampart, t, level.max(RING_LEVEL));
        }
    }
    // Our sites stack nothing on a container; Hivemind paves the controller's.
    for t in plan
        .of("road")
        .into_iter()
        .filter(|t| !plan.has("container", *t))
    {
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
    let mut ring: Vec<Item> = main
        .into_iter()
        .chain(ramps)
        .map(item(Rampart, 3))
        .collect();
    for (label, level) in [("wall.deco", 4), ("wall.quad", 6)] {
        ring.extend(plan.of(label).into_iter().map(item(Wall, level)));
    }
    ring.sort_by_key(|i| i.level);

    let unfit = (sized().into_iter())
        .filter(|kind| {
            (items.iter().filter(|i| i.kind == *kind).count() as i32)
                < rules.allowed(*kind, engine::CONTROLLER_MAX_LEVEL)
        })
        .collect();

    let planned: Vec<&Item> = items.iter().chain(&ring).collect();
    let open = |t: Tile| ground.terrain[t] != place::WALL;
    let muster = shape::muster(ground.room, built.center, &planned, open);
    Plan {
        room: ground.room,
        anchor: built.center,
        items,
        ring,
        unfit,
        muster,
    }
}

/// Hivemind's search, a few variations a tick as its planner spreads them.
#[derive(Clone, Debug, PartialEq)]
pub struct Drawing {
    walls: Vec<u8>,
    exits: Vec<u8>,
    next: usize,
    best: Option<(f64, usize)>,
}

pub enum Drawn {
    Drawing,
    /// The first of the best scored, or none where no variation fits.
    Done(Option<Plan>),
}

impl Drawing {
    pub fn of(ground: &Ground) -> Drawing {
        let (walls, exits) = place::distances(&ground.terrain);
        Drawing {
            walls,
            exits,
            next: 0,
            best: None,
        }
    }

    /// Builds variations while `budget` lasts.
    pub fn advance(&mut self, rules: &Rules, ground: &Ground, budget: &mut usize) -> Drawn {
        let dist = (&self.walls[..], &self.exits[..]);
        let variations = score::variations(ground, dist.0, dist.1);
        while self.next < variations.len() {
            if *budget == 0 {
                return Drawn::Drawing;
            }
            *budget -= 1;
            if let Some(b) = build::build(rules, ground, dist, &variations[self.next]) {
                let s = score::score(rules, &b, &ground.terrain, dist.1, ground);
                if self.best.is_none_or(|(top, _)| s > top) {
                    self.best = Some((s, self.next));
                }
            }
            self.next += 1;
        }
        let best = self
            .best
            .and_then(|(_, i)| build::build(rules, ground, dist, &variations[i]));
        Drawn::Done(best.map(|b| to_plan(rules, ground, &b)))
    }
}

pub fn of_ground(rules: &Rules, ground: &Ground) -> Option<Plan> {
    let mut budget = usize::MAX;
    match Drawing::of(ground).advance(rules, ground, &mut budget) {
        Drawn::Done(plan) => plan,
        Drawn::Drawing => None,
    }
}

/// The first variation alone: whether a room fits a whole plan, at a
/// fraction of the search.
pub fn first_of(rules: &Rules, ground: &Ground) -> Option<Plan> {
    let (walls, exits) = place::distances(&ground.terrain);
    let v = score::variations(ground, &walls, &exits)
        .into_iter()
        .next()?;
    build::build(rules, ground, (&walls, &exits), &v).map(|b| to_plan(rules, ground, &b))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixtures as fx;
    use crate::ground;

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
            let rocks: Vec<Tile> = (ground.sources.iter().chain(&ground.minerals))
                .chain(&ground.controller)
                .copied()
                .collect();
            for level in 0..=engine::CONTROLLER_MAX_LEVEL {
                for kind in sized()
                    .into_iter()
                    .chain([StructureType::Link, StructureType::Container])
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
            let core: Vec<(Tile, i32)> = (plan.items.iter())
                .filter(|i| !bay(i))
                .filter_map(|i| match &i.kind {
                    StructureType::Link => Some((i.tile, 0)),
                    k if shot.contains(k) => Some((i.tile, engine::RANGED_RANGE)),
                    _ => None,
                })
                .chain(ground.controller.map(|c| (c, 1)))
                .collect();

            let mut reached = vec![false; tiles::COUNT];
            let mut queue: Vec<Tile> = (tiles::all())
                .filter(|t| tiles::on_edge(*t) && open(*t))
                .collect();
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
            for (c, reach) in &core {
                for t in tiles::within(*reach, *c) {
                    assert!(
                        !reached[t] || beside_exit(t),
                        "{t} in reach of {c} outside the ring"
                    );
                }
            }
        }
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
            let last = level_of(
                &rules,
                StructureType::Extension,
                early + bays.len() as i32 - 1,
            );
            for b in bays {
                assert!(b.level <= last, "{} at level {}", b.tile, b.level);
            }
        }
    }

    #[test]
    fn a_muster_holds_no_planned_road_and_no_spawns_door_within_its_reach() {
        for colony in fx::samples(4).take(fx::MAX_TEST) {
            let Some((_, plan)) = fx::plan_of(&colony) else {
                continue;
            };
            let Some(muster) = plan.muster else {
                continue;
            };
            let at = tiles::of_pos(&muster);
            for i in plan.items.iter().chain(&plan.ring) {
                let reach = tiles::range(i.tile, at);
                assert!(
                    !(i.kind == StructureType::Road
                        && reach <= fabot_base::declarations::MUSTER_RANGE),
                    "road at {}",
                    i.tile
                );
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
                assert!(
                    bare.is_empty() && roads.count() >= 1,
                    "{bare:?} bare beside {c:?}"
                );
            }
        }
    }

    #[test]
    fn every_structure_can_be_walked_to_from_the_core() {
        for colony in fx::samples(4).take(fx::MAX_TEST) {
            let Some((ground, plan)) = fx::plan_of(&colony) else {
                continue;
            };
            let seen = fx::reached(&ground, &plan);
            for i in &plan.items {
                let beside = tiles::within(1, i.tile).into_iter().any(|n| seen[n]);
                assert!(beside, "{i:?} unreached");
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
            let (Some(a), Some(b)) = (
                ground::of_room(&colony.room),
                ground::of_room(&shuffled.room),
            ) else {
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
