//! Hivemind's variation generator and plan scorer (MIT, © David Franke): the
//! room centres and harvest-bay spawns it tries, and how it picks one.
use fabot_base::tiles::{self, Tile, index};
use fabot_base::world::Rules;

use super::build::{self, Built, Variation};
use super::ground::Ground;
use super::path::{Goal, Search};
use super::place::{BLOCKED, Labels, ROAD, SWAMP, WALL, xy};

/// `varyRoomCenter` then `varySourceSpawns`, in Hivemind's key order.
pub fn variations(ground: &Ground, walls: &[u8], exits: &[u8]) -> Vec<Variation> {
    let Some(controller) = ground.controller else {
        return Vec::new();
    };
    let mut cores: Vec<Tile> = Vec::new();
    for x in 0..50 {
        for y in 0..50 {
            let t = index(x, y);
            if ground.terrain[t.index()] & WALL == 0
                && (3..255).contains(&walls[t.index()])
                && exits[t.index()] > 8
            {
                cores.push(t);
            }
        }
    }
    let (mut cx, mut cy) = xy(controller);
    let mut count = 1;
    let rocks = (build::exit_centers(&ground.terrain).into_iter())
        .chain(ground.minerals.iter().copied())
        .chain(ground.sources.iter().copied());
    for t in rocks {
        count += 1;
        cx += tiles::x_of(t);
        cy += tiles::y_of(t);
    }
    let mid = index(cx.div_euclid(count), cy.div_euclid(count));
    let Some(weighted) = (cores.iter().copied()).min_by_key(|t| tiles::range(*t, mid)) else {
        return Vec::new();
    };

    let mut centers = vec![weighted];
    let mut grid: Vec<((i32, i32), Tile)> = Vec::new();
    for t in &cores {
        let key = (tiles::x_of(*t) * 3 / 50, tiles::y_of(*t) * 3 / 50);
        match grid.iter_mut().find(|(k, _)| *k == key) {
            Some(slot) => {
                if walls[slot.1.index()] < walls[t.index()] {
                    slot.1 = *t;
                }
            }
            None => grid.push((key, *t)),
        }
    }
    centers.extend(grid.into_iter().map(|(_, t)| t));

    let n = ground.sources.len();
    let mut out = Vec::new();
    for center in centers {
        let mut spawns: Vec<Vec<bool>> = vec![Vec::new()];
        for _ in 0..n {
            spawns = (spawns.into_iter())
                .flat_map(|s| {
                    let mut with = s.clone();
                    with.push(true);
                    let mut without = s;
                    without.push(false);
                    [without, with]
                })
                .collect();
        }
        out.extend(
            spawns
                .into_iter()
                .map(|spawns| Variation { center, spawns }),
        );
    }
    out
}

const OBSTACLES: [&str; 14] = [
    "road",
    "harvester",
    "bay_center",
    "wall",
    "spawn",
    "extension",
    "link",
    "storage",
    "tower",
    "observer",
    "powerSpawn",
    "lab",
    "terminal",
    "nuker",
];

fn navigation(plan: &Labels, terrain: &[u8]) -> Vec<u8> {
    let mut m = vec![0u8; tiles::COUNT];
    for label in plan
        .labels()
        .iter()
        .filter(|l| OBSTACLES.contains(&l.as_str()) || *l == "factory")
    {
        for t in plan.of(label) {
            if label != "road" {
                m[t.index()] = BLOCKED;
            } else if m[t.index()] == 0 && terrain[t.index()] & WALL == 0 {
                m[t.index()] = ROAD;
            }
        }
    }
    m
}

fn amount(rules: &Rules, plan: &Labels, label: &str) -> f64 {
    let cap = match label {
        "rampart" | "road" | "wall.quad" => 2500,
        other => rules.allowed(other, 8),
    };
    plan.count(label).min(cap.max(0) as usize) as f64
}

pub(super) fn effect(a: Tile, b: Tile) -> f64 {
    let r = (tiles::range(a, b) + 2).clamp(5, 20);
    1.0 - f64::from(r - 5) / 15.0
}

/// Departure (owner): each source's and the controller's walk to the first
/// spawn built, not a harvest bay's (`plan::order`), which Hivemind weighs
/// 0.003 and 0.002 to its nearest: ours hauls from that one spawn until RCL7.
const SOURCE_PULL: f64 = 0.02;

/// `getScore`'s total, its terms summed as lodash 3's `_.sum` sums them.
pub fn score(rules: &Rules, built: &Built, terrain: &[u8], exits: &[u8], ground: &Ground) -> f64 {
    let plan = &built.plan;
    let a = |l: &str| amount(rules, plan, l);
    let mut structures = 0.0;
    for (w, l) in [
        (0.01, "extension"),
        (0.01, "factory"),
        (0.01, "observer"),
        (0.02, "lab"),
        (0.02, "nuker"),
        (0.05, "extractor"),
        (0.05, "powerSpawn"),
        (0.1, "spawn"),
        (0.2, "terminal"),
        (0.2, "tower"),
    ] {
        structures += w * a(l);
    }
    structures += a("storage");

    let mut maintenance = 0.0;
    maintenance -= 0.001 * a("rampart") * 300.0 / 100.0;
    maintenance -= 0.001 * a("container") * 5000.0 / 500.0;
    for t in plan.of("road") {
        let mut factor = 0.002;
        if terrain[t.index()] == SWAMP {
            factor *= 5.0;
        }
        if terrain[t.index()] & WALL != 0 {
            factor *= 150.0;
        }
        maintenance -= factor * 100.0 / 1000.0;
    }

    let ramparts = plan.of("rampart");
    let towers_at = plan.of("tower");
    let towers = if ramparts.is_empty() || towers_at.is_empty() {
        0.0
    } else {
        let mut total = 0.0;
        for r in &ramparts {
            for t in &towers_at {
                total += effect(*t, *r);
            }
        }
        0.2 * total / ramparts.len() as f64
    };

    let mut close = 0.0;
    for r in &ramparts {
        let d = f64::from(exits[r.index()]);
        if d < 6.0 {
            close += (6.0 - d) * (6.0 - d);
        }
    }
    let defense = (-0.003 * close) - 0.001 * a("wall.quad");

    let nav = navigation(plan, terrain);
    let search = Search {
        terrain,
        matrix: &nav,
        plain: 2,
        swamp: 10,
        weight: 1.2,
    };
    let length = |from: Tile, goals: &[Goal]| -> f64 {
        let found = search.run(from, goals);
        if found.incomplete {
            1000.0
        } else {
            found.path.len() as f64
        }
    };
    let harvest = plan.of("harvester");
    let spawns: Vec<Goal> = ((0..10).flat_map(|i| plan.of(&format!("spawn.{i}"))))
        .find(|t| harvest.iter().all(|h| tiles::range(*h, *t) > 1))
        .map(|tile| Goal { tile, range: 1 })
        .into_iter()
        .collect();
    let center = [Goal {
        tile: built.center,
        range: 0,
    }];
    let to_spawn = |t: Tile| {
        if spawns.is_empty() {
            1000.0
        } else {
            length(t, &spawns)
        }
    };
    let sum = |ts: Vec<Tile>, f: &dyn Fn(Tile) -> f64| ts.into_iter().rev().map(f).sum::<f64>();
    let upgrader = (plan.of("container.controller").first().copied()).or(ground.controller);
    let mut distance = 0.0;
    distance -= SOURCE_PULL * sum(plan.of("harvester"), &to_spawn);
    if let Some(u) = upgrader {
        distance -= SOURCE_PULL * to_spawn(u);
    }
    for m in &ground.minerals {
        distance -= 0.001 * to_spawn(*m);
    }
    distance -= 0.005 * sum(plan.of("bay_center"), &|t| length(t, &center));
    distance -= 0.001 * sum(plan.of("harvester"), &|t| length(t, &center));
    if let Some(u) = upgrader {
        distance -= 0.01 * length(u, &center);
    }
    distance -= 0.001 * sum(plan.of("lab"), &|t| length(t, &center));
    distance -= 0.001 * sum(plan.of("tower"), &|t| length(t, &center));

    0.0 + distance + defense + towers + maintenance + structures
}
