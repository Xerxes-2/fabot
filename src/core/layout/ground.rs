//! Built from the room alone, so a plan drawn on it moves only when the
//! ground does.
use crate::core::engine;
use crate::core::grid::{self, Weight};
use crate::core::tiles::{self, Tile};
use crate::core::world::{Room, terrain};

#[derive(Clone, Debug)]
pub struct Ground {
    pub room: String,
    /// Our own structures are not read: the plan is drawn on the ground,
    /// never on itself.
    pub weights: Vec<Weight>,
    pub free: Vec<bool>,
    pub sources: Vec<Tile>,
    pub minerals: Vec<Tile>,
    pub controller: Option<Tile>,
}

/// Upgraders work within range 3 of the controller; this leaves them the two
/// rings inside that.
const CONTROLLER_KEEP: i32 = 2;

/// The ring inside the border where the engine refuses a structure beside an
/// exit; roads and containers are allowed there but nothing here plans one.
const EXIT_KEEP: i32 = 1;

/// Holds the plan's core of hub, spawns, towers and near extensions: a design
/// constant of the lattice (`Plan`), not an engine fact.
pub const CORE_RADIUS: i32 = 4;

/// The centres of the four lattice cells beside an anchor (`Plan`).
pub const SPAWN_OFFSETS: [(i32, i32); 4] = [(2, 0), (-2, 0), (0, 2), (0, -2)];

/// The only part of a room's ground that can change.
pub fn obstacles(room: &Room) -> Vec<Tile> {
    let mut found: Vec<Tile> = room
        .structures
        .iter()
        .filter(|s| !s.owner.is_ours() && !s.walkable)
        .map(|s| tiles::of_pos(&s.pos))
        .collect();
    found.sort_unstable();
    found
}

pub fn of_room(room: &Room) -> Option<Ground> {
    if room.terrain.len() != tiles::COUNT {
        return None;
    }

    let mut weights: Vec<Weight> = room
        .terrain
        .iter()
        .map(|t| {
            if t & terrain::WALL != 0 {
                None
            } else if t & terrain::SWAMP != 0 {
                grid::weight(engine::SWAMP_WEIGHT)
            } else {
                grid::weight(engine::PLAIN_WEIGHT)
            }
        })
        .collect();

    for t in obstacles(room) {
        weights[t] = None;
    }

    let mut sources: Vec<Tile> = room.sources.iter().map(|s| tiles::of_pos(&s.pos)).collect();
    sources.sort_unstable();
    let mut minerals: Vec<Tile> = room
        .minerals
        .iter()
        .map(|m| tiles::of_pos(&m.pos))
        .collect();
    minerals.sort_unstable();

    let controller = room.controller.as_ref().map(|c| tiles::of_pos(&c.pos));

    for t in &sources {
        weights[*t] = None;
    }
    for t in &minerals {
        weights[*t] = None;
    }
    if let Some(c) = controller {
        weights[c] = None;
    }

    let mut free = vec![false; tiles::COUNT];
    let edge = EXIT_KEEP + 1;

    for i in tiles::all() {
        let x = tiles::x_of(i);
        let y = tiles::y_of(i);
        free[i] = weights[i].is_some()
            && x >= edge
            && y >= edge
            && x < tiles::SIDE - edge
            && y < tiles::SIDE - edge;
    }

    for rock in sources.iter().chain(minerals.iter()) {
        for t in tiles::within(1, *rock) {
            free[t] = false;
        }
    }

    if let Some(c) = controller {
        for t in tiles::within(CONTROLLER_KEEP, c) {
            free[t] = false;
        }
    }

    Some(Ground {
        room: room.name.clone(),
        weights,
        free,
        sources,
        minerals,
        controller,
    })
}

pub fn spawn_slots(anchor: Tile) -> Vec<Tile> {
    SPAWN_OFFSETS
        .iter()
        .map(|(dx, dy)| (tiles::x_of(anchor) + dx, tiles::y_of(anchor) + dy))
        .filter(|(x, y)| tiles::in_room(*x, *y))
        .map(|(x, y)| tiles::index(x, y))
        .collect()
}

// Range, not path cost: the cheapest measure of the walk the colony pays
// from its core.
fn reach_sum(ground: &Ground, tile: Tile) -> i32 {
    let mut sum = 0;
    for s in &ground.sources {
        sum += tiles::range(tile, *s);
    }
    sum + ground
        .controller
        .map(|c| tiles::range(tile, c))
        .unwrap_or(0)
}

fn openness(ground: &Ground) -> Vec<i32> {
    let n = tiles::SIDE + 1;
    let mut sums = vec![0i32; (n * n) as usize];

    for y in 0..tiles::SIDE {
        for x in 0..tiles::SIDE {
            let here = if ground.free[tiles::index(x, y)] {
                1
            } else {
                0
            };
            sums[((y + 1) * n + x + 1) as usize] =
                here + sums[(y * n + x + 1) as usize] + sums[((y + 1) * n + x) as usize]
                    - sums[(y * n + x) as usize];
        }
    }

    tiles::all()
        .map(|t| {
            let x0 = 0.max(tiles::x_of(t) - CORE_RADIUS);
            let y0 = 0.max(tiles::y_of(t) - CORE_RADIUS);
            let x1 = (tiles::SIDE - 1).min(tiles::x_of(t) + CORE_RADIUS) + 1;
            let y1 = (tiles::SIDE - 1).min(tiles::y_of(t) + CORE_RADIUS) + 1;

            sums[(y1 * n + x1) as usize]
                - sums[(y0 * n + x1) as usize]
                - sums[(y1 * n + x0) as usize]
                + sums[(y0 * n + x0) as usize]
        })
        .collect()
}

fn valid(ground: &Ground, anchor: Tile) -> bool {
    let x = tiles::x_of(anchor);
    let y = tiles::y_of(anchor);

    x >= 2
        && y >= 2
        && x < tiles::SIDE - 2
        && y < tiles::SIDE - 2
        && ground.weights[anchor].is_some()
        && spawn_slots(anchor).iter().any(|s| ground.free[*s])
}

pub fn anchors(ground: &Ground, spawns: &[Tile]) -> Vec<Tile> {
    let opens = openness(ground);

    let candidates: Vec<Tile> = if spawns.is_empty() {
        tiles::all().collect()
    } else {
        let mut seen: Vec<Tile> = Vec::new();
        for s in spawns {
            for (dx, dy) in SPAWN_OFFSETS {
                let x = tiles::x_of(*s) - dx;
                let y = tiles::y_of(*s) - dy;
                if tiles::in_room(x, y) {
                    let t = tiles::index(x, y);
                    if !seen.contains(&t) {
                        seen.push(t);
                    }
                }
            }
        }
        seen
    };

    let mut valid_ones: Vec<Tile> = candidates
        .into_iter()
        .filter(|a| valid(ground, *a))
        .collect();

    valid_ones.sort_by_key(|a| (-opens[*a], reach_sum(ground, *a), *a));
    valid_ones
}
