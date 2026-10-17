//! Built from the room alone, so a plan drawn on it moves only when the
//! ground does.
use crate::core::engine;
use crate::core::tiles;
use crate::core::world::{terrain, Room};

#[derive(Clone, Debug)]
pub struct Ground {
    pub room: String,
    /// -1 where no creep crosses. Our own structures are not read: the plan is
    /// drawn on the ground, never on itself.
    pub weights: Vec<i32>,
    pub free: Vec<bool>,
    pub sources: Vec<i32>,
    pub minerals: Vec<i32>,
    pub controller: Option<i32>,
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
pub fn obstacles(room: &Room) -> Vec<i32> {
    let mut found: Vec<i32> = room
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

    let mut weights = vec![0i32; tiles::COUNT];

    for (i, t) in room.terrain.iter().enumerate().take(tiles::COUNT) {
        weights[i] = if t & terrain::WALL != 0 {
            -1
        } else if t & terrain::SWAMP != 0 {
            engine::SWAMP_WEIGHT
        } else {
            engine::PLAIN_WEIGHT
        };
    }

    for t in obstacles(room) {
        weights[t as usize] = -1;
    }

    let mut sources: Vec<i32> = room.sources.iter().map(|s| tiles::of_pos(&s.pos)).collect();
    sources.sort_unstable();
    let mut minerals: Vec<i32> = room.minerals.iter().map(|m| tiles::of_pos(&m.pos)).collect();
    minerals.sort_unstable();

    let controller = room.controller.as_ref().map(|c| tiles::of_pos(&c.pos));

    for t in &sources {
        weights[*t as usize] = -1;
    }
    for t in &minerals {
        weights[*t as usize] = -1;
    }
    if let Some(c) = controller {
        weights[c as usize] = -1;
    }

    let mut free = vec![false; tiles::COUNT];
    let edge = EXIT_KEEP + 1;

    for i in 0..tiles::COUNT as i32 {
        let x = tiles::x_of(i);
        let y = tiles::y_of(i);
        free[i as usize] = weights[i as usize] >= 0
            && x >= edge
            && y >= edge
            && x < tiles::SIDE - edge
            && y < tiles::SIDE - edge;
    }

    for rock in sources.iter().chain(minerals.iter()) {
        for t in tiles::within(1, *rock) {
            free[t as usize] = false;
        }
    }

    if let Some(c) = controller {
        for t in tiles::within(CONTROLLER_KEEP, c) {
            free[t as usize] = false;
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

pub fn spawn_slots(anchor: i32) -> Vec<i32> {
    SPAWN_OFFSETS
        .iter()
        .map(|(dx, dy)| {
            let x = tiles::x_of(anchor) + dx;
            let y = tiles::y_of(anchor) + dy;
            if tiles::in_room(x, y) {
                tiles::index(x, y)
            } else {
                -1
            }
        })
        .collect()
}

// Range, not path cost: the cheapest measure of the walk the colony pays
// from its core.
fn reach_sum(ground: &Ground, tile: i32) -> i32 {
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
            let here = if ground.free[tiles::index(x, y) as usize] {
                1
            } else {
                0
            };
            sums[((y + 1) * n + x + 1) as usize] =
                here + sums[(y * n + x + 1) as usize] + sums[((y + 1) * n + x) as usize]
                    - sums[(y * n + x) as usize];
        }
    }

    (0..tiles::COUNT as i32)
        .map(|t| {
            let x0 = 0.max(tiles::x_of(t) - CORE_RADIUS);
            let y0 = 0.max(tiles::y_of(t) - CORE_RADIUS);
            let x1 = (tiles::SIDE - 1).min(tiles::x_of(t) + CORE_RADIUS) + 1;
            let y1 = (tiles::SIDE - 1).min(tiles::y_of(t) + CORE_RADIUS) + 1;

            sums[(y1 * n + x1) as usize] - sums[(y0 * n + x1) as usize]
                - sums[(y1 * n + x0) as usize]
                + sums[(y0 * n + x0) as usize]
        })
        .collect()
}

fn valid(ground: &Ground, anchor: i32) -> bool {
    let x = tiles::x_of(anchor);
    let y = tiles::y_of(anchor);

    x >= 2
        && y >= 2
        && x < tiles::SIDE - 2
        && y < tiles::SIDE - 2
        && ground.weights[anchor as usize] >= 0
        && spawn_slots(anchor)
            .iter()
            .any(|s| *s >= 0 && ground.free[*s as usize])
}

pub fn anchors(ground: &Ground, spawns: &[i32]) -> Vec<i32> {
    let opens = openness(ground);

    let candidates: Vec<i32> = if spawns.is_empty() {
        (0..tiles::COUNT as i32).collect()
    } else {
        let mut seen: Vec<i32> = Vec::new();
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

    let mut valid_ones: Vec<i32> = candidates
        .into_iter()
        .filter(|a| valid(ground, *a))
        .collect();

    valid_ones.sort_by_key(|a| {
        (
            -opens[*a as usize],
            reach_sum(ground, *a),
            *a,
        )
    });
    valid_ones
}
