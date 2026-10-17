//! A tile is `y * 50 + x`, the terrain raw buffer's index, so every per-room
//! array in the Atlas reads the terrain's own order.
use std::sync::OnceLock;

use crate::core::engine;
use crate::core::world::Pos;

pub const SIDE: i32 = engine::ROOM_SIDE;
pub const COUNT: usize = 2500;

pub fn index(x: i32, y: i32) -> i32 {
    y * SIDE + x
}

pub fn x_of(tile: i32) -> i32 {
    tile % SIDE
}

pub fn y_of(tile: i32) -> i32 {
    tile / SIDE
}

pub fn in_room(x: i32, y: i32) -> bool {
    (0..SIDE).contains(&x) && (0..SIDE).contains(&y)
}

pub fn of_pos(pos: &Pos) -> i32 {
    index(pos.x, pos.y)
}

pub fn to_pos(room: &str, tile: i32) -> Pos {
    Pos::new(room, x_of(tile), y_of(tile))
}

/// The engine's `getRangeTo`.
pub fn range(a: i32, b: i32) -> i32 {
    (x_of(a) - x_of(b)).abs().max((y_of(a) - y_of(b)).abs())
}

/// The engine's directions, TOP = 1 clockwise to TOP_LEFT = 8.
pub const TOP: i32 = 1;
pub const TOP_RIGHT: i32 = 2;
pub const RIGHT: i32 = 3;
pub const BOTTOM_RIGHT: i32 = 4;
pub const BOTTOM: i32 = 5;
pub const BOTTOM_LEFT: i32 = 6;
pub const LEFT: i32 = 7;
pub const TOP_LEFT: i32 = 8;

pub fn dx(direction: i32) -> i32 {
    match direction {
        2..=4 => 1,
        6..=8 => -1,
        _ => 0,
    }
}

/// Rows grow downward.
pub fn dy(direction: i32) -> i32 {
    match direction {
        8 | 1 | 2 => -1,
        4..=6 => 1,
        _ => 0,
    }
}

pub fn neighbour(tile: i32, direction: i32) -> i32 {
    let x = x_of(tile) + dx(direction);
    let y = y_of(tile) + dy(direction);
    if in_room(x, y) { index(x, y) } else { -1 }
}

fn around_table() -> &'static Vec<Vec<i32>> {
    static TABLE: OnceLock<Vec<Vec<i32>>> = OnceLock::new();
    TABLE.get_or_init(|| {
        (0..COUNT)
            .map(|t| {
                let mut ns = Vec::new();
                for d in 1..=8 {
                    let n = neighbour(t as i32, d);
                    if n >= 0 {
                        ns.push(n);
                    }
                }
                ns
            })
            .collect()
    })
}

/// The in-room neighbours, in direction order.
pub fn around(tile: i32) -> &'static [i32] {
    &around_table()[tile as usize]
}

/// The neighbour costing least under `bound`, the first in direction order
/// on a tie.
pub fn least_around(tile: i32, bound: i32, mut cost: impl FnMut(i32) -> i32) -> Option<i32> {
    let mut best = None;
    let mut least = bound;
    for &n in around(tile) {
        let c = cost(n);
        if c < least {
            best = Some(n);
            least = c;
        }
    }
    best
}

pub fn direction(from: i32, onto: i32) -> i32 {
    let ddx = x_of(onto) - x_of(from);
    let ddy = y_of(onto) - y_of(from);
    match (ddx, ddy) {
        (0, -1) => TOP,
        (1, -1) => TOP_RIGHT,
        (1, 0) => RIGHT,
        (1, 1) => BOTTOM_RIGHT,
        (0, 1) => BOTTOM,
        (-1, 1) => BOTTOM_LEFT,
        (-1, 0) => LEFT,
        (-1, -1) => TOP_LEFT,
        _ => 0,
    }
}

pub fn within(radius: i32, tile: i32) -> Vec<i32> {
    let x = x_of(tile);
    let y = y_of(tile);
    let mut tiles = Vec::new();
    for ty in (y - radius).max(0)..=(y + radius).min(SIDE - 1) {
        for tx in (x - radius).max(0)..=(x + radius).min(SIDE - 1) {
            tiles.push(index(tx, ty));
        }
    }
    tiles
}

const LAST: i32 = SIDE - 1;

pub fn on_edge(tile: i32) -> bool {
    let x = x_of(tile);
    let y = y_of(tile);
    x == 0 || y == 0 || x == LAST || y == LAST
}

/// The engine's FIND_EXIT_* values; a corner lies on two borders and lands
/// nowhere.
pub fn exit_of(tile: i32) -> i32 {
    let x = x_of(tile);
    let y = y_of(tile);
    let on_x = x == 0 || x == LAST;
    let on_y = y == 0 || y == LAST;
    if on_x == on_y {
        0
    } else if y == 0 {
        TOP
    } else if x == LAST {
        RIGHT
    } else if y == LAST {
        BOTTOM
    } else {
        LEFT
    }
}

fn exit_bands() -> &'static Vec<Vec<i32>> {
    static BANDS: OnceLock<Vec<Vec<i32>>> = OnceLock::new();
    BANDS.get_or_init(|| {
        (0..9)
            .map(|exit| match exit {
                1 => (1..=SIDE - 2).map(|i| index(i, 0)).collect(),
                3 => (1..=SIDE - 2).map(|i| index(LAST, i)).collect(),
                5 => (1..=SIDE - 2).map(|i| index(i, LAST)).collect(),
                7 => (1..=SIDE - 2).map(|i| index(0, i)).collect(),
                _ => Vec::new(),
            })
            .collect()
    })
}

/// Shared: never written.
pub fn exit_tiles(exit: i32) -> &'static [i32] {
    if (0..9).contains(&exit) {
        &exit_bands()[exit as usize]
    } else {
        &[]
    }
}

pub fn landing(tile: i32) -> Option<i32> {
    let x = x_of(tile);
    let y = y_of(tile);
    match exit_of(tile) {
        1 => Some(index(x, LAST)),
        3 => Some(index(0, y)),
        5 => Some(index(x, 0)),
        7 => Some(index(LAST, y)),
        _ => None,
    }
}

pub fn room_coords(name: &str) -> Option<(i32, i32)> {
    let bytes = name.as_bytes();
    let digit = |i: usize| i < bytes.len() && bytes[i].is_ascii_digit();
    let mut x_end = 1usize;
    while digit(x_end) {
        x_end += 1;
    }
    let mut y_end = x_end + 1;
    while digit(y_end) {
        y_end += 1;
    }
    let number = |start: usize, stop: usize| -> i32 {
        let mut value = 0;
        for byte in &bytes[start..stop] {
            value = value * 10 + (byte - b'0') as i32;
        }
        value
    };
    let axis = |letter: u8, outward: u8, inward: u8, value: i32| -> Option<i32> {
        if letter == outward {
            Some(-value - 1)
        } else if letter == inward {
            Some(value)
        } else {
            None
        }
    };
    if name.len() < 4 || x_end == 1 || y_end == x_end + 1 || y_end != name.len() {
        return None;
    }
    match (
        axis(bytes[0], b'W', b'E', number(1, x_end)),
        axis(bytes[x_end], b'N', b'S', number(x_end + 1, y_end)),
    ) {
        (Some(x), Some(y)) => Some((x, y)),
        _ => None,
    }
}

/// Across rooms, where adjacent rooms' edge tiles are one tile.
pub fn seam_range(a: &Pos, b: &Pos) -> Option<i32> {
    if a.room == b.room {
        a.range(b)
    } else {
        match (room_coords(&a.room), room_coords(&b.room)) {
            (Some((ax, ay)), Some((bx, by))) => Some(
                (((ax - bx) * (SIDE - 1) + a.x - b.x).abs())
                    .max(((ay - by) * (SIDE - 1) + a.y - b.y).abs()),
            ),
            _ => None,
        }
    }
}

pub fn room_name(x: i32, y: i32) -> String {
    let axis = |outward: &str, inward: &str, value: i32| -> String {
        if value < 0 {
            format!("{}{}", outward, -value - 1)
        } else {
            format!("{}{}", inward, value)
        }
    };
    format!("{}{}", axis("W", "E", x), axis("N", "S", y))
}

pub fn room_toward(name: &str, exit: i32) -> Option<String> {
    match exit {
        1 | 3 | 5 | 7 => room_coords(name).map(|(x, y)| room_name(x + dx(exit), y + dy(exit))),
        _ => None,
    }
}

pub fn exit_toward(from: &str, onto: &str) -> Option<i32> {
    match (room_coords(from), room_coords(onto)) {
        (Some((fx, fy)), Some((tx, ty))) => match (tx - fx, ty - fy) {
            (0, -1) => Some(TOP),
            (1, 0) => Some(RIGHT),
            (0, 1) => Some(BOTTOM),
            (-1, 0) => Some(LEFT),
            _ => None,
        },
        _ => None,
    }
}
