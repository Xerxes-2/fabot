//! A tile is `y * 50 + x`, the terrain raw buffer's index, so every per-room
//! array in the Atlas reads the terrain's own order.
use std::fmt;
use std::ops::{Index, IndexMut};
use std::sync::OnceLock;

use crate::engine;
use crate::world::{Pos, RoomName};

pub const SIDE: i32 = engine::ROOM_SIDE;
pub const COUNT: usize = screeps::ROOM_AREA;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Tile(u16);

impl Tile {
    #[inline]
    pub fn of_index(i: usize) -> Tile {
        Tile(i as u16)
    }

    #[inline]
    pub fn index(self) -> usize {
        self.0 as usize
    }
}

impl fmt::Display for Tile {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.fmt(f)
    }
}

impl<T> Index<Tile> for [T] {
    type Output = T;
    fn index(&self, tile: Tile) -> &T {
        &self[tile.index()]
    }
}

impl<T> IndexMut<Tile> for [T] {
    fn index_mut(&mut self, tile: Tile) -> &mut T {
        &mut self[tile.index()]
    }
}

impl<T> Index<Tile> for Vec<T> {
    type Output = T;
    fn index(&self, tile: Tile) -> &T {
        &self[tile.index()]
    }
}

impl<T> IndexMut<Tile> for Vec<T> {
    fn index_mut(&mut self, tile: Tile) -> &mut T {
        &mut self[tile.index()]
    }
}

/// One value a tile, in tile order.
pub type Table<T> = Box<[T; COUNT]>;

/// A keeper's lair and the resource it guards.
pub type Post = (Tile, Tile);

/// Built on the heap: a `Box::new` of the array builds it on the stack first.
pub fn table<T: Clone>(value: T) -> Table<T> {
    vec![value; COUNT]
        .into_boxed_slice()
        .try_into()
        .unwrap_or_else(|_| unreachable!())
}

/// A room's tiles, a bit each.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TileSet([u64; COUNT.div_ceil(64)]);

impl Default for TileSet {
    fn default() -> TileSet {
        TileSet([0; COUNT.div_ceil(64)])
    }
}

impl TileSet {
    /// Whether `tile` was not in the set.
    #[inline]
    pub fn insert(&mut self, tile: Tile) -> bool {
        let (word, bit) = (tile.index() / 64, 1u64 << (tile.index() % 64));
        let new = self.0[word] & bit == 0;
        self.0[word] |= bit;
        new
    }

    #[inline]
    pub fn contains(&self, tile: Tile) -> bool {
        self.0[tile.index() / 64] & (1u64 << (tile.index() % 64)) != 0
    }
}

impl FromIterator<Tile> for TileSet {
    fn from_iter<I: IntoIterator<Item = Tile>>(tiles: I) -> TileSet {
        let mut set = TileSet::default();
        for t in tiles {
            set.insert(t);
        }
        set
    }
}

pub fn all() -> impl Iterator<Item = Tile> {
    (0..COUNT as u16).map(Tile)
}

#[inline]
pub fn index(x: i32, y: i32) -> Tile {
    Tile((y * SIDE + x) as u16)
}

#[inline]
pub fn x_of(tile: Tile) -> i32 {
    i32::from(tile.0) % SIDE
}

#[inline]
pub fn y_of(tile: Tile) -> i32 {
    i32::from(tile.0) / SIDE
}

#[inline]
pub fn in_room(x: i32, y: i32) -> bool {
    (0..SIDE).contains(&x) && (0..SIDE).contains(&y)
}

#[inline]
pub fn of_pos(pos: &Pos) -> Tile {
    index(pos.x, pos.y)
}

#[inline]
pub fn to_pos(room: RoomName, tile: Tile) -> Pos {
    Pos::new(room, x_of(tile), y_of(tile))
}

/// The engine's `getRangeTo`.
#[inline]
pub fn range(a: Tile, b: Tile) -> i32 {
    (x_of(a) - x_of(b)).abs().max((y_of(a) - y_of(b)).abs())
}

pub const TOP: i32 = 1;
pub const TOP_RIGHT: i32 = 2;
pub const RIGHT: i32 = 3;
pub const BOTTOM_RIGHT: i32 = 4;
pub const BOTTOM: i32 = 5;
pub const BOTTOM_LEFT: i32 = 6;
pub const LEFT: i32 = 7;
pub const TOP_LEFT: i32 = 8;

#[inline]
pub fn dx(direction: i32) -> i32 {
    match direction {
        2..=4 => 1,
        6..=8 => -1,
        _ => 0,
    }
}

/// Rows grow downward.
#[inline]
pub fn dy(direction: i32) -> i32 {
    match direction {
        8 | 1 | 2 => -1,
        4..=6 => 1,
        _ => 0,
    }
}

#[inline]
pub fn neighbour(tile: Tile, direction: i32) -> Option<Tile> {
    let x = x_of(tile) + dx(direction);
    let y = y_of(tile) + dy(direction);
    in_room(x, y).then(|| index(x, y))
}

fn around_table() -> &'static Vec<Vec<Tile>> {
    static TABLE: OnceLock<Vec<Vec<Tile>>> = OnceLock::new();
    TABLE.get_or_init(|| {
        all()
            .map(|t| (1..=8).filter_map(|d| neighbour(t, d)).collect())
            .collect()
    })
}

/// The in-room neighbours, in direction order.
#[inline]
pub fn around(tile: Tile) -> &'static [Tile] {
    &around_table()[tile]
}

/// The neighbour costing least under `bound`, the first in direction order
/// on a tie.
pub fn least_around(tile: Tile, bound: i32, mut cost: impl FnMut(Tile) -> i32) -> Option<Tile> {
    (around(tile).iter())
        .map(|&n| (cost(n), n))
        .filter(|(c, _)| *c < bound)
        .min_by_key(|(c, _)| *c)
        .map(|(_, n)| n)
}

#[inline]
pub fn direction(from: Tile, onto: Tile) -> i32 {
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

pub fn within(radius: i32, tile: Tile) -> Vec<Tile> {
    let x = x_of(tile);
    let y = y_of(tile);
    let side = (2 * radius + 1).clamp(0, SIDE) as usize;
    let mut tiles = Vec::with_capacity(side * side);
    for ty in (y - radius).max(0)..=(y + radius).min(SIDE - 1) {
        for tx in (x - radius).max(0)..=(x + radius).min(SIDE - 1) {
            tiles.push(index(tx, ty));
        }
    }
    tiles
}

const LAST: i32 = SIDE - 1;

#[inline]
pub fn on_edge(tile: Tile) -> bool {
    let x = x_of(tile);
    let y = y_of(tile);
    x == 0 || y == 0 || x == LAST || y == LAST
}

/// The engine's FIND_EXIT_* values; a corner lies on two borders and lands
/// nowhere.
#[inline]
pub fn exit_of(tile: Tile) -> i32 {
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

fn exit_bands() -> &'static Vec<Vec<Tile>> {
    static BANDS: OnceLock<Vec<Vec<Tile>>> = OnceLock::new();
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
#[inline]
pub fn exit_tiles(exit: i32) -> &'static [Tile] {
    exit_bands().get(exit as usize).map_or(&[], Vec::as_slice)
}

#[inline]
pub fn landing(tile: Tile) -> Option<Tile> {
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

/// `exit_tiles(exit)` each with its `landing`, in the same order.
pub fn exit_landings(exit: i32) -> &'static [(Tile, Tile)] {
    static LANDINGS: OnceLock<Vec<Vec<(Tile, Tile)>>> = OnceLock::new();
    let all = LANDINGS.get_or_init(|| {
        (0..9)
            .map(|e| {
                exit_tiles(e)
                    .iter()
                    .filter_map(|&t| Some((t, landing(t)?)))
                    .collect()
            })
            .collect()
    });
    all.get(exit as usize).map_or(&[], Vec::as_slice)
}

pub fn rooms_apart(a: RoomName, b: RoomName) -> i32 {
    match (a.on_grid(), b.on_grid()) {
        (Some(a), Some(b)) => (a.0 - b.0).abs().max((a.1 - b.1).abs()),
        _ => 0,
    }
}

fn in_sector((x, y): (i32, i32)) -> (i32, i32) {
    let local = |c: i32| (if c < 0 { -c - 1 } else { c }) % 10;
    (local(x), local(y))
}

#[inline]
pub fn is_keeper_room(c: (i32, i32)) -> bool {
    let (x, y) = in_sector(c);
    (4..=6).contains(&x) && (4..=6).contains(&y) && (x, y) != (5, 5)
}

pub fn has_controller(c: (i32, i32)) -> bool {
    let (x, y) = in_sector(c);
    x != 0 && y != 0 && !((4..=6).contains(&x) && (4..=6).contains(&y))
}

/// Across rooms, where adjacent rooms' edge tiles are one tile.
#[inline]
pub fn seam_range(a: &Pos, b: &Pos) -> Option<i32> {
    if a.room == b.room {
        a.range(b)
    } else {
        let ((ax, ay), (bx, by)) = (a.room.on_grid()?, b.room.on_grid()?);
        Some(
            (((ax - bx) * (SIDE - 1) + a.x - b.x).abs())
                .max(((ay - by) * (SIDE - 1) + a.y - b.y).abs()),
        )
    }
}

pub fn room_toward(name: RoomName, exit: i32) -> Option<RoomName> {
    match exit {
        1 | 3 | 5 | 7 => name.offset(dx(exit), dy(exit)),
        _ => None,
    }
}

pub fn exit_toward(from: RoomName, onto: RoomName) -> Option<i32> {
    let ((fx, fy), (tx, ty)) = (from.on_grid()?, onto.on_grid()?);
    match (tx - fx, ty - fy) {
        (0, -1) => Some(TOP),
        (1, 0) => Some(RIGHT),
        (0, 1) => Some(BOTTOM),
        (-1, 0) => Some(LEFT),
        _ => None,
    }
}
