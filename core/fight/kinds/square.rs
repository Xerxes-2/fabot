//! Four bodies standing and walking as one 2x2 square, each beside every
//! other, so each one's HEAL reaches whichever the towers strike.
use std::collections::BTreeSet;

use fabot_atlas::{Atlas, Field, Mover, Target};
use fabot_base::{
    engine,
    grid::{self, Footprint},
    tiles::{self, TileSet},
    vocabulary::Part,
    world::{Creep, Pos, Room, RoomName},
};

pub(crate) const BLOCK: usize = 4;

/// The tiles a body never stands on: the ramparts and the garrison's reach, as it never leaves them.
pub(crate) fn closed(room: &Room) -> impl Fn(&Pos) -> bool {
    let ramparts: Vec<Pos> = (room.structures.iter())
        .filter(|s| fabot_atlas::invader_rampart(s))
        .map(|s| s.pos)
        .collect();
    let reach = (room.hostiles())
        .filter(|h| fabot_atlas::cast_by_core(h) && ramparts.contains(&h.pos))
        .map(|h| fabot_exchange::fighter::best_range(&crate::casting::fighter_of(h)))
        .max();
    move |p: &Pos| {
        ramparts.contains(p) || reach.is_some_and(|r| ramparts.iter().any(|w| w.within(r, p)))
    }
}

pub(crate) fn at(
    room: &Room,
    atlas: &Atlas,
    closed: &impl Fn(&Pos) -> bool,
    (x, y): (i32, i32),
) -> Option<[Pos; BLOCK]> {
    let at = |x: i32, y: i32| tiles::in_room(x, y).then(|| Pos::new(room.name, x, y));
    let tiles = [at(x, y)?, at(x + 1, y)?, at(x, y + 1)?, at(x + 1, y + 1)?];
    let open = |p: &Pos| {
        let t = tiles::of_pos(p);
        atlas.standable(room.name, t) && !closed(p)
    };
    tiles.iter().all(open).then_some(tiles)
}

/// The top-left tiles a square cannot stand on whole: its top-left walks the
/// rest as one body, as the atlas floods no `Footprint::Quad` to a target in its room.
pub(crate) fn walls(room: &Room, atlas: &Atlas, closed: &impl Fn(&Pos) -> bool) -> TileSet {
    let mut walls = TileSet::default();
    for t in tiles::all()
        .filter(|t| at(room, atlas, closed, (tiles::x_of(*t), tiles::y_of(*t))).is_none())
    {
        walls.insert(t);
    }
    walls
}

fn walk(set: &[&Creep], room: RoomName, tiles: Vec<tiles::Tile>, range: i32) -> (Mover, Target) {
    let factor = fabot_mv::blob::slowest(set.iter().map(|m| grid::factor_of(m)));
    let mover = Mover {
        footprint: Footprint::One,
        factor: factor.unwrap_or(grid::FatigueFactor { weight: 1, moves: 1 }),
    };
    let target = Target { room, tiles, range, avoid: BTreeSet::new(), max_hops: 0 };
    (mover, target)
}

pub(crate) fn from_border(atlas: &Atlas, walls: &TileSet, room: RoomName) -> Option<Field> {
    let border = tiles::all().filter(|t| {
        let (x, y) = (tiles::x_of(*t), tiles::y_of(*t));
        (x == 1 || y == 1 || x == tiles::SIDE - 3 || y == tiles::SIDE - 3) && !walls.contains(*t)
    });
    let (mover, target) = walk(&[], room, border.collect(), 0);
    atlas.field_round(mover, &target, &tiles::middle(room), Some(walls))
}

pub(crate) fn step(
    atlas: &Atlas,
    walls: &TileSet,
    set: &[&Creep],
    (from, to): (&Pos, &Pos),
) -> Option<i32> {
    let (mover, target) = walk(set, to.room, vec![tiles::of_pos(to)], 0);
    let next = *atlas.walk(mover, &target, Some(walls), from, 1).first()?;
    Some(tiles::direction(tiles::of_pos(from), tiles::of_pos(&next)))
}

pub(crate) fn far(set: &[&Creep], p: &Pos) -> i32 {
    set.iter().filter_map(|m| m.pos.range(p)).sum()
}

/// Each body's tile of `tiles`, in name order, its weapons first on the tiles
/// nearest `mark`: the towers strike the nearest, and only a weapon holds TOUGH.
pub(crate) fn seat(tiles: &[Pos; BLOCK], mark: &Pos, set: &[&Creep], c: &Creep) -> Option<Pos> {
    let mut ranked: Vec<&Creep> = set.to_vec();
    let strikes = |m: &Creep| m.live(Part::RangedAttack) > 0 || m.live(Part::Attack) > 0;
    ranked.sort_by_key(|m| (!strikes(m), &m.name));
    let mut free = tiles.to_vec();
    for m in ranked.into_iter().take(BLOCK) {
        let (i, _) = free.iter().enumerate().min_by_key(|(_, p)| {
            let near = m.pos.range(p).unwrap_or(engine::ROOM_SIDE);
            (p.range(mark).unwrap_or(engine::ROOM_SIDE), near, p.x, p.y)
        })?;
        let p = free.remove(i);
        if m.name == c.name {
            return Some(p);
        }
    }
    None
}

pub(crate) fn corner(set: &[&Creep]) -> Option<Pos> {
    let first = set.first()?.pos;
    let x = set.iter().map(|m| m.pos.x).min()?;
    let y = set.iter().map(|m| m.pos.y).min()?;
    let on = |m: &&Creep| m.pos.room == first.room && m.pos.x - x <= 1 && m.pos.y - y <= 1;
    (set.len() <= BLOCK && set.iter().all(on)).then_some(Pos::new(first.room, x, y))
}
