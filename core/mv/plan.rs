/// The caller reads the creep's pace once (`Grid.factorOf`) and hands it in.
use fabot_atlas::{Atlas, Mover, Target};
use fabot_base::{
    engine,
    grid::{self, FatigueFactor, Footprint},
    tiles::{self, Tile, TileSet},
    vocabulary::Part,
    wish::{Idle, Wish},
    world::{Creep, RoomName},
};

pub fn can_move(creep: &Creep, factor: FatigueFactor) -> bool {
    creep.fatigue == 0 && factor.moves > 0
}

/// Within two ticks: what a walker meeting it head-on waits to swap with.
pub fn rests_soon(creep: &Creep, factor: FatigueFactor) -> bool {
    grid::rest(factor, creep.fatigue).is_some_and(|r| r <= 2)
}

fn beside(atlas: &Atlas, creep: &Creep) -> Vec<Tile> {
    let at = tiles::of_pos(&creep.pos);
    let mut near = tiles::within(1, at);
    near.retain(|tile| *tile != at && atlas.passable(creep.pos.room, *tile));
    near
}

fn wish(
    creep: &Creep,
    moves: bool,
    head: Tile,
    onward: Option<Tile>,
    tail: Vec<Tile>,
    working: Vec<Tile>,
    weight: i32,
) -> Wish {
    Wish {
        creep: creep.name.clone(),
        footprint: Footprint::One,
        at: tiles::of_pos(&creep.pos),
        head,
        onward,
        tail,
        working,
        weight,
        moves,
        idle: None,
    }
}

/// The engine carries a body that ends its tick on the border across, so a
/// body staying in the room stays a step inside it, out of danger where it can.
pub fn stay(
    atlas: &Atlas,
    creep: &Creep,
    factor: FatigueFactor,
    weight: i32,
    danger: Option<&TileSet>,
) -> Wish {
    let at = tiles::of_pos(&creep.pos);
    let mut tail = beside(atlas, creep);
    tail.retain(|tile| !tiles::on_edge(*tile));
    tail.sort_by_key(|tile| danger.is_some_and(|d| d.contains(*tile)));
    let moves = can_move(creep, factor);
    let head = if moves && tiles::on_edge(at) && !tail.is_empty() { tail.remove(0) } else { at };
    wish(creep, moves, head, None, tail, Vec::new(), weight)
}

pub fn toward(
    atlas: &mut Atlas,
    creep: &Creep,
    mover: Mover,
    target: &Target,
    weight: i32,
) -> Wish {
    toward_round(atlas, creep, mover, target, weight, None)
}

/// The tail is the way round a body the Atlas's walk does not see.
pub fn toward_round(
    atlas: &mut Atlas,
    creep: &Creep,
    mover: Mover,
    target: &Target,
    weight: i32,
    danger: Option<&TileSet>,
) -> Wish {
    let room = &creep.pos.room;
    let at = tiles::of_pos(&creep.pos);
    let moves = can_move(creep, mover.factor);

    let refused =
        |atlas: &Atlas| stay(atlas, creep, mover.factor, weight, danger).idling(Idle::NoWalk);
    if !rests_soon(creep, mover.factor) {
        return stay(atlas, creep, mover.factor, weight, danger);
    }
    let Some(field) = atlas.field_round(mover, target, &creep.pos, danger) else {
        return refused(atlas);
    };

    let ticks = |tile: Tile| field.ticks(tile).unwrap_or(grid::UNREACHED);
    let here = ticks(at);

    if here == 0 && room == &target.room {
        let near = beside(atlas, creep);
        let (inside, outside): (Vec<Tile>, Vec<Tile>) = near
            .into_iter()
            .filter(|tile| !tiles::on_edge(*tile))
            .partition(|tile| ticks(*tile) == 0);
        let mut tail = inside.clone();
        tail.extend(outside);
        let mut working = vec![at];
        working.extend(inside);
        wish(creep, moves, at, None, tail, working, weight)
    } else if here == 0 {
        // The engine carries a body ending its tick on the border across with
        // its fatigue: a step along the band lands it too tired to step off.
        wish(creep, moves, at, None, Vec::new(), vec![at], weight)
    } else {
        match field.step(at).and_then(|d| tiles::neighbour(at, d)) {
            None => refused(atlas),
            Some(head) => {
                let onward = field.step(head).and_then(|d| tiles::neighbour(head, d));
                let mut tail = beside(atlas, creep);
                tail.retain(|tile| {
                    *tile != head
                        && (ticks(*tile) < here
                            || (ticks(*tile) < grid::UNREACHED && tiles::range(*tile, head) == 1))
                        && (ticks(*tile) == 0 || !tiles::on_edge(*tile))
                });
                tail.sort_by_key(|tile| (ticks(*tile), *tile));
                wish(creep, moves, head, onward, tail, Vec::new(), weight)
            }
        }
    }
}

/// v1's kiteStep: toward `target` round the tiles `swung` marks. From inside them, first onto
/// a clear tile beside, else onto the one a swing strikes latest where that is later than here,
/// else standing; standing too where its own tile is clear and the walk's first step is not.
pub fn kited(
    atlas: &mut Atlas,
    creep: &Creep,
    (mover, target, weight): (Mover, &Target, i32),
    hostiles: &[Creep],
) -> Wish {
    let swung = swung(atlas, creep, hostiles);
    let walk = toward_round(atlas, creep, mover, target, weight, swung.as_ref());
    let Some(d) = swung.filter(|d| d.contains(walk.head)) else {
        return walk;
    };
    let (room, here) = (creep.pos.room, tiles::of_pos(&creep.pos));
    let later = |t: Tile| soonest(room, hostiles, t).unwrap_or(i32::MAX);
    let mut beside = beside(atlas, creep);
    beside.retain(|t| !tiles::on_edge(*t));
    let clear = (beside.iter()).filter(|t| !d.contains(**t));
    let clear = clear.min_by_key(|t| (tiles::range(**t, walk.head), **t));
    let latest = (beside.iter()).max_by_key(|t| (later(**t), std::cmp::Reverse(**t)));
    let latest = latest.filter(|t| later(**t) > later(here));
    let moves = can_move(creep, mover.factor);
    match clear.or(latest).filter(|_| d.contains(here) && moves) {
        Some(t) => wish(creep, moves, *t, None, Vec::new(), Vec::new(), weight),
        None => wish(creep, moves, here, None, Vec::new(), Vec::new(), weight),
    }
}

/// v1's kiteUnsafeFor without its arrival maps: each tile near `creep` that a swing of
/// `hostiles` strikes by the tick `creep` could stand on it and be gone, never sooner than the
/// tick after (`max 1 leaves`): its rest, a plain step a tile before, and the hold of its step
/// onto the tile's own ground.
pub fn swung(atlas: &Atlas, creep: &Creep, hostiles: &[Creep]) -> Option<TileSet> {
    let (room, here) = (creep.pos.room, tiles::of_pos(&creep.pos));
    let held = |t| grid::step_ticks(grid::factor_of(creep), atlas.ground_at(room, t)?);
    let leaves = |t| match tiles::range(here, t) {
        // A body that cannot move never leaves.
        0 => Some(grid::rest(grid::factor_of(creep), creep.fatigue).unwrap_or(i32::MAX)),
        d => Some(arrives(creep, d)? - 1 + held(t)?),
    };
    let struck = |t: &Tile| {
        let soonest = soonest(room, hostiles, *t);
        soonest.zip(leaves(*t)).is_some_and(|(s, by)| s <= by.max(1))
    };
    let near = tiles::within(2 * engine::RANGED_RANGE + 1, here);
    let reach: Vec<Tile> = near.into_iter().filter(struck).collect();
    (!reach.is_empty()).then(|| reach.into_iter().collect())
}

/// The soonest tick a swing of `hostiles` strikes `t`, each closing at its own pace on plain.
fn soonest(room: RoomName, hostiles: &[Creep], t: Tile) -> Option<i32> {
    let at = tiles::to_pos(room, t);
    let closes = |h: &Creep| Some(h.pos.range(&at)? - engine::MELEE_RANGE);
    let swings = hostiles.iter().filter(|h| h.live(Part::Attack) > 0);
    swings.filter_map(|h| arrives(h, closes(h)?)).min()
}

/// The tick a body stands `n` tiles on at its pace on plain, its rest first; now for none.
fn arrives(creep: &Creep, n: i32) -> Option<i32> {
    let factor = grid::factor_of(creep);
    let step = grid::step_ticks(factor, engine::PLAIN_WEIGHT);
    if n > 0 { Some(grid::rest(factor, creep.fatigue)? + (n - 1) * step? + 1) } else { Some(0) }
}
