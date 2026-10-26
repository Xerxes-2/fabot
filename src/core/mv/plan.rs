/// The caller reads the creep's pace once (`Grid.factorOf`) and hands it in.
use crate::core::atlas::{Atlas, Field, Mover, Target};
use crate::core::grid::{self, FatigueFactor, Footprint};
use crate::core::mv::Wish;
use crate::core::tiles::{self, Tile};
use crate::core::world::Creep;

pub fn can_move(creep: &Creep, factor: FatigueFactor) -> bool {
    creep.fatigue == 0 && factor.moves > 0
}

/// Within two ticks: what a walker meeting it head-on waits to swap with.
pub fn rests_soon(creep: &Creep, factor: FatigueFactor) -> bool {
    factor.moves > 0 && creep.fatigue <= 4 * factor.moves
}

fn beside(atlas: &Atlas, creep: &Creep) -> Vec<Tile> {
    let at = tiles::of_pos(&creep.pos);
    tiles::within(1, at)
        .into_iter()
        .filter(|tile| *tile != at && atlas.passable(creep.pos.room, *tile))
        .collect()
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
    }
}

/// The engine carries a body that ends its tick on the border across, so a
/// body staying in the room stays a step inside it.
pub fn stay(atlas: &Atlas, creep: &Creep, factor: FatigueFactor, weight: i32) -> Wish {
    let at = tiles::of_pos(&creep.pos);
    let mut tail: Vec<Tile> = beside(atlas, creep)
        .into_iter()
        .filter(|tile| !tiles::on_edge(*tile))
        .collect();
    let moves = can_move(creep, factor);
    let head = if moves && tiles::on_edge(at) && !tail.is_empty() {
        tail.remove(0)
    } else {
        at
    };
    wish(creep, moves, head, None, tail, Vec::new(), weight)
}

/// The tail is the way round a body the Atlas's walk does not see.
pub fn toward(
    atlas: &mut Atlas,
    creep: &Creep,
    mover: Mover,
    target: &Target,
    weight: i32,
) -> Wish {
    let room = &creep.pos.room;
    let at = tiles::of_pos(&creep.pos);
    let moves = can_move(creep, mover.factor);

    let field: Option<Field> = if rests_soon(creep, mover.factor) {
        atlas.field(mover, target, &creep.pos)
    } else {
        None
    };

    let Some(field) = field else {
        return stay(atlas, creep, mover.factor, weight);
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
            None => stay(atlas, creep, mover.factor, weight),
            Some(head) => {
                let onward = field.step(head).and_then(|d| tiles::neighbour(head, d));
                let mut tail: Vec<Tile> = beside(atlas, creep)
                    .into_iter()
                    .filter(|tile| {
                        *tile != head
                            && (ticks(*tile) < here
                                || (ticks(*tile) < grid::UNREACHED
                                    && tiles::range(*tile, head) == 1))
                            && (ticks(*tile) == 0 || !tiles::on_edge(*tile))
                    })
                    .collect();
                tail.sort_by_key(|tile| (ticks(*tile), *tile));
                wish(creep, moves, head, onward, tail, Vec::new(), weight)
            }
        }
    }
}
