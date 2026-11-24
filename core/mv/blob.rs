//! A set that walks as one: one field priced at its slowest body, each member
//! kept near the median member along the walk.
use std::cmp::Reverse;
use std::collections::BTreeSet;

use crate::plan;
use fabot_atlas::{Atlas, Field, Mover, Target};
use fabot_base::engine;
use fabot_base::grid::{self, FatigueFactor, Footprint, factor_of, step_ticks};
use fabot_base::tiles::{self, Tile};
use fabot_base::wish::Wish;
use fabot_base::world::{Creep, RoomName};

/// ⌈(n−1)/2⌉: less, and a file of n deadlocks in a corridor.
pub fn radius(n: usize) -> i32 {
    (n / 2) as i32
}

/// The gait a set walks at: its slowest mover's on plain, then swamp, then road.
pub fn slowest(factors: impl IntoIterator<Item = FatigueFactor>) -> Option<FatigueFactor> {
    let pace = |f: &FatigueFactor| {
        [
            engine::PLAIN_WEIGHT,
            engine::SWAMP_WEIGHT,
            engine::ROAD_WEIGHT,
        ]
        .map(|w| step_ticks(*f, w))
    };
    (factors.into_iter())
        .filter(|f| f.moves > 0)
        .min_by_key(|f| Reverse(pace(f)))
}

fn mover(members: &[&Creep]) -> Mover {
    let factor = factor_of(members[0]);
    Mover {
        footprint: Footprint::One,
        factor: slowest(members.iter().map(|c| factor_of(c))).unwrap_or(factor),
    }
}

fn fatigue_ticks(creep: &Creep) -> Option<i32> {
    grid::rest(factor_of(creep), creep.fatigue)
}

pub fn wishes(
    atlas: &mut Atlas,
    target: &Target,
    weight: i32,
    members: &[&Creep],
    fortified: &BTreeSet<RoomName>,
) -> Vec<Wish> {
    let mover = mover(members);

    fn left(
        atlas: &mut Atlas,
        mover: Mover,
        target: &Target,
        hops: i32,
        room: RoomName,
        tile: Tile,
    ) -> i32 {
        let field = atlas.field(mover, target, &tiles::to_pos(room, tile));
        match field {
            // No field: one plain crossing, so a member short of it ranks behind one past it.
            None if !atlas.sees(room) => step_ticks(mover.factor, engine::PLAIN_WEIGHT)
                .map_or(grid::UNREACHED, |step| tiles::SIDE * step),
            None => grid::UNREACHED,
            Some(field) => {
                let exit = |field: &Field, mut t: Tile| -> Tile {
                    while let Some(n) = field.step(t).and_then(|d| tiles::neighbour(t, d)) {
                        t = n;
                    }
                    t
                };
                match field.ticks(tile) {
                    None => grid::UNREACHED,
                    Some(ticks) if room == target.room => ticks,
                    Some(_) if hops == 0 => grid::UNREACHED,
                    Some(ticks) => {
                        let edge = exit(&field, tile);
                        match (
                            tiles::room_toward(room, tiles::exit_of(edge)),
                            tiles::landing(edge),
                        ) {
                            (Some(next), Some(landing)) => {
                                let rest = left(atlas, mover, target, hops - 1, next, landing);
                                if rest == grid::UNREACHED {
                                    grid::UNREACHED
                                } else {
                                    ticks + rest
                                }
                            }
                            _ => grid::UNREACHED,
                        }
                    }
                }
            }
        }
    }

    let lags: Vec<i32> = members
        .iter()
        .map(|c| {
            let ticks = left(
                atlas,
                mover,
                target,
                target.max_hops,
                c.pos.room,
                tiles::of_pos(&c.pos),
            );
            match (ticks, fatigue_ticks(c)) {
                (t, Some(resting)) if t != grid::UNREACHED => t + resting,
                _ => grid::UNREACHED,
            }
        })
        .collect();

    let mut ranked: Vec<usize> = (0..members.len()).collect();
    ranked.sort_by_cached_key(|&i| (lags[i], members[i].name.as_str()));
    let a = ranked[(ranked.len() - 1) / 2];
    let anchor = members[a];
    let r = radius(members.len());

    let walks: Vec<Wish> = members
        .iter()
        .map(|c| plan::toward(atlas, c, mover, target, weight))
        .collect();

    let anchor_wish = walks[a].clone();

    // Ticks behind and out of reach of its step: one behind on terrain alone,
    // in reach, closes no faster for the anchor waiting.
    let lagging = members.iter().zip(&lags).any(|(c, &lag)| {
        lag != grid::UNREACHED
            && lag - lags[a] > r + 1
            && tiles::seam_range(&c.pos, &tiles::to_pos(anchor.pos.room, anchor_wish.head))
                .is_none_or(|range| range > r + 1)
    });

    let head_tile = if lagging {
        anchor_wish.at
    } else {
        anchor_wish.head
    };
    let head = tiles::to_pos(anchor.pos.room, head_tile);

    let step_back = step_ticks(mover.factor, engine::PLAIN_WEIGHT).map_or(0, |step| 2 * step);

    let to_band: Vec<Option<i32>> = members
        .iter()
        .map(|c| match (c.pos.room != target.room, fatigue_ticks(c)) {
            (true, Some(resting)) => atlas
                .field(mover, target, &c.pos)
                .and_then(|field| field.ticks(tiles::of_pos(&c.pos)))
                .map(|ticks| ticks + resting),
            _ => None,
        })
        .collect();

    // One landing first under towers fights alone, its seat beside one still
    // behind: into such a room the set steps onto the band only gathered.
    let gathered = |i: usize| -> bool {
        (members.iter().zip(&to_band).zip(&lags)).all(|((m, band), &lag)| {
            lag == grid::UNREACHED
                || if m.pos.room == members[i].pos.room {
                    band.is_some_and(|ticks| ticks <= step_back)
                } else {
                    lag <= lags[i]
                }
        })
    };

    let fits = |i: usize, tile: Tile| -> bool {
        let creep = members[i];
        let at = tiles::to_pos(creep.pos.room, tile);
        // Ahead of an anchor that waits, a pass closes no lag: it swaps two in front.
        (creep.name == anchor.name
            || lags[i] >= lags[a]
            || (tiles::seam_range(&at, &head).is_some_and(|range| range <= r)
                && (!lagging || members.iter().all(|m| m.name == creep.name || m.pos != at))))
            && (!tiles::room_toward(creep.pos.room, tiles::exit_of(tile))
                .is_some_and(|next| fortified.contains(&next))
                || gathered(i))
    };

    // A step into swamp falls behind: a walker sidesteps no slower than its
    // head, one waiting is shoved at most a plain step back, so one behind passes.
    let keeps = |atlas: &mut Atlas, slack: i32, creep: &Creep, tile: Tile| -> bool {
        match atlas.field(mover, target, &creep.pos) {
            Some(field) => match field.ticks(tiles::of_pos(&creep.pos)) {
                Some(here) if here > 0 => {
                    field.onto(tile).is_some_and(|ticks| ticks <= here + slack)
                }
                _ => true,
            },
            None => true,
        }
    };

    let mut out = Vec::with_capacity(members.len());
    for (i, (creep, wish)) in members.iter().zip(walks).enumerate() {
        let waits = (i == a && lagging) || !(wish.stays() || fits(i, wish.head));
        if waits {
            let stay = plan::stay(atlas, creep, mover.factor, weight, None);
            let mut holds = |tile: Tile| fits(i, tile) && keeps(atlas, step_back, creep, tile);
            let mut tail: Vec<Tile> = stay.tail.iter().copied().filter(|t| holds(*t)).collect();
            let regroup = stay.head != stay.at && !holds(stay.head) && !tail.is_empty();
            let head = if regroup { tail.remove(0) } else { stay.head };
            out.push(Wish { head, tail, ..stay });
        } else {
            let tail = wish
                .tail
                .iter()
                .copied()
                .filter(|tile| fits(i, *tile) && keeps(atlas, 0, creep, *tile))
                .collect();
            out.push(Wish { tail, ..wish });
        }
    }
    out
}
