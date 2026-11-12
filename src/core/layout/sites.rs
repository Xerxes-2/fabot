use std::collections::BTreeSet;

use crate::core::atlas::Atlas;
use crate::core::engine;
use crate::core::tiles::{self, Tile};
use crate::core::world::{Creep, Kind, Pos, Room, Rules, Structure, terrain};

use super::plan::{controller_link, haul_level, sized};
use super::shape::{Item, Plan};

/// A level's first batch of extensions, and few enough that the builders'
/// energy goes to the plan's first items before its later ones.
pub const SITES_IN_FLIGHT: i32 = 5;

/// A site of any other kind under a creep is placed but not built while the
/// creep stays.
fn standable(kind: &Kind) -> bool {
    kind.is_road() || kind.is_container() || kind.is_rampart()
}

/// What grows the economy: a road, a rampart or a late kind waits on it.
fn pays(kind: &Kind) -> bool {
    matches!(
        kind,
        Kind::Spawn | Kind::Extension | Kind::Container | Kind::Storage | Kind::Link | Kind::Tower
    )
}

fn kinds() -> i32 {
    Kind::Other(String::new()).ordinal() + 1
}

/// `(tile, ticks a hauler needs to the source, the tiles a post may stand on)`.
type Post = (Tile, Option<(Tile, Vec<Tile>)>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Placement {
    pub x: i32,
    pub y: i32,
    pub kind: Kind,
}

/// A `byte` per tile, not a `bool`: a `bool[]` costs `Tiles.count` element
/// writes a room a tick, where a typed array is zeroed by its allocation.
#[derive(Clone, Debug)]
pub struct Census {
    pub solid: [u8; tiles::COUNT],
    pub road: [u8; tiles::COUNT],
    pub rampart: [u8; tiles::COUNT],
    pub site: [u8; tiles::COUNT],
    pub occupied: [u8; tiles::COUNT],
    pub walkable: [u8; tiles::COUNT],
    /// Tiles whose container's rock, else controller, has one already: one
    /// standing off the plan's tile is kept, never doubled.
    pub served: [u8; tiles::COUNT],
    /// None once a link serves the source: one standing off the plan's tile is
    /// kept, never doubled.
    pub posts: Vec<Post>,
    pub buffer: Option<(Tile, Tile)>,
    /// A solid site there waits on its upgrader as under any creep.
    pub seat: [u8; tiles::COUNT],
    /// Every owner's, and sites: CONTROLLER_STRUCTURES counts them all. By
    /// `Kind.ordinal`.
    pub held: Vec<i32>,
    /// One under a creep is not counted, so it never holds back the rest.
    pub pending: i32,
    pub paying: i32,
}

pub fn census_of(room: &Room, ours: &[Creep]) -> Census {
    let mut solid = [0u8; tiles::COUNT];
    let mut road = [0u8; tiles::COUNT];
    let mut rampart = [0u8; tiles::COUNT];
    let mut site = [0u8; tiles::COUNT];
    let mut held = vec![0i32; kinds() as usize];
    let mut occupied = [0u8; tiles::COUNT];
    let mut walkable = [0u8; tiles::COUNT];

    for (t, w) in walkable.iter_mut().enumerate() {
        if room.terrain.as_ref().map_or(0, |terrain| terrain[t]) & terrain::WALL == 0 {
            *w = 1;
        }
    }

    for c in ours.iter().chain(room.hostiles()) {
        occupied[tiles::of_pos(&c.pos)] = 1;
    }

    for s in &room.structures {
        let t = tiles::of_pos(&s.pos);

        if s.kind.is_road() {
            road[t] = 1;
        } else if s.kind.is_rampart() {
            rampart[t] = 1;
        } else {
            solid[t] = 1;
        }

        if !s.walkable {
            walkable[t] = 0;
        }

        held[s.kind.ordinal() as usize] += 1;
    }

    let mut pending = 0;
    let mut paying = 0;

    for s in &room.sites {
        let t = tiles::of_pos(&s.pos);
        site[t] = 1;
        held[s.kind.ordinal() as usize] += 1;

        if !s.kind.walkable() {
            walkable[t] = 0;
        }

        if s.owner.is_ours() && (standable(&s.kind) || occupied[t] == 0) {
            pending += 1;
            paying += i32::from(pays(&s.kind));
        }
    }

    let standing = |ordinal: i32| -> Vec<Pos> {
        let mut found: Vec<Pos> = Vec::new();

        for s in &room.structures {
            if s.kind.ordinal() == ordinal {
                found.push(s.pos);
            }
        }

        for s in &room.sites {
            if s.kind.ordinal() == ordinal {
                found.push(s.pos);
            }
        }

        found
    };

    let containers = standing(Kind::Container.ordinal());

    let rocks: Vec<Pos> = room
        .sources
        .iter()
        .map(|s| s.pos)
        .chain(room.minerals.iter().map(|m| m.pos))
        .collect();

    let mut served = [0u8; tiles::COUNT];

    if let Some(c) = &room.controller
        && containers.iter().any(|p| Atlas::is_buffer(room, p))
    {
        for t in tiles::within(engine::UPGRADE_RANGE, tiles::of_pos(&c.pos)) {
            served[t] = 1;
        }
    }

    for rock in &rocks {
        if containers.iter().any(|p| p.within(1, rock)) {
            for t in tiles::within(1, tiles::of_pos(rock)) {
                served[t] = 1;
            }
        }
    }

    for rock in &rocks {
        walkable[tiles::of_pos(rock)] = 0;
    }

    if let Some(c) = &room.controller {
        walkable[tiles::of_pos(&c.pos)] = 0;
    }

    let mut seat = [0u8; tiles::COUNT];

    let buffer: Option<(Tile, &Structure)> = match (&room.controller, Atlas::buffer(room)) {
        (Some(c), Some(b)) => Some((tiles::of_pos(&c.pos), b)),
        _ => None,
    };

    if let Some((c, b)) = buffer {
        for t in Atlas::upgrader_seats(|t: Tile| walkable[t] > 0, c, tiles::of_pos(&b.pos)) {
            seat[t] = 1;
        }
    }

    let links = standing(Kind::Link.ordinal());

    let unheld = |t: Tile| -> bool {
        walkable[t] > 0
            && solid[t] == 0
            && road[t] == 0
            && site[t] == 0
            && tiles::x_of(t).min(tiles::y_of(t)) >= 2
            && tiles::x_of(t).max(tiles::y_of(t)) < tiles::SIDE - 2
    };

    let mut posts: Vec<Post> = Vec::new();

    for s in &room.sources {
        if links.iter().any(|p| p.within(2, &s.pos)) {
            posts.push((tiles::of_pos(&s.pos), None));
        } else if let Some((p, _)) = Atlas::post_of(room, &s.pos) {
            let post = tiles::of_pos(&p);
            let beside = tiles::around(post)
                .iter()
                .copied()
                .filter(|t| unheld(*t))
                .collect();
            posts.push((tiles::of_pos(&s.pos), Some((post, beside))));
        }
    }

    Census {
        solid,
        road,
        rampart,
        site,
        occupied,
        walkable,
        served,
        posts,
        buffer: buffer
            .filter(|(_, b)| b.kind.is_container())
            .map(|(c, b)| (c, tiles::of_pos(&b.pos))),
        seat,
        held,
        pending,
        paying,
    }
}

/// Whether a structure on `t` parts the tiles beside it but across `post`,
/// where a miner works and steps aside for no walker. Beside, not the whole
/// room: a walk round the room prices dearer than a step through the miner.
fn closes(census: &Census, post: Tile, t: Tile) -> bool {
    let beside: Vec<Tile> = tiles::around(t)
        .iter()
        .copied()
        .filter(|n| *n != post && census.walkable[*n] > 0)
        .collect();

    let mut reached: Vec<Tile> = beside.iter().take(1).copied().collect();
    let mut i = 0;

    while i < reached.len() {
        for n in &beside {
            if tiles::range(*n, reached[i]) == 1 && !reached.contains(n) {
                reached.push(*n);
            }
        }

        i += 1;
    }

    reached.len() < beside.len()
}

fn held_of(census: &Census, kind: &Kind) -> i32 {
    census.held[kind.ordinal() as usize]
}

/// Below the haul's level no road or container is due, so only a kind short
/// of its allowance can be placed; false lets the caller skip the plan.
pub fn may_place(rules: &Rules, census: &Census, level: i32) -> bool {
    let mut kinds = vec![Kind::Link, Kind::Extractor];
    kinds.extend(sized());

    level >= haul_level(rules)
        || kinds
            .iter()
            .any(|kind| held_of(census, kind) < rules.allowed(&kind.name(), level))
}

/// The engine stacks a road only with a rampart, any other kind only with a
/// road or a rampart.
pub fn place(
    rules: &Rules,
    census: &Census,
    level: i32,
    plan: &Plan,
    budget: i32,
) -> Vec<Placement> {
    let mut placed: Vec<Placement> = Vec::new();
    let mut paid = census.paying;
    let mut added = vec![0i32; kinds() as usize];
    let added_of = |added: &Vec<i32>, kind: &Kind| -> i32 { added[kind.ordinal() as usize] };

    let mut taken: BTreeSet<Tile> = BTreeSet::new();

    let mut planned = [0u8; tiles::COUNT];
    let mut solid = [0u8; tiles::COUNT];
    // A rampart over a planned structure waits for it to stand.
    let mut under = [0u8; tiles::COUNT];

    for i in &plan.items {
        planned[i.tile] = 1;

        if !i.kind.walkable() {
            solid[i.tile] = 1;
        }
        if !i.kind.is_road() && !i.kind.is_rampart() {
            under[i.tile] = 1;
        }
    }

    // A source's link stands beside the post its miner works from, which
    // another bot may have set off the plan's container tile. The roads the
    // plan drew there lead to that unbuilt container, and the tile farthest
    // from the anchor lies behind the miner, off the way in. The last open
    // tile is the miner's way in.
    let tile_of = |item: &Item| -> Option<Tile> {
        let post = census
            .posts
            .iter()
            .find(|(rock, _)| item.kind.is_link() && tiles::range(item.tile, *rock) <= 2);

        match post {
            Some((_, None)) => None,
            None if item.kind.is_link()
                && census.seat[item.tile] > 0
                && census.buffer.is_some() =>
            {
                let (c, b) = census.buffer.unwrap();
                let walkable = |t: Tile| census.walkable[t] > 0;
                let open = |t: Tile| {
                    walkable(t)
                        && census.solid[t] == 0
                        && census.road[t] == 0
                        && census.site[t] == 0
                        && planned[t] == 0
                };

                controller_link(
                    walkable,
                    open,
                    |t: Tile| t == item.tile || solid[t] == 0,
                    c,
                    b,
                )
            }
            Some((_, Some((post, beside))))
                if tiles::range(item.tile, *post) != 1 || closes(census, *post, item.tile) =>
            {
                let mut candidates: Vec<Tile> = beside
                    .iter()
                    .copied()
                    .filter(|t| {
                        plan.items.iter().all(|i| i.tile != *t || i.kind.is_road())
                            && !closes(census, *post, *t)
                    })
                    .collect();

                candidates.sort_by_key(|t| {
                    (
                        plan.items.iter().any(|i| i.tile == *t),
                        -(tiles::range(*t, plan.anchor)),
                        *t,
                    )
                });

                let head = candidates.first().copied();
                if beside.len() > 1 { head } else { None }
            }
            _ => Some(item.tile),
        }
    };

    for item in plan.items.iter().filter(|i| i.level <= level) {
        match tile_of(item) {
            Some(t)
                if (placed.len() as i32) < budget
                    && if pays(&item.kind) {
                        paid
                    } else {
                        census.pending + placed.len() as i32
                    } < SITES_IN_FLIGHT
                    && census.site[t] == 0
                    && (if item.kind.is_rampart() {
                        (census.solid[t] > 0 || under[t] == 0) && census.rampart[t] == 0
                    } else {
                        census.solid[t] == 0
                    })
                    && (standable(&item.kind) || census.occupied[t] == 0)
                    && !taken.contains(&t)
                    && (!item.kind.is_container() || census.served[t] == 0)
                    && (!item.kind.is_road() || census.road[t] == 0)
                    && held_of(census, &item.kind) + added_of(&added, &item.kind)
                        < rules.allowed(&item.kind.name(), level) =>
            {
                taken.insert(t);
                paid += i32::from(pays(&item.kind));
                added[item.kind.ordinal() as usize] = added_of(&added, &item.kind) + 1;

                placed.push(Placement {
                    x: tiles::x_of(t),
                    y: tiles::y_of(t),
                    kind: item.kind.clone(),
                });
            }
            _ => (),
        }
    }

    placed
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::layout::fixtures as fx;
    use crate::core::world::{Owner, Structure};

    #[test]
    fn no_controller_links_site_lands_on_a_seat_of_a_buffer_another_bot_set() {
        let mut rng = fx::Rng::new(10);

        for _ in 0..fx::MAX_TEST {
            let colony = fx::colony(&mut rng);
            let pick = rng.choose(-1_000, 1_000);

            let Some((ground, plan)) = fx::plan_of(&colony) else {
                continue;
            };

            let Some(controller) = ground.controller else {
                continue;
            };

            let rocks: Vec<Tile> = ground
                .sources
                .iter()
                .chain(ground.minerals.iter())
                .copied()
                .collect();

            let off_plan: Vec<Tile> = tiles::within(engine::UPGRADE_RANGE, controller)
                .into_iter()
                .filter(|t| {
                    ground.weights[*t].is_some()
                        && rocks.iter().all(|r| tiles::range(*r, *t) > 1)
                        && plan.items.iter().all(|i| i.tile != *t || i.kind.is_road())
                })
                .collect();

            if off_plan.is_empty() {
                continue;
            }

            let buffer = { i64::from(pick) % off_plan.len() as i64 }.unsigned_abs() as usize;
            let buffer = off_plan[buffer];

            let mut room = colony.room.clone();
            room.structures.push(fx::structure(
                "buffer",
                Kind::Container,
                Owner::Ours,
                buffer,
            ));

            let census = census_of(&room, &[]);

            for i in plan.items.iter().filter(|i| i.kind.is_link()) {
                let mut one = plan.clone();
                one.items = vec![i.clone()];

                for p in place(
                    &Rules::vanilla(),
                    &census,
                    engine::CONTROLLER_MAX_LEVEL,
                    &one,
                    1,
                ) {
                    let t = tiles::index(p.x, p.y);

                    assert!(
                        tiles::range(t, buffer) > 1
                            || tiles::range(t, controller) > engine::UPGRADE_RANGE
                            || ground.sources.iter().any(|r| tiles::range(*r, t) <= 2)
                    );
                }
            }
        }
    }

    #[test]
    fn sites_keep_to_the_level_the_allowance_and_the_caps() {
        let mut rng = fx::Rng::new(11);

        for _ in 0..fx::MAX_TEST {
            let case = fx::census_case(&mut rng);

            let Some((_, plan)) = fx::plan_of(&case.colony) else {
                continue;
            };

            let pick = |ranks: &[i32]| -> Vec<Item> {
                let mut distinct: Vec<i32> = Vec::new();
                for r in ranks {
                    if !distinct.contains(r) {
                        distinct.push(*r);
                    }
                }

                distinct
                    .into_iter()
                    .filter(|r| *r >= 0 && (*r as usize) < plan.items.len())
                    .map(|r| plan.items[r as usize].clone())
                    .collect()
            };

            let built = pick(&case.built);

            let pending: Vec<Item> = pick(&case.pending)
                .into_iter()
                .filter(|i| !built.iter().any(|b| b.tile == i.tile))
                .collect();

            let mut room = case.colony.room.clone();
            room.structures.extend(built.iter().map(|i| {
                fx::structure(&format!("b{}", i.tile), i.kind.clone(), Owner::Ours, i.tile)
            }));
            room.sites = pending
                .iter()
                .map(|i| fx::site(&format!("x{}", i.tile), i.kind.clone(), i.tile))
                .collect();

            let counted = census_of(&room, &[]);
            let rules = Rules::vanilla();
            let placed = place(&rules, &counted, case.level, &plan, case.budget);

            let held = |kind: &Kind| -> i32 {
                (room
                    .structures
                    .iter()
                    .filter(|s| s.kind.name() == kind.name())
                    .count()
                    + room
                        .sites
                        .iter()
                        .filter(|s| s.kind.name() == kind.name())
                        .count()) as i32
            };

            let tiles_placed: Vec<Tile> = placed.iter().map(|p| tiles::index(p.x, p.y)).collect();

            let paid =
                |kinds: &mut dyn Iterator<Item = &Kind>| kinds.filter(|k| pays(k)).count() as i32;
            let paying = paid(&mut room.sites.iter().map(|s| &s.kind));
            let paying_after = paying + paid(&mut placed.iter().map(|p| &p.kind));
            let after = room.sites.len() as i32 + placed.len() as i32;

            let solid = |s: &Structure| !s.kind.is_road() && !s.kind.is_rampart();

            // What a kind may go down over: a rampart only over a standing
            // structure no rampart covers yet; a road shares a rampart, any
            // other kind a road or a rampart.
            let clear = |kind: &Kind, here: &[Structure]| -> bool {
                if kind.is_rampart() {
                    here.iter().any(solid) && !here.iter().any(|s| s.kind.is_rampart())
                } else {
                    here.iter()
                        .all(|s| s.kind.is_rampart() || (s.kind.is_road() && !kind.is_road()))
                }
            };

            let standing = |t: Tile| -> Vec<Structure> {
                room.structures
                    .iter()
                    .filter(|s| tiles::of_pos(&s.pos) == t)
                    .cloned()
                    .collect()
            };

            // An item the rules admit after what was placed: its level
            // reached, its kind short, its tile clear.
            let admitted = |i: &Item| -> bool {
                let t = i.tile;
                let added = placed
                    .iter()
                    .filter(|q| q.kind.name() == i.kind.name())
                    .count() as i32;

                i.level <= case.level
                    && !room.sites.iter().any(|x| tiles::of_pos(&x.pos) == t)
                    && !tiles_placed.contains(&t)
                    && !(i.kind.is_container() && counted.served[t] > 0)
                    && clear(&i.kind, &standing(t))
                    && held(&i.kind) + added < rules.allowed(&i.kind.name(), case.level)
            };

            assert!(may_place(&rules, &counted, case.level) || placed.is_empty());
            let full = |i: &Item| {
                (placed.len() as i32) >= case.budget
                    || if pays(&i.kind) { paying_after } else { after } >= SITES_IN_FLIGHT
            };
            assert!((placed.len() as i32) <= case.budget.max(0));
            assert!(paying_after <= SITES_IN_FLIGHT.max(paying));
            assert!((placed.iter().enumerate()).all(|(i, p)| {
                pays(&p.kind) || room.sites.len() + i < SITES_IN_FLIGHT as usize
            }));
            assert!(plan.items.iter().all(|i| !admitted(i) || full(i)));
            assert_eq!(fx::distinct(&tiles_placed), tiles_placed);

            for p in &placed {
                let t = tiles::index(p.x, p.y);

                assert!(!room.sites.iter().any(|x| tiles::of_pos(&x.pos) == t));
                assert!(clear(&p.kind, &standing(t)));
            }

            for p in &placed {
                let added = placed
                    .iter()
                    .filter(|q| q.kind.name() == p.kind.name())
                    .count() as i32;

                assert!(held(&p.kind) + added <= rules.allowed(&p.kind.name(), case.level));
            }

            for p in &placed {
                assert!(plan.items.iter().any(|i| {
                    i.tile == tiles::index(p.x, p.y)
                        && i.kind.name() == p.kind.name()
                        && i.level <= case.level
                }));
            }
        }
    }
}
