//! Drawn once for the eighth level, so nothing moves or is rebuilt as the
//! room levels up. A diagonal lattice, so a hauler on one road tile reaches up
//! to four.
use std::borrow::Cow;
use std::collections::BTreeMap;

use crate::core::atlas::Atlas;
use crate::core::engine;
use crate::core::grid::{self, Footprint, UNREACHED, Weight};
use crate::core::tiles::{self, Tile};
use crate::core::world::Kind;

use super::ground::{self, Ground, SPAWN_OFFSETS};
use super::shape::{Item, Plan};

/// Leaves cells of five tiles, each beside a road. A design constant, not an
/// engine fact.
const PERIOD: i32 = 4;

fn wrap(v: i32) -> i32 {
    ((v % PERIOD) + PERIOD) % PERIOD
}

pub fn on_lattice(anchor: Tile, tile: Tile) -> bool {
    let dx = tiles::x_of(tile) - tiles::x_of(anchor);
    let dy = tiles::y_of(tile) - tiles::y_of(anchor);
    wrap(dx + dy) == 0 || wrap(dx - dy) == 0
}

fn labs() -> i32 {
    engine::controller_structures("lab", engine::CONTROLLER_MAX_LEVEL)
}

pub fn level_of(kind: &Kind, rank: i32) -> i32 {
    let name = kind.name();
    let mut level = 0;
    while level <= engine::CONTROLLER_MAX_LEVEL
        && engine::controller_structures(&name, level) <= rank
    {
        level += 1;
    }
    level
}

pub fn priority(kind: &Kind) -> i32 {
    match kind {
        Kind::Spawn => 0,
        // The tower charged first (owner, 2026-10-18).
        Kind::Tower => 1,
        Kind::Extension => 2,
        Kind::Rampart => 3,
        Kind::Storage => 4,
        Kind::Container => 5,
        Kind::Link => 6,
        Kind::Road => 7,
        Kind::Terminal => 8,
        Kind::Extractor => 9,
        Kind::Lab => 10,
        Kind::Factory => 11,
        Kind::PowerSpawn => 12,
        Kind::Nuker => 13,
        Kind::Observer => 14,
        _ => 15,
    }
}

fn lab_cluster(inputs: &[Tile], rank: &[i32], is_open: impl Fn(Tile) -> bool) -> Option<Vec<Tile>> {
    let reach = engine::LAB_REACTION_RANGE;
    let labs = labs();
    let mut best: Option<Vec<Tile>> = None;
    let mut best_cost = i32::MAX;

    for a in inputs {
        let a = *a;
        // Every cluster through a later input costs at least its rank.
        if is_open(a) && rank[a] < best_cost {
            for direction in [
                tiles::RIGHT,
                tiles::BOTTOM_RIGHT,
                tiles::BOTTOM,
                tiles::BOTTOM_LEFT,
            ] {
                if let Some(b) = tiles::neighbour(a, direction)
                    && is_open(b)
                {
                    let x0 = tiles::x_of(a).max(tiles::x_of(b)) - reach;
                    let x1 = tiles::x_of(a).min(tiles::x_of(b)) + reach;
                    let y0 = tiles::y_of(a).max(tiles::y_of(b)) - reach;
                    let y1 = tiles::y_of(a).min(tiles::y_of(b)) + reach;
                    let mut window: Vec<Tile> = Vec::new();

                    for y in 0.max(y0)..=(tiles::SIDE - 1).min(y1) {
                        for x in 0.max(x0)..=(tiles::SIDE - 1).min(x1) {
                            let t = tiles::index(x, y);

                            if t != a && t != b && is_open(t) {
                                window.push(t);
                            }
                        }
                    }

                    if window.len() as i32 >= labs - 2 {
                        window.sort_by_key(|t| rank[*t]);
                        let outputs: Vec<Tile> =
                            window.into_iter().take((labs - 2) as usize).collect();

                        let cost =
                            rank[a] + rank[b] + outputs.iter().map(|t| rank[*t]).sum::<i32>();

                        if cost < best_cost {
                            best_cost = cost;
                            let mut cluster = vec![a, b];
                            cluster.extend(outputs);
                            best = Some(cluster);
                        }
                    }
                }
            }
        }
    }

    best
}

/// A few labs' worth of first inputs: enough on open ground.
fn lab_search() -> i32 {
    3 * labs()
}

const HUB: [Kind; 4] = [Kind::Storage, Kind::Link, Kind::Terminal, Kind::Factory];

fn count(kind: &Kind) -> i32 {
    engine::controller_structures(&kind.name(), engine::CONTROLLER_MAX_LEVEL)
}

/// Not links, containers or roads: they are seated by what they serve.
pub fn sized() -> Vec<Kind> {
    vec![
        Kind::Spawn,
        Kind::Extension,
        Kind::Tower,
        Kind::Storage,
        Kind::Terminal,
        Kind::Factory,
        Kind::PowerSpawn,
        Kind::Nuker,
        Kind::Observer,
        Kind::Lab,
    ]
}

/// What a raid comes for.
const KEEP: [Kind; 4] = [Kind::Spawn, Kind::Tower, Kind::Storage, Kind::Terminal];

/// The first tower: the room's first defence.
pub fn rampart_level() -> i32 {
    level_of(&Kind::Tower, 0)
}

/// A road leaves the lattice only to go round a wall. A design constant.
const DETOUR: i32 = 2 * engine::PLAIN_WEIGHT;

fn steps() -> Vec<Option<u16>> {
    (0..=(engine::SWAMP_WEIGHT + DETOUR) as u16)
        .map(Some)
        .collect()
}

const NONE: i32 = i32::MAX;

/// At the first level a room's energy goes to its controller alone.
pub fn haul_level() -> i32 {
    level_of(&Kind::Extension, 0)
}

struct Posted {
    rock: Tile,
    container: Tile,
    link: Option<Tile>,
    path: Vec<Tile>,
}

/// Walks past it sort together.
const ROAD_SPAN: i32 = 1 << 14;

pub fn down(weights: &[Weight; tiles::COUNT], dist: &[i32], t: Tile) -> Option<Tile> {
    if dist[t] == 0 {
        None
    } else {
        tiles::least_around(t, UNREACHED, |n| match grid::value(weights[n]) {
            Some(w) if dist[n] != UNREACHED && (dist[n] == 0 || !tiles::on_edge(n)) => dist[n] + w,
            _ => UNREACHED,
        })
    }
}

pub fn descend(weights: &[Weight; tiles::COUNT], dist: &[i32], start: Tile) -> Vec<Tile> {
    let mut path: Vec<Tile> = Vec::new();
    let mut here = down(weights, dist, start);

    while let Some(at) = here
        && path.len() < tiles::COUNT
    {
        path.push(at);
        here = down(weights, dist, at);
    }

    match path.last() {
        Some(last) if dist[*last] == 0 => path,
        _ => Vec::new(),
    }
}

/// Sharing one of the container's seats, so a body there draws from either.
pub fn controller_link(
    walkable: impl Fn(Tile) -> bool,
    linkable: impl Fn(Tile) -> bool,
    standable: impl Fn(Tile) -> bool,
    controller: Tile,
    container: Tile,
) -> Option<Tile> {
    let seated: Vec<(Tile, Vec<Tile>)> = tiles::within(engine::UPGRADE_RANGE, controller)
        .into_iter()
        .filter(|t| tiles::range(*t, container) > 1 && linkable(*t))
        .map(|t| (t, Atlas::upgrader_seats(&walkable, controller, t)))
        .filter(|(_, seats)| {
            seats.iter().any(|n| tiles::range(*n, container) <= 1)
                && seats.iter().all(|n| standable(*n))
        })
        .collect();

    let mut best: Option<(Tile, Vec<Tile>)> = None;
    for (t, seats) in seated {
        let key = (-(seats.len() as i32), tiles::range(t, container), t);
        let better = match &best {
            None => true,
            Some((bt, bseats)) => {
                let best_key = (-(bseats.len() as i32), tiles::range(*bt, container), *bt);
                key < best_key
            }
        };
        if better {
            best = Some((t, seats));
        }
    }

    best.map(|(t, _)| t)
}

fn is_open(t: Tile, rank: &[i32], taken: &[bool]) -> bool {
    rank[t] != NONE && !taken[t]
}

fn fill(
    kind: &Kind,
    n: i32,
    ordered: &[Tile],
    taken: &mut [bool],
    cursor: &mut usize,
    picked: &mut Vec<(Kind, Tile)>,
) {
    for _ in 0..n {
        while *cursor < ordered.len() && taken[ordered[*cursor]] {
            *cursor += 1;
        }

        if *cursor < ordered.len() {
            let t = ordered[*cursor];
            taken[t] = true;
            picked.push((kind.clone(), t));
        }
    }
}

/// `spawns` only order the spawn slots, so a spawn on a slot never moves the
/// plan. Ways are laid before slots are handed out, so no structure stands on
/// one.
fn draw_with(on_first: bool, ground: &Ground, anchor: Tile, spawns: &[Tile]) -> (Plan, bool) {
    let weights: &[Weight; tiles::COUNT] = &ground.weights;
    let mut unfit: Vec<Cow<'static, str>> = Vec::new();

    let mut on_lat = [false; tiles::COUNT];
    let mut lane: [Weight; tiles::COUNT] = [None; tiles::COUNT];

    for t in tiles::all() {
        let on = on_lattice(anchor, t);
        on_lat[t] = on;

        lane[t] = match grid::value(weights[t]) {
            Some(w) if !tiles::on_edge(t) => grid::weight(if on { w } else { w + DETOUR }),
            _ => None,
        };
    }

    lane[anchor] = grid::weight(
        grid::value(weights[anchor]).map_or(engine::PLAIN_WEIGHT, |w| w.max(engine::PLAIN_WEIGHT)),
    );
    let walk = grid::flood(Footprint::One, &lane, &steps(), &[anchor]);

    let mut clear = [false; tiles::COUNT];
    let lattice = |t: Tile| -> bool { on_lat[t] && lane[t].is_some() };

    let mut walked = [false; tiles::COUNT];

    for t in tiles::all() {
        if lattice(t) && walk[t] != UNREACHED {
            let mut step = down(&lane, &*walk, t);

            while let Some(at) = step
                && !walked[at]
            {
                walked[at] = true;

                if !lattice(at) {
                    clear[at] = true;
                }

                step = down(&lane, &*walk, at);
            }
        }
    }

    fn way_out(
        lane: &[Weight; tiles::COUNT],
        walk: &[i32],
        clear: &mut [bool],
        rock: Tile,
    ) -> Vec<Tile> {
        let path = descend(lane, walk, rock);

        for &t in &path {
            clear[t] = true;
        }

        path
    }

    let mut source_walks: Vec<(Tile, Vec<Tile>)> = Vec::new();
    for s in &ground.sources {
        let path = way_out(&lane, &*walk, &mut clear, *s);
        if path.is_empty() {
            unfit.push("source unreached".into());
        } else {
            source_walks.push((*s, path));
        }
    }

    let controller_walk: Vec<Tile> = match ground.controller {
        Some(c) => {
            let path = way_out(&lane, &*walk, &mut clear, c);
            if path.is_empty() {
                unfit.push("controller unreached".into());
                Vec::new()
            } else {
                path
            }
        }
        None => Vec::new(),
    };

    let mut mineral_walks: Vec<(Tile, Vec<Tile>)> = Vec::new();
    for m in &ground.minerals {
        let path = way_out(&lane, &*walk, &mut clear, *m);
        if path.is_empty() {
            unfit.push("mineral unreached".into());
        } else {
            mineral_walks.push((*m, path));
        }
    }

    let controller_seat: Option<(Tile, Tile, Vec<Tile>)> =
        match (ground.controller, controller_walk.as_slice()) {
            (Some(c), [_, second, rest @ ..])
                if !on_first && tiles::range(*second, c) <= engine::UPGRADE_RANGE =>
            {
                Some((c, *second, rest.to_vec()))
            }
            (Some(c), [first, rest @ ..]) => Some((c, *first, rest.to_vec())),
            _ => None,
        };

    let walkable = |t: Tile| weights[t].is_some();
    let mut held = [false; tiles::COUNT];

    if let Some((c, buffer, _)) = &controller_seat {
        for n in Atlas::upgrader_seats(walkable, *c, *buffer) {
            held[n] = true;
        }
    }

    let mut access: [Option<Tile>; tiles::COUNT] = [None; tiles::COUNT];
    let mut slot_list: Vec<(i32, Tile)> = Vec::new();

    for t in tiles::all() {
        if ground.free[t] && !clear[t] && !on_lat[t] {
            let road =
                tiles::least_around(
                    t,
                    UNREACHED,
                    |n| {
                        if lattice(n) { walk[n] } else { UNREACHED }
                    },
                );

            if let Some(road) = road {
                access[t] = Some(road);
                slot_list.push((walk[road], t));
            }
        }
    }

    let mut keys = slot_list;
    keys.sort_unstable();
    let ordered: Vec<Tile> = keys.iter().map(|(_, t)| *t).collect();

    let mut rank = [NONE; tiles::COUNT];
    for (i, t) in ordered.iter().enumerate() {
        rank[*t] = i as i32;
    }

    let mut taken = [false; tiles::COUNT];
    let mut picked: Vec<(Kind, Tile)> = Vec::new();
    let mut cursor = 0usize;

    let mut hub_filled = 0usize;

    for direction in [tiles::TOP, tiles::RIGHT, tiles::BOTTOM, tiles::LEFT] {
        if let Some(t) = tiles::neighbour(anchor, direction)
            && hub_filled < HUB.len()
            && rank[t] != NONE
            && !taken[t]
        {
            taken[t] = true;
            picked.push((HUB[hub_filled].clone(), t));
            hub_filled += 1;
        }
    }

    for kind in HUB.iter().skip(hub_filled) {
        fill(kind, 1, &ordered, &mut taken, &mut cursor, &mut picked);
    }

    let slots = ground::spawn_slots(anchor);

    let order: Vec<Tile> = slots
        .iter()
        .filter(|t| spawns.contains(t))
        .chain(slots.iter().filter(|t| !spawns.contains(t)))
        .copied()
        .collect();

    let mut placed_spawns = 0;

    for t in order {
        if placed_spawns < count(&Kind::Spawn) && rank[t] != NONE && !taken[t] {
            taken[t] = true;
            picked.push((Kind::Spawn, t));
            placed_spawns += 1;
        }
    }

    for (kind, n) in [
        (Kind::Spawn, count(&Kind::Spawn) - placed_spawns),
        (Kind::Tower, count(&Kind::Tower)),
        (Kind::Extension, count(&Kind::Extension)),
        (Kind::PowerSpawn, count(&Kind::PowerSpawn)),
        (Kind::Nuker, count(&Kind::Nuker)),
    ] {
        fill(&kind, n, &ordered, &mut taken, &mut cursor, &mut picked);
    }

    let nearest: Vec<Tile> = ordered
        .iter()
        .copied()
        .filter(|t| is_open(*t, &rank, &taken))
        .collect();

    let head: Vec<Tile> = nearest
        .iter()
        .take(lab_search() as usize)
        .copied()
        .collect();
    let cluster = lab_cluster(&head, &rank, |t| is_open(t, &rank, &taken))
        .or_else(|| lab_cluster(&nearest, &rank, |t| is_open(t, &rank, &taken)));

    if let Some(cluster) = cluster {
        for t in cluster {
            taken[t] = true;
            picked.push((Kind::Lab, t));
        }
    }

    fill(
        &Kind::Observer,
        count(&Kind::Observer),
        &ordered,
        &mut taken,
        &mut cursor,
        &mut picked,
    );

    let mut road_level = [NONE; tiles::COUNT];
    let mut ranks: BTreeMap<Kind, i32> = BTreeMap::new();
    let mut items: Vec<Item> = Vec::new();

    for (kind, t) in &picked {
        let r = ranks.get(kind).copied().unwrap_or(0);
        ranks.insert(kind.clone(), r + 1);
        let level = level_of(kind, r);
        items.push(Item {
            kind: kind.clone(),
            tile: *t,
            level,
        });

        let mut road = access[*t].or_else(|| {
            tiles::least_around(*t, UNREACHED, |n| {
                if lane[n].is_some() && !taken[n] {
                    walk[n]
                } else {
                    UNREACHED
                }
            })
        });

        while let Some(at) = road {
            road_level[at] = road_level[at].min(level.max(haul_level()));
            road = down(&lane, &*walk, at);
        }
    }

    road_level[anchor] = road_level[anchor].min(haul_level());

    let mut board = Board {
        weights,
        anchor,
        walk,
        taken,
        road_level,
        clear,
        on_way: [false; tiles::COUNT],
        held,
        items,
        extra: Vec::new(),
    };

    // A container takes a dead end off its way out, so no body walking past
    // shoves its harvester off it.
    let rock_level = haul_level();

    for (_, path) in &source_walks {
        for t in path {
            board.on_way[*t] = true;
        }
    }

    for t in &controller_walk {
        board.on_way[*t] = true;
    }

    for (_, path) in &mineral_walks {
        for t in path {
            board.on_way[*t] = true;
        }
    }

    // A dead end's link is held as it is planted, so no other container or
    // link takes the room it was chosen for.
    let mut posted: Vec<Posted> = Vec::new();

    for (rock, path) in &source_walks {
        let head = path[0];

        let found = tiles::within(1, *rock)
            .into_iter()
            .find(|t| board.dead_end(*rock, head, *t));

        match found {
            Some(t) if !board.taken[head] => {
                board.plant(&Kind::Container, t, rock_level);
                board.pave(path, rock_level);
                let link = board.source_link(t);
                if let Some(l) = link {
                    board.taken[l] = true;
                }

                posted.push(Posted {
                    rock: *rock,
                    container: t,
                    link,
                    path: path.clone(),
                });
            }
            _ => {
                if !board.taken[head] {
                    board.plant(&Kind::Container, head, rock_level);
                }

                board.pave(&path[1..], rock_level);

                posted.push(Posted {
                    rock: *rock,
                    container: head,
                    link: None,
                    path: path.clone(),
                });
            }
        }
    }

    if let Some((_, buffer, rest)) = &controller_seat {
        if !board.taken[*buffer] {
            board.plant(&Kind::Container, *buffer, rock_level);
        }

        board.pave(rest, rock_level);
    }

    let dig_level = level_of(&Kind::Extractor, 0);

    for (m, path) in &mineral_walks {
        board.plant(&Kind::Extractor, *m, dig_level);

        if !board.taken[path[0]] {
            board.plant(&Kind::Container, path[0], dig_level);
        }

        board.pave(&path[1..], dig_level);
    }

    let mut links: Vec<Tile> = Vec::new();

    let mut linked = controller_seat.is_none();

    if let Some((c, container, _)) = &controller_seat {
        let found = {
            let walkable = |t: Tile| board.walkable(t);
            let linkable = |t: Tile| board.linkable(t);
            let standable = |t: Tile| board.standable(t);
            controller_link(walkable, linkable, standable, *c, *container)
        };

        match found {
            Some(t) => {
                board.taken[t] = true;
                links.push(t);
                linked = true;

                let seats = Atlas::upgrader_seats(|t: Tile| board.walkable(t), *c, t);
                for n in seats {
                    board.held[n] = true;
                }
            }
            // A link built late beats none.
            None if on_first => {
                let candidates: Vec<Tile> = tiles::around(*container)
                    .iter()
                    .copied()
                    .filter(|t| board.free(*t) && tiles::range(*t, *c) <= engine::UPGRADE_RANGE)
                    .collect();

                let mut best: Option<(i32, Tile)> = None;
                for t in candidates {
                    let key = (-board.open_count(t), t);
                    if best.is_none_or(|b| key < b) {
                        best = Some(key);
                    }
                }

                if let Some((_, t)) = best {
                    board.taken[t] = true;
                    links.push(t);
                }
            }
            None => (),
        }
    }

    let mut order: Vec<usize> = (0..posted.len()).collect();
    order.sort_by_key(|i| (-(posted[*i].path.len() as i32), posted[*i].rock));

    for i in order {
        let container = posted[i].container;
        let link = posted[i].link.or_else(|| board.source_link(container));
        if let Some(t) = link {
            board.taken[t] = true;
            links.push(t);
        }
    }

    let hub_links = board.items.iter().filter(|i| i.kind.is_link()).count() as i32;
    let outer: Vec<Tile> = links
        .iter()
        .take((count(&Kind::Link) - hub_links).max(0) as usize)
        .copied()
        .collect();

    for (i, t) in outer.iter().enumerate() {
        board.extra.push(Item {
            kind: Kind::Link,
            tile: *t,
            level: level_of(&Kind::Link, i as i32),
        });
    }

    // The hub's link comes last: it pays the 3% loss on all the storage's
    // energy to save a walk, where a source's link saves a whole haul.
    for k in 0..board.items.len() {
        if board.items[k].kind.is_link() {
            board.items[k].level = level_of(&Kind::Link, outer.len() as i32);
        }
    }

    let mut road_keys: Vec<(i32, i32, Tile)> = Vec::new();

    for t in tiles::all() {
        if board.road_level[t] != NONE && !board.taken[t] {
            road_keys.push((board.road_level[t], (ROAD_SPAN - 1).min(board.walk[t]), t));
        }
    }

    road_keys.sort_unstable();

    let roads: Vec<Item> = road_keys
        .iter()
        .map(|&(_, _, t)| Item {
            kind: Kind::Road,
            tile: t,
            level: board.road_level[t],
        })
        .collect();

    // A source's container too: its harvester cannot leave it.
    let mut covered: Vec<(Tile, i32)> = Vec::new();
    for i in &board.items {
        if KEEP.contains(&i.kind) {
            covered.push((i.tile, i.level));
        }
    }
    for p in &posted {
        covered.push((p.container, rock_level));
    }

    let mut distinct: Vec<(Tile, i32)> = Vec::new();
    for c in covered {
        if !distinct.iter().any(|d| d.0 == c.0) {
            distinct.push(c);
        }
    }

    let ramparts: Vec<Item> = distinct
        .iter()
        .map(|(t, level)| Item {
            kind: Kind::Rampart,
            tile: *t,
            level: rampart_level().max(*level),
        })
        .collect();

    let mut all: Vec<Item> = Vec::new();
    all.extend(board.items.iter().cloned());
    all.extend(board.extra.iter().cloned());
    all.extend(roads.iter().cloned());
    all.extend(ramparts.iter().cloned());

    for kind in sized() {
        if (all.iter().filter(|i| i.kind.name() == kind.name()).count() as i32) < count(&kind) {
            unfit.push(kind.name());
        }
    }

    // A container whose rock other seats ring goes before the extensions: its
    // harvester digs beside the crews. A rock's only seat waits for the bank,
    // or a small harvester holds it a life.
    let crowded: Vec<Tile> = posted
        .iter()
        .filter(|p| {
            tiles::around(p.rock)
                .iter()
                .filter(|n| board.weights[**n].is_some())
                .count()
                > 1
        })
        .map(|p| p.container)
        .collect();

    let buffer = controller_seat.as_ref().map(|(_, t, _)| *t);

    // The upgraders' only seat: a room that reaches a level with lower items
    // unbuilt builds it with them.
    let rank_of = |i: &Item| -> i32 {
        if i.kind.is_container() && crowded.contains(&i.tile) {
            2 * priority(&Kind::Extension) - 1
        } else if i.kind.is_container() && buffer == Some(i.tile) {
            2 * priority(&Kind::Extension)
        } else {
            2 * priority(&i.kind)
        }
    };

    let mut keys: Vec<i32> = (0..all.len())
        .map(|i| {
            let item = &all[i];
            (rank_of(item) * (engine::CONTROLLER_MAX_LEVEL + 2) + item.level) * all.len() as i32
                + i as i32
        })
        .collect();

    keys.sort_unstable();

    let items: Vec<Item> = keys
        .iter()
        .map(|key| all[(*key % all.len() as i32) as usize].clone())
        .collect();

    (
        Plan {
            room: ground.room.clone(),
            anchor,
            items,
            unfit,
        },
        linked,
    )
}

/// The controller's container a tile further out, leaving the first for an
/// upgrader to stand on.
pub fn draw(ground: &Ground, anchor: Tile, spawns: &[Tile]) -> Plan {
    let (plan, linked) = draw_with(false, ground, anchor, spawns);
    if linked {
        plan
    } else {
        draw_with(true, ground, anchor, spawns).0
    }
}

/// Scored so the spawns a plan places, built where it says, give the plan
/// again: an anchor before it in the order seats no spawn, standing or not.
pub fn of_ground(ground: &Ground, spawns: &[Tile]) -> Option<Plan> {
    let candidates = ground::anchors(ground, spawns);

    let score = |plan: &Plan| -> i32 {
        let placed: Vec<Tile> = plan
            .items
            .iter()
            .filter(|i| i.kind.is_spawn())
            .map(|i| i.tile)
            .collect();

        let kept = spawns.iter().filter(|s| placed.contains(s)).count() as i32;

        let seated = placed
            .first()
            .is_some_and(|s| ground::spawn_slots(plan.anchor).contains(s));

        2 * kept + i32::from(seated)
    };

    let tried = if spawns.is_empty() {
        SPAWN_OFFSETS.len().min(candidates.len())
    } else {
        candidates.len()
    };

    let mut best: Option<(i32, Plan)> = None;
    let mut i = 0;

    while i < tried
        && best
            .as_ref()
            .is_none_or(|(s, _)| *s <= 2 * spawns.len() as i32)
    {
        let plan = draw(ground, candidates[i], spawns);
        let s = score(&plan);

        if best.as_ref().is_none_or(|(b, _)| s > *b) {
            best = Some((s, plan));
        }

        i += 1;
    }

    best.map(|(_, plan)| plan)
}

struct Board<'a> {
    weights: &'a [Weight; tiles::COUNT],
    anchor: Tile,
    walk: tiles::Table<i32>,
    taken: [bool; tiles::COUNT],
    road_level: [i32; tiles::COUNT],
    clear: [bool; tiles::COUNT],
    on_way: [bool; tiles::COUNT],
    held: [bool; tiles::COUNT],
    items: Vec<Item>,
    extra: Vec<Item>,
}

impl Board<'_> {
    fn walkable(&self, t: Tile) -> bool {
        self.weights[t].is_some()
    }

    fn free(&self, n: Tile) -> bool {
        let x = tiles::x_of(n);
        let y = tiles::y_of(n);

        self.weights[n].is_some()
            && !self.taken[n]
            && !self.on_way[n]
            && self.road_level[n] == NONE
            && x >= 2
            && y >= 2
            && x < tiles::SIDE - 2
            && y < tiles::SIDE - 2
    }

    fn linkable(&self, n: Tile) -> bool {
        self.free(n) && !self.held[n]
    }

    fn link_beside(&self, onto: Tile, key: impl Fn(Tile) -> i32) -> Option<Tile> {
        let mut best: Option<(i32, Tile)> = None;
        for n in tiles::around(onto) {
            if self.linkable(*n) {
                let k = key(*n);
                if best.is_none_or(|b| (k, *n) < b) {
                    best = Some((k, *n));
                }
            }
        }
        best.map(|(_, n)| n)
    }

    fn source_link(&self, container: Tile) -> Option<Tile> {
        self.link_beside(container, |t| tiles::range(t, self.anchor))
    }

    fn dead_end(&self, rock: Tile, head: Tile, t: Tile) -> bool {
        let t_u = t;

        t != head
            && tiles::range(t, rock) == 1
            && tiles::range(t, head) == 1
            && self.weights[t_u].is_some()
            && !tiles::on_edge(t)
            && !self.taken[t_u]
            && !self.clear[t_u]
            && !self.on_way[t_u]
            && self.road_level[t_u] == NONE
            && tiles::around(t)
                .iter()
                .any(|n| self.linkable(*n) && !self.clear[*n])
    }

    fn plant(&mut self, kind: &Kind, t: Tile, level: i32) {
        self.taken[t] = true;
        self.road_level[t] = NONE;
        self.extra.push(Item {
            kind: kind.clone(),
            tile: t,
            level,
        });
    }

    fn pave(&mut self, path: &[Tile], level: i32) {
        for t in path {
            if !self.taken[*t] {
                self.road_level[*t] = self.road_level[*t].min(level);
            }
        }
    }

    fn standable(&self, t: Tile) -> bool {
        !self.taken[t]
            || self
                .extra
                .iter()
                .any(|i| i.tile == t && i.kind.is_container())
    }

    fn open_count(&self, t: Tile) -> i32 {
        tiles::around(t)
            .iter()
            .map(|m| i32::from(self.walkable(*m)))
            .sum()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::layout::fixtures as fx;

    const NAMES: [&str; 16] = [
        "spawn",
        "extension",
        "link",
        "road",
        "constructedWall",
        "rampart",
        "storage",
        "tower",
        "observer",
        "powerSpawn",
        "extractor",
        "terminal",
        "lab",
        "container",
        "nuker",
        "factory",
    ];

    fn allowed(kind: &Kind) -> i32 {
        engine::controller_structures(&kind.name(), engine::CONTROLLER_MAX_LEVEL)
    }

    fn rankable() -> Vec<Kind> {
        let mut kinds = vec![Kind::Link];
        kinds.extend(sized());
        kinds
    }

    #[test]
    fn the_allowance_never_falls_as_the_level_rises() {
        for name in NAMES {
            for level in 0..engine::CONTROLLER_MAX_LEVEL {
                assert!(
                    engine::controller_structures(name, level)
                        <= engine::controller_structures(name, level + 1),
                    "{name} at {level}"
                );
            }
        }
    }

    #[test]
    fn a_ranks_level_is_the_first_the_allowance_admits_it_at() {
        for kind in rankable() {
            let top = allowed(&kind);

            for rank in 0..top {
                let level = level_of(&kind, rank);

                assert!(
                    engine::controller_structures(&kind.name(), level) > rank,
                    "admitted: {} rank {} at {}",
                    kind.name(),
                    rank,
                    level
                );

                if level > 0 {
                    assert!(
                        engine::controller_structures(&kind.name(), level - 1) <= rank,
                        "not before: {} rank {} at {}",
                        kind.name(),
                        rank,
                        level
                    );
                }
            }
        }
    }

    #[test]
    fn a_plan_builds_only_where_the_engine_allows_one_item_a_tile_but_a_rampart_over_another() {
        for colony in fx::samples(1).take(fx::MAX_TEST) {
            let Some((ground, plan)) = fx::plan_of(&colony) else {
                continue;
            };

            let ramparts: Vec<Item> = plan
                .items
                .iter()
                .filter(|i| i.kind.is_rampart())
                .cloned()
                .collect();
            let others: Vec<Item> = plan
                .items
                .iter()
                .filter(|i| !i.kind.is_rampart())
                .cloned()
                .collect();

            let tiles: Vec<Tile> = others.iter().map(|i| i.tile).collect();
            let covered: Vec<Tile> = ramparts.iter().map(|i| i.tile).collect();

            assert_eq!(fx::distinct(&tiles), tiles);
            assert_eq!(fx::distinct(&covered), covered);
            assert!(covered.iter().all(|t| tiles.contains(t)));

            for i in &plan.items {
                let x = tiles::x_of(i.tile);
                let y = tiles::y_of(i.tile);

                assert!(!tiles::on_edge(i.tile), "on the edge: {}", i.tile);
                assert!(
                    !(ground.sources.contains(&i.tile) || ground.controller == Some(i.tile)),
                    "on fixed rock: {}",
                    i.tile
                );
                assert!(
                    if i.kind.is_extractor() {
                        ground.minerals.contains(&i.tile)
                    } else {
                        ground.weights[i.tile].is_some()
                    },
                    "on ground no creep crosses: {}",
                    i.tile
                );
                assert!(
                    fx::walkable(&i.kind) || (x >= 2 && y >= 2 && x <= 47 && y <= 47),
                    "on the exit ring: {}",
                    i.tile
                );
            }
        }
    }

    #[test]
    fn no_kind_past_its_allowance_and_a_fit_plan_seats_every_sized_kind_whole() {
        for colony in fx::samples(2).take(fx::MAX_TEST) {
            let Some((_, plan)) = fx::plan_of(&colony) else {
                continue;
            };

            let mut kinds = vec![Kind::Link, Kind::Container, Kind::Extractor, Kind::Road];
            kinds.extend(sized());

            let within = kinds
                .iter()
                .all(|kind| fx::count_of(kind, &plan.items) <= allowed(kind));

            let whole = sized().iter().all(|kind| {
                fx::count_of(kind, &plan.items) == allowed(kind)
                    || plan.unfit.contains(&kind.name())
            });

            assert!(within && whole);
        }
    }

    #[test]
    fn each_items_level_admits_it_and_every_lower_rank_of_its_kind() {
        for colony in fx::samples(3).take(fx::MAX_TEST) {
            let Some((_, plan)) = fx::plan_of(&colony) else {
                continue;
            };

            for kind in rankable() {
                for level in 0..=engine::CONTROLLER_MAX_LEVEL {
                    let seated = plan
                        .items
                        .iter()
                        .filter(|i| i.kind.name() == kind.name() && i.level <= level)
                        .count() as i32;

                    assert!(
                        seated <= engine::controller_structures(&kind.name(), level),
                        "{} at {}: {} seated",
                        kind.name(),
                        level,
                        seated
                    );
                }
            }
        }
    }

    #[test]
    fn every_planned_structure_can_be_walked_to_from_the_anchor() {
        for colony in fx::samples(4).take(fx::MAX_TEST) {
            let Some((ground, plan)) = fx::plan_of(&colony) else {
                continue;
            };

            let seen = fx::reached(&ground, &plan);
            let beside = |t: Tile| tiles::within(1, t).into_iter().any(|n| n != t && seen[n]);

            for i in &plan.items {
                assert!(
                    i.kind.is_rampart()
                        || if fx::walkable(&i.kind) {
                            seen[i.tile]
                        } else {
                            beside(i.tile)
                        },
                    "{:?} at {} unreached",
                    i.kind.name(),
                    i.tile
                );
            }
        }
    }

    #[test]
    fn the_controllers_link_is_planned_off_the_upgraders_seats_where_any_tile_is() {
        use crate::core::atlas::Atlas;

        for colony in fx::samples(5).take(fx::MAX_TEST) {
            let Some((ground, plan)) = fx::plan_of(&colony) else {
                continue;
            };

            let Some(controller) = ground.controller else {
                continue;
            };

            let tiles_of = |kind: &Kind| -> Vec<Tile> {
                plan.items
                    .iter()
                    .filter(|i| i.kind.name() == kind.name())
                    .map(|i| i.tile)
                    .collect()
            };

            let rocks: Vec<Tile> = ground
                .sources
                .iter()
                .chain(ground.minerals.iter())
                .copied()
                .collect();

            let nearest = |kind: &Kind, off: &[Tile], reach: i32| -> Option<Tile> {
                let mut found: Vec<Tile> = tiles_of(kind)
                    .into_iter()
                    .filter(|t| {
                        tiles::range(*t, controller) <= engine::UPGRADE_RANGE
                            && off.iter().all(|o| tiles::range(*o, *t) > reach)
                    })
                    .collect();

                found.sort_by_key(|t| (tiles::range(*t, controller), *t));
                found.first().copied()
            };

            let seats = |buffer: Tile| -> Vec<Tile> {
                Atlas::upgrader_seats(|t: Tile| ground.weights[t].is_some(), controller, buffer)
                    .into_iter()
                    .filter(|t| *t != buffer)
                    .collect()
            };

            let solid: Vec<Tile> = plan
                .items
                .iter()
                .filter(|i| !fx::walkable(&i.kind))
                .map(|i| i.tile)
                .collect();

            let container = nearest(&Kind::Container, &rocks, 1);

            let mut off_link: Vec<Tile> = ground.sources.clone();
            off_link.extend(tiles_of(&Kind::Storage));
            let link = nearest(&Kind::Link, &off_link, 2);

            let on = |buffer: Tile, t: Tile| seats(buffer).contains(&t);
            let clear = |l: Tile| solid.iter().all(|t| !on(l, *t));

            // A link goes on a seat only on the second draw, whose container
            // takes the walk's first tile, and holds none.
            match container {
                Some(c) => {
                    if let Some(l) = link {
                        if on(c, l) {
                            assert_eq!(tiles::range(c, controller), 1);
                        } else {
                            assert!(clear(l));
                        }
                    }
                }
                None => {
                    if let Some(l) = link {
                        assert!(clear(l));
                    }
                }
            }
        }
    }

    #[test]
    fn the_labs_react_every_lab_within_reach_of_two_inputs() {
        for colony in fx::samples(6).take(fx::MAX_TEST) {
            let Some((_, plan)) = fx::plan_of(&colony) else {
                continue;
            };

            let labs: Vec<Tile> = plan
                .items
                .iter()
                .filter(|i| i.kind.is_lab())
                .map(|i| i.tile)
                .collect();

            let reach = engine::LAB_REACTION_RANGE;

            let reacts = labs.is_empty()
                || labs.iter().any(|a| {
                    labs.iter().any(|b| {
                        a != b
                            && labs.iter().all(|t| {
                                t == a
                                    || t == b
                                    || (tiles::range(*t, *a) <= reach
                                        && tiles::range(*t, *b) <= reach)
                            })
                    })
                });

            assert!(reacts);
        }
    }

    #[test]
    fn the_plan_is_the_same_whatever_order_the_room_lists_things_in() {
        let mut rng = fx::Rng::new(7);

        for _ in 0..fx::MAX_TEST {
            let colony = fx::colony(&mut rng);
            let seed = rng.choose(1, 1_000_000) as u64;

            match (
                fx::plan_of(&colony),
                fx::plan_of(&fx::shuffled(&colony, seed)),
            ) {
                (Some((_, a)), Some((_, b))) => assert!(fx::same(&a, &b)),
                (None, None) => (),
                _ => panic!("a plan one way, none the other"),
            }
        }
    }

    #[test]
    fn a_rooms_founding_spawn_standing_where_its_plan_says_leaves_the_plan_unchanged() {
        for colony in fx::samples(8).take(fx::MAX_TEST) {
            let Some((ground, plan)) = fx::plan_of(&fx::bare(&colony)) else {
                continue;
            };

            match plan.items.iter().find(|i| i.kind.is_spawn()) {
                None => assert!(plan.unfit.contains(&Kind::Spawn.name())),
                Some(first) => match of_ground(&ground, &[first.tile]) {
                    Some(founded) => assert!(fx::same(&plan, &founded)),
                    None => panic!("no plan from a founding spawn"),
                },
            }
        }
    }

    #[test]
    fn a_second_and_third_spawn_on_their_planned_tiles_never_move_the_plan() {
        for colony in fx::samples(9).take(fx::MAX_TEST) {
            let Some((ground, plan)) = fx::plan_of(&fx::bare(&colony)) else {
                continue;
            };

            let spawns: Vec<Tile> = plan
                .items
                .iter()
                .filter(|i| i.kind.is_spawn())
                .map(|i| i.tile)
                .collect();

            for n in 1..=spawns.len() {
                let mut taken = spawns[0..n].to_vec();
                taken.sort_unstable();

                match of_ground(&ground, &taken) {
                    Some(grown) => assert!(fx::same(&plan, &grown)),
                    None => panic!("no plan from {n} spawns"),
                }
            }
        }
    }
}
