//! Hivemind's room plan, distance matrices and placement manager (MIT, ©
//! David Franke), in its order: a label's tiles are listed by `y * 50 + x`
//! as its plan's integer keys are.
use std::collections::{BTreeMap, BTreeSet};

use crate::tiles::{self, Tile, index};

use super::path::{Goal, Search};

pub const WALL: u8 = 1;
pub const SWAMP: u8 = 2;
pub const ROAD: u8 = 1;
const DISCOURAGED: u8 = 5;
pub const BLOCKED: u8 = 255;
const THROUGH_WALL: u8 = 30;

/// Every tile's label, the labels in the order first planned.
#[derive(Clone, Default)]
pub struct Labels {
    order: Vec<String>,
    sets: BTreeMap<String, BTreeSet<Tile>>,
}

impl Labels {
    pub fn add(&mut self, label: &str, t: Tile) {
        if !self.sets.contains_key(label) {
            self.order.push(label.to_string());
        }
        self.sets.entry(label.to_string()).or_default().insert(t);
    }

    pub fn remove(&mut self, label: &str, t: Tile) {
        if let Some(s) = self.sets.get_mut(label) {
            s.remove(&t);
        }
    }

    pub fn remove_all(&mut self, label: &str) {
        self.sets.remove(label);
        self.order.retain(|l| l != label);
    }

    pub fn has(&self, label: &str, t: Tile) -> bool {
        self.sets.get(label).is_some_and(|s| s.contains(&t))
    }

    pub fn of(&self, label: &str) -> Vec<Tile> {
        self.sets
            .get(label)
            .map_or_else(Vec::new, |s| s.iter().copied().collect())
    }

    pub fn count(&self, label: &str) -> usize {
        self.sets.get(label).map_or(0, BTreeSet::len)
    }

    pub fn labels(&self) -> &[String] {
        &self.order
    }
}

/// `handleMapArea`: the square around `(x, y)` clipped to the room, `x`
/// outer; the visit stops where `f` says false.
pub fn area(x: i32, y: i32, range: i32, mut f: impl FnMut(i32, i32) -> bool) {
    for dx in -range..=range {
        if !(0..50).contains(&(x + dx)) {
            continue;
        }
        for dy in -range..=range {
            if (0..50).contains(&(y + dy)) && !f(x + dx, y + dy) {
                return;
            }
        }
    }
}

pub fn xy(t: Tile) -> (i32, i32) {
    (tiles::x_of(t), tiles::y_of(t))
}

/// The wall and exit distance matrices (`RoomPlanMatrixGenerator`).
pub fn distances(terrain: &[u8]) -> (Vec<u8>, Vec<u8>) {
    let wall = |x: i32, y: i32| terrain[index(x, y).index()] & WALL != 0;
    let mut walls = vec![0u8; tiles::COUNT];
    let mut exits = vec![0u8; tiles::COUNT];
    for x in 0..50 {
        for y in 0..50 {
            let t = index(x, y).index();
            if wall(x, y) {
                walls[t] = 255;
                exits[t] = 255;
                continue;
            }
            if x == 0 || x == 49 || y == 0 || y == 49 {
                exits[t] = 1;
            }
            area(x, y, 1, |ax, ay| {
                if wall(ax, ay) {
                    walls[t] = 1;
                }
                !wall(ax, ay)
            });
        }
    }
    for m in [&mut walls, &mut exits] {
        let mut d = 1u8;
        loop {
            let mut changed = false;
            for x in 0..50 {
                for y in 0..50 {
                    let t = index(x, y).index();
                    if m[t] != 0 {
                        continue;
                    }
                    let mut found = false;
                    area(x, y, 1, |ax, ay| {
                        found = m[index(ax, ay).index()] == d;
                        !found
                    });
                    if found {
                        m[t] = d + 1;
                        changed = true;
                    }
                }
            }
            if !changed {
                break;
            }
            d += 1;
        }
    }
    // Hivemind leaves a room with no exit (a crafted one) all beside an exit,
    // where it builds nothing.
    if !exits.contains(&1) {
        for e in exits.iter_mut().filter(|e| **e == 0) {
            *e = 254;
        }
    }
    (walls, exits)
}

struct Spot {
    tile: Tile,
    range: i32,
    node: usize,
    alive: bool,
}

/// Hivemind's `PlacementManager`: the building matrix every road is searched
/// on, and the flood from the core that hands out building spots.
pub struct Manager<'a> {
    pub plan: Labels,
    pub matrix: Vec<u8>,
    pub terrain: &'a [u8],
    pub walls: &'a [u8],
    pub exits: &'a [u8],
    origin: Tile,
    entrances: Vec<Tile>,
    spots: Vec<Spot>,
    open: Vec<Option<usize>>,
    closed: Vec<bool>,
    /// A spot's path: its tile and its parent's node.
    nodes: Vec<(Tile, usize)>,
    current: Option<(Tile, i32)>,
}

impl<'a> Manager<'a> {
    pub fn new(terrain: &'a [u8], walls: &'a [u8], exits: &'a [u8]) -> Manager<'a> {
        let mut matrix = vec![0u8; tiles::COUNT];
        for t in tiles::all() {
            let (x, y) = xy(t);
            matrix[t.index()] = if terrain[t.index()] & WALL != 0 {
                THROUGH_WALL
            } else if x == 0 || y == 0 || x == 49 || y == 49 {
                BLOCKED
            } else if exits[t.index()] <= 2 {
                DISCOURAGED * 2
            } else {
                0
            };
        }
        Manager {
            plan: Labels::default(),
            matrix,
            terrain,
            walls,
            exits,
            origin: index(0, 0),
            entrances: Vec::new(),
            spots: Vec::new(),
            open: vec![None; tiles::COUNT],
            closed: vec![false; tiles::COUNT],
            nodes: Vec::new(),
            current: None,
        }
    }

    pub fn wall(&self, x: i32, y: i32) -> bool {
        self.terrain[index(x, y).index()] & WALL != 0
    }

    pub fn exit_distance(&self, x: i32, y: i32) -> u8 {
        self.exits[index(x, y).index()]
    }

    /// `cost` None is Hivemind's `null`: the matrix is left as it is.
    pub fn plan_at(&mut self, t: Tile, label: &str, cost: Option<u8>) {
        self.plan.add(label, t);
        if let Some(c) = cost
            && self.matrix[t.index()] < 100
        {
            self.matrix[t.index()] = c;
        }
    }

    pub fn place(&mut self, t: Tile, label: &str) {
        self.plan_at(t, label, Some(BLOCKED));
    }

    pub fn discourage(&mut self, x: i32, y: i32) {
        let m = &mut self.matrix[index(x, y).index()];
        if *m < DISCOURAGED && *m != ROAD {
            *m = DISCOURAGED;
        }
    }

    pub fn block(&mut self, t: Tile) {
        self.matrix[t.index()] = BLOCKED;
    }

    pub fn blocked(&self, x: i32, y: i32) -> bool {
        self.matrix[index(x, y).index()] >= 100
    }

    pub fn buildable(&self, x: i32, y: i32, roads: bool, near_exit: bool) -> bool {
        if !tiles::in_room(x, y) || self.wall(x, y) {
            return false;
        }
        if self.exit_distance(x, y) <= if near_exit { 2 } else { 5 } {
            return false;
        }
        let m = self.matrix[index(x, y).index()];
        if m > 100 {
            return false;
        }
        if m == DISCOURAGED && self.walls[index(x, y).index()] <= 2 {
            return true;
        }
        m <= 1 && (m != ROAD || roads)
    }

    pub fn start(&mut self, origin: Option<(Tile, Vec<Tile>)>) {
        if let Some((o, e)) = origin {
            self.origin = o;
            self.entrances = e;
        }
        self.spots.clear();
        self.nodes.clear();
        self.open.fill(None);
        self.closed.fill(false);
        self.nodes.push((self.origin, usize::MAX));
        self.spots.push(Spot {
            tile: self.origin,
            range: 0,
            node: 0,
            alive: true,
        });
        self.open[self.origin.index()] = Some(0);
    }

    fn on_path(&self, node: usize, t: Tile) -> bool {
        let mut n = node;
        while n != usize::MAX {
            if self.nodes[n].0 == t {
                return true;
            }
            n = self.nodes[n].1;
        }
        false
    }

    pub fn next_spot(&mut self) -> Option<Tile> {
        loop {
            let mut best: Option<usize> = None;
            for (i, s) in self.spots.iter().enumerate().filter(|(_, s)| s.alive) {
                if best.is_none_or(|b| self.spots[b].range == 0 || s.range < self.spots[b].range) {
                    best = Some(i);
                }
            }
            let i = best?;
            let (tile, range, node) = (self.spots[i].tile, self.spots[i].range, self.spots[i].node);
            self.spots[i].alive = false;
            self.open[tile.index()] = None;
            self.closed[tile.index()] = true;
            let (x, y) = xy(tile);
            let mut found: Vec<Tile> = Vec::new();
            area(x, y, 1, |nx, ny| {
                if (nx, ny) != (x, y) && self.buildable(nx, ny, true, false) {
                    found.push(index(nx, ny));
                }
                true
            });
            for n in found {
                if self.open[n.index()].is_none() && !self.closed[n.index()] {
                    self.nodes.push((n, node));
                    self.open[n.index()] = Some(self.spots.len());
                    self.spots.push(Spot {
                        tile: n,
                        range: range + 1,
                        node: self.nodes.len() - 1,
                        alive: true,
                    });
                }
            }
            if tiles::range(tile, self.origin) < 3 || !self.buildable(x, y, false, false) {
                continue;
            }
            self.current = Some((tile, range));
            return Some(tile);
        }
    }

    pub fn spot_range(&self) -> i32 {
        self.current.map_or(0, |c| c.1)
    }

    pub fn filter(&mut self, t: Tile) {
        for i in 0..self.spots.len() {
            if self.spots[i].alive && self.on_path(self.spots[i].node, t) {
                self.spots[i].alive = false;
                self.open[self.spots[i].tile.index()] = None;
            }
        }
    }

    pub fn place_all(&mut self, label: &str, allowed: usize, road: bool) {
        while self.plan.count(label) < allowed {
            let Some(next) = self.next_spot() else {
                break;
            };
            self.place(next, label);
            self.filter(next);
            if road {
                self.access_road(next);
            }
        }
    }

    pub fn access_road(&mut self, to: Tile) {
        let entrances = self.entrances.clone();
        for t in self.road(to, &entrances, false) {
            self.plan_at(t, "road", Some(ROAD));
        }
    }

    /// `findAccessRoad`; `through` prices a planned extension at 25 so a road
    /// may take one.
    pub fn road(&self, from: Tile, to: &[Tile], through: bool) -> Vec<Tile> {
        let mut matrix = self.matrix.clone();
        if through {
            for t in self.plan.of("extension") {
                matrix[t.index()] = 25;
            }
        }
        let goals: Vec<Goal> = to.iter().map(|t| Goal { tile: *t, range: 0 }).collect();
        let search = Search {
            terrain: self.terrain,
            matrix: &matrix,
            plain: 2,
            swamp: 2,
            weight: 0.9,
        };
        search.run(from, &goals).path
    }

    pub fn accessible(&self, from: Tile) -> bool {
        let goals: Vec<Goal> = (self.entrances.iter())
            .map(|t| Goal { tile: *t, range: 0 })
            .collect();
        let search = Search {
            terrain: self.terrain,
            matrix: &self.matrix,
            plain: 1,
            swamp: 1,
            weight: 1.2,
        };
        let found = search.run(from, &goals);
        !found.incomplete
            && (found.path.iter())
                .all(|t| self.terrain[t.index()] & WALL == 0 || self.plan.has("road", *t))
    }
}
