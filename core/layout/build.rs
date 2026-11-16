//! Hivemind's room variation builder and its tower step (MIT, © David
//! Franke), step for step: one variation's plan, as labels.
use crate::tiles::{self, Tile};
use crate::world::Rules;

use super::cut::{self, Rect};
use super::ground::Ground;
use super::place::{BLOCKED, Labels, Manager, ROAD, SWAMP, WALL, area, at, xy};

const ENDANGERED: u8 = 0;
const SAFE: u8 = 1;
const UNSAFE: u8 = 2;
const UNSAFE_NEAR_WALL: u8 = 3;
/// Hivemind's `minCutRampartDistance`.
const RAMPART_DISTANCE: i32 = 4;

const DECORATIVE: [[u8; 12]; 14] = [
    [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1],
    [0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0],
    [0, 0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1],
];

/// Hivemind's room intel on one side: a side with no exit tile is safe.
pub fn safe_sides(terrain: &[u8]) -> [bool; 4] {
    let open = |x: i32, y: i32| terrain[at(x, y).index()] & WALL == 0;
    let any = |f: &dyn Fn(i32) -> bool| (1..49).any(f);
    [
        !any(&|i| open(i, 0)),
        !any(&|i| open(49, i)),
        !any(&|i| open(i, 49)),
        !any(&|i| open(0, i)),
    ]
}

/// `getExitCenters`, by side N, S, W, E.
pub fn exit_centers(terrain: &[u8]) -> Vec<Tile> {
    let open = |x: i32, y: i32| terrain[at(x, y).index()] & WALL == 0;
    let mut centers = Vec::new();
    let sides: [&dyn Fn(i32) -> (i32, i32); 4] =
        [&|i| (i, 0), &|i| (i, 49), &|i| (0, i), &|i| (49, i)];
    for side in sides {
        let coords: Vec<(i32, i32)> = (1..49).map(side).filter(|(x, y)| open(*x, *y)).collect();
        let mut start: Option<(i32, i32)> = None;
        let mut prev: Option<(i32, i32)> = None;
        let mid = |a: (i32, i32), b: (i32, i32)| {
            let half = |p: i32, q: i32| (p + q + 1).div_euclid(2);
            at(half(a.0, b.0), half(a.1, b.1))
        };
        for c in coords {
            let s = *start.get_or_insert(c);
            if let Some(p) = prev
                && (c.0 - p.0).abs().max((c.1 - p.1).abs()) > 1
            {
                centers.push(mid(p, s));
                start = Some(c);
            }
            prev = Some(c);
        }
        if let (Some(s), Some(p)) = (start, prev) {
            centers.push(mid(p, s));
        }
    }
    centers
}

pub struct Variation {
    pub center: Tile,
    /// The sources (by index) whose harvest bay seats a spawn.
    pub spawns: Vec<bool>,
}

pub struct Built {
    pub plan: Labels,
    pub center: Tile,
}

struct Builder<'a> {
    m: Manager<'a>,
    ground: &'a Ground,
    rules: &'a Rules,
    rects: Vec<Rect>,
    entrances: Vec<Tile>,
    harvest: Vec<Tile>,
    safety: Vec<u8>,
    /// The cut kept, and whether a walk from the core reaches it.
    walls: Vec<(Tile, bool)>,
}

fn allowed(rules: &Rules, label: &str) -> usize {
    let name = match label {
        "wall" => "constructedWall",
        other => other,
    };
    rules.allowed(name, 8).max(0) as usize
}

impl Builder<'_> {
    fn more(&self, label: &str) -> bool {
        self.m.plan.count(label) < allowed(self.rules, label)
    }

    fn protect(&mut self, t: Tile, d: i32) {
        let (x, y) = xy(t);
        self.rects.push(Rect {
            x1: (x - d).max(2),
            x2: (x + d).min(47),
            y1: (y - d).max(2),
            y2: (y + d).min(47),
        });
    }

    fn container(&mut self, roads: &[Tile], label: &str) -> Option<Tile> {
        let pick = (roads.iter().take(2).copied())
            .find(|t| self.m.buildable(xy(*t).0, xy(*t).1, true, true))
            .or_else(|| self.link_spot(roads))?;
        self.m.plan_at(pick, &format!("container.{label}"), None);
        self.m.plan_at(pick, "container", Some(ROAD));
        Some(pick)
    }

    fn link_spot(&self, roads: &[Tile]) -> Option<Tile> {
        for r in roads.iter().take(3) {
            let (rx, ry) = xy(*r);
            let mut found = None;
            area(rx, ry, 1, |x, y| {
                if self.m.buildable(x, y, false, true) {
                    found = Some(at(x, y));
                }
                found.is_none()
            });
            if found.is_some() {
                return found;
            }
        }
        None
    }

    fn harvest_spot(&self, rock: Tile) -> Option<Tile> {
        let (sx, sy) = xy(rock);
        let mut best: Option<(Tile, i32)> = None;
        for near_exit in [false, true] {
            area(sx, sy, 1, |x, y| {
                if self.m.buildable(x, y, false, near_exit) {
                    let mut free = 0;
                    area(x, y, 1, |x2, y2| {
                        free += i32::from(self.m.buildable(x2, y2, false, near_exit));
                        true
                    });
                    if best.is_none_or(|(_, f)| f < free) {
                        best = Some((at(x, y), free));
                    }
                }
                true
            });
            if best.is_some() {
                break;
            }
        }
        best.map(|(t, _)| t)
    }

    fn harvesters(&mut self) -> Option<()> {
        let sources = self.ground.sources.clone();
        for (i, s) in sources.iter().enumerate() {
            let h = self.harvest_spot(*s)?;
            self.m.plan_at(h, "harvester", None);
            self.m.plan_at(h, &format!("harvester.{i}"), None);
            self.m.plan_at(h, "bay_center", None);
            let (hx, hy) = xy(h);
            area(hx, hy, 1, |x, y| {
                if !self.m.wall(x, y) {
                    self.m.discourage(x, y);
                }
                true
            });
            self.m.block(h);
            self.harvest.push(h);
        }
        for mineral in self.ground.minerals.clone() {
            self.m.place(mineral, "extractor");
            let roads = self.m.road(mineral, &self.entrances, false);
            for t in &roads {
                self.m.plan_at(*t, "road", Some(ROAD));
                self.m.plan_at(*t, "road.mineral", None);
            }
            self.container(&roads, "mineral");
            self.m.block(*roads.first()?);
        }
        Some(())
    }

    fn spots(&self, x: i32, y: i32, controller: Tile) -> i32 {
        let mut n = 0;
        area(x, y, 1, |x2, y2| {
            n += i32::from(!self.m.wall(x2, y2) && tiles::range(controller, at(x2, y2)) <= 3);
            true
        });
        n
    }

    fn upgrader(&mut self, controller: Tile) -> Option<()> {
        self.protect(controller, 1);
        let (cx, cy) = xy(controller);
        let mut best: Option<(Vec<Tile>, f64)> = None;
        area(cx, cy, 3, |x, y| {
            if self.m.wall(x, y) {
                return true;
            }
            let spots = self.spots(x, y, controller);
            if spots < 3 {
                return true;
            }
            let mut path = vec![at(x, y)];
            path.extend(self.m.road(at(x, y), &self.entrances, false));
            let score = path.len() as f64 - f64::from(spots) * 1.1;
            if best.as_ref().is_none_or(|(_, s)| *s > score) {
                best = Some((path, score));
            }
            true
        });
        let roads = best?.0;
        for (i, t) in roads.iter().enumerate() {
            self.protect(*t, 0);
            if i > 0 && tiles::range(*t, roads[0]) > 0 {
                self.m.plan_at(*t, "road", Some(ROAD));
                self.m.plan_at(*t, "road.controller", None);
            }
        }
        self.m.plan_at(roads[0], "upgrader.0", Some(ROAD));
        self.protect(roads[0], 1);
        let container = self.container(&roads, "controller")?;
        self.m.plan_at(container, "road", None);
        self.m.plan_at(container, "road.controller", None);

        let (ux, uy) = xy(roads[0]);
        let mut link: Option<(Tile, f64)> = None;
        area(ux, uy, 1, |x, y| {
            if self.m.buildable(x, y, false, true) {
                let mut score = f64::from(self.spots(x, y, controller) - 1);
                if tiles::range(controller, at(x, y)) > 3 {
                    score += 1.5;
                }
                if link.is_none_or(|(_, s)| s < score) {
                    link = Some((at(x, y), score));
                }
            }
            true
        });
        if let Some((l, _)) = link {
            self.m.plan_at(l, "link.controller", None);
            self.m.place(l, "link");
            self.protect(l, 0);
        }

        let (kx, ky) = xy(container);
        area(kx, ky, 1, |x, y| {
            if self.m.buildable(x, y, false, true) && self.m.terrain[at(x, y).index()] == SWAMP {
                self.m.plan_at(at(x, y), "road", Some(ROAD));
                self.m.plan_at(at(x, y), "road.controller", None);
            }
            true
        });
        self.m.block(roads[0]);
        Some(())
    }

    fn exits(&mut self, centers: &[Tile]) {
        for c in centers {
            for t in self.m.road(*c, &self.entrances, false) {
                self.m.plan_at(t, "road.exit", Some(ROAD));
            }
        }
        self.m.plan.remove_all("road.exit");
    }

    fn core(&mut self, c: Tile) {
        let (cx, cy) = xy(c);
        for (dx, dy) in [
            (-2, 0),
            (2, 0),
            (0, -2),
            (0, 2),
            (-1, -1),
            (-1, 1),
            (1, -1),
            (1, 1),
        ] {
            self.m.plan_at(at(cx + dx, cy + dy), "road", Some(ROAD));
        }
        self.m.place(at(cx, cy + 1), "storage");
        self.m.place(at(cx, cy - 1), "terminal");
        self.m.place(at(cx + 1, cy), "factory");
        self.m.place(at(cx - 1, cy), "link");
        self.m.place(at(cx - 1, cy), "link.storage");
    }

    fn bay(&mut self, bay: Tile, spawn: bool, source: bool, id: Option<usize>) {
        let near_exit = source && !spawn;
        let (bx, by) = xy(bay);
        if spawn && self.more("spawn") {
            area(bx, by, 1, |x, y| {
                if self.m.wall(x, y)
                    || !self.m.buildable(x, y, false, near_exit)
                    || (x, y) == (bx, by)
                {
                    return true;
                }
                let mut placed = false;
                area(x, y, 1, |x2, y2| {
                    if (x2, y2) == (bx, by) || !self.m.plan.has("road", at(x2, y2)) {
                        return true;
                    }
                    let n = self.m.plan.count("spawn");
                    self.m.place(at(x, y), &format!("spawn.{n}"));
                    self.m.place(at(x, y), "spawn");
                    placed = true;
                    false
                });
                !placed
            });
        }

        let mut linked = !self.more("link") || !source;
        area(bx, by, 1, |x, y| {
            if self.m.wall(x, y) || !self.m.buildable(x, y, false, near_exit) || (x, y) == (bx, by)
            {
                return true;
            }
            let t = at(x, y);
            if linked {
                self.m.place(t, "extension");
                if source {
                    self.m.place(t, "extension.harvester");
                } else {
                    if let Some(i) = id {
                        self.m.place(t, &format!("extension.bay.{i}"));
                    }
                    self.m.place(t, "extension.bay");
                }
            } else {
                self.m.place(t, "link");
                if source {
                    self.m.place(t, "link.source");
                }
                linked = true;
            }
            true
        });
    }

    fn harvest_bays(&mut self, spawns: &[bool]) {
        for (i, h) in self.harvest.clone().into_iter().enumerate() {
            let spawn = spawns.get(i).copied().unwrap_or(false);
            for t in self.m.road(h, &self.entrances, false) {
                self.m.plan_at(t, "road", Some(ROAD));
                self.m.plan_at(t, "road.source", None);
                if spawn {
                    self.protect(t, 0);
                }
            }
            self.m.plan_at(h, "container.source", None);
            self.m.plan_at(h, "container", None);
            self.bay(h, spawn, true, None);
        }
    }

    fn parking(&mut self, center: Tile) {
        self.m.start(Some((center, self.entrances.clone())));
        let Some(next) = self.m.next_spot() else {
            return;
        };
        self.m.plan_at(next, "road", Some(BLOCKED));
        self.m.place(next, "helper_parking");
        self.m.access_road(next);
        self.m.filter(next);
    }

    fn labs(&mut self) {
        self.m.start(None);
        while self.more("lab") {
            let Some(next) = self.m.next_spot() else {
                break;
            };
            let (x, y) = xy(next);
            if self.m.exit_distance(x, y) < 8 {
                continue;
            }
            for (dx, dy) in [(1, 1), (-1, 1), (1, -1), (-1, -1)] {
                let around = [
                    (x - dx, y + dy),
                    (x - dx, y),
                    (x - dx, y - dy),
                    (x, y - dy),
                    (x + dx, y - dy),
                    (x, y + 2 * dy),
                    (x + dx, y + 2 * dy),
                    (x + 2 * dx, y + 2 * dy),
                    (x + 2 * dx, y + dy),
                    (x + 2 * dx, y),
                ];
                let free = |m: &Manager, (a, b): (i32, i32)| m.buildable(a, b, false, false);
                let fits = [(x, y), (x + dx, y), (x, y + dy), (x + dx, y + dy)]
                    .iter()
                    .all(|c| free(&self.m, *c))
                    && around.iter().filter(|c| free(&self.m, **c)).count() > 8;
                if !fits {
                    continue;
                }
                self.m.plan_at(at(x, y), "road", Some(ROAD));
                for (lx, ly) in [(x + dx, y), (x, y + dy)] {
                    self.m.place(at(lx, ly), "lab");
                    self.m.place(at(lx, ly), "lab.reaction");
                }
                self.m.plan_at(at(x + dx, y + dy), "road", Some(ROAD));
                self.m.access_road(next);
                for (lx, ly) in around {
                    if !self.more("lab") {
                        break;
                    }
                    if free(&self.m, (lx, ly)) {
                        self.m.place(at(lx, ly), "lab");
                        self.m.place(at(lx, ly), "lab.reaction");
                    }
                }
                break;
            }
        }
        self.m.start(None);
    }

    fn bay_spot(&mut self, center: Tile) -> Option<Tile> {
        let mut most = 0;
        let mut best: Option<(Tile, f64)> = None;
        self.m.start(Some((center, self.entrances.clone())));
        while most < 8 {
            let Some(next) = self.m.next_spot() else {
                break;
            };
            let (x, y) = xy(next);
            if self.m.exit_distance(x, y) < 8 || !self.m.buildable(x, y, false, false) {
                continue;
            }
            let count = [
                (-1, 0),
                (1, 0),
                (0, -1),
                (0, 1),
                (-1, -1),
                (1, -1),
                (-1, 1),
                (1, 1),
            ]
            .iter()
            .filter(|(dx, dy)| self.m.buildable(x + dx, y + dy, false, false))
            .count();
            if count <= most {
                continue;
            }
            most = count;
            let score = count as f64 / f64::from(self.m.spot_range() + 10);
            if score > best.map_or(0.0, |b| b.1) && count >= 4 {
                best = Some((next, score));
            }
        }
        if most < 4 { None } else { best.map(|b| b.0) }
    }

    fn bays(&mut self, center: Tile) {
        self.m.start(None);
        let mut id = 0;
        while self.more("extension") {
            let Some(pos) = self.bay_spot(center) else {
                return;
            };
            self.m.access_road(pos);
            self.m.plan_at(pos, "bay_center", Some(ROAD));
            self.bay(pos, true, false, Some(id));
            id += 1;
            self.protect(pos, RAMPART_DISTANCE);
            self.m.start(None);
        }
    }

    fn ramparts(&mut self, controller: Tile, safe: [bool; 4], center: Tile) {
        self.protect(controller, 1);
        let kept = [
            "spawn",
            "rampart",
            "storage",
            "tower",
            "observer",
            "powerSpawn",
            "lab",
            "terminal",
            "nuker",
            "factory",
            "constructedWall",
        ];
        for label in self.m.plan.labels().to_vec() {
            let base = label.split('.').next().unwrap_or("");
            if kept.contains(&base) {
                for t in self.m.plan.of(&label) {
                    self.protect(t, RAMPART_DISTANCE);
                }
            }
        }
        let terrain = self.m.terrain;
        let wall = |x: i32, y: i32| terrain[at(x, y).index()] & WALL != 0;
        let cut = cut::tiles_of(&wall, &self.rects, safe);
        self.prune(&cut, center, controller, safe);
        for (t, walked) in self.walls.clone() {
            if self.m.terrain[t.index()] & WALL == 0 {
                self.m.plan_at(t, "rampart", None);
                if walked {
                    self.m.plan_at(t, "road", None);
                    self.m.plan_at(t, "road.rampart", None);
                }
            }
        }
    }

    /// `pruneWalls`: a cut tile is kept where the floods from the core and
    /// from the exits both touch it.
    fn prune(&mut self, cut: &[Tile], center: Tile, controller: Tile, safe: [bool; 4]) {
        self.safety = vec![ENDANGERED; tiles::COUNT];
        let mut relevant = vec![false; cut.len()];
        let mut irrelevant = vec![false; cut.len()];
        // Hivemind floods from the core alone, so it drops the cut round a
        // protected tile no walk from the core reaches, as one behind a wall.
        let mut kept = Vec::new();
        for r in &self.rects {
            for t in (r.x1..=r.x2).flat_map(|x| (r.y1..=r.y2).map(move |y| at(x, y))) {
                match cut.iter().position(|c| *c == t) {
                    Some(w) => relevant[w] = true,
                    None if self.m.terrain[t.index()] & WALL == 0 => kept.push(t),
                    None => (),
                }
            }
        }
        self.flood(cut, &kept, ENDANGERED, &mut relevant, &irrelevant);
        let mut starts = vec![center, controller];
        starts.extend(&self.ground.sources);
        starts.extend(&self.ground.minerals);
        let mut walked = vec![false; cut.len()];
        self.flood(cut, &starts, SAFE, &mut walked, &irrelevant);
        for (r, w) in relevant.iter_mut().zip(&walked) {
            *r |= w;
        }

        let open = |x: i32, y: i32| self.m.terrain[at(x, y).index()] & WALL == 0;
        let mut exits = Vec::new();
        for i in 0..50 {
            for (side, x, y) in [(3, 0, i), (1, 49, i), (0, i, 0), (2, i, 49)] {
                if open(x, y) && !safe[side] {
                    exits.push(at(x, y));
                }
            }
        }
        for i in 0..cut.len() {
            irrelevant[i] = !relevant[i];
            relevant[i] = false;
        }
        self.flood(cut, &exits, UNSAFE, &mut relevant, &irrelevant);

        for x in 0..50 {
            for y in 0..50 {
                if self.safety[at(x, y).index()] == UNSAFE {
                    area(x, y, 3, |ax, ay| {
                        let s = &mut self.safety[at(ax, ay).index()];
                        if *s == SAFE {
                            *s = ENDANGERED;
                        }
                        true
                    });
                }
            }
        }
        self.walls = (cut.iter().zip(relevant).zip(walked))
            .filter(|((_, r), _)| *r)
            .map(|((t, _), w)| (*t, w))
            .collect();
    }

    fn flood(
        &mut self,
        cut: &[Tile],
        starts: &[Tile],
        value: u8,
        relevant: &mut [bool],
        irrelevant: &[bool],
    ) {
        let mut order: Vec<Tile> = Vec::new();
        let mut listed = vec![false; tiles::COUNT];
        let mut closed = vec![false; tiles::COUNT];
        for s in starts {
            if !listed[s.index()] {
                listed[s.index()] = true;
                order.push(*s);
            }
        }
        let mut i = 0;
        while i < order.len() {
            let t = order[i];
            i += 1;
            self.safety[t.index()] = value;
            closed[t.index()] = true;
            let (x, y) = xy(t);
            area(x, y, 1, |nx, ny| {
                let n = at(nx, ny);
                if n == t || !(1..=48).contains(&nx) || !(1..=48).contains(&ny) {
                    return true;
                }
                if self.m.wall(nx, ny) && !self.m.plan.has("road", n) {
                    return true;
                }
                if listed[n.index()] || closed[n.index()] {
                    return true;
                }
                if let Some(w) = cut.iter().position(|c| *c == n).filter(|w| !irrelevant[*w]) {
                    relevant[w] = true;
                    closed[n.index()] = true;
                } else {
                    listed[n.index()] = true;
                    order.push(n);
                }
                true
            });
        }
    }

    fn quad(&mut self) {
        for r in self.m.plan.of("rampart") {
            let (rx, ry) = xy(r);
            area(rx, ry, 3, |x, y| {
                let t = at(x, y);
                if self.safety[t.index()] != UNSAFE || self.m.wall(x, y) {
                    return true;
                }
                self.safety[t.index()] = UNSAFE_NEAR_WALL;
                if self.m.exit_distance(x, y) < 3
                    || self.m.blocked(x, y)
                    || self.m.plan.has("road", t)
                {
                    return true;
                }
                let near = [(-1, 0), (1, 0), (0, -1), (0, 1)].iter().any(|(dx, dy)| {
                    let n = at(x + dx, y + dy);
                    self.m.plan.has("road", n) && !self.m.plan.has("road.rampart", n)
                });
                if near || (x + y) % 2 != 0 {
                    self.m.plan_at(t, "wall", None);
                    self.m.plan_at(t, "wall.quad", None);
                }
                true
            });
        }
    }

    fn decorative(&mut self) {
        for x in 1..49 {
            for y in 1..49 {
                let t = at(x, y);
                if DECORATIVE[(y % 14) as usize][(x % 12) as usize] == 0
                    || self.safety[t.index()] != UNSAFE
                    || self.m.exit_distance(x, y) < 3
                    || self.m.blocked(x, y)
                    || self.m.plan.has("road", t)
                {
                    continue;
                }
                self.m.plan_at(t, "wall", None);
                self.m.plan_at(t, "wall.deco", None);
            }
        }
    }

    fn seal(&mut self) {
        for x in 1..49 {
            for y in 1..49 {
                let t = at(x, y);
                if self.m.wall(x, y) {
                    self.m.block(t);
                } else if self.safety[t.index()] == SAFE {
                    self.m.plan_at(t, "safe", None);
                } else if self.safety[t.index()] != ENDANGERED {
                    self.m.block(t);
                }
            }
        }
    }

    fn effect(a: Tile, b: Tile) -> f64 {
        let r = (tiles::range(a, b) + 2).clamp(5, 20);
        1.0 - f64::from(r - 5) / 15.0
    }

    fn towers(&mut self, safe: [bool; 4]) {
        let mut positions: Vec<(Tile, f64)> = Vec::new();
        if !safe.iter().all(|s| *s) {
            for x in 1..49 {
                for y in 1..49 {
                    if self.m.buildable(x, y, false, false) && self.safety[at(x, y).index()] == SAFE
                    {
                        positions.push((at(x, y), 0.0));
                    }
                }
            }
        }
        let mut ramparts: Vec<(Tile, f64)> = self
            .m
            .plan
            .of("rampart")
            .into_iter()
            .map(|t| (t, 1.0))
            .collect();
        let mut count = 0;
        let total = allowed(self.rules, "tower");
        while self.more("tower") {
            let mut fresh: Vec<Tile> = Vec::new();
            let remaining = total - self.m.plan.count("tower");
            self.rescore(&mut ramparts, &mut positions);
            while fresh.len() < remaining {
                let mut best: Option<usize> = None;
                for (i, p) in positions.iter().enumerate() {
                    if best.is_none_or(|b| p.1 > positions[b].1) {
                        best = Some(i);
                    }
                }
                let Some(b) = best.filter(|b| positions[*b].1 >= 0.0) else {
                    break;
                };
                positions[b].1 = -1.0;
                let pos = positions[b].0;
                if !self.m.accessible(pos) {
                    continue;
                }
                fresh.push(pos);
                self.m.place(pos, "tower_placeholder");
                if fresh.len() < remaining {
                    self.rescore(&mut ramparts, &mut positions);
                }
            }
            if fresh.is_empty() {
                break;
            }
            for pos in fresh {
                if !self.m.accessible(pos) {
                    continue;
                }
                self.m.plan_at(pos, "tower", None);
                self.m.plan.remove("tower_placeholder", pos);
                self.m.access_road(pos);
                self.m.place(pos, &format!("tower.{count}"));
                count += 1;
            }
            self.m.plan.remove_all("tower_placeholder");
        }
    }

    fn rescore(&self, ramparts: &mut [(Tile, f64)], positions: &mut [(Tile, f64)]) {
        let towers: Vec<Tile> = (self.m.plan.of("tower").into_iter())
            .chain(self.m.plan.of("tower_placeholder"))
            .collect();
        for r in ramparts.iter_mut() {
            let mut s = 1.0;
            for t in &towers {
                s *= 1.0 - 0.8 * Builder::effect(*t, r.0);
            }
            r.1 = s;
        }
        for p in positions.iter_mut() {
            let (x, y) = xy(p.0);
            if !self.m.buildable(x, y, false, false) {
                p.1 = -1.0;
            }
            if p.1 < 0.0 {
                continue;
            }
            let mut s = 0.0;
            for r in ramparts.iter() {
                s += r.1 * Builder::effect(r.0, p.0);
            }
            p.1 = s;
        }
    }

    fn groups(&self) -> Vec<Vec<Tile>> {
        let all = self.m.plan.of("rampart");
        let mut used = vec![false; all.len()];
        let mut groups = Vec::new();
        for i in 0..all.len() {
            if used[i] {
                continue;
            }
            used[i] = true;
            let mut group = vec![all[i]];
            loop {
                let mut added = false;
                for j in 0..all.len() {
                    if !used[j] && group.iter().any(|g| tiles::range(*g, all[j]) == 1) {
                        used[j] = true;
                        group.push(all[j]);
                        added = true;
                    }
                }
                if !added {
                    break;
                }
            }
            groups.push(group);
        }
        groups
    }

    fn roads_to_ramps(&mut self) {
        for group in self.groups() {
            if !group.iter().any(|t| self.m.plan.has("road.rampart", *t)) {
                continue;
            }
            for t in self.m.road(self.entrances[0], &group, true) {
                if self.m.plan.has("extension", t) {
                    self.m.plan.remove("extension", t);
                    self.m.plan.remove("extension.bay", t);
                    for i in 0..10 {
                        self.m.plan.remove(&format!("extension.bay.{i}"), t);
                    }
                    self.m.matrix[t.index()] = 0;
                }
                self.m.plan_at(t, "road", Some(ROAD));
            }
        }
    }

    fn on_ramps(&mut self) {
        for r in self.m.plan.of("rampart") {
            let (rx, ry) = xy(r);
            area(rx, ry, 3, |x, y| {
                let t = at(x, y);
                if self.safety[t.index()] == ENDANGERED
                    && !self.m.wall(x, y)
                    && (self.m.plan.has("road", t) || self.m.plan.has("road.toRampart", t))
                    && !self.m.plan.has("rampart", t)
                {
                    self.m.plan_at(t, "rampart", None);
                    self.m.plan_at(t, "rampart.ramp", None);
                }
                true
            });
        }
    }
}

/// One variation's plan; None where Hivemind's builder throws.
pub fn build(rules: &Rules, ground: &Ground, dist: (&[u8], &[u8]), v: &Variation) -> Option<Built> {
    let controller = ground.controller?;
    let terrain = &ground.terrain[..];
    let safe = safe_sides(terrain);
    let mut b = Builder {
        m: Manager::new(terrain, dist.0, dist.1),
        ground,
        rules,
        rects: Vec::new(),
        entrances: Vec::new(),
        harvest: Vec::new(),
        safety: vec![ENDANGERED; tiles::COUNT],
        walls: Vec::new(),
    };
    let centers = exit_centers(terrain);
    for c in &centers {
        b.m.plan_at(*c, "exit", None);
    }
    let center = v.center;
    let (cx, cy) = xy(center);
    b.entrances = vec![
        at(cx + 2, cy),
        at(cx - 2, cy),
        at(cx, cy + 2),
        at(cx, cy - 2),
    ];
    b.m.plan_at(center, "center", None);
    b.harvesters()?;
    b.upgrader(controller)?;
    b.exits(&centers);
    b.core(center);
    b.harvest_bays(&v.spawns);
    b.parking(center);
    b.labs();
    b.bays(center);
    b.m.place_all("powerSpawn", allowed(rules, "powerSpawn"), true);
    b.m.place_all("nuker", allowed(rules, "nuker"), true);
    b.m.place_all("observer", allowed(rules, "observer"), false);
    b.ramparts(controller, safe, center);
    b.quad();
    b.decorative();
    b.seal();
    b.towers(safe);
    b.roads_to_ramps();
    b.on_ramps();
    Some(Built {
        plan: b.m.plan,
        center,
    })
}
