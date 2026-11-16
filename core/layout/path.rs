//! xxscreeps' `PathFinder.search` (Marcel Laverdet's A* and jump point search
//! over a Dial heap) inside one room, with no ops limit as there, so a ported
//! planner draws the roads its original draws: ties fall as that heap lets them.
use crate::tiles::{self, Tile};

const OBSTACLE: u32 = u32::MAX;
/// One room, at world coordinates 50..100 so a step off it lands in another,
/// which a one-room search never loads.
const BASE: i32 = 50;

pub struct Goal {
    pub tile: Tile,
    pub range: u32,
}

/// `terrain`: 0 plain, 1 wall, 2 swamp. `matrix` a CostMatrix by tile, 255 an
/// obstacle and 0 the terrain's cost.
pub struct Search<'a> {
    pub terrain: &'a [u8],
    pub matrix: &'a [u8],
    pub plain: u32,
    pub swamp: u32,
    pub weight: f64,
}

pub struct Found {
    pub path: Vec<Tile>,
    pub incomplete: bool,
}

#[derive(Clone, Copy, PartialEq)]
struct P {
    x: i32,
    y: i32,
}

const NULL: P = P { x: 0, y: 0 };

fn border(v: i32) -> bool {
    (v + 1) % 50 < 2
}

fn near_border(v: i32) -> bool {
    (v + 2) % 50 < 4
}

struct Run<'a> {
    s: &'a Search<'a>,
    weight: f64,
    goals: Vec<(P, u32)>,
    scores: Vec<u32>,
    heap: Heap,
    open: Vec<u8>,
    parents: Vec<usize>,
}

impl Run<'_> {
    fn index(p: P) -> usize {
        ((p.x - BASE) * 50 + (p.y - BASE)) as usize
    }

    fn pos(index: usize) -> P {
        P {
            x: (index / 50) as i32 + BASE,
            y: (index % 50) as i32 + BASE,
        }
    }

    fn look(&self, p: P) -> u32 {
        if !(BASE..BASE + 50).contains(&p.x) || !(BASE..BASE + 50).contains(&p.y) {
            return OBSTACLE;
        }
        let t = tiles::index(p.x - BASE, p.y - BASE);
        match self.s.matrix[t.index()] {
            255 => OBSTACLE,
            0 => match self.s.terrain[t.index()] {
                0 => self.s.plain,
                2 => self.s.swamp,
                _ => OBSTACLE,
            },
            c => u32::from(c),
        }
    }

    fn heuristic(&self, p: P) -> u32 {
        let mut ret = u32::MAX;
        for (g, range) in &self.goals {
            let dist = (p.x - g.x).unsigned_abs().max((p.y - g.y).unsigned_abs());
            if dist > *range {
                ret = ret.min(dist - range);
            } else {
                ret = 0;
            }
        }
        ret
    }

    fn weighted(&self, p: P) -> u32 {
        (f64::from(self.heuristic(p)) * self.weight) as u32
    }

    fn push(&mut self, parent: usize, node: P, g: u32) {
        let index = Run::index(node);
        if self.open[index] == 2 {
            return;
        }
        let f = self.weighted(node).wrapping_add(g);
        if self.open[index] == 0 || self.scores[index] > f {
            self.scores[index] = f;
            self.heap.push((index, f));
            self.open[index] = 1;
            self.parents[index] = parent;
        }
    }

    fn astar(&mut self, index: usize, p: P, g: u32) {
        for d in 1..=8 {
            let n = P {
                x: p.x + tiles::dx(d),
                y: p.y + tiles::dy(d),
            };
            let edge = |v: i32, nv: i32, same: bool| -> bool {
                (v % 50 == 0 && (nv % 50 == 49 && !same || nv == v))
                    || (v % 50 == 49 && (nv % 50 == 0 && !same || nv == v))
            };
            if p.x % 50 == 0 || p.x % 50 == 49 {
                if edge(p.x, n.x, p.y == n.y) {
                    continue;
                }
            } else if (p.y % 50 == 0 || p.y % 50 == 49) && edge(p.y, n.y, p.x == n.x) {
                continue;
            }
            let c = self.look(n);
            if c != OBSTACLE {
                self.push(index, n, g + c);
            }
        }
    }

    fn jump_x(&self, cost: u32, mut p: P, dx: i32) -> P {
        let mut up = self.look(P { x: p.x, y: p.y - 1 });
        let mut down = self.look(P { x: p.x, y: p.y + 1 });
        loop {
            if self.heuristic(p) == 0 || near_border(p.x) {
                break;
            }
            let cu = self.look(P {
                x: p.x + dx,
                y: p.y - 1,
            });
            let cd = self.look(P {
                x: p.x + dx,
                y: p.y + 1,
            });
            if (cu != OBSTACLE && up != cost) || (cd != OBSTACLE && down != cost) {
                break;
            }
            up = cu;
            down = cd;
            p.x += dx;
            let c = self.look(p);
            if c == OBSTACLE {
                return NULL;
            } else if c != cost {
                break;
            }
        }
        p
    }

    fn jump_y(&self, cost: u32, mut p: P, dy: i32) -> P {
        let mut left = self.look(P { x: p.x - 1, y: p.y });
        let mut right = self.look(P { x: p.x + 1, y: p.y });
        loop {
            if self.heuristic(p) == 0 || near_border(p.y) {
                break;
            }
            let cl = self.look(P {
                x: p.x - 1,
                y: p.y + dy,
            });
            let cr = self.look(P {
                x: p.x + 1,
                y: p.y + dy,
            });
            if (cl != OBSTACLE && left != cost) || (cr != OBSTACLE && right != cost) {
                break;
            }
            left = cl;
            right = cr;
            p.y += dy;
            let c = self.look(p);
            if c == OBSTACLE {
                return NULL;
            } else if c != cost {
                break;
            }
        }
        p
    }

    fn jump_xy(&self, cost: u32, mut p: P, dx: i32, dy: i32) -> P {
        let mut px = self.look(P {
            x: p.x - dx,
            y: p.y,
        });
        let mut py = self.look(P {
            x: p.x,
            y: p.y - dy,
        });
        loop {
            if self.heuristic(p) == 0 || near_border(p.x) || near_border(p.y) {
                break;
            }
            if (self.look(P {
                x: p.x - dx,
                y: p.y + dy,
            }) != OBSTACLE
                && px != cost)
                || (self.look(P {
                    x: p.x + dx,
                    y: p.y - dy,
                }) != OBSTACLE
                    && py != cost)
            {
                break;
            }
            px = self.look(P {
                x: p.x,
                y: p.y + dy,
            });
            py = self.look(P {
                x: p.x + dx,
                y: p.y,
            });
            if (py != OBSTACLE
                && self.jump_x(
                    cost,
                    P {
                        x: p.x + dx,
                        y: p.y,
                    },
                    dx,
                ) != NULL)
                || (px != OBSTACLE
                    && self.jump_y(
                        cost,
                        P {
                            x: p.x,
                            y: p.y + dy,
                        },
                        dy,
                    ) != NULL)
            {
                break;
            }
            p.x += dx;
            p.y += dy;
            let c = self.look(p);
            if c == OBSTACLE {
                return NULL;
            } else if c != cost {
                break;
            }
        }
        p
    }

    fn jump(&self, cost: u32, p: P, dx: i32, dy: i32) -> P {
        match (dx, dy) {
            (_, 0) => self.jump_x(cost, p, dx),
            (0, _) => self.jump_y(cost, p, dy),
            _ => self.jump_xy(cost, p, dx, dy),
        }
    }

    fn jump_neighbor(&mut self, p: P, index: usize, n: P, g: u32, cost: u32, n_cost: u32) {
        let mut n = n;
        let mut g = g;
        if n_cost != cost || border(n.x) || border(n.y) {
            if n_cost == OBSTACLE {
                return;
            }
            g += n_cost;
        } else {
            n = self.jump(n_cost, n, n.x - p.x, n.y - p.y);
            if n == NULL {
                return;
            }
            let range = (n.x - p.x).unsigned_abs().max((n.y - p.y).unsigned_abs());
            g += n_cost * (range - 1) + self.look(n);
        }
        self.push(index, n, g);
    }

    fn jps(&mut self, index: usize, p: P, g: u32) {
        let parent = Run::pos(self.parents[index]);
        let dx = (p.x - parent.x).signum();
        let dy = (p.y - parent.y).signum();
        let at = |x: i32, y: i32| P { x, y };

        let special: Vec<P> = match (p.x % 50, p.y % 50) {
            (0, _) if dx == -1 => vec![at(p.x - 1, p.y)],
            (0, _) if dx == 1 => vec![at(p.x + 1, p.y - 1), at(p.x + 1, p.y), at(p.x + 1, p.y + 1)],
            (0, _) => Vec::new(),
            (49, _) if dx == 1 => vec![at(p.x + 1, p.y)],
            (49, _) if dx == -1 => {
                vec![at(p.x - 1, p.y - 1), at(p.x - 1, p.y), at(p.x - 1, p.y + 1)]
            }
            (49, _) => Vec::new(),
            (_, 0) if dy == -1 => vec![at(p.x, p.y - 1)],
            (_, 0) if dy == 1 => vec![at(p.x - 1, p.y + 1), at(p.x, p.y + 1), at(p.x + 1, p.y + 1)],
            (_, 49) if dy == 1 => vec![at(p.x, p.y + 1)],
            (_, 49) if dy == -1 => {
                vec![at(p.x - 1, p.y - 1), at(p.x, p.y - 1), at(p.x + 1, p.y - 1)]
            }
            _ => Vec::new(),
        };
        if !special.is_empty() {
            for n in special {
                let c = self.look(n);
                if c != OBSTACLE {
                    self.push(index, n, g + c);
                }
            }
            return;
        }

        let border_dx = match p.x % 50 {
            1 => -1,
            48 => 1,
            _ => 0,
        };
        let border_dy = match p.y % 50 {
            1 => -1,
            48 => 1,
            _ => 0,
        };

        let cost = self.look(p);
        if dx != 0 {
            let n = at(p.x + dx, p.y);
            let c = self.look(n);
            if c != OBSTACLE {
                if border_dy == 0 {
                    self.jump_neighbor(p, index, n, g, cost, c);
                } else {
                    self.push(index, n, g + c);
                }
            }
        }
        if dy != 0 {
            let n = at(p.x, p.y + dy);
            let c = self.look(n);
            if c != OBSTACLE {
                if border_dx == 0 {
                    self.jump_neighbor(p, index, n, g, cost, c);
                } else {
                    self.push(index, n, g + c);
                }
            }
        }

        let forced = |run: &mut Self, n: P| {
            let c = run.look(n);
            run.jump_neighbor(p, index, n, g, cost, c);
        };
        if dx != 0 {
            if dy != 0 {
                let n = at(p.x + dx, p.y + dy);
                if self.look(n) != OBSTACLE {
                    forced(self, n);
                }
                if self.look(at(p.x - dx, p.y)) != cost {
                    forced(self, at(p.x - dx, p.y + dy));
                }
                if self.look(at(p.x, p.y - dy)) != cost {
                    forced(self, at(p.x + dx, p.y - dy));
                }
            } else {
                if border_dy == 1 || self.look(at(p.x, p.y + 1)) != cost {
                    forced(self, at(p.x + dx, p.y + 1));
                }
                if border_dy == -1 || self.look(at(p.x, p.y - 1)) != cost {
                    forced(self, at(p.x + dx, p.y - 1));
                }
            }
        } else {
            if border_dx == 1 || self.look(at(p.x + 1, p.y)) != cost {
                forced(self, at(p.x + 1, p.y + dy));
            }
            if border_dx == -1 || self.look(at(p.x - 1, p.y)) != cost {
                forced(self, at(p.x - 1, p.y + dy));
            }
        }
    }
}

/// xxscreeps' Dial heap: a window of 256 last-in-first-out buckets, the
/// scores outside it in a binary heap.
#[derive(Default)]
struct Heap {
    buckets: Vec<Vec<(usize, u32)>>,
    count: usize,
    base: u32,
    overflow: Vec<(usize, u32)>,
}

const WINDOW: u32 = 256;

impl Heap {
    fn push(&mut self, value: (usize, u32)) {
        if self.buckets.is_empty() {
            self.buckets = vec![Vec::new(); WINDOW as usize];
        }
        let key = value.1;
        if key < self.base || key - self.base >= WINDOW {
            self.overflow.push(value);
            let last = self.overflow.len() - 1;
            self.sift_up(last);
        } else {
            self.buckets[(key % WINDOW) as usize].push(value);
            self.count += 1;
        }
    }

    fn sift_up(&mut self, mut pos: usize) {
        let val = self.overflow[pos];
        while pos != 0 {
            let parent = (pos - 1) / 2;
            if self.overflow[parent].1 > val.1 {
                self.overflow[pos] = self.overflow[parent];
                pos = parent;
            } else {
                break;
            }
        }
        self.overflow[pos] = val;
    }

    fn pop_overflow(&mut self) -> (usize, u32) {
        let h = &mut self.overflow;
        let size = h.len();
        let top = h[0];
        let mut hole = 0;
        loop {
            let (left, right) = (hole * 2 + 1, hole * 2 + 2);
            if right < size {
                let larger = if h[left].1 > h[right].1 { right } else { left };
                h[hole] = h[larger];
                hole = larger;
            } else {
                if left < size {
                    h[hole] = h[left];
                    hole = left;
                }
                break;
            }
        }
        h[hole] = h[size - 1];
        self.sift_up(hole);
        self.overflow.pop();
        top
    }

    fn pop(&mut self) -> Option<(usize, u32)> {
        if self.count == 0 {
            let top = *self.overflow.first()?;
            self.base = self.base.max(top.1);
            return Some(self.pop_overflow());
        }
        while self.buckets[(self.base % WINDOW) as usize].is_empty() {
            self.base += 1;
        }
        if self.overflow.first().is_some_and(|o| o.1 < self.base) {
            return Some(self.pop_overflow());
        }
        self.count -= 1;
        self.buckets[(self.base % WINDOW) as usize].pop()
    }
}

impl Search<'_> {
    pub fn run(&self, origin: Tile, goals: &[Goal]) -> Found {
        let p = |t: Tile| P {
            x: tiles::x_of(t) + BASE,
            y: tiles::y_of(t) + BASE,
        };
        let weight = self.weight.clamp(1.0, 9.0);
        let mut run = Run {
            s: self,
            weight,
            goals: goals.iter().map(|g| (p(g.tile), g.range)).collect(),
            scores: vec![0; tiles::COUNT],
            heap: Heap::default(),
            open: vec![0; tiles::COUNT],
            parents: vec![usize::MAX; tiles::COUNT],
        };
        let origin = p(origin);
        if goals.is_empty() || run.heuristic(origin) == 0 {
            return Found {
                path: Vec::new(),
                incomplete: false,
            };
        }
        let mut min_node = Run::index(origin);
        let mut min_h = u32::MAX;
        run.open[min_node] = 2;
        run.scores[min_node] = run.weighted(origin);
        run.astar(min_node, origin, 0);
        while let Some((index, f)) = run.heap.pop() {
            if run.scores[index] != f {
                continue;
            }
            run.open[index] = 2;
            let at = Run::pos(index);
            let h = run.heuristic(at);
            let g = f.wrapping_sub(run.weighted(at));
            if h == 0 {
                min_node = index;
                min_h = 0;
                break;
            } else if h < min_h {
                min_node = index;
                min_h = h;
            }
            if weight == 1.0 {
                run.astar(index, at, g);
            } else {
                run.jps(index, at, g);
            }
        }

        let mut path: Vec<Tile> = Vec::new();
        let tile = |q: P| tiles::index(q.x - BASE, q.y - BASE);
        let mut index = min_node;
        let mut at = Run::pos(index);
        while at != origin {
            path.push(tile(at));
            index = run.parents[index];
            let next = Run::pos(index);
            let (sx, sy) = ((next.x - at.x).signum(), (next.y - at.y).signum());
            while (next.x - at.x).abs().max((next.y - at.y).abs()) > 1 {
                at = P {
                    x: at.x + sx,
                    y: at.y + sy,
                };
                path.push(tile(at));
            }
            at = next;
        }
        path.reverse();
        Found {
            path,
            incomplete: min_h != 0,
        }
    }
}
