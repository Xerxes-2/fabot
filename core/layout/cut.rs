//! Hivemind's min cut (MIT, © David Franke, after Saruss and Chobobobo): Dinic
//! on a graph of tile halves, built and searched in its order so the cut is
//! the one its planner draws.
use fabot_base::tiles::{self, Tile};

const UNWALKABLE: i8 = -1;
const NORMAL: i8 = 0;
const PROTECTED: i8 = 1;
const TO_EXIT: i8 = 2;
const EXIT: i8 = 3;
const INFINITY: i64 = i64::MAX / 4;

const AROUND: [(i32, i32); 8] =
    [(0, -1), (-1, -1), (-1, 0), (-1, 1), (0, 1), (1, 1), (1, 0), (1, -1)];

pub struct Rect {
    pub x1: i32,
    pub y1: i32,
    pub x2: i32,
    pub y2: i32,
}

#[derive(Clone, Copy, Default)]
struct Edge {
    v: usize,
    rev: usize,
    c: i64,
    f: i64,
}

/// `[x][y]`, as Hivemind's array.
type Grid = Vec<[i8; 50]>;

/// `wall`: terrain by tile. `safe`: the sides (N, E, S, W) whose exits are
/// kept inside.
fn grid(wall: &dyn Fn(i32, i32) -> bool, safe: [bool; 4]) -> Grid {
    let mut a: Grid = vec![[UNWALKABLE; 50]; 50];
    for x in 0..50 {
        for y in 0..50 {
            if !wall(x, y) {
                a[x as usize][y as usize] =
                    if x == 0 || y == 0 || x == 49 || y == 49 { EXIT } else { NORMAL };
            }
        }
    }
    for i in 1..49usize {
        for (side, inner) in [(0usize, 1usize), (49, 48)] {
            if a[side][i - 1] == EXIT || a[side][i] == EXIT || a[side][i + 1] == EXIT {
                a[inner][i] = TO_EXIT;
            }
            if a[i - 1][side] == EXIT || a[i][side] == EXIT || a[i + 1][side] == EXIT {
                a[i][inner] = TO_EXIT;
            }
        }
    }
    let mut kept = vec![false; tiles::COUNT];
    for i in 1..49 {
        for (on, x, y) in [(safe[0], i, 1), (safe[2], i, 48), (safe[3], 1, i), (safe[1], 48, i)] {
            if on && a[x as usize][y as usize] == TO_EXIT {
                a[x as usize][y as usize] = PROTECTED;
                kept[tiles::index(x, y)] = true;
                for (dx, dy) in AROUND {
                    let cell = &mut a[(x + dx) as usize][(y + dy) as usize];
                    if *cell == NORMAL {
                        *cell = PROTECTED;
                    }
                }
            }
        }
    }
    for corner in [
        [(1, 1), (1, 2), (2, 1)],
        [(48, 1), (48, 2), (47, 1)],
        [(1, 48), (1, 47), (2, 48)],
        [(48, 48), (48, 47), (47, 48)],
    ] {
        let at = |a: &Grid, (x, y): (i32, i32)| a[x as usize][y as usize];
        if corner.iter().any(|c| at(&a, *c) == PROTECTED)
            && corner.iter().any(|c| at(&a, *c) == TO_EXIT)
        {
            let mut queue: Vec<(i32, i32)> =
                corner.into_iter().filter(|c| at(&a, *c) == PROTECTED).collect();
            while !queue.is_empty() {
                let (x, y) = queue.remove(0);
                if a[x as usize][y as usize] == PROTECTED {
                    a[x as usize][y as usize] =
                        if kept[tiles::index(x, y)] { TO_EXIT } else { NORMAL };
                    for (dx, dy) in AROUND {
                        if at(&a, (x + dx, y + dy)) == PROTECTED {
                            queue.push((x + dx, y + dy));
                        }
                    }
                }
            }
        }
    }
    for i in 1..49 {
        a[0][i] = UNWALKABLE;
        a[49][i] = UNWALKABLE;
        a[i][0] = UNWALKABLE;
        a[i][49] = UNWALKABLE;
    }
    a
}

/// Edges keep the order Hivemind pushed them: replay depends on it.
struct Graph {
    level: Vec<i32>,
    start: Vec<usize>,
    edges: Vec<Edge>,
}

impl Graph {
    fn of(n: usize, adds: &[(usize, usize, i64)]) -> Graph {
        let mut start = vec![0; n + 1];
        for &(u, v, _) in adds {
            start[u + 1] += 1;
            start[v + 1] += 1;
        }
        for i in 0..n {
            start[i + 1] += start[i];
        }
        let mut next = start.clone();
        let mut edges = vec![Edge::default(); start[n]];
        for &(u, v, c) in adds {
            let (i, j) = (next[u], next[v]);
            (next[u], next[v]) = (i + 1, j + 1);
            edges[i] = Edge { v, rev: j, c, f: 0 };
            edges[j] = Edge { v: u, rev: i, ..Edge::default() };
        }
        Graph { level: vec![-1; n], start, edges }
    }

    fn levels(&mut self, s: usize, t: usize) -> bool {
        self.level.fill(-1);
        self.level[s] = 0;
        let mut queue = std::collections::VecDeque::from([s]);
        while let Some(u) = queue.pop_front() {
            for e in &self.edges[self.start[u]..self.start[u + 1]] {
                if self.level[e.v] < 0 && e.f < e.c {
                    self.level[e.v] = self.level[u] + 1;
                    queue.push_back(e.v);
                }
            }
        }
        self.level[t] >= 0
    }

    fn flow(&mut self, u: usize, f: i64, t: usize, c: &mut [usize]) -> i64 {
        if u == t {
            return f;
        }
        while c[u] < self.start[u + 1] {
            let Edge { v, rev, c: cap, f: used } = self.edges[c[u]];
            if self.level[v] == self.level[u] + 1 && used < cap {
                let sent = self.flow(v, f.min(cap - used), t, c);
                if sent > 0 {
                    self.edges[c[u]].f += sent;
                    self.edges[rev].f -= sent;
                    return sent;
                }
            }
            c[u] += 1;
        }
        0
    }

    fn reachable_cut(&mut self, s: usize) -> Vec<usize> {
        let mut cut: Vec<(usize, usize)> = Vec::new();
        self.level.fill(-1);
        self.level[s] = 1;
        let mut queue = std::collections::VecDeque::from([s]);
        while let Some(u) = queue.pop_front() {
            for e in &self.edges[self.start[u]..self.start[u + 1]] {
                if e.f < e.c && self.level[e.v] < 1 {
                    self.level[e.v] = 1;
                    queue.push_back(e.v);
                }
                if e.f == e.c && e.c > 0 {
                    cut.push((u, e.v));
                }
            }
        }
        (cut.into_iter()).filter(|(_, v)| self.level[*v] == -1).map(|(u, _)| u).collect()
    }
}

/// Hivemind's `getCutTiles` on the whole room: the tiles parting `rects` from
/// the exits a raid can come by.
pub fn tiles_of(wall: &dyn Fn(i32, i32) -> bool, rects: &[Rect], safe: [bool; 4]) -> Vec<Tile> {
    let mut a = grid(wall, safe);
    for r in rects.iter().filter(|r| r.x1 <= r.x2 && r.y1 <= r.y2) {
        for x in r.x1..=r.x2 {
            for y in r.y1..=r.y2 {
                let cell = &mut a[x as usize][y as usize];
                if x == r.x1 || x == r.x2 || y == r.y1 || y == r.y2 {
                    if *cell == NORMAL {
                        *cell = PROTECTED;
                    }
                } else {
                    *cell = UNWALKABLE;
                }
            }
        }
    }

    let n = 2 * tiles::COUNT + 2;
    let (source, sink) = (n - 2, n - 1);
    let mut adds: Vec<(usize, usize, i64)> = Vec::new();
    for x in 1..49i32 {
        for y in 1..49i32 {
            let top = (y * 50 + x) as usize;
            let bot = top + tiles::COUNT;
            let cell = a[x as usize][y as usize];
            if cell == NORMAL || cell == PROTECTED {
                if cell == PROTECTED {
                    adds.push((source, top, INFINITY));
                }
                adds.push((top, bot, 1));
                for (dx, dy) in AROUND {
                    let next = a[(x + dx) as usize][(y + dy) as usize];
                    if next == NORMAL || next == TO_EXIT {
                        adds.push((bot, ((y + dy) * 50 + x + dx) as usize, INFINITY));
                    }
                }
            } else if cell == TO_EXIT {
                adds.push((top, sink, INFINITY));
            }
        }
    }

    let mut g = Graph::of(n, &adds);
    let mut total = 0;
    while g.levels(source, sink) {
        let mut count = g.start.clone();
        while let f @ 1.. = g.flow(source, INFINITY, sink, &mut count) {
            total += f;
        }
    }
    if total == 0 {
        return Vec::new();
    }
    (g.reachable_cut(source).into_iter()).map(|u| Tile::of_index(u % tiles::COUNT)).collect()
}
