//! The planner's one path search: least cost inside one room, every tie
//! settled by the tiles alone, so a road is the same whatever order a search
//! visits them in.
use std::{cmp::Reverse, collections::BinaryHeap};

use fabot_base::tiles::{self, Table, Tile};

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
}

/// `incomplete`: no goal reached, so the path ends nearest one.
pub struct Found {
    pub path: Vec<Tile>,
    pub incomplete: bool,
}

/// The engine never steps along an exit edge from a tile on it.
fn steps(from: Tile, onto: Tile) -> bool {
    let edge = |v: i32| v == 0 || v == tiles::SIDE - 1;
    let (x, y) = (tiles::x_of(from), tiles::y_of(from));
    if edge(x) { tiles::x_of(onto) != x } else { !edge(y) || tiles::y_of(onto) != y }
}

impl Search<'_> {
    fn cost(&self, t: Tile) -> Option<u32> {
        match self.matrix[t] {
            255 => None,
            0 => match self.terrain[t] {
                0 => Some(self.plain),
                2 => Some(self.swamp),
                _ => None,
            },
            c => Some(u32::from(c)),
        }
    }

    /// Each tile's least cost from `seeds`, until `stop` takes one.
    fn flood(
        &self,
        seeds: impl IntoIterator<Item = Tile>,
        mut stop: impl FnMut(Tile, u32) -> bool,
    ) -> (Table<u32>, Table<bool>) {
        let mut dist = tiles::table(u32::MAX);
        let mut done = tiles::table(false);
        let mut heap: BinaryHeap<_> = seeds.into_iter().map(|t| Reverse((0, t))).collect();
        for Reverse((_, t)) in &heap {
            dist[*t] = 0;
        }
        while let Some(Reverse((d, t))) = heap.pop() {
            if std::mem::replace(&mut done[t], true) {
                continue;
            }
            if stop(t, d) {
                break;
            }
            for &n in tiles::around(t) {
                if let Some(c) = self.cost(n).filter(|_| steps(t, n))
                    && d + c < dist[n]
                {
                    dist[n] = d + c;
                    heap.push(Reverse((d + c, n)));
                }
            }
        }
        (dist, done)
    }

    /// Each tile's least cost from beside `rock`.
    pub fn from(&self, rock: Tile) -> Table<u32> {
        self.flood(tiles::around(rock).iter().copied(), |_, _| false).0
    }

    pub fn run(&self, origin: Tile, goals: &[Goal]) -> Found {
        let short = |t: Tile| {
            (goals.iter())
                .map(|g| (tiles::range(t, g.tile) as u32).saturating_sub(g.range))
                .min()
                .unwrap_or(u32::MAX)
        };
        let mut nearest = (short(origin), 0, origin);
        let mut end = None;
        let (dist, done) = self.flood([origin], |t, d| {
            end = (short(t) == 0).then_some(t);
            nearest = nearest.min((short(t), d, t));
            end.is_some()
        });

        // Back from the end, a straight line kept where it ties, then the
        // lowest tile.
        let mut path = Vec::new();
        let mut at = end.unwrap_or(nearest.2);
        let mut ahead: Option<Tile> = None;
        while at != origin {
            path.push(at);
            let into = self.cost(at).map_or(u32::MAX, |c| dist[at] - c);
            let straight =
                |p: Tile| ahead.is_none_or(|a| tiles::direction(p, at) != tiles::direction(at, a));
            let back = (tiles::around(at).iter().copied())
                .filter(|p| done[*p] && dist[*p] == into && steps(*p, at))
                .min_by_key(|p| (straight(*p), *p));
            let Some(p) = back else { break };
            ahead = Some(at);
            at = p;
        }
        path.reverse();
        Found { path, incomplete: end.is_none() }
    }
}
