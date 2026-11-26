//! A highway room's terrain, ported from xxscreeps's `room-gen.js` (ISC, Marcel Laverdet), whose
//! constants it fitted to the live world: an open lane, wall masses grown from world-space noise on
//! the sector-facing borders, stamped clutter, and a mild swamp on half the rooms.
use krebs::{
    mapgen::{BOTTOM, LEFT, RIGHT, TOP},
    room_name::RoomXY,
    terrain::RoomTerrain,
};
use krebs_util::rng::Rng;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Lane {
    Vertical,
    Horizontal,
    Crossing,
}

impl Lane {
    pub fn of(xy: RoomXY) -> Lane {
        let axis = |c: i32| (if c < 0 { -c - 1 } else { c }) % 10 == 0;
        match (axis(xy.x), axis(xy.y)) {
            (true, true) => Lane::Crossing,
            (true, false) => Lane::Vertical,
            _ => Lane::Horizontal,
        }
    }

    /// The borders the lane runs out through; the others face the sectors and carry the masses.
    pub fn ends(self, side: usize) -> bool {
        match self {
            Lane::Vertical => side == TOP || side == BOTTOM,
            Lane::Horizontal => side == LEFT || side == RIGHT,
            Lane::Crossing => true,
        }
    }
}

struct Mass {
    base: f64,
    amp: f64,
    expo: f64,
}

const LANE_MASS: Mass = Mass { base: 0.5, amp: 27.5, expo: 2.9 };
const CORNER_MASS: Mass = Mass { base: 0.2, amp: 19.0, expo: 2.5 };
const SEAL_FLOOR: f64 = 3.0;
const CLIP_SLOPE: f64 = 3.0;
const BLOB_CELL: i32 = 8;
const BLOB_RADII: (f64, f64) = (1.3, 3.6);
const BLOB_ASPECT: f64 = 1.35;
/// Swamp type 1 of map.js: fill, smoothing passes, factor.
const SWAMP: (f64, u32, u32) = (0.3, 3, 5);

/// The world-space noise one seed lays: masses flow across the borders rooms share.
pub struct Field(pub u32);

impl Field {
    fn lattice(&self, ix: i32, iy: i32) -> f64 {
        let mix = |mut h: u32| {
            h ^= h >> 16;
            h = h.wrapping_mul(0x21f0_aaad);
            h ^= h >> 15;
            h = h.wrapping_mul(0x735a_2d97);
            h ^ (h >> 15)
        };
        let combine = |l: u32, r: u32| mix(l.wrapping_sub(0x61c8_8647).wrapping_add(r));
        f64::from(combine(combine(mix(ix as u32), iy as u32), self.0)) / 4_294_967_296.0
    }

    fn value(&self, wx: f64, wy: f64, cell: f64) -> f64 {
        let (gx, gy) = (wx / cell, wy / cell);
        let (ix, iy) = (gx.floor() as i32, gy.floor() as i32);
        let ease = |t: f64| t * t * (3.0 - 2.0 * t);
        let (sx, sy) = (ease(gx - f64::from(ix)), ease(gy - f64::from(iy)));
        let row = |y| {
            let a = self.lattice(ix, y);
            a + (self.lattice(ix + 1, y) - a) * sx
        };
        let top = row(iy);
        top + (row(iy + 1) - top) * sy
    }

    fn depth(&self, wx: i32, wy: i32, mass: &Mass) -> f64 {
        let (wx, wy) = (f64::from(wx), f64::from(wy));
        let noise =
            self.value(wx, wy, 22.0) * 0.7 + self.value(wx + 1000.0, wy + 1000.0, 6.0) * 0.3;
        mass.base + mass.amp * noise.powf(mass.expo)
    }

    /// The opening a lane end gets: 21 to 43 tiles, unbroken, centred between its flanking masses.
    pub fn lane_exit(&self, xy: RoomXY, lane: Lane, side: usize) -> Vec<u8> {
        let mass = if lane == Lane::Crossing { &CORNER_MASS } else { &LANE_MASS };
        let along = side == LEFT || side == RIGHT;
        let inboard = if side == TOP || side == LEFT { 2 } else { 47 };
        let at = |end: i32| {
            let (bx, by) = if along { (inboard, end) } else { (end, inboard) };
            let depth = self.depth(xy.x * 50 + bx, xy.y * 50 + by, mass);
            (depth * taper(inboard)).round() as i32
        };
        let (from, to) = (at(0) + 1, 48 - at(49));
        let length = (to - from + 1).clamp(21, 43);
        let start = (f64::from(from + to - length) / 2.0).round() as i32;
        (start.clamp(2, 48 - length)..start.clamp(2, 48 - length) + length)
            .map(|i| i as u8)
            .collect()
    }

    fn clutter(&self, wox: i32, woy: i32) -> Vec<(f64, f64, f64, f64)> {
        let reach = 2;
        let cells =
            |o: i32| o.div_euclid(BLOB_CELL) - reach..=(o + 49).div_euclid(BLOB_CELL) + reach;
        let mut blobs = Vec::new();
        for ix in cells(wox) {
            for iy in cells(woy).filter(|&iy| self.lattice(ix, iy) < 0.4) {
                let roll = self.lattice(ix + 0x1000, iy + 0x1000);
                let radius = BLOB_RADII.0 + (BLOB_RADII.1 - BLOB_RADII.0) * roll * roll;
                let stretch = BLOB_ASPECT.sqrt();
                let (rx, ry) = if self.lattice(ix + 0x3000, iy + 0x3000) < 0.5 {
                    (radius / stretch, radius * stretch)
                } else {
                    (radius * stretch, radius / stretch)
                };
                let cx = (f64::from(ix) + self.lattice(ix + 0x2000, iy)) * f64::from(BLOB_CELL);
                let cy = (f64::from(iy) + self.lattice(ix, iy + 0x2000)) * f64::from(BLOB_CELL);
                blobs.push((cx, cy, rx, ry));
            }
        }
        blobs
    }

    /// `exits` by side, as `GenRoomOpts::exits`; an empty side is sealed.
    pub fn terrain(&self, xy: RoomXY, exits: &[Vec<u8>; 4], rng: &mut Rng) -> RoomTerrain {
        let lane = Lane::of(xy);
        let (wox, woy) = (xy.x * 50, xy.y * 50);
        let mass = if lane == Lane::Crossing { &CORNER_MASS } else { &LANE_MASS };
        let tile = |side: usize, i: usize| [(i, 0), (49, i), (i, 49), (0, i)][side];
        let mut open = [false; 2500];
        let mut points = Vec::new();
        for side in [TOP, RIGHT, BOTTOM, LEFT] {
            for &e in &exits[side] {
                let (x, y) = tile(side, usize::from(e));
                let (ix, iy) = [(x, 1), (48, y), (x, 48), (1, y)][side];
                open[y * 50 + x] = true;
                open[iy * 50 + ix] = true;
                points.push((x as i32, y as i32));
            }
        }
        let depth = |side: usize| -> Vec<f64> {
            let sealed = exits[side].is_empty();
            if lane != Lane::Crossing && lane.ends(side) && !sealed {
                return vec![0.0; 50];
            }
            let mass = if sealed { &LANE_MASS } else { mass };
            let floor = if sealed { SEAL_FLOOR } else { 0.0 };
            (0..50)
                .map(|i| {
                    let (bx, by) = tile(side, i);
                    let (bx, by) = (bx as i32, by as i32);
                    let d = self.depth(wox + bx, woy + by, mass) * taper(i as i32);
                    d.max(floor) * clearance(bx, by, &points)
                })
                .collect()
        };
        let bound = |side: usize, low: bool| match (
            low,
            exits[side].iter().min(),
            exits[side].iter().max(),
        ) {
            (true, Some(&lo), _) => f64::from(lo) - 1.0,
            (false, _, Some(&hi)) => 48.0 - f64::from(hi),
            _ => f64::INFINITY,
        };
        let clip = |d: Vec<f64>, low: f64, high: f64| -> Vec<f64> {
            let away = |n: i32| f64::from(n.max(0)) * CLIP_SLOPE;
            (d.into_iter().enumerate())
                .map(|(i, t)| t.min(low + away(i as i32 - 2)).min(high + away(47 - i as i32)))
                .collect()
        };
        let left = clip(depth(LEFT), bound(TOP, true), bound(BOTTOM, true));
        let right = clip(depth(RIGHT), bound(TOP, false), bound(BOTTOM, false));
        let top = clip(depth(TOP), bound(LEFT, true), bound(RIGHT, true));
        let bottom = clip(depth(BOTTOM), bound(LEFT, false), bound(RIGHT, false));
        let blobs = self.clutter(wox, woy);
        let mut wall = [false; 2500];
        for (i, w) in wall.iter_mut().enumerate().filter(|(i, _)| !open[*i]) {
            let (x, y) = (i % 50, i / 50);
            let (fx, fy) = (x as f64, y as f64);
            let (wx, wy) = (f64::from(wox) + fx, f64::from(woy) + fy);
            *w = border(x, y)
                || fx <= left[y]
                || 49.0 - fx <= right[y]
                || fy <= top[x]
                || 49.0 - fy <= bottom[x]
                || (blobs.iter())
                    .any(|b| ((wx - b.0) / b.2).powi(2) + ((wy - b.1) / b.3).powi(2) <= 1.0);
        }
        for _ in 0..2 {
            wall = smooth(&wall, 5, true, &open);
        }
        connect(&mut wall, exits);
        fill_unreachable(&mut wall);
        let mut swamp = [false; 2500];
        if rng.next_f64() >= 0.5 {
            for (i, s) in swamp.iter_mut().enumerate() {
                *s = !open[i] && rng.next_f64() < SWAMP.0;
            }
            for _ in 0..SWAMP.1 {
                swamp = smooth(&swamp, SWAMP.2, false, &open);
            }
        }
        let mut terrain = RoomTerrain::default();
        for (i, (w, s)) in wall.into_iter().zip(swamp).enumerate() {
            let code = if w { 1 } else { 2 * u8::from(s) };
            terrain.set_raw((i % 50) as u8, (i / 50) as u8, code);
        }
        terrain
    }
}

fn border(x: usize, y: usize) -> bool {
    x == 0 || y == 0 || x == 49 || y == 49
}

/// Deepens a mass toward the corners, where the diagonal sector blocks reach, keeping its mean.
fn taper(along: i32) -> f64 {
    let falloff = |c: i32| 0.4 + 0.6 * (-f64::from(c) / 7.0).exp();
    let norm = (0..25).map(falloff).sum::<f64>() / 25.0;
    falloff(along.min(49 - along)) / norm
}

/// Recedes a mass within 3 tiles of an exit, so a throat opens as a mouth.
fn clearance(bx: i32, by: i32, points: &[(i32, i32)]) -> f64 {
    let near = points.iter().map(|&(x, y)| (bx - x).abs().max((by - y).abs())).min();
    near.filter(|&n| n < 3).map_or(1.0, |n| (f64::from(n) / 3.0).sqrt())
}

fn smooth(grid: &[bool; 2500], factor: u32, wall: bool, open: &[bool; 2500]) -> [bool; 2500] {
    let mut next = *grid;
    for (i, cell) in next.iter_mut().enumerate() {
        let (x, y) = ((i % 50) as i32, (i / 50) as i32);
        let mut count = 0;
        for (nx, ny) in (-1..=1).flat_map(|dy| (-1..=1).map(move |dx| (x + dx, y + dy))) {
            let inside = (0..50).contains(&nx) && (0..50).contains(&ny);
            count += u32::from(if inside { grid[(ny * 50 + nx) as usize] } else { wall });
        }
        *cell = count >= factor;
        if wall {
            *cell = (*cell || border(x as usize, y as usize)) && !open[i];
        }
    }
    next
}

const ORTHOGONAL: [(i32, i32); 4] = [(0, -1), (0, 1), (-1, 0), (1, 0)];

fn step(i: usize, (dx, dy): (i32, i32)) -> Option<usize> {
    let (x, y) = ((i % 50) as i32 + dx, (i / 50) as i32 + dy);
    ((0..50).contains(&x) && (0..50).contains(&y)).then_some((y * 50 + x) as usize)
}

/// Joins every exit throat to the lane, the largest open area one sits in, by the shortest slot
/// through whatever wall cut it off.
fn connect(wall: &mut [bool; 2500], exits: &[Vec<u8>; 4]) {
    let throats: Vec<usize> = [TOP, BOTTOM, LEFT, RIGHT]
        .into_iter()
        .flat_map(|side| exits[side].iter().map(move |&e| (side, usize::from(e))))
        .map(|(side, e)| {
            let (x, y) = [(e, 1), (48, e), (e, 48), (1, e)][side];
            y * 50 + x
        })
        .collect();
    let area = |from: usize| {
        let mut seen = vec![false; 2500];
        let mut stack = vec![from];
        seen[from] = true;
        while let Some(i) = stack.pop() {
            for n in ORTHOGONAL.iter().filter_map(|&d| step(i, d)) {
                if !wall[n] && !seen[n] {
                    seen[n] = true;
                    stack.push(n);
                }
            }
        }
        seen
    };
    let mut covered = vec![false; 2500];
    let mut lane: Option<(usize, Vec<bool>)> = None;
    for &t in &throats {
        if !covered[t] {
            let a = area(t);
            (0..2500).filter(|&i| a[i]).for_each(|i| covered[i] = true);
            let size = a.iter().filter(|&&s| s).count();
            if lane.as_ref().is_none_or(|(best, _)| size > *best) {
                lane = Some((size, a));
            }
        }
    }
    let Some((_, mut reached)) = lane else { return };
    for &t in &throats {
        if reached[t] {
            continue;
        }
        let mut prev = vec![usize::MAX; 2500];
        prev[t] = t;
        let mut queue = std::collections::VecDeque::from([t]);
        while let Some(i) = queue.pop_front() {
            if i != t && reached[i] {
                let mut at = i;
                loop {
                    if !border(at % 50, at / 50) {
                        wall[at] = false;
                    }
                    reached[at] = true;
                    if at == t {
                        break;
                    }
                    at = prev[at];
                }
                break;
            }
            for n in ORTHOGONAL.iter().filter_map(|&d| step(i, d)) {
                if prev[n] == usize::MAX && !(border(n % 50, n / 50) && wall[n]) {
                    prev[n] = i;
                    queue.push_back(n);
                }
            }
        }
    }
}

/// Walls the open ground no border reaches (8-connected): no reroll exists to discard it.
fn fill_unreachable(wall: &mut [bool; 2500]) {
    let mut reached = [false; 2500];
    let mut stack: Vec<usize> = (0..2500).filter(|&i| border(i % 50, i / 50) && !wall[i]).collect();
    if stack.is_empty() {
        return;
    }
    stack.iter().for_each(|&i| reached[i] = true);
    while let Some(i) = stack.pop() {
        let around = (-1..=1).flat_map(|dy| (-1..=1).map(move |dx| (dx, dy)));
        for n in around.filter_map(|d| step(i, d)) {
            if !wall[n] && !reached[n] {
                reached[n] = true;
                stack.push(n);
            }
        }
    }
    for (w, r) in wall.iter_mut().zip(reached) {
        *w |= !r;
    }
}
