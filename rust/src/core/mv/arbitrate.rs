//! An augmenting search over the room's whole matching, so swaps and
//! rotations fall out of it rather than being special cases.
use crate::core::grid::Footprint;
use crate::core::mv::Wish;
use crate::core::tiles;

pub struct Resolution {
    pub steps: Vec<(String, i32)>,
}

fn covered(footprint: Footprint, anchor: i32) -> Vec<i32> {
    match footprint {
        Footprint::One => vec![anchor],
        Footprint::Quad => vec![
            anchor,
            anchor + 1,
            anchor + tiles::SIDE,
            anchor + tiles::SIDE + 1,
        ],
    }
}

fn in_room(tile: i32) -> bool {
    tile >= 0 && (tile as usize) < tiles::COUNT
}

// A stayer only ever asks back onto its work or its own tile, so movers go
// first. Names are unique, so the order is total and the input's never shows.
fn offered(a: &Wish, b: &Wish) -> std::cmp::Ordering {
    match a.stays().cmp(&b.stays()) {
        std::cmp::Ordering::Equal => match (b.weight.max(1)).cmp(&a.weight.max(1)) {
            std::cmp::Ordering::Equal => a.creep.cmp(&b.creep),
            c => c,
        },
        c => c,
    }
}

fn above(s: i32, h: i32, s0: i32, h0: i32) -> bool {
    s > s0 || (s == s0 && h > h0)
}

/// The four tables a resolution reads, kept across the rooms of one tick: a
/// room refills them from empty anyway, so they are allocations otherwise.
#[derive(Clone, Debug)]
pub struct Scratch {
    blocked: Vec<bool>,
    steps_onto: Vec<i32>,
    holder: Vec<i32>,
    changed_at: Vec<i32>,
}

pub fn scratch() -> Scratch {
    Scratch {
        blocked: vec![false; tiles::COUNT],
        steps_onto: vec![-1; tiles::COUNT],
        holder: vec![-1; tiles::COUNT],
        changed_at: vec![0; tiles::COUNT],
    }
}

/// `held`: the tiles of bodies outside the wishes (theirs, and ours with none).
pub fn resolve(
    scratch: &mut Scratch,
    passable: impl Fn(i32) -> bool,
    held: &[i32],
    wishes: &[Wish],
) -> Resolution {
    let blocked = &mut scratch.blocked;
    let steps_onto = &mut scratch.steps_onto;
    let holder = &mut scratch.holder;
    let changed_at = &mut scratch.changed_at;

    blocked.fill(false);
    steps_onto.fill(-1);
    holder.fill(-1);
    changed_at.fill(0);

    for &tile in held {
        if in_room(tile) {
            blocked[tile as usize] = true;
        }
    }

    // A swap a tick or two away beats the way round: with a held body that
    // steps onto this tile, or a mover through the same head onto it.
    for wish in wishes {
        if !wish.movable() {
            for tile in covered(wish.footprint, wish.at) {
                if in_room(tile) {
                    blocked[tile as usize] = true;
                }
            }
            if in_room(wish.at) {
                steps_onto[wish.at as usize] = if wish.stays() { -1 } else { wish.head };
            }
        }
    }

    let mut movable: Vec<&Wish> = wishes.iter().filter(|w| w.movable()).collect();
    movable.sort_by(|a, b| offered(a, b));

    let mut placed: Vec<Wish> = Vec::new();
    for wish in movable {
        if in_room(wish.at) && !blocked[wish.at as usize] && holder[wish.at as usize] < 0 {
            holder[wish.at as usize] = placed.len() as i32;
            placed.push(wish.clone());
        }
    }

    let order = placed;
    let n = order.len();
    let mut tile_of: Vec<i32> = order.iter().map(|w| w.at).collect();

    let head_on = |wish: &Wish| -> bool {
        order.iter().any(|other| {
            other.head == wish.head && other.onward == Some(wish.at) && other.creep != wish.creep
        })
    };

    fn consider(
        found: &mut Vec<i32>,
        blocked: &[bool],
        passable: &dyn Fn(i32) -> bool,
        at: i32,
        tile: i32,
    ) {
        if tile != at
            && in_room(tile)
            && tiles::range(at, tile) == 1
            && !blocked[tile as usize]
            && passable(tile)
            && !found.contains(&tile)
        {
            found.push(tile);
        }
    }

    let candidates: Vec<Vec<i32>> = order
        .iter()
        .map(|wish| {
            let at = wish.at;
            let mut found: Vec<i32> = Vec::new();
            if !(in_room(wish.head) && steps_onto[wish.head as usize] == at) {
                consider(&mut found, blocked, &passable, at, wish.head);
                if found.is_empty() || !head_on(wish) {
                    for tile in &wish.tail {
                        consider(&mut found, blocked, &passable, at, *tile);
                    }
                }
            }
            found.push(at);
            found
        })
        .collect();

    let reachable: Vec<i32> = (0..n)
        .map(|i| {
            candidates[i]
                .iter()
                .map(|tile| order[i].value(*tile))
                .max()
                .unwrap()
        })
        .collect();

    let mut search = Search {
        order: &order,
        candidates: &candidates,
        tile_of: tile_of.clone(),
        holder: holder.clone(),
        changed_at: changed_at.clone(),
        on_path: vec![false; n],
        label_stamp: vec![0; n],
        label_score: vec![0; n],
        label_home: vec![0; n],
        path: Vec::new(),
        best_chain: Vec::new(),
        best_score: 0,
        best_home: 0,
        taken: 0,
        stamp: 0,
    };

    let mut searched_at = vec![-1i32; n];
    let mut clock = 0i32;
    let mut changed = true;

    while changed {
        changed = false;
        for i in 0..n {
            let from = search.tile_of[i];
            let wish = &order[i];
            let stale = searched_at[i] < 0
                || candidates[i]
                    .iter()
                    .any(|tile| search.changed_at[*tile as usize] > searched_at[i]);
            let content = wish.value(from) == reachable[i]
                && (from == wish.at || wish.value(wish.at) < wish.value(from));

            if stale {
                searched_at[i] = clock;
            }

            if stale && !content {
                search.stamp += 1;
                search.best_chain.clear();
                search.best_score = 0;
                search.best_home = 0;
                search.taken = 0;
                search.holder[from as usize] = -1;
                search.explore(i, from, 0, 0, None);

                if !search.best_chain.is_empty() {
                    clock += 1;
                    let chain = search.best_chain.clone();
                    for &(m, _) in &chain {
                        let t = search.tile_of[m];
                        search.changed_at[t as usize] = clock;
                        search.holder[t as usize] = -1;
                    }
                    for &(m, tile) in &chain {
                        search.changed_at[tile as usize] = clock;
                        search.tile_of[m] = tile;
                        search.holder[tile as usize] = m as i32;
                    }
                    changed = true;
                } else {
                    search.holder[from as usize] = i as i32;
                }
            }
        }
    }

    tile_of = search.tile_of.clone();

    let mut steps: Vec<(String, i32)> = (0..n)
        .filter(|i| tile_of[*i] != order[*i].at)
        .map(|i| {
            (
                order[i].creep.clone(),
                tiles::direction(order[i].at, tile_of[i]),
            )
        })
        .collect();
    steps.sort_by(|a, b| a.0.cmp(&b.0));

    Resolution { steps }
}

struct Search<'a> {
    order: &'a [Wish],
    candidates: &'a [Vec<i32>],
    tile_of: Vec<i32>,
    holder: Vec<i32>,
    changed_at: Vec<i32>,
    on_path: Vec<bool>,
    label_stamp: Vec<i32>,
    label_score: Vec<i32>,
    label_home: Vec<i32>,
    path: Vec<(usize, i32)>,
    best_chain: Vec<(usize, i32)>,
    best_score: i32,
    best_home: i32,
    taken: i32,
    stamp: i32,
}

impl<'a> Search<'a> {
    fn finish(&mut self, s: i32, h: i32) {
        if above(s, h, 0, 0) {
            self.taken += 1;
            if above(s, h, self.best_score, self.best_home)
                || (s == self.best_score
                    && h == self.best_home
                    && self.path.len() < self.best_chain.len())
            {
                self.best_score = s;
                self.best_home = h;
                self.best_chain = self.path.clone();
            }
        }
    }

    // A creep reached before is explored again only at a better (score, home):
    // what a chain makes past it depends only on the creeps already on it.
    fn explore(&mut self, i: usize, from: i32, score: i32, home: i32, onward: Option<i32>) {
        self.on_path[i] = true;
        let wish = &self.order[i];
        let leaving = wish.value(from);
        let leaving_home = if from == wish.at { 1 } else { 0 };
        let candidates = self.candidates[i].clone();

        let before = self.taken;
        let skip = onward.unwrap_or(from);

        for tile in &candidates {
            if *tile != from && *tile != skip {
                self.step(i, *tile, score, home, leaving, leaving_home, wish.onward);
            }
        }

        if let Some(o) = onward {
            if o != from && self.taken == before && candidates.contains(&o) {
                self.step(i, o, score, home, leaving, leaving_home, wish.onward);
            }
        }

        self.on_path[i] = false;
    }

    #[allow(clippy::too_many_arguments)]
    fn step(
        &mut self,
        i: usize,
        tile: i32,
        score: i32,
        home: i32,
        leaving: i32,
        leaving_home: i32,
        onward: Option<i32>,
    ) {
        let wish = &self.order[i];
        let s = score + wish.value(tile) - leaving;
        let h = home + (if tile == wish.at { 1 } else { 0 }) - leaving_home;
        let occupant = self.holder[tile as usize];
        self.path.push((i, tile));

        if occupant < 0 {
            self.finish(s, h);
        } else {
            let occupant = occupant as usize;
            if !self.on_path[occupant]
                && (self.label_stamp[occupant] != self.stamp
                    || above(s, h, self.label_score[occupant], self.label_home[occupant]))
            {
                self.label_stamp[occupant] = self.stamp;
                self.label_score[occupant] = s;
                self.label_home[occupant] = h;
                self.explore(occupant, tile, s, h, onward);
            }
        }

        self.path.pop();
    }
}

pub fn arbitrate_with(
    scratch: &mut Scratch,
    passable: impl Fn(i32) -> bool,
    held: &[i32],
    wishes: &[Wish],
) -> Vec<(String, i32)> {
    resolve(scratch, passable, held, wishes).steps
}

pub fn arbitrate(
    passable: impl Fn(i32) -> bool,
    held: &[i32],
    wishes: &[Wish],
) -> Vec<(String, i32)> {
    arbitrate_with(&mut scratch(), passable, held, wishes)
}
