//! An augmenting search over the room's whole matching, so swaps and
//! rotations fall out of it rather than being special cases.
use fabot_base::grid::Footprint;
use fabot_base::tiles::{self, Tile};
use fabot_base::wish::Wish;

fn covered(footprint: Footprint, anchor: Tile) -> Vec<Tile> {
    match footprint {
        Footprint::One => vec![anchor],
        Footprint::Quad => {
            let (x, y) = (tiles::x_of(anchor), tiles::y_of(anchor));
            [(x, y), (x + 1, y), (x, y + 1), (x + 1, y + 1)]
                .into_iter()
                .filter(|(x, y)| tiles::in_room(*x, *y))
                .map(|(x, y)| tiles::index(x, y))
                .collect()
        }
    }
}

// A stayer only ever asks back onto its work or its own tile, so movers go
// first. Names are unique, so the order is total and the input's never shows.
fn offered(a: &Wish, b: &Wish) -> std::cmp::Ordering {
    (a.stays().cmp(&b.stays()))
        .then_with(|| b.weight.max(1).cmp(&a.weight.max(1)))
        .then_with(|| a.creep.cmp(&b.creep))
}

fn above(s: i32, h: i32, s0: i32, h0: i32) -> bool {
    s > s0 || (s == s0 && h > h0)
}

/// The four tables a resolution reads, kept across the rooms of one tick: a
/// room refills them from empty anyway, so they are allocations otherwise.
#[derive(Clone, Debug)]
pub struct Scratch {
    blocked: [bool; tiles::COUNT],
    steps_onto: [Option<Tile>; tiles::COUNT],
    holder: [Option<u16>; tiles::COUNT],
    changed_at: [i32; tiles::COUNT],
}

pub fn scratch() -> Scratch {
    Scratch {
        blocked: [false; tiles::COUNT],
        steps_onto: [None; tiles::COUNT],
        holder: [None; tiles::COUNT],
        changed_at: [0; tiles::COUNT],
    }
}

/// `held`: the tiles of bodies outside the wishes (theirs, and ours with none).
pub fn arbitrate_with(
    scratch: &mut Scratch,
    passable: impl Fn(Tile) -> bool,
    held: &[Tile],
    wishes: &[&Wish],
) -> Vec<(String, i32)> {
    let blocked = &mut scratch.blocked;
    let steps_onto = &mut scratch.steps_onto;
    let holder = &mut scratch.holder;
    let changed_at = &mut scratch.changed_at;

    blocked.fill(false);
    steps_onto.fill(None);
    holder.fill(None);
    changed_at.fill(0);

    for &tile in held {
        blocked[tile] = true;
    }

    // A swap a tick or two away beats the way round: with a held body that
    // steps onto this tile, or a mover through the same head onto it.
    for wish in wishes {
        if !wish.movable() {
            for tile in covered(wish.footprint, wish.at) {
                blocked[tile] = true;
            }
            steps_onto[wish.at] = (!wish.stays()).then_some(wish.head);
        }
    }

    let mut movable: Vec<&Wish> = wishes.iter().copied().filter(|w| w.movable()).collect();
    movable.sort_by(|a, b| offered(a, b));

    let mut placed: Vec<&Wish> = Vec::new();
    for wish in movable {
        if !blocked[wish.at] && holder[wish.at].is_none() {
            holder[wish.at] = Some(placed.len() as u16);
            placed.push(wish);
        }
    }

    let order = placed;
    let n = order.len();
    let mut tile_of: Vec<Tile> = order.iter().map(|w| w.at).collect();

    let head_on = |wish: &Wish| -> bool {
        order.iter().any(|other| {
            other.head == wish.head && other.onward == Some(wish.at) && other.creep != wish.creep
        })
    };

    fn consider(
        found: &mut Vec<Tile>,
        blocked: &[bool],
        passable: &dyn Fn(Tile) -> bool,
        at: Tile,
        tile: Tile,
    ) {
        if tile != at
            && tiles::range(at, tile) == 1
            && !blocked[tile]
            && passable(tile)
            && !found.contains(&tile)
        {
            found.push(tile);
        }
    }

    let candidates: Vec<Vec<Tile>> = order
        .iter()
        .map(|wish| {
            let at = wish.at;
            let mut found: Vec<Tile> = Vec::new();
            if steps_onto[wish.head] != Some(at) {
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
            let wish = &order[i];
            (candidates[i].iter())
                .map(|tile| wish.value(*tile))
                .fold(wish.value(wish.at), i32::max)
        })
        .collect();

    let mut search = Search {
        order: &order,
        candidates: &candidates,
        tile_of: tile_of.clone(),
        holder: *holder,
        changed_at: *changed_at,
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

    let mut searched_at: Vec<Option<i32>> = vec![None; n];
    let mut clock = 0i32;
    let mut changed = true;

    while changed {
        changed = false;
        for i in 0..n {
            let from = search.tile_of[i];
            let wish = &order[i];
            let stale = searched_at[i].is_none_or(|at| {
                candidates[i]
                    .iter()
                    .any(|tile| search.changed_at[*tile] > at)
            });
            let content = wish.value(from) == reachable[i]
                && (from == wish.at || wish.value(wish.at) < wish.value(from));

            if stale {
                searched_at[i] = Some(clock);
            }

            if stale && !content {
                search.stamp += 1;
                search.best_chain.clear();
                search.best_score = 0;
                search.best_home = 0;
                search.taken = 0;
                search.holder[from] = None;
                search.explore(i, from, 0, 0, None);

                if !search.best_chain.is_empty() {
                    clock += 1;
                    let chain = search.best_chain.clone();
                    for &(m, _) in &chain {
                        let t = search.tile_of[m];
                        search.changed_at[t] = clock;
                        search.holder[t] = None;
                    }
                    for &(m, tile) in &chain {
                        search.changed_at[tile] = clock;
                        search.tile_of[m] = tile;
                        search.holder[tile] = Some(m as u16);
                    }
                    changed = true;
                } else {
                    search.holder[from] = Some(i as u16);
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

    steps
}

struct Search<'a> {
    order: &'a [&'a Wish],
    candidates: &'a [Vec<Tile>],
    tile_of: Vec<Tile>,
    holder: [Option<u16>; tiles::COUNT],
    changed_at: [i32; tiles::COUNT],
    on_path: Vec<bool>,
    label_stamp: Vec<i32>,
    label_score: Vec<i32>,
    label_home: Vec<i32>,
    path: Vec<(usize, Tile)>,
    best_chain: Vec<(usize, Tile)>,
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
    fn explore(&mut self, i: usize, from: Tile, score: i32, home: i32, onward: Option<Tile>) {
        self.on_path[i] = true;
        let wish = &self.order[i];
        let leaving = wish.value(from);
        let leaving_home = i32::from(from == wish.at);
        let candidates = self.candidates[i].clone();

        let before = self.taken;
        let skip = onward.unwrap_or(from);

        for tile in &candidates {
            if *tile != from && *tile != skip {
                self.step(i, *tile, score, home, leaving, leaving_home, wish.onward);
            }
        }

        if let Some(o) = onward
            && o != from
            && self.taken == before
            && candidates.contains(&o)
        {
            self.step(i, o, score, home, leaving, leaving_home, wish.onward);
        }

        self.on_path[i] = false;
    }

    #[allow(clippy::too_many_arguments)]
    fn step(
        &mut self,
        i: usize,
        tile: Tile,
        score: i32,
        home: i32,
        leaving: i32,
        leaving_home: i32,
        onward: Option<Tile>,
    ) {
        let wish = &self.order[i];
        let s = score + wish.value(tile) - leaving;
        let h = home + i32::from(tile == wish.at) - leaving_home;
        self.path.push((i, tile));

        if let Some(occupant) = self.holder[tile].map(usize::from) {
            if !self.on_path[occupant]
                && (self.label_stamp[occupant] != self.stamp
                    || above(s, h, self.label_score[occupant], self.label_home[occupant]))
            {
                self.label_stamp[occupant] = self.stamp;
                self.label_score[occupant] = s;
                self.label_home[occupant] = h;
                self.explore(occupant, tile, s, h, onward);
            }
        } else {
            self.finish(s, h);
        }

        self.path.pop();
    }
}

pub fn arbitrate(
    passable: impl Fn(Tile) -> bool,
    held: &[Tile],
    wishes: &[Wish],
) -> Vec<(String, i32)> {
    let wishes: Vec<&Wish> = wishes.iter().collect();
    arbitrate_with(&mut scratch(), passable, held, &wishes)
}
