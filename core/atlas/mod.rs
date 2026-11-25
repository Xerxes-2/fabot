//! The floods, trees and walks are kept across ticks (`ARCHITECTURE.md` §1).
//! Built twice from one `World`, an Atlas answers identically: a kept value is
//! one on equal reads.
use std::cell::{Cell, RefCell};
use std::collections::BTreeSet;

use fabot_base::HashMap;
use std::rc::Rc;

mod derived;
mod graph;
mod loot;

pub use self::derived::{
    BANK_LEVEL, Context, Derived, Home, derive, nearest, plan_of, server, server_of, sponsor,
    target_at, worked,
};
pub use self::graph::Tree;
use self::graph::Way;
pub use self::loot::{
    cast_by_core, fallen, fallen_with_loot, garrison, invader_rampart, invader_rampart_at, loot,
    loot_in, loot_wall, walls_of,
};

use fabot_base::engine;
use fabot_base::grid::{
    self, FatigueFactor, Footprint, UNREACHED, Weight, flood, on_edge, step_table, step_ticks,
    weight_of,
};
use fabot_base::tiles::{self, Post, Table, Tile, TileSet};
use fabot_base::world::{Built, Id, Owner, Pos, Room, RoomName, StructureType, World};

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Mover {
    pub footprint: Footprint,
    pub factor: FatigueFactor,
}

/// `tiles` are tile indices; `avoid` never holds a walk back from its ends.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Target {
    pub room: RoomName,
    pub tiles: Vec<Tile>,
    pub range: i32,
    pub avoid: BTreeSet<RoomName>,
    pub max_hops: i32,
}

pub fn middle_in(reached: &TileSet, room: RoomName) -> Pos {
    (tiles::all().filter(|t| reached.contains(*t)))
        .min_by_key(|t| (tiles::range(tiles::of_pos(&tiles::middle(room)), *t), *t))
        .map_or_else(|| tiles::middle(room), |t| tiles::to_pos(room, t))
}

/// A room out of sight is what stood there when last seen, else its
/// terrain, and the strikes intel places.
#[derive(Clone, Debug)]
pub struct RoomGrid {
    /// No creep: floods and the room's read key on these, so a hostile's
    /// step redoes no flood, tree or walk.
    pub weights: Rc<Table<Weight>>,
    /// Our creeps are no wall (who yields is `Move`'s call); a creep not ours
    /// yields to nothing we decide, so a step never lands on its tile.
    held: Option<Rc<Table<Weight>>>,
    floods: Floods,
    /// Floods round `held` or danger.
    live: Floods,
    /// The open tiles a keeper strikes, walled.
    pub struck: Option<Rc<TileSet>>,
    /// The weights without creeps or strikes: what stands once out of sight.
    standing: Rc<Table<Weight>>,
    /// Terrain alone: a walk on it reads no structure seen, so a reset moves
    /// it only through the room tree it crosses.
    bare: [Rc<Table<Weight>>; 2],
    seats: Vec<(Tile, Weight)>,
    /// Out of sight, the keepers' posts the strikes were placed from.
    placed: Option<Vec<Post>>,
    seen: bool,
    /// A keeper room out of sight whose lairs no intel places: crossed last.
    blind: bool,
    /// Intel last saw towers here that fire on us: a live stronghold's or a non-ally's.
    towered: bool,
}

/// What a flood is a function of besides the room's weights: the mover's
/// footprint and pace, its goals, and the tiles it walks round.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct FloodOf<'a> {
    footprint: Footprint,
    steps: [Option<i32>; 3],
    goal: Goal<'a>,
    danger: Option<&'a TileSet>,
    held: bool,
}

/// What a flood walls besides the room's creep-free weights. Only `Kept`
/// floods outlive the tick.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Ground<'a> {
    Kept,
    /// A carried walk's round: the walk's key holds it, so its flood is
    /// redone only when the walk is.
    Round(&'a TileSet),
    /// This tick's step: hostiles and danger.
    Live(Option<&'a TileSet>),
}

impl<'a> Ground<'a> {
    fn walls(self) -> Option<&'a TileSet> {
        match self {
            Ground::Kept => None,
            Ground::Round(walls) => Some(walls),
            Ground::Live(danger) => danger,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Goal<'a> {
    /// Sorted, each tile once.
    Seats {
        range: i32,
        tiles: &'a [Tile],
    },
    Exit(&'a [(Tile, Tile)]),
    /// Off the border band, since a body left on it is carried across.
    Clear,
}

/// A kept flood's `FloodOf`, owning its goal; a lookup borrows it, so a hit
/// allocates nothing.
#[derive(Clone, Debug)]
pub struct FloodKey {
    footprint: Footprint,
    steps: [Option<i32>; 3],
    goal: KeptGoal,
    danger: Option<TileSet>,
    held: bool,
}

#[derive(Clone, Debug)]
enum KeptGoal {
    Seats { range: i32, tiles: Vec<Tile> },
    Exit(Vec<(Tile, Tile)>),
    Clear,
}

impl FloodOf<'_> {
    fn of<'a>(
        mover: Mover,
        goal: Goal<'a>,
        danger: Option<&'a TileSet>,
        held: bool,
    ) -> FloodOf<'a> {
        let grounds = [
            engine::ROAD_WEIGHT,
            engine::PLAIN_WEIGHT,
            engine::SWAMP_WEIGHT,
        ];
        FloodOf {
            footprint: mover.footprint,
            steps: grounds.map(|w| step_ticks(mover.factor, w)),
            goal,
            danger,
            held,
        }
    }

    fn kept(&self) -> FloodKey {
        let goal = match self.goal {
            Goal::Seats { range, tiles } => KeptGoal::Seats {
                range,
                tiles: tiles.to_vec(),
            },
            Goal::Exit(exit) => KeptGoal::Exit(exit.to_vec()),
            Goal::Clear => KeptGoal::Clear,
        };
        FloodKey {
            footprint: self.footprint,
            steps: self.steps,
            goal,
            danger: self.danger.cloned(),
            held: self.held,
        }
    }
}

trait Keyed {
    fn key(&self) -> FloodOf<'_>;
}

impl Keyed for FloodOf<'_> {
    fn key(&self) -> FloodOf<'_> {
        *self
    }
}

impl Keyed for FloodKey {
    fn key(&self) -> FloodOf<'_> {
        let goal = match &self.goal {
            KeptGoal::Seats { range, tiles } => Goal::Seats {
                range: *range,
                tiles,
            },
            KeptGoal::Exit(exit) => Goal::Exit(exit),
            KeptGoal::Clear => Goal::Clear,
        };
        FloodOf {
            footprint: self.footprint,
            steps: self.steps,
            goal,
            danger: self.danger.as_ref(),
            held: self.held,
        }
    }
}

impl<'a> std::borrow::Borrow<dyn Keyed + 'a> for FloodKey {
    fn borrow(&self) -> &(dyn Keyed + 'a) {
        self
    }
}

impl PartialEq for dyn Keyed + '_ {
    fn eq(&self, other: &Self) -> bool {
        self.key() == other.key()
    }
}

impl Eq for dyn Keyed + '_ {}

impl std::hash::Hash for dyn Keyed + '_ {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.key().hash(state);
    }
}

impl PartialEq for FloodKey {
    fn eq(&self, other: &Self) -> bool {
        self.key() == other.key()
    }
}

impl Eq for FloodKey {}

impl std::hash::Hash for FloodKey {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.key().hash(state);
    }
}

impl RoomGrid {
    fn held(&self) -> &Rc<Table<Weight>> {
        self.held.as_ref().unwrap_or(&self.weights)
    }

    fn ground(&self, key: FloodOf) -> &Rc<Table<Weight>> {
        if key.held { self.held() } else { &self.weights }
    }

    /// The flood toward `goal`, whether it was kept, whether live, and the tiles it relaxed.
    fn flood(
        &self,
        mover: Mover,
        goal: Goal,
        ground: Ground,
        check: bool,
    ) -> (Rc<Table<i32>>, bool, bool, i32) {
        let held = matches!(ground, Ground::Live(_)) && self.held.is_some();
        let key = FloodOf::of(mover, goal, ground.walls(), held);
        let live = key.danger.is_some() || held;
        let floods = if live { &self.live } else { &self.floods };
        let now = self.ground(key);
        let kept = (floods.borrow().get(&key as &dyn Keyed)).map(|k| {
            k.unasked.set(0);
            (k.read.clone(), k.value.clone())
        });
        let fresh = |dist: &Table<i32>| !check || *dist == self.fresh(mover, key).0;
        if let Some((_, dist)) = kept.as_ref().filter(|(read, _)| Rc::ptr_eq(read, now)) {
            assert!(fresh(dist), "a kept flood differs from a fresh one");
            return (dist.clone(), true, live, 0);
        }
        let kept = kept.as_ref().map(|(read, dist)| (&**read, &**dist));
        let (dist, relaxed, carried) = self.flooded(mover, key, kept);
        assert!(
            !carried || fresh(&dist),
            "a repaired flood differs from a fresh one"
        );
        let dist = Rc::new(dist);
        let kept = Kept {
            read: now.clone(),
            value: dist.clone(),
            unasked: Cell::new(0),
        };
        floods.borrow_mut().insert(key.kept(), kept);
        (dist, carried, live, relaxed)
    }

    fn fresh(&self, mover: Mover, key: FloodOf) -> (Table<i32>, i32) {
        let (dist, relaxed, _) = self.flooded(mover, key, None);
        (dist, relaxed)
    }

    /// Fresh, or `kept` (a flood and the weights it read) carried onto the
    /// weights now while no tile rose or closed: a road built or a structure
    /// gone only lowers them, so only the tiles that changed relax again.
    fn flooded(
        &self,
        mover: Mover,
        key: FloodOf,
        kept: Option<(&Table<Weight>, &Table<i32>)>,
    ) -> (Table<i32>, i32, bool) {
        let footprint = mover.footprint;
        let now = RoomGrid::walled(key, self.ground(key));
        let carried = kept.and_then(|(read, kept)| {
            let was = RoomGrid::walled(key, read);
            let mut seeds = Vec::new();
            for t in tiles::all() {
                match (weight_of(footprint, &was, t), weight_of(footprint, &now, t)) {
                    (a, b) if a == b => {}
                    (Some(a), Some(b)) if b < a => seeds.push((t, kept[t])),
                    (None, Some(_)) => seeds.extend(tiles::around(t).iter().map(|&n| (n, kept[n]))),
                    _ => return None,
                }
            }
            seeds.retain(|&(t, d)| d != UNREACHED && !on_edge(footprint, t));
            Some((seeds, kept.clone()))
        });
        let repaired = carried.is_some();
        let (mut seeds, dist) = carried.unwrap_or_else(|| (Vec::new(), tiles::table(UNREACHED)));
        let goals = RoomGrid::goals(footprint, key, &now).into_iter();
        seeds.extend(goals.map(|g| (g, 0)));
        seeds.sort_by_key(|&(_, d)| d);
        let steps = step_table(mover.factor);
        let (dist, relaxed) = flood(footprint, &now, &steps, dist, seeds.into_iter());
        (dist, relaxed, repaired)
    }

    fn walled<'w>(key: FloodOf, ground: &'w Table<Weight>) -> std::borrow::Cow<'w, Table<Weight>> {
        match key.danger.filter(|_| key.goal != Goal::Clear) {
            Some(danger) => {
                let mut weights = ground.clone();
                for t in tiles::all().filter(|t| danger.contains(*t)) {
                    weights[t] = None;
                }
                std::borrow::Cow::Owned(weights)
            }
            None => std::borrow::Cow::Borrowed(ground),
        }
    }

    fn goals(footprint: Footprint, key: FloodOf, weights: &Table<Weight>) -> Vec<Tile> {
        match key.goal {
            Goal::Seats {
                range,
                tiles: targets,
            } => {
                let mut goals = Vec::new();
                for &tile in targets {
                    if tile.index() < tiles::COUNT {
                        Atlas::seats_near(footprint, weights, range, tile, &mut goals);
                    }
                }
                goals
            }
            Goal::Exit(exit) => (exit.iter())
                .flat_map(|(a, b)| Atlas::exit_goals(footprint, weights, Atlas::stretch(*a, *b)))
                .collect(),
            Goal::Clear => tiles::all()
                .filter(|t| {
                    weight_of(footprint, weights, *t).is_some()
                        && !on_edge(footprint, *t)
                        && !key.danger.is_some_and(|d| d.contains(*t))
                })
                .collect(),
        }
    }
}

type Coords = (i32, i32);

#[derive(Clone, Debug)]
pub struct Field {
    pub footprint: Footprint,
    pub weights: Rc<Table<Weight>>,
    pub factor: FatigueFactor,
    pub dist: Rc<Table<i32>>,
    pub struck: Option<Rc<TileSet>>,
    /// The ticks on from where the room is left to the target.
    pub onward: i32,
}

#[derive(Clone, Debug, Default)]
pub struct Atlas {
    rooms: HashMap<Coords, RoomGrid>,
    /// The rooms intel saw whose lairs the name formula misreads.
    pub lairs: HashMap<Coords, bool>,
    closed: Vec<Coords>,
    misses: Cell<i32>,
    hits: Cell<i32>,
    /// Floods run round what moves (`RoomGrid::live`): never kept, so no miss.
    live: Cell<i32>,
    cold: bool,
    relaxed: Cell<i32>,
    trees: Carried<(RoomName, BTreeSet<RoomName>, i32), Tree>,
    walks: Carried<WalkKey, Vec<Pos>>,
    towards: Carried<Toward, Walked>,
    reads: RefCell<HashMap<(Coords, i32), Reads>>,
    last_reads: HashMap<(Coords, i32), Reads>,
    recomputed: Cell<i32>,
    /// Each carried value is computed afresh too and must equal it.
    pub check: bool,
}

type Carried<K, T, R = Reads> = RefCell<HashMap<K, Kept<T, R>>>;
/// A room's floods read its weights alone, which the room's grid keys.
type Floods = Carried<FloodKey, Rc<Table<i32>>, Rc<Table<Weight>>>;

/// A walk toward `to` from `room`, by its region `way`'s way on, else by any.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
struct Toward {
    to: Pos,
    max_hops: i32,
    room: Coords,
    way: Option<usize>,
    bare: Option<bool>,
}

/// A room's flood off its weights (by address) and the ticks its exits are
/// seeded with: kept while those read the same.
#[derive(Clone, Debug)]
struct Walked {
    weights: Option<Rc<Table<Weight>>>,
    seeds: Vec<(Tile, i32)>,
    dist: Option<Rc<Table<i32>>>,
}

impl Walked {
    fn reads_as(&self, other: &Walked) -> bool {
        let at = |w: &Walked| w.weights.as_ref().map(Rc::as_ptr);
        at(self) == at(other) && self.seeds == other.seeds
    }

    fn flooded(self) -> Walked {
        let plain = || Atlas::ground(&[0; tiles::COUNT]);
        let weights = self.weights.as_deref().map_or_else(plain, Clone::clone);
        let steps = step_table(Atlas::PACE.factor);
        let seeds = self.seeds.iter().copied();
        let unreached = tiles::table(UNREACHED);
        let dist = (!self.seeds.is_empty())
            .then(|| Rc::new(flood(Footprint::One, &weights, &steps, unreached, seeds).0));
        Walked { dist, ..self }
    }
}

type Reads = Rc<[RoomRead]>;
type WalkKey = (Mover, Target, Option<TileSet>, Pos, usize);

/// Weights by address: an unchanged room keeps its `Rc`.
#[derive(Clone, Debug)]
struct RoomRead(Option<Rc<Table<Weight>>>, Option<Rc<TileSet>>, [bool; 4]);

impl PartialEq for RoomRead {
    fn eq(&self, other: &RoomRead) -> bool {
        let at = |r: &RoomRead| r.0.as_ref().map(Rc::as_ptr);
        at(self) == at(other) && self.1 == other.1 && self.2 == other.2
    }
}

#[derive(Clone, Debug)]
struct Kept<T, R = Reads> {
    read: R,
    value: T,
    unasked: Cell<u32>,
}

impl Field {
    fn via(&self, next: Tile) -> i32 {
        let Some(weight) = grid::value(weight_of(self.footprint, &self.weights, next)) else {
            return UNREACHED;
        };
        let d = self.dist[next];
        if d == UNREACHED || (d > 0 && on_edge(self.footprint, next)) {
            UNREACHED
        } else {
            step_ticks(self.factor, weight).map_or(UNREACHED, |step| d + step)
        }
    }

    fn best(&self, tile: Tile) -> Option<Tile> {
        tiles::least_around(tile, UNREACHED, |n| self.via(n))
    }

    fn is_struck(&self, tile: Tile) -> bool {
        self.struck.as_ref().is_some_and(|s| s.contains(tile))
    }

    /// None on a struck tile, so no walk but its own way out takes one.
    pub fn ticks(&self, tile: Tile) -> Option<i32> {
        if self.is_struck(tile) {
            return None;
        }
        let d = if weight_of(self.footprint, &self.weights, tile).is_some() {
            self.dist[tile]
        } else {
            self.best(tile).map_or(UNREACHED, |next| self.via(next))
        };
        if d == UNREACHED { None } else { Some(d) }
    }

    /// The ticks onward from a neighbour of `next`, through it.
    pub fn onto(&self, next: Tile) -> Option<i32> {
        Some(self.via(next)).filter(|d| *d != UNREACHED)
    }

    /// Out of a strike a body stands in, the shortest way across struck
    /// tiles, never deeper, onto the open tile nearest the goal.
    fn escape(&self, from: Tile) -> Option<i32> {
        let struck = self
            .struck
            .as_ref()
            .filter(|_| self.footprint == Footprint::One)?;
        let mut seen = TileSet::default();
        seen.insert(from);
        let mut met = vec![(from, 0)];
        let mut frontier = vec![0];
        while !frontier.is_empty() {
            let mut out: Option<(i32, Tile, usize)> = None;
            let mut next = Vec::new();
            for &i in &frontier {
                for &n in tiles::around(met[i].0) {
                    let d = self.dist[n];
                    let open = self.weights[n].is_some() && d != UNREACHED;
                    if !seen.insert(n) || !(open || struck.contains(n)) {
                        continue;
                    }
                    if !open {
                        met.push((n, i));
                        next.push(met.len() - 1);
                    } else if out.is_none_or(|(od, ot, _)| (d, n) < (od, ot)) {
                        out = Some((d, n, i));
                    }
                }
            }
            if let Some((_, mut step, mut i)) = out {
                while i != 0 {
                    step = met[i].0;
                    i = met[i].1;
                }
                return Some(tiles::direction(from, step));
            }
            frontier = next;
        }
        None
    }

    pub fn step(&self, tile: Tile) -> Option<i32> {
        if self.is_struck(tile) {
            return self.escape(tile);
        }
        match self.ticks(tile) {
            Some(d) if d > 0 => self.best(tile).map(|next| tiles::direction(tile, next)),
            _ => None,
        }
    }
}

impl Atlas {
    pub fn is_buffer(room: &Room, pos: &Pos) -> bool {
        room.controller
            .as_ref()
            .is_some_and(|c| c.pos.within(engine::UPGRADE_RANGE, pos))
            && !room.sources.iter().any(|s| s.pos.within(1, pos))
            && !room.minerals.iter().any(|m| m.pos.within(1, pos))
    }

    /// Within 2: beside the source's container.
    pub fn is_source_link(room: &Room, pos: &Pos) -> bool {
        room.sources.iter().any(|s| s.pos.within(2, pos))
    }

    pub fn controller_link<'a>(room: &'a Room) -> Option<&'a fabot_base::world::Structure> {
        let c = room.controller.as_ref()?;
        (room.structures.iter())
            .filter(|s| {
                s.kind == StructureType::Link
                    && s.owner.is_ours()
                    && c.pos.within(engine::UPGRADE_RANGE, &s.pos)
                    && !Atlas::is_source_link(room, &s.pos)
            })
            .min_by_key(|s| (c.pos.range(&s.pos), tiles::of_pos(&s.pos)))
    }

    pub fn buffer<'a>(room: &'a Room) -> Option<&'a fabot_base::world::Structure> {
        Atlas::controller_link(room).or_else(|| {
            room.structures
                .iter()
                .find(|s| s.kind == StructureType::Container && Atlas::is_buffer(room, &s.pos))
        })
    }

    /// No site is built under a creep, so these are kept clear of solid ones.
    pub fn upgrader_seats(
        walkable: impl Fn(Tile) -> bool,
        controller: Tile,
        buffer: Tile,
    ) -> Vec<Tile> {
        std::iter::once(buffer)
            .chain((tiles::within(1, buffer).into_iter()).filter(|t| *t != buffer))
            .filter(|t| walkable(*t) && tiles::range(*t, controller) <= engine::UPGRADE_RANGE)
            .collect()
    }

    /// Farthest from the controller first, so a controller container on a seat
    /// is no post; a rock's own before one beside another, so an ore's digger
    /// never seats on a source's post.
    pub fn post_of(room: &Room, source: &Pos) -> Option<(Pos, Option<Id>)> {
        let from_controller = |p: &Pos| Atlas::from_controller(room, p);
        let rocks =
            (room.sources.iter().map(|s| &s.pos)).chain(room.minerals.iter().map(|m| &m.pos));
        let shared = |p: &Pos| rocks.clone().any(|r| r != source && p.within(1, r));
        let built = (room.structures.iter())
            .filter(|s| s.kind == StructureType::Container)
            .map(|s| (s.pos, Some(s.id)));
        let sites = (room.sites.iter())
            .filter(|s| s.kind == StructureType::Container && s.owner.is_ours())
            .map(|s| (s.pos, None));
        (built.chain(sites))
            .filter(|(p, _)| p.within(1, source) && p != source)
            .min_by_key(|(p, built)| (shared(p), -from_controller(p), built.is_none(), p.y, p.x))
    }

    /// A seat that parts its neighbours ranks last: a harvester there holds the room.
    pub fn bare_post(&self, room: &Room, seats: Vec<Pos>) -> Option<Pos> {
        let weights = self.grid(room.name).map(|g| &g.weights);
        let open = |t: Tile| weights.is_some_and(|w| w[t].is_some());
        let key = |p: &Pos| {
            let at = tiles::of_pos(p);
            (parts(at, open), -Atlas::from_controller(room, p), at)
        };
        seats.into_iter().min_by_key(key)
    }

    fn from_controller(room: &Room, p: &Pos) -> i32 {
        (room.controller.as_ref())
            .and_then(|c| c.pos.range(p))
            .unwrap_or(0)
    }

    pub fn source_posts(room: &Room) -> Vec<Pos> {
        (room.sources.iter())
            .filter_map(|s| Atlas::post_of(room, &s.pos).map(|(p, _)| p))
            .collect()
    }

    /// A harvest bay's extension: its harvester keeps it, not the carriers.
    pub fn in_bay(room: &Room) -> impl Fn(StructureType, &Pos) -> bool {
        let posts = Atlas::source_posts(room);
        move |kind, pos| kind == StructureType::Extension && posts.iter().any(|p| p.within(1, pos))
    }

    /// A link buffer's upgrader tiles stay open: held, they priced every walk
    /// past the controller dearer.
    pub fn work_seats(room: &Room) -> Vec<Tile> {
        let ore = (room.minerals.iter()).filter_map(|m| Some(Atlas::post_of(room, &m.pos)?.0));
        let posts = Atlas::source_posts(room).into_iter().chain(ore);
        let mut seats: Vec<Tile> = posts.map(|p| tiles::of_pos(&p)).collect();
        if let Some(b) = Atlas::buffer(room)
            && b.kind == StructureType::Container
        {
            seats.push(tiles::of_pos(&b.pos));
        }
        seats
    }

    /// The resource a keeper standing at `from` guards: a source before a
    /// mineral, the nearest within its guard.
    fn guarded(room: &Room, from: Tile) -> Option<Tile> {
        let near = |p: &Pos| tiles::range(from, tiles::of_pos(p));
        let sources = room.sources.iter().map(|s| (false, &s.pos));
        let minerals = room.minerals.iter().map(|m| (true, &m.pos));
        let guarded = sources.chain(minerals);
        let near_enough = guarded.filter(|(_, p)| near(p) <= engine::KEEPER_GUARD_RANGE);
        let (_, resource) = near_enough.min_by_key(|(mineral, p)| (*mineral, near(p)))?;
        Some(tiles::of_pos(resource))
    }

    /// Each lair and the resource its keeper guards, for intel to place
    /// the keepers of a room out of sight.
    pub fn posts(room: &Room) -> Vec<Post> {
        let lairs = room
            .structures
            .iter()
            .filter(|s| s.kind == StructureType::KeeperLair);
        let post = |t: Tile| (t, Atlas::guarded(room, t).unwrap_or(t));
        lairs.map(|l| post(tiles::of_pos(&l.pos))).collect()
    }

    /// A keeper's walk from `from` to its post: the tile beside `resource`
    /// that a walk reaches first. One standing nowhere near a resource (its
    /// own tile) stays.
    fn walk_of(terrain: &[u8], from: Tile, resource: Tile) -> Vec<Tile> {
        if resource == from {
            return vec![from];
        }
        let open = |t: Tile| terrain[t.index()] & fabot_base::world::terrain::WALL == 0;
        let mut seen = TileSet::default();
        seen.insert(from);
        let mut met = vec![(from, 0)];
        let mut at = 0;
        while let Some(&(t, _)) = met.get(at) {
            if tiles::range(t, resource) == 1 {
                let mut walk = vec![t];
                while at != 0 {
                    at = met[at].1;
                    walk.push(met[at].0);
                }
                return walk;
            }
            for &n in tiles::around(t) {
                if open(n)
                    && tiles::range(n, from) <= 2 * engine::KEEPER_GUARD_RANGE
                    && seen.insert(n)
                {
                    met.push((n, at));
                }
            }
            at += 1;
        }
        vec![from]
    }

    fn strikes(terrain: &[u8], strikers: &[(Tile, Tile, i32)]) -> Vec<Tile> {
        let walks = strikers.iter().flat_map(|&(from, resource, reach)| {
            let walk = Atlas::walk_of(terrain, from, resource);
            walk.into_iter().flat_map(move |w| tiles::within(reach, w))
        });
        walks.collect()
    }

    fn keeper_reach() -> i32 {
        let body = engine::keeper_body();
        engine::strike_reach(|p| body.contains(&p))
    }

    /// The tiles the keepers strike from anywhere on the walk to their posts:
    /// a living one's, and a dead one's successor's from its lair once it
    /// spawns before a crossing ends.
    fn struck(room: &Room) -> Vec<Tile> {
        let living = room.others.iter().filter(|c| c.owner == Owner::Keeper);
        let striker =
            |from: Tile, reach: i32| (from, Atlas::guarded(room, from).unwrap_or(from), reach);
        let mut strikers: Vec<(Tile, Tile, i32)> = living
            .map(|c| {
                striker(
                    tiles::of_pos(&c.pos),
                    engine::strike_reach(|p| c.live(p) > 0),
                )
            })
            .collect();
        for lair in room
            .structures
            .iter()
            .filter(|s| s.kind == StructureType::KeeperLair)
        {
            if lair.spawn.is_some_and(|t| t <= Atlas::CROSSING_TICKS) {
                strikers.push(striker(tiles::of_pos(&lair.pos), Atlas::keeper_reach()));
            }
        }
        match room.terrain {
            Some(terrain) => Atlas::strikes(terrain.get_bits(), &strikers),
            None => Vec::new(),
        }
    }

    fn reused(previous: Option<&Rc<Table<Weight>>>, weights: Table<Weight>) -> Rc<Table<Weight>> {
        match previous {
            Some(last) if **last == weights => last.clone(),
            _ => Rc::new(weights),
        }
    }

    /// A flood round hostiles or danger is kept while they stand still: a tick
    /// unasked, they have moved.
    fn kept(previous: Option<RoomGrid>) -> (Floods, Floods) {
        previous.map_or_else(Default::default, |grid| {
            (
                Atlas::asked_floods(grid.floods),
                Atlas::unasked(grid.live, 1),
            )
        })
    }

    fn bare(last: Option<&RoomGrid>, terrain: &[u8]) -> [Rc<Table<Weight>>; 2] {
        last.map_or_else(
            || {
                let paved: Vec<u8> = (terrain.iter())
                    .map(|t| t & fabot_base::world::terrain::WALL)
                    .collect();
                [Atlas::ground(terrain), Atlas::ground(&paved)].map(Rc::new)
            },
            |g| g.bare.clone(),
        )
    }

    fn ground(terrain: &[u8]) -> Table<Weight> {
        let mut weights: Table<Weight> = tiles::table(None);
        for (weight, t) in weights.iter_mut().zip(terrain) {
            *weight = if t & fabot_base::world::terrain::WALL != 0 {
                None
            } else if t & fabot_base::world::terrain::SWAMP != 0 {
                grid::weight(engine::SWAMP_WEIGHT)
            } else {
                grid::weight(engine::PLAIN_WEIGHT)
            };
        }
        weights
    }

    fn walled(weights: &mut Table<Weight>, strikes: Vec<Tile>) -> Option<Rc<TileSet>> {
        let mut struck = TileSet::default();
        let mut any = false;
        for t in strikes {
            if weights[t].take().is_some() {
                any |= struck.insert(t);
            }
        }
        any.then(|| Rc::new(struck))
    }

    /// A room whose terrain buffer is not a whole room has no grid: routed by
    /// name, never walked. `seen` is intel's: each room, its keepers' posts
    /// (`None` where unread) and its towers; `terrains` holds rooms out of sight too.
    pub fn following<'a>(
        mut previous: Atlas,
        world: &World,
        seen: impl IntoIterator<Item = (RoomName, Option<&'a [Post]>, bool)>,
        terrains: impl IntoIterator<Item = (RoomName, &'a [u8; tiles::COUNT])>,
    ) -> Atlas {
        let cold = previous.rooms.is_empty();
        let mut rooms: HashMap<Coords, RoomGrid> = HashMap::default();

        for (c, room, terrain) in
            (world.rooms.iter()).filter_map(|r| Some((r.name.coords(), r, r.terrain?)))
        {
            let mut weights = Atlas::ground(terrain.get_bits());

            for s in &room.structures {
                if s.kind == StructureType::Road {
                    weights[tiles::of_pos(&s.pos)] = grid::weight(engine::ROAD_WEIGHT);
                }
            }

            let seats: Vec<(Tile, Weight)> = (Atlas::work_seats(room).into_iter())
                .map(|t| (t, weights[t]))
                .collect();
            for (tile, _) in &seats {
                weights[*tile] = grid::weight(grid::SEAT_WEIGHT);
            }

            let structures = (room.structures.iter()).filter(|s| !s.walkable);
            let sites = (room.sites.iter()).filter(|s| s.owner.is_ours() && !s.kind.walkable());
            let solid = (structures.map(|s| &s.pos))
                .chain(&room.blocked)
                .chain(sites.map(|s| &s.pos))
                .chain(room.sources.iter().map(|s| &s.pos))
                .chain(room.minerals.iter().map(|m| &m.pos))
                .chain(room.controller.iter().map(|c| &c.pos));
            for pos in solid {
                weights[tiles::of_pos(pos)] = None;
            }

            let standing = Rc::new(weights.clone());
            let struck = Atlas::walled(&mut weights, Atlas::struck(room));
            let last = previous.rooms.remove(&c);
            let bare = Atlas::bare(last.as_ref(), terrain.get_bits());
            let weights = Atlas::reused(last.as_ref().map(|g| &g.weights), weights);
            let hostiles = room.hostiles().map(|c| tiles::of_pos(&c.pos));
            let open: Vec<Tile> = hostiles.filter(|t| weights[*t].is_some()).collect();
            let held = (!open.is_empty()).then(|| {
                let mut held = (*weights).clone();
                open.into_iter().for_each(|t| held[t] = None);
                Atlas::reused(last.as_ref().and_then(|g| g.held.as_ref()), held)
            });
            let (floods, live) = Atlas::kept(last);
            let grid = RoomGrid {
                floods,
                weights,
                held,
                live,
                struck,
                standing,
                bare,
                seats,
                placed: None,
                seen: true,
                blind: false,
                towered: false,
            };
            rooms.insert(c, grid);
        }

        let mut towered: Vec<Coords> = Vec::new();
        let posts: HashMap<RoomName, Option<&[Post]>> = (seen.into_iter())
            .map(|(room, posts, towers)| {
                if towers {
                    towered.push(room.coords());
                }
                (room, posts)
            })
            .collect();
        let lairs: HashMap<Coords, bool> = (posts.iter())
            .filter_map(|(name, posts)| Some((name.coords(), !(*posts)?.is_empty())))
            .filter(|(c, lairs)| *lairs != tiles::is_keeper_room(*c))
            .collect();
        let keeper = |c: Coords| lairs.get(&c).copied().unwrap_or(tiles::is_keeper_room(c));
        for (name, terrain) in terrains {
            let c = name.coords();
            if rooms.contains_key(&c) {
                continue;
            }
            let placed = posts.get(&name).copied().flatten().filter(|_| keeper(c));
            let blind = keeper(c) && placed.is_none();
            let unseen = |standing: &Rc<Table<Weight>>| {
                let mut weights = (**standing).clone();
                let keeper_of = |&(lair, resource): &Post| (lair, resource, Atlas::keeper_reach());
                let strikers: Vec<_> = placed.unwrap_or(&[]).iter().map(keeper_of).collect();
                let struck = Atlas::walled(&mut weights, Atlas::strikes(terrain, &strikers));
                (weights, struck)
            };
            let grid = match previous.rooms.remove(&c) {
                Some(mut grid)
                    if !grid.seen && grid.blind == blind && grid.placed.as_deref() == placed =>
                {
                    if previous.check {
                        let (weights, struck) = unseen(&grid.standing);
                        let fresh = (weights == *grid.weights) && struck == grid.struck;
                        assert!(fresh, "a kept unseen grid differs from a fresh one");
                    }
                    grid.floods = Atlas::asked_floods(grid.floods);
                    grid.live = Atlas::unasked(grid.live, 1);
                    grid
                }
                last => {
                    let standing = match &last {
                        Some(grid) => grid.standing.clone(),
                        None => Rc::new(Atlas::ground(terrain)),
                    };
                    let (weights, struck) = unseen(&standing);
                    let weights = Atlas::reused(last.as_ref().map(|g| &g.weights), weights);
                    let bare = Atlas::bare(last.as_ref(), terrain);
                    let (floods, live) = Atlas::kept(last);
                    RoomGrid {
                        floods,
                        held: None,
                        live,
                        weights,
                        standing,
                        bare,
                        seats: Vec::new(),
                        placed: placed.map(<[Post]>::to_vec),
                        struck,
                        seen: false,
                        blind,
                        towered: false,
                    }
                }
            };
            rooms.insert(c, grid);
        }

        for (c, grid) in &mut rooms {
            grid.towered = towered.contains(c);
        }
        Atlas {
            rooms,
            lairs,
            closed: world.closed.iter().map(|r| r.coords()).collect(),
            trees: Atlas::unasked(previous.trees, 1),
            walks: Atlas::unasked(previous.walks, 1),
            towards: Atlas::unasked(previous.towards, Atlas::TRIP_TICKS),
            last_reads: previous.reads.into_inner(),
            cold,
            check: previous.check,
            ..Atlas::default()
        }
    }

    pub fn of_world(world: &World) -> Atlas {
        Atlas::following(Atlas::default(), world, [], [])
    }

    pub fn misses(&self) -> i32 {
        self.misses.get()
    }

    pub fn hits(&self) -> i32 {
        self.hits.get()
    }

    pub fn live(&self) -> i32 {
        self.live.get()
    }

    pub fn cold(&self) -> bool {
        self.cold
    }

    pub fn relaxed(&self) -> i32 {
        self.relaxed.get()
    }

    pub fn recomputed(&self) -> i32 {
        self.recomputed.get()
    }

    /// Tiles are asked of rooms in sight: out of sight a grid is its
    /// terrain alone, the room graph's.
    fn grid(&self, room: RoomName) -> Option<&RoomGrid> {
        self.rooms.get(&room.coords()).filter(|g| g.seen)
    }

    pub fn held_weights(&self, room: RoomName) -> Option<Table<Weight>> {
        self.grid(room).map(|grid| (**grid.held()).clone())
    }

    /// The tiles a walk from the room's exits reaches without breaking a
    /// barrier, out of sight by what last stood there.
    pub fn from_exits(&self, room: RoomName) -> TileSet {
        let Some(grid) = self.rooms.get(&room.coords()) else {
            return TileSet::default();
        };
        let open = |t: Tile| grid.weights[t].is_some();
        let exits = (Atlas::EXITS.iter()).flat_map(|exit| tiles::exit_tiles(*exit).iter().copied());
        let steps = tiles::reach(exits.filter(|t| open(*t)), open);
        tiles::all()
            .filter(|t| steps[*t] != tiles::UNREACHED)
            .collect()
    }

    /// With no tile reached (no terrain, or a sealed room), the geometric middle, which may be wall.
    pub fn reached_middle(&self, room: RoomName) -> Pos {
        middle_in(&self.from_exits(room), room)
    }

    /// Terrain and road alone, as v1's arrivalGroundIn: no seat's or keeper's weight.
    pub fn ground_at(&self, room: RoomName, tile: Tile) -> Option<i32> {
        let grid = self.grid(room)?;
        let seat = (grid.seats.iter()).find_map(|(t, w)| (*t == tile).then_some(*w));
        grid::value(seat.unwrap_or(grid.standing[tile]))
    }

    /// Nothing solid beside, no road, edge or seat near: a body idle there blocks no walk or work.
    pub fn idle(&self, room: RoomName, at: Tile, range: i32) -> Option<Tile> {
        let grid = self.grid(room)?;
        let idle = |tile: &Tile| {
            let ground = grid::value(grid.standing[*tile]);
            !tiles::on_edge(*tile)
                && ground.is_some_and(|w| w != engine::ROAD_WEIGHT)
                && (tiles::around(*tile).iter()).all(|t| grid.standing[*t].is_some())
                && grid.seats.iter().all(|(t, _)| tiles::range(*t, *tile) > 1)
        };
        (tiles::within(range, at).into_iter().filter(idle))
            .min_by_key(|t| (tiles::range(at, *t), *t))
    }

    pub fn passable(&self, room: RoomName, tile: Tile) -> bool {
        self.grid(room)
            .is_some_and(|grid| grid.held()[tile].is_some())
    }

    /// Walled for a walk in, open for a body standing in it to walk out.
    pub fn is_struck(&self, room: RoomName, tile: Tile) -> bool {
        let grid = self.grid(room).and_then(|g| g.struck.as_ref());
        grid.is_some_and(|s| s.contains(tile))
    }

    pub fn sees(&self, room: RoomName) -> bool {
        self.grid(room).is_some()
    }

    #[inline]
    pub fn has_terrain(&self, room: RoomName) -> bool {
        self.rooms.contains_key(&room.coords())
    }

    fn seat(footprint: Footprint, weights: &[Weight; tiles::COUNT], tile: Tile) -> bool {
        weight_of(footprint, weights, tile).is_some() && !on_edge(footprint, tile)
    }

    fn reach(footprint: Footprint, anchor: Tile, tile: Tile) -> i32 {
        match footprint {
            Footprint::One => tiles::range(anchor, tile),
            Footprint::Quad => {
                let gap = |a: i32, t: i32| (a - t).max(t - a - 1).max(0);
                gap(tiles::x_of(anchor), tiles::x_of(tile))
                    .max(gap(tiles::y_of(anchor), tiles::y_of(tile)))
            }
        }
    }

    fn seats_near(
        footprint: Footprint,
        weights: &[Weight; tiles::COUNT],
        range: i32,
        tile: Tile,
        into: &mut Vec<Tile>,
    ) {
        let spread = match footprint {
            Footprint::One => range,
            Footprint::Quad => range + 1,
        };
        let near = tiles::within(spread, tile);
        for anchor in near {
            if Atlas::reach(footprint, anchor, tile) <= range
                && Atlas::seat(footprint, weights, anchor)
            {
                into.push(anchor);
            }
        }
    }

    pub fn seats(&self, footprint: Footprint, range: i32, target: &Pos) -> Vec<Pos> {
        match self.grid(target.room) {
            Some(grid) => {
                let mut found = Vec::new();
                Atlas::seats_near(
                    footprint,
                    &grid.weights,
                    range,
                    tiles::of_pos(target),
                    &mut found,
                );
                found
                    .into_iter()
                    .map(|tile| tiles::to_pos(target.room, tile))
                    .collect()
            }
            None => Vec::new(),
        }
    }

    /// A body that steps every tick off swamp: the pace a route is priced at.
    const PACE: Mover = Mover {
        footprint: Footprint::One,
        factor: FatigueFactor {
            weight: 1,
            moves: 1,
        },
    };

    const EXITS: [i32; 4] = [tiles::TOP, tiles::RIGHT, tiles::BOTTOM, tiles::LEFT];

    /// Towers cover the whole room: a walk ends there or goes round it, never through.
    fn towered(&self, c: Coords) -> bool {
        self.rooms.get(&c).is_some_and(|grid| grid.towered)
    }

    /// A keeper room whose lairs no intel places.
    fn crossed_last(&self, c: Coords) -> bool {
        match self.rooms.get(&c) {
            Some(grid) => grid.blind,
            None => self
                .lairs
                .get(&c)
                .copied()
                .unwrap_or(tiles::is_keeper_room(c)),
        }
    }

    /// Past `MOST_FLOODS`, the least lately asked go first.
    fn asked_floods(floods: Floods) -> Floods {
        let mut floods = Atlas::unasked(floods, Atlas::FLOOD_TICKS);
        let table = floods.get_mut();
        if table.len() > Atlas::MOST_FLOODS {
            let mut ages: Vec<u32> = table.values().map(|k| k.unasked.get()).collect();
            let (_, &mut oldest, _) = ages.select_nth_unstable(Atlas::MOST_FLOODS);
            table.retain(|_, k| k.unasked.get() < oldest);
        }
        floods
    }

    fn unasked<K, T, R>(mut carried: Carried<K, T, R>, ticks: u32) -> Carried<K, T, R> {
        (carried.get_mut()).retain(|_, k| k.unasked.replace(k.unasked.get() + 1) < ticks);
        carried
    }

    /// The rooms within `hops` of `(x, y)`, read once a tick; equal to last
    /// tick's, it is last tick's `Rc`, so a kept walk checks it by address.
    fn read_around(&self, (x, y): Coords, hops: i32) -> Reads {
        let reading = || -> Vec<RoomRead> {
            (-hops..=hops)
                .flat_map(|dx| {
                    (-(hops - dx.abs())..=hops - dx.abs()).map(move |dy| (x + dx, y + dy))
                })
                .map(|c| {
                    let grid = self.rooms.get(&c);
                    let (seen, towered) = grid.map_or((false, false), |g| (g.seen, g.towered));
                    let flags = [
                        seen,
                        self.crossed_last(c),
                        towered,
                        self.closed.contains(&c),
                    ];
                    RoomRead(
                        grid.map(|g| g.weights.clone()),
                        grid.and_then(|g| g.struck.clone()),
                        flags,
                    )
                })
                .collect()
        };
        (self.reads.borrow_mut().entry(((x, y), hops)))
            .or_insert_with(|| {
                let read = reading();
                match self.last_reads.get(&((x, y), hops)) {
                    Some(last) if **last == *read => last.clone(),
                    _ => read.into(),
                }
            })
            .clone()
    }

    /// A search from `root` reads no room more than `hops` crossings off, so
    /// `value` is carried while each of those reads as it did.
    fn carried<K: std::hash::Hash + Eq, T: Clone + PartialEq>(
        &self,
        table: &Carried<K, T>,
        (key, (x, y), hops): (K, Coords, i32),
        fresh: impl Fn() -> T,
    ) -> T {
        let read = self.read_around((x, y), hops);
        let same = |k: &&Kept<T>| Rc::ptr_eq(&k.read, &read) || k.read == read;
        if let Some(k) = table.borrow().get(&key).filter(same) {
            k.unasked.set(0);
            if !self.check {
                return k.value.clone();
            }
            let value = fresh();
            assert!(value == k.value, "a carried value differs from a fresh one");
            return value;
        }
        let value = fresh();
        self.recomputed.set(self.recomputed.get() + 1);
        let kept = Kept {
            read,
            value: value.clone(),
            unasked: Cell::new(0),
        };
        table.borrow_mut().insert(key, kept);
        value
    }

    pub fn tree(&self, avoid: &BTreeSet<RoomName>, max_hops: i32, root: RoomName) -> Tree {
        let key = (root, avoid.clone(), max_hops);
        self.carried(&self.trees, (key, root.coords(), max_hops), || {
            graph::search(self, Atlas::PACE, root, None, avoid, max_hops, &[]).tree()
        })
    }

    /// The engine builds nothing on the border. `round` is walled in the target's room.
    pub fn walk(
        &self,
        mover: Mover,
        target: &Target,
        round: Option<&TileSet>,
        from: &Pos,
        longest: usize,
    ) -> Vec<Pos> {
        let key = (mover, target.clone(), round.cloned(), *from, longest);
        let field_at = |at: &Pos| {
            let round = round.filter(|_| at.room == target.room);
            self.field_in(mover, target, at, round.map_or(Ground::Kept, Ground::Round))
        };
        // A stop's region is resolved against the rooms beside it.
        let reach = (key, target.room.coords(), target.max_hops + 1);
        self.carried(&self.walks, reach, || {
            let mut path: Vec<Pos> = Vec::new();
            let mut room = from.room;
            let mut tile = tiles::of_pos(from);
            let mut field = field_at(from);
            while let Some(f) = field.as_ref().filter(|_| path.len() < longest) {
                let step = (f.step(tile)).and_then(|direction| tiles::neighbour(tile, direction));
                field = match step {
                    None => None,
                    Some(next) if tiles::on_edge(next) => {
                        let onto = tiles::room_toward(room, tiles::exit_of(next));
                        onto.zip(tiles::landing(next)).and_then(|(onto, landing)| {
                            (room, tile) = (onto, landing);
                            field_at(&tiles::to_pos(room, tile))
                        })
                    }
                    Some(next) => {
                        tile = next;
                        path.push(tiles::to_pos(room, tile));
                        field
                    }
                };
            }
            path
        })
    }

    pub fn route(
        &self,
        avoid: &BTreeSet<RoomName>,
        max_hops: i32,
        from: RoomName,
        onto: RoomName,
    ) -> Option<Vec<RoomName>> {
        if from == onto {
            return Some(vec![from]);
        }
        let stop = [(from.coords(), None)];
        let ways = graph::search(self, Atlas::PACE, onto, None, avoid, max_hops, &stop);
        ways.tree().route(from, onto)
    }

    fn anchor(footprint: Footprint, t: Tile) -> Tile {
        let inner = tiles::SIDE - 2;
        match footprint {
            Footprint::One => t,
            Footprint::Quad => tiles::index(tiles::x_of(t).min(inner), tiles::y_of(t).min(inner)),
        }
    }

    fn exit_goals(
        footprint: Footprint,
        weights: &[Weight; tiles::COUNT],
        edge: &[Tile],
    ) -> Vec<Tile> {
        let goals = edge.iter().map(|t| Atlas::anchor(footprint, *t));
        goals
            .filter(|t| weight_of(footprint, weights, *t).is_some())
            .collect()
    }

    fn stretch(first: Tile, last: Tile) -> &'static [Tile] {
        let band = tiles::exit_tiles(tiles::exit_of(first));
        let at = |t: Tile| band.iter().position(|b| *b == t).unwrap_or(0);
        &band[at(first)..=at(last)]
    }

    fn flood_seats(
        &self,
        grid: &RoomGrid,
        mover: Mover,
        target: &Target,
        ground: Ground,
    ) -> Rc<Table<i32>> {
        let mut owned = Vec::new();
        let tiles = if target.tiles.is_sorted_by(|a, b| a < b) {
            &target.tiles
        } else {
            owned.extend_from_slice(&target.tiles);
            owned.sort_unstable();
            owned.dedup();
            &owned
        };
        let range = target.range;
        self.flood_in(grid, mover, Goal::Seats { range, tiles }, ground)
    }

    fn flood_in(
        &self,
        grid: &RoomGrid,
        mover: Mover,
        goal: Goal,
        ground: Ground,
    ) -> Rc<Table<i32>> {
        let (dist, kept, live, relaxed) = grid.flood(mover, goal, ground, self.check);
        let count = match (kept, live) {
            (true, true) => None,
            (true, false) => Some(&self.hits),
            (false, false) => Some(&self.misses),
            (false, true) => Some(&self.live),
        };
        count.inspect(|c| c.set(c.get() + 1));
        self.relaxed.set(self.relaxed.get() + relaxed);
        dist
    }

    pub fn field(&self, mover: Mover, target: &Target, at: &Pos) -> Option<Field> {
        self.field_round(mover, target, at, None)
    }

    pub fn reaches_live(&self, from: Option<&Pos>, to: &Pos) -> bool {
        let Some(field) = self.field(Atlas::PACE, &target_at(to, 1), to) else {
            return true;
        };
        let walked = |t: &Tile| field.ticks(*t).is_some();
        if let Some(from) = from.filter(|f| f.room == to.room) {
            return walked(&tiles::of_pos(from));
        }
        (Atlas::EXITS.iter()).any(|e| tiles::exit_tiles(*e).iter().any(walked))
    }

    /// Out of another room than the target's, the flood toward the exit the
    /// room graph leaves `at`'s region by. `danger` is walled in this room
    /// only, since the room graph prices rooms, not tiles; from inside it in
    /// the target's room, the walk is to the nearest tile clear of it.
    pub fn field_round(
        &self,
        mover: Mover,
        target: &Target,
        at: &Pos,
        danger: Option<&TileSet>,
    ) -> Option<Field> {
        self.field_in(mover, target, at, Ground::Live(danger))
    }

    fn field_in(&self, mover: Mover, target: &Target, at: &Pos, ground: Ground) -> Option<Field> {
        let grid = self.grid(at.room).filter(|_| mover.factor.moves > 0)?;
        let tile = tiles::of_pos(at);
        let danger = ground.walls();
        let inside = danger.is_some_and(|d| d.contains(tile));
        let field = |dist, onward| Atlas::field_of(grid, mover, dist, onward, ground);
        if at.room == target.room && inside {
            let clear = self.flood_in(grid, mover, Goal::Clear, ground);
            return Some(field(clear, 0));
        }
        if at.room == target.room {
            let seats = self.flood_seats(grid, mover, target, ground);
            return Some(field(seats, 0));
        }
        let (avoid, hops) = (&target.avoid, target.max_hops);
        let stop = [(at.room.coords(), Some(tile))];
        let ways = graph::search(self, mover, target.room, Some(target), avoid, hops, &stop);
        let (dist, onward, exit) = ways.out_of(self, mover, at.room, tile)?;
        // Reached on its way through, a body leaves by the graph's way: a
        // hostile hunting it follows it anywhere in the room, never across.
        let danger = danger.filter(|_| !inside);
        let live = matches!(ground, Ground::Live(_));
        if !live || (danger.is_none() && grid.held.is_none()) {
            return Some(field(dist, onward));
        }
        let walled = self.flood_in(grid, mover, Goal::Exit(exit), Ground::Live(danger));
        if walled[tile] != UNREACHED {
            return Some(field(walled, onward));
        }
        // The graph's way out is walled: any other way on the whole tree knows.
        let all = graph::search(self, mover, target.room, Some(target), avoid, hops, &[]);
        let (dist, onward) = all
            .round(self, mover, at.room, tile, danger)
            .unwrap_or((walled, onward));
        Some(field(dist, onward))
    }

    fn field_of(
        grid: &RoomGrid,
        mover: Mover,
        dist: Rc<Table<i32>>,
        onward: i32,
        ground: Ground,
    ) -> Field {
        let live = matches!(ground, Ground::Live(_));
        Field {
            footprint: mover.footprint,
            weights: if live { grid.held() } else { &grid.weights }.clone(),
            factor: mover.factor,
            dist,
            struck: grid.struck.clone(),
            onward,
        }
    }

    pub fn travel_ticks(&self, mover: Mover, target: &Target, from: &Pos) -> Option<i32> {
        self.field_in(mover, target, from, Ground::Kept)
            .and_then(|field| field.ticks(tiles::of_pos(from)))
    }

    pub const CROSSING_TICKS: i32 = engine::ROOM_SIDE;

    /// A body asks its walks at its choice of work, about once a trip.
    const TRIP_TICKS: u32 = 100;

    /// A room's goals come back a few trips apart: at `TRIP_TICKS` a soak floods 15% more.
    const FLOOD_TICKS: u32 = 300;

    /// ~10 kB each, and wasm keeps its high-water mark: twice a soak's busiest room.
    const MOST_FLOODS: usize = 2000;

    /// `ticks_to` off `tree`, grown from the target's room: each room the
    /// mover's walk from where it lands to where the tree leaves it.
    pub fn ticks_on(&self, tree: &Tree, mover: Mover, target: &Target, from: &Pos) -> Option<i32> {
        let (start, root) = (from.room.coords(), tree.root?);
        let flood = |c: Coords, exit: &graph::Exit| {
            Some(self.flood_in(self.rooms.get(&c)?, mover, Goal::Exit(exit), Ground::Kept))
        };
        // On the band already, a body lands on its own mirror tile: the leg
        // is the walk it takes on either side of the border.
        let at = tiles::of_pos(from);
        let mirror = tiles::on_edge(at).then(|| tiles::landing(at)).flatten();
        let landed = |dist: &Table<i32>, exit: &graph::Exit, own: Option<Tile>| {
            let into = exit.iter().flat_map(|(a, b)| Atlas::stretch(*a, *b));
            let side = into.filter_map(|t| Some(dist[tiles::landing(*t)?])).min();
            own.map(|l| dist[l]).or(side)
        };
        let by = |out: &Way| -> Option<i32> {
            let grid = self.rooms.get(&start)?;
            let out_by = flood(start, &out.exit)?;
            let mut ticks = Atlas::field_of(grid, mover, out_by, 0, Ground::Kept).ticks(at)?;
            let mut own = mirror.filter(|_| ticks == 0);
            let mut last = out;
            while last.next != root {
                let way = tree.ways(last.next).get(last.onward)?;
                let walk = flood(last.next, &way.exit)
                    .map_or(Some(Atlas::CROSSING_TICKS), |d| landed(&d, &last.exit, own));
                ticks = ticks.saturating_add(walk?);
                own = None;
                last = way;
            }
            let seats = self
                .grid(target.room)
                .map(|g| self.flood_seats(g, mover, target, Ground::Kept));
            let entry = match seats {
                Some(seats) => landed(&seats, &last.exit, own)?,
                None => Atlas::CROSSING_TICKS,
            };
            Some(ticks.saturating_add(entry))
        };
        let ticks = tree.ways(start).iter().filter_map(by).min()?;
        (ticks < UNREACHED).then_some(ticks)
    }

    /// The ticks from each tile of `key.room` to beside `key.to`
    /// (`ARCHITECTURE.md` §1); a room with no terrain is open plain. The tree
    /// never re-enters its root: a body walled off in it reads None.
    fn toward(&self, key: Toward) -> Option<Rc<Table<i32>>> {
        let rooms = self.read_around(key.to.room.coords(), key.max_hops);
        let held = (self.towards.borrow().get(&key))
            .filter(|k| Rc::ptr_eq(&k.read, &rooms))
            .map(|k| {
                k.unasked.set(0);
                k.value.dist.clone()
            });
        if let Some(dist) = held.clone().filter(|_| !self.check) {
            return dist;
        }
        let weights = (self.rooms.get(&key.room)).map(|g| match key.bare {
            Some(paved) => g.bare[usize::from(paved)].clone(),
            None => g.weights.clone(),
        });
        let mut seeds = Vec::new();
        // No way leads out of the tree's root: its walk needs no tree.
        let root = key.room == key.to.room.coords();
        let tree = (!root).then(|| self.tree(&BTreeSet::new(), key.max_hops, key.to.room));
        if root {
            let plain = Atlas::ground(&[0; tiles::COUNT]);
            let ground = weights.as_deref().unwrap_or(&plain);
            Atlas::seats_near(
                Footprint::One,
                ground,
                1,
                tiles::of_pos(&key.to),
                &mut seeds,
            );
        }
        let mut seeds: Vec<(Tile, i32)> = seeds.into_iter().map(|t| (t, 0)).collect();
        let ways = tree.as_ref().map_or(&[][..], |t| t.ways(key.room));
        if key.way.is_none() && ways.len() == 1 {
            let one = Toward {
                way: Some(0),
                ..key
            };
            let dist = self.toward(one);
            let walked = self.towards.borrow()[&one].value.clone();
            self.keep(key, rooms, walked);
            return dist;
        }
        let ways = key
            .way
            .map_or(ways, |w| ways.get(w..=w).unwrap_or_default());
        for way in ways {
            let way_on = (way.next != key.to.room.coords()).then_some(way.onward);
            let next = Toward {
                room: way.next,
                way: way_on,
                ..key
            };
            let Some(onto) = self.toward(next) else {
                continue;
            };
            let exit = way.exit.iter().flat_map(|(a, b)| Atlas::stretch(*a, *b));
            seeds.extend(exit.filter_map(|t| Some((*t, onto[tiles::landing(*t)?]))));
        }
        seeds.retain(|(_, d)| *d != UNREACHED);
        seeds.sort_unstable_by_key(|(t, d)| (*d, *t));
        // A room off the tree lies past the read that holds the walk.
        let walked = Walked {
            weights: weights.filter(|_| !seeds.is_empty()),
            seeds,
            dist: None,
        };
        let kept = (self.towards.borrow().get(&key))
            .filter(|k| k.value.reads_as(&walked))
            .map(|k| k.value.dist.clone());
        assert!(
            held.is_none() || kept.is_some(),
            "a held walk reads otherwise"
        );
        let walked = match kept {
            Some(dist) if !self.check => Walked { dist, ..walked },
            Some(dist) => {
                let walked = walked.flooded();
                assert!(
                    walked.dist == dist,
                    "a carried value differs from a fresh one"
                );
                walked
            }
            None => {
                self.recomputed.set(self.recomputed.get() + 1);
                walked.flooded()
            }
        };
        let dist = walked.dist.clone();
        self.keep(key, rooms, walked);
        dist
    }

    fn keep(&self, key: Toward, read: Reads, value: Walked) {
        let kept = Kept {
            read,
            value,
            unasked: Cell::new(0),
        };
        self.towards.borrow_mut().insert(key, kept);
    }

    /// `toward`'s ticks at `from`; a tile no body stands on (a structure's)
    /// is a step past its nearest.
    pub fn ticks_toward(&self, max_hops: i32, to: &Pos, from: &Pos) -> Option<i32> {
        self.ticks_on_way(max_hops, to, from, None)
    }

    /// `ticks_toward` on terrain alone, or `paved`, as if every open tile were.
    pub fn ticks_bare(&self, paved: bool, max_hops: i32, to: &Pos, from: &Pos) -> Option<i32> {
        self.ticks_on_way(max_hops, to, from, Some(paved))
    }

    fn ticks_on_way(&self, max_hops: i32, to: &Pos, from: &Pos, bare: Option<bool>) -> Option<i32> {
        let key = Toward {
            to: *to,
            max_hops,
            room: from.room.coords(),
            way: None,
            bare,
        };
        let dist = self.toward(key)?;
        let tile = tiles::of_pos(from);
        match dist[tile] {
            UNREACHED => Some(dist[tiles::least_around(tile, UNREACHED, |n| dist[n])?] + 1),
            d => Some(d),
        }
    }

    pub fn ticks_to(&self, mover: Mover, target: &Target, from: &Pos) -> Option<i32> {
        let field = self.field_in(mover, target, from, Ground::Kept)?;
        Some(field.ticks(tiles::of_pos(from))? + field.onward)
    }

    pub fn first_step(&self, mover: Mover, target: &Target, from: &Pos) -> Option<i32> {
        self.field(mover, target, from)
            .and_then(|field| field.step(tiles::of_pos(from)))
    }
}

/// Where a body with work goes next (owner: wherever there is work, never
/// home): the most worth a tick of the walk, the nearest on a tie.
pub fn onward(candidates: impl Iterator<Item = (RoomName, i32, i32)>) -> Option<RoomName> {
    candidates
        .filter(|(_, worth, _)| *worth > 0)
        .max_by_key(|&(room, worth, walk)| {
            (
                rate(worth, walk),
                std::cmp::Reverse(walk),
                std::cmp::Reverse(room),
            )
        })
        .map(|(room, _, _)| room)
}

/// What `onward` ranks by: worth a tick of the walk, a room's crossing counted.
pub fn rate(worth: i32, walk: i32) -> i64 {
    i64::from(worth) * 1000 / i64::from(walk + engine::ROOM_SIDE)
}

/// Beside, not the whole room: a walk round it prices dearer than a step through a seated body.
pub fn parts(at: Tile, open: impl Fn(Tile) -> bool) -> bool {
    let beside: Vec<Tile> = (tiles::around(at).iter().copied())
        .filter(|n| open(*n))
        .collect();
    let mut reached: Vec<Tile> = beside.iter().take(1).copied().collect();
    let mut i = 0;
    while let Some(&t) = reached.get(i) {
        let next: Vec<Tile> = (beside.iter().copied())
            .filter(|n| !reached.contains(n) && tiles::range(*n, t) == 1)
            .collect();
        reached.extend(next);
        i += 1;
    }
    reached.len() < beside.len()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_kept_flood_is_found_by_its_borrowed_goal_and_by_no_other() {
        let t = Tile::of_index;
        let (tiles, exit) = ([t(3), t(7)], [(t(3), t(7))]);
        let seats = Goal::Seats {
            range: 1,
            tiles: &tiles,
        };
        let mut kept: HashMap<FloodKey, i32> = HashMap::default();
        kept.insert(FloodOf::of(Atlas::PACE, seats, None, false).kept(), 1);
        kept.insert(
            FloodOf::of(Atlas::PACE, Goal::Exit(&exit), None, false).kept(),
            2,
        );
        let at = |goal| {
            kept.get(&FloodOf::of(Atlas::PACE, goal, None, false) as &dyn Keyed)
                .copied()
        };
        assert_eq!(at(seats), Some(1));
        assert_eq!(at(Goal::Exit(&exit)), Some(2));
        assert_eq!(
            at(Goal::Seats {
                range: 2,
                tiles: &tiles
            }),
            None
        );
    }
}
