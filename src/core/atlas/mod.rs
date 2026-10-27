//! The floods are the one cache (`ARCHITECTURE.md` §1). Built twice from one
//! `World`, an Atlas answers identically: a kept flood is one on equal weights.
use std::cell::{Cell, RefCell};
use std::collections::BTreeSet;

use crate::core::HashMap;
use std::rc::Rc;

mod graph;
pub mod grid;
pub mod tiles;

pub use self::graph::Tree;
use self::graph::Way;

use self::grid::{
    FatigueFactor, Footprint, UNREACHED, Weight, flood, on_edge, step_table, step_ticks, weight_of,
};
use self::tiles::{Table, Tile, TileSet};
use super::engine;
use super::exchange::fighter::{self, Fighter};
use super::vocabulary::BodyPart;
use super::world::{Kind, Owner, Pos, Room, RoomName, World};

/// A keeper's lair and the resource it guards.
pub type Post = (Tile, Tile);

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Mover {
    pub footprint: Footprint,
    pub factor: FatigueFactor,
}

/// `tiles` are tile indices; `avoid` never holds a walk back from its ends.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Target {
    pub room: RoomName,
    pub tiles: Vec<Tile>,
    pub range: i32,
    pub avoid: BTreeSet<RoomName>,
    pub max_hops: i32,
}

/// Our creeps are no wall (who yields is `Move`'s call); a creep not ours
/// yields to nothing we decide, so nothing may stand on its tile this tick.
/// A room out of sight is what stood there when last seen, else its
/// terrain, and the strikes intel places.
#[derive(Clone, Debug)]
pub struct RoomGrid {
    pub weights: Rc<Table<Weight>>,
    floods: RefCell<HashMap<FloodKey, Rc<Table<i32>>>>,
    /// The open tiles a keeper strikes, walled.
    pub struck: Option<Rc<TileSet>>,
    /// The weights without creeps or strikes: what stands once out of sight.
    standing: Rc<Table<Weight>>,
    seen: bool,
    /// A keeper room out of sight whose lairs no intel places: crossed last.
    blind: bool,
}

/// What a flood is a function of besides the room's weights: the mover's
/// footprint and pace, and its goals.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FloodKey {
    footprint: Footprint,
    steps: [Option<i32>; 3],
    goal: Goal,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum Goal {
    Seats { range: i32, tiles: Vec<Tile> },
    Exit(graph::Exit),
}

impl FloodKey {
    fn of(mover: Mover, goal: Goal) -> FloodKey {
        let grounds = [
            engine::ROAD_WEIGHT,
            engine::PLAIN_WEIGHT,
            engine::SWAMP_WEIGHT,
        ];
        FloodKey {
            footprint: mover.footprint,
            steps: grounds.map(|w| step_ticks(mover.factor, w)),
            goal,
        }
    }
}

impl RoomGrid {
    /// The flood toward `goal`, and whether it was kept.
    fn flood(&self, mover: Mover, goal: Goal) -> (Rc<Table<i32>>, bool) {
        let key = FloodKey::of(mover, goal);
        if let Some(dist) = self.floods.borrow().get(&key) {
            return (dist.clone(), true);
        }
        let footprint = mover.footprint;
        let goals = match &key.goal {
            Goal::Seats {
                range,
                tiles: targets,
            } => {
                let mut goals = Vec::new();
                for &tile in targets {
                    if tile.index() < tiles::COUNT {
                        Atlas::seats_near(footprint, &self.weights, *range, tile, &mut goals);
                    }
                }
                goals
            }
            Goal::Exit(exit) => (exit.iter())
                .flat_map(|(a, b)| {
                    Atlas::exit_goals(footprint, &self.weights, Atlas::stretch(*a, *b))
                })
                .collect(),
        };
        let steps = step_table(mover.factor);
        let dist = Rc::new(flood(footprint, &self.weights, &steps, &goals));
        self.floods.borrow_mut().insert(key, dist.clone());
        (dist, false)
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

#[derive(Clone, Debug)]
pub struct Atlas {
    rooms: HashMap<Coords, RoomGrid>,
    /// The rooms intel saw whose lairs the name formula misreads.
    pub lairs: HashMap<Coords, bool>,
    closed: Vec<Coords>,
    misses: Cell<i32>,
    hits: Cell<i32>,
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
        match self.via(next) {
            d if d == UNREACHED => None,
            d => Some(d),
        }
    }

    /// Out of a strike a body stands in, the shortest way across struck
    /// tiles, never deeper, onto the open tile nearest the goal.
    fn escape(&self, from: Tile) -> Option<i32> {
        let struck = self.struck.as_ref().filter(|_| self.footprint.is_one())?;
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

    pub fn controller_link(room: &Room) -> Option<&super::world::Structure> {
        let c = room.controller.as_ref()?;
        let mut links: Vec<&super::world::Structure> = room
            .structures
            .iter()
            .filter(|s| {
                s.kind.is_link()
                    && s.owner.is_ours()
                    && c.pos.within(engine::UPGRADE_RANGE, &s.pos)
                    && !Atlas::is_source_link(room, &s.pos)
            })
            .collect();
        links.sort_by_key(|s| (c.pos.range(&s.pos), tiles::of_pos(&s.pos)));
        links.into_iter().next()
    }

    pub fn buffer(room: &Room) -> Option<&super::world::Structure> {
        Atlas::controller_link(room).or_else(|| {
            room.structures
                .iter()
                .find(|s| s.kind.is_container() && Atlas::is_buffer(room, &s.pos))
        })
    }

    /// No site is built under a creep, so these are kept clear of solid ones.
    pub fn upgrader_seats(
        walkable: impl Fn(Tile) -> bool,
        controller: Tile,
        buffer: Tile,
    ) -> Vec<Tile> {
        tiles::within(1, buffer)
            .into_iter()
            .filter(|t| walkable(*t) && tiles::range(*t, controller) <= engine::UPGRADE_RANGE)
            .collect()
    }

    /// Farthest from the controller first, so a controller container on a seat
    /// is no post.
    pub fn post_of(room: &Room, source: &Pos) -> Option<(Pos, Option<String>)> {
        let from_controller = |p: &Pos| -> i32 {
            room.controller
                .as_ref()
                .and_then(|c| c.pos.range(p))
                .unwrap_or(0)
        };
        let mut candidates: Vec<(Pos, Option<String>)> = Vec::new();
        for s in &room.structures {
            if s.kind.is_container() {
                candidates.push((s.pos, Some(s.id.clone())));
            }
        }
        for s in &room.sites {
            if s.kind.is_container() && s.owner.is_ours() {
                candidates.push((s.pos, None));
            }
        }
        candidates.retain(|(p, _)| p.within(1, source) && p != source);
        candidates
            .sort_by_key(|(p, built)| (-from_controller(p), built.is_none(), tiles::of_pos(p)));
        candidates.into_iter().next()
    }

    /// A link buffer's upgrader tiles stay open: held, they priced every walk
    /// past the controller dearer.
    pub fn work_seats(room: &Room) -> Vec<Tile> {
        let mut seats: Vec<Tile> = room
            .sources
            .iter()
            .filter_map(|s| Atlas::post_of(room, &s.pos).map(|(p, _)| tiles::of_pos(&p)))
            .collect();
        if let Some(b) = Atlas::buffer(room)
            && b.kind.is_container()
        {
            seats.push(tiles::of_pos(&b.pos));
        }
        seats
    }

    pub fn empty() -> Atlas {
        Atlas {
            rooms: HashMap::default(),
            lairs: HashMap::default(),
            closed: Vec::new(),
            misses: Cell::new(0),
            hits: Cell::new(0),
        }
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
        let lairs = room.structures.iter().filter(|s| s.kind.is_keeper_lair());
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
        let open = |t: Tile| terrain[t.index()] & super::world::terrain::WALL == 0;
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

    fn strikes(terrain: &[u8], strikers: &[(Tile, Tile, Fighter)]) -> Vec<Tile> {
        let walks = strikers.iter().flat_map(|(from, resource, f)| {
            let walk = Atlas::walk_of(terrain, *from, *resource);
            walk.into_iter().flat_map(|w| tiles::within(f.reach, w))
        });
        walks.collect()
    }

    /// The tiles the keepers strike from anywhere on the walk to their posts:
    /// a living one's, and a dead one's successor's from its lair once it
    /// spawns before a crossing ends.
    fn struck(room: &Room) -> Vec<Tile> {
        let living = room.others.iter().filter(|c| c.owner == Owner::Keeper);
        let striker =
            |from: Tile, f: Fighter| (from, Atlas::guarded(room, from).unwrap_or(from), f);
        let mut strikers: Vec<(Tile, Tile, Fighter)> = living
            .map(|c| {
                let body: Vec<BodyPart> = c.body.iter().map(|p| p.kind).collect();
                let boosts: Vec<_> = c.body.iter().map(|p| p.boost).collect();
                let f = fighter::of_parts(&body, &boosts, c.hits, None);
                striker(tiles::of_pos(&c.pos), f)
            })
            .collect();
        for lair in room.structures.iter().filter(|s| s.kind.is_keeper_lair()) {
            if lair.spawn.is_some_and(|t| t <= Atlas::crossing_ticks()) {
                let f = fighter::cast(&engine::keeper_body());
                strikers.push(striker(tiles::of_pos(&lair.pos), f));
            }
        }
        match &room.terrain {
            Some(terrain) => Atlas::strikes(&terrain[..], &strikers),
            None => Vec::new(),
        }
    }

    /// ~10 kB each. A dropped resource's tile is a new goal, so a room's keys
    /// are unbounded; past this the room floods afresh.
    const KEPT_FLOODS: usize = 500;

    fn same(a: &[Weight; tiles::COUNT], b: &[Weight; tiles::COUNT]) -> bool {
        a == b
    }

    /// The floods `previous` kept, while its weights hold and the room's
    /// keys stay bounded.
    fn kept(
        previous: Option<RoomGrid>,
        weights: &[Weight; tiles::COUNT],
    ) -> RefCell<HashMap<FloodKey, Rc<Table<i32>>>> {
        match previous {
            Some(grid)
                if grid.floods.borrow().len() < Atlas::KEPT_FLOODS
                    && Atlas::same(&grid.weights, weights) =>
            {
                grid.floods
            }
            _ => RefCell::default(),
        }
    }

    fn ground(terrain: &[u8]) -> Table<Weight> {
        let mut weights: Table<Weight> = tiles::table(None);
        for (weight, t) in weights.iter_mut().zip(terrain) {
            *weight = if t & super::world::terrain::WALL != 0 {
                None
            } else if t & super::world::terrain::SWAMP != 0 {
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
    /// name, never walked. `seen` is intel's: each room and its keepers'
    /// posts, `None` where unread; `terrains` holds rooms out of sight too.
    pub fn following<'a>(
        mut previous: Atlas,
        world: &World,
        seen: impl IntoIterator<Item = (RoomName, Option<&'a [Post]>)>,
        terrains: impl IntoIterator<Item = (RoomName, &'a [u8; tiles::COUNT])>,
    ) -> Atlas {
        let mut rooms: HashMap<Coords, RoomGrid> = HashMap::default();

        for (c, room, terrain) in
            (world.rooms.iter()).filter_map(|r| Some((r.name.coords(), r, r.terrain.as_deref()?)))
        {
            let mut weights = Atlas::ground(terrain);

            for s in &room.structures {
                if s.kind == Kind::Road {
                    weights[tiles::of_pos(&s.pos)] = grid::weight(engine::ROAD_WEIGHT);
                }
            }

            for tile in Atlas::work_seats(room) {
                weights[tile] = grid::weight(grid::SEAT_WEIGHT);
            }

            for s in &room.structures {
                if !s.walkable {
                    weights[tiles::of_pos(&s.pos)] = None;
                }
            }

            for site in &room.sites {
                if site.owner.is_ours() && !site.kind.walkable() {
                    weights[tiles::of_pos(&site.pos)] = None;
                }
            }

            for source in &room.sources {
                weights[tiles::of_pos(&source.pos)] = None;
            }

            for mineral in &room.minerals {
                weights[tiles::of_pos(&mineral.pos)] = None;
            }

            if let Some(c) = &room.controller {
                weights[tiles::of_pos(&c.pos)] = None;
            }

            let standing = Rc::new(weights.clone());
            for creep in room.hostiles() {
                weights[tiles::of_pos(&creep.pos)] = None;
            }

            let struck = Atlas::walled(&mut weights, Atlas::struck(room));
            let weights = Rc::new(weights);
            let grid = RoomGrid {
                floods: Atlas::kept(previous.rooms.remove(&c), &weights),
                weights,
                struck,
                standing,
                seen: true,
                blind: false,
            };
            rooms.insert(c, grid);
        }

        let posts: HashMap<RoomName, Option<&[Post]>> = seen.into_iter().collect();
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
            let grid = match previous.rooms.remove(&c) {
                Some(grid) if !grid.seen => {
                    if grid.floods.borrow().len() >= Atlas::KEPT_FLOODS {
                        grid.floods.borrow_mut().clear();
                    }
                    grid
                }
                last => {
                    let mut weights = match &last {
                        Some(grid) => (*grid.standing).clone(),
                        None => Atlas::ground(terrain),
                    };
                    let placed = posts.get(&name).copied().flatten().filter(|_| keeper(c));
                    let keeper_of = |&(lair, resource): &Post| {
                        (lair, resource, fighter::cast(&engine::keeper_body()))
                    };
                    let strikers: Vec<_> = placed.unwrap_or(&[]).iter().map(keeper_of).collect();
                    let struck = Atlas::walled(&mut weights, Atlas::strikes(terrain, &strikers));
                    let weights = Rc::new(weights);
                    RoomGrid {
                        floods: Atlas::kept(last, &weights),
                        standing: weights.clone(),
                        weights,
                        struck,
                        seen: false,
                        blind: keeper(c) && placed.is_none(),
                    }
                }
            };
            rooms.insert(c, grid);
        }

        Atlas {
            rooms,
            lairs,
            closed: world.closed.iter().map(|r| r.coords()).collect(),
            misses: Cell::new(0),
            hits: Cell::new(0),
        }
    }

    pub fn of_world(world: &World) -> Atlas {
        Atlas::following(Atlas::empty(), world, [], [])
    }

    pub fn misses(&self) -> i32 {
        self.misses.get()
    }

    pub fn hits(&self) -> i32 {
        self.hits.get()
    }

    /// Tiles are asked of rooms in sight: out of sight a grid is its
    /// terrain alone, the room graph's.
    fn grid(&self, room: RoomName) -> Option<&RoomGrid> {
        self.rooms.get(&room.coords()).filter(|g| g.seen)
    }

    pub fn weights(&self, room: RoomName) -> Option<Table<Weight>> {
        self.grid(room).map(|grid| (*grid.weights).clone())
    }

    /// The tiles a walk from the room's exits reaches without breaking a
    /// barrier.
    pub fn from_exits(&self, room: RoomName) -> TileSet {
        let mut reached = TileSet::default();
        let Some(grid) = self.grid(room) else {
            return reached;
        };
        let open = |t: &Tile| grid.weights[*t].is_some();
        let mut frontier: Vec<Tile> = [tiles::TOP, tiles::RIGHT, tiles::BOTTOM, tiles::LEFT]
            .iter()
            .flat_map(|exit| tiles::exit_tiles(*exit).iter().copied())
            .filter(open)
            .collect();
        while let Some(t) = frontier.pop() {
            if reached.insert(t) {
                frontier.extend(
                    tiles::around(t)
                        .iter()
                        .filter(|n| open(n) && !reached.contains(**n)),
                );
            }
        }
        reached
    }

    pub fn passable(&self, room: RoomName, tile: Tile) -> bool {
        self.grid(room)
            .is_some_and(|grid| grid.weights[tile].is_some())
    }

    /// Walled for a walk in, open for a body standing in it to walk out.
    pub fn is_struck(&self, room: RoomName, tile: Tile) -> bool {
        let grid = self.grid(room).and_then(|g| g.struck.as_ref());
        grid.is_some_and(|s| s.contains(tile))
    }

    pub fn sees(&self, room: RoomName) -> bool {
        self.grid(room).is_some()
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

    pub fn tree(&self, avoid: &BTreeSet<RoomName>, max_hops: i32, root: RoomName) -> Tree {
        graph::search(self, Atlas::PACE, root, None, avoid, max_hops, &[]).tree()
    }

    /// `tree`, grown only until each of `onto` is reached.
    pub fn tree_to(
        &self,
        avoid: &BTreeSet<RoomName>,
        max_hops: i32,
        root: RoomName,
        onto: &[RoomName],
    ) -> Tree {
        let stops: Vec<graph::Stop> = (onto.iter()).map(|r| (r.coords(), None)).collect();
        graph::search(self, Atlas::PACE, root, None, avoid, max_hops, &stops).tree()
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

    fn seats_of(target: &Target) -> Goal {
        let mut targets = target.tiles.clone();
        targets.sort_unstable();
        targets.dedup();
        Goal::Seats {
            range: target.range,
            tiles: targets,
        }
    }

    fn flood_in(&self, grid: &RoomGrid, mover: Mover, goal: Goal) -> Rc<Table<i32>> {
        let (dist, kept) = grid.flood(mover, goal);
        let count = if kept { &self.hits } else { &self.misses };
        count.set(count.get() + 1);
        dist
    }

    /// Out of another room than the target's, the flood toward the exit the
    /// room graph leaves `at`'s region by.
    pub fn field(&self, mover: Mover, target: &Target, at: &Pos) -> Option<Field> {
        let grid = self.grid(at.room).filter(|_| mover.factor.moves > 0)?;
        let tile = tiles::of_pos(at);
        if at.room == target.room {
            let seats = self.flood_in(grid, mover, Atlas::seats_of(target));
            return Some(Atlas::field_of(grid, mover, seats, 0));
        }
        let (avoid, hops) = (&target.avoid, target.max_hops);
        let stop = [(at.room.coords(), Some(tile))];
        let ways = graph::search(self, mover, target.room, Some(target), avoid, hops, &stop);
        let (dist, onward) = ways.out_of(self, mover, at.room, tile)?;
        Some(Atlas::field_of(grid, mover, dist, onward))
    }

    fn field_of(grid: &RoomGrid, mover: Mover, dist: Rc<Table<i32>>, onward: i32) -> Field {
        Field {
            footprint: mover.footprint,
            weights: grid.weights.clone(),
            factor: mover.factor,
            dist,
            struck: grid.struck.clone(),
            onward,
        }
    }

    pub fn travel_ticks(&self, mover: Mover, target: &Target, from: &Pos) -> Option<i32> {
        self.field(mover, target, from)
            .and_then(|field| field.ticks(tiles::of_pos(from)))
    }

    pub fn crossing_ticks() -> i32 {
        engine::ROOM_SIDE
    }

    /// `ticks_to` off `tree`, grown from the target's room: each room the
    /// mover's walk from where it lands to where the tree leaves it.
    pub fn ticks_on(&self, tree: &Tree, mover: Mover, target: &Target, from: &Pos) -> Option<i32> {
        let (start, root) = (from.room.coords(), tree.root?);
        let flood = |c: Coords, exit: &graph::Exit| {
            Some(self.flood_in(self.rooms.get(&c)?, mover, Goal::Exit(exit.clone())))
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
            let mut ticks = Atlas::field_of(grid, mover, out_by, 0).ticks(at)?;
            let mut own = mirror.filter(|_| ticks == 0);
            let mut last = out;
            while last.next != root {
                let way = tree.ways(last.next).get(last.onward)?;
                let walk = flood(last.next, &way.exit).map_or(Some(Atlas::crossing_ticks()), |d| {
                    landed(&d, &last.exit, own)
                });
                ticks = ticks.saturating_add(walk?);
                own = None;
                last = way;
            }
            let seats = self
                .grid(target.room)
                .map(|g| self.flood_in(g, mover, Atlas::seats_of(target)));
            let entry = match seats {
                Some(seats) => landed(&seats, &last.exit, own)?,
                None => Atlas::crossing_ticks(),
            };
            Some(ticks.saturating_add(entry))
        };
        let ticks = tree.ways(start).iter().filter_map(by).min()?;
        (ticks < UNREACHED).then_some(ticks)
    }

    pub fn ticks_to(&self, mover: Mover, target: &Target, from: &Pos) -> Option<i32> {
        let field = self.field(mover, target, from)?;
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
        .max_by_key(|(room, worth, walk)| {
            let rate = i64::from(*worth) * 1000 / i64::from(walk + engine::ROOM_SIDE);
            (rate, std::cmp::Reverse(*walk), std::cmp::Reverse(*room))
        })
        .map(|(room, _, _)| room)
}
