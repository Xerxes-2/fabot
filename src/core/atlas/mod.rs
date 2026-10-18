//! The floods are the one cache (`ARCHITECTURE.md` §1). Built twice from one
//! `World`, an Atlas answers identically: a kept flood is one on equal weights.
use std::cmp::Reverse;
use std::collections::{BTreeSet, BinaryHeap};

use crate::core::HashMap;
use std::rc::Rc;

pub mod grid;
pub mod tiles;

use self::grid::{
    FatigueFactor, Footprint, UNREACHED, Weight, flood, on_edge, step_table, step_ticks, weight_of,
};
use self::tiles::{Table, Tile, TileSet};
use super::engine;
use super::exchange::fighter::{self, Fighter};
use super::vocabulary::BodyPart;
use super::world::{Kind, Owner, Pos, Room, World};

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Mover {
    pub footprint: Footprint,
    pub factor: FatigueFactor,
}

/// `tiles` are tile indices; `avoid` never holds a walk back from its ends.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Target {
    pub room: String,
    pub tiles: Vec<Tile>,
    pub range: i32,
    pub avoid: BTreeSet<String>,
    pub max_hops: i32,
}

/// Our creeps are no wall (who yields is `Move`'s call); a creep not ours
/// yields to nothing we decide, so nothing may stand on its tile this tick.
#[derive(Clone, Debug)]
pub struct RoomGrid {
    pub weights: Rc<Table<Weight>>,
    pub floods: HashMap<FloodKey, Rc<Table<i32>>>,
    /// The open tiles a keeper strikes, walled; with any, the room graph
    /// prices the room's crossing.
    pub struck: Option<Rc<TileSet>>,
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
    Exit(i32),
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
    fn flood(&mut self, mover: Mover, goal: Goal) -> (Rc<Table<i32>>, bool) {
        let key = FloodKey::of(mover, goal);
        if let Some(dist) = self.floods.get(&key) {
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
            Goal::Exit(exit) => Atlas::exit_goals(footprint, &self.weights, *exit),
        };
        let steps = step_table(mover.factor);
        let dist = Rc::new(flood(footprint, &self.weights, &steps, &goals));
        self.floods.insert(key, dist.clone());
        (dist, false)
    }
}

type Coords = (i32, i32);

/// Each room a search reached, to the room it was reached from.
#[derive(Clone, Debug, Default)]
pub struct Tree(HashMap<Coords, Coords>);

impl Tree {
    pub fn contains(&self, room: &str) -> bool {
        tiles::room_coords(room).is_some_and(|c| self.0.contains_key(&c))
    }

    /// `Atlas::route`'s answer, read off the tree searched from `from`.
    pub fn route(&self, from: &str, onto: &str) -> Option<Vec<String>> {
        if from == onto {
            return Some(vec![from.to_string()]);
        }
        let start = tiles::room_coords(from)?;
        let mut at = tiles::room_coords(onto)?;
        let mut chain = vec![at];
        while at != start {
            at = *self.0.get(&at)?;
            chain.push(at);
        }
        Some(
            chain
                .into_iter()
                .rev()
                .map(|(x, y)| tiles::room_name(x, y))
                .collect(),
        )
    }
}

#[derive(Clone, Debug)]
pub struct Field {
    pub footprint: Footprint,
    pub weights: Rc<Table<Weight>>,
    pub factor: FatigueFactor,
    pub dist: Rc<Table<i32>>,
    pub struck: Option<Rc<TileSet>>,
}

#[derive(Clone, Debug)]
pub struct Atlas {
    pub rooms: HashMap<String, RoomGrid>,
    /// The rooms intel saw whose lairs the name formula misreads.
    pub lairs: HashMap<Coords, bool>,
    closed: Vec<Coords>,
    pub misses: i32,
    pub hits: i32,
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
                candidates.push((s.pos.clone(), Some(s.id.clone())));
            }
        }
        for s in &room.sites {
            if s.kind.is_container() && s.owner.is_ours() {
                candidates.push((s.pos.clone(), None));
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
            misses: 0,
            hits: 0,
        }
    }

    /// A keeper's walk from `from` to its post: the tile beside the resource
    /// it guards that a walk reaches first. One standing nowhere near a
    /// resource stays.
    fn walk_of(room: &Room, from: Tile) -> Vec<Tile> {
        let near = |p: &Pos| tiles::range(from, tiles::of_pos(p));
        let sources = room.sources.iter().map(|s| (false, &s.pos));
        let Some((_, resource)) = sources
            .chain(room.minerals.iter().map(|m| (true, &m.pos)))
            .filter(|(_, p)| near(p) <= engine::KEEPER_GUARD_RANGE)
            .min_by_key(|(mineral, p)| (*mineral, near(p)))
        else {
            return vec![from];
        };
        let resource = tiles::of_pos(resource);
        let open = |t: Tile| room.terrain[t] & super::world::terrain::WALL == 0;
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

    /// The tiles the keepers strike from anywhere on the walk to their posts:
    /// a living one's, and a dead one's successor's from its lair once it
    /// spawns before a crossing ends.
    fn struck(room: &Room) -> Vec<Tile> {
        let living = room.others.iter().filter(|c| c.owner == Owner::Keeper);
        let mut strikers: Vec<(Tile, Fighter)> = living
            .map(|c| {
                let body: Vec<BodyPart> = c.body.iter().map(|p| p.kind).collect();
                let boosts: Vec<_> = c.body.iter().map(|p| p.boost).collect();
                let f = fighter::of_parts(&body, &boosts, c.hits, None);
                (tiles::of_pos(&c.pos), f)
            })
            .collect();
        for lair in room.structures.iter().filter(|s| s.kind.is_keeper_lair()) {
            if lair.spawn.is_some_and(|t| t <= Atlas::crossing_ticks()) {
                let f = fighter::cast(&engine::keeper_body());
                strikers.push((tiles::of_pos(&lair.pos), f));
            }
        }
        let walks = strikers.iter().flat_map(|(from, f)| {
            let walk = Atlas::walk_of(room, *from);
            walk.into_iter().flat_map(|w| tiles::within(f.reach, w))
        });
        walks.collect()
    }

    /// ~10 kB each. A dropped resource's tile is a new goal, so a room's keys
    /// are unbounded; past this the room floods afresh.
    const KEPT_FLOODS: usize = 500;

    fn same(a: &[Weight; tiles::COUNT], b: &[Weight; tiles::COUNT]) -> bool {
        a == b
    }

    /// A room whose terrain buffer is not a whole room has no grid: routed by
    /// name, never walked. `seen` is intel's: each room and whether it holds a lair.
    pub fn following<'a>(
        previous: &Atlas,
        world: &World,
        seen: impl IntoIterator<Item = (&'a str, bool)>,
    ) -> Atlas {
        let mut rooms: HashMap<String, RoomGrid> = HashMap::default();

        for room in world
            .rooms
            .iter()
            .filter(|r| r.terrain.len() == tiles::COUNT)
        {
            let mut weights: Table<Weight> = tiles::table(None);
            let terrain = &room.terrain;

            for (weight, t) in weights.iter_mut().zip(terrain) {
                *weight = if t & super::world::terrain::WALL != 0 {
                    None
                } else if t & super::world::terrain::SWAMP != 0 {
                    grid::weight(engine::SWAMP_WEIGHT)
                } else {
                    grid::weight(engine::PLAIN_WEIGHT)
                };
            }

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

            for creep in room.hostiles() {
                weights[tiles::of_pos(&creep.pos)] = None;
            }

            let mut struck = TileSet::default();
            let mut any = false;
            for t in Atlas::struck(room) {
                if weights[t].take().is_some() {
                    any |= struck.insert(t);
                }
            }

            let weights = Rc::new(weights);
            let floods = match previous.rooms.get(&room.name) {
                Some(grid)
                    if grid.floods.len() < Atlas::KEPT_FLOODS
                        && Atlas::same(&grid.weights, &weights) =>
                {
                    grid.floods.clone()
                }
                _ => HashMap::default(),
            };

            let mut grid = RoomGrid {
                weights,
                floods,
                struck: any.then(|| Rc::new(struck)),
            };
            if grid.struck.is_some() {
                for exit in Atlas::EXITS {
                    grid.flood(Atlas::PACE, Goal::Exit(exit));
                }
            }
            rooms.insert(room.name.clone(), grid);
        }

        let lairs = seen
            .into_iter()
            .filter_map(|(name, lairs)| Some((tiles::room_coords(name)?, lairs)))
            .filter(|(c, lairs)| *lairs != tiles::is_keeper_room(*c))
            .collect();
        Atlas {
            rooms,
            lairs,
            closed: world
                .closed
                .iter()
                .filter_map(|r| tiles::room_coords(r))
                .collect(),
            misses: 0,
            hits: 0,
        }
    }

    pub fn of_world(world: &World) -> Atlas {
        Atlas::following(&Atlas::empty(), world, [])
    }

    pub fn misses(&self) -> i32 {
        self.misses
    }

    pub fn hits(&self) -> i32 {
        self.hits
    }

    pub fn weights(&self, room: &str) -> Option<Table<Weight>> {
        self.rooms.get(room).map(|grid| (*grid.weights).clone())
    }

    /// The tiles a walk from the room's exits reaches without breaking a
    /// barrier.
    pub fn from_exits(&self, room: &str) -> BTreeSet<Tile> {
        let mut reached: BTreeSet<Tile> = BTreeSet::new();
        let Some(grid) = self.rooms.get(room) else {
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
                        .filter(|n| open(n) && !reached.contains(n)),
                );
            }
        }
        reached
    }

    pub fn passable(&self, room: &str, tile: Tile) -> bool {
        self.rooms
            .get(room)
            .is_some_and(|grid| grid.weights[tile].is_some())
    }

    /// Walled for a walk in, open for a body standing in it to walk out.
    pub fn is_struck(&self, room: &str, tile: Tile) -> bool {
        let grid = self.rooms.get(room).and_then(|g| g.struck.as_ref());
        grid.is_some_and(|s| s.contains(tile))
    }

    pub fn sees(&self, room: &str) -> bool {
        self.rooms.contains_key(room)
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
        match self.rooms.get(&target.room) {
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
                    .map(|tile| tiles::to_pos(&target.room, tile))
                    .collect()
            }
            None => Vec::new(),
        }
    }

    fn linked(near: Option<&RoomGrid>, far: Option<&RoomGrid>, exit: i32) -> bool {
        let near = near.map(|g| &g.weights);
        let far = far.map(|g| &g.weights);
        for &tile in tiles::exit_tiles(exit) {
            let near_walkable = near.is_none_or(|weights| weights[tile].is_some());
            let far_walkable = far
                .is_none_or(|weights| tiles::landing(tile).is_some_and(|l| weights[l].is_some()));
            if near_walkable && far_walkable {
                return true;
            }
        }
        false
    }

    /// A body that steps every tick off swamp: the pace a room's crossing is
    /// priced at.
    const PACE: Mover = Mover {
        footprint: Footprint::One,
        factor: FatigueFactor {
            weight: 1,
            moves: 1,
        },
    };

    const EXITS: [i32; 4] = [tiles::TOP, tiles::RIGHT, tiles::BOTTOM, tiles::LEFT];

    /// Whether `room` is a keeper room crossed blind, and the ticks across it
    /// from its `entry` side out its `exit`, never under a crossing's: priced
    /// round the tiles a keeper strikes, two crossings out of sight with
    /// lairs, as cartographers price it.
    fn crossing(&self, grid: Option<&RoomGrid>, room: Coords, entry: i32, exit: i32) -> (i32, i32) {
        let flood = grid
            .filter(|g| g.struck.is_some())
            .and_then(|g| g.floods.get(&FloodKey::of(Atlas::PACE, Goal::Exit(exit))));
        let lairs = || (self.lairs.get(&room).copied()).unwrap_or(tiles::is_keeper_room(room));
        let ticks = Atlas::crossing_ticks();
        match flood {
            _ if entry == 0 => (0, ticks),
            Some(dist) => {
                let entered = tiles::exit_tiles(entry).iter().map(|t| dist[*t]);
                (0, entered.min().map_or(UNREACHED, |d| d.max(ticks)))
            }
            None if grid.is_none() && lairs() => (1, 2 * ticks),
            None => (0, ticks),
        }
    }

    /// Over world coordinates, so a room met is named only on the route out.
    /// Fewest blind keeper rooms first, since a guard may stand on the tile a
    /// blind entry lands on, then cheapest; `max_hops` crossings its budget;
    /// equal costs in the order met, so on plain rooms it is the hop search.
    fn search(&self, avoid: &BTreeSet<String>, max_hops: i32, from: &str, onto: &str) -> Tree {
        let mut parent: HashMap<Coords, Coords> = HashMap::default();
        let Some(start) = tiles::room_coords(from) else {
            return Tree(parent);
        };
        let onto = tiles::room_coords(onto);
        let avoid: Vec<Coords> = avoid.iter().filter_map(|r| tiles::room_coords(r)).collect();
        let grids: HashMap<Coords, &RoomGrid> = self
            .rooms
            .iter()
            .filter_map(|(name, grid)| Some((tiles::room_coords(name)?, grid)))
            .collect();
        let budget = max_hops * Atlas::crossing_ticks();

        let mut cost: HashMap<Coords, (i32, i32)> = HashMap::default();
        let mut queue = BinaryHeap::new();
        let mut met = 0;
        parent.insert(start, start);
        cost.insert(start, (0, 0));
        queue.push(Reverse(((0, 0), met, start)));

        while let Some(Reverse((spent, _, room))) = queue.pop() {
            if Some(room) == onto {
                break;
            }
            if spent > cost[&room] {
                continue;
            }
            let back = parent[&room];
            let entry = Atlas::EXITS
                .into_iter()
                .find(|e| (room.0 + tiles::dx(*e), room.1 + tiles::dy(*e)) == back)
                .unwrap_or(0);
            let grid = grids.get(&room).copied();
            for exit in Atlas::EXITS {
                let there = (room.0 + tiles::dx(exit), room.1 + tiles::dy(exit));
                if exit == entry
                    || self.closed.contains(&there)
                    || (avoid.contains(&there) && Some(there) != onto)
                {
                    continue;
                }
                let (blind, ticks) = self.crossing(grid, room, entry, exit);
                let total = (spent.0 + blind, spent.1.saturating_add(ticks));
                if total.1 <= budget
                    && cost.get(&there).is_none_or(|c| total < *c)
                    && Atlas::linked(grid, grids.get(&there).copied(), exit)
                {
                    met += 1;
                    cost.insert(there, total);
                    parent.insert(there, room);
                    queue.push(Reverse((total, met, there)));
                }
            }
        }

        Tree(parent)
    }

    pub fn tree(&self, max_hops: i32, from: &str) -> Tree {
        self.search(&BTreeSet::new(), max_hops, from, "")
    }

    pub fn route(
        &self,
        avoid: &BTreeSet<String>,
        max_hops: i32,
        from: &str,
        onto: &str,
    ) -> Option<Vec<String>> {
        if from != onto
            && (tiles::room_coords(from).is_none() || tiles::room_coords(onto).is_none())
        {
            None
        } else {
            self.search(avoid, max_hops, from, onto).route(from, onto)
        }
    }

    fn exit_goals(footprint: Footprint, weights: &[Weight; tiles::COUNT], exit: i32) -> Vec<Tile> {
        match footprint {
            Footprint::One => tiles::exit_tiles(exit)
                .iter()
                .filter(|tile| weights[**tile].is_some())
                .copied()
                .collect(),
            Footprint::Quad => {
                let inner = tiles::SIDE - 2;
                let mut goals = Vec::new();
                for i in 1..inner - 1 {
                    let anchor = match exit {
                        1 => tiles::index(i, 0),
                        3 => tiles::index(inner, i),
                        5 => tiles::index(i, inner),
                        _ => tiles::index(0, i),
                    };
                    if weight_of(Footprint::Quad, weights, anchor).is_some() {
                        goals.push(anchor);
                    }
                }
                goals
            }
        }
    }

    fn field_on(
        &mut self,
        mover: Mover,
        target: &Target,
        room: &str,
        onward: impl FnOnce(&Atlas) -> Option<Vec<String>>,
    ) -> Option<Field> {
        if mover.factor.moves <= 0 || !self.rooms.contains_key(room) {
            return None;
        }
        let footprint = mover.footprint;
        let goal = if room == target.room {
            let mut targets = target.tiles.clone();
            targets.sort_unstable();
            targets.dedup();
            Goal::Seats {
                range: target.range,
                tiles: targets,
            }
        } else {
            match onward(self) {
                Some(rooms) if rooms.len() >= 2 => Goal::Exit(tiles::exit_toward(room, &rooms[1])?),
                _ => return None,
            }
        };

        let grid = self.rooms.get_mut(room).unwrap();
        let (dist, kept) = grid.flood(mover, goal);
        if kept {
            self.hits += 1;
        } else {
            self.misses += 1;
        }

        Some(Field {
            footprint,
            weights: grid.weights.clone(),
            factor: mover.factor,
            dist,
            struck: grid.struck.clone(),
        })
    }

    pub fn field(&mut self, mover: Mover, target: &Target, room: &str) -> Option<Field> {
        self.field_on(mover, target, room, |atlas| {
            atlas.route(&target.avoid, target.max_hops, room, &target.room)
        })
    }

    /// `field` out of another room on a known route to the target's.
    pub fn field_on_route(
        &mut self,
        mover: Mover,
        target: &Target,
        rooms: &[String],
    ) -> Option<Field> {
        let first = rooms.first()?.clone();
        let rooms = rooms.to_vec();
        self.field_on(mover, target, &first, |_| Some(rooms))
    }

    /// In another room: the ticks to leaving this one toward it.
    pub fn travel_ticks(&mut self, mover: Mover, target: &Target, from: &Pos) -> Option<i32> {
        self.field(mover, target, &from.room)
            .and_then(|field| field.ticks(tiles::of_pos(from)))
    }

    pub fn entry_ticks(&mut self, mover: Mover, target: &Target, from: &str) -> Option<i32> {
        let exit = tiles::exit_toward(&target.room, from);
        match (self.field(mover, target, &target.room), exit) {
            (Some(field), Some(exit)) => {
                let mut least = UNREACHED;
                for &tile in tiles::exit_tiles(exit) {
                    if let Some(d) = field.ticks(tile)
                        && d < least
                    {
                        least = d;
                    }
                }
                if least == UNREACHED {
                    None
                } else {
                    Some(least)
                }
            }
            _ => None,
        }
    }

    pub fn crossing_ticks() -> i32 {
        engine::ROOM_SIDE
    }

    pub fn ticks_to(
        &mut self,
        route: impl Fn(&Target, &str) -> Option<Vec<String>>,
        mover: Mover,
        target: &Target,
        from: &Pos,
    ) -> Option<i32> {
        if from.room == target.room {
            self.travel_ticks(mover, target, from)
        } else {
            let rooms = route(target, &from.room)?;
            let field = self.field_on_route(mover, target, &rooms)?;
            let leg = field.ticks(tiles::of_pos(from))?;
            let n = rooms.len();
            let far = self
                .entry_ticks(mover, target, &rooms[n - 2])
                .unwrap_or(Atlas::crossing_ticks());
            Some(leg + Atlas::crossing_ticks() * (n as i32 - 2) + far)
        }
    }

    pub fn first_step(&mut self, mover: Mover, target: &Target, from: &Pos) -> Option<i32> {
        self.field(mover, target, &from.room)
            .and_then(|field| field.step(tiles::of_pos(from)))
    }
}

pub fn owner_is_ours(owner: &Owner) -> bool {
    owner.is_ours()
}
