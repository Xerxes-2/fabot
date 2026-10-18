//! The floods are the one cache (`ARCHITECTURE.md` §1). Built twice from one
//! `World`, an Atlas answers identically: a kept flood is one on equal weights.
use std::collections::BTreeSet;

use crate::core::HashMap;
use std::rc::Rc;

pub mod grid;
pub mod tiles;

use self::grid::{
    FatigueFactor, Footprint, UNREACHED, Weight, flood, on_edge, step_table, step_ticks, weight_of,
};
use self::tiles::{Table, Tile};
use super::engine;
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
}

#[derive(Clone, Debug)]
pub struct Atlas {
    pub rooms: HashMap<String, RoomGrid>,
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

    pub fn ticks(&self, tile: Tile) -> Option<i32> {
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

    pub fn step(&self, tile: Tile) -> Option<i32> {
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
            misses: 0,
            hits: 0,
        }
    }

    /// Past a keeper's pin, its ranged reach and a step each, its and ours, a
    /// tile is out of its strike.
    const KEEPER_MARGIN: i32 = engine::KEEPER_PIN + engine::RANGED_RANGE + 2;

    /// ~10 kB each. A dropped resource's tile is a new goal, so a room's keys
    /// are unbounded; past this the room floods afresh.
    const KEPT_FLOODS: usize = 500;

    fn same(a: &[Weight; tiles::COUNT], b: &[Weight; tiles::COUNT]) -> bool {
        a == b
    }

    /// A room whose terrain buffer is not a whole room has no grid: routed by
    /// name, never walked.
    pub fn following(previous: &Atlas, world: &World) -> Atlas {
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

            if room.structures.iter().any(|s| s.kind.is_keeper_lair()) {
                let lairs = room.structures.iter().filter(|s| s.kind.is_keeper_lair());
                // A keeper is cast on its lair and pinned only to its rock.
                let lairs = lairs.map(|s| (&s.pos, Atlas::KEEPER_MARGIN - engine::KEEPER_PIN));
                let rocks = room.sources.iter().map(|s| &s.pos);
                let rocks = rocks.chain(room.minerals.iter().map(|m| &m.pos));
                for (pos, margin) in lairs.chain(rocks.map(|p| (p, Atlas::KEEPER_MARGIN))) {
                    for tile in tiles::within(margin, tiles::of_pos(pos)) {
                        weights[tile] = None;
                    }
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

            rooms.insert(room.name.clone(), RoomGrid { weights, floods });
        }

        Atlas {
            rooms,
            misses: 0,
            hits: 0,
        }
    }

    pub fn of_world(world: &World) -> Atlas {
        Atlas::following(&Atlas::empty(), world)
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

    pub fn passable(&self, room: &str, tile: Tile) -> bool {
        self.rooms
            .get(room)
            .is_some_and(|grid| grid.weights[tile].is_some())
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

    /// Over world coordinates, so a room met is named only on the route out.
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

        parent.insert(start, start);
        let mut frontier = vec![start];
        let mut hops = 0;

        while !frontier.is_empty()
            && hops < max_hops
            && !onto.is_some_and(|o| parent.contains_key(&o))
        {
            let mut next = Vec::new();
            for &room in &frontier {
                for e in 0..4 {
                    let exit = tiles::TOP + 2 * e;
                    let there = (room.0 + tiles::dx(exit), room.1 + tiles::dy(exit));
                    if !parent.contains_key(&there)
                        && (Some(there) == onto
                            || !(avoid.contains(&there) || tiles::is_keeper_room(there)))
                        && Atlas::linked(
                            grids.get(&room).copied(),
                            grids.get(&there).copied(),
                            exit,
                        )
                    {
                        parent.insert(there, room);
                        next.push(there);
                    }
                }
            }
            frontier = next;
            hops += 1;
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

        let factor = mover.factor;
        let key = FloodKey {
            footprint,
            steps: [
                engine::ROAD_WEIGHT,
                engine::PLAIN_WEIGHT,
                engine::SWAMP_WEIGHT,
            ]
            .map(|w| step_ticks(factor, w)),
            goal,
        };

        let grid = self.rooms.get_mut(room).unwrap();
        let dist = if let Some(dist) = grid.floods.get(&key) {
            self.hits += 1;
            dist.clone()
        } else {
            self.misses += 1;
            let goals = match &key.goal {
                Goal::Seats {
                    range,
                    tiles: targets,
                } => {
                    let mut goals = Vec::new();
                    for &tile in targets {
                        if tile.index() < tiles::COUNT {
                            Atlas::seats_near(footprint, &grid.weights, *range, tile, &mut goals);
                        }
                    }
                    goals
                }
                Goal::Exit(exit) => Atlas::exit_goals(footprint, &grid.weights, *exit),
            };
            let dist = Rc::new(flood(footprint, &grid.weights, &step_table(factor), &goals));
            grid.floods.insert(key, dist.clone());
            dist
        };

        Some(Field {
            footprint,
            weights: grid.weights.clone(),
            factor,
            dist,
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
