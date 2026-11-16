//! The rooms Layout's suites draw their cases from: walled blobs and swamp over
//! open ground, with a few walls of another owner, one or two sources, a
//! mineral, a controller and none, one or two spawns.
use std::collections::BTreeSet;

use crate::layout::ground::{self, Ground};
use crate::layout::plan;
use crate::layout::shape::Item;
use crate::layout::shape::Plan;
use crate::tick::{ColonyPlan, Stage};
use crate::tiles::{self, Tile};
use crate::vocabulary::Resource;
use crate::world::{
    Controller, Creep, Kind, Mineral, Owner, Pos, Room, RoomName, Rules, Site, Source, Spawn,
    Store, Structure, World, terrain,
};

pub const ROOM: RoomName = RoomName::named("W1N1");

/// `FsCheckConfig.maxTest`: the samples each property runs over.
pub const MAX_TEST: usize = 150;

/// A deterministic stream in FsCheck's place: the cases are the same shape, not
/// the same bits.
pub struct Rng(u64);

impl Rng {
    pub fn new(seed: u64) -> Rng {
        Rng(seed | 1)
    }

    fn word(&mut self) -> u32 {
        let mut x = self.0;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.0 = x;
        (x >> 32) as u32
    }

    /// `lo..=hi`.
    pub fn choose(&mut self, lo: i32, hi: i32) -> i32 {
        let span = (hi - lo + 1) as u32;
        lo + (self.word() % span) as i32
    }

    pub fn elements<T: Copy>(&mut self, items: &[T]) -> T {
        items[(self.word() % items.len() as u32) as usize]
    }

    pub fn shuffle<T>(&mut self, items: &mut [T]) {
        for i in (1..items.len()).rev() {
            let j = (self.word() % (i as u32 + 1)) as usize;
            items.swap(i, j);
        }
    }
}

pub fn at(tile: Tile) -> Pos {
    tiles::to_pos(ROOM, tile)
}

pub fn structure(id: &str, kind: Kind, owner: Owner, tile: Tile) -> Structure {
    let walkable = kind.walkable();

    Structure {
        id: id.to_string(),
        kind,
        pos: at(tile),
        owner,
        hits: 1,
        hits_max: 1,
        store: Store::empty(),
        cooldown: 0,
        collapse: 0,
        spawn: None,
        level: 0,
        walkable,
    }
}

/// A generated colony room and the tiles of the spawns standing in it.
#[derive(Clone)]
pub struct Colony {
    pub room: Room<'static>,
    pub spawns: Vec<Tile>,
}

fn slice(picks: &[Tile], a: usize, b: usize) -> Vec<Tile> {
    // `picks.[a..b]` clamps at the array's end and is empty when `b < a`.
    if a >= picks.len() || b < a {
        return Vec::new();
    }
    picks[a..=b.min(picks.len() - 1)].to_vec()
}

fn colony_of(rng: &mut Rng, blobs: i32) -> Option<Colony> {
    let mut terrain = vec![0u8; tiles::COUNT];

    for _ in 0..blobs {
        let x = rng.choose(0, 49);
        let y = rng.choose(0, 49);
        let w = rng.choose(1, 12);
        let h = rng.choose(1, 12);
        let swamp = rng.elements(&[true, false]);

        for ty in y..=(y + h).min(tiles::SIDE - 1) {
            for tx in x..=(x + w).min(tiles::SIDE - 1) {
                terrain[tiles::index(tx, ty)] = if swamp { terrain::SWAMP } else { terrain::WALL };
            }
        }
    }

    let edge = rng.elements(&[true, false]);

    if edge {
        for i in 0..tiles::SIDE {
            terrain[tiles::index(i, 0)] = terrain::WALL;
            terrain[tiles::index(0, i)] = terrain::WALL;
        }
    }

    let mut inner: Vec<Tile> = Vec::new();

    for t in tiles::all() {
        let x = tiles::x_of(t);
        let y = tiles::y_of(t);

        if terrain[t] & terrain::WALL == 0 && x >= 3 && y >= 3 && x <= 46 && y <= 46 {
            inner.push(t);
        }
    }

    if inner.len() < 8 {
        return None;
    }

    rng.shuffle(&mut inner);

    let sources = rng.choose(1, 2);
    let spawns = rng.choose(0, 2);
    let foreign = rng.choose(0, 3);

    let source_tiles = slice(&inner, 0, (sources - 1) as usize);
    let controller = inner[sources as usize];
    let mineral = inner[sources as usize + 1];
    let spawn_tiles = slice(
        &inner,
        (sources + 2) as usize,
        (sources + 1 + spawns) as usize,
    );
    let foreign_tiles = slice(
        &inner,
        (sources + 2 + spawns) as usize,
        (sources + 1 + spawns + foreign) as usize,
    );

    let mut structures: Vec<Structure> = spawn_tiles
        .iter()
        .enumerate()
        .map(|(i, t)| structure(&format!("p{i}"), Kind::Spawn, Owner::Ours, *t))
        .collect();
    structures.extend(
        foreign_tiles
            .iter()
            .enumerate()
            .map(|(i, t)| structure(&format!("w{i}"), Kind::Wall, Owner::Nobody, *t)),
    );

    let room = Room {
        name: ROOM,
        terrain: terrain::leak(terrain),
        controller: Some(Controller {
            id: "c".to_string(),
            pos: at(controller),
            owner: Owner::Ours,
            level: 8,
            progress: 0,
            progress_total: 0,
            ticks_to_downgrade: 0,
            reservation: None,
            safe_mode: 0,
            safe_mode_available: 0,
            safe_mode_cooldown: 0,
            upgrade_blocked: 0,
        }),
        energy_available: 0,
        energy_capacity: 0,
        sources: source_tiles
            .iter()
            .enumerate()
            .map(|(i, t)| Source {
                id: format!("s{i}"),
                pos: at(*t),
                energy: 0,
                energy_capacity: 0,
                ticks_to_regeneration: 0,
            })
            .collect(),
        minerals: vec![Mineral {
            id: "m".to_string(),
            pos: at(mineral),
            resource: Resource::Energy,
            amount: 0,
        }],
        structures,
        sites: Vec::new(),
        piles: Vec::new(),
        remains: Vec::new(),
        others: Vec::new(),
    };

    Some(Colony {
        room,
        spawns: spawn_tiles,
    })
}

/// Open rooms and cramped ones, where no square of clearance 5 is left.
pub fn colony(rng: &mut Rng) -> Colony {
    loop {
        let blobs = if rng.choose(0, 1) == 0 {
            rng.choose(0, 8)
        } else {
            rng.choose(40, 90)
        };

        if let Some(colony) = colony_of(rng, blobs) {
            return colony;
        }
    }
}

pub fn samples(seed: u64) -> impl Iterator<Item = Colony> {
    let mut rng = Rng::new(seed);
    std::iter::from_fn(move || Some(colony(&mut rng)))
}

pub fn plan_of(colony: &Colony) -> Option<(Ground, Plan)> {
    let ground = ground::of_room(&colony.room)?;
    let plan = plan::of_ground(&Rules::vanilla(), &ground, &colony.spawns)?;
    Some((ground, plan))
}

pub fn walkable(kind: &Kind) -> bool {
    kind.is_road() || kind.is_container() || kind.is_rampart()
}

pub fn count_of(kind: &Kind, items: &[Item]) -> i32 {
    items
        .iter()
        .filter(|i| i.kind.name() == kind.name())
        .count() as i32
}

pub fn distinct<T: PartialEq + Copy>(values: &[T]) -> Vec<T> {
    let mut out: Vec<T> = Vec::new();
    for v in values {
        if !out.contains(v) {
            out.push(*v);
        }
    }
    out
}

/// The tiles reached on foot from the anchor, every planned structure no creep
/// crosses standing.
pub fn reached(ground: &Ground, plan: &Plan) -> Vec<bool> {
    let mut blocked: Vec<bool> = (0..tiles::COUNT)
        .map(|t| ground.weights[t].is_none())
        .collect();

    for i in &plan.items {
        if !walkable(&i.kind) {
            blocked[i.tile] = true;
        }
    }

    let mut seen = vec![false; tiles::COUNT];
    let mut queue: std::collections::VecDeque<Tile> = std::collections::VecDeque::new();
    seen[plan.anchor] = true;
    queue.push_back(plan.anchor);

    while let Some(t) = queue.pop_front() {
        for &n in tiles::around(t) {
            if !seen[n] && !blocked[n] {
                seen[n] = true;
                queue.push_back(n);
            }
        }
    }

    seen
}

/// The colony with its lists in another order.
pub fn shuffled(colony: &Colony, seed: u64) -> Colony {
    let mut rng = Rng::new(seed);
    let mut next = colony.clone();
    rng.shuffle(&mut next.room.sources);
    rng.shuffle(&mut next.room.minerals);
    rng.shuffle(&mut next.room.structures);
    next
}

/// The colony with no spawn standing.
pub fn bare(colony: &Colony) -> Colony {
    let mut next = colony.clone();
    next.spawns = Vec::new();
    next.room.structures.retain(|s| !s.kind.is_spawn());
    next
}

pub fn same(a: &Plan, b: &Plan) -> bool {
    a.anchor == b.anchor
        && a.items.len() == b.items.len()
        && a.items
            .iter()
            .zip(b.items.iter())
            .all(|(x, y)| x.kind.name() == y.kind.name() && x.tile == y.tile && x.level == y.level)
}

/// Our structures and sites standing on some of the plan's tiles, at a level.
pub struct CensusCase {
    pub colony: Colony,
    pub level: i32,
    pub built: Vec<i32>,
    pub pending: Vec<i32>,
    pub budget: i32,
}

pub fn census_case(rng: &mut Rng) -> CensusCase {
    let colony = colony(rng);
    let level = rng.choose(1, 8);

    let list = |rng: &mut Rng| -> Vec<i32> {
        let n = rng.choose(0, 4);
        (0..n).map(|_| rng.choose(0, 400)).collect()
    };

    CensusCase {
        colony,
        level,
        built: list(rng),
        pending: list(rng),
        budget: rng.choose(0, crate::engine::MAX_CONSTRUCTION_SITES),
    }
}

pub fn world_of(room: Room, spawns: Vec<Spawn>, creeps: Vec<Creep>) -> World {
    World {
        time: 1,
        me: String::new(),
        cpu_limit: 20.0,
        bucket: 10_000,
        rooms: vec![room],
        spawns,
        creeps,
        attacks: Vec::new(),
        destroyed: Vec::new(),
        our_sites: 0,
        rooms_allowed: 1,
        closed: Default::default(),
        rules: Rules::vanilla(),
    }
}

pub fn exploited(owned: &BTreeSet<RoomName>) -> Vec<ColonyPlan> {
    (owned.iter())
        .map(|room| ColonyPlan {
            room: *room,
            stage: Stage::Exploit,
            target: 8,
            reserve: 0,
            rampart_hits: 0,
            ring: false,
            owes: false,
            pile: None,
            muster: None,
        })
        .collect()
}

pub fn spawn_named(id: &str, tile: Tile) -> Spawn {
    Spawn {
        id: id.to_string(),
        name: id.to_string(),
        pos: at(tile),
        spawning: None,
    }
}

pub fn site(id: &str, kind: Kind, tile: Tile) -> Site {
    Site {
        id: id.to_string(),
        kind,
        pos: at(tile),
        owner: Owner::Ours,
        progress: 0,
        progress_total: 1,
    }
}

pub fn creep(id: &str, owner: Owner, hits: i32, tile: Tile) -> Creep {
    Creep {
        id: id.to_string(),
        name: id.to_string(),
        owner,
        pos: at(tile),
        body: vec![crate::world::BodyPart {
            kind: crate::vocabulary::Part::Attack,
            hits: 100,
            boost: None,
        }],
        hits,
        hits_max: 100,
        ticks_to_live: 1000,
        fatigue: 0,
        store: Store::empty(),
    }
}

pub fn tower(id: &str, tile: Tile, energy: i32) -> Structure {
    Structure {
        store: Store {
            held: vec![(Resource::Energy, energy)],
            capacity: 1000,
        },
        ..structure(id, Kind::Tower, Owner::Ours, tile)
    }
}
