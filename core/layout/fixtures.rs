//! The rooms Layout's suites draw their cases from: walled blobs and swamp over
//! open ground, with a few walls of another owner, one or two sources, a
//! mineral, a controller and none, one or two spawns of ours.
use std::collections::BTreeSet;

use fabot_base::{
    declarations::{ColonyPlan, Stage},
    tiles::{self, Tile},
    vocabulary::ResourceType,
    world::{
        Built, Controller, Creep, Mineral, Owner, Pos, Room, RoomName, Rules, Site, Source, Spawn,
        Store, Structure, StructureType, World, terrain,
    },
};

use crate::{
    ground::{self, Ground},
    plan,
    shape::Plan,
};

pub const ROOM: RoomName = RoomName::W1N1;

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

pub fn structure(id: &str, kind: StructureType, owner: Owner, tile: Tile) -> Structure {
    let walkable = kind.walkable();

    Structure {
        id: id.into(),
        kind,
        pos: at(tile),
        owner,
        hits: 1,
        hits_max: 1,
        store: Store::default(),
        cooldown: 0,
        collapse: 0,
        deploy: 0,
        spawn: None,
        level: 0,
        walkable,
    }
}

/// A generated colony room; the spawns of ours standing in it are not read.
#[derive(Clone)]
pub struct Colony {
    pub room: Room<'static>,
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
    let spawn_tiles = slice(&inner, (sources + 2) as usize, (sources + 1 + spawns) as usize);
    let foreign_tiles =
        slice(&inner, (sources + 2 + spawns) as usize, (sources + 1 + spawns + foreign) as usize);

    let mut structures: Vec<Structure> = spawn_tiles
        .iter()
        .enumerate()
        .map(|(i, t)| structure(&format!("p{i}"), StructureType::Spawn, Owner::Ours, *t))
        .collect();
    structures.extend(foreign_tiles.iter().enumerate().map(|(i, t)| {
        structure(&format!("w{i}"), StructureType::Spawn, Owner::Stranger("x".into()), *t)
    }));

    // The engine stands rocks and the controller on wall.
    for t in source_tiles.iter().chain([&controller, &mineral]) {
        terrain[*t] = terrain::WALL;
    }
    let room = Room {
        terrain: terrain::leak(terrain),
        controller: Some(Controller {
            id: "c".into(),
            pos: at(controller),
            owner: Owner::Ours,
            level: 8,
            progress: 0,
            progress_total: 0,
            ticks_to_downgrade: 0,
            reservation: None,
            sign: None,
            safe_mode: 0,
            safe_mode_available: 0,
            safe_mode_cooldown: 0,
            upgrade_blocked: 0,
        }),
        sources: source_tiles
            .iter()
            .enumerate()
            .map(|(i, t)| Source {
                id: format!("s{i}").as_str().into(),
                pos: at(*t),
                energy: 0,
                energy_capacity: 0,
                ticks_to_regeneration: 0,
            })
            .collect(),
        minerals: vec![Mineral {
            id: "m".into(),
            pos: at(mineral),
            resource: None,
            amount: 0,
            density: 0,
        }],
        structures,
        ..Room::empty(ROOM)
    };

    Some(Colony { room })
}

/// Open rooms and cramped ones, where no square of clearance 5 is left.
pub fn colony(rng: &mut Rng) -> Colony {
    loop {
        let blobs = if rng.choose(0, 1) == 0 { rng.choose(0, 8) } else { rng.choose(40, 90) };

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
    let plan = plan::of_ground(&Rules::vanilla(), &ground)?;
    Some((ground, plan))
}

/// Ground whose quad breakers, placed as Hivemind places them, close a pocket
/// of cut ramparts in against the terrain (`sim --gen 1145141`'s home).
pub fn pocketed() -> Ground {
    let rows: [&str; 50] = [
        "11100000000000000001111111111111111111111111111111",
        "11100000000000000000111111111100111111111111111111",
        "11000000000000000000111110011000011111111111111011",
        "11000000000000000001110000000020000111111111112221",
        "10000000000002110011111111000000000011111111110220",
        "10000000110002111111110000000000000111111111100220",
        "11000001111000111111110000000000000011111000000000",
        "11000001111000111111110000000000000011110000000000",
        "11000001110220011111110000002200000000110000000000",
        "11000001110000000011110000002001110000011000000000",
        "10022201111100000011110000000011111000011000000000",
        "10220211111110000111100000000011111100011000100000",
        "10200111111110001111000000000001111110111001110000",
        "00000111111000011111000000000000111111110001111000",
        "00000000110000011111100011111100011111100000111000",
        "00000000000000011111111111111110001111200000020000",
        "00000000000000011111111111111110001110200000020000",
        "00000000000022011111111111111100000000000000000000",
        "00001000000000111111111111000000000000000000000000",
        "00011100000000111102000000000010000001111100000000",
        "00011102200000111222000020000011000011111111011000",
        "00011100000001111022000020000011000111111111111100",
        "00011200000001111000000020000001111111001111111100",
        "00000000011001111011110220000001111110000111111100",
        "00000022011000111111111220000001111111000111111100",
        "00000002011100011111111200000001111111100002211100",
        "10000000021110021111111000000001111000000000011100",
        "11100000021111000111111001110000110000000000111100",
        "11110000001111000111111111110000000000220001111100",
        "11111000001111000011111111111000000000001111111100",
        "11111100000110000001100111111000000000011111111100",
        "11111111000000000000000000011000000000111111111000",
        "11111111111000000000000000000000000020111100000000",
        "11100111111122000000000111000000000000111100000000",
        "11100111111110000000000111100000000000011110002000",
        "11000011111111111110000111110000000000001111000000",
        "11000011111111111111000111100001100000000111110000",
        "11000001111111111111000011100011100000000011111100",
        "11000000000111111112200011110111100000000001111100",
        "11000000000011110000200011111111100000000000111000",
        "11000000000001120000022011111111000000000000010000",
        "10000000000000222000000011111110000000000000000000",
        "10000000000002200000000001111100000000000000000000",
        "10000000000000011000000000111100000011100000000000",
        "10000000011000111000000000011100000111100000000000",
        "10000001111100011000220000111100001111112220000000",
        "11001111111100000000220001111100011111112200000001",
        "11111111111000222000000011111111110011110000000001",
        "11111100000000020000001111111111100000000000000011",
        "11111100000000000111111111111111100000000000011111",
    ];
    let at = |x, y| tiles::index(x, y);
    Ground {
        room: ROOM,
        terrain: rows.concat().bytes().map(|c| c - b'0').collect(),
        sources: vec![at(28, 17), at(10, 23)],
        minerals: vec![at(25, 33)],
        controller: Some(at(6, 35)),
    }
}

/// `pocketed` with its ore in a nook: the post its one open side.
pub fn nook() -> Ground {
    let mut ground = pocketed();
    for (x, y) in [(24, 32), (25, 32), (26, 33)] {
        ground.terrain[tiles::index(x, y)] = 1;
    }
    ground
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

/// The colony with its lists in another order.
pub fn shuffled(colony: &Colony, seed: u64) -> Colony {
    let mut rng = Rng::new(seed);
    let mut next = colony.clone();
    rng.shuffle(&mut next.room.sources);
    rng.shuffle(&mut next.room.minerals);
    rng.shuffle(&mut next.room.structures);
    next
}

pub fn same(a: &Plan, b: &Plan) -> bool {
    a.anchor == b.anchor
        && a.items.len() == b.items.len()
        && a.items
            .iter()
            .zip(b.items.iter())
            .all(|(x, y)| x.kind == y.kind && x.tile == y.tile && x.level == y.level)
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
        budget: rng.choose(0, fabot_base::engine::MAX_CONSTRUCTION_SITES),
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
            fights: false,
            pile: None,
            muster: None,
        })
        .collect()
}

pub fn site(id: &str, kind: StructureType, tile: Tile) -> Site {
    Site { id: id.into(), kind, pos: at(tile), owner: Owner::Ours, progress: 0, progress_total: 1 }
}

pub fn creep(id: &str, owner: Owner, hits: i32, tile: Tile) -> Creep {
    Creep {
        id: id.into(),
        name: id.to_string(),
        owner,
        pos: at(tile),
        body: vec![fabot_base::world::BodyPart {
            kind: fabot_base::vocabulary::Part::Attack,
            hits: 100,
            boost: None,
        }],
        hits,
        hits_max: 100,
        ticks_to_live: 1000,
        fatigue: 0,
        store: Store::default(),
    }
}

pub fn tower(id: &str, tile: Tile, energy: i32) -> Structure {
    Structure {
        store: Store { held: vec![(ResourceType::Energy, energy)], capacity: 1000 },
        ..structure(id, StructureType::Tower, Owner::Ours, tile)
    }
}

/// A body that holds its tile while it works.
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Holder {
    Harvester,
    Digger,
    Upgrader,
    Outpost,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum End {
    Spawn,
    Source,
    Controller,
    Exit,
    Road,
}

/// An end is reached where a tile within its range of one of its tiles is.
pub type Ends = Vec<(End, Vec<Tile>, i32)>;

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub enum Cause {
    /// Apart on the bare ground: no plan joins them.
    Terrain,
    Structure,
    Held(Holder),
    /// Freed only with more than one kind of holder gone.
    Holders,
}

/// The ends a walk from the first end misses over `open` with every holder's
/// tile taken, each with what cuts it off; `ground` is `open` with nothing built.
pub fn cut_off(
    (ground, open): (&[bool], &[bool]),
    ends: &Ends,
    holders: &[(Holder, Tile)],
) -> Vec<(End, Tile, Cause)> {
    let missed = |open: &[bool], free: &dyn Fn(Holder) -> bool| {
        let mut o = open.to_vec();
        holders.iter().filter(|(h, _)| !free(*h)).for_each(|(_, t)| o[*t] = false);
        let near = |(_, at, range): &(End, Vec<Tile>, i32)| {
            at.iter().flat_map(|t| tiles::within(*range, *t)).collect::<Vec<_>>()
        };
        let steps = tiles::reach(near(&ends[0]).into_iter().filter(|t| o[*t]), |t| o[t]);
        (ends.iter())
            .filter(|e| near(e).iter().all(|n| !o[*n] || steps[*n] == tiles::UNREACHED))
            .map(|(end, at, _)| (*end, at[0]))
            .collect::<Vec<_>>()
    };
    let kinds = [Holder::Harvester, Holder::Digger, Holder::Upgrader, Holder::Outpost];
    let (bare, all) = (missed(ground, &|_| true), missed(open, &|_| true));
    let cause = |miss: &(End, Tile)| {
        let one = kinds.into_iter().find(|h| !missed(open, &|k| k == *h).contains(miss));
        match one {
            _ if bare.contains(miss) => Cause::Terrain,
            Some(h) => Cause::Held(h),
            None if all.contains(miss) => Cause::Structure,
            None => Cause::Holders,
        }
    };
    (missed(open, &|_| false).iter()).map(|miss| (miss.0, miss.1, cause(miss))).collect()
}

/// Each side's runs of open border tiles.
pub fn exit_groups(open: &[bool]) -> Vec<Vec<Tile>> {
    let mut groups: Vec<Vec<Tile>> = Vec::new();
    for side in [tiles::TOP, tiles::RIGHT, tiles::BOTTOM, tiles::LEFT] {
        let mut run = false;
        for &t in tiles::exit_tiles(side) {
            match (open[t], run) {
                (false, _) => run = false,
                (true, false) => {
                    groups.push(vec![t]);
                    run = true;
                }
                (true, true) => groups.last_mut().expect("a run").push(t),
            }
        }
    }
    groups
}

/// Road tiles with one road beside them or none.
pub fn road_ends(roads: &[Tile]) -> Vec<Tile> {
    let beside = |t: Tile| roads.iter().filter(|r| tiles::range(**r, t) == 1).count();
    roads.iter().copied().filter(|t| beside(*t) <= 1).collect()
}
