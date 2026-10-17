use std::cmp::Ordering;
use std::collections::BTreeMap;

use super::engine;
use super::vocabulary::{BodyPart, Boost, Resource};

/// Compared by hand: Fable's record compare walks every field, per tile per tick.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Pos {
    pub room: String,
    pub x: i32,
    pub y: i32,
}

impl Pos {
    pub fn new(room: impl Into<String>, x: i32, y: i32) -> Pos {
        Pos {
            room: room.into(),
            x,
            y,
        }
    }

    pub fn range(&self, b: &Pos) -> Option<i32> {
        if self.room == b.room {
            Some((self.x - b.x).abs().max((self.y - b.y).abs()))
        } else {
            None
        }
    }

    pub fn within(&self, range: i32, b: &Pos) -> bool {
        self.room == b.room && (self.x - b.x).abs().max((self.y - b.y).abs()) <= range
    }
}

impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Pos {
    fn cmp(&self, other: &Self) -> Ordering {
        match self.room.cmp(&other.room) {
            Ordering::Equal => (self.y * 50 + self.x).cmp(&(other.y * 50 + other.x)),
            c => c,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Owner {
    Ours,
    Theirs(String),
    Nobody,
}

impl Owner {
    pub fn is_ours(&self) -> bool {
        *self == Owner::Ours
    }

    pub fn is_theirs(&self) -> bool {
        matches!(self, Owner::Theirs(_))
    }

    pub fn is_nobody(&self) -> bool {
        *self == Owner::Nobody
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Store {
    pub held: Vec<(Resource, i32)>,
    /// Over every resource; 0 for no store.
    pub capacity: i32,
}

impl Store {
    pub fn empty() -> Store {
        Store {
            held: Vec::new(),
            capacity: 0,
        }
    }

    pub fn amount_of(&self, resource: &Resource) -> i32 {
        self.held
            .iter()
            .filter(|(r, _)| r == resource)
            .map(|(_, amount)| *amount)
            .sum()
    }

    pub fn used(&self) -> i32 {
        self.held.iter().map(|(_, amount)| *amount).sum()
    }

    pub fn free(&self) -> i32 {
        (self.capacity - self.used()).max(0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Kind {
    Spawn,
    Extension,
    Tower,
    Road,
    Container,
    Storage,
    Link,
    Rampart,
    Wall,
    Extractor,
    Terminal,
    Observer,
    Lab,
    Factory,
    PowerSpawn,
    Nuker,
    KeeperLair,
    InvaderCore,
    PowerBank,
    Portal,
    Other(String),
}

impl Kind {
    pub fn ordinal(&self) -> i32 {
        match self {
            Kind::Spawn => 0,
            Kind::Extension => 1,
            Kind::Tower => 2,
            Kind::Road => 3,
            Kind::Container => 4,
            Kind::Storage => 5,
            Kind::Link => 6,
            Kind::Rampart => 7,
            Kind::Wall => 8,
            Kind::Extractor => 9,
            Kind::Terminal => 10,
            Kind::Observer => 11,
            Kind::Lab => 12,
            Kind::Factory => 13,
            Kind::PowerSpawn => 14,
            Kind::Nuker => 15,
            Kind::KeeperLair => 16,
            Kind::InvaderCore => 17,
            Kind::PowerBank => 18,
            Kind::Portal => 19,
            Kind::Other(_) => 20,
        }
    }

    pub fn of_name(name: &str) -> Kind {
        match name {
            "spawn" => Kind::Spawn,
            "extension" => Kind::Extension,
            "tower" => Kind::Tower,
            "road" => Kind::Road,
            "container" => Kind::Container,
            "storage" => Kind::Storage,
            "link" => Kind::Link,
            "rampart" => Kind::Rampart,
            "constructedWall" => Kind::Wall,
            "extractor" => Kind::Extractor,
            "terminal" => Kind::Terminal,
            "observer" => Kind::Observer,
            "lab" => Kind::Lab,
            "factory" => Kind::Factory,
            "powerSpawn" => Kind::PowerSpawn,
            "nuker" => Kind::Nuker,
            "keeperLair" => Kind::KeeperLair,
            "invaderCore" => Kind::InvaderCore,
            "powerBank" => Kind::PowerBank,
            "portal" => Kind::Portal,
            other => Kind::Other(other.to_string()),
        }
    }

    pub fn name(&self) -> String {
        match self {
            Kind::Spawn => "spawn".to_string(),
            Kind::Extension => "extension".to_string(),
            Kind::Tower => "tower".to_string(),
            Kind::Road => "road".to_string(),
            Kind::Container => "container".to_string(),
            Kind::Storage => "storage".to_string(),
            Kind::Link => "link".to_string(),
            Kind::Rampart => "rampart".to_string(),
            Kind::Wall => "constructedWall".to_string(),
            Kind::Extractor => "extractor".to_string(),
            Kind::Terminal => "terminal".to_string(),
            Kind::Observer => "observer".to_string(),
            Kind::Lab => "lab".to_string(),
            Kind::Factory => "factory".to_string(),
            Kind::PowerSpawn => "powerSpawn".to_string(),
            Kind::Nuker => "nuker".to_string(),
            Kind::KeeperLair => "keeperLair".to_string(),
            Kind::InvaderCore => "invaderCore".to_string(),
            Kind::PowerBank => "powerBank".to_string(),
            Kind::Portal => "portal".to_string(),
            Kind::Other(name) => name.clone(),
        }
    }

    /// A rampart is walkable only if ours or public: `Structure.Walkable`.
    pub fn walkable(&self) -> bool {
        matches!(
            self,
            Kind::Road | Kind::Container | Kind::Rampart | Kind::Portal
        )
    }

    pub fn is_spawn(&self) -> bool {
        *self == Kind::Spawn
    }
    pub fn is_extension(&self) -> bool {
        *self == Kind::Extension
    }
    pub fn is_tower(&self) -> bool {
        *self == Kind::Tower
    }
    pub fn is_road(&self) -> bool {
        *self == Kind::Road
    }
    pub fn is_container(&self) -> bool {
        *self == Kind::Container
    }
    pub fn is_storage(&self) -> bool {
        *self == Kind::Storage
    }
    pub fn is_link(&self) -> bool {
        *self == Kind::Link
    }
    pub fn is_rampart(&self) -> bool {
        *self == Kind::Rampart
    }
    pub fn is_wall(&self) -> bool {
        *self == Kind::Wall
    }
    pub fn is_extractor(&self) -> bool {
        *self == Kind::Extractor
    }
    pub fn is_terminal(&self) -> bool {
        *self == Kind::Terminal
    }
    pub fn is_observer(&self) -> bool {
        *self == Kind::Observer
    }
    pub fn is_lab(&self) -> bool {
        *self == Kind::Lab
    }
    pub fn is_factory(&self) -> bool {
        *self == Kind::Factory
    }
    pub fn is_power_spawn(&self) -> bool {
        *self == Kind::PowerSpawn
    }
    pub fn is_nuker(&self) -> bool {
        *self == Kind::Nuker
    }
    pub fn is_keeper_lair(&self) -> bool {
        *self == Kind::KeeperLair
    }
    pub fn is_invader_core(&self) -> bool {
        *self == Kind::InvaderCore
    }
    pub fn is_power_bank(&self) -> bool {
        *self == Kind::PowerBank
    }
    pub fn is_portal(&self) -> bool {
        *self == Kind::Portal
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Structure {
    pub id: String,
    pub kind: Kind,
    pub pos: Pos,
    pub owner: Owner,
    pub hits: i32,
    pub hits_max: i32,
    pub store: Store,
    pub cooldown: i32,
    /// Ticks to an Invader core's collapse, at the latest; 0 for any other structure.
    pub collapse: i32,
    /// An Invader core's level, 0 for an expansion core; 0 for any other structure.
    pub level: i32,
    pub walkable: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Site {
    pub id: String,
    pub kind: Kind,
    pub pos: Pos,
    pub owner: Owner,
    pub progress: i32,
    pub progress_total: i32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Source {
    pub id: String,
    pub pos: Pos,
    pub energy: i32,
    pub energy_capacity: i32,
    /// 0 while the engine has no timer running.
    pub ticks_to_regeneration: i32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Mineral {
    pub id: String,
    pub pos: Pos,
    pub resource: Resource,
    pub amount: i32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pile {
    pub id: String,
    pub pos: Pos,
    pub resource: Resource,
    pub amount: i32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Controller {
    pub id: String,
    pub pos: Pos,
    pub owner: Owner,
    pub level: i32,
    pub progress: i32,
    pub progress_total: i32,
    /// 0 on a controller nobody owns.
    pub ticks_to_downgrade: i32,
    /// Username and ticks left.
    pub reservation: Option<(String, i32)>,
    pub safe_mode: i32,
    pub safe_mode_available: i32,
    pub safe_mode_cooldown: i32,
    pub upgrade_blocked: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Part {
    pub kind: BodyPart,
    pub hits: i32,
    pub boost: Boost,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Creep {
    pub id: String,
    pub name: String,
    pub owner: Owner,
    pub pos: Pos,
    /// Head first, as the engine holds it.
    pub body: Vec<Part>,
    pub hits: i32,
    pub hits_max: i32,
    /// Undefined while spawning; those are not in the world.
    pub ticks_to_live: i32,
    pub fatigue: i32,
    pub store: Store,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fallen {
    pub creep: Creep,
    pub died: i32,
}

/// A tombstone or a ruin.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Remains {
    pub id: String,
    pub pos: Pos,
    pub store: Store,
    pub fallen: Option<Fallen>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Room {
    pub name: String,
    /// `Game.map.getRoomTerrain(name).getRawBuffer()`.
    pub terrain: Vec<u8>,
    pub controller: Option<Controller>,
    /// 0 in a room without spawns.
    pub energy_available: i32,
    pub energy_capacity: i32,
    pub sources: Vec<Source>,
    pub minerals: Vec<Mineral>,
    /// Every structure, any owner.
    pub structures: Vec<Structure>,
    /// Every construction site, any owner.
    pub sites: Vec<Site>,
    pub piles: Vec<Pile>,
    pub remains: Vec<Remains>,
    pub hostiles: Vec<Creep>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Oven {
    pub name: String,
    pub remaining: i32,
    pub body: Vec<Part>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Spawn {
    pub id: String,
    pub name: String,
    pub pos: Pos,
    pub spawning: Option<Oven>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct World {
    pub time: i32,
    /// Empty while we own nothing.
    pub me: String,
    pub cpu_limit: f64,
    pub bucket: i32,
    /// Visible rooms.
    pub rooms: Vec<Room>,
    /// Ours.
    pub spawns: Vec<Spawn>,
    /// Ours, standing (still in an oven excluded).
    pub creeps: Vec<Creep>,
    /// In every room, seen or not: what MAX_CONSTRUCTION_SITES counts.
    pub our_sites: i32,
}

pub mod terrain {
    pub const WALL: u8 = 1;
    pub const SWAMP: u8 = 2;

    pub fn is_wall(terrain: &[u8], x: i32, y: i32) -> bool {
        terrain[(y * 50 + x) as usize] & WALL != 0
    }

    pub fn is_swamp(terrain: &[u8], x: i32, y: i32) -> bool {
        let t = terrain[(y * 50 + x) as usize];
        t & WALL == 0 && t & SWAMP != 0
    }
}

pub mod hostiles {
    use super::{Creep, Room, World};
    use std::collections::BTreeSet;

    fn strikes(creep: &Creep) -> bool {
        creep
            .body
            .iter()
            .any(|p| p.kind.is_attack() || p.kind.is_ranged_attack())
    }

    pub fn owned(rooms: &[Room]) -> BTreeSet<String> {
        rooms
            .iter()
            .filter_map(|r| match &r.controller {
                Some(c) if c.owner.is_ours() => Some(r.name.clone()),
                _ => None,
            })
            .collect()
    }

    /// HEAL and WORK threaten beside a striker (owner's call): alone they are
    /// a rival's healer in transit or its remote harvester, but WORK in a room
    /// whose controller is ours can only be there to dismantle.
    pub fn armed(owned: &BTreeSet<String>, hostiles: &[Creep], creep: &Creep) -> bool {
        let works = creep.body.iter().any(|p| p.kind.is_work());
        strikes(creep)
            || works && owned.contains(&creep.pos.room)
            || (works || creep.body.iter().any(|p| p.kind.is_heal()))
                && hostiles
                    .iter()
                    .any(|o| o.pos.room == creep.pos.room && strikes(o))
    }

    pub fn world_creeps(world: &World) -> Vec<&Creep> {
        world.rooms.iter().flat_map(|r| r.hostiles.iter()).collect()
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Seen {
    pub gone: i32,
    pub armed: bool,
}

pub type Sightings = BTreeMap<String, BTreeMap<String, Seen>>;

pub mod sightings {
    use super::{Seen, Sightings, World, hostiles};
    use std::collections::BTreeMap;

    pub fn step(world: &World, previous: &Sightings) -> Sightings {
        let mut next: Sightings = previous
            .iter()
            .map(|(room, seen)| {
                (
                    room.clone(),
                    seen.iter()
                        .filter(|(_, s)| s.gone > world.time)
                        .map(|(id, s)| (id.clone(), *s))
                        .collect::<BTreeMap<_, _>>(),
                )
            })
            .filter(|(_, seen)| !seen.is_empty())
            .collect();

        let owned = hostiles::owned(&world.rooms);

        // A room in sight holds what stands there now: a wave killed reopens it.
        for room in &world.rooms {
            let mut seen: BTreeMap<String, Seen> = BTreeMap::new();

            for creep in &room.hostiles {
                if hostiles::armed(&owned, &room.hostiles, creep) {
                    seen.insert(
                        creep.id.clone(),
                        Seen {
                            gone: world.time + creep.ticks_to_live,
                            armed: true,
                        },
                    );
                }
            }

            for s in &room.structures {
                if s.kind.is_invader_core() {
                    seen.insert(
                        s.id.clone(),
                        Seen {
                            gone: world.time + s.collapse,
                            armed: s.level > 0,
                        },
                    );
                }
            }

            if seen.is_empty() {
                next.remove(&room.name);
            } else {
                next.insert(room.name.clone(), seen);
            }
        }

        next
    }

    fn none(counts: impl Fn(&Seen) -> bool, sightings: &Sightings, time: i32, room: &str) -> bool {
        match sightings.get(room) {
            Some(seen) => seen.values().all(|s| s.gone <= time || !counts(s)),
            None => true,
        }
    }

    pub fn clear(sightings: &Sightings, time: i32, room: &str) -> bool {
        none(|_| true, sightings, time, room)
    }

    pub fn unarmed(sightings: &Sightings, time: i32, room: &str) -> bool {
        none(|s| s.armed, sightings, time, room)
    }
}

impl Creep {
    pub fn is_ours(&self) -> bool {
        self.owner.is_ours()
    }
}

impl World {
    pub fn room(&self, name: &str) -> Option<&Room> {
        self.rooms.iter().find(|r| r.name == name)
    }
}

/// The parts of a creep's body, as the engine reports a live part.
pub fn part_hits(part: &Part) -> i32 {
    part.hits
}

pub fn body_kinds(creep: &Creep) -> Vec<BodyPart> {
    creep.body.iter().map(|p| p.kind).collect()
}

pub fn body_boosts(creep: &Creep) -> Vec<Boost> {
    creep.body.iter().map(|p| p.boost).collect()
}

/// A creep's body as the engine prices it.
pub fn body_cost(creep: &Creep) -> i32 {
    engine::body_cost(&body_kinds(creep))
}
