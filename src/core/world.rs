use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};

use super::engine;
use super::tick::Declarations;
use super::vocabulary::{BodyPart, Boost, Resource};

/// A room by its world coordinates: W0 is x = -1, N0 is y = -1.
/// Ordered west to east, then north to south: not as its name is.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RoomName {
    pub x: i16,
    pub y: i16,
}

impl RoomName {
    pub const fn new(x: i32, y: i32) -> RoomName {
        RoomName {
            x: x as i16,
            y: y as i16,
        }
    }

    /// The simulator's one room, off the grid, its name kept to write back.
    pub const SIM: RoomName = RoomName {
        x: i16::MIN,
        y: i16::MIN,
    };

    /// `None` outside the `W3N1` grammar but for `sim`.
    pub const fn parse(name: &str) -> Option<RoomName> {
        let b = name.as_bytes();
        if let [b's', b'i', b'm'] = b {
            return Some(RoomName::SIM);
        }
        match RoomName::axis(b, 0, b'W', b'E') {
            Some((x, at)) => match RoomName::axis(b, at, b'N', b'S') {
                Some((y, end)) if end == b.len() => Some(RoomName::new(x, y)),
                _ => None,
            },
            None => None,
        }
    }

    const fn axis(b: &[u8], at: usize, outward: u8, inward: u8) -> Option<(i32, usize)> {
        if at >= b.len() || (b[at] != outward && b[at] != inward) {
            return None;
        }
        let mut end = at + 1;
        let mut value: i32 = 0;
        while end < b.len() && b[end].is_ascii_digit() && end - at <= 4 {
            value = value * 10 + (b[end] - b'0') as i32;
            end += 1;
        }
        if end == at + 1 {
            return None;
        }
        Some((if b[at] == outward { -value - 1 } else { value }, end))
    }

    /// `parse`, for names written in code: a fixture's or a plan's.
    pub const fn named(name: &str) -> RoomName {
        match RoomName::parse(name) {
            Some(room) => room,
            None => panic!("no room name"),
        }
    }

    /// `sim` has no neighbours: its sentinel's would be far off the map.
    pub fn on_grid(self) -> Option<(i32, i32)> {
        (self != RoomName::SIM).then(|| self.coords())
    }

    pub fn coords(self) -> (i32, i32) {
        (i32::from(self.x), i32::from(self.y))
    }

    /// Spelt on the stack: `Display` writes a name with no `String` between.
    fn bytes(self) -> [u8; 12] {
        let mut out = [0u8; 12];
        if self == RoomName::SIM {
            out[..3].copy_from_slice(b"sim");
            return out;
        }
        let mut at = 0;
        for (outward, inward, value) in [(b'W', b'E', self.x), (b'N', b'S', self.y)] {
            let (side, value) = if value < 0 {
                (outward, -i32::from(value) - 1)
            } else {
                (inward, i32::from(value))
            };
            out[at] = side;
            at += 1;
            let start = at;
            let mut rest = value;
            loop {
                out[at] = b'0' + (rest % 10) as u8;
                at += 1;
                rest /= 10;
                if rest == 0 {
                    break;
                }
            }
            out[start..at].reverse();
        }
        out
    }
}

impl std::fmt::Display for RoomName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let bytes = self.bytes();
        let len = bytes.iter().position(|&b| b == 0).unwrap_or(bytes.len());
        f.write_str(std::str::from_utf8(&bytes[..len]).unwrap_or_default())
    }
}

/// `write!`'s `Formatter` costs more than the digits.
pub fn push_int(out: &mut String, n: i32) {
    if n < 0 {
        out.push('-');
    }
    let mut digits = [0u8; 10];
    let mut at = digits.len();
    let mut rest = n.unsigned_abs();
    loop {
        at -= 1;
        digits[at] = b'0' + (rest % 10) as u8;
        rest /= 10;
        if rest == 0 {
            break;
        }
    }
    out.push_str(std::str::from_utf8(&digits[at..]).unwrap_or_default());
}

impl std::fmt::Debug for RoomName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{self}\"")
    }
}

/// Compared by hand: Fable's record compare walks every field, per tile per tick.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Pos {
    pub room: RoomName,
    pub x: i32,
    pub y: i32,
}

impl Pos {
    pub fn new(room: RoomName, x: i32, y: i32) -> Pos {
        Pos { room, x, y }
    }

    pub fn index(&self) -> i32 {
        self.y * 50 + self.x
    }

    pub fn range(&self, b: &Pos) -> Option<i32> {
        (self.room == b.room).then(|| (self.x - b.x).abs().max((self.y - b.y).abs()))
    }

    pub fn within(&self, range: i32, b: &Pos) -> bool {
        self.range(b).map(|r| r <= range).unwrap_or_default()
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
            Ordering::Equal => self.index().cmp(&other.index()),
            c => c,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Owner {
    Ours,
    Ally(String),
    Enemy(String),
    /// Neither declared: fought where it threatens, never hunted.
    Stranger(String),
    Invader,
    Keeper,
    Nobody,
}

impl Owner {
    pub fn of_user(user: String, me: &str, declarations: &Declarations) -> Owner {
        match user.as_str() {
            u if u == me => Owner::Ours,
            engine::INVADER_USER => Owner::Invader,
            engine::KEEPER_USER => Owner::Keeper,
            u if declarations.allies.contains(u) => Owner::Ally(user),
            u if declarations.enemies.contains(u) => Owner::Enemy(user),
            _ => Owner::Stranger(user),
        }
    }

    pub fn is_ours(&self) -> bool {
        *self == Owner::Ours
    }

    pub fn is_ally(&self) -> bool {
        matches!(self, Owner::Ally(_))
    }

    pub fn is_theirs(&self) -> bool {
        !self.is_ours() && !self.is_nobody()
    }

    pub fn is_nobody(&self) -> bool {
        *self == Owner::Nobody
    }

    /// The username behind the stance; none for an Invader, a keeper or an
    /// empty seat.
    pub fn user(&self) -> Option<&str> {
        match self {
            Owner::Ally(user) | Owner::Enemy(user) | Owner::Stranger(user) => Some(user),
            _ => None,
        }
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
            .filter_map(|(r, n)| (r == resource).then_some(n))
            .sum()
    }

    pub fn used(&self) -> i32 {
        self.held.iter().map(|(_, amount)| amount).sum()
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
    /// Hivemind's (MIT) build-out, and the ore RCL6 is reached for (owner).
    pub fn pays_back(&self) -> bool {
        matches!(
            self,
            Kind::Spawn
                | Kind::Extension
                | Kind::Container
                | Kind::Storage
                | Kind::Link
                | Kind::Tower
                | Kind::Extractor
                | Kind::Terminal
        )
    }

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

    pub fn name(&self) -> Cow<'static, str> {
        match self {
            Kind::Spawn => "spawn".into(),
            Kind::Extension => "extension".into(),
            Kind::Tower => "tower".into(),
            Kind::Road => "road".into(),
            Kind::Container => "container".into(),
            Kind::Storage => "storage".into(),
            Kind::Link => "link".into(),
            Kind::Rampart => "rampart".into(),
            Kind::Wall => "constructedWall".into(),
            Kind::Extractor => "extractor".into(),
            Kind::Terminal => "terminal".into(),
            Kind::Observer => "observer".into(),
            Kind::Lab => "lab".into(),
            Kind::Factory => "factory".into(),
            Kind::PowerSpawn => "powerSpawn".into(),
            Kind::Nuker => "nuker".into(),
            Kind::KeeperLair => "keeperLair".into(),
            Kind::InvaderCore => "invaderCore".into(),
            Kind::PowerBank => "powerBank".into(),
            Kind::Portal => "portal".into(),
            Kind::Other(name) => name.to_owned().into(),
        }
    }

    /// The kinds the engine gives a `store`: the seam reads none on the rest.
    pub fn has_store(&self) -> bool {
        matches!(
            self,
            Kind::Spawn
                | Kind::Extension
                | Kind::Tower
                | Kind::Container
                | Kind::Storage
                | Kind::Link
                | Kind::Terminal
                | Kind::Lab
                | Kind::Factory
                | Kind::PowerSpawn
                | Kind::Nuker
                | Kind::InvaderCore
                | Kind::Other(_)
        )
    }

    /// The kinds the engine gives a `cooldown`.
    pub fn has_cooldown(&self) -> bool {
        matches!(
            self,
            Kind::Tower
                | Kind::Link
                | Kind::Lab
                | Kind::Extractor
                | Kind::Terminal
                | Kind::Factory
                | Kind::Nuker
                | Kind::PowerSpawn
        )
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
    pub fn is_keeper_lair(&self) -> bool {
        *self == Kind::KeeperLair
    }
    pub fn is_invader_core(&self) -> bool {
        *self == Kind::InvaderCore
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
    /// A keeper lair's ticks to its next keeper; None while one lives.
    pub spawn: Option<i32>,
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
    /// Who holds it and the ticks left.
    pub reservation: Option<(Owner, i32)>,
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
    pub name: RoomName,
    /// `Game.map.getRoomTerrain(name).getRawBuffer()`, `None` where unread.
    pub terrain: Option<terrain::Terrain>,
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
    /// Every creep not ours, an ally's among them.
    pub others: Vec<Creep>,
}

impl Room {
    pub fn hostiles(&self) -> impl Iterator<Item = &Creep> {
        self.others.iter().filter(|c| !c.owner.is_ally())
    }
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
    pub rooms_allowed: i32,
    /// Rooms whose `Game.map.getRoomStatus` is not our spawns' room's: the
    /// engine leaves a creep on the exit tile into one, never carrying it across.
    pub closed: BTreeSet<RoomName>,
    /// Last tick's strikes, `(attacker, target)` ids, off the rooms' event logs (§3).
    pub attacks: Vec<(String, String)>,
    /// Last tick's structures destroyed, off the same logs, with their build cost.
    pub destroyed: Vec<(RoomName, String, i32)>,
    pub rules: Rules,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Rules {
    pub structures: BTreeMap<String, Vec<i32>>,
}

impl Rules {
    /// `CONTROLLER_STRUCTURES`; sites count with standing structures.
    pub fn allowed(&self, kind: &str, level: i32) -> i32 {
        let row = self.structures.get(kind).map_or(&[][..], Vec::as_slice);
        let at = usize::try_from(level).map_or(None, |l| row.get(l).or(row.last()));
        at.copied().unwrap_or(0)
    }

    #[cfg(any(test, feature = "tools"))]
    pub fn vanilla() -> Rules {
        let row = |r: [i32; 9]| r.to_vec();
        let from =
            |level: usize, n: i32| row(std::array::from_fn(|l| if l >= level { n } else { 0 }));
        let structures = [
            ("spawn", row([0, 1, 1, 1, 1, 1, 1, 2, 3])),
            ("extension", row([0, 0, 5, 10, 20, 30, 40, 50, 60])),
            ("link", row([0, 0, 0, 0, 0, 2, 3, 4, 6])),
            ("road", from(0, 2500)),
            ("constructedWall", from(2, 2500)),
            ("rampart", from(2, 2500)),
            ("storage", from(4, 1)),
            ("tower", row([0, 0, 0, 1, 1, 2, 2, 3, 6])),
            ("observer", from(8, 1)),
            ("powerSpawn", from(8, 1)),
            ("extractor", from(6, 1)),
            ("terminal", from(6, 1)),
            ("lab", row([0, 0, 0, 0, 0, 0, 3, 6, 10])),
            ("container", from(0, 5)),
            ("nuker", from(8, 1)),
            ("factory", from(7, 1)),
        ];
        Rules {
            structures: (structures.into_iter())
                .map(|(kind, row)| (kind.to_string(), row))
                .collect(),
        }
    }
}

pub mod terrain {
    use crate::core::atlas::tiles::COUNT;
    use std::rc::Rc;

    pub const WALL: u8 = 1;
    pub const SWAMP: u8 = 2;

    /// A whole room's raw buffer, shared between the map cache and each tick's rooms.
    pub type Terrain = Rc<[u8; COUNT]>;

    /// The engine's buffer is short for a room the map doesn't hold: unread,
    /// which an all-plain room must not pass for.
    pub fn of(raw: Vec<u8>) -> Option<Terrain> {
        Some(Rc::new(<[u8; COUNT]>::try_from(raw).ok()?))
    }

    /// An unread room is neither wall nor swamp: its callers (layout, a
    /// fight's casting) walk rooms in sight, and a guess beats a panic there.
    pub fn is_wall(terrain: Option<&[u8; COUNT]>, x: i32, y: i32) -> bool {
        terrain.is_some_and(|t| t[(y * 50 + x) as usize] & WALL != 0)
    }

    pub fn is_swamp(terrain: Option<&[u8; COUNT]>, x: i32, y: i32) -> bool {
        terrain.is_some_and(|t| {
            let t = t[(y * 50 + x) as usize];
            t & WALL == 0 && t & SWAMP != 0
        })
    }
}

pub mod hostiles {
    use super::{Creep, Room, RoomName};
    use crate::core::engine;
    use std::collections::BTreeSet;

    pub fn strikes(creep: &Creep) -> bool {
        creep
            .body
            .iter()
            .any(|p| p.kind.is_attack() || p.kind.is_ranged_attack())
    }

    /// The tiles a body can strike before our next decision: its longest
    /// weapon's range plus the two ticks v1's `ReachHorizon` let it close in.
    pub fn reach(creep: &Creep) -> i32 {
        let range = if creep.body.iter().any(|p| p.kind.is_ranged_attack()) {
            engine::RANGED_RANGE
        } else {
            engine::MELEE_RANGE
        };

        range + 2
    }

    pub fn owned(rooms: &[Room]) -> BTreeSet<RoomName> {
        rooms
            .iter()
            .filter_map(|r| match &r.controller {
                Some(c) if c.owner.is_ours() => Some(r.name),
                _ => None,
            })
            .collect()
    }

    /// HEAL and WORK threaten beside a striker (owner's call): alone they are
    /// a rival's healer in transit or its remote harvester, but WORK in a room
    /// whose controller is ours can only be there to dismantle.
    pub fn armed(owned: &BTreeSet<RoomName>, hostiles: &[Creep], creep: &Creep) -> bool {
        let works = creep.body.iter().any(|p| p.kind.is_work());
        strikes(creep)
            || works && owned.contains(&creep.pos.room)
            || (works || creep.body.iter().any(|p| p.kind.is_heal()))
                && hostiles
                    .iter()
                    .any(|o| o.pos.room == creep.pos.room && strikes(o))
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Seen {
    pub gone: i32,
    pub armed: bool,
}

pub type Sightings = BTreeMap<RoomName, BTreeMap<String, Seen>>;

pub mod sightings {
    use super::{Creep, RoomName, Seen, Sightings, World, hostiles};
    use std::collections::BTreeMap;

    pub fn step(world: &World, previous: Sightings) -> Sightings {
        let mut next = previous;
        next.iter_mut()
            .for_each(|(_, seen)| seen.retain(|_, s| s.gone > world.time));
        next.retain(|_, r| !r.is_empty());

        let owned = hostiles::owned(&world.rooms);

        // A room in sight holds what stands there now: a wave killed reopens it.
        for room in &world.rooms {
            let mut seen: BTreeMap<String, Seen> = BTreeMap::new();

            let foes: Vec<Creep> = room.hostiles().cloned().collect();
            for creep in &foes {
                if hostiles::armed(&owned, &foes, creep) {
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
                next.insert(room.name, seen);
            }
        }

        next
    }

    fn none(
        counts: impl Fn(&Seen) -> bool,
        sightings: &Sightings,
        time: i32,
        room: RoomName,
    ) -> bool {
        match sightings.get(&room) {
            Some(seen) => seen.values().all(|s| s.gone <= time || !counts(s)),
            None => true,
        }
    }

    pub fn clear(sightings: &Sightings, time: i32, room: RoomName) -> bool {
        none(|_| true, sightings, time, room)
    }

    pub fn unarmed(sightings: &Sightings, time: i32, room: RoomName) -> bool {
        none(|s| s.armed, sightings, time, room)
    }
}

impl Creep {
    pub fn is_ours(&self) -> bool {
        self.owner.is_ours()
    }

    pub fn live(&self, kind: BodyPart) -> i32 {
        (self.body.iter())
            .filter(|p| p.hits > 0 && super::vocabulary::same_part(p.kind, kind))
            .count() as i32
    }
}

impl World {
    pub fn room(&self, name: RoomName) -> Option<&Room> {
        self.rooms.iter().find(|r| r.name == name)
    }
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
