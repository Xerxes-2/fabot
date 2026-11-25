use screeps::LocalRoomTerrain;
pub use screeps::StructureType;
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};

use super::engine;
use super::vocabulary::{Part, ResourceType};
use crate::declarations::Declarations;

/// An object id in `screeps::RawObjectId`'s 128 bits, its digits left-aligned
/// so it orders as its text: Memory's id sets and every tie-break on ids were
/// written in that order.
///
/// Four words, most significant first: a `u128` aligns to 16 and widens every
/// object that holds one.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id([u32; 4]);

impl Id {
    pub fn parse(text: &str) -> Option<Id> {
        let len = text.len();
        if len == 0 || len > 24 {
            return None;
        }
        // Two words, not `from_str_radix`: its checked 128-bit multiply cost
        // `world` 3% on the bench (2026-11-18). Lowercase only, as the engine
        // writes them: `A1` would equal `a1` and sort apart from its text.
        let hex = |digits: &[u8]| {
            (digits.iter()).try_fold(0u64, |v, b| {
                let digit = char::from(*b)
                    .to_digit(16)
                    .filter(|_| !b.is_ascii_uppercase());
                Some(v << 4 | u64::from(digit?))
            })
        };
        let (high, low) = text.as_bytes().split_at(len.saturating_sub(16));
        let value = u128::from(hex(high)?) << 64 | u128::from(hex(low)?);
        let key = (value << (4 * (24 - len))) << 32 | len as u128;
        Some(Id([96, 64, 32, 0].map(|at| (key >> at) as u32)))
    }

    /// No text parses to it: `World::drop_misread` drops the object holding it.
    pub const MISREAD: Id = Id([0; 4]);

    pub fn of_engine(text: &str) -> Id {
        Id::parse(text).unwrap_or(Id::MISREAD)
    }

    pub fn text(self) -> IdText {
        let [w0, w1, w2, len] = self.0.map(u64::from);
        let (high, low, len) = (w0, w1 << 32 | w2, len as usize);
        let mut out = [0u8; 24];
        for (i, b) in out[..len].iter_mut().enumerate() {
            let word = if i < 8 {
                high >> (4 * (7 - i))
            } else {
                low >> (4 * (23 - i))
            };
            *b = b"0123456789abcdef"[(word & 15) as usize];
        }
        IdText(out, len)
    }
}

pub struct IdText([u8; 24], usize);

impl std::ops::Deref for IdText {
    type Target = str;

    fn deref(&self) -> &str {
        std::str::from_utf8(&self.0[..self.1]).unwrap_or_default()
    }
}

impl serde::Serialize for Id {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(&self.text())
    }
}

impl<'de> serde::Deserialize<'de> for Id {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        let text = String::deserialize(d)?;
        Id::parse(&text).ok_or_else(|| serde::de::Error::custom("no object id"))
    }
}

/// A fixture's readable tag, hexed so tags keep their order.
#[cfg(any(test, feature = "test-support", feature = "tools"))]
#[allow(clippy::expect_used)]
impl From<&str> for Id {
    fn from(tag: &str) -> Id {
        let hex: String = tag.bytes().map(|b| format!("{b:02x}")).collect();
        Id::parse(&hex).expect("a tag of 12 bytes at most")
    }
}

impl std::fmt::Display for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.text())
    }
}

impl std::fmt::Debug for Id {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{self}\"")
    }
}

/// A room by its world coordinates (W0 is x = -1, N0 is y = -1); `sim` is W127N127, on no map.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct RoomName(screeps::RoomName);

impl RoomName {
    pub const SIM: RoomName = RoomName(screeps::RoomName::from_packed(0));

    #[cfg(any(test, feature = "test-support", feature = "tools"))]
    pub const W1N1: RoomName = RoomName::new(-2, -2);

    pub const fn new(x: i32, y: i32) -> RoomName {
        debug_assert!(-128 <= x && x <= 127 && -128 <= y && y <= 127);
        let (x, y) = ((x + 128) as u16, (y + 128) as u16);
        RoomName(screeps::RoomName::from_packed(x << 8 | y))
    }

    /// `None` past the grid's edge; `sim` has no neighbours.
    pub fn offset(self, dx: i32, dy: i32) -> Option<RoomName> {
        if (dx, dy) == (0, 0) {
            return Some(self);
        }
        let (x, y) = self.on_grid().map(|(x, y)| (x + dx, y + dy))?;
        let on = |c: i32| (-128..=127).contains(&c);
        (on(x) && on(y) && (x, y) != (-128, -128)).then(|| RoomName::new(x, y))
    }

    pub fn on_map(self) -> Option<screeps::RoomName> {
        (self != RoomName::SIM).then_some(self.0)
    }

    pub fn on_grid(self) -> Option<(i32, i32)> {
        self.on_map().map(|r| (r.x_coord(), r.y_coord()))
    }

    /// `sim`'s are far off the grid, so no room reads near it.
    pub fn coords(self) -> (i32, i32) {
        let sentinel = i32::from(i16::MIN);
        self.on_grid().unwrap_or((sentinel, sentinel))
    }
}

impl std::str::FromStr for RoomName {
    type Err = screeps::local::RoomNameParseError;

    fn from_str(name: &str) -> Result<RoomName, Self::Err> {
        match name {
            "sim" => Ok(RoomName::SIM),
            _ => name.parse().map(RoomName),
        }
    }
}

impl std::fmt::Display for RoomName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.on_map() {
            Some(room) => room.fmt(f),
            None => f.write_str("sim"),
        }
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

impl serde::Serialize for RoomName {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.collect_str(self)
    }
}

impl<'de> serde::Deserialize<'de> for RoomName {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        let name = String::deserialize(d)?;
        name.parse().map_err(serde::de::Error::custom)
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash, serde::Serialize, serde::Deserialize)]
pub struct Pos {
    pub room: RoomName,
    pub x: i32,
    pub y: i32,
}

impl Pos {
    pub fn new(room: RoomName, x: i32, y: i32) -> Pos {
        Pos { room, x, y }
    }

    #[inline]
    pub fn range(&self, b: &Pos) -> Option<i32> {
        (self.room == b.room).then(|| (self.x - b.x).abs().max((self.y - b.y).abs()))
    }

    #[inline]
    pub fn within(&self, range: i32, b: &Pos) -> bool {
        self.range(b).map(|r| r <= range).unwrap_or_default()
    }

    pub fn crow_flies(&self, b: &Pos) -> i32 {
        let ((ax, ay), (bx, by)) = (self.room.coords(), b.room.coords());
        let side = engine::ROOM_SIDE;

        (side * (ax - bx) + self.x - b.x)
            .abs()
            .max((side * (ay - by) + self.y - b.y).abs())
    }
}

impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Row-major, as a tile index orders, where a derived order would put `x` first.
impl Ord for Pos {
    fn cmp(&self, other: &Self) -> Ordering {
        (self.room, self.y, self.x).cmp(&(other.room, other.y, other.x))
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
            screeps::INVADER_USERNAME => Owner::Invader,
            screeps::SOURCE_KEEPER_USERNAME => Owner::Keeper,
            u if declarations.allies.contains(u) => Owner::Ally(user),
            u if declarations.enemies.contains(u) => Owner::Enemy(user),
            _ => Owner::Stranger(user),
        }
    }

    #[inline]
    pub fn is_ours(&self) -> bool {
        *self == Owner::Ours
    }

    pub fn is_ally(&self) -> bool {
        matches!(self, Owner::Ally(_))
    }

    pub fn is_theirs(&self) -> bool {
        !self.is_ours() && !self.is_ally() && *self != Owner::Nobody
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
    pub held: Vec<(ResourceType, i32)>,
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

    #[inline]
    pub fn amount_of(&self, resource: ResourceType) -> i32 {
        self.held
            .iter()
            .filter_map(|(r, n)| (*r == resource).then_some(n))
            .sum()
    }

    pub fn used(&self) -> i32 {
        self.held.iter().map(|(_, amount)| amount).sum()
    }

    #[inline]
    pub fn free(&self) -> i32 {
        (self.capacity - self.used()).max(0)
    }
}

pub trait Built: Copy {
    /// Hivemind's (MIT) build-out, and the ore RCL6 is reached for (owner).
    fn pays_back(self) -> bool;
    fn has_store(self) -> bool;
    fn has_cooldown(self) -> bool;
    fn walkable(self) -> bool;
}

impl Built for StructureType {
    fn pays_back(self) -> bool {
        use StructureType::*;
        matches!(
            self,
            Spawn | Extension | Container | Storage | Link | Tower | Extractor | Terminal
        )
    }

    /// read.js's KNOWN minus STORED: the kinds it streams no store for.
    fn has_store(self) -> bool {
        use StructureType::*;
        !matches!(
            self,
            Road | Rampart | Wall | Extractor | Observer | KeeperLair | PowerBank | Portal
        )
    }

    fn has_cooldown(self) -> bool {
        use StructureType::*;
        matches!(
            self,
            Tower | Link | Lab | Extractor | Terminal | Factory | Nuker | PowerSpawn
        )
    }

    fn walkable(self) -> bool {
        use StructureType::*;
        matches!(self, Road | Container | Rampart | Portal)
    }
}

pub const KIND_COUNT: usize = <StructureType as enum_iterator::Sequence>::CARDINALITY;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Structure {
    pub id: Id,
    pub kind: StructureType,
    pub pos: Pos,
    pub owner: Owner,
    pub hits: i32,
    pub hits_max: i32,
    pub store: Store,
    pub cooldown: i32,
    /// Ticks to an Invader core's collapse, at the latest; 0 for any other structure.
    pub collapse: i32,
    /// Ticks an Invader core stays invulnerable before it deploys; 0 once deployed.
    pub deploy: i32,
    /// An Invader core's level, 0 for an expansion core; 0 for any other structure.
    pub level: i32,
    /// A keeper lair's ticks to its next keeper; None while one lives.
    pub spawn: Option<i32>,
    pub walkable: bool,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Site {
    pub id: Id,
    pub kind: StructureType,
    pub pos: Pos,
    pub owner: Owner,
    pub progress: i32,
    pub progress_total: i32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Source {
    pub id: Id,
    pub pos: Pos,
    pub energy: i32,
    pub energy_capacity: i32,
    /// 0 while the engine has no timer running.
    pub ticks_to_regeneration: i32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Mineral {
    pub id: Id,
    pub pos: Pos,
    pub resource: Option<ResourceType>,
    pub amount: i32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pile {
    pub id: Id,
    pub pos: Pos,
    pub resource: ResourceType,
    pub amount: i32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Controller {
    pub id: Id,
    pub pos: Pos,
    pub owner: Owner,
    pub level: i32,
    pub progress: i32,
    pub progress_total: i32,
    /// 0 on a controller nobody owns.
    pub ticks_to_downgrade: i32,
    /// Who holds it and the ticks left.
    pub reservation: Option<(Owner, i32)>,
    /// Who signed it and the text.
    pub sign: Option<(Owner, String)>,
    pub safe_mode: i32,
    pub safe_mode_available: i32,
    pub safe_mode_cooldown: i32,
    pub upgrade_blocked: i32,
}

impl Controller {
    pub fn safe_mode_fireable(&self) -> bool {
        self.safe_mode_available > 0
            && self.safe_mode_cooldown == 0
            && self.upgrade_blocked == 0
            && self.ticks_to_downgrade >= engine::safe_mode_downgrade_line(self.level)
    }

    pub fn our_safe_mode(&self) -> i32 {
        if self.owner.is_ours() {
            self.safe_mode
        } else {
            0
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BodyPart {
    pub kind: Part,
    pub hits: i32,
    pub boost: Option<ResourceType>,
}

impl Borrow<Part> for BodyPart {
    fn borrow(&self) -> &Part {
        &self.kind
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Creep {
    pub id: Id,
    pub name: String,
    pub owner: Owner,
    pub pos: Pos,
    /// Head first, as the engine holds it.
    pub body: Vec<BodyPart>,
    pub hits: i32,
    pub hits_max: i32,
    pub ticks_to_live: i32,
    pub fatigue: i32,
    pub store: Store,
}

/// One of ours, as its tombstone keeps it.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fallen {
    pub body: Vec<Part>,
    pub ticks_to_live: i32,
    pub died: i32,
}

impl Fallen {
    pub fn lived(&self) -> Option<i32> {
        (self.ticks_to_live > engine::AGED_OUT_TICKS_TO_LIVE)
            .then(|| engine::lifetime(self.body.iter().copied()) - self.ticks_to_live)
    }
}

/// A tombstone or a ruin.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Remains {
    pub id: Id,
    pub pos: Pos,
    pub store: Store,
    pub fallen: Option<Fallen>,
    /// The id of another's creep its tombstone keeps: the engine's word it died.
    pub theirs: Option<Id>,
}

#[derive(Clone, Debug)]
pub struct Room<'a> {
    pub name: RoomName,
    pub terrain: Option<&'a LocalRoomTerrain>,
    pub controller: Option<Controller>,
    /// 0 in a room without spawns.
    pub energy_available: i32,
    pub energy_capacity: i32,
    pub sources: Vec<Source>,
    pub minerals: Vec<Mineral>,
    /// Every structure, any owner, of a kind the crate names.
    pub structures: Vec<Structure>,
    /// Under a structure of a kind the crate does not name: no walking or building there.
    pub blocked: Vec<Pos>,
    /// Every construction site, any owner.
    pub sites: Vec<Site>,
    pub piles: Vec<Pile>,
    pub remains: Vec<Remains>,
    /// Every creep not ours, an ally's among them.
    pub others: Vec<Creep>,
}

impl Room<'_> {
    pub fn empty(name: RoomName) -> Room<'static> {
        Room {
            name,
            terrain: None,
            controller: None,
            energy_available: 0,
            energy_capacity: 0,
            sources: Vec::new(),
            minerals: Vec::new(),
            structures: Vec::new(),
            blocked: Vec::new(),
            sites: Vec::new(),
            piles: Vec::new(),
            remains: Vec::new(),
            others: Vec::new(),
        }
    }

    pub fn hostiles(&self) -> impl Iterator<Item = &Creep> {
        self.others.iter().filter(|c| !c.owner.is_ally())
    }

    pub fn invader_core(&self) -> Option<&Structure> {
        (self.structures.iter()).find(|s| s.kind == StructureType::InvaderCore && s.level > 0)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Oven {
    pub name: String,
    pub remaining: i32,
    pub body: Vec<BodyPart>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Spawn {
    pub id: Id,
    pub name: String,
    pub pos: Pos,
    pub spawning: Option<Oven>,
}

#[derive(Clone, Debug)]
pub struct World<'a> {
    pub time: i32,
    /// Empty while we own nothing.
    pub me: String,
    pub cpu_limit: f64,
    pub bucket: i32,
    pub rooms: Vec<Room<'a>>,
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
    pub attacks: Vec<(Id, Id)>,
    /// Last tick's structures destroyed, off the same logs, with their build cost.
    pub destroyed: Vec<(RoomName, Id, i32)>,
    pub rules: Rules,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Rules {
    pub structures: Vec<Vec<i32>>,
}

impl Rules {
    pub fn of(rows: impl IntoIterator<Item = (StructureType, Vec<i32>)>) -> Rules {
        let mut structures = vec![Vec::new(); KIND_COUNT];
        for (kind, row) in rows {
            structures[kind as usize] = row;
        }
        Rules { structures }
    }

    /// `CONTROLLER_STRUCTURES`; sites count with standing structures.
    #[inline]
    pub fn allowed(&self, kind: StructureType, level: i32) -> i32 {
        let row = (self.structures.get(kind as usize)).map_or(&[][..], Vec::as_slice);
        let at = usize::try_from(level).map_or(None, |l| row.get(l).or(row.last()));
        at.copied().unwrap_or(0)
    }

    /// `room`'s structures the engine lets act (`utils.checkStructureAgainstController`),
    /// in its order: equal ranges to the controller rank in the object order `find` reads.
    pub fn working<'r>(&self, room: &'r Room) -> impl Iterator<Item = &'r Structure> + use<'r> {
        let holder = (room.controller.as_ref()).filter(|c| c.level >= 1);
        let range = move |s: &Structure| holder.and_then(|c| c.pos.range(&s.pos)).unwrap_or(0);
        let capped: [bool; KIND_COUNT] =
            std::array::from_fn(|k| self.structures.get(k).is_some_and(|row| !row.is_empty()));
        // Per kind, the range nearer than which all act, and how many at it still do.
        let mut cut = [(i32::MAX, usize::MAX); KIND_COUNT];
        if let Some(c) = holder {
            let held = || room.structures.iter().filter(|s| s.owner == c.owner);
            let mut count = [0; KIND_COUNT];
            held().for_each(|s| count[s.kind as usize] += 1);
            for kind in enum_iterator::all::<StructureType>() {
                let (k, allowed) = (kind as usize, self.allowed(kind, c.level).max(0) as usize);
                if allowed == 0 {
                    cut[k] = (-1, 0);
                } else if count[k] > allowed && self.allowed(kind, 8) != 1 {
                    let mut ranges: Vec<i32> =
                        held().filter(|s| s.kind == kind).map(range).collect();
                    let at = *ranges.select_nth_unstable(allowed - 1).1;
                    cut[k] = (at, allowed - ranges.iter().filter(|r| **r < at).count());
                }
            }
        }
        room.structures.iter().filter(move |s| {
            let player = s.owner.is_ours() || s.owner.user().is_some();
            if !player || !capped[s.kind as usize] {
                return true;
            }
            if !holder.is_some_and(|c| c.owner == s.owner) {
                return false;
            }
            let ((at, left), r) = (&mut cut[s.kind as usize], range(s));
            if r == *at && *left > 0 {
                *left -= 1;
                return true;
            }
            r < *at
        })
    }

    #[cfg(any(test, feature = "test-support", feature = "tools"))]
    pub fn vanilla() -> Rules {
        let row = |kind: StructureType| (0..=8).map(move |l| kind.controller_structures(l) as i32);
        Rules::of(enum_iterator::all::<StructureType>().map(|k| (k, row(k).collect())))
    }
}

/// `read.js`'s pass decoded in the order it wrote it: the world bar what
/// only the engine's globals answer (the CPU limit, terrain, room status).
pub mod read;

pub mod terrain {
    use screeps::LocalRoomTerrain;

    pub const WALL: u8 = screeps::Terrain::Wall as u8;
    pub const SWAMP: u8 = screeps::Terrain::Swamp as u8;

    /// For tests and tools, which hold no map cache for a room to borrow from.
    #[cfg(any(test, feature = "test-support", feature = "tools"))]
    pub fn leak(bits: Vec<u8>) -> Option<&'static LocalRoomTerrain> {
        let bits = bits.into_boxed_slice().try_into().ok()?;
        Some(Box::leak(Box::new(LocalRoomTerrain::new_from_bits(bits))))
    }

    /// An unread room is neither wall nor swamp: its callers (layout, a
    /// fight's casting) walk rooms in sight, and a guess beats a panic there.
    pub fn is_wall(terrain: Option<&LocalRoomTerrain>, x: i32, y: i32) -> bool {
        terrain.is_some_and(|t| t.get_bits()[(y * 50 + x) as usize] & WALL != 0)
    }

    pub fn is_swamp(terrain: Option<&LocalRoomTerrain>, x: i32, y: i32) -> bool {
        terrain.is_some_and(|t| t.get_bits()[(y * 50 + x) as usize] & (WALL | SWAMP) == SWAMP)
    }
}

pub mod hostiles {
    use super::{BodyPart, Creep, Owner, Part, Room, RoomName};
    use crate::engine;
    use std::collections::BTreeSet;

    pub fn works(creep: &Creep) -> bool {
        (creep.body.iter())
            .any(|p| p.kind == Part::Work || p.kind == Part::Carry || p.kind == Part::Claim)
    }

    pub fn foe(creep: &Creep) -> bool {
        creep.owner != Owner::Keeper
    }

    fn weapon(p: &BodyPart) -> bool {
        matches!(p.kind, Part::Attack | Part::RangedAttack)
    }

    pub fn weaponed(creep: &Creep) -> bool {
        creep.body.iter().any(weapon)
    }

    /// A weapon shot off strikes nothing back, as `Exchange` prices it,
    /// until a HEAL of its own grows it again.
    pub fn strikes(creep: &Creep) -> bool {
        (creep.body.iter()).any(|p| weapon(p) && (p.hits > 0 || creep.live(Part::Heal) > 0))
    }

    pub fn outlives(creep: &Creep, safe_left: i32) -> bool {
        creep.ticks_to_live >= safe_left
    }

    /// The tiles a body can strike before our next decision: its longest
    /// weapon's range plus the two ticks v1's `ReachHorizon` let it close in.
    pub fn reach(creep: &Creep) -> i32 {
        let ranged = creep.body.iter().any(|p| p.kind == Part::RangedAttack);
        2 + if ranged {
            engine::RANGED_RANGE
        } else {
            engine::MELEE_RANGE
        }
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
        let works = creep.body.iter().any(|p| p.kind == Part::Work);
        strikes(creep)
            || works && owned.contains(&creep.pos.room)
            || (works || creep.body.iter().any(|p| p.kind == Part::Heal))
                && hostiles
                    .iter()
                    .any(|o| o.pos.room == creep.pos.room && strikes(o))
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Seen {
    pub gone: i32,
    pub armed: bool,
    pub last: Option<Glimpse>,
}

/// What pricing and the approach read of a raider out of sight.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Glimpse {
    pub body: Vec<BodyPart>,
    pub hits: i32,
    pub hits_max: i32,
    pub pos: Pos,
    pub invader: bool,
}

impl Glimpse {
    pub fn of(creep: &Creep) -> Glimpse {
        Glimpse {
            body: creep.body.clone(),
            hits: creep.hits,
            hits_max: creep.hits_max,
            pos: creep.pos,
            invader: creep.owner == Owner::Invader,
        }
    }
}

pub type Sightings = BTreeMap<RoomName, BTreeMap<Id, Seen>>;

pub mod sightings {
    use super::hostiles;
    use super::{
        Creep, Glimpse, Id, Owner, Pos, RoomName, Seen, Sightings, Store, StructureType, World,
    };
    use std::collections::BTreeMap;

    fn standing<'w>(world: &'w World) -> BTreeMap<Id, (RoomName, &'w Creep)> {
        (world.rooms.iter())
            .flat_map(|r| r.hostiles().map(move |h| (h.id, (r.name, h))))
            .collect()
    }

    pub fn step(world: &World, previous: Sightings) -> Sightings {
        let mut next = previous;
        next.iter_mut()
            .for_each(|(_, seen)| seen.retain(|_, s| s.gone > world.time));
        next.retain(|_, r| !r.is_empty());

        let owned = hostiles::owned(&world.rooms);

        // A room in sight holds what stands there now: a wave killed reopens it.
        for room in &world.rooms {
            let mut seen: BTreeMap<Id, Seen> = BTreeMap::new();

            let foes: Vec<Creep> = room.hostiles().cloned().collect();
            for creep in &foes {
                if hostiles::armed(&owned, &foes, creep) {
                    seen.insert(
                        creep.id,
                        Seen {
                            gone: world.time + creep.ticks_to_live,
                            armed: true,
                            last: Some(Glimpse::of(creep)),
                        },
                    );
                }
            }

            for s in &room.structures {
                if s.kind == StructureType::InvaderCore {
                    seen.insert(
                        s.id,
                        Seen {
                            gone: world.time + s.collapse,
                            armed: s.level > 0,
                            last: None,
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

        // A body seen in one room is not another's raid.
        let standing = standing(world);
        for (room, seen) in &mut next {
            (seen.iter_mut())
                .filter(|(id, _)| standing.get(id).is_some_and(|(at, _)| at != room))
                .for_each(|(_, s)| s.last = None);
        }

        next
    }

    fn none(
        counts: impl Fn(&Seen) -> bool,
        sightings: &Sightings,
        time: i32,
        room: RoomName,
    ) -> bool {
        (sightings.get(&room))
            .is_none_or(|seen| seen.values().all(|s| s.gone <= time || !counts(s)))
    }

    pub fn clear(sightings: &Sightings, time: i32, room: RoomName) -> bool {
        none(|_| true, sightings, time, room)
    }

    pub fn unarmed(sightings: &Sightings, time: i32, room: RoomName) -> bool {
        none(|s| s.armed, sightings, time, room)
    }

    pub fn remembered(sightings: &Sightings, world: &World) -> BTreeMap<RoomName, Vec<Creep>> {
        let blind = |room: &RoomName| world.rooms.iter().all(|r| r.name != *room);
        (sightings.iter().filter(|(room, _)| blind(room)))
            .map(|(&room, seen)| (room, glimpsed(seen, world.time)))
            .filter(|(_, raid)| !raid.is_empty())
            .collect()
    }

    fn glimpsed(seen: &BTreeMap<Id, Seen>, time: i32) -> Vec<Creep> {
        (seen.iter().filter(|(_, s)| s.armed && s.gone > time))
            .filter_map(|(&id, s)| {
                let g = s.last.clone()?;
                Some(Creep {
                    id,
                    name: String::new(),
                    owner: if g.invader {
                        Owner::Invader
                    } else {
                        Owner::Nobody
                    },
                    pos: g.pos,
                    body: g.body,
                    hits: g.hits,
                    hits_max: g.hits_max,
                    ticks_to_live: s.gone - time,
                    fatigue: 0,
                    store: Store::empty(),
                })
            })
            .collect()
    }

    pub fn raid_at(raid: &[Creep], from: &[Pos]) -> Option<Pos> {
        (raid.iter().map(|h| h.pos)).min_by_key(|p| from.iter().map(|f| f.crow_flies(p)).min())
    }

    /// `--check`: a room in sight holds its armed bodies as seen now and expiring
    /// with their life; one out of sight remembers no body standing anywhere now.
    pub fn kept(world: &World, sightings: &Sightings) -> bool {
        let standing = standing(world);
        let owned = hostiles::owned(&world.rooms);
        let blind = |room: &RoomName| world.rooms.iter().all(|r| r.name != *room);
        let glimpsed = world.rooms.iter().all(|r| {
            let foes: Vec<Creep> = r.hostiles().cloned().collect();
            let seen = |h: &Creep| sightings.get(&r.name)?.get(&h.id)?.last.as_ref();
            (foes.iter())
                .filter(|h| hostiles::armed(&owned, &foes, h))
                .all(|h| seen(h).is_some())
        });
        glimpsed
            && sightings.iter().all(|(room, seen)| {
                seen.iter()
                    .all(|(id, s)| match (&s.last, standing.get(id)) {
                        (None, _) => s.gone > world.time,
                        (Some(_), Some(_)) if blind(room) => false,
                        (Some(_), None) => blind(room) && s.gone > world.time,
                        (Some(g), Some((at, h))) => {
                            at == room
                                && *g == Glimpse::of(h)
                                && s.gone == world.time + h.ticks_to_live
                        }
                    })
            })
    }
}

impl Creep {
    /// The engine starts a creep's clock on its first tick out: none reads while
    /// it spawns, nor on an NPC's first tick, and its whole life is ahead.
    pub fn clocked(mut self) -> Self {
        if self.ticks_to_live <= 0 {
            self.ticks_to_live = engine::lifetime(self.body.iter().map(|p| p.kind));
        }
        self
    }

    pub fn is_ours(&self) -> bool {
        self.owner.is_ours()
    }

    #[inline]
    pub fn live(&self, kind: Part) -> i32 {
        live(&self.body, kind)
    }

    pub fn swings(&self) -> bool {
        self.body.iter().any(|p| p.kind == Part::Attack)
    }
}

/// Legs and a hold alone fight for nothing; only a home's towers heal them back.
pub fn spent(body: &[BodyPart]) -> bool {
    !(body.iter()).any(|p| p.hits > 0 && !matches!(p.kind, Part::Move | Part::Tough | Part::Carry))
}

#[inline]
pub fn live(body: &[BodyPart], kind: Part) -> i32 {
    (body.iter())
        .filter(|p| p.hits > 0 && p.kind == kind)
        .count() as i32
}

#[inline]
pub fn cast(body: &[impl Borrow<Part>], kind: Part) -> i32 {
    (body.iter()).filter(|p| *(*p).borrow() == kind).count() as i32
}

impl World<'_> {
    /// The engine's word of a death last tick: another's tombstone, or a structure destroyed.
    pub fn fallen(&self) -> BTreeSet<Id> {
        let tombs = (self.rooms.iter()).flat_map(|r| r.remains.iter().filter_map(|t| t.theirs));
        tombs
            .chain(self.destroyed.iter().map(|(_, id, _)| *id))
            .collect()
    }

    #[inline]
    pub fn room(&self, name: RoomName) -> Option<&Room<'_>> {
        self.rooms.iter().find(|r| r.name == name)
    }

    pub fn working_spawns(&self) -> impl Iterator<Item = &Spawn> {
        self.spawns.iter().filter(|spawn| {
            let room = self.room(spawn.pos.room);
            room.is_some_and(|r| self.rules.working(r).any(|s| s.id == spawn.id))
        })
    }

    /// The objects whose id the engine wrote in no hex, dropped and counted:
    /// kept, two of them would share one id.
    pub fn drop_misread(&mut self) -> usize {
        let ok = |id: &Id| *id != Id::MISREAD;
        let before = self.objects();
        for r in &mut self.rooms {
            r.controller = r.controller.take().filter(|c| ok(&c.id));
            r.sources.retain(|o| ok(&o.id));
            r.minerals.retain(|o| ok(&o.id));
            r.structures.retain(|o| ok(&o.id));
            r.sites.retain(|o| ok(&o.id));
            r.piles.retain(|o| ok(&o.id));
            r.remains.retain(|o| ok(&o.id));
            r.others.retain(|o| ok(&o.id));
        }
        self.spawns.retain(|o| ok(&o.id));
        self.creeps.retain(|o| ok(&o.id));
        self.attacks.retain(|(by, at)| ok(by) && ok(at));
        self.destroyed.retain(|(_, id, _)| ok(id));
        before - self.objects()
    }

    fn objects(&self) -> usize {
        let rooms = self.rooms.iter().map(|r| {
            usize::from(r.controller.is_some())
                + r.sources.len()
                + r.minerals.len()
                + r.structures.len()
                + r.sites.len()
                + r.piles.len()
                + r.remains.len()
                + r.others.len()
        });
        rooms.sum::<usize>()
            + self.spawns.len()
            + self.creeps.len()
            + self.attacks.len()
            + self.destroyed.len()
    }
}

pub fn body_kinds(creep: &Creep) -> Vec<Part> {
    creep.body.iter().map(|p| p.kind).collect()
}

pub fn body_boosts(creep: &Creep) -> Vec<Option<ResourceType>> {
    creep.body.iter().map(|p| p.boost).collect()
}

pub fn power(creep: &Creep, part: Part) -> i32 {
    engine::power_at(&body_kinds(creep), &body_boosts(creep), creep.hits)(part)
}

/// A body's `power` to strike, read once for the many tiles it is asked about.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Striker {
    pub pos: Pos,
    pub attack: i32,
    pub ranged: i32,
    pub moves: bool,
}

impl Striker {
    pub fn of(creep: &Creep) -> Striker {
        let (kinds, boosts) = (body_kinds(creep), body_boosts(creep));
        let power = engine::power_at(&kinds, &boosts, creep.hits);
        Striker {
            pos: creep.pos,
            attack: power(Part::Attack),
            ranged: power(Part::RangedAttack),
            moves: power(Part::Move) > 0,
        }
    }

    /// `next`: after the step it can take toward `at` first.
    pub fn on(&self, at: &Pos, next: bool) -> i32 {
        let step = i32::from(next && self.moves);
        (self.pos.range(at)).map_or(0, |r| engine::strike_at(self.attack, self.ranged, r - step))
    }
}

#[cfg(test)]
mod tests {
    use super::{BodyPart, Creep, Id, Owner, Pos, Room, RoomName, Rules, Sightings, Store};
    use super::{World, sightings};
    use crate::vocabulary::Part;
    use proptest::array::uniform3;
    use proptest::collection::vec;
    use proptest::prelude::*;
    use proptest::sample::Index;

    /// An ally's structures are not theirs: no route avoids its towers and no raid razes them.
    #[test]
    fn an_ally_is_not_theirs() {
        assert!(!Owner::Ally("a".to_string()).is_theirs());
        assert!(Owner::Enemy("e".to_string()).is_theirs() && Owner::Invader.is_theirs());
    }

    fn seen_in(name: RoomName, others: Vec<Creep>) -> Room<'static> {
        Room {
            others,
            ..Room::empty(name)
        }
    }

    proptest! {
        #![proptest_config(crate::testing::config(256))]

        #[test]
        fn a_room_out_of_sight_never_remembers_a_body_standing_now(
            ticks in vec((any::<Index>(), 1..=48, 1..=48, 1..=200, uniform3(any::<bool>())), 39),
        ) {
            let names = [
                RoomName::W1N1,
                "W2N1".parse::<RoomName>().unwrap(),
                "W3N1".parse::<RoomName>().unwrap(),
            ];
            let part = |kind| BodyPart {
                kind,
                hits: 100,
                boost: None,
            };
            let mut kept = Sightings::new();
            for (time, (at, x, y, hits, seen)) in (1..).zip(ticks) {
                let at = names[at.index(names.len())];
                let raider = Creep {
                    id: Id::from("r1"),
                    name: String::new(),
                    owner: Owner::Invader,
                    pos: Pos::new(at, x, y),
                    body: vec![part(Part::Attack), part(Part::Move)],
                    hits,
                    hits_max: 200,
                    ticks_to_live: 1500 - time,
                    fatigue: 0,
                    store: Store::empty(),
                };
                let rooms = (names.iter().zip(seen).filter(|(_, seen)| *seen))
                    .map(|(&r, _)| {
                        seen_in(
                            r,
                            if r == at {
                                vec![raider.clone()]
                            } else {
                                Vec::new()
                            },
                        )
                    })
                    .collect();
                let world = World {
                    time,
                    me: String::new(),
                    cpu_limit: 20.0,
                    bucket: 10_000,
                    rooms,
                    spawns: Vec::new(),
                    creeps: Vec::new(),
                    our_sites: 0,
                    rooms_allowed: 1,
                    closed: Default::default(),
                    attacks: Vec::new(),
                    destroyed: Vec::new(),
                    rules: Rules::vanilla(),
                };
                kept = sightings::step(&world, kept);
                prop_assert!(sightings::kept(&world, &kept));
            }
        }
    }

    #[test]
    fn a_structure_past_its_level_is_inactive_the_farthest_first() {
        use super::{Controller, Spawn, Structure, StructureType as K};
        let at = |x: i32| Pos::new(RoomName::W1N1, x, 10);
        let built = |id: &str, kind: K, owner: Owner, x: i32| Structure {
            id: id.into(),
            kind,
            pos: at(x),
            owner,
            hits: 1,
            hits_max: 1,
            store: Store::empty(),
            cooldown: 0,
            collapse: 0,
            deploy: 0,
            level: 0,
            spawn: None,
            walkable: false,
        };
        let ally = Owner::Ally("a".to_string());
        let structures = vec![
            built("far", K::Spawn, Owner::Ours, 13),
            built("near", K::Spawn, Owner::Ours, 11),
            built("tied", K::Spawn, Owner::Ours, 12),
            built("tied2", K::Spawn, Owner::Ours, 12),
            built("observer", K::Observer, Owner::Ours, 20),
            built("terminal", K::Terminal, Owner::Ours, 20),
            built("allied", K::Extension, ally, 20),
            built("invader", K::Tower, Owner::Invader, 20),
            built("road", K::Road, Owner::Nobody, 20),
        ];
        let controller = Controller {
            id: "c".into(),
            pos: at(10),
            owner: Owner::Ours,
            level: 7,
            progress: 0,
            progress_total: 0,
            ticks_to_downgrade: 0,
            reservation: None,
            sign: None,
            safe_mode: 0,
            safe_mode_available: 0,
            safe_mode_cooldown: 0,
            upgrade_blocked: 0,
        };
        let spawn = |id: &str, x: i32| Spawn {
            id: id.into(),
            name: String::new(),
            pos: at(x),
            spawning: None,
        };
        let world = World {
            time: 0,
            me: String::new(),
            cpu_limit: 20.0,
            bucket: 0,
            rooms: vec![Room {
                controller: Some(controller),
                structures,
                ..Room::empty(RoomName::W1N1)
            }],
            spawns: vec![spawn("far", 13), spawn("near", 11)],
            creeps: Vec::new(),
            our_sites: 0,
            rooms_allowed: 1,
            closed: Default::default(),
            attacks: Vec::new(),
            destroyed: Vec::new(),
            rules: Rules::vanilla(),
        };
        let ids = |of: Vec<Id>| of.iter().map(Id::to_string).collect::<Vec<_>>();
        let working = world.rules.working(&world.rooms[0]).map(|s| s.id).collect();
        let near = ["near", "tied", "terminal", "invader", "road"].map(Id::from);
        assert_eq!(ids(working), ids(near.to_vec()));
        let spawns = world.working_spawns().map(|s| s.id).collect();
        assert_eq!(ids(spawns), ids(vec![Id::from("near")]));
    }

    #[test]
    fn an_id_reads_back_as_its_text_and_orders_as_it() {
        let texts = [
            "0",
            "1",
            "a",
            "a0",
            "ab",
            "00",
            "10000000",
            "100000000",
            "1000000000000000",
            "10000000000000000",
            "ffffffffffffffff",
            "fffffffffffffffff",
            "0df4aea318bd552",
            "5bbcab1d9099fc012e632dbc",
            "5bbcab1d9099fc012e632dbd",
            "ffffffffffffffffffffffff",
        ];
        for a in texts {
            let id = Id::parse(a).unwrap();
            assert_eq!(id.to_string(), a);
            for b in texts {
                assert_eq!(id.cmp(&Id::parse(b).unwrap()), a.cmp(b), "{a} {b}");
            }
        }
        let bad = ["", "ring-W1N1", "+1", "A1", "0000000000000000000000000"];
        assert!(bad.map(Id::parse).iter().all(Option::is_none));
    }
}
