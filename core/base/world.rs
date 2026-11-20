use screeps::LocalRoomTerrain;
use std::cmp::Ordering;
use std::collections::{BTreeMap, BTreeSet};
use strum::{AsRefStr, EnumDiscriminants, EnumString};

use super::engine;
use super::vocabulary::{Part, Resource, ResourceType};
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

impl serde::Serialize for RoomName {
    fn serialize<S: serde::Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.collect_str(self)
    }
}

impl<'de> serde::Deserialize<'de> for RoomName {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        let name = String::deserialize(d)?;
        RoomName::parse(&name).ok_or_else(|| serde::de::Error::custom("no room name"))
    }
}

/// Compared by hand: Fable's record compare walks every field, per tile per tick.
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

    pub fn index(&self) -> i32 {
        self.y * 50 + self.x
    }

    #[inline]
    pub fn range(&self, b: &Pos) -> Option<i32> {
        (self.room == b.room).then(|| (self.x - b.x).abs().max((self.y - b.y).abs()))
    }

    #[inline]
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

    #[inline]
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

    #[inline]
    pub fn amount_of(&self, resource: &Resource) -> i32 {
        self.held
            .iter()
            .filter_map(|(r, n)| (r == resource).then_some(n))
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

#[derive(
    Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, EnumString, AsRefStr, EnumDiscriminants,
)]
#[strum(serialize_all = "camelCase")]
#[strum_discriminants(vis(pub(crate)))]
pub enum Kind {
    Spawn,
    Extension,
    Tower,
    Road,
    Container,
    Storage,
    Link,
    Rampart,
    #[strum(serialize = "constructedWall")]
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
    #[strum(default, transparent)]
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
        KindDiscriminants::from(self) as i32
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
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Structure {
    pub id: Id,
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
    pub id: Id,
    pub kind: Kind,
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
    pub resource: Resource,
    pub amount: i32,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Pile {
    pub id: Id,
    pub pos: Pos,
    pub resource: Resource,
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
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct BodyPart {
    pub kind: Part,
    pub hits: i32,
    pub boost: Option<ResourceType>,
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Fallen {
    pub creep: Creep,
    pub died: i32,
}

/// A tombstone or a ruin.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Remains {
    pub id: Id,
    pub pos: Pos,
    pub store: Store,
    pub fallen: Option<Fallen>,
}

#[derive(Clone, Debug)]
pub struct Room<'a> {
    pub name: RoomName,
    /// `Game.map.getRoomTerrain(name)`, `None` where unread.
    pub terrain: Option<&'a LocalRoomTerrain>,
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

impl Room<'_> {
    pub fn hostiles(&self) -> impl Iterator<Item = &Creep> {
        self.others.iter().filter(|c| !c.owner.is_ally())
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
    /// Visible rooms.
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
    pub structures: BTreeMap<String, Vec<i32>>,
}

impl Rules {
    /// `CONTROLLER_STRUCTURES`; sites count with standing structures.
    #[inline]
    pub fn allowed(&self, kind: &Kind, level: i32) -> i32 {
        let row = self
            .structures
            .get(kind.as_ref())
            .map_or(&[][..], Vec::as_slice);
        let at = usize::try_from(level).map_or(None, |l| row.get(l).or(row.last()));
        at.copied().unwrap_or(0)
    }

    #[cfg(any(test, feature = "test-support", feature = "tools"))]
    #[allow(clippy::unwrap_used)]
    pub fn vanilla() -> Rules {
        use screeps::StructureType;
        let built = "spawn extension link road constructedWall rampart storage tower observer \
                     powerSpawn extractor terminal lab container nuker factory";
        let row = |kind: StructureType| (0..=8).map(move |l| kind.controller_structures(l) as i32);
        let structures = (built.split_whitespace())
            .map(|name| (name.to_string(), row(name.parse().unwrap()).collect()));
        Rules {
            structures: structures.collect(),
        }
    }
}

/// `read.js`'s pass decoded in the order it wrote it: the world bar what
/// only the engine's globals answer (the CPU limit, terrain, room status).
pub mod read {
    use std::collections::BTreeSet;
    use std::str::Split;

    use screeps::ResourceType;
    use screeps::game::map::RoomStatus;

    use crate::tiles::{self, BOTTOM, LEFT, RIGHT, TOP};

    use super::*;
    use crate::declarations::Declarations;
    use crate::engine;
    use crate::vocabulary::{self, Part};

    struct Reader<'a> {
        ints: &'a [i32],
        at: usize,
        strs: Split<'a, char>,
        words: Vec<&'a str>,
        me: String,
        declarations: &'a Declarations,
    }

    impl<'a> Reader<'a> {
        fn n(&mut self) -> i32 {
            self.at += 1;
            self.ints[self.at - 1]
        }

        fn flag(&mut self) -> bool {
            self.n() != 0
        }

        fn s(&mut self) -> String {
            self.str().to_string()
        }

        fn id(&mut self) -> Id {
            Id::of_engine(self.str())
        }

        /// The engine names every room in the grammar; a name outside it reads
        /// as `sim`'s, the one room the grid has no square for, not a panic.
        fn room_name(&mut self) -> RoomName {
            RoomName::parse(self.word()).unwrap_or(RoomName::SIM)
        }

        fn word(&mut self) -> &'a str {
            let i = self.n();
            self.words.get(i as usize).copied().unwrap_or_default()
        }

        /// A string only parsed: no copy.
        fn str(&mut self) -> &'a str {
            self.strs.next().unwrap_or_default()
        }

        fn list<T>(&mut self, each: impl FnMut(&mut Self) -> T) -> Vec<T> {
            self.list_into(Vec::new(), each)
        }

        fn list_into<T>(
            &mut self,
            mut out: Vec<T>,
            mut each: impl FnMut(&mut Self) -> T,
        ) -> Vec<T> {
            let count = self.n();
            out.extend((0..count).map(|_| each(self)));
            out
        }

        fn owner(&mut self) -> Owner {
            match self.n() {
                1 => Owner::Ours,
                2 => {
                    let name = self.word().to_string();
                    self.user(name)
                }
                _ => Owner::Nobody,
            }
        }

        fn user(&self, name: String) -> Owner {
            Owner::of_user(name, &self.me, self.declarations)
        }

        fn pos(&mut self) -> Pos {
            let room = self.room_name();
            let x = self.n();
            let y = self.n();
            Pos::new(room, x, y)
        }

        fn pos_in(&mut self, room: RoomName) -> Pos {
            let x = self.n();
            let y = self.n();
            Pos::new(room, x, y)
        }

        /// Only the resources `screeps-game-api` names count, as its `store_types` read them.
        fn store(&mut self) -> Store {
            let count = self.n();
            let held = (0..count)
                .map(|_| (self.word(), self.n()))
                .filter(|(name, amount)| *amount > 0 && name.parse::<ResourceType>().is_ok())
                .map(|(name, amount)| (vocabulary::resource_of_name(name), amount))
                .collect();
            Store {
                held,
                capacity: self.n(),
            }
        }

        fn body(&mut self) -> Vec<BodyPart> {
            self.list(|r| {
                let kind = r.word().parse().unwrap_or(Part::Tough);
                let hits = r.n();
                let boost = r.word();
                BodyPart {
                    kind,
                    hits,
                    boost: boost.parse().ok(),
                }
            })
        }

        fn creep(&mut self) -> Creep {
            Creep {
                id: self.id(),
                name: self.s(),
                owner: self.owner(),
                pos: self.pos(),
                body: self.body(),
                hits: self.n(),
                hits_max: self.n(),
                ticks_to_live: self.n(),
                fatigue: self.n(),
                store: self.store(),
            }
            .clocked()
        }

        fn controller(&mut self) -> Controller {
            Controller {
                id: self.id(),
                pos: self.pos(),
                owner: self.owner(),
                level: self.n(),
                progress: self.n(),
                progress_total: self.n(),
                ticks_to_downgrade: self.n(),
                reservation: self.flag().then(|| {
                    let user = self.word().to_string();
                    (self.user(user), self.n())
                }),
                safe_mode: self.n(),
                safe_mode_available: self.n(),
                safe_mode_cooldown: self.n(),
                upgrade_blocked: self.n(),
            }
        }

        fn structure(&mut self, room: RoomName) -> Structure {
            let kind = Kind::from(self.word());
            let id = self.id();
            let pos = self.pos_in(room);
            let owner = self.owner();
            let hits = self.n();
            let hits_max = self.n();
            let store = if kind.has_store() {
                self.store()
            } else {
                Store::empty()
            };
            let cooldown = if kind.has_cooldown() { self.n() } else { 0 };
            let spawn = (kind == Kind::KeeperLair)
                .then(|| self.n())
                .filter(|t| *t >= 0);
            let (collapse, level) = if kind == Kind::InvaderCore {
                let running = self.flag();
                let ticks = self.n();
                let collapse = if running {
                    ticks
                } else {
                    ticks + engine::STRONGHOLD_LONGEST_COLLAPSE
                };
                (collapse, self.n())
            } else {
                (0, 0)
            };
            let walkable = if kind == Kind::Rampart {
                let public = self.flag();
                owner.is_ours() || public
            } else {
                kind.walkable()
            };
            Structure {
                id,
                kind,
                pos,
                owner,
                hits,
                hits_max,
                store,
                cooldown,
                collapse,
                level,
                spawn,
                walkable,
            }
        }

        /// A tombstone's creep keeps no hits or fatigue.
        fn remains(&mut self) -> Remains {
            Remains {
                id: self.id(),
                pos: self.pos(),
                store: self.store(),
                fallen: self.flag().then(|| {
                    let mut creep = self.creep();
                    creep.hits = 0;
                    creep.fatigue = 0;
                    Fallen {
                        creep,
                        died: self.n(),
                    }
                }),
            }
        }

        fn room(&mut self, buffers: &mut Buffers) -> Room<'static> {
            let name = self.room_name();
            let lists = buffers.rooms.pop().unwrap_or_default();
            Room {
                terrain: None,
                energy_available: self.n(),
                energy_capacity: self.n(),
                controller: self.flag().then(|| self.controller()),
                sources: self.list_into(lists.sources, |r| Source {
                    id: r.id(),
                    pos: r.pos(),
                    energy: r.n(),
                    energy_capacity: r.n(),
                    ticks_to_regeneration: r.n(),
                }),
                minerals: self.list_into(lists.minerals, |r| Mineral {
                    id: r.id(),
                    pos: r.pos(),
                    resource: vocabulary::resource_of_name(r.word()),
                    amount: r.n(),
                }),
                structures: self.list_into(lists.structures, |r| r.structure(name)),
                sites: self.list_into(lists.sites, |r| Site {
                    kind: Kind::from(r.word()),
                    id: r.id(),
                    pos: r.pos_in(name),
                    owner: r.owner(),
                    progress: r.n(),
                    progress_total: r.n(),
                }),
                piles: self.list_into(lists.piles, |r| Pile {
                    id: r.id(),
                    pos: r.pos(),
                    resource: vocabulary::resource_of_name(r.word()),
                    amount: r.n(),
                }),
                remains: self.list_into(lists.remains, Reader::remains),
                others: self.list_into(lists.others, Reader::creep),
                name,
            }
        }

        fn spawn(&mut self) -> Spawn {
            Spawn {
                id: self.id(),
                name: self.s(),
                pos: self.pos(),
                spawning: self.flag().then(|| Oven {
                    name: self.s(),
                    remaining: self.n(),
                    body: self.body(),
                }),
            }
        }
    }

    #[derive(Default)]
    pub struct Buffers {
        rooms: Vec<Lists>,
        spawns: Vec<Spawn>,
        creeps: Vec<Creep>,
        attacks: Vec<(Id, Id)>,
        destroyed: Vec<(RoomName, Id, i32)>,
    }

    #[derive(Default)]
    struct Lists {
        sources: Vec<Source>,
        minerals: Vec<Mineral>,
        structures: Vec<Structure>,
        sites: Vec<Site>,
        piles: Vec<Pile>,
        remains: Vec<Remains>,
        others: Vec<Creep>,
    }

    fn emptied<T>(mut list: Vec<T>) -> Vec<T> {
        list.clear();
        list
    }

    impl Buffers {
        pub fn keep(&mut self, world: World) {
            for room in world.rooms {
                self.rooms.push(Lists {
                    sources: emptied(room.sources),
                    minerals: emptied(room.minerals),
                    structures: emptied(room.structures),
                    sites: emptied(room.sites),
                    piles: emptied(room.piles),
                    remains: emptied(room.remains),
                    others: emptied(room.others),
                });
            }
            (self.spawns, self.creeps) = (emptied(world.spawns), emptied(world.creeps));
            (self.attacks, self.destroyed) = (emptied(world.attacks), emptied(world.destroyed));
        }
    }

    pub fn world(
        ints: &[i32],
        text: &str,
        declarations: &Declarations,
        buffers: &mut Buffers,
    ) -> World<'static> {
        let count = ints.last().map_or(0, |n| *n as usize);
        let mut words: Vec<&str> = text.rsplit('\0').take(count).collect();
        words.reverse();
        let mut r = Reader {
            ints,
            at: 0,
            strs: text.split('\0'),
            words,
            me: String::new(),
            declarations,
        };
        r.me = r.s();
        let time = r.n();
        let bucket = r.n();
        let rooms = r.list(|r| r.room(buffers));
        let attacks = r.list_into(std::mem::take(&mut buffers.attacks), |r| (r.id(), r.id()));
        let destroyed = r.list_into(std::mem::take(&mut buffers.destroyed), |r| {
            (r.room_name(), r.id(), r.n())
        });
        let spawns = r.list_into(std::mem::take(&mut buffers.spawns), Reader::spawn);
        let creeps = r.list_into(std::mem::take(&mut buffers.creeps), Reader::creep);
        World {
            time,
            me: r.me.clone(),
            cpu_limit: 0.0,
            bucket,
            rooms,
            spawns,
            creeps,
            our_sites: r.n(),
            rooms_allowed: r.n(),
            rules: Rules {
                structures: r
                    .list(|r| (r.word().to_string(), r.list(Reader::n)))
                    .into_iter()
                    .collect(),
            },
            closed: BTreeSet::new(),
            attacks,
            destroyed,
        }
    }

    /// The rooms in reach the engine keeps us out of: those whose status is
    /// not our spawn's room's.
    pub fn closed(
        world: &World,
        declarations: &Declarations,
        mut status: impl FnMut(RoomName) -> Option<RoomStatus>,
    ) -> BTreeSet<RoomName> {
        let ours = world.spawns.first().and_then(|s| status(s.pos.room));
        let read = (world.rooms.iter())
            .flat_map(|room| {
                let around = [TOP, RIGHT, BOTTOM, LEFT].map(|e| tiles::room_toward(room.name, e));
                around.into_iter().flatten().chain([room.name])
            })
            .chain(declarations.outposts.iter().map(|o| o.room))
            .chain(declarations.claims.iter().map(|o| o.room))
            .chain(declarations.sieges.iter().copied())
            .collect::<BTreeSet<RoomName>>();
        read.into_iter()
            .filter(|room| ours.is_some() && status(*room).is_some_and(|s| Some(s) != ours))
            .collect()
    }
}

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
        terrain.is_some_and(|t| {
            let t = t.get_bits()[(y * 50 + x) as usize];
            t & WALL == 0 && t & SWAMP != 0
        })
    }
}

pub mod hostiles {
    use super::{Creep, Part, Room, RoomName};
    use crate::engine;
    use std::collections::BTreeSet;

    pub fn works(creep: &Creep) -> bool {
        (creep.body.iter())
            .any(|p| p.kind == Part::Work || p.kind == Part::Carry || p.kind == Part::Claim)
    }

    pub fn strikes(creep: &Creep) -> bool {
        creep
            .body
            .iter()
            .any(|p| p.kind == Part::Attack || p.kind == Part::RangedAttack)
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Seen {
    pub gone: i32,
    pub armed: bool,
}

pub type Sightings = BTreeMap<RoomName, BTreeMap<Id, Seen>>;

pub mod sightings {
    use super::{Creep, Id, Kind, RoomName, Seen, Sightings, World, hostiles};
    use std::collections::BTreeMap;

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
                        },
                    );
                }
            }

            for s in &room.structures {
                if s.kind == Kind::InvaderCore {
                    seen.insert(
                        s.id,
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
        (sightings.get(&room))
            .is_none_or(|seen| seen.values().all(|s| s.gone <= time || !counts(s)))
    }

    pub fn clear(sightings: &Sightings, time: i32, room: RoomName) -> bool {
        none(|_| true, sightings, time, room)
    }

    pub fn unarmed(sightings: &Sightings, time: i32, room: RoomName) -> bool {
        none(|s| s.armed, sightings, time, room)
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

    /// Legs and a hold alone fight for nothing; only a home's towers heal them back.
    pub fn spent(&self) -> bool {
        !(self.body.iter())
            .any(|p| p.hits > 0 && !matches!(p.kind, Part::Move | Part::Tough | Part::Carry))
    }
}

#[inline]
pub fn live(body: &[BodyPart], kind: Part) -> i32 {
    (body.iter())
        .filter(|p| p.hits > 0 && p.kind == kind)
        .count() as i32
}

#[inline]
pub fn cast(body: &[BodyPart], kind: Part) -> i32 {
    (body.iter()).filter(|p| p.kind == kind).count() as i32
}

impl World<'_> {
    #[inline]
    pub fn room(&self, name: RoomName) -> Option<&Room<'_>> {
        self.rooms.iter().find(|r| r.name == name)
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
            (r.remains).retain(|o| ok(&o.id) && o.fallen.as_ref().is_none_or(|f| ok(&f.creep.id)));
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

/// A creep's body as the engine prices it.
pub fn body_cost(creep: &Creep) -> i32 {
    engine::body_cost(&body_kinds(creep))
}

#[cfg(test)]
mod tests {
    use super::{Id, Kind};

    #[test]
    fn a_kind_the_engine_adds_keeps_its_name() {
        assert_eq!(Kind::from("foo").as_ref(), "foo");
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
