/// Below this file nothing in `Economy` reads `World` (`ARCHITECTURE.md` §1).
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

use fabot_atlas::{self as atlas, Atlas, Tree};
use fabot_base::HashMap;
use fabot_base::boost::{Boosting, Load};
use fabot_base::declarations::{self, Declarations, OpKind, Outpost};
use fabot_base::engine;
use fabot_base::grid::Footprint;
use fabot_base::intel::{self, Intel, Scout};
use fabot_base::tiles::{self, Tile};
use fabot_base::vocabulary::ResourceType::{self, Energy};
use fabot_base::vocabulary::{Part, part_count_in};
use fabot_base::world::{
    self, BodyPart, Built, Controller, Creep, Id, Owner, Pos, Room, RoomName, Rules, Source, Spawn,
    Store, Structure, StructureType, World, terrain,
};

use super::balance::{Holding, Level, Shipment, balance, takes, want};
use super::bodies::{self, Pattern};
use super::task::Target;

#[derive(Clone, Debug)]
pub struct Body {
    /// None while it spawns.
    pub id: Option<Id>,
    pub name: String,
    pub pos: Pos,
    /// Live parts: a part shot to no hits does nothing.
    pub work: i32,
    pub carry: i32,
    pub moves: i32,
    pub claim: i32,
    /// Dead parts counted: the length a successor is spawned at.
    pub size: i32,
    /// Dead parts counted too: a wound a tower heals never makes a body too
    /// small for its seat.
    pub cast_work: i32,
    pub cast_carry: i32,
    pub store: Store,
    pub energy: i32,
    pub free: i32,
    pub ticks_to_live: i32,
    pub pattern: Pattern,
}

#[derive(Clone, Debug)]
pub struct SourceFact {
    pub id: Id,
    pub pos: Pos,
    pub rate: i32,
    pub post: Option<Pos>,
    pub container: Option<Id>,
    pub link: Option<Id>,
    pub seats: Option<i32>,
    pub energy: i32,
    /// What its bay's extensions hold full: its container keeps it for them.
    pub bay: i32,
    /// What every income and flow is read off, never `rate` alone.
    pub dug: i32,
}

#[derive(Clone, Debug)]
pub struct TerminalFact {
    pub id: Id,
    pub pos: Pos,
    pub store: Store,
    pub cooldown: i32,
}

#[derive(Clone, Debug)]
pub struct LabFact {
    pub id: Id,
    pub pos: Pos,
    /// Its one mineral and how much of it.
    pub held: Option<(ResourceType, i32)>,
    pub cooldown: i32,
}

/// A deposit our extractor stands on.
#[derive(Clone, Debug)]
pub struct MineralFact {
    pub id: Id,
    pub pos: Pos,
    pub resource: ResourceType,
    pub amount: i32,
    pub cooldown: i32,
    pub post: Option<Pos>,
    pub container: Option<Id>,
    pub held: i32,
}

#[derive(Clone, Debug)]
pub struct RefillFact {
    pub id: Id,
    pub pos: Pos,
    pub kind: StructureType,
    pub free: i32,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum StoreKind {
    Container,
    Storage,
    Buffer,
    Remains,
    Pile,
}

impl StoreKind {
    pub fn is_buffer(self) -> bool {
        self == StoreKind::Buffer
    }
    pub fn is_remains(self) -> bool {
        self == StoreKind::Remains
    }
    pub fn is_pile(self) -> bool {
        self == StoreKind::Pile
    }
}

#[derive(Clone, Debug)]
pub struct StoreFact {
    pub id: Target,
    pub pos: Pos,
    pub kind: StoreKind,
    pub energy: i32,
    pub free: i32,
    pub source_dug: i32,
    pub owned: bool,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LinkRole {
    Source,
    Controller,
    Hub,
}

impl LinkRole {
    pub fn is_source(self) -> bool {
        self == LinkRole::Source
    }
    pub fn is_controller(self) -> bool {
        self == LinkRole::Controller
    }
    pub fn is_hub(self) -> bool {
        self == LinkRole::Hub
    }
}

#[derive(Clone, Debug)]
pub struct LinkFact {
    pub id: Id,
    pub pos: Pos,
    pub role: LinkRole,
    pub energy: i32,
    pub free: i32,
    pub cooldown: i32,
}

#[derive(Clone, Debug)]
pub struct SiteFact {
    pub id: Id,
    pub kind: StructureType,
    pub pos: Pos,
    pub left: i32,
}

#[derive(Clone, Debug)]
pub struct RepairFact {
    pub id: Id,
    pub kind: StructureType,
    pub pos: Pos,
    pub hits: i32,
    pub hits_max: i32,
    pub decay: f64,
    /// Hits one decay takes; when the next falls is unseen.
    pub chunk: i32,
    /// Hits a tick worn off by each energy a tick hauled across it.
    pub wear: f64,
    /// Hits last tick's hostile strikers took off it.
    pub struck: i32,
    pub rebuild: i32,
}

impl RepairFact {
    pub fn loss(&self, haul: i32) -> f64 {
        self.decay + self.wear * f64::from(haul)
    }

    /// Lower first: 0 if it may fall next tick, else 1 + rampart decay periods
    /// past the next decay, capped at a life. Unbucketed ticks walked builders across the ring.
    pub fn urgency(&self, haul: i32) -> i32 {
        let lasts =
            f64::from((self.hits - self.chunk).max(0)) / (self.loss(haul) + f64::from(self.struck));
        let life = f64::from(engine::CREEP_LIFETIME);
        if lasts < 1.0 {
            0
        } else {
            1 + (lasts.min(life) / f64::from(engine::RAMPART_DECAY_TIME)) as i32
        }
    }

    /// Mended ahead of upgrade and sites.
    pub fn urgent(&self, haul: i32) -> bool {
        self.urgency(haul) < SAFE_PERIODS
    }

    pub fn at_risk(&self, haul: i32) -> bool {
        f64::from(self.hits) < self.loss(haul) * f64::from(engine::CREEP_LIFETIME)
    }
}

#[derive(Clone, Debug)]
pub struct PileFact {
    pub id: Id,
    pub pos: Pos,
    pub resource: ResourceType,
    pub amount: i32,
}

/// A room the economy works: a colony's own, or one declared to it.
#[derive(Clone, Debug)]
pub struct Ground {
    pub room: RoomName,
    pub raided: bool,
    pub controller: Option<Controller>,
    pub sources: Vec<SourceFact>,
    pub minerals: Vec<MineralFact>,
    pub refills: Vec<RefillFact>,
    pub bays: Vec<RefillFact>,
    pub stores: Vec<StoreFact>,
    pub sites: Vec<SiteFact>,
    pub repairs: Vec<RepairFact>,
    pub piles: Vec<PileFact>,
    /// The engine accepts our reservation or claim on its controller, or a
    /// claimer attacks the Invader's hold off it.
    pub reservable: bool,
    pub our_reservation: i32,
    pub upgrade_seats: Option<i32>,
    pub buffer: Option<StoreFact>,
    /// The buffer's tile, built (a link or container) or still a container
    /// site, so the site ranks first in a nursed room.
    pub buffer_tile: Option<Pos>,
    pub feed: Option<StoreFact>,
    pub links: Vec<LinkFact>,
    pub receiver: Option<LinkFact>,
    pub buffer_seats: Vec<Pos>,
    pub labs: Vec<LabFact>,
    /// What our storage and terminal hold besides energy: (store, its tile,
    /// resource, amount).
    pub ores: Vec<(Id, Pos, ResourceType, i32)>,
    /// Remains' non-energy: (remains, its tile, resource, amount).
    pub loot: Vec<(Id, Pos, ResourceType, i32)>,
    /// Only a cluster's home has one.
    pub lab_plan: Option<super::labs::Plan>,
    /// The lab a cast waiting at home is boosted from, and what its parts
    /// still ask of the compound and the energy to spend with it.
    pub boost_lab: Option<BoostLab>,
    pub terminal: Option<TerminalFact>,
    /// What this colony sends, and the colony and its terminal's free room.
    pub shipment: Vec<(ResourceType, i32)>,
    pub ship_to: Option<(RoomName, i32)>,
    pub abandoned: bool,
}

impl Ground {
    pub fn has_storage(&self) -> bool {
        self.stores.iter().any(|s| s.owned)
    }

    pub fn banks_on_pile(&self) -> bool {
        self.bank().is_some_and(|b| b.kind.is_pile()) && self.terminal.is_none()
    }

    fn is_terminal(&self, s: &StoreFact) -> bool {
        self.terminal.as_ref().is_some_and(|t| s.id == t.id.into())
    }

    pub fn banks(&self, s: &StoreFact) -> bool {
        s.owned || self.is_terminal(s)
    }

    pub fn bank(&self) -> Option<&StoreFact> {
        self.stores.iter().find(|s| s.owned || s.kind.is_pile())
    }

    /// Both full, the storage with no room: no carrier is sent to either.
    pub fn deposit(&self) -> Option<&StoreFact> {
        let bank = self.bank()?;
        let open = || (self.stores.iter()).find(|s| self.is_terminal(s) && s.free > 0);
        (bank.free > 0).then_some(bank).or_else(open).or(Some(bank))
    }

    /// The energy the terminal's shipment takes, sent and fees, a hundred over
    /// as the fee rounds up per send.
    pub fn shipped(&self) -> i32 {
        let Some((home, _)) = &self.ship_to else {
            return 0;
        };
        let apart = tiles::rooms_apart(self.room, *home);
        let fee: i32 = (self.shipment.iter())
            .map(|(r, n)| engine::send_cost(*n, apart) + if *r == Energy { *n } else { 0 })
            .sum();
        if fee > 0 { fee + 100 } else { 0 }
    }

    /// What of `s` is the stock: a terminal's past what its shipment takes.
    fn own(&self, s: &StoreFact) -> i32 {
        if self.is_terminal(s) {
            0.max(s.energy - self.shipped())
        } else {
            s.energy
        }
    }

    pub fn stock(&self) -> Option<i32> {
        self.held(|s| self.banks(s), |s| self.own(s))
    }

    pub fn stock_and_shipment(&self) -> Option<i32> {
        self.held(|s| self.banks(s), |s| s.energy)
    }

    fn held(&self, of: impl Fn(&StoreFact) -> bool, n: impl Fn(&StoreFact) -> i32) -> Option<i32> {
        let held = (self.stores.iter()).filter(|s| of(s)).map(n);
        self.has_storage().then(|| held.sum())
    }

    /// What a worker may draw from `s`: the stock past the reserve, the
    /// storage's first and the terminal's after, so the two never give it twice.
    pub fn spare(&self, s: &StoreFact, reserve: i32) -> i32 {
        if !self.banks(s) {
            return s.energy;
        }
        let past = self.stock().unwrap_or(0) - reserve;
        let from_storage = self.held(|s| s.owned, |s| s.energy).unwrap_or(0).min(past);
        if s.owned {
            from_storage
        } else {
            self.own(s).min(past - from_storage.max(0))
        }
    }

    /// Hivemind's `link-network.ts`.
    pub fn link_net(&self) -> LinkNet {
        let room = |by: &dyn Fn(&LinkFact) -> bool| -> i32 {
            (self.links.iter().filter(|l| by(l)))
                .map(|l| l.energy + l.free)
                .sum()
        };
        let least = room(&|l| l.role.is_controller());
        LinkNet {
            energy: self.links.iter().map(|l| l.energy).sum(),
            least,
            most: least + room(&|l| l.role.is_hub()),
        }
    }

    /// A site that waits pays back nothing, so while one stands its builders
    /// draw the reserve.
    pub fn kept_from_builders(&self, reserve: i32) -> i32 {
        if self.sites.iter().any(|s| drawn_for(s.kind)) {
            0
        } else {
            reserve
        }
    }
}

/// A rampart guards the reserve and costs one energy.
pub fn drawn_for(kind: StructureType) -> bool {
    kind.pays_back() || kind == StructureType::Rampart
}

/// The lab a waiting body walks to, and what the colony's stores owe it.
#[derive(Clone, Debug)]
pub struct BoostLab {
    pub id: Id,
    pub pos: Pos,
    pub short: Load,
}

pub struct LinkNet {
    pub energy: i32,
    /// Every controller link full.
    pub least: i32,
    /// Every hub full too.
    pub most: i32,
}

/// A declared room's loot in sight, hauled by the colony nearest it.
#[derive(Clone, Debug)]
pub struct Pickup {
    pub colony: usize,
    pub loot: Vec<(Id, Pos, ResourceType, i32)>,
}

fn reached(atlas: &Atlas, at: &Pos) -> bool {
    let here = tiles::of_pos(at);
    std::iter::once(&here)
        .chain(tiles::around(here))
        .any(|t| atlas.passable(at.room, *t))
}

fn pickups_of(
    (world, atlas): (&World, &Atlas),
    declarations: &Declarations,
    ours: &atlas::Derived,
    (colonies, grounds): (&[Colony], &[Ground]),
) -> Vec<Pickup> {
    let crossed = |ci: usize, room| {
        let home = colonies[ci].room;
        ours.tree(home)?.crossings(home, room)
    };
    (declarations
        .rooms(OpKind::Pickup)
        .filter_map(|r| world.room(r)))
    .filter_map(|room| {
        let walled = |at: &Pos| {
            (room.structures.iter())
                .any(|s| s.kind == StructureType::Rampart && s.pos == *at && !s.owner.is_ours())
        };
        let containers = (room.structures.iter())
            .filter(|s| s.kind == StructureType::Container && !s.owner.is_ours())
            .map(|s| (s.id, s.pos, &s.store));
        let held = (room.remains.iter().map(|r| (r.id, r.pos, &r.store)))
            .chain(containers)
            .filter(|(_, at, _)| !walled(at) && reached(atlas, at));
        let loot: Vec<(Id, Pos, ResourceType, i32)> = held
            .flat_map(|(id, pos, store)| store.held.iter().map(move |(x, n)| (id, pos, *x, *n)))
            .filter(|(_, _, x, n)| *x != Energy && *n > 0)
            .collect();
        // `jobs` fetches only for a bank with room for the load: one with none idles a carrier.
        let least = loot.iter().map(|l| l.3).min()?;
        let takes = |ci: usize| (grounds[ci].bank()).is_some_and(|b| b.owned && b.free >= least);
        let colony = (0..colonies.len())
            .filter(|ci| takes(*ci))
            .filter_map(|ci| Some((crossed(ci, room.name)?, ci)))
            .min()?
            .1;
        Some(Pickup { colony, loot })
    })
    .collect()
}

/// A resource, not a decider (`ARCHITECTURE.md` §2).
#[derive(Clone, Debug)]
pub struct Colony {
    pub room: RoomName,
    pub controller: Controller,
    pub spawns: Vec<Spawn>,
    pub bank: i32,
    pub bank_capacity: i32,
    pub reserve: i32,
    pub owes: bool,
}

#[derive(Clone, Debug)]
pub struct Derived<'t> {
    pub time: i32,
    /// Colony `i`'s room is `grounds[i]`.
    pub colonies: Vec<Colony>,
    pub grounds: Vec<Ground>,
    pub claims: Vec<Controller>,
    /// Each room's centre, its age and whether a scout is cast for it.
    pub scouts: Vec<(Pos, i32, bool)>,
    pub bodies: Vec<Body>,
    /// Hold their slots as if they stood, so no slot is cast twice.
    pub ovens: Vec<Body>,
    /// From each room a spawn or body stands in, `Atlas::tree`'s.
    routes: BTreeMap<RoomName, Cow<'t, Tree>>,
    /// The declared outposts a foe stands down this tick.
    pub closed: Vec<Outpost>,
    pub spots: HashMap<Target, Pos>,
    pub pickups: Vec<Pickup>,
    pub ours: &'t atlas::Derived,
    abandoning: Vec<RoomName>,
    minerals: Vec<Shipment>,
}

impl Derived<'_> {
    pub fn route(&self, room: RoomName) -> Option<&Tree> {
        self.routes.get(&room).map(AsRef::as_ref)
    }

    pub fn haul(&self, owners: &[Option<usize>], gi: usize) -> i32 {
        let home = gi < self.colonies.len();
        (self.grounds.iter().zip(owners).enumerate())
            .filter(|(gj, (_, o))| if home { **o == Some(gi) } else { *gj == gi })
            .flat_map(|(_, (g, _))| &g.sources)
            .filter(|s| s.container.is_some() && s.link.is_none())
            .map(|s| s.dug)
            .sum()
    }
}

fn body_of(
    (id, name): (Option<Id>, &str),
    pos: &Pos,
    parts: &[BodyPart],
    store: &Store,
    life: i32,
) -> Body {
    let [work, carry, moves, claim] =
        [Part::Work, Part::Carry, Part::Move, Part::Claim].map(|k| world::live(parts, k));
    Body {
        id,
        name: name.to_string(),
        pos: *pos,
        work,
        carry,
        moves,
        claim,
        size: parts.len() as i32,
        cast_work: world::cast(parts, Part::Work),
        cast_carry: world::cast(parts, Part::Carry),
        store: store.clone(),
        energy: store.amount_of(Energy),
        free: store.free(),
        ticks_to_live: life,
        pattern: bodies::pattern_of_name(name)
            .unwrap_or_else(|| bodies::pattern_of_parts(work, carry, moves, claim)),
    }
}

fn reservable(c: &Controller) -> bool {
    c.owner.is_nobody() && c.reservation.as_ref().is_none_or(|(by, _)| by.is_ours())
}

pub fn invader_held(c: &Controller) -> bool {
    matches!(c.reservation, Some((Owner::Invader, _)))
}

/// v1's: a claimer attacks the Invader's hold off before it reserves.
fn takeable(c: &Controller) -> bool {
    reservable(c) || invader_held(c)
}

/// `None` where the Atlas counts none: unknown, not zero.
fn seats_at(atlas: &Atlas, range: i32, target: &Pos) -> Option<i32> {
    Some(atlas.seats(Footprint::One, range, target).len() as i32).filter(|n| *n > 0)
}

/// So the dig finishes the source inside its regeneration window despite the
/// ticks a body loses walking and dropping its load.
pub const HARVEST_SPARE_WORK: i32 = 1;

pub fn dig_work(rate: i32) -> i32 {
    (rate + engine::HARVEST_PER_WORK - 1) / engine::HARVEST_PER_WORK + HARVEST_SPARE_WORK
}

pub fn dug_at(capacity: i32, home: bool, rate: i32, posted: bool, seats: Option<i32>) -> i32 {
    let body = if !posted {
        bodies::sized(Pattern::Worker, engine::MAX_BODY_PARTS as i32, capacity)
    } else if home {
        bodies::home_harvester(dig_work(rate), capacity)
    } else {
        bodies::sized(Pattern::Harvester, dig_work(rate), capacity)
    };

    let Some(body) = body else { return 0 };
    // Not knowing a source's seats is no crew.
    let diggers = if posted { 1 } else { seats.unwrap_or(0) };
    rate.min(diggers * part_count_in(&body, Part::Work) * engine::HARVEST_PER_WORK)
}

/// Every bot's rampart margin in decay periods, the next counted: 3000 hits unstruck.
const SAFE_PERIODS: i32 = 10;

/// What keeping `s` standing costs, where it is below the hits we hold it at.
fn repair_of(room: &Room, wall: i32, owned_room: bool, s: &Structure) -> Option<RepairFact> {
    let per = |amount: i32, time: i32| f64::from(amount) / f64::from(time);
    let kind = s.kind;
    let (hits_max, chunk, decay, wear, rebuild) = match kind {
        StructureType::Rampart if s.owner.is_ours() => {
            let chunk = engine::RAMPART_DECAY_AMOUNT;
            let decay = per(chunk, engine::RAMPART_DECAY_TIME);
            (wall.min(s.hits_max), chunk, decay, 0.0, 0)
        }
        StructureType::Road => {
            let ratio = if terrain::is_swamp(room.terrain, s.pos.x, s.pos.y) {
                engine::ROAD_SWAMP_RATIO
            } else {
                1
            };
            let chunk = engine::ROAD_DECAY_AMOUNT * ratio;
            let block = bodies::block_of(Pattern::Hauler);
            let load = part_count_in(block, Part::Carry) * engine::CARRY_PART_CAPACITY;
            let steps = 2.0 / f64::from(load);
            let decay = per(chunk, engine::ROAD_DECAY_TIME);
            let wear = decay * steps * f64::from(block.len() as i32 * engine::ROAD_WEAROUT);
            (
                s.hits_max,
                chunk,
                decay,
                wear,
                engine::ROAD_BUILD_COST * ratio,
            )
        }
        StructureType::Container => {
            let period = if owned_room {
                engine::CONTAINER_DECAY_TIME_OWNED
            } else {
                engine::CONTAINER_DECAY_TIME
            };
            let chunk = engine::CONTAINER_DECAY_AMOUNT;
            (
                s.hits_max,
                chunk,
                per(chunk, period),
                0.0,
                engine::CONTAINER_BUILD_COST,
            )
        }
        _ => return None,
    };
    (s.hits < hits_max).then_some(RepairFact {
        id: s.id,
        kind,
        pos: s.pos,
        hits: s.hits,
        hits_max,
        decay,
        chunk,
        wear,
        struck: 0,
        rebuild,
    })
}

/// At its strikers' full live power: `World.attacks` drops the event's damage.
fn strikes_of(world: &World) -> BTreeMap<Id, i32> {
    let hostiles: BTreeMap<Id, &Creep> = (world.rooms.iter())
        .flat_map(|r| r.hostiles().map(|c| (c.id, c)))
        .collect();
    let mut strikes = BTreeMap::new();
    for (by, at) in &world.attacks {
        if let Some(c) = hostiles.get(by) {
            *strikes.entry(*at).or_default() += engine::ATTACK_POWER
                * world::power(c, Part::Attack)
                + engine::RANGED_ATTACK_POWER * world::power(c, Part::RangedAttack)
                + engine::DISMANTLE_POWER * world::power(c, Part::Work);
        }
    }
    strikes
}

/// The engine gates a link's send, never what it is sent: the controller's
/// link stays a receiver past the allowance.
fn links_of(
    room: &Room,
    working: &[&Structure],
    is_buffer: &dyn Fn(Id) -> bool,
    storage: Option<&Structure>,
) -> Vec<LinkFact> {
    let role_of = |s: &Structure| {
        if s.kind != StructureType::Link || !s.owner.is_ours() {
            None
        } else if Atlas::is_source_link(room, &s.pos) {
            Some(LinkRole::Source)
        } else if is_buffer(s.id) {
            Some(LinkRole::Controller)
        } else if storage.is_some_and(|st| st.pos.within(1, &s.pos)) {
            Some(LinkRole::Hub)
        } else {
            None
        }
    };
    (room.structures.iter())
        .filter_map(|s| {
            let role = role_of(s)?;
            let sends = working.iter().any(|w| w.id == s.id);
            (role.is_controller() || sends).then_some(LinkFact {
                id: s.id,
                pos: s.pos,
                role,
                energy: s.store.amount_of(Energy),
                free: s.store.free(),
                cooldown: s.cooldown,
            })
        })
        .collect()
}

fn minerals_of(room: &Room, working: &[&Structure]) -> Vec<MineralFact> {
    (room.minerals.iter())
        .filter_map(|m| {
            let extractor = working.iter().find(|s| {
                s.kind == StructureType::Extractor && s.owner.is_ours() && s.pos == m.pos
            })?;
            let resource = m.resource?;
            let post = Atlas::post_of(room, &m.pos);
            let container = post.as_ref().and_then(|(_, id)| *id);
            let held = container
                .as_ref()
                .and_then(|id| room.structures.iter().find(|s| s.id == *id))
                .map_or(0, |c| c.store.amount_of(resource));
            Some(MineralFact {
                id: m.id,
                pos: m.pos,
                resource,
                amount: m.amount,
                cooldown: extractor.cooldown,
                post: post.map(|(p, _)| p),
                container,
                held,
            })
        })
        .collect()
}

/// The stores a body draws energy from or fills: its containers, storage and
/// terminal, and any remains with energy. `buffered`: an owned room's buffer
/// container is its buffer.
fn stores_of(
    room: &Room,
    buffered: bool,
    is_buffer: &dyn Fn(Id) -> bool,
    served_dug: &dyn Fn(&Pos) -> i32,
) -> Vec<StoreFact> {
    use StructureType::*;
    let mut stores: Vec<StoreFact> = Vec::new();

    for s in &room.structures {
        match s.kind {
            Container | Storage | Terminal if s.kind != Terminal || s.owner.is_ours() => {
                let energy = s.store.amount_of(Energy);
                let owned = s.kind == Storage && s.owner.is_ours();
                let dug = if s.kind == Container {
                    served_dug(&s.pos)
                } else {
                    0
                };

                let kind = if matches!(s.kind, Storage | Terminal) {
                    StoreKind::Storage
                } else if buffered && is_buffer(s.id) {
                    StoreKind::Buffer
                } else {
                    StoreKind::Container
                };

                let send_room = if s.kind == Terminal { MINERAL_ROOM } else { 0 };
                if energy > 0 || owned || kind.is_buffer() || s.kind == Terminal {
                    stores.push(StoreFact {
                        id: Target::Object(s.id),
                        pos: s.pos,
                        kind,
                        energy,
                        free: 0.max(s.store.free() - send_room),
                        source_dug: dug,
                        owned,
                    })
                }
            }
            _ => (),
        }
    }

    for r in &room.remains {
        let energy = r.store.amount_of(Energy);

        if energy > 0 {
            stores.push(StoreFact {
                id: Target::Object(r.id),
                pos: r.pos,
                kind: StoreKind::Remains,
                energy,
                free: 0,
                source_dug: 0,
                owned: false,
            })
        }
    }
    stores
}

fn labs_of(working: &[&Structure]) -> Vec<LabFact> {
    working
        .iter()
        .filter(|s| s.kind == StructureType::Lab && s.owner.is_ours())
        .map(|s| LabFact {
            id: s.id,
            pos: s.pos,
            held: s
                .store
                .held
                .iter()
                .find(|(r, n)| *r != Energy && *n > 0)
                .copied(),
            cooldown: s.cooldown,
        })
        .collect()
}

fn ores_of(room: &Room) -> Vec<(Id, Pos, ResourceType, i32)> {
    use StructureType::*;
    room.structures
        .iter()
        .filter(|s| matches!(s.kind, Storage | Terminal) && s.owner.is_ours())
        .flat_map(|s| {
            s.store
                .held
                .iter()
                .filter(|(r, n)| fabot_base::vocabulary::is_mineral(*r) && *n > 0)
                .map(|(r, n)| (s.id, s.pos, *r, *n))
        })
        .collect()
}

/// The tiles an upgrader drinks from the buffer and reaches the controller.
fn buffer_seats_of(atlas: &Atlas, room: &Room, buffer: Option<&StoreFact>) -> Vec<Pos> {
    match (buffer, &room.controller) {
        (Some(b), Some(c)) if atlas.sees(room.name) => {
            let walkable = |t: Tile| atlas.passable(room.name, t) && !tiles::on_edge(t);
            Atlas::upgrader_seats(walkable, tiles::of_pos(&c.pos), tiles::of_pos(&b.pos))
                .into_iter()
                .map(|t| tiles::to_pos(room.name, t))
                .collect()
        }
        _ => Vec::new(),
    }
}

fn raided(room: &Room, owned: bool) -> bool {
    let foes: Vec<Creep> = room.hostiles().cloned().collect();
    let ours: BTreeSet<RoomName> = owned.then_some(room.name).into_iter().collect();
    (foes.iter()).any(|c| fabot_base::world::hostiles::armed(&ours, &foes, c))
}

fn ground_of(
    (atlas, rules): (&Atlas, &Rules),
    capacity: i32,
    rate: i32,
    room: &Room,
    wall: i32,
) -> Ground {
    let working: Vec<&Structure> = rules.working(room).collect();
    let owned_room = room.controller.as_ref().is_some_and(|c| c.owner.is_ours());
    let built = Atlas::buffer(room);
    let is_buffer = |id: Id| built.is_some_and(|b| b.id == id);
    let storage = room
        .structures
        .iter()
        .find(|s| s.kind == StructureType::Storage && s.owner.is_ours());

    let links = links_of(room, &working, &is_buffer, storage);

    let controller_link = links.iter().find(|l| l.role.is_controller()).cloned();
    let hub_link = links.iter().find(|l| l.role.is_hub()).cloned();

    let in_bay = Atlas::in_bay(room);
    let source_of = |s: &Source| {
        // An owned room's source is dug from the first bank, before its container's site.
        let post = Atlas::post_of(room, &s.pos).or_else(|| {
            let seats = owned_room.then(|| atlas.seats(Footprint::One, 1, &s.pos))?;
            Atlas::bare_post(room, seats).map(|p| (p, None))
        });
        let seats = seats_at(atlas, 1, &s.pos);
        SourceFact {
            id: s.id,
            pos: s.pos,
            rate,
            post: post.as_ref().map(|(p, _)| *p),
            container: post.as_ref().and_then(|(_, id)| *id),
            link: links
                .iter()
                .find(|l| l.role.is_source() && l.pos.within(2, &s.pos))
                .map(|l| l.id),
            seats,
            energy: s.energy,
            bay: (working.iter())
                .filter(|x| x.kind == StructureType::Extension && x.owner.is_ours())
                .filter(|x| post.as_ref().is_some_and(|(p, _)| p.within(1, &x.pos)))
                .map(|x| x.store.capacity)
                .sum(),
            dug: dug_at(capacity, owned_room, rate, post.is_some(), seats),
        }
    };
    let sources: Vec<SourceFact> = room.sources.iter().map(source_of).collect();

    let minerals = minerals_of(room, &working);

    let dug_of =
        |by: &dyn Fn(&SourceFact) -> bool| sources.iter().filter(|s| by(s)).map(|s| s.dug).sum();
    let served_dug = |pos: &Pos| -> i32 { dug_of(&|s| s.pos.range(pos) == Some(1)) };

    let (bays, refills): (Vec<RefillFact>, Vec<RefillFact>) = (working.iter())
        .filter(|s| {
            matches!(
                s.kind,
                StructureType::Spawn | StructureType::Extension | StructureType::Tower
            )
        })
        .filter(|s| s.owner.is_ours() && s.store.free() > 0)
        .map(|s| RefillFact {
            id: s.id,
            pos: s.pos,
            kind: s.kind,
            free: s.store.free(),
        })
        .partition(|r| in_bay(r.kind, &r.pos));
    let mut stores = stores_of(room, owned_room, &is_buffer, &served_dug);
    let repairs: Vec<RepairFact> = (room.structures.iter())
        .flat_map(|s| repair_of(room, wall, owned_room, s))
        .collect();

    let store_of = |kind: StoreKind, dug: i32, l: &LinkFact| StoreFact {
        id: Target::Object(l.id),
        pos: l.pos,
        kind,
        energy: l.energy,
        free: l.free,
        source_dug: dug,
        owned: false,
    };

    let feed = match (&controller_link, &hub_link) {
        (Some(c), hub) => {
            stores.push(store_of(StoreKind::Buffer, 0, c));
            hub.as_ref()
                .map(|hub| store_of(StoreKind::Container, 0, hub))
        }
        (None, Some(hub)) => {
            let dug: i32 = dug_of(&|s| s.link.is_some());

            if hub.energy > 0 {
                stores.push(store_of(StoreKind::Container, dug, hub));
            }

            None
        }
        (None, None) => None,
    };

    let buffer = stores.iter().find(|s| s.kind.is_buffer()).cloned();

    Ground {
        room: room.name,
        raided: raided(room, owned_room),
        controller: room.controller.clone(),
        sources,
        minerals,
        refills,
        bays,
        stores,
        sites: room
            .sites
            .iter()
            .filter(|s| s.owner.is_ours())
            .map(|s| SiteFact {
                id: s.id,
                kind: s.kind,
                pos: s.pos,
                left: 0.max(s.progress_total - s.progress),
            })
            .collect(),
        repairs,
        piles: room
            .piles
            .iter()
            .map(|p| PileFact {
                id: p.id,
                pos: p.pos,
                resource: p.resource,
                amount: p.amount,
            })
            .collect(),
        reservable: room.controller.as_ref().is_some_and(takeable),
        our_reservation: (room.controller.as_ref())
            .and_then(|c| c.reservation.as_ref())
            .filter(|(owner, _)| *owner == Owner::Ours)
            .map_or(0, |(_, ticks)| *ticks),
        upgrade_seats: match &room.controller {
            Some(c) => seats_at(atlas, engine::UPGRADE_RANGE, &c.pos),
            None => Some(0),
        },
        buffer_tile: built.map(|b| b.pos).or_else(|| {
            room.sites
                .iter()
                .find(|s| s.kind == StructureType::Container && Atlas::is_buffer(room, &s.pos))
                .map(|s| s.pos)
        }),
        feed: feed.or_else(|| buffer.clone()),
        links,
        receiver: controller_link.or(hub_link),
        labs: labs_of(&working),
        ores: ores_of(room),
        loot: (room.remains.iter())
            .flat_map(|r| r.store.held.iter().map(|(x, n)| (r.id, r.pos, *x, *n)))
            .filter(|(_, _, x, n)| *x != Energy && *n > 0)
            .collect(),
        lab_plan: None,
        boost_lab: None,
        terminal: (working.iter())
            .find(|s| s.kind == StructureType::Terminal && s.owner.is_ours())
            .map(|s| TerminalFact {
                id: s.id,
                pos: s.pos,
                store: s.store.clone(),
                cooldown: s.cooldown,
            }),
        shipment: Vec::new(),
        ship_to: None,
        abandoned: false,
        buffer_seats: buffer_seats_of(atlas, room, buffer.as_ref()),
        buffer,
    }
}

fn tree_of(routes: &mut BTreeMap<RoomName, Cow<'_, Tree>>, atlas: &Atlas, room: RoomName) {
    routes.entry(room).or_insert_with(|| {
        Cow::Owned(atlas.tree(&BTreeSet::new(), declarations::REACH_HOPS, room))
    });
}

fn free_at(grounds: &[Ground], room: RoomName) -> i32 {
    (grounds.iter().find(|g| g.room == room))
        .and_then(|g| g.terminal.as_ref())
        .map_or(0, |t| t.store.free())
}

fn minerals_dug_or_held(world: &World, colonies: &[Ground]) -> Vec<ResourceType> {
    let carried = (world.creeps.iter().filter(|c| c.owner.is_ours()))
        .flat_map(|c| c.store.held.iter().filter(|(_, n)| *n > 0).map(|(r, _)| *r));
    (colonies.iter())
        .flat_map(|g| {
            let held = (g.ores.iter().map(|o| o.2))
                .chain(g.labs.iter().filter_map(|l| l.held.map(|h| h.0)));
            (g.minerals.iter().map(|m| m.resource)).chain(held)
        })
        .chain(carried)
        .collect()
}

/// A load between a store and a lab is still the room's.
fn held_in(world: &World, g: &Ground, r: ResourceType) -> i32 {
    (g.ores.iter().filter(|o| o.2 == r).map(|o| o.3).sum::<i32>())
        + (g.labs.iter())
            .filter_map(|l| l.held.filter(|h| h.0 == r).map(|h| h.1))
            .sum::<i32>()
        + (world.creeps.iter())
            .filter(|c| c.owner.is_ours() && c.pos.room == g.room)
            .map(|c| c.store.amount_of(r))
            .sum::<i32>()
}

/// `roles` and `bench` are `None` with too few labs to react.
struct Cluster<'g> {
    ground: &'g Ground,
    boosting: &'g Boosting,
    kept: BTreeMap<Id, ResourceType>,
    roles: Option<fabot_base::boost::Roles>,
    bench: Option<super::labs::Bench>,
}

fn cluster_of<'g>(g: &'g Ground, boosting: &'g Boosting) -> Cluster<'g> {
    use fabot_base::boost;
    // The lab a body waiting at home is boosted from is the cast's and not
    // a reaction's: its compound is kept while the body still lacks it.
    let kept: BTreeMap<Id, ResourceType> = (boosting.waits.iter())
        .map(|w| (w.lab, w.load.compound))
        .collect();
    // Roles and rank off every lab, a kept one only leaving the outputs: else
    // a boost lent a lab moved the pair and stopped or unranked a running reaction.
    let labs: Vec<(Id, Pos)> = g.labs.iter().map(|l| (l.id, l.pos)).collect();
    let all = boost::roles_of(&labs);
    let outputs = all.as_ref().map_or(0, |r| r.outputs.len());
    let roles = all
        .filter(|r| !kept.contains_key(&r.reagents.0) && !kept.contains_key(&r.reagents.1))
        .map(|mut r| {
            r.outputs.retain(|o| !kept.contains_key(o));
            r
        });
    let bench = roles.as_ref().map(|roles| {
        let (a, b) = roles.reagents;
        super::labs::Bench {
            reagents_held: [a, b]
                .iter()
                .filter_map(|id| lab_held(g, *id).map(|h| h.0))
                .collect(),
            outputs,
        }
    });
    Cluster {
        ground: g,
        boosting,
        kept,
        roles,
        bench,
    }
}

fn lab_held(g: &Ground, id: Id) -> Option<(ResourceType, i32)> {
    g.labs.iter().find(|l| l.id == id).and_then(|l| l.held)
}

pub fn once(rs: impl IntoIterator<Item = ResourceType>) -> Vec<ResourceType> {
    let mut out = Vec::new();
    for r in rs {
        if !out.contains(&r) {
            out.push(r);
        }
    }
    out
}

/// A lab holds one mineral: a waiting body's booster is drained of others.
fn plan_cluster(
    world: &World,
    c: &Cluster,
    reaction: Option<super::labs::Reaction>,
) -> super::labs::Plan {
    let mut plan = super::labs::plan_for(reaction, c.roles.as_ref(), &|id| lab_held(c.ground, id));
    for (id, compound) in &c.kept {
        for (r, n) in lab_at(world, c.ground.room, *id)
            .iter()
            .flat_map(|l| &l.store.held)
        {
            if *n > 0 && r != compound && fabot_base::vocabulary::is_mineral(*r) {
                plan.drains.push((*id, *r));
            }
        }
    }
    plan
}

/// Only a store ships: a product another colony is short of leaves the labs
/// as soon as it makes a send, not once they stand half full.
fn drain_asked(
    plan: &mut super::labs::Plan,
    c: &Cluster,
    holdings: &[Holding],
    spent: &[ResourceType],
) {
    for l in &c.ground.labs {
        let Some((r, n)) = l.held else { continue };
        let asked = (holdings.iter()).any(|h| h.room != c.ground.room && h.level(r).short() > 0);
        let drained = plan.drains.iter().any(|d| d.0 == l.id);
        if asked && n >= engine::TERMINAL_MIN_SEND && !spent.contains(&r) && !drained {
            plan.drains.push((l.id, r));
        }
    }
}

fn lab_at<'a>(world: &'a World<'a>, home: RoomName, id: Id) -> Option<&'a world::Structure> {
    (world.rooms.iter())
        .filter(|r| r.name == home)
        .flat_map(|r| r.structures.iter())
        .find(|s| s.id == id)
}

/// The lab a waiting cast is boosted from is stocked from the home's stores:
/// the reaction refuses it as a lab of its own.
fn boost_lab(world: &World, home: RoomName, boosting: &Boosting) -> Option<BoostLab> {
    let mut waited: BTreeMap<Id, Load> = BTreeMap::new();
    for w in boosting.waits.iter().filter(|w| w.stands) {
        let at = waited.entry(w.lab).or_insert(Load {
            mineral: 0,
            energy: 0,
            ..w.load
        });
        at.mineral += w.load.mineral;
        at.energy += w.load.energy;
    }
    waited
        .into_iter()
        .filter_map(|(id, load)| {
            let lab = lab_at(world, home, id)?;
            let short = Load {
                mineral: load.mineral - lab.store.amount_of(load.compound),
                energy: load.energy - lab.store.amount_of(Energy),
                ..load
            };
            Some(BoostLab {
                id,
                pos: lab.pos,
                short,
            })
        })
        .find(|b| b.short.mineral > 0 || b.short.energy > 0)
}

struct Running {
    room: RoomName,
    labs: [Id; 2],
    reagents: [ResourceType; 2],
    /// What its product reacts with next, as much as it makes: it stays, so
    /// the step after runs here and it never goes out and back.
    partner: Option<(ResourceType, i32)>,
}

struct Ours<'a> {
    world: &'a World<'a>,
    grounds: &'a [Ground],
    given_up: &'a dyn Fn(RoomName) -> bool,
}

impl<'a> Ours<'a> {
    fn iter(&self) -> impl Iterator<Item = &'a Ground> + '_ {
        self.grounds.iter().filter(|g| !(self.given_up)(g.room))
    }

    fn held(&self, g: &Ground, r: ResourceType) -> i32 {
        held_in(self.world, g, r)
    }

    fn empire(&self, r: ResourceType) -> i32 {
        self.iter().map(|g| self.held(g, r)).sum()
    }
}

/// What our colonies hold and give, one reading for lab assignment and balance.
struct Stocks<'a> {
    ours: Ours<'a>,
    boosts: &'a [ResourceType],
    fights: &'a dyn Fn(RoomName) -> bool,
    running: Vec<Running>,
}

impl Stocks<'_> {
    /// A running reaction spends what its reagent labs hold, and keeps its
    /// partner; a product leaves its outputs once asked, so it counts.
    fn sendable(&self, g: &Ground, r: ResourceType) -> i32 {
        let held = self.ours.held(g, r);
        let here = self.running.iter().filter(|c| c.room == g.room);
        let in_labs: i32 = (here.clone().flat_map(|c| c.labs))
            .filter_map(|id| lab_held(g, id).filter(|h| h.0 == r))
            .map(|h| h.1)
            .sum();
        let partner: i32 = (here.filter_map(|c| c.partner))
            .filter(|(m, _)| *m == r)
            .map(|(_, n)| held.min(n))
            .sum();
        let through = (g.terminal.as_ref()).map_or(0, |t| t.store.amount_of(r) + t.store.free());
        (held - in_labs - partner).min(through)
    }

    fn level(&self, g: &Ground, r: ResourceType, reagents: &[ResourceType]) -> Level {
        Level {
            want: want(r, reagents, self.boosts, (self.fights)(g.room)),
            held: self.ours.held(g, r),
            shippable: self.sendable(g, r),
            boost: self.boosts.contains(&r),
        }
    }

    /// What `g` gives of `r` before assignment: what balance would ship were
    /// each cluster to keep the reaction its labs run.
    fn spare(&self, g: &Ground, r: ResourceType) -> i32 {
        let running = self.running.iter().find(|c| c.room == g.room);
        self.level(g, r, running.map_or(&[][..], |c| &c.reagents[..]))
            .spare()
    }
}

fn running_of(
    cluster: &[Cluster],
    wanted: &[super::labs::Reaction],
    targets: &[(ResourceType, i32)],
) -> Vec<Running> {
    let benches: Vec<_> = cluster.iter().map(|c| c.bench.as_ref()).collect();
    (cluster.iter().zip(super::labs::running(&benches, wanted)))
        .filter_map(|(c, running)| {
            let (product, reagents, amount) = running?;
            let roles = c.roles.as_ref()?;
            Some(Running {
                room: c.ground.room,
                labs: [roles.reagents.0, roles.reagents.1],
                reagents: *reagents,
                partner: super::labs::sibling(targets, *product).map(|s| (s, *amount)),
            })
        })
        .collect()
}

fn holdings_of(stocks: &Stocks, spent: &BTreeMap<RoomName, Vec<ResourceType>>) -> Vec<Holding> {
    let wanted = once(spent.values().flatten().chain(stocks.boosts).copied());
    let holding = |g: &Ground| {
        let t = g.terminal.as_ref()?;
        let reagents = spent.get(&g.room).map_or(&[][..], Vec::as_slice);
        Some(Holding {
            room: g.room,
            levels: wanted
                .iter()
                .map(|r| (*r, stocks.level(g, *r, reagents)))
                .collect(),
            energy: g.stock_and_shipment().unwrap_or(0),
            free: t.store.free(),
        })
    };
    stocks.ours.iter().filter_map(holding).collect()
}

fn assign_clusters(
    stocks: &Stocks,
    cluster: &[Cluster],
    wanted: &[super::labs::Reaction],
) -> Vec<Option<super::labs::Reaction>> {
    let bring = |i: usize, r: ResourceType| {
        let to = cluster[i].ground;
        let open = (to.terminal.as_ref()).is_some_and(|t| takes(t.store.free()));
        (stocks.ours.iter())
            .filter(|g| open && g.room != to.room)
            .map(|g| (stocks.spare(g, r), tiles::rooms_apart(g.room, to.room)))
            .chain([(stocks.ours.held(to, r), 0)])
            .filter(|(n, _)| *n >= engine::LAB_REACTION_AMOUNT)
            .map(|(_, apart)| apart)
            .min()
    };
    let benches: Vec<Option<&super::labs::Bench>> =
        cluster.iter().map(|c| c.bench.as_ref()).collect();
    super::labs::assign(wanted, &benches, &bring)
}

fn stock_labs(
    world: &World,
    grounds: &mut [Ground],
    colonies: usize,
    (derived, targets): (&atlas::Derived, &[(ResourceType, i32)]),
    (given_up, fights): (&dyn Fn(RoomName) -> bool, &dyn Fn(RoomName) -> bool),
) -> Vec<Shipment> {
    let ours = Ours {
        world,
        grounds: &grounds[..colonies],
        given_up,
    };
    let cluster: Vec<Cluster> = (derived.boosts.iter())
        .filter_map(|(c, b)| Some(cluster_of(ours.grounds.iter().find(|g| g.room == *c)?, b)))
        .collect();
    let boosts: Vec<ResourceType> = targets.iter().map(|(b, _)| *b).collect();
    let wanted = super::labs::wanted(targets, &|r| ours.empire(r));
    let stocks = Stocks {
        ours,
        boosts: &boosts,
        fights,
        running: running_of(&cluster, &wanted, targets),
    };
    let reactions = assign_clusters(&stocks, &cluster, &wanted);

    let spent: BTreeMap<RoomName, Vec<ResourceType>> = (cluster.iter().zip(&reactions))
        .map(|(c, r)| (c.ground.room, r.map_or_else(Vec::new, |r| r.1.to_vec())))
        .collect();
    let mut holdings = holdings_of(&stocks, &spent);
    let set: Vec<(RoomName, super::labs::Plan, Option<BoostLab>)> =
        (cluster.iter().zip(&reactions))
            .map(|(c, r)| {
                let mut plan = plan_cluster(world, c, *r);
                drain_asked(&mut plan, c, &holdings, &spent[&c.ground.room]);
                (
                    c.ground.room,
                    plan,
                    boost_lab(world, c.ground.room, c.boosting),
                )
            })
            .collect();

    for (home, plan, boost_lab) in set {
        if let Some(g) = grounds.iter_mut().find(|g| g.room == home) {
            g.lab_plan = Some(plan);
            g.boost_lab = boost_lab;
        }
    }
    balance(&mut holdings)
}

/// v1's `AidDonorMargin`: over a load and its fee.
const AID_MARGIN: i32 = 20_000;
/// Terminal room aid and deposits leave: energy filling it blocked every mineral send.
const MINERAL_ROOM: i32 = super::balance::BATCH;

struct Taker {
    room: RoomName,
    level: i32,
    held: i32,
    reserve: i32,
    free: i32,
}

impl Taker {
    /// A colony spending down to its reserve is not short.
    fn short(&self) -> Option<i32> {
        (2 * self.held < self.reserve).then_some(self.reserve - self.held)
    }
}

/// v1's energy aid. Held counts the terminal's, so stocking it never moves
/// the line; each send is booked on its taker, so givers never pile on one.
fn give_aid(
    colonies: &[Colony],
    grounds: &[Ground],
    given_up: &dyn Fn(RoomName) -> bool,
    shipping: &[Shipment],
) -> Vec<Shipment> {
    let mut takers: Vec<Taker> = (colonies.iter().zip(grounds.iter()))
        .filter(|(c, _)| !given_up(c.room))
        .filter_map(|(c, g)| {
            let t = g.terminal.as_ref()?;
            let held = g.stock_and_shipment()?;
            Some(Taker {
                room: c.room,
                level: c.controller.level,
                held,
                reserve: c.reserve,
                free: t.store.free(),
            })
        })
        .collect();
    let mut out = Vec::new();
    for g in grounds.iter().take(colonies.len()) {
        let Some(gi) = takers.iter().position(|t| t.room == g.room) else {
            continue;
        };
        let owed: i32 = g.sites.iter().map(|s| s.left).sum();
        let spare = takers[gi].held - takers[gi].reserve - AID_MARGIN - owed;
        if shipping.iter().any(|s| s.from == g.room) {
            continue;
        }
        let Some(ti) = recipient(&takers[gi], &takers) else {
            continue;
        };
        let to = &takers[ti];
        let sent = (spare.min(to.free - MINERAL_ROOM)).min(to.short().unwrap_or(i32::MAX));
        if sent < engine::TERMINAL_MIN_SEND {
            continue;
        }
        out.push(Shipment {
            from: g.room,
            to: to.room,
            taker_free: to.free,
            load: vec![(Energy, sent)],
        });
        takers[gi].held -= sent;
        takers[ti].held += sent;
        takers[ti].free -= sent;
    }
    out
}

/// Never to a colony holding more, so energy never flows back up. The nearest
/// pays the least fee.
fn recipient(giver: &Taker, takers: &[Taker]) -> Option<usize> {
    let top = engine::CONTROLLER_MAX_LEVEL;
    (takers.iter().enumerate())
        .filter(|(_, t)| t.held < giver.held)
        .filter_map(|(i, t)| {
            let apart = tiles::rooms_apart(giver.room, t.room);
            let rank = if let Some(short) = t.short() {
                (0, -short, apart)
            } else if giver.level == top && t.level < top {
                (1, t.level, apart)
            } else {
                return None;
            };
            Some(((rank, t.room), i))
        })
        .min_by_key(|(key, _)| *key)
        .map(|(_, i)| i)
}

/// At the rate the reserver holds, not this tick's: a body lives longer
/// than a lapse lasts. The engine refuses a harvest on another's hold.
fn remote_of(
    atlas: &Atlas,
    world: &World,
    intel: &Intel,
    declarations: &Declarations,
    richest: i32,
    outpost: &Outpost,
) -> Ground {
    let room: Cow<Room> = world.room(outpost.room).map_or_else(
        || Cow::Owned(intel::blind(outpost, intel, world, declarations)),
        Cow::Borrowed,
    );

    match &room.controller {
        Some(c) if !c.owner.is_ours() && !reservable(c) => {
            let mut bare = room.clone().into_owned();
            bare.sources = Vec::new();
            ground_of((atlas, &world.rules), richest, 0, &bare, 0)
        }
        _ if richest >= engine::body_cost(bodies::block_of(Pattern::Claimer)) => ground_of(
            (atlas, &world.rules),
            richest,
            engine::HELD_OUTPUT_PER_TICK,
            &room,
            0,
        ),
        _ => ground_of(
            (atlas, &world.rules),
            richest,
            engine::NEUTRAL_OUTPUT_PER_TICK,
            &room,
            0,
        ),
    }
}

fn ovens_of(world: &World) -> Vec<Body> {
    world
        .spawns
        .iter()
        .filter_map(|s| {
            s.spawning
                .as_ref()
                .filter(|oven| !declarations::fights(&oven.name))
                .map(|oven| {
                    body_of(
                        (None, &oven.name),
                        &s.pos,
                        &oven.body,
                        &Store::empty(),
                        engine::lifetime(oven.body.iter().map(|p| p.kind)),
                    )
                })
        })
        .collect()
}

/// The colonies' own facts, read once before Strategy picks the outposts.
#[derive(Clone, Debug)]
pub struct Homes {
    colonies: Vec<Colony>,
    grounds: Vec<Ground>,
    bodies: Vec<Body>,
    ovens: Vec<Body>,
    minerals: Vec<Shipment>,
}

pub fn homes(
    world: &World,
    creeps: &[&Creep],
    atlas: &Atlas,
    declarations: &Declarations,
    ours: &atlas::Derived,
) -> Homes {
    let colony_rooms: Vec<&Room> = (ours.colonies.iter())
        .filter_map(|name| world.room(*name))
        .collect();
    let plan_of = |room: RoomName| (declarations.colonies.iter()).find(|p| p.room == room);
    let colonies: Vec<Colony> = (colony_rooms.iter())
        .filter_map(|room| {
            Some(Colony {
                room: room.name,
                reserve: 0,
                owes: false,
                controller: room.controller.clone()?,
                spawns: (world.working_spawns())
                    .filter(|s| s.pos.room == room.name)
                    .cloned()
                    .collect(),
                bank: room.energy_available,
                bank_capacity: room.energy_capacity,
            })
        })
        .collect();

    let mut grounds: Vec<Ground> = colony_rooms
        .iter()
        .map(|room| {
            ground_of(
                (atlas, &world.rules),
                room.energy_capacity,
                engine::HELD_OUTPUT_PER_TICK,
                room,
                plan_of(room.name).map_or(0, |p| p.rampart_hits),
            )
        })
        .collect();

    for g in &mut grounds {
        if let Some(pos) = plan_of(g.room).and_then(|p| p.pile) {
            let energy: Vec<PileFact> = (g.piles)
                .extract_if(.., |p| p.pos == pos && p.resource == Energy)
                .collect();
            // A draw picks up the pile by its id; a drop needs none.
            let id = energy
                .first()
                .map_or(Target::Pile(g.room), |p| Target::Object(p.id));
            g.stores.push(StoreFact {
                id,
                pos,
                kind: StoreKind::Pile,
                energy: energy.iter().map(|p| p.amount).sum(),
                free: engine::STORAGE_CAPACITY,
                source_dug: 0,
                owned: false,
            });
        }
    }

    let given_up = |room: RoomName| declarations.abandoning(room);
    let mut minerals = Vec::new();
    if !ours.boosts.is_empty() {
        let targets = super::labs::targets(&minerals_dug_or_held(world, &grounds));
        minerals = stock_labs(
            world,
            &mut grounds,
            colonies.len(),
            (ours, &targets),
            (&given_up, &|room| plan_of(room).is_some_and(|p| p.fights)),
        );
    }

    for (i, room) in colony_rooms.iter().enumerate() {
        if given_up(room.name) {
            let to = grounds
                .iter()
                .filter(|g| g.terminal.is_some() && !given_up(g.room))
                .map(|g| (g.room, free_at(&grounds, g.room)))
                .max_by_key(|(_, free)| *free);
            super::abandon::evacuate(&mut grounds[i], room, &world.creeps, to);
        }
    }

    let bodies: Vec<Body> = creeps
        .iter()
        .map(|c| {
            body_of(
                (Some(c.id), &c.name),
                &c.pos,
                &c.body,
                &c.store,
                c.ticks_to_live,
            )
        })
        .collect();
    let mut homes = Homes {
        colonies,
        grounds,
        bodies,
        ovens: ovens_of(world),
        minerals,
    };
    homes.struck(world, 0);
    homes
}

impl Homes {
    /// A bay with no harvester beside to keep it is the carriers' (Hivemind's).
    fn struck(&mut self, world: &World, from: usize) {
        let strikes = strikes_of(world);
        let bodies = &self.bodies;
        for g in &mut self.grounds[from..] {
            for r in &mut g.repairs {
                r.struck = strikes.get(&r.id).copied().unwrap_or(0);
            }
            let kept = |r: &RefillFact| {
                (bodies.iter()).any(|b| {
                    b.pattern == Pattern::Harvester
                        && b.store.capacity > 0
                        && b.pos.within(1, &r.pos)
                })
            };
            g.refills.extend(g.bays.extract_if(.., |r| !kept(r)));
        }
    }
}

impl Derived<'_> {
    /// Aid ships again against the final reserves; an evacuation stays.
    pub fn into_homes(mut self) -> Homes {
        self.grounds.truncate(self.colonies.len());
        for g in &mut self.grounds {
            if !self.abandoning.contains(&g.room) {
                g.shipment.clear();
                g.ship_to = None;
            }
        }
        Homes {
            colonies: self.colonies,
            grounds: self.grounds,
            bodies: self.bodies,
            ovens: self.ovens,
            minerals: self.minerals,
        }
    }
}

pub fn of_world<'t>(
    homes: Homes,
    (world, creeps): (&World, &[&Creep]),
    atlas: &Atlas,
    declarations: &Declarations,
    ours: &'t atlas::Derived,
    (intel, scouts): (&Intel, &[Scout]),
) -> Derived<'t> {
    let mut homes = homes;
    let plan_of = |room: RoomName| (declarations.colonies.iter()).find(|p| p.room == room);
    for c in &mut homes.colonies {
        let plan = plan_of(c.room);
        c.reserve = plan.map_or(0, |p| p.reserve);
        c.owes = plan.is_some_and(|p| p.owes);
    }

    // A declared room is dug by bodies its richest colony casts.
    let richest = (homes.colonies.iter())
        .map(|c| c.bank_capacity)
        .max()
        .unwrap_or(0);
    let from = homes.grounds.len();
    for outpost in &ours.worked {
        if !homes.colonies.iter().any(|c| c.room == outpost.room) {
            let ground = remote_of(atlas, world, intel, declarations, richest, outpost);
            homes.grounds.push(ground);
        }
    }
    if homes.grounds.len() > from {
        homes.struck(world, from);
    }
    let Homes {
        colonies,
        mut grounds,
        bodies,
        ovens,
        minerals,
    } = homes;

    // A terminal sends to one colony a tick: minerals first, as aid can wait
    // in the stock while a lab cannot.
    let given_up = |room: RoomName| declarations.abandoning(room);
    let aid = give_aid(&colonies, &grounds, &given_up, &minerals);
    for s in minerals.iter().cloned().chain(aid) {
        if let Some(g) = grounds.iter_mut().find(|g| g.room == s.from) {
            g.shipment = s.load;
            g.ship_to = Some((s.to, s.taker_free));
        }
    }

    let mut routes: BTreeMap<RoomName, Cow<Tree>> = (ours.trees.iter())
        .map(|(home, tree)| (*home, Cow::Borrowed(tree)))
        .collect();

    let spawns = world.spawns.iter().map(|s| s.pos.room);
    for room in spawns
        .chain(grounds.iter().map(|g| g.room))
        .chain(creeps.iter().map(|c| c.pos.room))
    {
        tree_of(&mut routes, atlas, room);
    }

    let claims: Vec<Controller> = declarations
        .claims
        .iter()
        .filter(|_| !ours.capped)
        .filter_map(|claim| match world.room(claim.room) {
            Some(room) => room.controller.clone(),
            None => intel::blind(claim, intel, world, declarations).controller,
        })
        .filter(reservable)
        .collect();
    let pickups = pickups_of((world, atlas), declarations, ours, (&colonies, &grounds));
    let mut spots = spots_of(&grounds, &claims, &bodies);
    for (id, pos, _, _) in pickups.iter().flat_map(|p| &p.loot) {
        spots.entry(Target::Object(*id)).or_insert(*pos);
    }

    Derived {
        time: world.time,
        abandoning: (colonies.iter())
            .map(|c| c.room)
            .filter(|room| given_up(*room))
            .collect(),
        colonies,
        grounds,
        scouts: scouts
            .iter()
            .map(|s| (Pos::new(s.room, 25, 25), s.age, s.cast))
            .collect(),
        spots,
        pickups,
        claims,
        ovens,
        routes,
        bodies,
        minerals,
        closed: (declarations.outposts.iter())
            .filter(|o| !ours.worked.iter().any(|w| w.room == o.room))
            .filter(|o| !ours.owned.contains(&o.room))
            .cloned()
            .collect(),
        ours,
    }
}

fn spots_of(grounds: &[Ground], claims: &[Controller], bodies: &[Body]) -> HashMap<Target, Pos> {
    let mut found: Vec<(Target, Pos)> = Vec::new();
    for g in grounds {
        let ids = (g.sources.iter().map(|x| (x.id, x.pos)))
            .chain(g.minerals.iter().map(|x| (x.id, x.pos)));
        found.extend(ids.map(|(id, pos)| (Target::Object(id), pos)));
        found.extend(g.stores.iter().map(|x| (x.id, x.pos)));
        let ids = (g.refills.iter().map(|x| (x.id, x.pos)))
            .chain(g.sites.iter().map(|x| (x.id, x.pos)))
            .chain(g.repairs.iter().map(|x| (x.id, x.pos)))
            .chain(g.piles.iter().map(|x| (x.id, x.pos)))
            .chain(g.labs.iter().map(|x| (x.id, x.pos)))
            .chain(g.links.iter().map(|x| (x.id, x.pos)))
            .chain(g.terminal.iter().map(|x| (x.id, x.pos)))
            .chain(g.boost_lab.iter().map(|x| (x.id, x.pos)))
            .chain(g.ores.iter().map(|x| (x.0, x.1)))
            .chain(g.controller.iter().map(|x| (x.id, x.pos)));
        found.extend(ids.map(|(id, pos)| (Target::Object(id), pos)));
    }
    let ids = (claims.iter().map(|c| (c.id, c.pos)))
        .chain(bodies.iter().filter_map(|b| Some((b.id?, b.pos))));
    found.extend(ids.map(|(id, pos)| (Target::Object(id), pos)));
    let mut spots = HashMap::with_capacity_and_hasher(found.len(), Default::default());
    for (id, pos) in found {
        spots.entry(id).or_insert(pos);
    }
    spots
}
