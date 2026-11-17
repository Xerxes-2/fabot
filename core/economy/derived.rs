/// Below this file nothing in `Economy` reads `World` (`ARCHITECTURE.md` §1).
use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet};

use crate::HashMap;
use crate::atlas::{Atlas, Tree};
use crate::engine;
use crate::grid::Footprint;
use crate::strategy::intel::{self, Intel, Scout};
use crate::tick::{self, Declarations, Outpost};
use crate::tiles::{self, Tile};
use crate::vocabulary::{Part, Resource, part_count_in};
use crate::world::{
    BodyPart, Controller, Creep, Kind, Owner, Pos, Room, RoomName, Source, Spawn, Store, Structure,
    World, terrain,
};

use super::bodies::{self, Pattern};

#[derive(Clone, Debug)]
pub struct Body {
    /// None while it spawns.
    pub id: Option<String>,
    pub name: String,
    pub pos: Pos,
    /// Live parts: a part shot to no hits does nothing.
    pub work: i32,
    pub carry: i32,
    pub moves: i32,
    pub claim: i32,
    /// Dead parts counted: the length a successor is spawned at.
    pub size: i32,
    pub store: Store,
    pub energy: i32,
    pub free: i32,
    pub ticks_to_live: i32,
    pub pattern: Pattern,
}

#[derive(Clone, Debug)]
pub struct SourceFact {
    pub id: String,
    pub pos: Pos,
    pub rate: i32,
    pub post: Option<Pos>,
    pub container: Option<String>,
    pub link: Option<String>,
    pub seats: Option<i32>,
    pub energy: i32,
    /// What its bay's extensions hold full: its container keeps it for them.
    pub bay: i32,
    /// What every income and flow is read off, never `rate` alone.
    pub dug: i32,
}

#[derive(Clone, Debug)]
pub struct TerminalFact {
    pub id: String,
    pub pos: Pos,
    pub store: Store,
    pub cooldown: i32,
}

#[derive(Clone, Debug)]
pub struct LabFact {
    pub id: String,
    pub pos: Pos,
    /// Its one mineral and how much of it.
    pub held: Option<(Resource, i32)>,
    pub cooldown: i32,
}

/// A deposit our extractor stands on.
#[derive(Clone, Debug)]
pub struct MineralFact {
    pub id: String,
    pub pos: Pos,
    pub resource: Resource,
    pub amount: i32,
    pub cooldown: i32,
    pub post: Option<Pos>,
    pub container: Option<String>,
    pub held: i32,
}

#[derive(Clone, Debug)]
pub struct RefillFact {
    pub id: String,
    pub pos: Pos,
    pub kind: Kind,
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
    pub fn is_storage(self) -> bool {
        self == StoreKind::Storage
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
    pub id: String,
    pub pos: Pos,
    pub kind: StoreKind,
    pub energy: i32,
    pub free: i32,
    pub fill: f64,
    pub source_dug: i32,
    pub owned: bool,
}

impl StoreFact {
    /// What a worker may draw: an owned store keeps the whole reserve, each
    /// one, since a draw is from one store.
    pub fn spare(&self, reserve: i32) -> i32 {
        self.energy - if self.owned { reserve } else { 0 }
    }
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
    pub id: String,
    pub pos: Pos,
    pub role: LinkRole,
    pub energy: i32,
    pub free: i32,
    pub cooldown: i32,
}

#[derive(Clone, Debug)]
pub struct SiteFact {
    pub id: String,
    pub kind: Kind,
    pub pos: Pos,
    pub left: i32,
}

#[derive(Clone, Debug)]
pub struct RepairFact {
    pub id: String,
    pub kind: Kind,
    pub pos: Pos,
    pub hits: i32,
    pub hits_max: i32,
    pub decay: f64,
    /// Hits a tick worn off by each energy a tick hauled across it.
    pub wear: f64,
    pub rebuild: i32,
}

impl RepairFact {
    pub fn loss(&self, haul: i32) -> f64 {
        self.decay + self.wear * f64::from(haul)
    }

    pub fn at_risk(&self, haul: i32) -> bool {
        f64::from(self.hits) < self.loss(haul) * f64::from(engine::CREEP_LIFETIME)
    }
}

#[derive(Clone, Debug)]
pub struct PileFact {
    pub id: String,
    pub pos: Pos,
    pub resource: Resource,
    pub amount: i32,
}

/// A room the economy works: a colony's own, or one declared to it.
#[derive(Clone, Debug)]
pub struct Ground {
    pub room: RoomName,
    pub controller: Option<Controller>,
    pub sources: Vec<SourceFact>,
    pub minerals: Vec<MineralFact>,
    pub refills: Vec<RefillFact>,
    pub bays: Vec<RefillFact>,
    pub stores: Vec<StoreFact>,
    pub sites: Vec<SiteFact>,
    pub repairs: Vec<RepairFact>,
    pub piles: Vec<PileFact>,
    pub income: i32,
    /// The only income a body is hired against.
    pub posted: i32,
    /// The engine accepts our reservation or claim on its controller, or a
    /// claimer attacks the Invader's hold off it.
    pub reservable: bool,
    pub our_reservation: i32,
    /// Sources the Invader's hold refuses us a harvest from.
    pub withheld: i32,
    pub upgrade_seats: Option<i32>,
    pub buffer: Option<StoreFact>,
    /// The buffer's tile, built (a link or container) or still a container
    /// site, so the site ranks first in a nursed room.
    pub buffer_tile: Option<Pos>,
    pub feed: Option<StoreFact>,
    pub links: Vec<LinkFact>,
    pub receiver: Option<LinkFact>,
    pub buffer_seats: Vec<Pos>,
    pub towers: i32,
    pub fire: i32,
    pub labs: Vec<LabFact>,
    /// What our storage and terminal hold besides energy: (store, its tile,
    /// resource, amount).
    pub ores: Vec<(String, Pos, Resource, i32)>,
    /// Only the declared cluster's home has one.
    pub lab_plan: Option<super::labs::Plan>,
    /// The lab a cast waiting at home is boosted from, and what its parts
    /// still ask of the compound and the energy to spend with it.
    pub boost_lab: Option<BoostLab>,
    pub terminal: Option<TerminalFact>,
    /// What this colony sends, and the colony and its terminal's free room.
    pub shipment: Vec<(Resource, i32)>,
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

    pub fn bank(&self) -> Option<&StoreFact> {
        self.stores.iter().find(|s| s.owned || s.kind.is_pile())
    }

    /// The energy its storage holds, the terminal's being the shipments';
    /// `None` without a storage.
    pub fn stock(&self) -> Option<i32> {
        self.has_storage().then(|| {
            self.stores
                .iter()
                .filter(|s| s.owned)
                .map(|s| s.energy)
                .sum()
        })
    }

    /// A site that waits pays back nothing, so while one stands its builders
    /// draw the reserve.
    pub fn kept_from_builders(&self, reserve: i32) -> i32 {
        if self.sites.iter().any(|s| drawn_for(&s.kind)) {
            0
        } else {
            reserve
        }
    }
}

/// A rampart guards the reserve and costs one energy.
pub fn drawn_for(kind: &Kind) -> bool {
    kind.pays_back() || *kind == Kind::Rampart
}

/// The lab a waiting body walks to, and what the colony's stores owe it.
#[derive(Clone, Debug)]
pub struct BoostLab {
    pub id: String,
    pub pos: Pos,
    pub compound: Resource,
    pub mineral: i32,
    pub energy: i32,
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
pub struct Derived {
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
    pub routes: BTreeMap<RoomName, Tree>,
    /// The declared outposts a foe stands down this tick.
    pub closed: Vec<Outpost>,
    pub spots: HashMap<String, Pos>,
}

impl Derived {
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
    (id, name): (Option<&str>, &str),
    pos: &Pos,
    parts: &[BodyPart],
    store: &Store,
    life: i32,
) -> Body {
    let mut work = 0;
    let mut carry = 0;
    let mut moves = 0;
    let mut claim = 0;

    for part in parts {
        if part.hits > 0 {
            match part.kind {
                Part::Work => work += 1,
                Part::Carry => carry += 1,
                Part::Move => moves += 1,
                Part::Claim => claim += 1,
                _ => (),
            }
        }
    }

    Body {
        id: id.map(str::to_string),
        name: name.to_string(),
        pos: *pos,
        work,
        carry,
        moves,
        claim,
        size: parts.len() as i32,
        store: store.clone(),
        energy: store.amount_of(&Resource::Energy),
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

/// What a claimer wins a tick: a held room's yield over a neutral one's, and
/// every source the Invader's hold refuses.
pub fn reserve_gain(ground: &Ground) -> i32 {
    ground.sources.len() as i32 * (engine::HELD_OUTPUT_PER_TICK - engine::NEUTRAL_OUTPUT_PER_TICK)
        + ground.withheld * engine::HELD_OUTPUT_PER_TICK
}

/// `None` where the Atlas counts none: unknown, not zero.
fn seats_at(atlas: &Atlas, range: i32, target: &Pos) -> Option<i32> {
    let seats = atlas.seats(Footprint::One, range, target);
    if seats.is_empty() {
        None
    } else {
        Some(seats.len() as i32)
    }
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

    match body {
        Some(body) => {
            // Not knowing a source's seats is no crew.
            let diggers = if posted { 1 } else { seats.unwrap_or(0) };
            rate.min(diggers * part_count_in(&body, Part::Work) * engine::HARVEST_PER_WORK)
        }
        None => 0,
    }
}

/// Ten decay periods: the margin every bot keeps against losing a rampart.
pub const RAMPART_SAFE: i32 = 10 * engine::RAMPART_DECAY_AMOUNT;

fn swamp(room: &Room, pos: &Pos) -> bool {
    terrain::is_swamp(room.terrain, pos.x, pos.y)
}

/// What keeping `s` standing costs, where it is below the hits we hold it at.
fn repair_of(room: &Room, wall: i32, owned_room: bool, s: &Structure) -> Option<RepairFact> {
    let per = |amount: i32, time: i32| f64::from(amount) / f64::from(time);
    let (hits_max, decay, wear, rebuild) = match s.kind {
        Kind::Rampart if s.owner.is_ours() => (
            wall.min(s.hits_max),
            per(engine::RAMPART_DECAY_AMOUNT, engine::RAMPART_DECAY_TIME),
            0.0,
            0,
        ),
        Kind::Road => {
            let ratio = if swamp(room, &s.pos) {
                engine::ROAD_SWAMP_RATIO
            } else {
                1
            };
            let block = bodies::block_of(Pattern::Hauler);
            let load = part_count_in(block, Part::Carry) * engine::CARRY_PART_CAPACITY;
            let steps = 2.0 / f64::from(load);
            let decay = per(engine::ROAD_DECAY_AMOUNT * ratio, engine::ROAD_DECAY_TIME);
            let wear = decay * steps * f64::from(block.len() as i32 * engine::ROAD_WEAROUT);
            (s.hits_max, decay, wear, engine::ROAD_BUILD_COST * ratio)
        }
        Kind::Container => {
            let period = if owned_room {
                engine::CONTAINER_DECAY_TIME_OWNED
            } else {
                engine::CONTAINER_DECAY_TIME
            };
            let decay = per(engine::CONTAINER_DECAY_AMOUNT, period);
            (s.hits_max, decay, 0.0, engine::CONTAINER_BUILD_COST)
        }
        _ => return None,
    };
    (s.hits < hits_max).then(|| RepairFact {
        id: s.id.clone(),
        kind: s.kind.clone(),
        pos: s.pos,
        hits: s.hits,
        hits_max,
        decay,
        wear,
        rebuild,
    })
}

fn links_of(
    room: &Room,
    is_buffer: &dyn Fn(&str) -> bool,
    storage: Option<&Structure>,
) -> Vec<LinkFact> {
    let role_of = |s: &Structure| {
        if !s.kind.is_link() || !s.owner.is_ours() {
            None
        } else if Atlas::is_source_link(room, &s.pos) {
            Some(LinkRole::Source)
        } else if is_buffer(&s.id) {
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
            Some(LinkFact {
                id: s.id.clone(),
                pos: s.pos,
                role,
                energy: s.store.amount_of(&Resource::Energy),
                free: s.store.free(),
                cooldown: s.cooldown,
            })
        })
        .collect()
}

fn minerals_of(room: &Room) -> Vec<MineralFact> {
    (room.minerals.iter())
        .filter_map(|m| {
            let extractor = (room.structures.iter())
                .find(|s| s.kind.is_extractor() && s.owner.is_ours() && s.pos == m.pos)?;
            let post = Atlas::post_of(room, &m.pos);
            let container = post.as_ref().and_then(|(_, id)| id.clone());
            let held = container
                .as_ref()
                .and_then(|id| room.structures.iter().find(|s| &s.id == id))
                .map_or(0, |c| c.store.amount_of(&m.resource));
            Some(MineralFact {
                id: m.id.clone(),
                pos: m.pos,
                resource: m.resource.clone(),
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
    is_buffer: &dyn Fn(&str) -> bool,
    served_dug: &dyn Fn(&Pos) -> i32,
) -> Vec<StoreFact> {
    let mut stores: Vec<StoreFact> = Vec::new();

    for s in &room.structures {
        match s.kind {
            Kind::Container | Kind::Storage | Kind::Terminal
                if !s.kind.is_terminal() || s.owner.is_ours() =>
            {
                let energy = s.store.amount_of(&Resource::Energy);
                let owned = s.kind.is_storage() && s.owner.is_ours();
                let dug = if s.kind.is_container() {
                    served_dug(&s.pos)
                } else {
                    0
                };

                let kind = if s.kind.is_storage() || s.kind.is_terminal() {
                    StoreKind::Storage
                } else if buffered && is_buffer(&s.id) {
                    StoreKind::Buffer
                } else {
                    StoreKind::Container
                };

                if energy > 0 || owned || kind.is_buffer() {
                    stores.push(StoreFact {
                        id: s.id.clone(),
                        pos: s.pos,
                        kind,
                        energy,
                        free: s.store.free(),
                        fill: if s.store.capacity > 0 {
                            f64::from(s.store.used()) / f64::from(s.store.capacity)
                        } else {
                            0.0
                        },
                        source_dug: dug,
                        owned,
                    })
                }
            }
            _ => (),
        }
    }

    for r in &room.remains {
        let energy = r.store.amount_of(&Resource::Energy);

        if energy > 0 {
            stores.push(StoreFact {
                id: r.id.clone(),
                pos: r.pos,
                kind: StoreKind::Remains,
                energy,
                free: 0,
                fill: 1.0,
                source_dug: 0,
                owned: false,
            })
        }
    }
    stores
}

fn labs_of(room: &Room) -> Vec<LabFact> {
    room.structures
        .iter()
        .filter(|s| s.kind.is_lab() && s.owner.is_ours())
        .map(|s| LabFact {
            id: s.id.clone(),
            pos: s.pos,
            held: s
                .store
                .held
                .iter()
                .find(|(r, n)| !r.is_energy() && *n > 0)
                .cloned(),
            cooldown: s.cooldown,
        })
        .collect()
}

fn ores_of(room: &Room) -> Vec<(String, Pos, Resource, i32)> {
    room.structures
        .iter()
        .filter(|s| (s.kind.is_storage() || s.kind.is_terminal()) && s.owner.is_ours())
        .flat_map(|s| {
            s.store
                .held
                .iter()
                .filter(|(r, n)| crate::vocabulary::is_mineral(r) && *n > 0)
                .map(|(r, n)| (s.id.clone(), s.pos, r.clone(), *n))
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

fn ground_of(atlas: &Atlas, capacity: i32, rate: i32, room: &Room, wall: i32) -> Ground {
    let owned_room = room.controller.as_ref().is_some_and(|c| c.owner.is_ours());
    let built = Atlas::buffer(room);
    let is_buffer = |id: &str| built.is_some_and(|b| b.id == id);
    let storage = room
        .structures
        .iter()
        .find(|s| s.kind.is_storage() && s.owner.is_ours());

    let links = links_of(room, &is_buffer, storage);

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
            id: s.id.clone(),
            pos: s.pos,
            rate,
            post: post.as_ref().map(|(p, _)| *p),
            container: post.as_ref().and_then(|(_, id)| id.clone()),
            link: links
                .iter()
                .find(|l| l.role.is_source() && l.pos.within(2, &s.pos))
                .map(|l| l.id.clone()),
            seats,
            energy: s.energy,
            bay: (room.structures.iter())
                .filter(|x| x.kind == Kind::Extension && x.owner.is_ours())
                .filter(|x| post.as_ref().is_some_and(|(p, _)| p.within(1, &x.pos)))
                .map(|x| x.store.capacity)
                .sum(),
            dug: dug_at(capacity, owned_room, rate, post.is_some(), seats),
        }
    };
    let sources: Vec<SourceFact> = room.sources.iter().map(source_of).collect();

    let minerals = minerals_of(room);

    let income: i32 = sources.iter().map(|s| s.dug).sum();
    let dug_of =
        |by: &dyn Fn(&SourceFact) -> bool| sources.iter().filter(|s| by(s)).map(|s| s.dug).sum();
    let posted: i32 = dug_of(&|s| s.container.is_some());
    let served_dug = |pos: &Pos| -> i32 { dug_of(&|s| s.pos.range(pos) == Some(1)) };

    let (bays, refills): (Vec<RefillFact>, Vec<RefillFact>) = (room.structures.iter())
        .filter(|s| matches!(s.kind, Kind::Spawn | Kind::Extension | Kind::Tower))
        .filter(|s| s.owner.is_ours() && s.store.free() > 0)
        .map(|s| RefillFact {
            id: s.id.clone(),
            pos: s.pos,
            kind: s.kind.clone(),
            free: s.store.free(),
        })
        .partition(|r| in_bay(&r.kind, &r.pos));
    let mut stores = stores_of(room, owned_room, &is_buffer, &served_dug);
    let repairs: Vec<RepairFact> = (room.structures.iter())
        .flat_map(|s| repair_of(room, wall, owned_room, s))
        .collect();

    let store_of = |kind: StoreKind, dug: i32, l: &LinkFact| StoreFact {
        id: l.id.clone(),
        pos: l.pos,
        kind,
        energy: l.energy,
        free: l.free,
        fill: f64::from(l.energy) / f64::from((l.energy + l.free).max(1)),
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
                id: s.id.clone(),
                kind: s.kind.clone(),
                pos: s.pos,
                left: 0.max(s.progress_total - s.progress),
            })
            .collect(),
        repairs,
        piles: room
            .piles
            .iter()
            .map(|p| PileFact {
                id: p.id.clone(),
                pos: p.pos,
                resource: p.resource.clone(),
                amount: p.amount,
            })
            .collect(),
        income,
        posted,
        reservable: room.controller.as_ref().is_some_and(takeable),
        our_reservation: match room
            .controller
            .as_ref()
            .and_then(|c| c.reservation.as_ref())
        {
            Some((Owner::Ours, ticks)) => *ticks,
            _ => 0,
        },
        withheld: 0,
        upgrade_seats: match &room.controller {
            Some(c) => seats_at(atlas, engine::UPGRADE_RANGE, &c.pos),
            None => Some(0),
        },
        buffer_tile: built.map(|b| b.pos).or_else(|| {
            room.sites
                .iter()
                .find(|s| s.kind.is_container() && Atlas::is_buffer(room, &s.pos))
                .map(|s| s.pos)
        }),
        buffer: buffer.clone(),
        feed: feed.or(buffer.clone()),
        links,
        receiver: controller_link.or(hub_link),
        labs: labs_of(room),
        ores: ores_of(room),
        lab_plan: None,
        boost_lab: None,
        terminal: room
            .structures
            .iter()
            .find(|s| s.kind.is_terminal() && s.owner.is_ours())
            .map(|s| TerminalFact {
                id: s.id.clone(),
                pos: s.pos,
                store: s.store.clone(),
                cooldown: s.cooldown,
            }),
        shipment: Vec::new(),
        ship_to: None,
        abandoned: false,
        towers: room
            .structures
            .iter()
            .filter(|s| s.kind.is_tower() && s.owner.is_ours())
            .count() as i32,
        fire: room
            .structures
            .iter()
            .filter(|s| s.kind.is_tower() && s.owner.is_ours())
            .map(|s| s.store.amount_of(&Resource::Energy))
            .sum(),
        buffer_seats: buffer_seats_of(atlas, room, buffer.as_ref()),
    }
}

fn tree_of(routes: &mut BTreeMap<RoomName, Tree>, atlas: &Atlas, room: RoomName) {
    routes
        .entry(room)
        .or_insert_with(|| atlas.tree(&BTreeSet::new(), tick::REACH_HOPS, room));
}

/// `creeps` is the world's, less every body a fight holds: the port of F#'s
/// `{ World with Creeps = filtered }`, whose record copy shares the rooms.
fn free_at(grounds: &[Ground], room: RoomName) -> i32 {
    (grounds.iter().find(|g| g.room == room))
        .and_then(|g| g.terminal.as_ref())
        .map_or(0, |t| t.store.free())
}

/// The declared lab home's plan and boost lab, and each other colony's ore
/// shipped to it.
fn stock_labs(
    world: &World,
    grounds: &mut [Ground],
    colonies: usize,
    (home, targets): (RoomName, &BTreeMap<crate::vocabulary::Compound, i32>),
    given_up: &dyn Fn(RoomName) -> bool,
) {
    let home = &home;
    let Some(g) = grounds.iter_mut().find(|g| &g.room == home) else {
        return;
    };
    // The lab a body waiting at home is boosted from is the cast's and not
    // a reaction's: its compound is kept while the body still lacks it.
    let kept: BTreeMap<String, Resource> = (world.creeps.iter())
        .filter(|c| &c.pos.room == home)
        .filter_map(|c| {
            let compound = crate::boost::of_name(&c.name)?;
            let lab = crate::boost::booster(world, *home, compound)?;
            Some((lab.id.clone(), Resource::Compound(compound)))
        })
        .collect();
    let labs: Vec<(String, Pos)> = (g.labs.iter())
        .filter(|l| !kept.contains_key(&l.id))
        .map(|l| (l.id.clone(), l.pos))
        .collect();
    let held = |id: &str| {
        g.labs
            .iter()
            .find(|l| l.id == id)
            .and_then(|l| l.held.clone())
    };
    let stock = |r: &Resource| -> i32 {
        g.ores
            .iter()
            .filter(|o| &o.2 == r)
            .map(|o| o.3)
            .sum::<i32>()
            + g.labs
                .iter()
                .filter_map(|l| l.held.as_ref().filter(|h| &h.0 == r).map(|h| h.1))
                .sum::<i32>()
            // A load between a store and a lab is still the home's.
            + world
                .creeps
                .iter()
                .filter(|c| &c.pos.room == home)
                .map(|c| c.store.amount_of(r))
                .sum::<i32>()
    };
    let mut plan = super::labs::plan_for(targets, &stock, &labs, &held);
    let lab_at = |id: &str| {
        (world.rooms.iter())
            .filter(|r| &r.name == home)
            .flat_map(|r| r.structures.iter())
            .find(|s| s.id == id)
    };
    // A lab holds one mineral: whatever else a body's booster holds is
    // drained, or the compound it waits on has nowhere to land. The
    // booster's own drain, so a cluster too small to run a reaction
    // still empties it.
    for (id, compound) in &kept {
        let Some(lab) = lab_at(id) else {
            continue;
        };
        for (r, n) in &lab.store.held {
            if *n > 0 && r != compound && crate::vocabulary::is_mineral(r) {
                plan.drains.push((id.clone(), r.clone()));
            }
        }
    }
    g.lab_plan = Some(plan);
    // The lab a waiting cast is boosted from is stocked from the home's
    // stores: the reaction refuses it as a lab of its own.
    let mut waited: BTreeMap<&str, (Resource, i32, i32)> = BTreeMap::new();
    for creep in (world.creeps.iter()).filter(|c| &c.pos.room == home) {
        let Some(w) = crate::boost::waiting(world, *home, creep) else {
            continue;
        };
        let at = waited
            .entry(w.lab.id.as_str())
            .or_insert((Resource::Compound(w.compound), 0, 0));
        at.1 += w.mineral;
        at.2 += w.energy;
    }
    g.boost_lab = waited
        .into_iter()
        .find_map(|(id, (compound, mineral, energy))| {
            let lab = lab_at(id)?;
            Some(BoostLab {
                id: id.to_string(),
                pos: lab.pos,
                compound: compound.clone(),
                mineral: mineral - lab.store.amount_of(&compound),
                energy: energy - lab.store.amount_of(&Resource::Energy),
            })
        });
    let mut short = super::labs::ore_short(targets, &stock);
    let home_free = free_at(grounds, *home);

    // The colonies in their order, each the ore it holds up to what is left.
    for g in grounds.iter_mut().take(colonies) {
        if &g.room == home || g.terminal.is_none() || given_up(g.room) {
            continue;
        }
        for (ore, left) in &mut short {
            let held: i32 = g.ores.iter().filter(|o| &o.2 == ore).map(|o| o.3).sum();
            let sent = held.min(*left);
            if sent > 0 {
                g.shipment.push((ore.clone(), sent));
                *left -= sent;
            }
        }
        if !g.shipment.is_empty() {
            g.ship_to = Some((*home, home_free));
        }
    }
}

/// v1's `AidDonorMargin`: over a load and its fee.
const AID_MARGIN: i32 = 20_000;

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
fn give_aid(colonies: &[Colony], grounds: &mut [Ground], given_up: &dyn Fn(RoomName) -> bool) {
    let mut takers: Vec<Taker> = (colonies.iter().zip(grounds.iter()))
        .filter(|(c, _)| !given_up(c.room))
        .filter_map(|(c, g)| {
            let t = g.terminal.as_ref()?;
            let held = g.stock()? + t.store.amount_of(&Resource::Energy);
            Some(Taker {
                room: c.room,
                level: c.controller.level,
                held,
                reserve: c.reserve,
                free: t.store.free(),
            })
        })
        .collect();
    for g in grounds.iter_mut().take(colonies.len()) {
        let Some(gi) = takers.iter().position(|t| t.room == g.room) else {
            continue;
        };
        let owed: i32 = g.sites.iter().map(|s| s.left).sum();
        let spare = takers[gi].held - takers[gi].reserve - AID_MARGIN - owed;
        if g.ship_to.is_some() {
            continue;
        }
        let Some(ti) = recipient(&takers[gi], &takers) else {
            continue;
        };
        let to = &takers[ti];
        let sent = spare.min(to.free).min(to.short().unwrap_or(i32::MAX));
        if sent < engine::TERMINAL_MIN_SEND {
            continue;
        }
        g.shipment = vec![(Resource::Energy, sent)];
        g.ship_to = Some((to.room, to.free));
        takers[gi].held -= sent;
        takers[ti].held += sent;
        takers[ti].free -= sent;
    }
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

    let withheld = |c: &Controller| {
        if invader_held(c) {
            room.sources.len() as i32
        } else {
            0
        }
    };

    match &room.controller {
        Some(c) if !c.owner.is_ours() && !reservable(c) => {
            let sources = withheld(c);
            let mut bare = room.clone().into_owned();
            bare.sources = Vec::new();
            let mut ground = ground_of(atlas, richest, 0, &bare, 0);
            ground.withheld = sources;
            ground
        }
        _ if richest >= engine::body_cost(bodies::block_of(Pattern::Claimer)) => {
            ground_of(atlas, richest, engine::HELD_OUTPUT_PER_TICK, &room, 0)
        }
        _ => ground_of(atlas, richest, engine::NEUTRAL_OUTPUT_PER_TICK, &room, 0),
    }
}

fn ovens_of(world: &World) -> Vec<Body> {
    world
        .spawns
        .iter()
        .filter_map(|s| {
            s.spawning
                .as_ref()
                .filter(|oven| !tick::fights(&oven.name))
                .map(|oven| {
                    let life = if oven.body.iter().any(|p| p.kind == Part::Claim) {
                        engine::CLAIM_LIFETIME
                    } else {
                        engine::CREEP_LIFETIME
                    };
                    body_of(
                        (None, &oven.name),
                        &s.pos,
                        &oven.body,
                        &Store::empty(),
                        life,
                    )
                })
        })
        .collect()
}

pub fn of_world(
    world: &World,
    creeps: &[&Creep],
    atlas: &Atlas,
    declarations: &Declarations,
    ours: &tick::Derived,
    intel: &Intel,
    scouts: &[Scout],
) -> Derived {
    let colony_rooms: Vec<&Room> = (ours.colonies.iter())
        .filter_map(|name| world.room(*name))
        .collect();
    let plan_of = |room: RoomName| (declarations.colonies.iter()).find(|p| p.room == room);
    let colonies: Vec<Colony> = (colony_rooms.iter())
        .filter_map(|room| {
            let controller = room.controller.clone()?;
            let plan = plan_of(room.name);
            Some(Colony {
                room: room.name,
                reserve: plan.map_or(0, |p| p.reserve),
                owes: plan.is_some_and(|p| p.owes),
                controller,
                spawns: (world.spawns.iter())
                    .filter(|s| s.pos.room == room.name)
                    .cloned()
                    .collect(),
                bank: room.energy_available,
                bank_capacity: room.energy_capacity,
            })
        })
        .collect();

    let is_colony = |name: RoomName| colonies.iter().any(|c| c.room == name);

    // A declared room is dug by bodies its richest colony casts.
    let richest = colony_rooms
        .iter()
        .map(|r| r.energy_capacity)
        .max()
        .unwrap_or(0);

    let remote_of =
        |outpost: &Outpost| remote_of(atlas, world, intel, declarations, richest, outpost);

    let mut grounds: Vec<Ground> = colony_rooms
        .iter()
        .map(|room| {
            ground_of(
                atlas,
                room.energy_capacity,
                engine::HELD_OUTPUT_PER_TICK,
                room,
                plan_of(room.name).map_or(0, |p| p.rampart_hits),
            )
        })
        .collect();

    for g in &mut grounds {
        let plan = (declarations.colonies.iter()).find(|p| p.room == g.room);
        if let Some(pos) = plan.and_then(|p| p.pile) {
            let (energy, rest): (Vec<PileFact>, Vec<PileFact>) =
                (g.piles.drain(..)).partition(|p| p.pos == pos && p.resource.is_energy());
            g.piles = rest;
            // A draw picks up the pile by its id; a drop needs none.
            let id = energy
                .first()
                .map_or_else(|| format!("pile-{}", g.room), |p| p.id.clone());
            g.stores.push(StoreFact {
                id,
                pos,
                kind: StoreKind::Pile,
                energy: energy.iter().map(|p| p.amount).sum(),
                free: engine::STORAGE_CAPACITY,
                fill: 0.0,
                source_dug: 0,
                owned: false,
            });
        }
    }

    for outpost in ours.worked.iter().filter(|o| !is_colony(o.room)) {
        grounds.push(remote_of(outpost));
    }

    let given_up = |room: RoomName| declarations.abandoning(room);

    if let Some((home, targets)) = &declarations.labs()
        && !given_up(*home)
    {
        stock_labs(
            world,
            &mut grounds,
            colonies.len(),
            (*home, targets),
            &given_up,
        );
    }

    for (i, room) in colony_rooms.iter().enumerate() {
        if given_up(room.name) {
            let to = grounds
                .iter()
                .take(colonies.len())
                .filter(|g| g.terminal.is_some() && !given_up(g.room))
                .map(|g| (g.room, free_at(&grounds, g.room)))
                .max_by_key(|(_, free)| *free);
            super::abandon::evacuate(&mut grounds[i], room, &world.creeps, to);
        }
    }

    give_aid(&colonies, &mut grounds, &given_up);

    let bodies: Vec<Body> = creeps
        .iter()
        .map(|c| {
            body_of(
                (Some(&c.id), &c.name),
                &c.pos,
                &c.body,
                &c.store,
                c.ticks_to_live,
            )
        })
        .collect();

    // A bay with no harvester beside to keep it is the carriers' (Hivemind's).
    for g in &mut grounds {
        let kept = |r: &RefillFact| {
            (bodies.iter()).any(|b| {
                b.pattern.is_harvester() && b.store.capacity > 0 && b.pos.within(1, &r.pos)
            })
        };
        let (kept, left): (Vec<_>, Vec<_>) = g.bays.drain(..).partition(kept);
        g.bays = kept;
        g.refills.extend(left);
    }

    let mut routes: BTreeMap<RoomName, Tree> = BTreeMap::new();

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
    let spots = spots_of(&grounds, &claims, &bodies);

    Derived {
        time: world.time,
        colonies,
        grounds,
        scouts: scouts
            .iter()
            .map(|s| (Pos::new(s.room, 25, 25), s.age, s.cast))
            .collect(),
        spots,
        claims,
        ovens: ovens_of(world),
        routes,
        bodies,
        closed: (declarations.outposts.iter())
            .filter(|o| !ours.worked.iter().any(|w| w.room == o.room))
            .filter(|o| !ours.colonies.contains(&o.room))
            .cloned()
            .collect(),
    }
}

fn spots_of(grounds: &[Ground], claims: &[Controller], bodies: &[Body]) -> HashMap<String, Pos> {
    let mut spots = HashMap::default();
    for g in grounds {
        let ids = (g.sources.iter().map(|x| (&x.id, x.pos)))
            .chain(g.minerals.iter().map(|x| (&x.id, x.pos)))
            .chain(g.stores.iter().map(|x| (&x.id, x.pos)))
            .chain(g.refills.iter().map(|x| (&x.id, x.pos)))
            .chain(g.sites.iter().map(|x| (&x.id, x.pos)))
            .chain(g.repairs.iter().map(|x| (&x.id, x.pos)))
            .chain(g.piles.iter().map(|x| (&x.id, x.pos)))
            .chain(g.labs.iter().map(|x| (&x.id, x.pos)))
            .chain(g.links.iter().map(|x| (&x.id, x.pos)))
            .chain(g.terminal.iter().map(|x| (&x.id, x.pos)))
            .chain(g.boost_lab.iter().map(|x| (&x.id, x.pos)))
            .chain(g.ores.iter().map(|x| (&x.0, x.1)))
            .chain(g.controller.iter().map(|x| (&x.id, x.pos)));
        for (id, pos) in ids {
            spots.entry(id.clone()).or_insert(pos);
        }
    }
    for c in claims {
        spots.entry(c.id.clone()).or_insert(c.pos);
    }
    for b in bodies {
        if let Some(id) = &b.id {
            spots.entry(id.clone()).or_insert(b.pos);
        }
    }
    spots
}
