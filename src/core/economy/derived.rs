/// Below this file nothing in `Economy` reads `World` (`ARCHITECTURE.md` §1).
use std::borrow::Cow;
use std::collections::BTreeMap;

use crate::core::atlas::{Atlas, Tree};
use crate::core::engine;
use crate::core::grid::Footprint;
use crate::core::tick::{self, Declarations, Outpost};
use crate::core::tiles::{self, Tile};
use crate::core::vocabulary::{BodyPart, Resource, part_count_in};
use crate::core::world::{
    Controller, Creep, Kind, Owner, Part, Pos, Room, Spawn, Store, World, terrain,
};

use super::bodies::{self, Pattern};

#[derive(Clone, Debug)]
pub struct Body {
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

pub fn key_parts_in(pattern: Pattern, body: &Body) -> i32 {
    match bodies::key_part_of(pattern) {
        BodyPart::Carry => body.carry,
        BodyPart::Claim => body.claim,
        _ => body.work,
    }
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
    /// What every income and flow is read off, never `rate` alone.
    pub dug: i32,
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
    /// What of it the container holds.
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
    pub rebuild: i32,
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
    pub room: String,
    pub controller: Option<Controller>,
    pub sources: Vec<SourceFact>,
    pub minerals: Vec<MineralFact>,
    pub refills: Vec<RefillFact>,
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
    pub feed: Option<StoreFact>,
    pub links: Vec<LinkFact>,
    pub receiver: Option<LinkFact>,
    pub buffer_seats: Vec<Pos>,
    pub towers: i32,
    /// The energy our towers hold.
    pub fire: i32,
    pub labs: Vec<LabFact>,
    /// What our storage and terminal hold besides energy: (store, its tile,
    /// resource, amount).
    pub ores: Vec<(String, Pos, Resource, i32)>,
    /// Only the declared cluster's home has one.
    pub lab_plan: Option<super::labs::Plan>,
}

/// A resource, not a decider (`ARCHITECTURE.md` §2).
#[derive(Clone, Debug)]
pub struct Colony {
    pub room: String,
    pub controller: Controller,
    pub spawns: Vec<Spawn>,
    pub bank: i32,
    pub bank_capacity: i32,
}

#[derive(Clone, Debug)]
pub struct Derived {
    pub time: i32,
    /// Colony `i`'s room is `grounds[i]`.
    pub colonies: Vec<Colony>,
    pub grounds: Vec<Ground>,
    pub claims: Vec<Controller>,
    pub bodies: Vec<Body>,
    /// Counted against the quota as if they stood, so no seat is cast twice.
    pub ovens: Vec<Body>,
    pub workers: Vec<Body>,
    pub carriers: Vec<Body>,
    pub claimers: Vec<Body>,
    pub posts: BTreeMap<String, Pos>,
    /// Creep lifetimes of tower fire a reserve holds.
    pub reserve_lifetimes: f64,
    /// From each room a spawn or body stands in, `Atlas::tree`'s.
    pub routes: BTreeMap<String, Tree>,
}

fn body_of(name: &str, pos: &Pos, parts: &[Part], store: &Store, life: i32) -> Body {
    let mut work = 0;
    let mut carry = 0;
    let mut moves = 0;
    let mut claim = 0;

    for part in parts {
        if part.hits > 0 {
            match part.kind {
                BodyPart::Work => work += 1,
                BodyPart::Carry => carry += 1,
                BodyPart::Move => moves += 1,
                BodyPart::Claim => claim += 1,
                _ => (),
            }
        }
    }

    Body {
        name: name.to_string(),
        pos: pos.clone(),
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

fn reservable(me: &str, c: &Controller) -> bool {
    c.owner.is_nobody()
        && match &c.reservation {
            Some((user, _)) => user == me,
            None => true,
        }
}

pub fn invader_held(c: &Controller) -> bool {
    c.reservation
        .as_ref()
        .is_some_and(|(user, _)| user == engine::INVADER_USER)
}

/// v1's: a claimer attacks the Invader's hold off before it reserves.
fn takeable(me: &str, c: &Controller) -> bool {
    reservable(me, c) || invader_held(c)
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
            rate.min(diggers * part_count_in(&body, BodyPart::Work) * engine::HARVEST_PER_WORK)
        }
        None => 0,
    }
}

/// Low while the room grows, so the walls never outbid its upgrading; a room
/// fallen under the first tower's level keeps that level's.
pub fn rampart_target(level: i32) -> i32 {
    match level {
        l if l <= 3 => 10_000,
        4 => 50_000,
        5 => 100_000,
        6 => 500_000,
        7 => 1_000_000,
        _ => 3_000_000,
    }
}

/// Ten decay periods: the margin every bot keeps against losing a rampart.
pub const RAMPART_SAFE: i32 = 10 * engine::RAMPART_DECAY_AMOUNT;

fn swamp(room: &Room, pos: &Pos) -> bool {
    room.terrain.len() == tiles::COUNT && terrain::is_swamp(&room.terrain, pos.x, pos.y)
}

fn ground_of(atlas: &Atlas, me: &str, capacity: i32, rate: i32, room: &Room) -> Ground {
    let owned_room = room.controller.as_ref().is_some_and(|c| c.owner.is_ours());
    let level = room.controller.as_ref().map_or(0, |c| c.level);
    let built = Atlas::buffer(room);
    let is_buffer = |id: &str| built.is_some_and(|b| b.id == id);
    let storage = room
        .structures
        .iter()
        .find(|s| s.kind.is_storage() && s.owner.is_ours());

    let mut links: Vec<LinkFact> = Vec::new();
    for s in &room.structures {
        let role = if !s.kind.is_link() || !s.owner.is_ours() {
            None
        } else if Atlas::is_source_link(room, &s.pos) {
            Some(LinkRole::Source)
        } else if is_buffer(&s.id) {
            Some(LinkRole::Controller)
        } else if storage.is_some_and(|st| st.pos.within(1, &s.pos)) {
            Some(LinkRole::Hub)
        } else {
            None
        };
        if let Some(role) = role {
            links.push(LinkFact {
                id: s.id.clone(),
                pos: s.pos.clone(),
                role,
                energy: s.store.amount_of(&Resource::Energy),
                free: s.store.free(),
                cooldown: s.cooldown,
            });
        }
    }

    let controller_link = links.iter().find(|l| l.role.is_controller()).cloned();
    let hub_link = links.iter().find(|l| l.role.is_hub()).cloned();

    let mut sources: Vec<SourceFact> = Vec::new();
    for s in &room.sources {
        let post = Atlas::post_of(room, &s.pos);
        let seats = seats_at(atlas, 1, &s.pos);
        sources.push(SourceFact {
            id: s.id.clone(),
            pos: s.pos.clone(),
            rate,
            post: post.as_ref().map(|(p, _)| p.clone()),
            container: post.as_ref().and_then(|(_, id)| id.clone()),
            link: links
                .iter()
                .find(|l| l.role.is_source() && l.pos.within(2, &s.pos))
                .map(|l| l.id.clone()),
            seats,
            energy: s.energy,
            dug: dug_at(capacity, owned_room, rate, post.is_some(), seats),
        });
    }

    let mut minerals: Vec<MineralFact> = Vec::new();
    for m in &room.minerals {
        let extractor = room
            .structures
            .iter()
            .find(|s| s.kind.is_extractor() && s.owner.is_ours() && s.pos == m.pos);
        if let Some(extractor) = extractor {
            let post = Atlas::post_of(room, &m.pos);
            let container = post.as_ref().and_then(|(_, id)| id.clone());
            let held = container
                .as_ref()
                .and_then(|id| room.structures.iter().find(|s| &s.id == id))
                .map_or(0, |c| c.store.amount_of(&m.resource));
            minerals.push(MineralFact {
                id: m.id.clone(),
                pos: m.pos.clone(),
                resource: m.resource.clone(),
                amount: m.amount,
                cooldown: extractor.cooldown,
                post: post.map(|(p, _)| p),
                container,
                held,
            });
        }
    }

    let income: i32 = sources.iter().map(|s| s.dug).sum();
    let posted: i32 = sources
        .iter()
        .map(|s| if s.container.is_some() { s.dug } else { 0 })
        .sum();

    let served_dug = |pos: &Pos| -> i32 {
        sources
            .iter()
            .map(|s| {
                if s.pos.range(pos) == Some(1) {
                    s.dug
                } else {
                    0
                }
            })
            .sum()
    };

    let mut refills: Vec<RefillFact> = Vec::new();
    let mut stores: Vec<StoreFact> = Vec::new();
    let mut repairs: Vec<RepairFact> = Vec::new();

    for s in &room.structures {
        match s.kind {
            Kind::Spawn | Kind::Extension | Kind::Tower
                if s.owner.is_ours() && s.store.free() > 0 =>
            {
                refills.push(RefillFact {
                    id: s.id.clone(),
                    pos: s.pos.clone(),
                    kind: s.kind.clone(),
                    free: s.store.free(),
                })
            }
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

                // A terminal is never filled: nothing sends from it.
                let kind = if s.kind.is_storage() || s.kind.is_terminal() {
                    StoreKind::Storage
                } else if owned_room && is_buffer(&s.id) {
                    StoreKind::Buffer
                } else {
                    StoreKind::Container
                };

                if energy > 0 || owned || kind.is_buffer() {
                    stores.push(StoreFact {
                        id: s.id.clone(),
                        pos: s.pos.clone(),
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

        match s.kind {
            Kind::Rampart
                if s.owner.is_ours() && s.hits < rampart_target(level).min(s.hits_max) =>
            {
                repairs.push(RepairFact {
                    id: s.id.clone(),
                    kind: s.kind.clone(),
                    pos: s.pos.clone(),
                    hits: s.hits,
                    hits_max: rampart_target(level).min(s.hits_max),
                    decay: f64::from(engine::RAMPART_DECAY_AMOUNT)
                        / f64::from(engine::RAMPART_DECAY_TIME),
                    rebuild: 0,
                })
            }
            Kind::Road | Kind::Container if s.hits < s.hits_max => {
                let swamp = swamp(room, &s.pos);

                let (decay, rebuild) = if s.kind.is_road() {
                    let ratio = if swamp { engine::ROAD_SWAMP_RATIO } else { 1 };
                    (
                        f64::from(engine::ROAD_DECAY_AMOUNT * ratio)
                            / f64::from(engine::ROAD_DECAY_TIME),
                        engine::ROAD_BUILD_COST * ratio,
                    )
                } else {
                    let period = if owned_room {
                        engine::CONTAINER_DECAY_TIME_OWNED
                    } else {
                        engine::CONTAINER_DECAY_TIME
                    };
                    (
                        f64::from(engine::CONTAINER_DECAY_AMOUNT) / f64::from(period),
                        engine::CONTAINER_BUILD_COST,
                    )
                };

                repairs.push(RepairFact {
                    id: s.id.clone(),
                    kind: s.kind.clone(),
                    pos: s.pos.clone(),
                    hits: s.hits,
                    hits_max: s.hits_max,
                    decay,
                    rebuild,
                })
            }
            _ => (),
        }
    }

    for r in &room.remains {
        let energy = r.store.amount_of(&Resource::Energy);

        if energy > 0 {
            stores.push(StoreFact {
                id: r.id.clone(),
                pos: r.pos.clone(),
                kind: StoreKind::Remains,
                energy,
                free: 0,
                fill: 1.0,
                source_dug: 0,
                owned: false,
            })
        }
    }

    let store_of = |kind: StoreKind, dug: i32, l: &LinkFact| StoreFact {
        id: l.id.clone(),
        pos: l.pos.clone(),
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
            let dug: i32 = sources
                .iter()
                .map(|s| if s.link.is_some() { s.dug } else { 0 })
                .sum();

            if hub.energy > 0 {
                stores.push(store_of(StoreKind::Container, dug, hub));
            }

            None
        }
        (None, None) => None,
    };

    let buffer = stores.iter().find(|s| s.kind.is_buffer()).cloned();

    Ground {
        room: room.name.clone(),
        controller: room.controller.clone(),
        sources,
        minerals,
        refills,
        stores,
        sites: room
            .sites
            .iter()
            .filter(|s| s.owner.is_ours())
            .map(|s| SiteFact {
                id: s.id.clone(),
                kind: s.kind.clone(),
                pos: s.pos.clone(),
                left: 0.max(s.progress_total - s.progress),
            })
            .collect(),
        repairs,
        piles: room
            .piles
            .iter()
            .map(|p| PileFact {
                id: p.id.clone(),
                pos: p.pos.clone(),
                resource: p.resource.clone(),
                amount: p.amount,
            })
            .collect(),
        income,
        posted,
        reservable: room.controller.as_ref().is_some_and(|c| takeable(me, c)),
        our_reservation: match room
            .controller
            .as_ref()
            .and_then(|c| c.reservation.as_ref())
        {
            Some((user, ticks)) if user == me => *ticks,
            _ => 0,
        },
        withheld: 0,
        upgrade_seats: match &room.controller {
            Some(c) => seats_at(atlas, engine::UPGRADE_RANGE, &c.pos),
            None => Some(0),
        },
        buffer: buffer.clone(),
        feed: feed.or(buffer.clone()),
        links,
        receiver: controller_link.or(hub_link),
        labs: room
            .structures
            .iter()
            .filter(|s| s.kind.is_lab() && s.owner.is_ours())
            .map(|s| LabFact {
                id: s.id.clone(),
                pos: s.pos.clone(),
                held: s
                    .store
                    .held
                    .iter()
                    .find(|(r, n)| !r.is_energy() && *n > 0)
                    .cloned(),
                cooldown: s.cooldown,
            })
            .collect(),
        ores: room
            .structures
            .iter()
            .filter(|s| (s.kind.is_storage() || s.kind.is_terminal()) && s.owner.is_ours())
            .flat_map(|s| {
                s.store
                    .held
                    .iter()
                    .filter(|(r, n)| crate::core::vocabulary::is_mineral(r) && *n > 0)
                    .map(|(r, n)| (s.id.clone(), s.pos.clone(), r.clone(), *n))
            })
            .collect(),
        lab_plan: None,
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
        buffer_seats: match (&buffer, &room.controller) {
            (Some(b), Some(c)) if atlas.sees(&room.name) => {
                let walkable = |t: Tile| atlas.passable(&room.name, t) && !tiles::on_edge(t);
                Atlas::upgrader_seats(walkable, tiles::of_pos(&c.pos), tiles::of_pos(&b.pos))
                    .into_iter()
                    .map(|t| tiles::to_pos(&room.name, t))
                    .collect()
            }
            _ => Vec::new(),
        },
    }
}

fn blind(outpost: &Outpost) -> Room {
    Room {
        name: outpost.room.clone(),
        terrain: Vec::new(),
        controller: Some(Controller {
            id: outpost.controller.id.clone(),
            pos: outpost.controller.pos.clone(),
            owner: Owner::Nobody,
            level: 0,
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
        sources: outpost
            .sources
            .iter()
            .map(|s| crate::core::world::Source {
                id: s.id.clone(),
                pos: s.pos.clone(),
                energy: 0,
                energy_capacity: 0,
                ticks_to_regeneration: 0,
            })
            .collect(),
        minerals: Vec::new(),
        structures: Vec::new(),
        sites: Vec::new(),
        piles: Vec::new(),
        remains: Vec::new(),
        hostiles: Vec::new(),
    }
}

fn tree_of(routes: &mut BTreeMap<String, Tree>, atlas: &Atlas, room: &str) {
    if !routes.contains_key(room) {
        routes.insert(room.to_string(), atlas.tree(tick::REACH_HOPS, room));
    }
}

/// `creeps` is the world's, less every body a fight holds: the port of F#'s
/// `{ World with Creeps = filtered }`, whose record copy shares the rooms.
pub fn of_world(
    world: &World,
    creeps: &[&Creep],
    atlas: &Atlas,
    declarations: &Declarations,
    worked: &[Outpost],
    claims: &[Outpost],
) -> Derived {
    let mut colonies: Vec<Colony> = Vec::new();
    let mut colony_rooms: Vec<&Room> = Vec::new();

    for room in &world.rooms {
        if let Some(c) = &room.controller
            && c.owner.is_ours()
        {
            let spawns: Vec<Spawn> = world
                .spawns
                .iter()
                .filter(|s| s.pos.room == room.name)
                .cloned()
                .collect();

            if spawns.is_empty() {
                continue;
            }

            colony_rooms.push(room);
            colonies.push(Colony {
                room: room.name.clone(),
                controller: c.clone(),
                spawns,
                bank: room.energy_available,
                bank_capacity: room.energy_capacity,
            });
        }
    }

    let is_colony = |name: &str| colonies.iter().any(|c| c.room == name);

    // A declared room is dug by bodies its richest colony casts.
    let richest = colony_rooms
        .iter()
        .map(|r| r.energy_capacity)
        .max()
        .unwrap_or(0);

    // At the rate the reserver holds, not this tick's: a body lives longer
    // than a lapse lasts. The engine refuses a harvest on another's hold.
    let remote_of = |outpost: &Outpost| -> Ground {
        let room: Cow<Room> = world
            .room(&outpost.room)
            .map_or_else(|| Cow::Owned(blind(outpost)), Cow::Borrowed);

        let withheld = |c: &Controller| {
            if invader_held(c) {
                room.sources.len() as i32
            } else {
                0
            }
        };

        match &room.controller {
            Some(c) if !c.owner.is_ours() && !reservable(&world.me, c) => {
                let sources = withheld(c);
                let mut bare = room.clone().into_owned();
                bare.sources = Vec::new();
                let mut ground = ground_of(atlas, &world.me, richest, 0, &bare);
                ground.withheld = sources;
                ground
            }
            _ if richest >= engine::body_cost(bodies::block_of(Pattern::Claimer)) => ground_of(
                atlas,
                &world.me,
                richest,
                engine::HELD_OUTPUT_PER_TICK,
                &room,
            ),
            _ => ground_of(
                atlas,
                &world.me,
                richest,
                engine::NEUTRAL_OUTPUT_PER_TICK,
                &room,
            ),
        }
    };

    let mut grounds: Vec<Ground> = colony_rooms
        .iter()
        .map(|room| {
            ground_of(
                atlas,
                &world.me,
                room.energy_capacity,
                engine::HELD_OUTPUT_PER_TICK,
                room,
            )
        })
        .collect();

    for outpost in worked.iter().filter(|o| !is_colony(&o.room)) {
        grounds.push(remote_of(outpost));
    }

    if let Some((home, targets)) = &declarations.labs
        && let Some(g) = grounds.iter_mut().find(|g| &g.room == home)
    {
        let labs: Vec<(String, Pos)> = g
            .labs
            .iter()
            .map(|l| (l.id.clone(), l.pos.clone()))
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
        };
        g.lab_plan = Some(super::labs::plan_for(targets, &stock, &labs, &held));
    }

    let bodies: Vec<Body> = creeps
        .iter()
        .map(|c| body_of(&c.name, &c.pos, &c.body, &c.store, c.ticks_to_live))
        .collect();

    let mut routes: BTreeMap<String, Tree> = BTreeMap::new();

    for s in &world.spawns {
        tree_of(&mut routes, atlas, &s.pos.room);
    }

    for c in creeps {
        tree_of(&mut routes, atlas, &c.pos.room);
    }

    Derived {
        time: world.time,
        colonies: colonies.clone(),
        posts: grounds
            .iter()
            .flat_map(|g| {
                g.sources
                    .iter()
                    .filter_map(|s| s.post.clone().map(|p| (s.id.clone(), p)))
            })
            .collect(),
        grounds,
        claims: claims
            .iter()
            .filter_map(|claim| match world.room(&claim.room) {
                Some(room) => room.controller.clone(),
                None => blind(claim).controller,
            })
            .filter(|c| reservable(&world.me, c))
            .collect(),
        ovens: world
            .spawns
            .iter()
            .filter_map(|s| {
                s.spawning
                    .as_ref()
                    .filter(|oven| !tick::fights(&oven.name))
                    .map(|oven| {
                        let life = if oven.body.iter().any(|p| p.kind == BodyPart::Claim) {
                            engine::CLAIM_LIFETIME
                        } else {
                            engine::CREEP_LIFETIME
                        };
                        body_of(&oven.name, &s.pos, &oven.body, &Store::empty(), life)
                    })
            })
            .collect(),
        workers: bodies
            .iter()
            .filter(|b| b.work > 0 && b.carry > 0)
            .cloned()
            .collect(),
        carriers: bodies
            .iter()
            .filter(|b| b.carry > 0 && b.work == 0)
            .cloned()
            .collect(),
        claimers: bodies.iter().filter(|b| b.claim > 0).cloned().collect(),
        reserve_lifetimes: declarations.reserve_lifetimes,
        routes,
        bodies,
    }
}
