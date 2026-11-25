use std::collections::BTreeSet;
use std::str::Split;

use screeps::game::map::RoomStatus;

use crate::tiles::{self, BOTTOM, LEFT, RIGHT, TOP};

use super::*;
use crate::declarations::Declarations;
use crate::engine;
use crate::vocabulary::Part;

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
        self.word().parse().unwrap_or(RoomName::SIM)
    }

    fn word(&mut self) -> &'a str {
        let i = self.n();
        self.words.get(i as usize).copied().unwrap_or_default()
    }

    fn str(&mut self) -> &'a str {
        self.strs.next().unwrap_or_default()
    }

    fn list<T>(&mut self, each: impl FnMut(&mut Self) -> T) -> Vec<T> {
        self.list_into(Vec::new(), each)
    }

    fn list_into<T>(&mut self, out: Vec<T>, mut each: impl FnMut(&mut Self) -> T) -> Vec<T> {
        self.list_some(out, |r| Some(each(r)))
    }

    fn list_some<T>(
        &mut self,
        mut out: Vec<T>,
        mut each: impl FnMut(&mut Self) -> Option<T>,
    ) -> Vec<T> {
        let count = self.n();
        out.extend((0..count).filter_map(|_| each(self)));
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
            .filter(|(_, amount)| *amount > 0)
            .filter_map(|(name, amount)| Some((name.parse().ok()?, amount)))
            .collect();
        Store {
            held,
            capacity: self.n(),
        }
    }

    fn body(&mut self, hits: i32) -> Vec<BodyPart> {
        let n = self.n() as usize;
        (0..n)
            .map(|i| BodyPart {
                kind: self.word().parse().unwrap_or(Part::Tough),
                hits: engine::part_hits_at(n, hits, i),
                boost: self.word().parse().ok(),
            })
            .collect()
    }

    fn creep(&mut self) -> Creep {
        let (id, name, owner, pos, hits) =
            (self.id(), self.s(), self.owner(), self.pos(), self.n());
        Creep {
            id,
            name,
            owner,
            pos,
            body: self.body(hits),
            hits,
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
            sign: self.flag().then(|| {
                let user = self.word().to_string();
                (self.user(user), self.s())
            }),
        }
    }

    fn structure(&mut self, room: RoomName, blocked: &mut Vec<Pos>) -> Option<Structure> {
        let kind = self.word().parse::<StructureType>();
        let id = self.id();
        let pos = self.pos_in(room);
        let owner = self.owner();
        let hits = self.n();
        let hits_max = self.n();
        let store = if kind.as_ref().is_ok_and(|k| !k.has_store()) {
            Store::default()
        } else {
            self.store()
        };
        let Ok(kind) = kind else {
            blocked.push(pos);
            return None;
        };
        let cooldown = if kind.has_cooldown() { self.n() } else { 0 };
        let spawn = (kind == StructureType::KeeperLair)
            .then(|| self.n())
            .filter(|t| *t >= 0);
        let (collapse, deploy, level) = if kind == StructureType::InvaderCore {
            let running = self.flag();
            let ticks = self.n();
            let (collapse, deploy) = if running {
                (ticks, 0)
            } else {
                (ticks + engine::STRONGHOLD_LONGEST_COLLAPSE, ticks)
            };
            (collapse, deploy, self.n())
        } else {
            (0, 0, 0)
        };
        let walkable = if kind == StructureType::Rampart {
            let public = self.flag();
            owner.is_ours() || public
        } else {
            kind.walkable()
        };
        Some(Structure {
            id,
            kind,
            pos,
            owner,
            hits,
            hits_max,
            store,
            cooldown,
            collapse,
            deploy,
            level,
            spawn,
            walkable,
        })
    }

    /// Our own fallen creep whole, another's by its id.
    fn remains(&mut self) -> Remains {
        let (id, pos, store) = (self.id(), self.pos(), self.store());
        let creep = self.n();
        Remains {
            id,
            pos,
            store,
            fallen: (creep == 1).then(|| Fallen {
                body: self.list(|r| r.word().parse().unwrap_or(Part::Tough)),
                ticks_to_live: self.n(),
                died: self.n(),
            }),
            theirs: (creep == 2).then(|| self.id()),
        }
    }

    fn room(&mut self, buffers: &mut Buffers) -> Room<'static> {
        let name = self.room_name();
        let kept = buffers.rooms.pop().unwrap_or_else(|| Room::empty(name));
        Room {
            name,
            energy_available: self.n(),
            energy_capacity: self.n(),
            controller: self.flag().then(|| self.controller()),
            ..kept
        }
    }

    /// Every room's objects a kind at a time, as `read.js` times them.
    fn kinds(&mut self, rooms: &mut [Room<'static>]) {
        use std::mem::take;
        for room in rooms.iter_mut() {
            room.sources = self.list_into(take(&mut room.sources), |r| Source {
                id: r.id(),
                pos: r.pos(),
                energy: r.n(),
                energy_capacity: r.n(),
                ticks_to_regeneration: r.n(),
            });
            room.minerals = self.list_into(take(&mut room.minerals), |r| Mineral {
                id: r.id(),
                pos: r.pos(),
                resource: r.word().parse().ok(),
                amount: r.n(),
            });
        }
        for room in rooms.iter_mut() {
            let (name, blocked) = (room.name, &mut room.blocked);
            room.structures =
                self.list_some(take(&mut room.structures), |r| r.structure(name, blocked));
        }
        for room in rooms.iter_mut() {
            let name = room.name;
            room.sites = self.list_some(take(&mut room.sites), |r| {
                let kind = r.word().parse().ok();
                let (id, pos, owner) = (r.id(), r.pos_in(name), r.owner());
                let (progress, progress_total) = (r.n(), r.n());
                Some(Site {
                    kind: kind?,
                    id,
                    pos,
                    owner,
                    progress,
                    progress_total,
                })
            });
        }
        for room in rooms.iter_mut() {
            room.piles = self.list_some(take(&mut room.piles), |r| {
                let (id, pos) = (r.id(), r.pos());
                let resource = r.word().parse().ok();
                let amount = r.n();
                Some(Pile {
                    id,
                    pos,
                    resource: resource?,
                    amount,
                })
            });
        }
        for room in rooms.iter_mut() {
            room.remains = self.list_into(take(&mut room.remains), Reader::remains);
        }
        for room in rooms.iter_mut() {
            room.others = self.list_into(take(&mut room.others), Reader::creep);
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
                // A spawning creep cannot be struck.
                body: self.body(i32::MAX),
            }),
        }
    }
}

#[derive(Default)]
pub struct Buffers {
    rooms: Vec<Room<'static>>,
    spawns: Vec<Spawn>,
    creeps: Vec<Creep>,
    attacks: Vec<(Id, Id)>,
    destroyed: Vec<(RoomName, Id, i32)>,
}

fn emptied<T>(mut list: Vec<T>) -> Vec<T> {
    list.clear();
    list
}

impl Buffers {
    pub fn keep(&mut self, world: World) {
        for room in world.rooms {
            self.rooms.push(Room {
                sources: emptied(room.sources),
                minerals: emptied(room.minerals),
                structures: emptied(room.structures),
                blocked: emptied(room.blocked),
                sites: emptied(room.sites),
                piles: emptied(room.piles),
                remains: emptied(room.remains),
                others: emptied(room.others),
                ..Room::empty(room.name)
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
    let mut rooms = r.list(|r| r.room(buffers));
    r.kinds(&mut rooms);
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
        rules: Rules::of(r.list_some(Vec::new(), |r| {
            let (kind, row) = (r.word().parse().ok(), r.list(Reader::n));
            Some((kind?, row))
        })),
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
