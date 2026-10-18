/// The fight's tick (`ARCHITECTURE.md` §6): who is out there, what we committed,
/// what each committed body does, and what it asks for.
use std::collections::{BTreeMap, BTreeSet};

use super::kinds::defend::{self, Facts as DefendFacts, Home};
use super::kinds::siege::{self, Facts as SiegeFacts};
use super::kinds::sweep;
use super::shape::{Cast, Engagement, Group, Objective, Phase, Plan, Tactic};
use super::{casting, tracker};
use crate::core::atlas::{Atlas, Mover, Target};
use crate::core::exchange::{self, Fighter, Ground, Losses, Outcome, Side, Stance, fighter};
use crate::core::grid::{self, Footprint};
use crate::core::intents::{self, Ask, ControllerAct, CreepAct, Intent, Rank};
use crate::core::mv::Wish;
use crate::core::state::State;
use crate::core::tactics;
use crate::core::tick::{self, Context, Declarations, Module};
use crate::core::tiles::Tile;
use crate::core::vocabulary::{BodyPart, Resource};
use crate::core::world::{
    self, Creep, Owner, Pos, Room, Sightings, Structure, World, hostiles, sightings,
};
use crate::core::{engine, tiles};

/// A body's slot in its set, its name's last part, kept by its successors.
fn slot_of(name: &str) -> String {
    name.rsplit('-').next().unwrap_or_default().to_string()
}

/// A merge keeps the older group's id; two engagements on one group are one,
/// the older surviving with both's bodies, losses and budget. Also each
/// merged id's survivor.
fn merged(
    events: &[tracker::Event],
    engagements: &[Engagement],
) -> (Vec<Engagement>, BTreeMap<i32, i32>) {
    let retargeted: Vec<Engagement> = engagements
        .iter()
        .map(|e| {
            let mut out = e.clone();

            for event in events {
                if let tracker::Event::Merged { into, from } = event
                    && out.group == *from
                {
                    out.group = *into;
                }
            }

            out
        })
        .collect();

    let mut ranked = retargeted;
    ranked.sort_by_key(|e| e.id);

    let mut kept: Vec<Engagement> = Vec::new();
    let mut into: BTreeMap<i32, i32> = BTreeMap::new();

    for e in ranked {
        let older = kept.iter().position(|k| {
            matches!(k.objective, Objective::Defend)
                && matches!(e.objective, Objective::Defend)
                && k.group == e.group
        });

        match older {
            Some(at) => {
                kept[at].ours = kept[at].ours.union(&e.ours).cloned().collect();
                kept[at].lost += e.lost;
                kept[at].budget += e.budget;
                into.insert(e.id, kept[at].id);
            }
            None => kept.push(e),
        }
    }

    (kept, into)
}

struct Asked {
    queued: BTreeMap<String, i32>,
    casts: Vec<Cast>,
}

impl Asked {
    fn queue(&mut self, room: &str, body: &[BodyPart]) {
        let added = engine::SPAWN_TICKS_PER_PART * body.len() as i32;
        let at = self.queued.entry(room.to_string()).or_insert(0);
        *at += added;
    }
}

/// What a sweep reads of a room once a tick: its escort and marks (prey
/// first) outside its barriers, what strikes each tile, what holding it is
/// worth and where the hunt stands.
struct Hunting {
    escort: Vec<Creep>,
    threat: tiles::Table<i32>,
    marks: Vec<tactics::Mark>,
    worth: i32,
    at: Pos,
}

/// What this tick's world says, read once and handed down.
struct Scene<'a> {
    world: &'a World,
    sightings: &'a Sightings,
    avoid: &'a BTreeSet<String>,
    homes: BTreeSet<String>,
    worked: BTreeSet<String>,
    foes: BTreeMap<String, Vec<Creep>>,
    cores: BTreeSet<String>,
    by_id: BTreeMap<i32, Group>,
    hostile_at: BTreeMap<String, Pos>,
    ours: BTreeMap<&'a str, &'a Creep>,
    armed: BTreeSet<String>,
    /// The cast bodies standing, and the ones still in an oven.
    landed: Vec<Cast>,
    waiting: Vec<Cast>,
}

/// `Casting.price`'s say in how many copies of a block to ask for.
type CopiesOf<'a> = Box<dyn Fn(&casting::Body) -> i32 + 'a>;

impl<'a> Scene<'a> {
    /// The reflex's stance (§7.2): a stranger only where it threatens.
    fn struck(&self, h: &Creep) -> bool {
        match h.owner {
            Owner::Ally(_) => false,
            Owner::Stranger(_) => self.armed.contains(&h.id),
            _ => true,
        }
    }

    fn rival(s: &Structure) -> bool {
        !s.owner.is_ours() && (s.kind.is_rampart() || s.kind.is_wall())
    }

    fn safe(room: &Room) -> bool {
        room.controller
            .as_ref()
            .is_some_and(|c| !c.owner.is_ours() && c.safe_mode > 0)
    }

    fn in_sight(&self, room: &str) -> Option<&'a Room> {
        self.world.rooms.iter().find(|r| r.name == room)
    }

    fn barrier_at(&self, room: &'a Room) -> BTreeMap<&'a str, &'a Structure> {
        room.structures
            .iter()
            .filter(|s| Scene::rival(s))
            .map(|s| (s.id.as_str(), s))
            .collect()
    }

    fn next_struck(&self, e: &Engagement) -> Option<&'a Structure> {
        let room = self.in_sight(&e.room)?;

        match (e.plan.order.first(), &e.objective) {
            (Some(first), _) => self.barrier_at(room).get(first.as_str()).copied(),
            (None, Objective::Siege(target)) => room.structures.iter().find(|s| s.id == *target),
            (None, Objective::Defend | Objective::Sweep) => None,
        }
    }

    fn beside(reached: &BTreeSet<Tile>, at: &Pos) -> bool {
        tiles::around(tiles::of_pos(at))
            .iter()
            .any(|t| reached.contains(t))
    }

    /// A siege picket's worth, as a sweep's: the harvest its post denies.
    fn denied(room: &Room, post: &Pos) -> i32 {
        room.sources
            .iter()
            .filter(|s| s.pos == *post)
            .map(|s| sweep::yield_of(s, engine::CREEP_LIFETIME))
            .sum()
    }

    fn exposed(atlas: &Atlas, room: &Room) -> Vec<Pos> {
        Scene::exposed_in(&atlas.from_exits(&room.name), room)
    }

    /// The sources a walk from the room's exits reaches beside.
    fn exposed_in(reached: &BTreeSet<Tile>, room: &Room) -> Vec<Pos> {
        room.sources
            .iter()
            .filter(|s| Scene::beside(reached, &s.pos))
            .map(|s| s.pos.clone())
            .collect()
    }

    fn ground(&self, atlas: &Atlas, room: &Room) -> Hunting {
        let reached = atlas.from_exits(&room.name);
        // Denied only once a body stands there: its life less the most a body
        // spawns in and its walk.
        let e = fresh(0, Objective::Sweep, &room.name, 0, Tactic::Kite);
        let walk = (self.colonies_of(atlas, &e).iter())
            .filter_map(|r| self.walk_to(atlas, &e, &r.name))
            .min();
        let spawn = engine::SPAWN_TICKS_PER_PART * engine::MAX_BODY_PARTS as i32;
        let held = walk.map_or(0, |w| engine::CREEP_LIFETIME - spawn - w);
        let all = sweep::prey(room);
        let prey: Vec<&Creep> = all
            .iter()
            .copied()
            .filter(|c| Scene::beside(&reached, &c.pos))
            .collect();
        let exposed = Scene::exposed_in(&reached, room);
        let escort: Vec<Creep> = self.foes[&room.name]
            .iter()
            .filter(|h| {
                matches!(h.owner, Owner::Enemy(_) | Owner::Invader)
                    && self.armed.contains(&h.id)
                    && !self.cores.contains(&h.id)
                    && all.iter().all(|p| p.id != h.id)
            })
            .cloned()
            .collect();
        let foes: Vec<Fighter> = escort.iter().map(casting::fighter_of).collect();
        let mut marks: Vec<tactics::Mark> = prey
            .iter()
            .copied()
            .chain(&escort)
            .map(|c| tactics::Mark {
                id: c.id.clone(),
                pos: c.pos.clone(),
                worth: sweep::worth(c),
                hits: c.hits,
                massed: true,
            })
            .collect();
        marks.extend(
            room.structures
                .iter()
                .filter(|s| s.kind.is_container() || s.kind.is_link() && !s.owner.is_ours())
                .filter(|s| Scene::beside(&reached, &s.pos))
                .map(|s| tactics::Mark {
                    id: s.id.clone(),
                    pos: s.pos.clone(),
                    worth: s.hits,
                    hits: s.hits,
                    massed: !s.kind.is_container(),
                }),
        );
        Hunting {
            threat: exchange::damage_map(&room.name, &self.charged(room), &foes),
            worth: sweep::room_worth(room, &prey, &exposed, held),
            at: exposed
                .first()
                .or(prey.first().map(|c| &c.pos))
                .cloned()
                .unwrap_or_else(|| middle(&room.name)),
            escort,
            marks,
        }
    }

    fn charged(&self, room: &'a Room) -> Vec<Pos> {
        self.towers_in(room)
            .into_iter()
            .filter(|s| s.store.amount_of(&Resource::Energy) >= engine::TOWER_ENERGY_COST)
            .map(|s| s.pos.clone())
            .collect()
    }

    fn towers_in(&self, room: &'a Room) -> Vec<&'a Structure> {
        room.structures
            .iter()
            .filter(|s| s.kind.is_tower() && !s.owner.is_ours())
            .collect()
    }

    fn members_of(&self, e: &Engagement) -> Vec<&'a Creep> {
        e.ours
            .iter()
            .filter_map(|n| self.ours.get(n.as_str()).copied())
            .collect()
    }

    fn walk_to(&self, atlas: &Atlas, e: &Engagement, from: &str) -> Option<i32> {
        let hops = match e.objective {
            Objective::Siege(_) | Objective::Sweep => tick::SIEGE_HOPS,
            Objective::Defend => tick::REACH_HOPS,
        };

        atlas
            .route(self.avoid, hops, from, &e.room)
            .map(|rooms| engine::ROOM_SIDE * (rooms.len() as i32 - 1))
    }

    /// A home's Defend is the home's alone.
    fn colonies_of(&self, atlas: &Atlas, e: &Engagement) -> Vec<&'a Room> {
        let world = self.world;

        world
            .rooms
            .iter()
            .filter(|r| {
                self.homes.contains(&r.name)
                    && if self.homes.contains(&e.room) {
                        r.name == e.room
                    } else {
                        self.walk_to(atlas, e, &r.name).is_some()
                    }
            })
            .collect()
    }

    fn capacity_of(&self, atlas: &Atlas, e: &Engagement) -> i32 {
        self.colonies_of(atlas, e)
            .iter()
            .fold(0, |most, r| most.max(r.energy_capacity))
    }

    fn spent(&self, e: &Engagement) -> bool {
        self.homes.contains(&e.room) && e.lost > e.budget
    }

    /// Each body in a slot no member nor cast of the engagement holds.
    fn ask(&self, atlas: &Atlas, e: &Engagement, set: &[casting::Body], asked: &mut Asked) {
        let held: BTreeSet<String> = e
            .ours
            .iter()
            .chain(
                asked
                    .casts
                    .iter()
                    .filter(|c| c.engagement == e.id)
                    .map(|c| &c.name),
            )
            .map(|n| slot_of(n))
            .collect();

        let free = (0..)
            .map(|i: i32| i.to_string())
            .filter(|i| !held.contains(i));
        let slots: Vec<(String, casting::Body)> = free.zip(set.iter().cloned()).collect();

        self.ask_slots(atlas, e, &slots, asked);
    }

    fn ask_slots(
        &self,
        atlas: &Atlas,
        e: &Engagement,
        set: &[(String, casting::Body)],
        asked: &mut Asked,
    ) {
        let colonies = self.colonies_of(atlas, e);

        for (slot, body) in set {
            let colony = casting::colony_for(
                &colonies,
                &self.world.spawns,
                &asked.queued,
                |from| self.walk_to(atlas, e, from),
                body,
            );

            if let Some(colony) = colony {
                asked.queue(&colony, body);
                asked.casts.push(Cast {
                    engagement: e.id,
                    name: format!(
                        "{}{}-{}-{}",
                        tick::FIGHT_PREFIX,
                        self.world.time,
                        e.id,
                        slot
                    ),
                    room: colony,
                    body: body.clone(),
                });
            }
        }
    }

    /// A body that dies before a successor asked now could stand is
    /// succeeded in its slot (§6.2's rotation).
    fn succeed(&self, atlas: &Atlas, e: &Engagement, asked: &mut Asked) {
        let Some(walk) = self
            .colonies_of(atlas, e)
            .iter()
            .filter_map(|r| self.walk_to(atlas, e, &r.name))
            .min()
        else {
            return;
        };

        let slot = slot_of;
        let members = self.members_of(e);

        let lasts =
            |c: &Creep| c.ticks_to_live > engine::SPAWN_TICKS_PER_PART * c.body.len() as i32 + walk;

        let held: BTreeSet<String> = members
            .iter()
            .filter(|c| lasts(c))
            .map(|c| slot(&c.name))
            .chain(
                asked
                    .casts
                    .iter()
                    .filter(|c| c.engagement == e.id)
                    .map(|c| slot(&c.name)),
            )
            .collect();

        let due: BTreeMap<String, casting::Body> = members
            .iter()
            .filter(|c| !held.contains(&slot(&c.name)))
            .map(|c| (slot(&c.name), c.body.iter().map(|p| p.kind).collect()))
            .collect();

        let due: Vec<(String, casting::Body)> = due.into_iter().collect();
        self.ask_slots(atlas, e, &due, asked);
    }

    fn facts_of(
        &self,
        e: &Engagement,
        standing: &BTreeSet<String>,
        aging_out: &BTreeSet<String>,
    ) -> DefendFacts {
        let group = self.by_id.get(&e.group).cloned();

        let near = |name: &str| -> bool {
            match (self.ours.get(name), &group) {
                (Some(creep), Some(g)) => g.members.iter().any(|m| {
                    self.hostile_at.contains_key(m)
                        && creep.pos.within(engine::RANGED_RANGE, &self.hostile_at[m])
                }),
                _ => false,
            }
        };

        let in_reach = e.ours.iter().any(|n| near(n));
        let home = e.ours.iter().all(|n| {
            self.ours
                .get(n.as_str())
                .is_some_and(|c| self.homes.contains(&c.pos.room))
        });
        let threatens = group
            .as_ref()
            .is_some_and(|g| self.homes.contains(&g.room) || self.worked.contains(&g.room));

        DefendFacts {
            time: self.world.time,
            group,
            standing: standing.clone(),
            aging_out: aging_out.clone(),
            in_reach,
            home,
            held: self.homes.contains(&e.room),
            asked: self.waiting.iter().any(|c| c.engagement == e.id),
            threatens,
            // A core in sight stands in a group of its own, so a room in sight
            // holds open only the engagement on that group.
            clear: sightings::clear(self.sightings, self.world.time, &e.room)
                || self.foes.contains_key(&e.room)
                    && sightings::unarmed(self.sightings, self.world.time, &e.room),
        }
    }

    fn siege_facts(
        &self,
        e: &Engagement,
        target: &str,
        standing: &BTreeSet<String>,
        aging_out: &BTreeSet<String>,
    ) -> SiegeFacts {
        SiegeFacts {
            time: self.world.time,
            standing: standing.clone(),
            aging_out: aging_out.clone(),
            asked: self.waiting.iter().any(|c| c.engagement == e.id),
            home: e.ours.iter().all(|n| {
                self.ours
                    .get(n.as_str())
                    .is_some_and(|c| self.homes.contains(&c.pos.room))
            }),
            under_fire: e.ours.iter().any(|n| {
                self.ours
                    .get(n.as_str())
                    .is_some_and(|c| c.pos.room == e.room && !tactics::picket_of(c))
            }),
            razed: self
                .in_sight(&e.room)
                .is_some_and(|room| room.structures.iter().all(|s| s.id != target)),
            safe: self.in_sight(&e.room).is_some_and(Scene::safe),
        }
    }

    /// Whole, as cast: a part struck off is healed back, and only a body lost
    /// re-prices the set.
    fn fielded(&self, e: &Engagement) -> Vec<Fighter> {
        self.fielded_bodies(e)
            .iter()
            .map(|b| fighter::cast(b))
            .collect()
    }

    /// One body a slot: a successor and the body it succeeds are one.
    fn fielded_bodies(&self, e: &Engagement) -> Vec<casting::Body> {
        let mut bodies: BTreeMap<String, casting::Body> = e
            .ours
            .iter()
            .filter_map(|n| self.ours.get(n.as_str()))
            .map(|c| (slot_of(&c.name), c.body.iter().map(|p| p.kind).collect()))
            .collect();

        bodies.extend(
            self.waiting
                .iter()
                .filter(|c| c.engagement == e.id)
                .map(|c| (slot_of(&c.name), c.body.clone())),
        );

        // A picket holds its post, not the breach.
        bodies
            .into_values()
            .filter(|b| !b.iter().any(|p| p.is_ranged_attack()))
            .collect()
    }

    fn unreached(&self, atlas: &mut Atlas, room: &Room, b: &Structure, in_room: &[&Creep]) -> bool {
        !in_room.is_empty()
            && in_room.iter().all(|m| {
                atlas
                    .field(
                        Mover {
                            footprint: Footprint::One,
                            factor: grid::factor_of(m),
                        },
                        &Target {
                            room: room.name.clone(),
                            tiles: vec![tiles::of_pos(&b.pos)],
                            range: engine::MELEE_RANGE,
                            avoid: BTreeSet::new(),
                            max_hops: tick::SIEGE_HOPS,
                        },
                        &room.name,
                    )
                    .and_then(|field| field.ticks(tiles::of_pos(&m.pos)))
                    .is_none()
            })
    }

    fn breach_in(
        &self,
        atlas: &Atlas,
        room: &Room,
        target: &Structure,
        colony: &str,
        set: &[Fighter],
    ) -> Option<(Vec<String>, Outcome)> {
        let barriers: Vec<&Structure> = self.barrier_at(room).values().copied().collect();
        let line: Vec<Pos> = barriers.iter().map(|b| b.pos.clone()).collect();

        let entry = atlas
            .route(&BTreeSet::new(), tick::SIEGE_HOPS, colony, &room.name)
            .and_then(|rooms| {
                [tiles::TOP, tiles::RIGHT, tiles::BOTTOM, tiles::LEFT]
                    .into_iter()
                    .find(|exit| {
                        rooms.len() >= 2
                            && tiles::room_toward(&room.name, *exit).as_deref()
                                == Some(rooms[rooms.len() - 2].as_str())
                    })
            });

        let gaits: Vec<grid::FatigueFactor> =
            set.iter().map(|f| f.gait).filter(|g| g.moves > 0).collect();

        let gait = gaits
            .iter()
            .copied()
            .min_by_key(|g| std::cmp::Reverse(grid::step_ticks(*g, engine::PLAIN_WEIGHT)));

        let mut weights = atlas.weights(&room.name)?;
        let exit = entry?;
        let gait = gait?;

        for b in &barriers {
            if room
                .structures
                .iter()
                .all(|s| s.pos != b.pos || s.walkable || Scene::rival(s))
            {
                weights[tiles::of_pos(&b.pos)] = grid::weight(
                    if world::terrain::is_swamp(&room.terrain, b.pos.x, b.pos.y) {
                        engine::SWAMP_WEIGHT
                    } else {
                        engine::PLAIN_WEIGHT
                    },
                );
            }
        }

        let mut hits_at: BTreeMap<Tile, i32> = BTreeMap::new();

        for b in &barriers {
            *hits_at.entry(tiles::of_pos(&b.pos)).or_insert(0) += b.hits;
        }

        let ground = Ground {
            room: room.name.clone(),
            weights,
            barriers: hits_at,
        };

        let repair = engine::REPAIR_POWER
            * self.foes[&room.name]
                .iter()
                .map(|h| casting::fighter_of(h).work)
                .sum::<i32>();

        let rate = engine::DISMANTLE_POWER * set.iter().map(|f| f.work).sum::<i32>() - repair;

        let towers: Vec<Pos> = self
            .towers_in(room)
            .into_iter()
            .map(|s| s.pos.clone())
            .collect();

        let attempt = |seated: i32| -> Option<(Vec<(Tile, Tile)>, i32)> {
            let sources: Vec<Tile> = tiles::exit_tiles(exit)
                .iter()
                .copied()
                .filter(|t| ground.weights[*t].is_some())
                .collect();

            exchange::breach_plan(
                &ground,
                gait,
                &towers,
                exchange::pooled_heal(seated, set),
                rate,
                &sources,
                tiles::around(tiles::of_pos(&target.pos)),
            )
        };

        let seats_of = |plan: &(Vec<(Tile, Tile)>, i32)| -> i32 {
            match plan.0.first() {
                Some((_, work)) => tactics::seats_at(atlas, &line, &room.name, *work),
                None => set.len() as i32,
            }
        };

        // Every barrier on a tile, its rampart first: the tile stands
        // in the plan until it is walkable.
        let ids_at = |tile: Tile| -> Vec<String> {
            let mut at: Vec<&Structure> = barriers
                .iter()
                .copied()
                .filter(|b| tiles::of_pos(&b.pos) == tile)
                .collect();

            at.sort_by_key(|b| (!b.kind.is_rampart(), b.id.clone()));
            at.into_iter().map(|b| b.id.clone()).collect()
        };

        // Seats follow the planned barrier: priced with every medic beside
        // the struck body, then with those its seats hold.
        let first = attempt(set.len() as i32)?;
        let seats = seats_of(&first);
        let plan = attempt(seats).filter(|plan| seats_of(plan) >= seats)?;

        Some((
            plan.0.iter().flat_map(|(tile, _)| ids_at(*tile)).collect(),
            Outcome::Win(plan.1, Losses { dead: 0, hits: 0 }),
        ))
    }

    /// Priced at the first sight of its room and as the bank grows while no
    /// set it buys staffs it; after, only on an event.
    fn besieged(
        &self,
        atlas: &mut Atlas,
        asked: &mut Asked,
        e: &Engagement,
        target: &str,
        left: bool,
        grew: bool,
    ) -> Option<Engagement> {
        let mut names: Vec<String> = self
            .colonies_of(atlas, e)
            .iter()
            .map(|r| r.name.clone())
            .collect();
        names.sort();

        let colony = names.into_iter().next()?;

        let Some(room) = self.in_sight(&e.room) else {
            return Some(e.clone());
        };

        let Some(goal) = room.structures.iter().find(|s| s.id == target) else {
            return Some(e.clone());
        };

        if e.unanswered && !grew {
            Some(e.clone())
        } else if e.unanswered {
            let capacity = self.capacity_of(atlas, e);

            let planned = casting::siege(capacity, |set: &[casting::Body]| {
                let fighters: Vec<Fighter> = set.iter().map(|b| fighter::cast(b)).collect();

                self.breach_in(atlas, room, goal, &colony, &fighters)
            });

            match planned {
                Some((set, (order, outcome))) => {
                    let charged = self.charged(room);
                    let mut set = set;

                    for post in Scene::exposed(atlas, room) {
                        let strike = exchange::tower_strike(&charged, &post);
                        set.extend(casting::squad(capacity, strike, Scene::denied(room, &post)));
                    }

                    self.ask(atlas, e, &set, asked);

                    Some(Engagement {
                        plan: Plan {
                            tactic: Tactic::Blob(Box::new(Tactic::Close)),
                            order,
                        },
                        budget: e.lost,
                        unanswered: false,
                        capacity,
                        predicted: Some(outcome),
                        ..e.clone()
                    })
                }
                // Unstaffed, it waits on the bank to grow.
                None => Some(Engagement {
                    capacity,
                    ..e.clone()
                }),
            }
        } else if e.phase == Phase::Disengaging {
            Some(e.clone())
        } else {
            // A forming set waits on every ask: a successor asked then would hold it.
            if e.phase != Phase::Forming {
                self.succeed(atlas, e, asked);
            }

            let set = self.fielded(e);

            let in_room: Vec<&Creep> = self
                .members_of(e)
                .into_iter()
                .filter(|m| m.pos.room == room.name)
                .collect();

            let towers: Vec<Pos> = self
                .towers_in(room)
                .into_iter()
                .map(|s| s.pos.clone())
                .collect();

            let struck = |b: &Structure| exchange::tower_strike(&towers, &b.pos);

            let late = match e.predicted {
                Some(Outcome::Win(ticks, _)) => {
                    e.phase == Phase::Fighting && self.world.time > e.since + ticks
                }
                _ => false,
            };

            let event = left
                || self.landed.iter().any(|c| c.engagement == e.id)
                || late
                || match self.next_struck(e) {
                    Some(b) => {
                        struck(b) > exchange::pooled_heal(set.len() as i32, &set)
                            || self.unreached(atlas, room, b, &in_room)
                    }
                    None => false,
                };

            if !event {
                return Some(e.clone());
            }

            let planned = self
                .breach_in(atlas, room, goal, &colony, &set)
                .map(|plan| (Vec::new(), plan))
                .or_else(|| {
                    casting::reinforce(
                        e.capacity,
                        &self.fielded_bodies(e),
                        |set: &[casting::Body]| {
                            let fighters: Vec<Fighter> =
                                set.iter().map(|b| fighter::cast(b)).collect();
                            self.breach_in(atlas, room, goal, &colony, &fighters)
                        },
                    )
                });

            match planned {
                Some((added, (order, outcome))) if added.is_empty() => {
                    // Once: a new plan whose first barrier the set still cannot
                    // reach is no plan, or the event would fire every tick.
                    let reached = match order.first() {
                        Some(first) => self
                            .barrier_at(room)
                            .get(first.as_str())
                            .is_some_and(|b| !self.unreached(atlas, room, b, &in_room)),
                        None => true,
                    };

                    let mut next = siege::replanned(self.world.time, reached, e);
                    next.plan = Plan {
                        tactic: e.plan.tactic.clone(),
                        order,
                    };
                    next.predicted = Some(outcome);
                    Some(next)
                }
                // Topped up, not recast: the set fights on as its medics walk in.
                Some((added, (order, outcome)))
                    if order.first().is_none_or(|first| {
                        self.barrier_at(room)
                            .get(first.as_str())
                            .is_some_and(|b| !self.unreached(atlas, room, b, &in_room))
                    }) =>
                {
                    self.ask(atlas, e, &added, asked);
                    let mut next = siege::replanned(self.world.time, true, e);
                    next.plan.order = order;
                    next.predicted = Some(outcome);
                    Some(next)
                }
                // A top-up whose first barrier the set cannot reach is none.
                Some(_) | None => Some(siege::replanned(self.world.time, false, e)),
            }
        }
    }

    fn joining(&self, atlas: &Atlas, e: &Engagement, free: &[String]) -> Vec<String> {
        free.iter()
            .filter(|n| {
                self.ours
                    .get(n.as_str())
                    .is_some_and(|c| self.walk_to(atlas, e, &c.pos.room).is_some())
            })
            .cloned()
            .collect()
    }

    /// The set beside `ours` that beats the room's escort and out-heals the
    /// towers where it works, or what strikes `ours` where they stand (owner:
    /// a set tanking two towers at 20 is cheap), within what holding the room
    /// is worth.
    fn hunt(
        &self,
        ground: &Hunting,
        room: &Room,
        ours: Vec<Fighter>,
        capacity: i32,
    ) -> Option<(Vec<casting::Body>, Outcome)> {
        let at = &ground.at;
        let worth = ground.worth + ground.escort.iter().map(sweep::worth).sum::<i32>();
        let theirs = Side {
            bodies: ground.escort.iter().map(casting::fighter_of).collect(),
            ramparts: BTreeMap::new(),
            towers: Vec::new(),
        };
        let (mut set, outcome, _) = casting::price(
            Stance::Standoff,
            &casting::side_in(room, ours.clone()),
            &theirs,
            casting::terrain_at(room, at),
            capacity,
            |body| worth / engine::body_cost(body),
            0,
        )?;

        let seen = ours
            .iter()
            .filter_map(|f| f.pos.as_ref())
            .filter(|p| p.room == room.name);
        let strike = seen
            .map(|p| ground.threat[tiles::of_pos(p)])
            .fold(exchange::tower_strike(&self.charged(room), at), i32::max);
        let holds = |set: &[casting::Body]| {
            let mut all = ours.clone();
            all.extend(set.iter().map(|b| fighter::cast(b)));
            casting::holds(&all, strike)
        };
        if !holds(&set) || ours.is_empty() && set.is_empty() {
            set.extend(casting::squad(capacity, strike, worth));
        }

        (holds(&set) && set.iter().map(|b| engine::body_cost(b)).sum::<i32>() <= worth)
            .then_some((set, outcome))
    }

    /// Each room an enemy works a sweep may commit to, its worth, the nearest
    /// colony's walk and whether a set beats it, priced only on a tick a
    /// group there was new, changed or came back into sight (`stirred`).
    fn candidates(
        &self,
        atlas: &Atlas,
        grounds: &BTreeMap<String, Hunting>,
        stirred: &BTreeSet<String>,
    ) -> Vec<(String, i32, i32, bool)> {
        grounds
            .iter()
            .filter(|(name, g)| stirred.contains(*name) && g.worth > 0)
            .filter_map(|(name, g)| {
                let room = self.in_sight(name).filter(|r| {
                    r.controller
                        .as_ref()
                        .is_none_or(|c| !c.owner.is_ours() && !c.owner.is_ally())
                        && !self.homes.contains(&r.name)
                        && !Scene::safe(r)
                })?;
                let e = fresh(0, Objective::Sweep, name, 0, Tactic::Kite);
                let walk = self
                    .colonies_of(atlas, &e)
                    .iter()
                    .filter_map(|r| self.walk_to(atlas, &e, &r.name))
                    .min()?;
                let capacity = self.capacity_of(atlas, &e);
                let won = self.hunt(g, room, Vec::new(), capacity).is_some();
                Some((name.clone(), g.worth, walk, won))
            })
            .collect()
    }

    /// Priced at open and on an event in its room: an escort or towers no
    /// set the room pays for beats close it, or leave it unanswered, priced
    /// again as the bank grows, while it holds nothing.
    fn swept(
        &self,
        atlas: &Atlas,
        asked: &mut Asked,
        e: &Engagement,
        (free, grounds): (&mut Vec<String>, &BTreeMap<String, Hunting>),
    ) -> Option<Engagement> {
        let (Some(room), Some(ground)) = (self.in_sight(&e.room), grounds.get(&e.room)) else {
            return Some(e.clone());
        };

        let joining = self.joining(atlas, e, free);
        let mut fielding: Vec<Fighter> = e
            .ours
            .iter()
            .chain(&joining)
            .filter_map(|n| self.ours.get(n.as_str()))
            .map(|c| casting::fighter_of(c))
            .collect();
        fielding.extend(
            asked
                .casts
                .iter()
                .filter(|c| c.engagement == e.id)
                .map(|c| fighter::cast(&c.body)),
        );

        let capacity = self.capacity_of(atlas, e);
        let Some((set, outcome)) = self.hunt(ground, room, fielding.clone(), capacity) else {
            return fielding.is_empty().then(|| Engagement {
                unanswered: true,
                capacity,
                ..e.clone()
            });
        };
        free.retain(|n| !joining.contains(n));
        self.ask(atlas, e, &set, asked);

        let escort = &ground.escort;
        let mut next = e.clone();
        next.ours.extend(joining);
        // A set walks in as one; in its room each body's step is its own.
        next.plan = Plan {
            tactic: if next.ours.len() + set.len() > 1 {
                Tactic::Blob(Box::new(Tactic::Kite))
            } else {
                Tactic::Kite
            },
            order: casting::kill_order(escort),
        };
        next.unanswered = false;
        next.capacity = capacity;
        next.predicted = if e.unanswered && !escort.is_empty() {
            Some(outcome)
        } else {
            e.predicted
        };
        Some(next)
    }

    fn priced(
        &self,
        atlas: &mut Atlas,
        asked: &mut Asked,
        e: &Engagement,
        free: &mut Vec<String>,
        declarations: &Declarations,
    ) -> Engagement {
        if self.spent(e) {
            return e.clone();
        }

        let Some(g) = self.by_id.get(&e.group).cloned() else {
            return e.clone();
        };

        let Some(room) = self.in_sight(&g.room) else {
            return e.clone();
        };

        if e.phase == Phase::Disengaging {
            return e.clone();
        }

        // A home's towers leave a body they cannot out-heal, armed or not, to its Defend.
        let raid: Vec<Creep> = self.foes[&room.name]
            .iter()
            .filter(|h| {
                self.armed.contains(&h.id)
                    || self.homes.contains(&room.name) && g.members.contains(&h.id)
            })
            .cloned()
            .collect();

        let theirs = Side {
            bodies: raid
                .iter()
                .filter(|h| !self.cores.contains(&h.id))
                .map(casting::fighter_of)
                .collect(),
            ramparts: BTreeMap::new(),
            towers: Vec::new(),
        };

        let joining = self.joining(atlas, e, free);

        let mut fielding_names: Vec<String> = e.ours.iter().cloned().collect();
        fielding_names.extend(joining.iter().cloned());

        let fielding: Vec<&Creep> = fielding_names
            .iter()
            .filter_map(|n| self.ours.get(n.as_str()).copied())
            .collect();

        let bodies: Vec<Fighter> = fielding.iter().map(|c| casting::fighter_of(c)).collect();

        let asked_bodies: Vec<casting::Body> = asked
            .casts
            .iter()
            .filter(|c| c.engagement == e.id)
            .map(|c| c.body.clone())
            .collect();

        let asked_fighters: Vec<Fighter> = asked_bodies.iter().map(|b| fighter::cast(b)).collect();

        let core_hits: i32 = raid
            .iter()
            .filter(|h| self.cores.contains(&h.id))
            .map(|h| h.hits)
            .sum();

        let colonies = self.colonies_of(atlas, e);
        let capacity = self.capacity_of(atlas, e);
        let terrain = casting::terrain_at(room, &g.centre);

        // A home is a post the raid comes to where our ramparts stand on it;
        // with none, or no striker to come, a Hold stands still while the raid stands off.
        let stance = if self.homes.contains(&e.room)
            && g.room == e.room
            && theirs.bodies.iter().any(fighter::strikes)
            && room
                .structures
                .iter()
                .any(|s| s.kind.is_rampart() && s.owner.is_ours())
        {
            Stance::Hold
        } else {
            Stance::Standoff
        };

        // An outpost is worth the yield the raid's life would stand it down
        // for; a core is the razer's and holds nothing at the wave's cost.
        let (copies_of, tick_worth): (CopiesOf<'_>, i32) =
            match declarations.outposts.iter().find(|o| o.room == e.room) {
                Some(outpost) if !self.homes.contains(&e.room) => {
                    let tick_worth = outpost.sources.len() as i32 * engine::HELD_OUTPUT_PER_TICK;

                    let ticks = raid
                        .iter()
                        .filter(|h| !self.cores.contains(&h.id))
                        .fold(0, |most, h| most.max(h.ticks_to_live));

                    let spawns = self
                        .world
                        .spawns
                        .iter()
                        .filter(|s| colonies.iter().any(|r| r.name == s.pos.room))
                        .count() as i32;

                    let razing = casting::razer(capacity, core_hits, &[]);

                    let fielded_cost: i32 = fielding
                        .iter()
                        .map(|c| c.body.iter().map(|p| p.kind).collect::<Vec<_>>())
                        .chain(asked_bodies.iter().cloned())
                        .filter(|b| !razing.contains(b))
                        .map(|b| engine::body_cost(&b))
                        .sum();

                    let worth = tick_worth * ticks - fielded_cost;

                    let spawn_ticks = spawns * ticks
                        - engine::SPAWN_TICKS_PER_PART
                            * asked_bodies.iter().map(|b| b.len() as i32).sum::<i32>();

                    (
                        Box::new(move |body: &casting::Body| {
                            casting::copies_within(worth, spawn_ticks, body)
                        }),
                        tick_worth,
                    )
                }
                _ => (Box::new(casting::home_copies), 0),
            };

        let mut ours_now: Vec<Fighter> = bodies.clone();
        ours_now.extend(asked_fighters.iter().cloned());

        match casting::price(
            stance,
            &casting::side_in(room, ours_now),
            &theirs,
            terrain,
            capacity,
            copies_of,
            tick_worth,
        ) {
            Some((set, outcome, tactic)) => {
                free.retain(|n| !joining.contains(n));

                let mut priced_bodies: Vec<Fighter> = bodies.clone();
                priced_bodies.extend(asked_fighters.iter().cloned());
                priced_bodies.extend(set.iter().map(|b| fighter::cast(b)));

                let mut set = set;
                set.extend(casting::razer(capacity, core_hits, &priced_bodies));

                self.ask(atlas, e, &set, asked);

                let mut next = e.clone();
                next.ours.extend(joining.iter().cloned());
                next.plan = Plan {
                    tactic,
                    order: casting::kill_order(&raid),
                };
                next.budget = e.lost
                    + match outcome {
                        Outcome::Win(_, losses) => losses.dead,
                        _ => 0,
                    };
                next.unanswered = false;
                next.capacity = capacity;
                // A core alone is no fight Exchange predicts (§10.1).
                next.predicted = if !e.unanswered {
                    e.predicted
                } else if theirs.bodies.is_empty() {
                    None
                } else {
                    Some(outcome)
                };

                let standing: Vec<Fighter> = next
                    .ours
                    .iter()
                    .filter_map(|n| self.ours.get(n.as_str()))
                    .map(|c| casting::fighter_of(c))
                    .collect();

                if casting::wins(stance, &casting::side_in(room, standing), &theirs, terrain) {
                    defend::stands_alone(self.world.time, &next)
                } else {
                    next
                }
            }
            None if e.unanswered || e.ours.is_empty() && asked_fighters.is_empty() => Engagement {
                unanswered: true,
                capacity,
                ..e.clone()
            },
            None => defend::give_up(self.homes.contains(&e.room), self.world.time, e),
        }
    }
}

fn middle(room: &str) -> Pos {
    Pos::new(
        room.to_string(),
        engine::ROOM_SIDE / 2,
        engine::ROOM_SIDE / 2,
    )
}

fn fresh(id: i32, objective: Objective, room: &str, time: i32, tactic: Tactic) -> Engagement {
    Engagement {
        id,
        objective,
        group: 0,
        room: room.to_string(),
        ours: BTreeSet::new(),
        plan: Plan {
            tactic,
            order: Vec::new(),
        },
        phase: Phase::Forming,
        opened: time,
        since: time,
        lost: 0,
        budget: 0,
        priority: 0.0,
        unanswered: true,
        capacity: 0,
        predicted: None,
    }
}

fn changed(e: &Engagement, events: &[tracker::Event], by_id: &BTreeMap<i32, Group>) -> bool {
    events.iter().any(|event| match event {
        tracker::Event::Split { from, into } => *from == e.group || *into == e.group,
        tracker::Event::Merged { into, .. } => *into == e.group,
        tracker::Event::Left(id) => *id == e.group,
        tracker::Event::Newcomer(id) => by_id.get(id).is_some_and(|g| g.room == e.room),
        _ => false,
    })
}

pub fn decide(context: &mut Context, state: &State) -> Result<(Vec<Intent>, State), String> {
    let world = context.world;
    let declarations = context.declarations;
    let context_trace = context.trace;
    let sightings = context.sightings;
    let atlas: &mut Atlas = &mut *context.atlas;

    let mut foes: BTreeMap<String, Vec<Creep>> = BTreeMap::new();
    let mut cores: BTreeSet<String> = BTreeSet::new();

    for r in &world.rooms {
        let razed: Vec<Creep> = r
            .structures
            .iter()
            .filter(|s| s.kind.is_invader_core() && s.level == 0)
            .map(casting::core_as_hostile)
            .collect();

        for c in &razed {
            cores.insert(c.id.clone());
        }

        let mut all: Vec<Creep> = r.hostiles().cloned().collect();
        all.extend(razed);
        foes.insert(r.name.clone(), all);
    }

    let hostiles: Vec<Creep> = world
        .rooms
        .iter()
        .flat_map(|r| foes[&r.name].clone())
        .collect();

    let owned = hostiles::owned(&world.rooms);
    let held = state
        .engagements
        .iter()
        .fold(0, |most, e| most.max(e.group));

    let (groups, events) = tracker::step(
        tracker::LOST_GROUP_TICKS,
        held,
        &owned,
        &state.groups,
        &hostiles,
        world.time,
    );

    let homes: BTreeSet<String> = tick::colonies(world).into_iter().collect();
    let worked: BTreeSet<String> = declarations
        .outposts
        .iter()
        .map(|o| o.room.clone())
        .collect();

    let by_id: BTreeMap<i32, Group> = groups.iter().map(|g| (g.id, g.clone())).collect();
    let hostile_at: BTreeMap<String, Pos> = hostiles
        .iter()
        .map(|h| (h.id.clone(), h.pos.clone()))
        .collect();
    let ours: BTreeMap<&str, &Creep> = world.creeps.iter().map(|c| (c.name.as_str(), c)).collect();
    let standing: BTreeSet<String> = world.creeps.iter().map(|c| c.name.clone()).collect();
    let aging_out: BTreeSet<String> = world
        .creeps
        .iter()
        .filter(|c| c.ticks_to_live <= 1)
        .map(|c| c.name.clone())
        .collect();

    let (mut engagements, into) = merged(&events, &state.engagements);

    let armed: BTreeSet<String> = hostiles
        .iter()
        .filter(|h| cores.contains(&h.id) || hostiles::armed(&owned, &hostiles, h))
        .map(|h| h.id.clone())
        .collect();

    // A Defend left open holds its room: a newcomer inside it is its target
    // now, with a fresh budget, where its own group is not there (a home's,
    // so a second raid is not left to towers) or was lost from sight.
    for e in &mut engagements {
        if !matches!(e.objective, Objective::Defend) {
            continue;
        }

        let in_room = |id: i32| -> bool {
            by_id
                .get(&id)
                .is_some_and(|g| g.room == e.room && g.last_seen == world.time)
        };

        let gone = if homes.contains(&e.room) {
            !in_room(e.group)
        } else {
            !by_id.contains_key(&e.group)
        };

        let newcomer = events.iter().find_map(|event| match event {
            tracker::Event::Newcomer(id)
                if gone && in_room(*id) && by_id[id].members.iter().any(|m| armed.contains(m)) =>
            {
                Some(*id)
            }
            _ => None,
        });

        if let Some(id) = newcomer {
            e.group = id;
            e.opened = world.time;
            e.budget = e.lost;
            e.unanswered = true;
            e.predicted = None;
        }
    }

    let alive: BTreeSet<i32> = engagements.iter().map(|e| e.id).collect();

    let (landed, waiting): (Vec<Cast>, Vec<Cast>) = state
        .casts
        .iter()
        .map(|c| match into.get(&c.engagement) {
            Some(&survivor) => Cast {
                engagement: survivor,
                ..c.clone()
            },
            None => c.clone(),
        })
        .filter(|c| {
            alive.contains(&c.engagement)
                && world
                    .rooms
                    .iter()
                    .any(|r| r.name == c.room && r.energy_capacity >= engine::body_cost(&c.body))
        })
        .partition(|c| standing.contains(&c.name));

    for e in &mut engagements {
        let landed_here: BTreeSet<String> = landed
            .iter()
            .filter(|c| c.engagement == e.id)
            .map(|c| c.name.clone())
            .collect();
        e.ours.extend(landed_here);
    }

    let scene = Scene {
        world,
        sightings,
        avoid: &declarations.avoid,
        homes,
        worked,
        foes,
        cores,
        by_id,
        hostile_at,
        ours,
        armed,
        landed,
        waiting,
    };

    // A planned barrier fallen advances the plan, never re-prices it; a
    // target fallen re-plans toward the next razed.
    let mut retargeted: BTreeSet<i32> = BTreeSet::new();

    for e in &mut engagements {
        if let Objective::Siege(target) = &e.objective
            && let Some(room) = scene.in_sight(&e.room)
        {
            let barriers = scene.barrier_at(room);
            e.plan.order.retain(|id| barriers.contains_key(id.as_str()));

            let near = scene.members_of(e).first().map(|c| c.pos.clone());

            if room.structures.iter().all(|s| s.id != *target)
                && let Some(next) = siege::next_razed(&room.structures, near.as_ref())
            {
                e.objective = Objective::Siege(next.id.clone());
                retargeted.insert(e.id);
            }
        }
    }

    let stirred: BTreeSet<String> = groups
        .iter()
        .filter(|g| g.last_seen == world.time)
        .filter(|g| {
            let was = state.groups.iter().find(|p| p.id == g.id);
            was.is_none_or(|p| p.last_seen < world.time - 1 || p.members != g.members)
        })
        .map(|g| g.room.clone())
        .collect();
    let sweeps = || {
        engagements
            .iter()
            .filter(|e| e.objective == Objective::Sweep)
    };
    let grounds: BTreeMap<String, Hunting> = (state.preyed.iter())
        .filter(|room| stirred.contains(*room))
        .chain(sweeps().map(|e| &e.room))
        .filter_map(|room| Some((room.clone(), scene.ground(atlas, scene.in_sight(room)?))))
        .collect();
    let cornered: BTreeSet<i32> = sweeps()
        .filter(|e| {
            let Some(ground) = grounds.get(&e.room) else {
                return false;
            };
            let members = scene.members_of(e);
            members.iter().filter(|c| c.pos.room == e.room).any(|c| {
                let mates: Vec<&Creep> = members
                    .iter()
                    .copied()
                    .filter(|m| m.name != c.name)
                    .collect();
                tactics::cornered(atlas, &ground.threat, &mates, c)
            })
        })
        .map(|e| e.id)
        .collect();
    let candidates = scene.candidates(atlas, &grounds, &stirred);

    let stepped: Vec<(Engagement, bool)> = engagements
        .iter()
        .filter_map(|e| {
            let next = match &e.objective {
                Objective::Defend => defend::step(&scene.facts_of(e, &standing, &aging_out), e),
                Objective::Siege(target) => {
                    siege::step(&scene.siege_facts(e, target, &standing, &aging_out), e)
                }
                Objective::Sweep => sweep::step(
                    &sweep::Facts {
                        time: world.time,
                        standing: standing.clone(),
                        aging_out: aging_out.clone(),
                        asked: scene.waiting.iter().any(|c| c.engagement == e.id),
                        arrived: scene.members_of(e).iter().all(|c| c.pos.room == e.room),
                        worth: grounds.get(&e.room).map(|g| g.worth > 0),
                        safe: scene.in_sight(&e.room).is_some_and(Scene::safe),
                        elsewhere: candidates.iter().any(|(r, _, _, won)| *won && *r != e.room),
                    },
                    e,
                ),
            }?;

            Some((
                next.clone(),
                next.ours.len() < e.ours.len() || retargeted.contains(&e.id),
            ))
        })
        .collect();

    let targeted: BTreeSet<String> = stepped
        .iter()
        .flat_map(|(e, _)| {
            let mut rooms = vec![e.room.clone()];

            if let Some(g) = scene.by_id.get(&e.group) {
                rooms.push(g.room.clone());
            }

            rooms
        })
        .collect();

    // Past every id held last tick, so none closed this tick is reused by one opened.
    let first_id = 1 + state.engagements.iter().fold(0, |most, e| most.max(e.id));

    let opened: Vec<(Engagement, bool)> = defend::opens(
        &scene.homes,
        &scene.worked,
        |id| scene.armed.contains(id),
        world.time,
        &targeted,
        &groups,
    )
    .into_iter()
    .enumerate()
    .map(|(i, (g, priority))| {
        let e = fresh(
            first_id + i as i32,
            Objective::Defend,
            &g.room,
            world.time,
            Tactic::Close,
        );
        (
            Engagement {
                group: g.id,
                priority,
                ..e
            },
            true,
        )
    })
    .collect();

    let besieging: BTreeSet<&str> = stepped
        .iter()
        .filter(|(e, _)| matches!(e.objective, Objective::Siege(_)))
        .map(|(e, _)| e.room.as_str())
        .collect();

    let declared: Vec<(Engagement, bool)> = declarations
        .sieges
        .iter()
        .filter(|room| !besieging.contains(room.as_str()))
        .filter_map(|room| {
            let room = scene.in_sight(room).filter(|r| !Scene::safe(r))?;
            siege::next_razed(&room.structures, None)
        })
        .enumerate()
        .map(|(i, target)| {
            let id = first_id + (opened.len() + i) as i32;
            let siege = Objective::Siege(target.id.clone());
            let tactic = Tactic::Blob(Box::new(Tactic::Close));
            (
                fresh(id, siege, &target.pos.room, world.time, tactic),
                false,
            )
        })
        .collect();

    let sweeping = stepped.iter().any(|(e, _)| e.objective == Objective::Sweep);
    let won = || candidates.iter().filter(|c| c.3);
    let swept = sweep::choice(won().map(|(r, w, k, _)| (r, *w, *k)))
        .or_else(|| sweep::choice(candidates.iter().map(|(r, w, k, _)| (r, *w, *k))))
        .filter(|_| !sweeping)
        .map(|room| {
            let id = first_id + (opened.len() + declared.len()) as i32;
            (
                fresh(id, Objective::Sweep, &room, world.time, Tactic::Kite),
                false,
            )
        });

    let committed: BTreeSet<String> = stepped
        .iter()
        .fold(BTreeSet::new(), |all: BTreeSet<String>, (e, _)| {
            all.union(&e.ours).cloned().collect()
        });

    let mut free: Vec<String> = world
        .creeps
        .iter()
        .filter(|c| {
            tick::fights(&c.name) && !(committed.contains(&c.name) || aging_out.contains(&c.name))
        })
        .map(|c| c.name.clone())
        .collect();

    let mut asked = Asked {
        queued: BTreeMap::new(),
        casts: scene.waiting.clone(),
    };

    for c in &scene.waiting {
        asked.queue(&c.room, &c.body);
    }

    let mut ranked: Vec<(Engagement, bool)> = stepped;
    ranked.extend(opened);
    ranked.extend(declared);
    ranked.extend(swept);
    ranked.sort_by(|(a, _), (b, _)| {
        b.priority
            .partial_cmp(&a.priority)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then(a.id.cmp(&b.id))
    });

    let mut decided: Vec<Engagement> = Vec::new();

    for (e, left) in ranked {
        let grew = e.unanswered && scene.capacity_of(atlas, &e) > e.capacity;

        match &e.objective {
            Objective::Siege(target) => {
                if let Some(next) = scene.besieged(atlas, &mut asked, &e, target, left, grew) {
                    decided.push(next);
                }
            }
            Objective::Sweep
                if e.since == world.time && e.phase == Phase::Forming
                    || grew
                    || left
                    || scene.landed.iter().any(|c| c.engagement == e.id)
                    || cornered.contains(&e.id)
                    || changed(&e, &events, &scene.by_id) =>
            {
                decided.extend(scene.swept(atlas, &mut asked, &e, (&mut free, &grounds)));
            }
            Objective::Defend
                if left
                    || grew
                    || changed(&e, &events, &scene.by_id)
                    || e.since == world.time && e.phase == Phase::Forming =>
            {
                decided.push(scene.priced(atlas, &mut asked, &e, &mut free, declarations));
            }
            Objective::Defend | Objective::Sweep => decided.push(e),
        }
    }

    let kept: BTreeSet<i32> = decided
        .iter()
        .filter(|e| !(e.phase == Phase::Disengaging || scene.spent(e)))
        .map(|e| e.id)
        .collect();

    let mut casts = asked.casts;
    casts.retain(|c| kept.contains(&c.engagement));

    let ovens: BTreeSet<String> = world
        .spawns
        .iter()
        .filter_map(|s| s.spawning.as_ref().map(|o| o.name.clone()))
        .collect();

    let mut sight = tactics::Sight {
        atlas,
        hostiles: &scene.foes,
        struck: scene
            .foes
            .values()
            .flatten()
            .filter(|h| scene.struck(h))
            .map(|h| h.id.clone())
            .collect(),
        spared: world
            .rooms
            .iter()
            .map(|r| {
                let creeps = r.others.iter().filter(|c| !scene.struck(c));
                let allied = r.structures.iter().filter(|s| s.owner.is_ally());
                let at = creeps.map(|c| c.pos.clone());
                (
                    r.name.clone(),
                    at.chain(allied.map(|s| s.pos.clone())).collect(),
                )
            })
            .collect(),
        ramparts: world
            .rooms
            .iter()
            .map(|r| {
                (
                    r.name.clone(),
                    r.structures
                        .iter()
                        .filter(|s| s.kind.is_rampart() && s.owner.is_ours())
                        .map(|s| s.pos.clone())
                        .collect(),
                )
            })
            .collect(),
        homes: world
            .spawns
            .iter()
            .filter(|s| scene.homes.contains(&s.pos.room))
            .map(|s| s.pos.clone())
            .collect(),
        avoid: &declarations.avoid,
    };

    let mut engaged_in: BTreeMap<String, usize> = BTreeMap::new();

    for (at, e) in decided.iter().enumerate() {
        for n in &e.ours {
            engaged_in.insert(n.clone(), at);
        }
    }

    let fighters: Vec<&Creep> = world
        .creeps
        .iter()
        .filter(|c| tick::fights(&c.name))
        .collect();

    let swings = |c: &Creep| c.body.iter().any(|p| p.kind.is_attack());

    // A group lost from sight is sought in its room, which brings it back in sight.
    let centre_of = |e: &Engagement| -> Pos {
        match (
            &e.objective,
            scene.by_id.get(&e.group),
            scene.next_struck(e),
        ) {
            (Objective::Siege(_), _, Some(b)) => b.pos.clone(),
            (Objective::Defend, Some(g), _) => g.centre.clone(),
            _ => middle(&e.room),
        }
    };

    let mut walking: BTreeMap<String, Wish> = BTreeMap::new();

    for e in &decided {
        if e.phase == Phase::Approaching
            && matches!(e.plan.tactic, Tactic::Blob(_))
            && !e.unanswered
        {
            let members: Vec<&Creep> = scene
                .members_of(e)
                .into_iter()
                .filter(|c| !swings(c))
                .collect();

            if !members.is_empty() {
                let centre = centre_of(e);

                for wish in tactics::blob(&mut sight, &centre, &members) {
                    walking.insert(wish.creep.clone(), wish);
                }
            }
        }
    }

    let mut fronts: BTreeMap<i32, tactics::Front> = BTreeMap::new();

    for e in &decided {
        if let (Objective::Siege(_), Some(room), Some(wall)) =
            (&e.objective, scene.in_sight(&e.room), scene.next_struck(e))
        {
            let towers = scene.charged(room);

            let strike = towers
                .iter()
                .map(|t| t.range(&wall.pos).map_or(0, engine::tower_attack_at))
                .sum();

            fronts.insert(
                e.id,
                tactics::Front {
                    breach: wall.pos.clone(),
                    wall: wall.id.clone(),
                    line: room
                        .structures
                        .iter()
                        .filter(|s| Scene::rival(s))
                        .map(|s| s.pos.clone())
                        .collect(),
                    posts: if scene.members_of(e).iter().any(|c| tactics::picket_of(c)) {
                        Scene::exposed(&*sight.atlas, room)
                            .into_iter()
                            .map(|post| {
                                let strike = exchange::tower_strike(&towers, &post);
                                let worth = Scene::denied(room, &post);
                                let n = casting::squad(e.capacity, strike, worth).len();
                                (post, n)
                            })
                            .filter(|(_, n)| *n > 0)
                            .collect()
                    } else {
                        Vec::new()
                    },
                    towers,
                    strike,
                },
            );
        }
    }

    let hunts: BTreeMap<i32, (&Hunting, bool)> = decided
        .iter()
        .filter(|e| e.objective == Objective::Sweep)
        .filter_map(|e| {
            let ground = grounds.get(&e.room)?;
            // The whole set scores its tiles at once, or none does (ibex).
            let contact = scene.members_of(e).iter().any(|m| {
                m.pos.room == e.room
                    && (ground.threat[tiles::of_pos(&m.pos)] > 0
                        || ground.marks.iter().any(|t| m.pos.within(4, &t.pos)))
            });
            Some((e.id, (ground, contact)))
        })
        .collect();

    let mut traced: Vec<Intent> = Vec::new();
    let mut lookups = 0;
    let mut step = |sight: &mut tactics::Sight, c: &Creep| -> (Option<CreepAct>, Wish) {
        let Some(&at) = engaged_in.get(&c.name) else {
            return (None, tactics::retreat(sight, c));
        };

        let e = &decided[at];

        // A picket walks with the set and takes its post once the set fights.
        if e.phase == Phase::Fighting
            && tactics::picket_of(c)
            && let Some(front) = fronts.get(&e.id)
            && !front.posts.is_empty()
        {
            return (None, tactics::picket(sight, front, &scene.members_of(e), c));
        }

        if e.phase == Phase::Fighting && fronts.contains_key(&e.id) {
            let members: Vec<&Creep> = scene
                .members_of(e)
                .into_iter()
                .filter(|m| !tactics::picket_of(m))
                .collect();
            return tactics::siege(sight, &fronts[&e.id], &members, c);
        }

        let centre = centre_of(e);
        let order = &e.plan.order;

        if e.unanswered {
            return (None, tactics::rampart(sight, c));
        }

        let wish = match e.phase {
            Phase::Forming => tactics::hold(sight, c),
            Phase::Pursuing => tactics::close(sight, order, &centre, c),
            Phase::Disengaging => tactics::retreat(sight, c),
            Phase::Approaching | Phase::Fighting => {
                if let Some(wish) = walking.get(&c.name) {
                    return (None, wish.clone());
                }

                // A transit room's hostiles are not the sweep's.
                if e.objective == Objective::Sweep && c.pos.room != e.room {
                    return (
                        None,
                        tactics::toward(sight, c, &centre, engine::RANGED_RANGE),
                    );
                }

                if let Some((ground, contact)) = hunts.get(&e.id) {
                    let mates: Vec<&Creep> = scene
                        .members_of(e)
                        .into_iter()
                        .filter(|m| m.name != c.name && m.pos.room == c.pos.room)
                        .collect();
                    let (strike, wish, scored) = tactics::hunt(
                        sight,
                        &ground.threat,
                        &ground.marks,
                        &mates,
                        (&ground.at, *contact),
                        c,
                    );
                    lookups += scored.len() as i32;
                    let at = |(t, s): &(Tile, i64)| (tiles::to_pos(&c.pos.room, *t), *s);
                    if context_trace && !scored.is_empty() {
                        traced.push(Intent::Trace(
                            c.name.clone(),
                            scored.iter().map(at).collect(),
                        ));
                    }
                    return (strike, wish);
                }

                let tactic = &e.plan.tactic;
                let blobbed = |inner: &Tactic| -> bool {
                    matches!(tactic, Tactic::Blob(boxed) if **boxed == *inner)
                };

                if blobbed(&Tactic::Close)
                    && tactics::exposed(sight, &centre.room, &scene.members_of(e), c)
                {
                    tactics::step_back(sight, &centre.room, &scene.members_of(e), c)
                } else if (matches!(tactic, Tactic::Kite) || blobbed(&Tactic::Kite)) && !swings(c) {
                    tactics::kite(sight, order, &centre, c)
                } else if matches!(tactic, Tactic::Hold) {
                    tactics::rampart(sight, c)
                } else {
                    tactics::close(sight, order, &centre, c)
                }
            }
        };
        (None, wish)
    };

    let mut homes_lost: Vec<Home> = Vec::new();

    for name in &scene.homes {
        let Some(room) = scene.in_sight(name) else {
            continue;
        };

        let own: Vec<Structure> = room
            .structures
            .iter()
            .filter(|s| s.owner.is_ours())
            .cloned()
            .collect();

        let raiding: Vec<&Engagement> = decided
            .iter()
            .filter(|e| e.unanswered && e.room == *name && scene.by_id.contains_key(&e.group))
            .collect();

        let raid: BTreeSet<String> = raiding
            .iter()
            .flat_map(|e| scene.by_id[&e.group].members.iter().cloned())
            .collect();

        if let Some(controller) = &room.controller {
            let opened = raiding
                .iter()
                .filter(|e| {
                    room.hostiles().any(|h| {
                        h.owner != Owner::Invader && scene.by_id[&e.group].members.contains(&h.id)
                    })
                })
                .fold(world.time, |first, e| first.min(e.opened));

            homes_lost.push(Home {
                controller: controller.clone(),
                towered: own.iter().any(|s| s.kind.is_tower()),
                raid: room
                    .hostiles()
                    .filter(|h| raid.contains(&h.id))
                    .cloned()
                    .collect(),
                hostiles: room.hostiles().cloned().collect(),
                ours: own,
                bodies: world
                    .creeps
                    .iter()
                    .filter(|c| c.pos.room == *name)
                    .cloned()
                    .collect(),
                time: world.time,
                opened,
                fallen: room
                    .remains
                    .iter()
                    .filter_map(|r| r.fallen.clone())
                    .collect(),
            });
        }
    }

    let running = world.rooms.iter().any(|r| {
        r.controller
            .as_ref()
            .is_some_and(|c| c.owner.is_ours() && c.safe_mode > 0)
    });

    let mut acts: Vec<Intent> = Vec::new();

    if let Some(id) = defend::safe_mode(running, &homes_lost) {
        acts.push(Intent::Controller(id, ControllerAct::ActivateSafeMode));
    }

    let incoming: BTreeMap<String, i32> = decided
        .iter()
        .filter_map(|e| {
            let front = fronts.get(&e.id)?;
            let members: Vec<&Creep> = scene
                .members_of(e)
                .into_iter()
                .filter(|m| m.pos.room == front.breach.room)
                .collect();

            tactics::focus_of(front, &members).map(|focus| (focus.name.clone(), front.strike))
        })
        .collect();

    let mut chosen: BTreeMap<String, Vec<CreepAct>> = BTreeMap::new();

    for c in &fighters {
        let (own, wish) = step(&mut sight, c);
        let order = engaged_in
            .get(&c.name)
            .map_or(&[][..], |&at| decided[at].plan.order.as_slice());
        let mut wanted: Vec<CreepAct> = own.into_iter().collect();
        // A hunt's scored shot stands for the reflex.
        if wanted.iter().all(|a| a.slots() & intents::RANGED == 0) {
            wanted.extend(tactics::strike(&sight, order, c));
        }
        chosen.insert(c.name.clone(), wanted);
        acts.push(Intent::Wish(c.pos.room.clone(), wish));
    }

    acts.extend(traced);
    acts.push(Intent::Count("threatLookups", lookups));
    let busy = |slot: u8| -> BTreeSet<String> {
        let taken = chosen
            .iter()
            .filter(|(_, a)| a.iter().any(|a| a.slots() & slot != 0));
        taken.map(|(n, _)| n.clone()).collect()
    };
    let (swinging, shooting) = (busy(intents::MELEE), busy(intents::RANGED));
    for (healer, act) in tactics::heals(&fighters, &world.creeps, &incoming, &swinging, &shooting) {
        chosen.entry(healer).or_default().push(act);
    }

    let mut out: Vec<Intent> = casts
        .iter()
        .filter(|c| !ovens.contains(&c.name))
        .map(|c| {
            Intent::Queue(
                c.room.clone(),
                Ask {
                    body: c.body.clone(),
                    name: c.name.clone(),
                    rank: Rank::Fight,
                },
            )
        })
        .collect();

    out.extend(acts);
    out.extend(chosen.into_iter().flat_map(|(name, wanted)| {
        let done = intents::performed(&wanted);
        done.into_iter()
            .map(move |a| Intent::Creep(name.clone(), a))
    }));

    let mut engagements = decided;
    engagements.sort_by_key(|e| e.id);

    Ok((
        out,
        State {
            groups,
            engagements,
            casts,
            ..state.clone()
        },
    ))
}

pub static FIGHT: Module = Module {
    name: "fight",
    run: decide,
};
