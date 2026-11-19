/// What each engagement commits to: its set, its tactic and what it asks of the ovens (§6.3).
use std::collections::{BTreeMap, BTreeSet};

use super::casting;
use super::kinds::{defend, raid, siege, sweep};
use super::scene::{Asked, Hunting, Scene, fresh, middle, picket};
use super::shape::{Engagement, Group, Objective, Phase, Plan};
use crate::atlas::{self, Atlas, Mover, Target};
use crate::exchange::{self, Fighter, Ground, Losses, Outcome, Side, Stance, fighter};
use crate::grid::{self, Footprint};
use crate::tactics::{self, Offer, Tactic};
use crate::tick::{self, Declarations};
use crate::tiles::Tile;
use crate::world::{self, Creep, Id, Pos, Room, RoomName, Structure};
use crate::{engine, tiles};

/// `Casting.price`'s say in how many copies of a block to ask for.
type CopiesOf<'a> = Box<dyn Fn(&casting::Body) -> i32 + 'a>;

impl<'a> Scene<'a> {
    pub(super) fn breach_in(
        &self,
        atlas: &Atlas,
        room: &Room,
        target: &Structure,
        colony: RoomName,
        set: &[Fighter],
        standing: &[&Creep],
    ) -> Option<(Vec<Id>, Outcome)> {
        let barriers: Vec<&Structure> = siege::barrier_at(room).values().copied().collect();
        let line: Vec<Pos> = barriers.iter().map(|b| b.pos).collect();

        let mut weights = atlas.weights(room.name)?;
        let gait = gait(set)?;

        for b in &barriers {
            if room
                .structures
                .iter()
                .all(|s| s.pos != b.pos || s.walkable || Scene::rival(s))
            {
                weights[tiles::of_pos(&b.pos)] = grid::weight(
                    if world::terrain::is_swamp(room.terrain, b.pos.x, b.pos.y) {
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
            room: room.name,
            weights,
            barriers: hits_at,
        };

        let repair = engine::REPAIR_POWER
            * self.foes[&room.name]
                .iter()
                .map(|h| casting::fighter_of(h).work)
                .sum::<i32>();

        let rate = engine::DISMANTLE_POWER * set.iter().map(|f| f.work).sum::<i32>() - repair;

        let towers: Vec<Pos> = self.towers_in(room).into_iter().map(|s| s.pos).collect();

        let from = (self.world.spawns.iter())
            .find(|s| s.pos.room == colony)
            .map_or_else(|| middle(colony), |s| s.pos);
        let sources = entries(atlas, &ground, gait, &from, set, standing);

        let attempt = |seated: i32| {
            exchange::breach_plan(
                &ground,
                gait,
                &towers,
                &[],
                set,
                seated,
                |t| tactics::seats_at(atlas, &line, room.name, t),
                rate,
                &sources,
                tiles::around(tiles::of_pos(&target.pos)),
            )
        };

        let seats_of = |plan: &(Vec<(Tile, Tile)>, i32)| -> i32 {
            match plan.0.first() {
                Some((_, work)) => tactics::seats_at(atlas, &line, room.name, *work),
                None => set.len() as i32,
            }
        };

        // Every barrier on a tile, its rampart first: the tile stands
        // in the plan until it is walkable.
        let ids_at = |tile: Tile| -> Vec<Id> {
            let mut at: Vec<&Structure> = barriers
                .iter()
                .copied()
                .filter(|b| tiles::of_pos(&b.pos) == tile)
                .collect();

            at.sort_by_key(|b| (!b.kind.is_rampart(), b.id));
            at.into_iter().map(|b| b.id).collect()
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
    pub(super) fn besieged(
        &self,
        atlas: &Atlas,
        asked: &mut Asked,
        e: &Engagement,
        target: Id,
        left: bool,
        grew: bool,
    ) -> Option<Engagement> {
        let mut names: Vec<RoomName> = self.colonies_of(atlas, e).iter().map(|r| r.name).collect();
        names.sort();

        let colony = names.into_iter().next()?;
        let from = (self.world.spawns.iter())
            .find(|s| s.pos.room == colony)
            .map_or_else(|| middle(colony), |s| s.pos);

        let room = self.in_sight(e.room);
        if let Some(c) = room.and_then(|r| siege::tapped(r, target)) {
            return Some(self.tapped(atlas, asked, e, c));
        }
        let Some((room, goal)) =
            room.and_then(|r| Some((r, r.structures.iter().find(|s| s.id == target)?)))
        else {
            return Some(e.clone());
        };

        if e.unanswered && !grew {
            Some(e.clone())
        } else if e.unanswered {
            let capacity = self.capacity_of(atlas, e);

            let priced = |set: &[casting::Body]| {
                self.breach_in(atlas, room, goal, colony, &cast_all(set), &[])
            };
            // Shot in where no barrier stands in the way, which a ranged set's
            // plan never breaks: it dismantles nothing. What the dismantlers'
            // own plan has to break, they break, and a room whose way in to his
            // towers still stands behind a rampart keeps them (owner, 2026-10-21).
            let siege = casting::siege(capacity, &priced);
            let open = siege
                .as_ref()
                .is_some_and(|(_, (order, _))| order.is_empty());
            let towers = self.towers_in(room);
            let breached = towers
                .iter()
                .all(|t| raid::breached(room, |tile| atlas.passable(room.name, tile), &t.pos));
            // A raid razes; the room his safe mode holds is drained instead,
            // as nothing a set shoots at there can fall (§6.2).
            let raid = (open && breached && !Scene::safe(room))
                .then(|| {
                    let hard = raid::hardest(room).map_or_else(|| goal.pos, |s| s.pos);
                    let priced = |set: &[casting::Body]| {
                        raid::plan(atlas, room, &hard, &from, &cast_all(set), &[])
                    };

                    raid::reinforce(capacity, &[], priced)
                })
                .flatten();
            let planned = raid.clone().or(siege).and_then(|(set, (order, outcome))| {
                let charged = raid::charged(room);
                let mut set = set;

                if raid.is_none() {
                    for post in siege::exposed(atlas, room) {
                        let strike = exchange::tower_strike(&charged, &post);
                        set.extend(casting::squad(capacity, strike, siege::denied(room, &post)));
                    }
                }

                let declared = if raid.is_some() {
                    siege::RAID
                } else {
                    siege::CANDIDATES
                };
                let (candidates, pin) = self.ruled(e, declared);
                let offers = siege::offers(&candidates, room, &set, outcome);
                let tactic = tactics::select(&offers, pin)?.tactic;

                Some((set, order, outcome, offers, tactic))
            });

            match planned {
                Some((set, order, outcome, offers, tactic)) => {
                    self.ask(atlas, e, &set, asked);

                    Some(Engagement {
                        plan: Plan {
                            tactic,
                            order,
                            offers,
                        },
                        budget: e.lost,
                        unanswered: false,
                        capacity,
                        predicted: Some(outcome),
                        ..e.clone()
                    })
                }
                // Unstaffed, or no candidate the rules leave it: it waits on the bank to grow.
                None => Some(Engagement {
                    capacity,
                    ..e.clone()
                }),
            }
        } else if e.phase == Phase::Disengaging {
            Some(e.clone())
        } else if e.phase == Phase::Draining {
            // The dance's own bodies, on top of the breach set: a bouncer and
            // a medic a pair of the bank, and a lost one of the two is asked.
            if !siege::gave_up(e) {
                let pairs = casting::pairs(e.capacity) as usize;
                let missing: Vec<casting::Body> = (casting::pair().iter())
                    .flat_map(|body| {
                        let have = self.paired(e, &asked.casts, std::slice::from_ref(body));
                        std::iter::repeat_n(body.clone(), pairs.saturating_sub(have))
                    })
                    .collect();
                self.ask(atlas, e, &missing, asked);
            }

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
            let breaking: Vec<&Creep> = in_room.iter().copied().filter(|m| !picket(e, m)).collect();

            let towers: Vec<Pos> = self.towers_in(room).into_iter().map(|s| s.pos).collect();

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
                        let guards = if set.iter().all(|f| f.work == 0) {
                            raid::guards(room)
                        } else {
                            Vec::new()
                        };

                        struck(b) + exchange::guard_strike(&guards, &b.pos)
                            > exchange::pooled_heal(set.len() as i32, &set)
                            || self.unreached(atlas, room, b, &in_room)
                    }
                    None => false,
                };

            if !event {
                return Some(e.clone());
            }

            let planned = self
                .breach_in(atlas, room, goal, colony, &set, &breaking)
                .map(|plan| (Vec::new(), plan))
                .or_else(|| {
                    let fielded = self.fielded_bodies(e);
                    // A set is topped up as itself: guns with guns, a breach with
                    // the dismantlers and medics it walks in with. A raid is
                    // topped up to the set its price buys, or it does not enter
                    // (owner, 2026-10-21).
                    if raid::raids(e) {
                        let hard = raid::hardest(room).map_or_else(|| goal.pos, |s| s.pos);
                        let priced = |set: &[casting::Body]| {
                            raid::plan(atlas, room, &hard, &from, &cast_all(set), &breaking)
                        };

                        return raid::reinforce(e.capacity, &fielded, priced);
                    }

                    let priced = |set: &[casting::Body]| {
                        self.breach_in(atlas, room, goal, colony, &cast_all(set), &breaking)
                    };

                    casting::reinforce(e.capacity, &fielded, &priced)
                });

            // A plan whose first barrier the set cannot reach is no plan, or
            // the event would fire every tick; a top-up to it is none.
            let Some((added, (order, outcome))) = planned else {
                return Some(siege::replanned(self.world.time, false, e));
            };
            let reached = order.first().is_none_or(|first| {
                (siege::barrier_at(room).get(first))
                    .is_some_and(|b| !self.unreached(atlas, room, b, &in_room))
            });
            if !added.is_empty() {
                if !reached {
                    return Some(siege::replanned(self.world.time, false, e));
                }
                // Topped up, not recast: the set fights on as its medics walk in.
                self.ask(atlas, e, &added, asked);
            }
            let mut next = siege::replanned(self.world.time, reached, e);
            next.plan.order = order;
            next.predicted = Some(outcome);
            Some(next)
        }
    }

    pub(super) fn tapped(
        &self,
        atlas: &Atlas,
        asked: &mut Asked,
        e: &Engagement,
        c: &world::Controller,
    ) -> Engagement {
        let body = casting::tapper(self.capacity_of(atlas, e));

        if let Some(body) = body.as_ref().filter(|_| e.phase != Phase::Disengaging) {
            let held = (self.members_of(e).iter())
                .any(|m| tactics::claims(m) && m.ticks_to_live > c.upgrade_blocked)
                || asked.casts.iter().any(|cast| cast.engagement == e.id);
            let spawn = engine::SPAWN_TICKS_PER_PART * body.len() as i32;

            if !held && siege::tap_due(c, spawn) {
                self.ask(atlas, e, std::slice::from_ref(body), asked);
            }
        }

        // A raid keeps its own step: a structure or a site he raises while it
        // taps is still its work, and its set answered with or without a tapper.
        let raid = raid::raids(e);
        let mut next = e.clone();
        if !raid {
            next.plan.tactic = Tactic::Close;
        }
        // The tap is no candidate's, so the price's offers no longer explain it.
        next.plan.offers.clear();
        next.unanswered = body.is_none() && !raid;
        next
    }

    pub(super) fn joining(&self, atlas: &Atlas, e: &Engagement, free: &[String]) -> Vec<String> {
        free.iter()
            .filter(|n| {
                self.ours
                    .get(n.as_str())
                    .is_some_and(|c| self.walk_to(atlas, e, c.pos.room).is_some())
            })
            .cloned()
            .collect()
    }

    /// The set beside `ours` that beats the room's escort and out-heals the
    /// towers where it works, or what strikes `ours` where they stand (owner:
    /// a set tanking two towers at 20 is cheap), within what holding the room
    /// is worth.
    pub(super) fn hunt(
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
            &casting::walking(capacity),
            |body| worth / engine::body_cost(body),
            0,
            self.boost,
        )?;

        let seen = ours
            .iter()
            .filter_map(|f| f.pos.as_ref())
            .filter(|p| p.room == room.name);
        let strike = seen
            .map(|p| ground.threat[tiles::of_pos(p)])
            .fold(exchange::tower_strike(&raid::charged(room), at), i32::max);
        let holds = |set: &[casting::Body]| {
            let mut all = ours.clone();
            all.extend(set.iter().map(|b| fighter::cast(b)));
            casting::holds(&all, strike)
        };
        if !holds(&set) {
            set.extend(casting::squad(capacity, strike, worth));
        }

        (holds(&set) && set.iter().map(|b| engine::body_cost(b)).sum::<i32>() <= worth)
            .then_some((set, outcome))
    }

    /// Each room an enemy works a sweep may commit to, its worth, the nearest
    /// colony's walk and whether a set beats it, priced only on a tick a
    /// group there was new, changed or came back into sight (`stirred`). An
    /// avoided room is none, as it is no patrol's.
    pub(super) fn candidates(
        &self,
        atlas: &Atlas,
        grounds: &BTreeMap<RoomName, Hunting>,
        stirred: &BTreeSet<RoomName>,
    ) -> Vec<(RoomName, i32, i32, bool)> {
        grounds
            .iter()
            .filter(|(name, g)| {
                stirred.contains(*name) && g.worth > 0 && !self.avoid.contains(*name)
            })
            .filter_map(|(name, g)| {
                let room = self.in_sight(*name).filter(|r| {
                    r.controller
                        .as_ref()
                        .is_none_or(|c| !c.owner.is_ours() && !c.owner.is_ally())
                        && !self.homes.contains(&r.name)
                        && !Scene::safe(r)
                })?;
                let e = fresh(0, Objective::Sweep, *name, 0, Tactic::Kite);
                let walk = self
                    .colonies_of(atlas, &e)
                    .iter()
                    .filter_map(|r| self.walk_to(atlas, &e, r.name))
                    .min()?;
                let capacity = self.capacity_of(atlas, &e);
                let won = self.hunt(g, room, Vec::new(), capacity).is_some();
                Some((*name, g.worth, walk, won))
            })
            .collect()
    }

    pub(super) fn armed_seen(&self, g: &Group) -> bool {
        let seen = self.sightings.get(&g.room);
        (g.members.iter()).any(|m| seen.and_then(|s| s.get(m)).is_some_and(|s| s.armed))
    }

    pub(super) fn walk_of(&self, atlas: &Atlas, e: &Engagement, room: RoomName) -> Option<i32> {
        let from = self.members_of(e).first()?.pos.room;
        let onto = fresh(0, Objective::Sweep, room, 0, Tactic::Kite);
        self.walk_to(atlas, &onto, from)
    }

    /// A move asks no cast.
    pub(super) fn elsewhere(
        &self,
        atlas: &Atlas,
        e: &Engagement,
        candidates: &[(RoomName, i32, i32, bool)],
        grounds: &BTreeMap<RoomName, Hunting>,
    ) -> Option<RoomName> {
        let ours: Vec<Fighter> = (self.members_of(e).into_iter())
            .map(casting::fighter_of)
            .collect();
        let beats = |room: RoomName| match (self.in_sight(room), grounds.get(&room)) {
            (Some(r), Some(g)) => {
                (self.hunt(g, r, ours.clone(), 0)).is_some_and(|(set, _)| set.is_empty())
            }
            _ => false,
        };
        let won = candidates
            .iter()
            .filter(|c| c.0 != e.room && if ours.is_empty() { c.3 } else { beats(c.0) });
        atlas::onward(won.filter_map(|(room, worth, walk, _)| {
            let walk = if ours.is_empty() {
                Some(*walk)
            } else {
                self.walk_of(atlas, e, *room)
            };
            Some((*room, *worth, walk?))
        }))
    }

    /// An unseen room is worth its staleness; one owned is priced only in
    /// sight, under its towers, and one where the tracker still holds an armed
    /// group is not stale (owner), so a set never walks straight back to the
    /// escort it left.
    pub(super) fn patrol(
        &self,
        atlas: &Atlas,
        e: &Engagement,
        preyed: &[RoomName],
        age: impl Fn(RoomName) -> Option<(i32, bool)>,
    ) -> Option<RoomName> {
        let open = preyed
            .iter()
            .filter(|r| **r != e.room && !self.avoid.contains(*r))
            .filter(|r| (self.by_id.values()).all(|g| g.room != **r || !self.armed_seen(g)));
        atlas::onward(open.filter_map(|r| match age(*r)? {
            (_, true) => None,
            (age, false) => Some((*r, age, self.walk_of(atlas, e, *r)?)),
        }))
    }

    /// Priced at open, on arrival and on an event in its room, and again as
    /// the bank grows while it holds nothing.
    pub(super) fn swept(
        &self,
        atlas: &Atlas,
        asked: &mut Asked,
        (e, onward): (&Engagement, impl FnOnce() -> Option<RoomName>),
        (free, grounds): (&mut Vec<String>, &BTreeMap<RoomName, Hunting>),
    ) -> Option<Engagement> {
        let (Some(room), Some(ground)) = (self.in_sight(e.room), grounds.get(&e.room)) else {
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
            if fielding.is_empty() {
                return Some(Engagement {
                    unanswered: true,
                    capacity,
                    ..e.clone()
                });
            }
            let onward = onward().filter(|_| !e.ours.is_empty());
            return onward.map(|r| sweep::onto(r, self.world.time, e));
        };

        let escort = &ground.escort;
        // A set walks in as one; in its room each body's step is its own.
        let together = e.ours.len() + joining.len() + set.len() > 1;
        let worth = ground.worth + escort.iter().map(sweep::worth).sum::<i32>();
        let (candidates, pin) = self.ruled(e, sweep::CANDIDATES);
        let offers = sweep_offers(&candidates, together, worth, &set);

        let Some(tactic) = tactics::select(&offers, pin).map(|o| o.tactic) else {
            let onward = onward().filter(|_| !e.ours.is_empty());
            return onward.map(|r| sweep::onto(r, self.world.time, e));
        };

        free.retain(|n| !joining.contains(n));
        self.ask(atlas, e, &set, asked);

        let mut next = e.clone();
        next.ours.extend(joining);
        next.plan = Plan {
            tactic,
            order: casting::kill_order(escort),
            offers,
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

    pub(super) fn priced(
        &self,
        atlas: &Atlas,
        asked: &mut Asked,
        e: &Engagement,
        free: &mut Vec<String>,
        declarations: &Declarations,
    ) -> Engagement {
        self.priced_after(atlas, asked, e, free, declarations, (None, 0))
    }

    /// `wait`: the ticks a cast holds the set forming (None: the casts
    /// asked), and `fight` the ticks its fight is priced to.
    fn priced_after(
        &self,
        atlas: &Atlas,
        asked: &mut Asked,
        e: &Engagement,
        free: &mut Vec<String>,
        declarations: &Declarations,
        (wait, fight): (Option<i32>, i32),
    ) -> Engagement {
        let g = self
            .by_id
            .get(&e.group)
            .filter(|_| !self.spent(e) && e.phase != Phase::Disengaging);
        let Some((g, room)) = g.and_then(|g| Some((g.clone(), self.in_sight(g.room)?))) else {
            return e.clone();
        };

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

        let asked_bodies: Vec<casting::Body> = (asked.casts.iter())
            .filter(|c| c.engagement == e.id)
            .map(|c| c.body.clone())
            .collect();
        let asked_fighters = cast_all(&asked_bodies);

        // A forming set joins when its casts stand there, spawned one after
        // another and walked from the nearest colony; held by none, it goes
        // as it stands. Held, it counts a body only if it lives to the fight's
        // end, or bodies with 19-99 ticks to live price a win it reaches with one.
        let colony = (self.colonies_of(atlas, e).iter())
            .filter_map(|r| self.walk_to(atlas, e, r.name))
            .min();
        let waiting = |casts: &[casting::Body]| match casts.len() {
            0 => 0,
            _ => engine::SPAWN_TICKS_PER_PART * casts.concat().len() as i32 + colony.unwrap_or(0),
        };
        let wait = wait.unwrap_or_else(|| waiting(&asked_bodies));
        let outlives = |c: &Creep, wait: i32, fight: i32| {
            (self.walk_to(atlas, e, c.pos.room))
                .is_some_and(|w| c.ticks_to_live > wait.max(w) + fight)
        };
        let lasts = |c: &&Creep| wait == 0 || outlives(c, wait, fight);
        let joining: Vec<String> = (self.joining(atlas, e, free).into_iter())
            .filter(|n| self.ours.get(n.as_str()).is_some_and(lasts))
            .collect();

        let mut fielding_names: Vec<String> = e.ours.iter().cloned().collect();
        fielding_names.extend(joining.iter().cloned());

        let fielding: Vec<&Creep> = fielding_names
            .iter()
            .filter_map(|n| self.ours.get(n.as_str()).copied())
            .filter(lasts)
            .collect();

        let bodies: Vec<Fighter> = fielding.iter().map(|c| casting::fighter_of(c)).collect();

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

                    let fielded_cost: i32 = (fielding.iter().map(|c| crate::world::body_kinds(c)))
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

        let blocks = if self.homes.contains(&room.name) {
            casting::affordable(capacity)
        } else {
            casting::walking(capacity)
        };

        match casting::price(
            stance,
            &casting::side_in(room, ours_now),
            &theirs,
            terrain,
            &blocks,
            copies_of,
            tick_worth,
            self.boost,
        ) {
            Some((set, outcome, landed)) => {
                // The set asked here holds the rest: priced again without a
                // body that would not outlast it. Each pass drops one.
                let after = wait.max(waiting(&[&asked_bodies[..], &set[..]].concat()));
                let held = (Some(after), outcome.ticks());
                if after > 0 && fielding.iter().any(|c| !outlives(c, after, held.1)) {
                    return self.priced_after(atlas, asked, e, free, declarations, held);
                }

                let mut priced_bodies: Vec<Fighter> = bodies;
                priced_bodies.extend(asked_fighters.iter().cloned());
                priced_bodies.extend(set.iter().map(|b| fighter::cast(b)));

                let mut set = set;
                set.extend(casting::razer(capacity, core_hits, &priced_bodies));

                let price = Priced {
                    raid: theirs
                        .bodies
                        .iter()
                        .map(|f| engine::body_cost(&f.body))
                        .sum(),
                    ranged: landed.bodies.len() > 1 && landed.bodies.iter().all(|f| f.attack == 0),
                    holds: exchange::stance_against(stance, &landed, &theirs, terrain)
                        == Stance::Hold,
                    won: outcome.is_win(),
                    losses: outcome.losses(),
                    ticks: outcome.ticks(),
                    set,
                    tick_worth,
                };

                let (candidates, pin) = self.ruled(e, defend::CANDIDATES);
                let offers = price.offers(&candidates);

                let Some(tactic) = tactics::select(&offers, pin).map(|o| o.tactic) else {
                    return defend::give_up(self.homes.contains(&e.room), self.world.time, e);
                };

                free.retain(|n| !joining.contains(n));
                self.ask(atlas, e, &price.set, asked);

                let mut next = e.clone();
                next.ours.extend(joining.iter().cloned());
                next.plan = Plan {
                    tactic,
                    order: casting::kill_order(&raid),
                    offers,
                };
                next.budget = e.lost + outcome.losses().dead;
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
                    .filter(|c| lasts(c))
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

fn cast_all(set: &[casting::Body]) -> Vec<Fighter> {
    set.iter().map(|b| fighter::cast(b)).collect()
}

pub(super) fn gait(set: &[Fighter]) -> Option<grid::FatigueFactor> {
    (set.iter())
        .map(|f| f.gait)
        .filter(|g| g.moves > 0)
        .min_by_key(|g| std::cmp::Reverse(grid::step_ticks(*g, engine::PLAIN_WEIGHT)))
}

pub(super) fn entries(
    atlas: &Atlas,
    ground: &Ground,
    gait: grid::FatigueFactor,
    from: &Pos,
    set: &[Fighter],
    standing: &[&Creep],
) -> Vec<(Tile, i32, i32)> {
    // The atlas walks no body out of the room it stands in: a set inside
    // is planned from where it stands.
    if !standing.is_empty() {
        return (standing.iter())
            .map(|c| (tiles::of_pos(&c.pos), 0, set.len() as i32))
            .collect();
    }
    let mover = Mover {
        footprint: Footprint::One,
        factor: gait,
    };
    let open = |t: &Tile| ground.weights[*t].is_some();
    [tiles::TOP, tiles::RIGHT, tiles::BOTTOM, tiles::LEFT]
        .into_iter()
        .flat_map(|exit| tiles::exit_tiles(exit).split(|t| !open(t)))
        .filter(|stretch| !stretch.is_empty())
        .filter_map(|stretch| {
            let onto = Target {
                room: ground.room,
                tiles: stretch.to_vec(),
                range: engine::MELEE_RANGE,
                avoid: BTreeSet::new(),
                max_hops: tick::SIEGE_HOPS,
            };
            let walk = atlas.ticks_to(mover, &onto, from)?;
            Some((stretch.iter()).map(move |t| (*t, walk, stretch.len() as i32)))
        })
        .flatten()
        .collect()
}

/// What a `Defend`'s price found, for its candidates to read (§6.3).
pub(super) struct Priced {
    /// What the rival spent on the bodies the engagement fights.
    pub(super) raid: i32,
    /// The set swings nothing, so it fights as one body.
    pub(super) ranged: bool,
    /// A home's ramparts are a post the raid comes to.
    pub(super) holds: bool,
    pub(super) won: bool,
    pub(super) losses: Losses,
    pub(super) ticks: i32,
    pub(super) set: Vec<casting::Body>,
    pub(super) tick_worth: i32,
}

impl Priced {
    pub(super) fn offers(&self, candidates: &[Tactic]) -> Vec<Offer> {
        candidates.iter().filter_map(|t| self.offer(*t)).collect()
    }

    /// One arm a candidate: what it wins against what it spends, and nothing
    /// where it cannot fight from here.
    pub(super) fn offer(&self, tactic: Tactic) -> Option<Offer> {
        let (value, ticks) = match tactic {
            // A post spends no ticks of the room: the raid comes to it.
            Tactic::Hold if self.holds => (self.raid, 0),
            Tactic::Close if self.won && !self.ranged => (self.raid, self.ticks),
            Tactic::Kite if !self.won && !self.ranged => (0, self.ticks),
            Tactic::BlobClose if self.won && self.ranged => (self.raid, self.ticks),
            Tactic::BlobKite if !self.won && self.ranged => (0, self.ticks),
            _ => return None,
        };

        let cost = casting::spent(&self.set, self.losses, ticks, self.tick_worth);

        Some(offer(tactic, &self.set, value, cost))
    }
}

/// The bodies a candidate fields are the tie-break's input (§6.3).
pub(super) fn offer(tactic: Tactic, set: &[casting::Body], value: i32, cost: i32) -> Offer {
    Offer {
        tactic,
        bodies: set.len() as i32,
        value,
        cost,
    }
}

/// What a `Sweep`'s set wins — the harvest it denies and the prey it kills —
/// against what it casts (§6.3).
pub(super) fn sweep_offers(
    candidates: &[Tactic],
    together: bool,
    worth: i32,
    set: &[casting::Body],
) -> Vec<Offer> {
    let cost = casting::spent(set, Losses { dead: 0, hits: 0 }, 0, 0);

    candidates
        .iter()
        .filter_map(|&tactic| match (tactic, together) {
            (Tactic::BlobKite, true) | (Tactic::Kite, false) => {
                Some(offer(tactic, set, worth, cost))
            }
            _ => None,
        })
        .collect()
}
