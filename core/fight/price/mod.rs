use std::cmp::Reverse;
use std::collections::{BTreeMap, BTreeSet};

mod account;
pub mod casting;
use crate::kinds::{defend, raid, siege, sweep};
use crate::scene::{Asked, Hunting, Scene, fresh, middle, picket};
use crate::shape::{Engagement, Group, Objective, Phase, Plan};
pub use account::{Actual, Closed, account, closed};
use fabot_atlas::{self as atlas, Atlas, Mover, Target};
use fabot_base::declarations::{self, Declarations};
use fabot_base::grid::{self, Footprint};
use fabot_base::tiles::Tile;
use fabot_base::vocabulary::Part::Attack;
use fabot_base::world::{self, Creep, Id, Pos, Room, RoomName, Structure, StructureType};
use fabot_base::{engine, tiles};
use fabot_exchange::{
    self as exchange, Fighter, Ground, Losses, Outcome, Side, Stance, Terrain, fighter,
};
use fabot_tactics::{self as tactics, Offer, Tactic};

type CopiesOf<'a> = Box<dyn Fn(&casting::Body) -> i32 + 'a>;

impl<'a> Scene<'a> {
    pub(super) fn breach_in(
        &self,
        atlas: &Atlas,
        room: &Room,
        target: &Structure,
        from: &Pos,
        set: &[Fighter],
        standing: &[&Creep],
    ) -> Option<(Vec<Id>, Outcome)> {
        let barriers: Vec<&Structure> = siege::barrier_at(room).values().copied().collect();
        let line: Vec<Pos> = barriers.iter().map(|b| b.pos).collect();

        let mut weights = atlas.held_weights(room.name)?;
        let gait = gait(set)?;

        for b in &barriers {
            if !room.blocked.contains(&b.pos)
                && (room.structures.iter()).all(|s| s.pos != b.pos || s.walkable || Scene::rival(s))
            {
                weights[tiles::of_pos(&b.pos)] =
                    grid::weight(casting::terrain_at(Some(room), &b.pos).weight);
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

        let sources = entries(atlas, &ground, gait, from, set, standing);

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

        // Rampart first: the tile stands in the plan until it is walkable.
        let ids_at = |tile: Tile| -> Vec<Id> {
            let mut at: Vec<&Structure> = barriers
                .iter()
                .copied()
                .filter(|b| tiles::of_pos(&b.pos) == tile)
                .collect();

            at.sort_by_key(|b| (b.kind != StructureType::Rampart, b.id));
            at.into_iter().map(|b| b.id).collect()
        };

        // Seats follow the planned barrier, so the plan is priced twice.
        let first = attempt(set.len() as i32)?;
        let seats = seats_of(&first);
        let plan = attempt(seats).filter(|plan| seats_of(plan) >= seats)?;

        Some((
            plan.0.iter().flat_map(|(tile, _)| ids_at(*tile)).collect(),
            Outcome::Win(plan.1, Losses::default()),
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
        let colony = self.colonies_of(atlas, e).iter().map(|r| r.name).min()?;
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
                self.breach_in(atlas, room, goal, &from, &cast_all(set), &[])
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
                    let boosts = self.ask(atlas, e, casting::bare(&set), asked);

                    Some(Engagement {
                        plan: Plan {
                            boosts,
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
            if !siege::gave_up(e) {
                let pairs = casting::pairs(e.capacity) as usize;
                let missing: Vec<casting::Body> = (casting::pair().iter())
                    .flat_map(|body| {
                        let have = self.paired(e, &asked.casts, std::slice::from_ref(body));
                        std::iter::repeat_n(body.clone(), pairs.saturating_sub(have))
                    })
                    .collect();
                self.ask(atlas, e, casting::bare(&missing), asked);
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

            let late = matches!(e.predicted, Some(Outcome::Win(ticks, _))
                if e.phase == Phase::Fighting && self.world.time > e.since + ticks);

            let (guards, reach) = match set.iter().all(|f| f.work == 0) {
                true => (raid::guards(room), engine::RANGED_RANGE),
                false => (Vec::new(), engine::MELEE_RANGE),
            };
            // Where the price stood the set: the struck's own tile holds no body.
            let struck = |b: &Structure| {
                (tiles::within(reach, tiles::of_pos(&b.pos)).into_iter())
                    .filter(|t| atlas.passable(room.name, *t))
                    .map(|t| exchange::struck(&towers, &guards, &tiles::to_pos(room.name, t)))
                    .min()
                    .unwrap_or(0)
            };

            let event = left
                || self.landed.iter().any(|c| c.engagement == e.id)
                || late
                || self.next_struck(e).is_some_and(|b| {
                    let priced = raid::hardest(room).filter(|_| reach == engine::RANGED_RANGE);
                    struck(priced.unwrap_or(b)) > exchange::pooled_heal(set.len() as i32, &set)
                        || self.unreached(atlas, room, b, &in_room)
                });

            if !event {
                return Some(e.clone());
            }

            let planned = self
                .breach_in(atlas, room, goal, &from, &set, &breaking)
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
                        self.breach_in(atlas, room, goal, &from, &cast_all(set), &breaking)
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
                self.ask(atlas, e, casting::bare(&added), asked);
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
                self.ask(atlas, e, casting::bare(std::slice::from_ref(body)), asked);
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
        let set: Vec<bool> = (e.ours.keys().filter_map(|n| self.ours.get(n.as_str())))
            .map(|c| c.swings())
            .chain(
                (self.spawning.iter())
                    .filter(|c| c.engagement == e.id)
                    .map(|c| c.body.contains(&Attack)),
            )
            .collect();
        let ranged = !set.is_empty() && !set.contains(&true);
        free.iter()
            .filter(|n| {
                self.ours.get(n.as_str()).is_some_and(|c| {
                    !(ranged && c.swings()) && self.walk_to(atlas, e, c.pos.room).is_some()
                })
            })
            .cloned()
            .collect()
    }

    /// The harasser `ours` need to hold the room (none if they hold it): the
    /// cheapest that out-reaches its escort and is paid by what holding it
    /// denies; None where one of `ours` cannot (owner: a sweep denies his
    /// economy and never fights his guards, so more guards price a room at nothing).
    pub(super) fn hunt(
        &self,
        ground: &Hunting,
        room: &Room,
        ours: &[Fighter],
        capacity: i32,
    ) -> Option<Vec<casting::Body>> {
        let escort: Vec<Fighter> = ground.escort.iter().map(casting::fighter_of).collect();
        // His safe mode covers this room alone, never his others.
        let barred = !raid::charged(room).is_empty() || Scene::safe(room);
        let holds = |f: &Fighter| !barred && escort.iter().all(|g| sweep::outreaches(f, g));
        if !ours.is_empty() {
            return ours.iter().all(holds).then(Vec::new);
        }
        let cost = |b: &casting::Body| engine::body_cost(b);
        (sweep::HARASSERS.iter())
            .map(|b| engine::with_move_last(b.to_vec()))
            .filter(|b| cost(b) <= capacity.min(ground.worth) && holds(&fighter::cast(b)))
            .min_by_key(cost)
            .map(|b| vec![b])
    }

    /// Each room an enemy works a harasser holds, its worth and the nearest
    /// colony's walk, priced only on a tick a group there was new, changed or
    /// came back into sight (`stirred`). An avoided room is none, as it is no patrol's.
    pub(super) fn candidates(
        &self,
        atlas: &Atlas,
        grounds: &BTreeMap<RoomName, Hunting>,
        stirred: &BTreeSet<RoomName>,
    ) -> Vec<(RoomName, i32, i32)> {
        let mut found: Vec<_> = grounds
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
                let walk = g.walk?;
                let capacity = self.capacity_of(atlas, &e);
                self.hunt(g, room, &[], capacity)?;
                Some((*name, g.worth, walk))
            })
            .collect();
        found.sort_by_key(|&(room, worth, walk)| (Reverse(atlas::rate(worth, walk)), walk, room));
        found
    }

    pub(super) fn armed_seen(&self, g: &Group) -> bool {
        let seen = self.sightings.get(&g.room());
        (g.members.iter()).any(|m| seen.and_then(|s| s.get(m)).is_some_and(|s| s.armed))
    }

    pub(super) fn walk_of(&self, atlas: &Atlas, e: &Engagement, room: RoomName) -> Option<i32> {
        let from = self.members_of(e).first()?.pos.room;
        let onto = fresh(0, Objective::Sweep, room, 0, Tactic::Kite);
        self.walk_to(atlas, &onto, from)
    }

    pub(super) fn elsewhere(
        &self,
        atlas: &Atlas,
        e: &Engagement,
        (candidates, claimed): (&[(RoomName, i32, i32)], &BTreeSet<RoomName>),
        grounds: &BTreeMap<RoomName, Hunting>,
    ) -> Option<RoomName> {
        let ours = self.members_of(e);
        let held = (candidates.iter())
            .filter(|c| !claimed.contains(&c.0))
            .filter_map(|&(room, worth, walk)| {
                if ours.is_empty() {
                    return Some((room, worth, walk));
                }
                let walk = self.walk_of(atlas, e, room)?;
                sweep::holds(self, e, (room, grounds)).then_some((room, worth, walk))
            });
        atlas::onward(held)
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
        let mut fielding: Vec<Fighter> = (e.ours.keys())
            .chain(&joining)
            .filter_map(|n| self.ours.get(n.as_str()))
            .map(|c| self.fighter(c))
            .collect();
        let casts = (asked.casts.iter()).filter(|c| c.engagement == e.id);
        fielding.extend(casts.map(|c| fighter::cast(&c.body)));

        let capacity = self.capacity_of(atlas, e);
        let Some(set) = self.hunt(ground, room, &fielding, capacity) else {
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

        // A set walks in as one; in its room each body's step is its own.
        let together = e.ours.len() + joining.len() + set.len() > 1;
        let (candidates, pin) = self.ruled(e, sweep::CANDIDATES);
        let offers = sweep_offers(&candidates, together, ground.worth, &set);

        let Some(tactic) = tactics::select(&offers, pin).map(|o| o.tactic) else {
            let onward = onward().filter(|_| !e.ours.is_empty());
            return onward.map(|r| sweep::onto(r, self.world.time, e));
        };

        free.retain(|n| !joining.contains(n));
        let boosts = self.ask(atlas, e, casting::bare(&set), asked);

        let mut next = e.clone();
        next.join(joining, &asked.casts);
        next.plan = Plan {
            boosts,
            tactic,
            order: Vec::new(),
            offers,
        };
        next.unanswered = false;
        next.capacity = capacity;
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
        if self.spent(e) || e.phase == Phase::Disengaging {
            return e.clone();
        }
        let g = self.by_id.get(&e.group);
        let at = g.map_or(e.room, Group::room);
        let room = self.in_sight(at);
        let home = room.is_some_and(|r| self.homes.contains(&r.name));
        // A home's towers leave a body they cannot out-heal, armed or not, to its Defend.
        let raid: Vec<Creep> = match (g, room) {
            (Some(g), Some(room)) => (self.foes[&room.name].iter())
                .filter(|h| self.armed.contains(&h.id) || home && g.members.contains(&h.id))
                .cloned()
                .collect(),
            _ => self.remembered.get(&at).cloned().unwrap_or_default(),
        };
        // Its kill order is the raid's owner's, a third party's bodies struck after.
        let member = |h: &Creep| g.is_some_and(|g| g.members.contains(&h.id));
        let kin: Vec<Creep> = (raid.iter())
            .filter(|h| {
                !raid.iter().any(member) || raid.iter().any(|m| member(m) && m.owner == h.owner)
            })
            .cloned()
            .collect();
        let seen = g.filter(|_| room.is_some()).map(|g| g.centre);
        let Some(centre) = seen.or_else(|| self.raid_at(e, at)) else {
            return e.clone();
        };

        let safe_left =
            (room.and_then(|r| r.controller.as_ref())).map_or(0, world::Controller::our_safe_mode);
        let theirs = Side {
            bodies: (raid.iter())
                .filter(|h| !self.cores.contains(&h.id) && world::hostiles::outlives(h, safe_left))
                .map(casting::fighter_of)
                .collect(),
            ..Side::default()
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
        let colony = self.nearest(atlas, e).map(|(walk, _)| walk);
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

        let fielding: Vec<&Creep> = (e.ours.keys().chain(&joining))
            .filter_map(|n| self.ours.get(n.as_str()).copied())
            .filter(lasts)
            .collect();

        let bodies: Vec<Fighter> = fielding.iter().map(|c| self.fighter(c)).collect();

        let core_hits: i32 = raid
            .iter()
            .filter(|h| self.cores.contains(&h.id))
            .map(|h| h.hits)
            .sum();

        let colonies = self.colonies_of(atlas, e);
        let capacity = self.capacity_of(atlas, e);
        let terrain = casting::terrain_at(room, &centre);

        let posted = home
            && !defend::off_post(self, e)
            && room.is_some_and(|r| r.name == e.room)
            && (room.iter().flat_map(|r| &r.structures))
                .any(|s| s.kind == StructureType::Rampart && s.owner.is_ours());

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

                    let spawns = self.spawns_in(&colonies);

                    let razing = casting::razer(capacity, core_hits, &[]);

                    let fielded_cost: i32 =
                        (fielding.iter().map(|c| fabot_base::world::body_kinds(c)))
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
                _ => (Box::new(|_: &casting::Body| casting::HOME_COPIES), 0),
            };

        let mut ours_now: Vec<Fighter> = bodies.clone();
        ours_now.extend(asked_fighters.iter().cloned());

        let blocks = if home {
            casting::affordable(capacity)
        } else if self.defended.contains(&at) {
            casting::defending(capacity)
        } else {
            casting::walking(capacity)
        };

        let arms: Vec<Priced> = arms(posted, &theirs)
            .filter_map(|(stance, together)| {
                let side = Side {
                    together,
                    cast_walk: colony,
                    ..casting::side_in(&self.world.rules, room, ours_now.clone())
                };
                let casting::Pricing {
                    mut set,
                    outcome,
                    landed,
                    boosted,
                } = casting::price(
                    stance,
                    &side,
                    &theirs,
                    terrain,
                    &blocks,
                    &copies_of,
                    tick_worth,
                    &self.boosts_for(atlas, e, &asked.boosts),
                )?;
                let cast = set.len();
                let priced = [&ours_now[..], &cast_all(&set)].concat();
                set.extend(casting::razer(capacity, core_hits, &priced));

                let price = Priced {
                    set,
                    boosted,
                    cast,
                    tick_worth,
                    ..Priced::arm(stance, (&landed, &theirs), terrain, outcome)
                };
                Some(price)
            })
            .collect();

        let (candidates, pin) = self.ruled(e, defend::CANDIDATES);
        let (offers, chosen) = picked(&arms, &candidates, pin);

        let starved =
            arms.is_empty() && (e.unanswered || e.ours.is_empty() && asked_fighters.is_empty());
        match chosen {
            Some((tactic, price)) => {
                // The set asked here holds the rest: priced again without a
                // body that would not outlast it. Each pass drops one.
                let after = wait.max(waiting(
                    &[&asked_bodies[..], &price.set[..price.cast]].concat(),
                ));
                let held = (Some(after), price.outcome.ticks());
                if after > 0 && fielding.iter().any(|c| !outlives(c, after, held.1)) {
                    return self.priced_after(atlas, asked, e, free, declarations, held);
                }

                free.retain(|n| !joining.contains(n));
                let boosts = self.ask(
                    atlas,
                    e,
                    casting::Asking {
                        bodies: &price.set,
                        boosted: price.boosted,
                    },
                    asked,
                );

                let mut next = e.clone();
                next.join(joining.iter().cloned(), &asked.casts);
                next.plan = Plan {
                    boosts,
                    tactic,
                    order: casting::kill_order(&kin),
                    offers,
                };
                if e.unanswered || (e.phase == Phase::Forming && e.since == self.world.time) {
                    next.budget = e.lost + price.outcome.losses().dead;
                }
                next.unanswered = false;
                next.capacity = capacity;
                // §10.1 scores the first answer's; a core alone is no fight Exchange predicts.
                next.predicted = if e.predicted.is_some() {
                    e.predicted
                } else if theirs.bodies.is_empty() {
                    None
                } else {
                    Some(price.outcome)
                };

                let standing: Vec<Fighter> = (next.ours.keys())
                    .filter_map(|n| self.ours.get(n.as_str()))
                    .filter(|c| lasts(c))
                    .map(|c| casting::fighter_of(c))
                    .collect();

                if casting::wins(
                    price.stance,
                    &casting::side_in(&self.world.rules, room, standing),
                    &theirs,
                    terrain,
                ) {
                    defend::stands_alone(self.world.time, &next)
                } else {
                    next
                }
            }
            None if starved => Engagement {
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
                max_hops: declarations::SIEGE_HOPS,
            };
            let walk = atlas.ticks_to(mover, &onto, from)?;
            Some((stretch.iter()).map(move |t| (*t, walk, stretch.len() as i32)))
        })
        .flatten()
        .collect()
}

/// Each arm a Defend prices, a stance and whether its bodies walk in together: a home is a
/// post the raid comes to where our ramparts stand on it (`posted`), priced beside the standoff
/// a set fights off it and walking no blob in; with none, or no striker to come, a Hold stands
/// still while the raid stands off.
pub fn arms(posted: bool, theirs: &Side) -> impl Iterator<Item = (Stance, bool)> {
    let held = posted && theirs.bodies.iter().any(fighter::strikes);
    let stances = [Stance::Hold, Stance::Standoff]
        .into_iter()
        .filter(move |s| held || *s == Stance::Standoff);
    stances
        .flat_map(|s| [(s, false), (s, true)])
        .filter(|(s, together)| *s == Stance::Standoff || !together)
}

/// Every arm's offers, and the selected one's tactic and arm: two stances may offer one tactic.
pub fn picked<'a>(
    arms: &'a [Priced],
    candidates: &[Tactic],
    pin: Option<Tactic>,
) -> (Vec<Offer>, Option<(Tactic, &'a Priced)>) {
    let offers: Vec<Offer> = arms.iter().flat_map(|p| p.offers(candidates)).collect();
    let pick = |o: &Offer| {
        Some((
            o.tactic,
            arms.iter().find(|p| p.offer(o.tactic) == Some(*o))?,
        ))
    };
    let chosen = tactics::select(&offers, pin).and_then(pick);
    (offers, chosen)
}

pub struct Priced {
    pub rival_spent: i32,
    pub together: bool,
    /// The set swings nothing, so it fights as one body.
    pub ranged: bool,
    pub stance: Stance,
    pub holds: bool,
    pub outcome: Outcome,
    pub set: Vec<casting::Body>,
    pub boosted: bool,
    /// The first `cast` of `set` the outcome prices; the rest raze.
    pub cast: usize,
    pub tick_worth: i32,
}

impl Priced {
    pub fn arm(
        stance: Stance,
        (landed, theirs): (&Side, &Side),
        terrain: Terrain,
        outcome: Outcome,
    ) -> Priced {
        Priced {
            rival_spent: theirs
                .bodies
                .iter()
                .map(|f| engine::body_cost(&f.body))
                .sum(),
            together: landed.together,
            ranged: landed.bodies.len() > 1 && landed.bodies.iter().all(|f| f.attack == 0),
            stance,
            holds: exchange::stance_against(stance, landed, theirs, terrain) == Stance::Hold,
            outcome,
            set: Vec::new(),
            boosted: false,
            cast: 0,
            tick_worth: 0,
        }
    }

    pub fn offers(&self, candidates: &[Tactic]) -> Vec<Offer> {
        candidates.iter().filter_map(|t| self.offer(*t)).collect()
    }

    pub fn offer(&self, tactic: Tactic) -> Option<Offer> {
        let (won, ticks) = (self.outcome.is_win(), self.outcome.ticks());
        let (value, ticks) = match (tactic, self.together) {
            // A post spends no ticks of the room: the raid comes to it.
            (Tactic::Hold, false) if self.holds => (if won { self.rival_spent } else { 0 }, 0),
            _ if self.stance == Stance::Hold => return None,
            (Tactic::Close, false) if won && !self.ranged => (self.rival_spent, ticks),
            (Tactic::Kite, false) if !won && !self.ranged => (0, ticks),
            (Tactic::BlobClose, true) if won => (self.rival_spent, ticks),
            (Tactic::BlobKite, true) if !won && self.ranged => (0, ticks),
            _ => return None,
        };

        let unpriced = &self.set[self.cast..];
        let cost = casting::spent(unpriced, self.outcome.losses(), ticks, self.tick_worth);

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

pub(super) fn sweep_offers(
    candidates: &[Tactic],
    together: bool,
    worth: i32,
    set: &[casting::Body],
) -> Vec<Offer> {
    let cost = casting::spent(set, Losses::default(), 0, 0);

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
