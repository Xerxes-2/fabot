/// The fight's tick (`ARCHITECTURE.md` §6): who is out there, what we committed,
/// what each committed body does, and what it asks for.
use std::collections::{BTreeMap, BTreeSet};

use super::kinds::defend::{self, Facts as DefendFacts, Home};
use super::kinds::raid;
use super::kinds::siege::{self, Facts as SiegeFacts};
use super::kinds::sweep;
use super::shape::{Cast, Engagement, Group, Objective, Phase, Plan};
use super::{casting, tracker};
use crate::core::atlas::{self, Atlas, Mover, Target, Tree};
use crate::core::exchange::{self, Fighter, Ground, Losses, Outcome, Side, Stance, fighter};
use crate::core::grid::{self, Footprint};
use crate::core::intents::{self, Ask, ControllerAct, CreepAct, Intent, Rank};
use crate::core::mv::Wish;
use crate::core::state::State;
use crate::core::tactics::{self, Offer, Tactic};
use crate::core::tick::{self, Context, Declarations, Module, TacticRule};
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
    /// The energy each room's queued bodies will draw of its bank.
    asked: BTreeMap<String, i32>,
    casts: Vec<Cast>,
}

impl Asked {
    fn queue(&mut self, room: &str, body: &[BodyPart]) {
        let ticks = engine::SPAWN_TICKS_PER_PART * body.len() as i32;
        *self.queued.entry(room.to_string()).or_insert(0) += ticks;
        *self.asked.entry(room.to_string()).or_insert(0) += engine::body_cost(body);
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
    /// The operator's tactic rules (§6.3), filtered per engagement.
    rules: &'a [TacticRule],
    walks: BTreeMap<(String, i32), Tree>,
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
    /// The compound the declared lab cluster covers, and the parts it buys.
    boost: Option<(&'static str, i32)>,
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

    /// The usernames whose room this is and who stands in it: what a rule's
    /// `rival` names (§6.3).
    fn rivals_of(&self, room: &str) -> BTreeSet<&'a str> {
        let Some(seen) = self.in_sight(room) else {
            return BTreeSet::new();
        };

        let owners = seen
            .structures
            .iter()
            .map(|s| &s.owner)
            .chain(seen.controller.as_ref().map(|c| &c.owner))
            .chain(seen.hostiles().map(|h| &h.owner));

        owners.filter_map(Owner::user).collect()
    }

    /// A kind's candidates as the operator's rules leave them, and the pin it
    /// names (§6.3).
    fn ruled(&self, e: &Engagement, candidates: &[Tactic]) -> (Vec<Tactic>, Option<Tactic>) {
        let ruling = tick::ruling(self.rules, &e.room, &self.rivals_of(&e.room));

        let left = candidates
            .iter()
            .copied()
            .filter(|t| !ruling.forbid.contains(t))
            .collect();

        (left, ruling.pin)
    }

    fn next_struck(&self, e: &Engagement) -> Option<&'a Structure> {
        let room = self.in_sight(&e.room)?;

        match (e.plan.order.first(), &e.objective) {
            (Some(first), _) => siege::barrier_at(room).get(first.as_str()).copied(),
            (None, Objective::Siege(target)) => room.structures.iter().find(|s| s.id == *target),
            (None, Objective::Defend | Objective::Sweep) => None,
        }
    }

    /// `was`: last tick's groups (§6.1), the only record of where a body stood.
    fn ground(&self, atlas: &Atlas, room: &Room, was: &[Group]) -> Hunting {
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
            .filter(|c| siege::beside(&reached, &c.pos))
            .collect();
        let exposed = siege::exposed_in(&reached, room);
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
        let weights = atlas.weights(&room.name);
        let shields: BTreeSet<Tile> = (room.structures.iter())
            .filter(|s| s.kind.is_rampart() && !s.owner.is_ours())
            .map(|s| tiles::of_pos(&s.pos))
            .collect();
        let solid: BTreeSet<Tile> = (room.structures.iter())
            .filter(|s| !s.walkable && !s.kind.is_rampart())
            .map(|s| tiles::of_pos(&s.pos))
            .collect();
        // A shot on a body under its owner's rampart lands on the rampart.
        let shot = |t: Tile| -> Option<Pos> {
            (!shields.contains(&t)).then(|| tiles::to_pos(&room.name, t))
        };
        let before = |id: &String| {
            (was.iter())
                .find(|g| {
                    g.last_seen == self.world.time - 1 && g.members == BTreeSet::from([id.clone()])
                })
                .map(|g| &g.centre)
        };
        // A body working its tile stays on it, and a fleeing one keeps its
        // heading: weighing every tile it could step to drew a hunter onto
        // the working body beside it rather than the one walking off.
        let flight = |c: &Creep| -> Vec<Option<Pos>> {
            let here = tiles::of_pos(&c.pos);
            let open = |t: &Tile| {
                let stands = weights.as_ref().is_some_and(|w| w[*t].is_some());
                stands || shields.contains(t) && !solid.contains(t)
            };
            let around = tiles::around(here).iter().copied().filter(|t| open(t));
            // The engine carries a body that ends its step on the edge across.
            let step = |t: Tile| shot(t).filter(|_| !tiles::on_edge(t));
            let left = before(&c.id)
                .filter(|p| p.room == c.pos.room)
                .map(tiles::of_pos);
            let onward: Vec<Tile> = (around.clone())
                .filter(|t| left.is_some_and(|l| tiles::range(*t, l) == 2))
                .collect();
            match left {
                _ if c.fatigue > 0 => vec![shot(here)],
                Some(l) if l == here => vec![shot(here)],
                Some(_) if !onward.is_empty() => onward.into_iter().map(step).collect(),
                _ => std::iter::once(shot(here))
                    .chain(around.map(step))
                    .collect(),
            }
        };
        // A body on the gate, standing there or back on it, is carried across
        // as we come: v1's `catchOdds`, a strike before it leaves, no walk.
        let dances = |c: &Creep| {
            tiles::on_edge(tiles::of_pos(&c.pos))
                && before(&c.id)
                    .is_none_or(|p| p.room == c.pos.room && tiles::on_edge(tiles::of_pos(p)))
        };
        let mut marks: Vec<tactics::Mark> = (prey.iter().copied())
            .map(|c| (c, true))
            .chain(escort.iter().map(|c| (c, false)))
            .map(|(c, unarmed)| tactics::Mark {
                id: c.id.clone(),
                pos: c.pos.clone(),
                worth: sweep::worth(c),
                hits: c.hits,
                massed: true,
                shielded: shields.contains(&tiles::of_pos(&c.pos)),
                fleeting: unarmed && dances(c),
                next: flight(c),
            })
            .collect();
        marks.extend(
            room.structures
                .iter()
                .filter(|s| s.kind.is_container() || s.kind.is_link() && !s.owner.is_ours())
                .filter(|s| siege::beside(&reached, &s.pos))
                .map(|s| tactics::Mark {
                    id: s.id.clone(),
                    pos: s.pos.clone(),
                    worth: s.hits,
                    hits: s.hits,
                    massed: !s.kind.is_container(),
                    shielded: shields.contains(&tiles::of_pos(&s.pos)),
                    fleeting: false,
                    next: vec![shot(tiles::of_pos(&s.pos))],
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

    fn hops(e: &Engagement) -> i32 {
        match e.objective {
            Objective::Siege(_) | Objective::Sweep => tick::SIEGE_HOPS,
            Objective::Defend => tick::REACH_HOPS,
        }
    }

    /// The room a set walks in from: the one before his on its colony's route.
    fn enter_room(&self, atlas: &Atlas, e: &Engagement) -> Option<String> {
        let colony = self.colonies_of(atlas, e).first()?.name.clone();
        let hops = Scene::hops(e);
        let rooms = match self.walks.get(&(e.room.clone(), hops)) {
            Some(tree) => tree.route(&colony, &e.room),
            None => atlas.route(self.avoid, hops, &colony, &e.room),
        }?;

        rooms.get(rooms.len().checked_sub(2)?)?.clone().into()
    }

    fn walk_to(&self, atlas: &Atlas, e: &Engagement, from: &str) -> Option<i32> {
        let hops = Scene::hops(e);
        let rooms = match self.walks.get(&(e.room.clone(), hops)) {
            Some(tree) => tree.route(from, &e.room),
            None => atlas.route(self.avoid, hops, from, &e.room),
        };
        rooms.map(|rooms| engine::ROOM_SIDE * (rooms.len() as i32 - 1))
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

    /// How many of the drain's bodies the set already holds, standing or in
    /// an oven: a pair's shape is no other body's.
    fn paired(&self, e: &Engagement, asked: &Asked, set: &[casting::Body]) -> usize {
        let of = |body: &[BodyPart]| set.iter().any(|s| s.as_slice() == body);

        (self.members_of(e))
            .iter()
            .filter(|c| of(&c.body.iter().map(|p| p.kind).collect::<Vec<_>>()))
            .count()
            + (asked.casts)
                .iter()
                .filter(|c| c.engagement == e.id && of(&c.body))
                .count()
    }

    fn ask_slots(
        &self,
        atlas: &Atlas,
        e: &Engagement,
        set: &[(String, casting::Body)],
        asked: &mut Asked,
    ) {
        let colonies = self.colonies_of(atlas, e);
        let boost = self.boost;

        for (slot, body) in set {
            let colony = casting::colony_for(
                &colonies,
                &self.world.spawns,
                &asked.queued,
                &asked.asked,
                |from| self.walk_to(atlas, e, from),
                body,
            );

            if let Some(colony) = colony {
                asked.queue(&colony, body);
                let compound = fighter::boosted_by(body, boost);

                asked.casts.push(Cast {
                    engagement: e.id,
                    name: match compound {
                        Some(compound) => format!(
                            "{}{}-{}-{}-{}",
                            tick::FIGHT_PREFIX,
                            self.world.time,
                            e.id,
                            slot,
                            compound
                        ),
                        None => format!(
                            "{}{}-{}-{}",
                            tick::FIGHT_PREFIX,
                            self.world.time,
                            e.id,
                            slot
                        ),
                    },
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
        declared: bool,
        standing: &BTreeSet<String>,
        aging_out: &BTreeSet<String>,
        gate: Option<&tactics::Gate>,
    ) -> SiegeFacts {
        let room = self.in_sight(&e.room);
        let towers = room.map_or_else(Vec::new, |r| self.charged(r));

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
                    .is_some_and(|c| c.pos.room == e.room && !picket(e, c))
            }),
            charged: !towers.is_empty(),
            drain: gate.is_some(),
            priced: siege::drainable(&e.room, e.capacity, &towers),
            worked: self.pair_worked(e),
            bounced: gate.is_some_and(|g| self.bounced(e, g)),
            drawn: self.drawn(e),
            gave_up: siege::gave_up(e),
            razed: room.is_some_and(|room| match &e.objective {
                Objective::Siege(target) => {
                    !siege::stands(room, target)
                        && (!raid::raids(e) || raid::mark(room, &self.members_of(e)).is_none())
                }
                _ => false,
            }),
            safe: room.is_some_and(Scene::safe),
            withdrawn: !declared,
        }
    }

    /// A bouncer carried back from his room this tick: it stands on the tile
    /// across from his, where the volley that answered the trip is in the log.
    fn bounced(&self, e: &Engagement, gate: &tactics::Gate) -> bool {
        let back = tiles::to_pos(&gate.from, gate.bounce);

        (self.members_of(e))
            .iter()
            .any(|c| c.pos == back && !c.body.iter().any(|p| p.kind == BodyPart::Heal))
    }

    /// One of his towers struck one of ours in his room on the trip just
    /// ended: what his room's event log is read for, and the only answer a
    /// drain counts.
    fn drawn(&self, e: &Engagement) -> bool {
        if self.world.attacks.is_empty() {
            return false;
        }

        let towers: BTreeSet<&String> = (self.in_sight(&e.room))
            .into_iter()
            .flat_map(|r| self.towers_in(r))
            .map(|t| &t.id)
            .collect();

        (self.world.attacks.iter())
            .any(|(by, at)| towers.contains(by) && (self.world.creeps).iter().any(|c| &c.id == at))
    }

    /// The heal the set seats on one body beside it: what his towers' strike
    /// is weighed against at the breach.
    fn seated(&self, e: &Engagement) -> i32 {
        let set = self.fielded(e);

        exchange::pooled_heal(set.len() as i32, &set)
    }

    /// His towers out-strike the heal the set seats at the breach, so the gate
    /// is worth working.
    fn outstruck(&self, e: &Engagement, towers: &[Pos]) -> bool {
        (self.next_struck(e))
            .is_some_and(|b| exchange::tower_strike(towers, &b.pos) > self.seated(e))
    }

    /// A pair of the drain stands or is in an oven, or one has tripped: a drain
    /// with none of those holds nothing.
    fn pair_worked(&self, e: &Engagement) -> bool {
        let set = casting::drains(e.capacity);
        let of = |body: &[BodyPart]| set.iter().any(|s| s.as_slice() == body);

        e.ignored > 0
            || (self.members_of(e)).iter().any(|c| {
                let body: Vec<BodyPart> = c.body.iter().map(|p| p.kind).collect();
                of(&body)
            })
            || (self.waiting)
                .iter()
                .any(|c| c.engagement == e.id && of(&c.body))
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
            .filter(|b| raid::raids(e) || !b.iter().any(|p| p.is_ranged_attack()))
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
                        &m.pos,
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
        standing: &[&Creep],
    ) -> Option<(Vec<String>, Outcome)> {
        let barriers: Vec<&Structure> = siege::barrier_at(room).values().copied().collect();
        let line: Vec<Pos> = barriers.iter().map(|b| b.pos.clone()).collect();

        let gaits: Vec<grid::FatigueFactor> =
            set.iter().map(|f| f.gait).filter(|g| g.moves > 0).collect();

        let gait = gaits
            .iter()
            .copied()
            .min_by_key(|g| std::cmp::Reverse(grid::step_ticks(*g, engine::PLAIN_WEIGHT)));

        let mut weights = atlas.weights(&room.name)?;
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

        let mover = Mover {
            footprint: Footprint::One,
            factor: gait,
        };
        let open = |t: &Tile| ground.weights[*t].is_some();
        // The atlas walks no body out of the room it stands in: a set inside
        // is planned from where it stands.
        let sources: Vec<(Tile, i32, i32)> = if standing.is_empty() {
            let from = (self.world.spawns.iter())
                .find(|s| s.pos.room == colony)
                .map_or_else(|| middle(colony), |s| s.pos.clone());
            [tiles::TOP, tiles::RIGHT, tiles::BOTTOM, tiles::LEFT]
                .into_iter()
                .flat_map(|exit| tiles::exit_tiles(exit).split(|t| !open(t)))
                .filter(|stretch| !stretch.is_empty())
                .filter_map(|stretch| {
                    let onto = Target {
                        room: room.name.clone(),
                        tiles: stretch.to_vec(),
                        range: engine::MELEE_RANGE,
                        avoid: BTreeSet::new(),
                        max_hops: tick::SIEGE_HOPS,
                    };
                    let walk = atlas.ticks_to(mover, &onto, &from)?;
                    Some(
                        stretch
                            .iter()
                            .map(move |t| (*t, walk, stretch.len() as i32)),
                    )
                })
                .flatten()
                .collect()
        } else {
            let n = set.len() as i32;
            standing
                .iter()
                .map(|c| (tiles::of_pos(&c.pos), 0, n))
                .collect()
        };

        let attempt = |seated: i32| {
            exchange::breach_plan(
                &ground,
                gait,
                &towers,
                &[],
                set,
                seated,
                |t| tactics::seats_at(atlas, &line, &room.name, t),
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
        let from = (self.world.spawns.iter())
            .find(|s| s.pos.room == colony)
            .map_or_else(|| middle(&colony), |s| s.pos.clone());

        let Some(room) = self.in_sight(&e.room) else {
            return Some(e.clone());
        };

        if let Some(c) = siege::tapped(room, target) {
            return Some(self.tapped(atlas, asked, e, c));
        }

        let Some(goal) = room.structures.iter().find(|s| s.id == target) else {
            return Some(e.clone());
        };

        if e.unanswered && !grew {
            Some(e.clone())
        } else if e.unanswered {
            let capacity = self.capacity_of(atlas, e);

            let priced = |set: &[casting::Body]| {
                let fighters: Vec<Fighter> = set.iter().map(|b| fighter::cast(b)).collect();

                self.breach_in(atlas, room, goal, &colony, &fighters, &[])
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
                .all(|t| raid::breached(room, |tile| atlas.passable(&room.name, tile), &t.pos));
            // A raid razes; the room his safe mode holds is drained instead,
            // as nothing a set shoots at there can fall (§6.2).
            let raid = (open && breached && !Scene::safe(room))
                .then(|| {
                    let hard =
                        raid::hardest(room).map_or_else(|| goal.pos.clone(), |s| s.pos.clone());
                    let priced = |set: &[casting::Body]| {
                        let ours: Vec<Fighter> = set.iter().map(|b| fighter::cast(b)).collect();

                        raid::plan(atlas, room, &hard, &from, &ours, &[])
                    };

                    raid::reinforce(capacity, &[], priced)
                })
                .flatten();
            let planned = raid.clone().or(siege).and_then(|(set, (order, outcome))| {
                let charged = self.charged(room);
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
                        let have = self.paired(e, asked, std::slice::from_ref(body));
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
                .breach_in(atlas, room, goal, &colony, &set, &breaking)
                .map(|plan| (Vec::new(), plan))
                .or_else(|| {
                    let fielded = self.fielded_bodies(e);
                    // A set is topped up as itself: guns with guns, a breach with
                    // the dismantlers and medics it walks in with. A raid is
                    // topped up to the set its price buys, or it does not enter
                    // (owner, 2026-10-21).
                    if raid::raids(e) {
                        let hard =
                            raid::hardest(room).map_or_else(|| goal.pos.clone(), |s| s.pos.clone());
                        let priced = |set: &[casting::Body]| {
                            let ours: Vec<Fighter> = set.iter().map(|b| fighter::cast(b)).collect();

                            raid::plan(atlas, room, &hard, &from, &ours, &breaking)
                        };

                        return raid::reinforce(e.capacity, &fielded, priced);
                    }

                    let priced = |set: &[casting::Body]| {
                        let fighters: Vec<Fighter> = set.iter().map(|b| fighter::cast(b)).collect();

                        self.breach_in(atlas, room, goal, &colony, &fighters, &breaking)
                    };

                    casting::reinforce(e.capacity, &fielded, &priced)
                });

            match planned {
                Some((added, (order, outcome))) if added.is_empty() => {
                    // Once: a new plan whose first barrier the set still cannot
                    // reach is no plan, or the event would fire every tick.
                    let reached = match order.first() {
                        Some(first) => siege::barrier_at(room)
                            .get(first.as_str())
                            .is_some_and(|b| !self.unreached(atlas, room, b, &in_room)),
                        None => true,
                    };

                    let mut next = siege::replanned(self.world.time, reached, e);
                    next.plan.order = order;
                    next.predicted = Some(outcome);
                    Some(next)
                }
                // Topped up, not recast: the set fights on as its medics walk in.
                Some((added, (order, outcome)))
                    if order.first().is_none_or(|first| {
                        siege::barrier_at(room)
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

    fn tapped(
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
            .fold(exchange::tower_strike(&self.charged(room), at), i32::max);
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

    fn armed_seen(&self, g: &Group) -> bool {
        let seen = self.sightings.get(&g.room);
        (g.members.iter()).any(|m| seen.and_then(|s| s.get(m)).is_some_and(|s| s.armed))
    }

    fn walk_of(&self, atlas: &Atlas, e: &Engagement, room: &str) -> Option<i32> {
        let from = self.members_of(e).first()?.pos.room.clone();
        let onto = fresh(0, Objective::Sweep, room, 0, Tactic::Kite);
        self.walk_to(atlas, &onto, &from)
    }

    /// A move asks no cast.
    fn elsewhere(
        &self,
        atlas: &Atlas,
        e: &Engagement,
        candidates: &[(String, i32, i32, bool)],
        grounds: &BTreeMap<String, Hunting>,
    ) -> Option<String> {
        let ours: Vec<Fighter> = (self.members_of(e).into_iter())
            .map(casting::fighter_of)
            .collect();
        let beats = |room: &String| match (self.in_sight(room), grounds.get(room)) {
            (Some(r), Some(g)) => {
                (self.hunt(g, r, ours.clone(), 0)).is_some_and(|(set, _)| set.is_empty())
            }
            _ => false,
        };
        let won = candidates
            .iter()
            .filter(|c| c.0 != e.room && if ours.is_empty() { c.3 } else { beats(&c.0) });
        atlas::onward(won.filter_map(|(room, worth, walk, _)| {
            let walk = if ours.is_empty() {
                Some(*walk)
            } else {
                self.walk_of(atlas, e, room)
            };
            Some((room, *worth, walk?))
        }))
    }

    /// An unseen room is worth its staleness; one owned is priced only in
    /// sight, under its towers, and one where the tracker still holds an armed
    /// group is not stale (owner), so a set never walks straight back to the
    /// escort it left.
    fn patrol(
        &self,
        atlas: &Atlas,
        e: &Engagement,
        preyed: &[String],
        age: impl Fn(&str) -> Option<(i32, bool)>,
    ) -> Option<String> {
        let open = preyed
            .iter()
            .filter(|r| **r != e.room && !self.avoid.contains(*r))
            .filter(|r| (self.by_id.values()).all(|g| g.room != **r || !self.armed_seen(g)));
        atlas::onward(open.filter_map(|r| match age(r)? {
            (_, true) => None,
            (age, false) => Some((r, age, self.walk_of(atlas, e, r)?)),
        }))
    }

    /// Priced at open, on arrival and on an event in its room, and again as
    /// the bank grows while it holds nothing.
    fn swept(
        &self,
        atlas: &Atlas,
        asked: &mut Asked,
        (e, onward): (&Engagement, impl FnOnce() -> Option<String>),
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
            if fielding.is_empty() {
                return Some(Engagement {
                    unanswered: true,
                    capacity,
                    ..e.clone()
                });
            }
            let onward = onward().filter(|_| !e.ours.is_empty());
            return onward.map(|r| sweep::onto(&r, self.world.time, e));
        };

        let escort = &ground.escort;
        // A set walks in as one; in its room each body's step is its own.
        let together = e.ours.len() + joining.len() + set.len() > 1;
        let worth = ground.worth + escort.iter().map(sweep::worth).sum::<i32>();
        let (candidates, pin) = self.ruled(e, sweep::CANDIDATES);
        let offers = sweep_offers(&candidates, together, worth, &set);

        let Some(tactic) = tactics::select(&offers, pin).map(|o| o.tactic) else {
            let onward = onward().filter(|_| !e.ours.is_empty());
            return onward.map(|r| sweep::onto(&r, self.world.time, e));
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
                let mut priced_bodies: Vec<Fighter> = bodies.clone();
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
                    losses: match outcome {
                        Outcome::Win(_, losses) => losses,
                        _ => Losses { dead: 0, hits: 0 },
                    },
                    ticks: match outcome {
                        Outcome::Win(ticks, _) | Outcome::Kite(ticks) => ticks,
                        Outcome::Lose => 0,
                    },
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

/// What a `Defend`'s price found, for its candidates to read (§6.3).
struct Priced {
    /// What the rival spent on the bodies the engagement fights.
    raid: i32,
    /// The set swings nothing, so it fights as one body.
    ranged: bool,
    /// A home's ramparts are a post the raid comes to.
    holds: bool,
    won: bool,
    losses: Losses,
    ticks: i32,
    set: Vec<casting::Body>,
    tick_worth: i32,
}

impl Priced {
    fn offers(&self, candidates: &[Tactic]) -> Vec<Offer> {
        candidates.iter().filter_map(|t| self.offer(*t)).collect()
    }

    /// One arm a candidate: what it wins against what it spends, and nothing
    /// where it cannot fight from here.
    fn offer(&self, tactic: Tactic) -> Option<Offer> {
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
fn offer(tactic: Tactic, set: &[casting::Body], value: i32, cost: i32) -> Offer {
    Offer {
        tactic,
        bodies: set.len() as i32,
        value,
        cost,
    }
}

/// What a `Sweep`'s set wins — the harvest it denies and the prey it kills —
/// against what it casts (§6.3).
fn sweep_offers(
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

/// A raid's guns are its set, not pickets.
fn picket(e: &Engagement, c: &Creep) -> bool {
    !raid::raids(e) && tactics::picket_of(c)
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
            offers: Vec::new(),
        },
        phase: Phase::Forming,
        opened: time,
        since: time,
        lost: 0,
        budget: 0,
        priority: 0.0,
        unanswered: true,
        ignored: 0,
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
    let intel = context.intel;
    let age = |room: &str| {
        intel
            .get(room)
            .map(|row| (world.time - row.seen, row.level > 0))
    };
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

    let walks = (engagements.iter())
        .map(|e| (e.room.clone(), Scene::hops(e)))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .map(|(room, hops)| {
            let tree = atlas.tree(&declarations.avoid, hops, &room);
            ((room, hops), tree)
        })
        .collect();
    let scene = Scene {
        world,
        sightings,
        avoid: &declarations.avoid,
        rules: &declarations.tactics,
        walks,
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
        boost: casting::cluster_boost(world, declarations),
    };

    // A planned barrier fallen advances the plan, never re-prices it; a
    // target fallen re-plans toward the next razed.
    let mut retargeted: BTreeSet<i32> = BTreeSet::new();

    for e in &mut engagements {
        if let Objective::Siege(target) = &e.objective
            && let Some(room) = scene.in_sight(&e.room)
        {
            let barriers = siege::barrier_at(room);
            e.plan.order.retain(|id| barriers.contains_key(id.as_str()));

            let near = scene.members_of(e).first().map(|c| c.pos.clone());

            let next = if raid::raids(e) {
                raid::target(room, &scene.members_of(e))
            } else {
                siege::next_target(room, near.as_ref())
            };

            if (!siege::stands(room, target) || siege::tapped(room, target).is_some())
                && let Some(next) = next
                && next != *target
            {
                e.objective = Objective::Siege(next);
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
        .filter_map(|room| {
            Some((
                room.clone(),
                scene.ground(atlas, scene.in_sight(room)?, &state.groups),
            ))
        })
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

    // The edge each siege would drain at: his border tile his towers reach,
    // nearest them, and the room across it our medics wait in. The gate is the
    // pair's: a tile his volley leaves the pairs the bank buys healing over.
    // A room with no such tile is not drained: the set breaches by its plan,
    // or stands off a safe mode as it did before the drain (§6.2).
    let gates: BTreeMap<i32, tactics::Gate> = (engagements.iter())
        .filter(|e| matches!(e.objective, Objective::Siege(_)) && !raid::raids(e))
        .filter_map(|e| {
            let room = scene.in_sight(&e.room)?;
            let towers = scene.charged(room);
            let drains = !towers.is_empty()
                && matches!(e.phase, Phase::Approaching | Phase::Draining)
                && (Scene::safe(room) || scene.outstruck(e, &towers));
            if !drains {
                return None;
            }
            let gate = siege::gate(&*atlas, &e.room, &scene.enter_room(&*atlas, e)?, &towers)?;
            siege::drain_pays(&e.room, e.capacity, &towers, &gate).then_some((e.id, gate))
        })
        .collect();
    let given_up: BTreeSet<i32> = (engagements.iter())
        .filter(|e| matches!(e.objective, Objective::Siege(_)) && siege::gave_up(e))
        .map(|e| e.id)
        .collect();

    let stepped: Vec<(Engagement, bool)> = engagements
        .iter()
        .filter_map(|e| {
            let next = match &e.objective {
                Objective::Defend => defend::step(&scene.facts_of(e, &standing, &aging_out), e),
                Objective::Siege(_) => {
                    let declared = declarations.sieges.contains(&e.room);
                    let f = scene.siege_facts(e, declared, &standing, &aging_out, gates.get(&e.id));
                    siege::step(&f, e)
                }
                Objective::Sweep => sweep::step(
                    &sweep::Facts {
                        time: world.time,
                        standing: standing.clone(),
                        aging_out: aging_out.clone(),
                        asked: scene.waiting.iter().any(|c| c.engagement == e.id),
                        arrived: scene.members_of(e).iter().all(|c| c.pos.room == e.room),
                        worth: grounds
                            .get(&e.room)
                            .map(|g| g.worth > 0 || !g.escort.is_empty()),
                        safe: scene.in_sight(&e.room).is_some_and(Scene::safe),
                        elsewhere: scene.elsewhere(atlas, e, &candidates, &grounds),
                        patrol: (grounds.get(&e.room))
                            .filter(|g| g.worth == 0 && g.escort.is_empty())
                            .and_then(|_| scene.patrol(atlas, e, &state.preyed, age)),
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
        // A safe mode refuses the breach but not the edge: his towers are
        // drawn there while it runs, and the siege opens on a drain only where
        // the pairs it casts out-heal his volley on the gate.
        .filter_map(|room| {
            let room = scene.in_sight(room)?;
            let target = siege::next_target(room, None)?;
            let towers = scene.charged(room);
            if !Scene::safe(room) {
                return Some((&room.name, target));
            }
            if towers.is_empty() {
                return None;
            }
            let mut watch = fresh(
                first_id,
                Objective::Siege(target.clone()),
                &room.name,
                world.time,
                Tactic::BlobClose,
            );
            watch.capacity = scene.capacity_of(&*atlas, &watch);
            // A safe mode's room opens on the drain that prices: the least his
            // towers strike a border tile is what a gate can draw at.
            siege::drainable(&room.name, watch.capacity, &towers).then_some((&room.name, target))
        })
        .enumerate()
        .map(|(i, (room, target))| {
            let id = first_id + (opened.len() + i) as i32;
            let tactic = Tactic::BlobClose;
            (
                fresh(id, Objective::Siege(target), room, world.time, tactic),
                false,
            )
        })
        .collect();

    let sweeping = stepped.iter().any(|(e, _)| e.objective == Objective::Sweep);
    let won = || candidates.iter().filter(|c| c.3);
    let swept = atlas::onward(won().map(|(r, w, k, _)| (r, *w, *k)))
        .or_else(|| atlas::onward(candidates.iter().map(|(r, w, k, _)| (r, *w, *k))))
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
        asked: BTreeMap::new(),
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
                if e.since == world.time && matches!(e.phase, Phase::Forming | Phase::Fighting)
                    || grew
                    || left
                    || scene.landed.iter().any(|c| c.engagement == e.id)
                    || cornered.contains(&e.id)
                    || changed(&e, &events, &scene.by_id) =>
            {
                let onward = || {
                    (scene.elsewhere(atlas, &e, &candidates, &grounds))
                        .or_else(|| scene.patrol(atlas, &e, &state.preyed, age))
                };
                decided.extend(scene.swept(atlas, &mut asked, (&e, onward), (&mut free, &grounds)));
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

    // A group lost from sight is sought in its room, which brings it back in
    // sight; a siege's room out of sight is entered anywhere, as its middle
    // may lie behind its walls.
    let centre_of = |e: &Engagement| -> (Pos, i32) {
        match (
            &e.objective,
            scene.by_id.get(&e.group),
            scene.next_struck(e),
        ) {
            // His towers are spent at the edge before the set breaches: it
            // waits in the room beside his, off the tiles a pair works.
            (Objective::Siege(_), _, _) if gates.contains_key(&e.id) => {
                (middle(&gates[&e.id].from), engine::ROOM_SIDE / 2)
            }
            (Objective::Siege(_), _, Some(b)) => (b.pos.clone(), engine::RANGED_RANGE),
            (Objective::Defend, Some(g), _) => (g.centre.clone(), engine::RANGED_RANGE),
            (Objective::Siege(_), _, None) => (middle(&e.room), engine::ROOM_SIDE / 2),
            _ => (middle(&e.room), engine::RANGED_RANGE),
        }
    };

    let mut walking: BTreeMap<String, Wish> = BTreeMap::new();
    // A siege's room is priced under its towers while out of sight.
    let fortified: BTreeSet<String> = world
        .rooms
        .iter()
        .filter(|r| !scene.charged(r).is_empty())
        .map(|r| r.name.clone())
        .chain(
            decided
                .iter()
                .filter(|e| matches!(e.objective, Objective::Siege(_)))
                .map(|e| e.room.clone()),
        )
        .collect();

    for e in &decided {
        // A set that walks in as one holds its formation into the fight.
        let formation = matches!(e.plan.tactic, Tactic::BlobClose | Tactic::BlobKite)
            && !e.unanswered
            && e.phase == Phase::Approaching;

        if formation {
            let members: Vec<&Creep> = scene
                .members_of(e)
                .into_iter()
                .filter(|c| raid::raids(e) || !swings(c))
                .collect();

            if !members.is_empty() {
                let (centre, range) = centre_of(e);

                for wish in tactics::blob(&mut sight, &centre, range, &members, &fortified) {
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
                        siege::exposed(&*sight.atlas, room)
                            .into_iter()
                            .map(|post| {
                                let strike = exchange::tower_strike(&towers, &post);
                                let worth = siege::denied(room, &post);
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

    let taps: BTreeMap<i32, &world::Controller> = (decided.iter())
        .filter(|e| e.phase != Phase::Disengaging)
        .filter_map(|e| match &e.objective {
            Objective::Siege(target) => {
                Some((e.id, siege::tapped(scene.in_sight(&e.room)?, target)?))
            }
            _ => None,
        })
        .collect();

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

        if let Some(controller) = taps.get(&e.id).filter(|_| tactics::claims(c)) {
            let tap = controller.upgrade_blocked == 0
                && c.pos.within(engine::MELEE_RANGE, &controller.pos);
            let act = tap.then(|| CreepAct::Unreserve(controller.id.clone()));
            return (
                act,
                tactics::toward(sight, c, &controller.pos, engine::MELEE_RANGE),
            );
        }

        // A picket walks with the set and takes its post once the set fights.
        if e.phase == Phase::Fighting
            && picket(e, c)
            && let Some(front) = fronts.get(&e.id)
            && !front.posts.is_empty()
        {
            return (None, tactics::picket(sight, front, &scene.members_of(e), c));
        }

        if e.phase == Phase::Fighting && fronts.contains_key(&e.id) && !raid::raids(e) {
            let members: Vec<&Creep> = scene
                .members_of(e)
                .into_iter()
                .filter(|m| !tactics::picket_of(m))
                .collect();
            return tactics::siege(sight, &fronts[&e.id], &members, c);
        }

        let (centre, _) = centre_of(e);
        let order = &e.plan.order;

        if e.unanswered {
            return (None, tactics::rampart(sight, c));
        }

        let wish = match e.phase {
            // The towers' room is the only one a drain presses into.
            // A drain that gave up stands off in the room beside his, casting
            // nothing more (§6.2).
            Phase::Draining => match gates.get(&e.id) {
                Some(gate) if given_up.contains(&e.id) => {
                    tactics::toward(sight, c, &middle(&gate.from), engine::ROOM_SIDE / 2)
                }
                Some(gate) => tactics::drain(sight, gate, c),
                None => tactics::hold(sight, c),
            },
            Phase::Forming => tactics::hold(sight, c),
            Phase::Pursuing => tactics::close(sight, order, &centre, c),
            Phase::Disengaging => tactics::retreat(sight, c),
            Phase::Approaching | Phase::Fighting => {
                if let Some(wish) = walking.get(&c.name) {
                    return (None, wish.clone());
                }

                if raid::raids(e) {
                    let (room, _) = centre_of(e);

                    return (
                        None,
                        match scene.in_sight(&e.room).filter(|r| r.name == c.pos.room) {
                            Some(in_sight) => raid::dance(sight, in_sight, &scene.members_of(e), c),
                            None => tactics::toward(sight, c, &room, engine::RANGED_RANGE),
                        },
                    );
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

                if matches!(tactic, Tactic::BlobClose)
                    && tactics::exposed(sight, &centre.room, &scene.members_of(e), c)
                {
                    tactics::step_back(sight, &centre.room, &scene.members_of(e), c)
                } else if matches!(tactic, Tactic::Kite | Tactic::BlobKite) && !swings(c) {
                    if raid::raids(e) {
                        tactics::kite(sight, Some(&centre), order, &centre, c)
                    } else {
                        tactics::kite(sight, None, order, &centre, c)
                    }
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
        let raid = engaged_in
            .get(&c.name)
            .map(|&at| &decided[at])
            .filter(|e| raid::raids(e));
        if wanted.iter().all(|a| a.slots() & intents::RANGED == 0) {
            let razed = raid
                .and_then(|e| scene.in_sight(&e.room))
                .filter(|r| r.name == c.pos.room)
                .and_then(|room| raid::volley(&sight, room, c));

            match (raid.is_some(), razed) {
                (true, Some(act)) => wanted.push(act),
                (armed, _) => wanted.extend(tactics::strike(&sight, order, c, armed)),
            }
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
