/// What the fight reads of a tick's world, once, before any engagement moves.
use std::collections::{BTreeMap, BTreeSet};

use super::casting;
use super::kinds::defend::Facts as DefendFacts;
use super::kinds::siege::{self, Facts as SiegeFacts};
use super::kinds::{raid, sweep};
use super::shape::{Cast, Engagement, Group, Objective, Phase, Plan};
use fabot_atlas::{Atlas, Mover, Target, Tree};
use fabot_base::declarations::{self, TacticRule};
use fabot_base::grid::{self, Footprint};
use fabot_base::tiles::Tile;
use fabot_base::vocabulary::{Part, ResourceType};
use fabot_base::world::{
    Creep, Id, Kind, Owner, Pos, Room, RoomName, Sightings, Structure, World, hostiles, sightings,
};
use fabot_base::{engine, tiles};
use fabot_exchange::{self as exchange, Fighter, fighter};
use fabot_tactics::{self as tactics, Tactic};

#[derive(Default)]
pub(super) struct Asked {
    pub(super) queued: BTreeMap<RoomName, i32>,
    /// The energy each room's queued bodies will draw of its bank.
    pub(super) asked: BTreeMap<RoomName, i32>,
    pub(super) casts: Vec<Cast>,
    pub(super) due: BTreeSet<RoomName>,
}

impl Asked {
    pub(super) fn queue(&mut self, room: RoomName, body: &[Part]) {
        let ticks = engine::SPAWN_TICKS_PER_PART * body.len() as i32;
        *self.queued.entry(room).or_insert(0) += ticks;
        *self.asked.entry(room).or_insert(0) += engine::body_cost(body);
    }
}

/// What a sweep reads of a room once a tick: its escort and marks (prey
/// first) outside its barriers, what strikes each tile, what holding it is
/// worth and where the hunt stands.
pub(super) struct Hunting {
    pub(super) escort: Vec<Creep>,
    pub(super) threat: tiles::Table<i32>,
    pub(super) marks: Vec<tactics::Mark>,
    pub(super) worth: i32,
    pub(super) at: Pos,
}

pub(super) struct Scene<'a> {
    pub(super) world: &'a World<'a>,
    pub(super) sightings: &'a Sightings,
    pub(super) remembered: BTreeMap<RoomName, Vec<Creep>>,
    pub(super) avoid: &'a BTreeSet<RoomName>,
    /// The operator's tactic rules (§6.3), filtered per engagement.
    pub(super) rules: &'a [TacticRule],
    pub(super) walks: BTreeMap<(RoomName, i32), Tree>,
    pub(super) homes: BTreeSet<RoomName>,
    pub(super) defended: BTreeSet<RoomName>,
    pub(super) musters: Vec<Pos>,
    pub(super) worked: BTreeSet<RoomName>,
    pub(super) foes: BTreeMap<RoomName, Vec<Creep>>,
    pub(super) cores: BTreeSet<Id>,
    pub(super) by_id: BTreeMap<i32, Group>,
    pub(super) hostile_at: BTreeMap<Id, Pos>,
    pub(super) ours: BTreeMap<&'a str, &'a Creep>,
    pub(super) armed: BTreeSet<Id>,
    pub(super) landed: Vec<Cast>,
    pub(super) spawning: Vec<Cast>,
    /// The compound the declared lab cluster covers, and the parts it buys.
    pub(super) boost: Option<(ResourceType, i32)>,
}

impl<'a> Scene<'a> {
    /// The reflex's stance (§7.2): a stranger where it threatens or works our room.
    pub(super) fn struck(&self, h: &Creep) -> bool {
        let ours = self.homes.contains(&h.pos.room) || self.worked.contains(&h.pos.room);
        match h.owner {
            Owner::Ally(_) => false,
            Owner::Stranger(_) => self.armed.contains(&h.id) || ours && hostiles::works(h),
            _ => true,
        }
    }

    pub(super) fn rival(s: &Structure) -> bool {
        !s.owner.is_ours() && matches!(s.kind, Kind::Rampart | Kind::Wall)
    }

    pub(super) fn safe(room: &Room) -> bool {
        room.controller
            .as_ref()
            .is_some_and(|c| !c.owner.is_ours() && c.safe_mode > 0)
    }

    pub(super) fn in_sight(&self, room: RoomName) -> Option<&'a Room<'a>> {
        self.world.rooms.iter().find(|r| r.name == room)
    }

    /// The usernames whose room this is and who stands in it: what a rule's
    /// `rival` names (§6.3).
    pub(super) fn rivals_of(&self, room: RoomName) -> BTreeSet<&'a str> {
        let seen = self.in_sight(room).into_iter();
        (seen.flat_map(|r| {
            let owners = r.structures.iter().map(|s| &s.owner);
            owners
                .chain(r.controller.as_ref().map(|c| &c.owner))
                .chain(r.hostiles().map(|h| &h.owner))
        }))
        .filter_map(Owner::user)
        .collect()
    }

    /// A kind's candidates as the operator's rules leave them, and the pin it
    /// names (§6.3).
    pub(super) fn ruled(
        &self,
        e: &Engagement,
        candidates: &[Tactic],
    ) -> (Vec<Tactic>, Option<Tactic>) {
        let ruling = tactics::ruling(self.rules, e.room, &self.rivals_of(e.room));

        let left = candidates
            .iter()
            .copied()
            .filter(|t| !ruling.forbid.contains(t))
            .collect();

        (left, ruling.pin)
    }

    pub(super) fn next_struck(&self, e: &Engagement) -> Option<&'a Structure> {
        let room = self.in_sight(e.room)?;

        match (e.plan.order.first(), &e.objective) {
            (Some(first), _) => siege::barrier_at(room).get(first).copied(),
            (None, Objective::Siege(target)) => room.structures.iter().find(|s| s.id == *target),
            (None, Objective::Defend | Objective::Sweep) => None,
        }
    }

    /// `was`: last tick's groups (§6.1), the only record of where a body stood.
    pub(super) fn ground(&self, atlas: &Atlas, room: &Room, was: &[Group]) -> Hunting {
        let reached = atlas.from_exits(room.name);
        // Denied only once a body stands there: its life less the most a body
        // spawns in and its walk.
        let e = fresh(0, Objective::Sweep, room.name, 0, Tactic::Kite);
        let walk = (self.colonies_of(atlas, &e).iter())
            .filter_map(|r| self.walk_to(atlas, &e, r.name))
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
        let weights = atlas.held_weights(room.name);
        let shields: BTreeSet<Tile> = (room.structures.iter())
            .filter(|s| s.kind == Kind::Rampart && !s.owner.is_ours())
            .map(|s| tiles::of_pos(&s.pos))
            .collect();
        let solid: BTreeSet<Tile> = (room.structures.iter())
            .filter(|s| !s.walkable && s.kind != Kind::Rampart)
            .map(|s| tiles::of_pos(&s.pos))
            .collect();
        // A shot on a body under its owner's rampart lands on the rampart.
        let shot = |t: Tile| -> Option<Pos> {
            (!shields.contains(&t)).then(|| tiles::to_pos(room.name, t))
        };
        let before = |id: &Id| {
            (was.iter())
                .find(|g| g.last_seen == self.world.time - 1 && g.members == BTreeSet::from([*id]))
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
                id: c.id,
                pos: c.pos,
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
                .filter(|s| s.kind == Kind::Container || s.kind == Kind::Link && !s.owner.is_ours())
                .filter(|s| siege::beside(&reached, &s.pos))
                .map(|s| tactics::Mark {
                    id: s.id,
                    pos: s.pos,
                    worth: s.hits,
                    hits: s.hits,
                    massed: s.kind != Kind::Container,
                    shielded: shields.contains(&tiles::of_pos(&s.pos)),
                    fleeting: false,
                    next: vec![shot(tiles::of_pos(&s.pos))],
                }),
        );
        Hunting {
            threat: exchange::damage_map(room.name, &raid::charged(room), &foes),
            worth: sweep::room_worth(room, &prey, &exposed, held),
            at: exposed
                .first()
                .or(prey.first().map(|c| &c.pos))
                .copied()
                .unwrap_or_else(|| middle(room.name)),
            escort,
            marks,
        }
    }

    pub(super) fn towers_in(&self, room: &'a Room<'a>) -> Vec<&'a Structure> {
        room.structures
            .iter()
            .filter(|s| s.kind == Kind::Tower && !s.owner.is_ours())
            .collect()
    }

    pub(super) fn members_of(&self, e: &Engagement) -> Vec<&'a Creep> {
        e.ours
            .iter()
            .filter_map(|n| self.ours.get(n.as_str()).copied())
            .collect()
    }

    pub(super) fn hops(e: &Engagement) -> i32 {
        match e.objective {
            Objective::Siege(_) | Objective::Sweep => declarations::SIEGE_HOPS,
            Objective::Defend => declarations::REACH_HOPS,
        }
    }

    /// The room a set walks in from: the one before his on its colony's route.
    pub(super) fn enter_room(&self, atlas: &Atlas, e: &Engagement) -> Option<RoomName> {
        let colony = self.colonies_of(atlas, e).first()?.name;
        let hops = Scene::hops(e);
        let rooms = match self.walks.get(&(e.room, hops)) {
            Some(tree) => tree.route(colony, e.room),
            None => atlas.route(self.avoid, hops, colony, e.room),
        }?;

        (*rooms.get(rooms.len().checked_sub(2)?)?).into()
    }

    pub(super) fn walk_to(&self, atlas: &Atlas, e: &Engagement, from: RoomName) -> Option<i32> {
        let hops = Scene::hops(e);
        let crossed = match self.walks.get(&(e.room, hops)) {
            Some(tree) => tree.crossings(from, e.room),
            None => (atlas.route(self.avoid, hops, from, e.room)).map(|r| r.len() as i32 - 1),
        };
        crossed.map(|n| engine::ROOM_SIDE * n)
    }

    pub(super) fn raid_at(&self, e: &Engagement, room: RoomName) -> Option<Pos> {
        let ours = self.members_of(e).into_iter().map(|c| c.pos);
        let from: Vec<Pos> = ours.chain(self.homes.iter().map(|&r| middle(r))).collect();
        sightings::raid_at(self.remembered.get(&room)?, &from)
    }

    /// A home's Defend is the home's alone.
    pub(super) fn colonies_of(&self, atlas: &Atlas, e: &Engagement) -> Vec<&'a Room<'a>> {
        self.world
            .rooms
            .iter()
            .filter(|r| {
                self.homes.contains(&r.name)
                    && if self.homes.contains(&e.room) {
                        r.name == e.room
                    } else {
                        self.walk_to(atlas, e, r.name).is_some()
                    }
            })
            .collect()
    }

    pub(super) fn capacity_of(&self, atlas: &Atlas, e: &Engagement) -> i32 {
        self.colonies_of(atlas, e)
            .iter()
            .fold(0, |most, r| most.max(r.energy_capacity))
    }

    pub(super) fn spent(&self, e: &Engagement) -> bool {
        self.homes.contains(&e.room) && e.lost > e.budget
    }

    pub(super) fn ask(
        &self,
        atlas: &Atlas,
        e: &Engagement,
        set: &[casting::Body],
        asked: &mut Asked,
    ) {
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

        self.ask_slots(atlas, e, free.zip(set.iter().cloned()), asked);
    }

    /// How many of the drain's bodies the set already holds, standing or in
    /// an oven: a pair's shape is no other body's.
    pub(super) fn paired(&self, e: &Engagement, ovens: &[Cast], set: &[casting::Body]) -> usize {
        let of = |body: &[Part]| set.iter().any(|b| b[..] == *body);
        let stand = self
            .members_of(e)
            .iter()
            .filter(|c| of(&fabot_base::world::body_kinds(c)))
            .count();
        stand
            + ovens
                .iter()
                .filter(|c| c.engagement == e.id && of(&c.body))
                .count()
    }

    pub(super) fn ask_slots(
        &self,
        atlas: &Atlas,
        e: &Engagement,
        set: impl IntoIterator<Item = (String, casting::Body)>,
        asked: &mut Asked,
    ) {
        let colonies = self.colonies_of(atlas, e);

        for (slot, body) in set {
            let colony = casting::colony_for(
                &colonies,
                &self.world.spawns,
                &asked.queued,
                &asked.asked,
                |from| self.walk_to(atlas, e, from),
                &body,
            );

            if let Some(colony) = colony {
                asked.queue(colony, &body);
                let suffix = fighter::boosted_by(&body, self.boost)
                    .map_or_else(String::new, |c| format!("-{c}"));
                let name = format!(
                    "{}{}-{}-{slot}{suffix}",
                    declarations::FIGHT_PREFIX,
                    self.world.time,
                    e.id
                );

                asked.casts.push(Cast {
                    engagement: e.id,
                    name,
                    room: colony,
                    body,
                });
            }
        }
    }

    /// A body that dies before a successor asked now could stand is
    /// succeeded in its slot (§6.2's rotation).
    pub(super) fn succeed(&self, atlas: &Atlas, e: &Engagement, asked: &mut Asked) {
        let Some((walk, home)) = (self.colonies_of(atlas, e).iter())
            .filter_map(|r| Some((self.walk_to(atlas, e, r.name)?, r.name)))
            .min()
        else {
            return;
        };

        let members = self.members_of(e);

        let lasts = |c: &Creep, lead: i32| {
            c.ticks_to_live > engine::SPAWN_TICKS_PER_PART * c.body.len() as i32 + walk + lead
        };

        let held: BTreeSet<String> = members
            .iter()
            .filter(|c| lasts(c, 0))
            .map(|c| slot_of(&c.name))
            .chain(
                asked
                    .casts
                    .iter()
                    .filter(|c| c.engagement == e.id)
                    .map(|c| slot_of(&c.name)),
            )
            .collect();

        let due: BTreeMap<String, casting::Body> = members
            .iter()
            .filter(|c| !held.contains(&slot_of(&c.name)))
            .map(|c| (slot_of(&c.name), fabot_base::world::body_kinds(c)))
            .collect();

        let lead = engine::SPAWN_TICKS_PER_PART * engine::MAX_BODY_PARTS as i32;
        asked
            .due
            .extend(members.iter().any(|c| !lasts(c, lead)).then_some(home));

        self.ask_slots(atlas, e, due, asked);
    }

    pub(super) fn facts_of(
        &self,
        e: &Engagement,
        standing: &BTreeSet<String>,
        aging_out: &BTreeSet<String>,
    ) -> DefendFacts {
        let group = self.by_id.get(&e.group).cloned();

        let near = |name: &str| -> bool {
            match (self.ours.get(name), &group) {
                (Some(creep), Some(g)) => (g.members.iter())
                    .filter_map(|m| self.hostile_at.get(m))
                    .any(|at| creep.pos.within(engine::RANGED_RANGE, at)),
                _ => false,
            }
        };

        let in_reach = e.ours.iter().any(|n| near(n));
        let on_our_ground = group
            .as_ref()
            .is_some_and(|g| self.defended.contains(&g.room()));

        DefendFacts {
            time: self.world.time,
            group,
            standing: standing.clone(),
            aging_out: aging_out.clone(),
            in_reach,
            home: self.home(e),
            held: self.homes.contains(&e.room),
            asked: self.spawning.iter().any(|c| c.engagement == e.id),
            on_our_ground,
            // A room in sight shows where its groups stand: one gone from it
            // has left, whoever else stands there (a core, or Invaders the
            // raid passed), and they open engagements of their own.
            clear: self.in_sight(e.room).is_some()
                || sightings::clear(self.sightings, self.world.time, e.room),
            raided: !sightings::unarmed(self.sightings, self.world.time, e.room),
            remembered: self.remembered.contains_key(&e.room),
        }
    }

    fn home(&self, e: &Engagement) -> bool {
        (e.ours.iter())
            .all(|n| (self.ours.get(n.as_str())).is_some_and(|c| self.homes.contains(&c.pos.room)))
    }

    pub(super) fn siege_facts(
        &self,
        e: &Engagement,
        declared: bool,
        standing: &BTreeSet<String>,
        aging_out: &BTreeSet<String>,
        gate: Option<&tactics::Gate>,
    ) -> SiegeFacts {
        let room = self.in_sight(e.room);
        let towers = room.map_or_else(Vec::new, |r| raid::charged(r));

        SiegeFacts {
            time: self.world.time,
            standing: standing.clone(),
            aging_out: aging_out.clone(),
            asked: self.spawning.iter().any(|c| c.engagement == e.id),
            home: self.home(e),
            under_fire: e.ours.iter().any(|n| {
                self.ours
                    .get(n.as_str())
                    .is_some_and(|c| c.pos.room == e.room && !picket(e, c))
            }),
            charged: !towers.is_empty(),
            drain: gate.is_some(),
            priced: siege::drainable(e.room, e.capacity, &towers),
            // A drain with no pair standing, in an oven, or tripped holds nothing.
            worked: e.ignored > 0
                || self.paired(e, &self.spawning, &casting::drains(e.capacity)) > 0,
            bounced: gate.is_some_and(|g| self.bounced(e, g)),
            drawn: self.drawn(e),
            gave_up: siege::gave_up(e),
            razed: room.is_some_and(|room| match &e.objective {
                Objective::Siege(target) => {
                    !siege::stands(room, *target)
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
    pub(super) fn bounced(&self, e: &Engagement, gate: &tactics::Gate) -> bool {
        let back = tiles::to_pos(gate.from, gate.bounce);

        (self.members_of(e))
            .iter()
            .any(|c| c.pos == back && !c.body.iter().any(|p| p.kind == Part::Heal))
    }

    /// One of his towers struck one of ours in his room on the trip just
    /// ended: the only answer a drain counts.
    pub(super) fn drawn(&self, e: &Engagement) -> bool {
        if e.phase != Phase::Draining || self.world.attacks.is_empty() {
            return false;
        }

        let towers: BTreeSet<&Id> = (self.in_sight(e.room))
            .into_iter()
            .flat_map(|r| self.towers_in(r))
            .map(|t| &t.id)
            .collect();

        (self.world.attacks.iter())
            .any(|(by, at)| towers.contains(by) && (self.world.creeps).iter().any(|c| &c.id == at))
    }

    /// His towers out-strike the heal the set seats on one body at the breach,
    /// so the gate is worth working.
    pub(super) fn outstruck(&self, e: &Engagement, towers: &[Pos]) -> bool {
        let set = self.fielded(e);
        let seated = exchange::pooled_heal(set.len() as i32, &set);
        (self.next_struck(e)).is_some_and(|b| exchange::tower_strike(towers, &b.pos) > seated)
    }

    /// Whole, as cast: a part struck off is healed back, and only a body lost
    /// re-prices the set.
    pub(super) fn fielded(&self, e: &Engagement) -> Vec<Fighter> {
        self.fielded_bodies(e)
            .iter()
            .map(|b| fighter::cast(b))
            .collect()
    }

    /// One body a slot: a successor and the body it succeeds are one.
    pub(super) fn fielded_bodies(&self, e: &Engagement) -> Vec<casting::Body> {
        let mut bodies: BTreeMap<String, casting::Body> = e
            .ours
            .iter()
            .filter_map(|n| self.ours.get(n.as_str()))
            .map(|c| (slot_of(&c.name), fabot_base::world::body_kinds(c)))
            .collect();

        bodies.extend(
            self.spawning
                .iter()
                .filter(|c| c.engagement == e.id)
                .map(|c| (slot_of(&c.name), c.body.clone())),
        );

        // A picket holds its post, not the breach.
        bodies
            .into_values()
            .filter(|b| raid::raids(e) || !b.contains(&Part::RangedAttack))
            .collect()
    }

    pub(super) fn unreached(
        &self,
        atlas: &Atlas,
        room: &Room,
        b: &Structure,
        in_room: &[&Creep],
    ) -> bool {
        !in_room.is_empty()
            && in_room.iter().all(|m| {
                atlas
                    .field(
                        Mover {
                            footprint: Footprint::One,
                            factor: grid::factor_of(m),
                        },
                        &Target {
                            room: room.name,
                            tiles: vec![tiles::of_pos(&b.pos)],
                            range: engine::MELEE_RANGE,
                            avoid: BTreeSet::new(),
                            max_hops: declarations::SIEGE_HOPS,
                        },
                        &m.pos,
                    )
                    .and_then(|field| field.ticks(tiles::of_pos(&m.pos)))
                    .is_none()
            })
    }
}

/// A raid's guns are its set, not pickets.
pub(super) fn picket(e: &Engagement, c: &Creep) -> bool {
    !raid::raids(e) && tactics::picket_of(c)
}

pub(super) fn middle(room: RoomName) -> Pos {
    Pos::new(room, engine::ROOM_SIDE / 2, engine::ROOM_SIDE / 2)
}

/// A body's slot in its set, its name's last part, kept by its successors.
pub(super) fn slot_of(name: &str) -> String {
    name.rsplit('-').next().unwrap_or_default().to_string()
}

pub(super) fn fresh(
    id: i32,
    objective: Objective,
    room: RoomName,
    time: i32,
    tactic: Tactic,
) -> Engagement {
    Engagement {
        id,
        objective,
        group: 0,
        room,
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
