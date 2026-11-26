/// What the fight reads of a tick's world, once, before any engagement moves.
use std::collections::{BTreeMap, BTreeSet};

use super::casting;
use super::kinds::defend::{self, Facts as DefendFacts};
use super::kinds::sweep;
use super::shape::{Boosted, Cast, Engagement, Group, Objective, Phase, Plan, Slot};
use fabot_atlas::{Atlas, Tree};
use fabot_base::declarations::{self, TacticRule};
use fabot_base::tiles::Tile;
use fabot_base::vocabulary::Part;
use fabot_base::world::{
    Creep, Id, Owner, Pos, Room, RoomName, Sightings, Structure, StructureType, World, hostiles,
    sightings,
};
use fabot_base::{boost, engine, tiles};
use fabot_exchange::{self as exchange, Fighter};
use fabot_tactics::{self as tactics, Tactic};

#[derive(Default)]
pub(super) struct Asked {
    /// Each room's queued bodies' part counts, in ask order.
    pub(super) queued: BTreeMap<RoomName, Vec<usize>>,
    /// The energy each room's queued bodies will draw of its bank.
    pub(super) asked: BTreeMap<RoomName, i32>,
    pub(super) casts: Vec<Cast>,
    pub(super) boosts: casting::Boosts,
}

impl Asked {
    pub(super) fn queue(&mut self, room: RoomName, body: &[Part]) {
        self.queued.entry(room).or_default().push(body.len());
        *self.asked.entry(room).or_insert(0) += engine::body_cost(body);
    }
}

/// What a sweep reads of a room once a tick: its escort, its marks (prey,
/// else containers) outside its barriers, what strikes each tile, what holding it is
/// worth and where the hunt stands.
pub(super) struct Hunting {
    pub(super) escort: Vec<Creep>,
    pub(super) threat: tiles::Table<i32>,
    pub(super) marks: Vec<tactics::Mark>,
    pub(super) worth: i32,
    pub(super) at: Pos,
    pub(super) middle: Pos,
    pub(super) walk: Option<i32>,
}

pub(super) struct Scene<'a> {
    pub(super) world: &'a World<'a>,
    pub(super) sightings: &'a Sightings,
    pub(super) intel: &'a fabot_base::intel::Intel,
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
    pub(super) boosts: casting::Boosts,
    pub(super) boosting: &'a [(RoomName, boost::Boosting)],
}

/// A set fighting never stands at a lab.
pub fn boosted(engagements: &[Engagement]) -> impl Iterator<Item = &Boosted> {
    (engagements.iter())
        .filter(|e| e.phase == Phase::Forming)
        .flat_map(|e| &e.plan.boosts)
}

/// A body to cast in `slot`, and the colony its price boosts it at.
pub(super) struct Wanted {
    slot: Slot,
    body: casting::Body,
    boost_at: Option<RoomName>,
}

impl<'a> Scene<'a> {
    fn at_labs(&self, c: &Creep) -> Option<&boost::Wait> {
        (self.boosting.iter())
            .find_map(|(_, b)| b.waits.iter().find(|w| w.carried && w.creep == c.id))
    }

    /// A set leaves Forming for its fight only once no body its plan boosts
    /// stands at its labs, whatever its kind, and leaving clears the plan's
    /// boosts: no fallback to Forming holds a body that has gone out.
    pub(super) fn boost_hold(&self, before: &[Engagement], decided: &mut [Engagement]) {
        for next in decided {
            let forming = (before.iter()).find(|e| e.id == next.id && e.phase == Phase::Forming);
            let at_labs = (self.members_of(next).iter()).any(|c| self.at_labs(c).is_some());
            let onward = matches!(next.phase, Phase::Approaching | Phase::Fighting);
            if let Some(was) = forming.filter(|_| at_labs && onward) {
                (next.phase, next.since) = (Phase::Forming, was.since);
            }
            if next.phase != Phase::Forming {
                next.plan.boosts.clear();
            }
        }
    }

    /// A body still at its labs walks once the wait its price allowed is out.
    pub(super) fn fighter(&self, c: &Creep) -> Fighter {
        Fighter {
            lab_wait: self.at_labs(c).map_or(0, |w| w.left),
            ..casting::fighter_of(c)
        }
    }

    /// The reflex's stance (§7.2): a stranger where it threatens or works our room.
    pub(super) fn struck(&self, h: &Creep) -> bool {
        let ours = self.homes.contains(&h.pos.room) || self.worked.contains(&h.pos.room);
        match h.owner {
            Owner::Ally(_) => false,
            Owner::Stranger(_) => {
                self.armed.contains(&h.id) || ours && (hostiles::works(h) || hostiles::weaponed(h))
            }
            _ => true,
        }
    }

    pub(super) fn safe(room: &Room) -> bool {
        room.controller
            .as_ref()
            .is_some_and(|c| c.owner.is_theirs() && c.safe_mode > 0)
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

    /// `was`: last tick's groups (§6.1), the only record of where a body stood.
    pub(super) fn ground(&self, atlas: &Atlas, room: &Room, was: &[Group]) -> Hunting {
        let reached = atlas.from_exits(room.name);
        let middle = fabot_atlas::middle_in(&reached, room.name);
        // Denied only once a body stands there: its life less the most a
        // harasser spawns in and its walk.
        let e = fresh(0, Objective::Sweep, room.name, 0, Tactic::Kite);
        let walk = self.nearest(atlas, &e).map(|(walk, _)| walk);
        let parts = sweep::HARASSERS.iter().map(|b| b.len()).max().unwrap_or(0);
        let spawn = engine::spawn_ticks(parts);
        let held = walk.map_or(0, |w| engine::CREEP_LIFETIME - spawn - w);
        let all = sweep::prey(room);
        let prey: Vec<&Creep> = all
            .iter()
            .copied()
            .filter(|c| sweep::beside(&reached, &c.pos))
            .collect();
        let exposed = sweep::exposed_in(&reached, room);
        let escort: Vec<Creep> = (room.hostiles())
            .filter(|h| {
                let strikes = hostiles::foe(h) && hostiles::strikes(h);
                (self.armed.contains(&h.id) || strikes) && all.iter().all(|p| p.id != h.id)
            })
            .cloned()
            .collect();
        let foes: Vec<Fighter> = escort.iter().map(casting::fighter_of).collect();
        let weights = atlas.held_weights(room.name);
        let shields: BTreeSet<Tile> = (room.structures.iter())
            .filter(|s| s.kind == StructureType::Rampart && s.owner.is_theirs())
            .map(|s| tiles::of_pos(&s.pos))
            .collect();
        let solid: BTreeSet<Tile> = (room.structures.iter())
            .filter(|s| !s.walkable && s.kind != StructureType::Rampart)
            .map(|s| tiles::of_pos(&s.pos))
            .chain(room.blocked.iter().map(tiles::of_pos))
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
        let mut marks: Vec<tactics::Mark> = (prey.iter())
            .map(|c| tactics::Mark {
                id: c.id,
                pos: c.pos,
                worth: sweep::worth(c),
                hits: c.hits,
                massed: true,
                creep: true,
                shielded: shields.contains(&tiles::of_pos(&c.pos)),
                fleeting: dances(c),
                next: flight(c),
            })
            .collect();
        let creepless = marks.is_empty();
        marks.extend(
            room.structures
                .iter()
                .filter(|s| {
                    s.kind == StructureType::Container
                        || s.kind == StructureType::Link && s.owner.is_theirs()
                })
                .filter(|s| creepless && sweep::beside(&reached, &s.pos))
                .map(|s| tactics::Mark {
                    id: s.id,
                    pos: s.pos,
                    worth: (s.kind.construction_cost())
                        .map_or(0, |c| c as i32 * s.hits / s.hits_max.max(1)),
                    hits: s.hits,
                    massed: s.kind != StructureType::Container,
                    creep: false,
                    shielded: shields.contains(&tiles::of_pos(&s.pos)),
                    fleeting: false,
                    next: vec![shot(tiles::of_pos(&s.pos))],
                }),
        );
        Hunting {
            threat: exchange::damage_map(room.name, &self.world.rules.charged_towers(room), &foes),
            worth: sweep::room_worth(room, &prey, &exposed, held)
                * sweep::kept(room, held, walk.unwrap_or(0))
                / held.max(1),
            at: exposed
                .first()
                .or(prey.first().map(|c| &c.pos))
                .copied()
                .unwrap_or(middle),
            middle,
            escort,
            marks,
            walk,
        }
    }

    pub(super) fn towers_in(&self, room: &'a Room<'a>) -> Vec<&'a Structure> {
        (self.world.rules.towers(room))
            .filter(|s| s.owner.is_theirs())
            .collect()
    }

    pub(super) fn members_of(&self, e: &Engagement) -> Vec<&'a Creep> {
        (e.ours.keys())
            .filter_map(|n| self.ours.get(n.as_str()).copied())
            .collect()
    }

    pub(super) fn hops(e: &Engagement) -> i32 {
        match e.objective {
            Objective::Defend => declarations::REACH_HOPS,
            _ => declarations::FAR_HOPS,
        }
    }

    /// The nearest colony and its walk: a set is priced as walked and boosted
    /// from it, and a body its stock boosts is cast there.
    pub(super) fn nearest(&self, atlas: &Atlas, e: &Engagement) -> Option<(i32, RoomName)> {
        (self.colonies_of(atlas, e).iter())
            .filter_map(|r| Some((self.walk_to(atlas, e, r.name)?, r.name)))
            .min()
    }

    pub(super) fn boosts_for(
        &self,
        atlas: &Atlas,
        e: &Engagement,
        left: &casting::Boosts,
    ) -> boost::Stock {
        (self
            .nearest(atlas, e)
            .map(|(_, home)| home)
            .filter(|_| e.phase == Phase::Forming))
        .and_then(|home| left.get(&home))
        .cloned()
        .unwrap_or_default()
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
        let from: Vec<Pos> = ours
            .chain(self.homes.iter().map(|&r| tiles::middle(r)))
            .collect();
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

    pub(super) fn spawns_in(&self, colonies: &[&Room]) -> i32 {
        (self.world.working_spawns())
            .filter(|s| colonies.iter().any(|r| r.name == s.pos.room))
            .count() as i32
    }

    pub(super) fn capacity_of(&self, atlas: &Atlas, e: &Engagement) -> i32 {
        self.colonies_of(atlas, e)
            .iter()
            .fold(0, |most, r| most.max(r.energy_capacity))
    }

    pub(super) fn spent(&self, e: &Engagement) -> bool {
        self.homes.contains(&e.room) && defend::beaten(e)
    }

    pub(super) fn asked(&self) -> Asked {
        let mut asked = Asked {
            casts: self.spawning.clone(),
            boosts: self.boosts.clone(),
            ..Asked::default()
        };
        for c in &self.spawning {
            asked.queue(c.room, &c.body);
        }
        asked
    }

    /// The plan's boosts after these casts: a set priced boosted while it
    /// forms is cast and boosted at the boost home.
    pub(super) fn ask(
        &self,
        atlas: &Atlas,
        e: &Engagement,
        set: casting::Asking,
        asked: &mut Asked,
    ) -> Vec<Boosted> {
        let boost_at = (self.nearest(atlas, e).map(|(_, home)| home))
            .filter(|_| set.boosted && e.phase == Phase::Forming);
        let asks: Vec<Wanted> = (e.free(&asked.casts).zip(set.bodies))
            .map(|(slot, body)| Wanted {
                slot,
                body: body.clone(),
                boost_at,
            })
            .collect();
        let cast = self.ask_slots(atlas, e, asks, asked);
        let held =
            |name: &str| e.ours.contains_key(name) || asked.casts.iter().any(|c| c.name == name);
        (e.plan.boosts.iter())
            .filter(|b| held(&b.name))
            .cloned()
            .chain(cast)
            .collect()
    }

    pub(super) fn ask_slots(
        &self,
        atlas: &Atlas,
        e: &Engagement,
        set: impl IntoIterator<Item = Wanted>,
        asked: &mut Asked,
    ) -> Vec<Boosted> {
        let colonies = self.colonies_of(atlas, e);
        let mut boosts = Vec::new();

        for Wanted {
            slot,
            body,
            boost_at: at,
        } in set
        {
            let pick = |from: &[&Room]| {
                casting::colony_for(
                    from,
                    &self.world.working_spawns().collect::<Vec<_>>(),
                    &asked.queued,
                    &asked.asked,
                    |from| self.walk_to(atlas, e, from),
                    &body,
                )
            };
            let home: Vec<&Room> = colonies
                .iter()
                .copied()
                .filter(|r| Some(r.name) == at)
                .collect();
            let colony = at.and_then(|_| pick(&home)).or_else(|| pick(&colonies));

            if let Some(colony) = colony {
                asked.queue(colony, &body);
                let name = defend::cast_name(self.world.time, e.id, slot);
                // A body priced boosted and cast elsewhere walks straight out.
                let stock = asked.boosts.get_mut(&colony).filter(|_| Some(colony) == at);
                if stock.is_some_and(|s| !boost::boosted_by(&body, s).compounds.is_empty()) {
                    boosts.push(Boosted {
                        name: name.clone(),
                        colony,
                    });
                }

                asked.casts.push(Cast {
                    engagement: e.id,
                    name,
                    slot,
                    room: colony,
                    body,
                });
            }
        }
        boosts
    }

    /// Its room's armed and with `own` its own, as Price fights: the tracker may split a raid.
    pub(super) fn raid<'s>(&'s self, g: &'s Group, own: bool) -> impl Iterator<Item = &'s Creep> {
        (self.foes.get(&g.room()).into_iter().flatten())
            .filter(move |h| self.armed.contains(&h.id) || own && g.members.contains(&h.id))
    }

    pub(crate) fn in_reach(&self, e: &Engagement) -> bool {
        let raid: Vec<&Pos> = (self.by_id.get(&e.group).into_iter())
            .flat_map(|g| self.raid(g, true).map(|h| &h.pos))
            .collect();
        let near = |c: &&Creep| raid.iter().any(|at| c.pos.within(engine::RANGED_RANGE, at));
        self.members_of(e).iter().any(near)
    }

    pub(super) fn facts_of(
        &self,
        e: &Engagement,
        standing: &BTreeSet<String>,
        aging_out: &BTreeSet<String>,
        was: &[Group],
    ) -> DefendFacts {
        let group = self.by_id.get(&e.group).cloned();
        let on_our_ground = group
            .as_ref()
            .is_some_and(|g| defend::on_ground(self, &g.centre));

        DefendFacts {
            time: self.world.time,
            group,
            standing: standing.clone(),
            aging_out: aging_out.clone(),
            in_reach: self.in_reach(e),
            home: self.home(e),
            held: self.homes.contains(&e.room),
            asked: self.spawning.iter().any(|c| c.engagement == e.id),
            on_our_ground,
            returned: defend::returned(self, was, e),
            // A room in sight shows where its groups stand: one gone from it
            // has left, whoever else stands there (a core, or Invaders the
            // raid passed), and they open engagements of their own.
            clear: self.in_sight(e.room).is_some()
                || sightings::clear(self.sightings, self.world.time, e.room),
            armed_in: !sightings::unarmed(self.sightings, self.world.time, e.room),
            remembered: self.remembered.contains_key(&e.room),
        }
    }

    fn home(&self, e: &Engagement) -> bool {
        (e.ours.keys())
            .all(|n| (self.ours.get(n.as_str())).is_some_and(|c| self.homes.contains(&c.pos.room)))
    }
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
        ours: BTreeMap::new(),
        plan: Plan {
            boosts: Vec::new(),
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
        capacity: 0,
        predicted: None,
        actual: Default::default(),
    }
}
