/// What a fight body does this tick (`ARCHITECTURE.md` §6.4): a strike and a
/// wish, read off the plan and what stands in its room. From v1's Emitter and
/// Resolver step functions.
use std::collections::{BTreeMap, BTreeSet};

use crate::core::atlas::{Atlas, Mover, Target};
use crate::core::grid::{self, Footprint};
use crate::core::intents::{CreepAct, Intent};
use crate::core::mv::{Wish, blob, plan};
use crate::core::tick;
use crate::core::tiles::Tile;
use crate::core::vocabulary::{BodyPart, same_part};
use crate::core::world::{Creep, Pos};
use crate::core::{engine, tiles};

pub struct Sight<'a> {
    pub atlas: &'a mut Atlas,
    pub hostiles: &'a BTreeMap<String, Vec<Creep>>,
    pub ramparts: BTreeMap<String, Vec<Pos>>,
    pub homes: Vec<Pos>,
}

/// Above every economy wish in the room's one arbitration: a fight step is
/// not shoved for a haul.
pub const WEIGHT: i32 = 1000;

fn live(kind: BodyPart, c: &Creep) -> i32 {
    c.body
        .iter()
        .filter(|p| p.hits > 0 && same_part(p.kind, kind))
        .count() as i32
}

fn hostiles_in<'a>(sight: &Sight<'a>, room: &str) -> &'a [Creep] {
    sight.hostiles.get(room).map_or(&[], Vec::as_slice)
}

fn ranked<'a>(sight: &Sight<'a>, order: &[String], creep: &Creep) -> Vec<&'a Creep> {
    let near = hostiles_in(sight, &creep.pos.room);

    let mut first: Vec<&Creep> = order
        .iter()
        .filter_map(|id| near.iter().find(|h| &h.id == id))
        .collect();

    let mut rest: Vec<&Creep> = near.iter().filter(|h| !order.contains(&h.id)).collect();
    rest.sort_by(|a, b| a.id.cmp(&b.id));

    first.extend(rest);
    first
}

pub fn strike(sight: &Sight, order: &[String], creep: &Creep) -> Option<CreepAct> {
    let targets = ranked(sight, order, creep);

    let melee = if live(BodyPart::Attack, creep) > 0 {
        targets
            .iter()
            .copied()
            .find(|h| creep.pos.within(engine::MELEE_RANGE, &h.pos))
    } else {
        None
    };

    if let Some(h) = melee {
        return Some(CreepAct::AttackCreep(h.id.clone()));
    }

    if live(BodyPart::RangedAttack, creep) == 0 {
        return None;
    }

    let in_reach: Vec<&Creep> = targets
        .iter()
        .copied()
        .filter(|h| creep.pos.within(engine::RANGED_RANGE, &h.pos))
        .collect();

    let head = in_reach.first()?;

    let volley: i32 = in_reach
        .iter()
        .map(|h| {
            creep
                .pos
                .range(&h.pos)
                .map_or(0, engine::mass_attack_power_at)
        })
        .sum();

    if volley > engine::RANGED_ATTACK_POWER {
        Some(CreepAct::RangedMassAttack)
    } else {
        Some(CreepAct::RangedAttackCreep(head.id.clone()))
    }
}

fn mover_of(creep: &Creep) -> Mover {
    Mover {
        footprint: Footprint::One,
        factor: grid::factor_of(creep),
    }
}

fn target_at(at: &Pos, range: i32) -> Target {
    Target {
        room: at.room.clone(),
        tiles: vec![tiles::of_pos(at)],
        range,
        avoid: BTreeSet::new(),
        max_hops: tick::REACH_HOPS,
    }
}

fn toward(sight: &mut Sight, creep: &Creep, at: &Pos, range: i32) -> Wish {
    plan::toward(
        sight.atlas,
        creep,
        mover_of(creep),
        &target_at(at, range),
        WEIGHT,
    )
}

fn hold(sight: &Sight, creep: &Creep) -> Wish {
    plan::stay(&*sight.atlas, creep, mover_of(creep).factor, WEIGHT)
}

fn aim(sight: &Sight, order: &[String], centre: &Pos, creep: &Creep) -> Pos {
    match ranked(sight, order, creep).into_iter().next() {
        Some(h) => h.pos.clone(),
        None => centre.clone(),
    }
}

pub fn close(
    sight: &mut Sight,
    order: &[String],
    centre: &Pos,
    creep: &Creep,
) -> (Option<CreepAct>, Wish) {
    let at = aim(sight, order, centre, creep);
    (
        strike(sight, order, creep),
        toward(sight, creep, &at, engine::MELEE_RANGE),
    )
}

pub fn pursue(
    sight: &mut Sight,
    order: &[String],
    centre: &Pos,
    creep: &Creep,
) -> (Option<CreepAct>, Wish) {
    close(sight, order, centre, creep)
}

/// How far `kind` of `h` strikes next tick, a step nearer.
fn reach(kind: BodyPart, h: &Creep) -> Option<i32> {
    let range = if kind == BodyPart::RangedAttack {
        engine::RANGED_RANGE
    } else {
        engine::MELEE_RANGE
    };

    (live(kind, h) > 0).then(|| range + live(BodyPart::Move, h).min(1))
}

fn struck(kind: BodyPart, h: &Creep, at: &Pos) -> bool {
    reach(kind, h).is_some_and(|r| at.within(r, &h.pos))
}

fn step_to(creep: &Creep, tile: Tile) -> Wish {
    Wish {
        creep: creep.name.clone(),
        footprint: Footprint::One,
        at: tiles::of_pos(&creep.pos),
        head: tile,
        onward: None,
        tail: Vec::new(),
        working: Vec::new(),
        weight: WEIGHT,
        moves: true,
    }
}

/// v1's kiteStep without its threat maps: off every swing's next reach, else
/// to a shot's range of the head.
pub fn kite(
    sight: &mut Sight,
    order: &[String],
    centre: &Pos,
    creep: &Creep,
) -> (Option<CreepAct>, Wish) {
    let room = creep.pos.room.clone();

    let swings: Vec<&Creep> = hostiles_in(sight, &room)
        .iter()
        .filter(|h| struck(BodyPart::Attack, h, &creep.pos))
        .collect();

    let gap = |tile: Tile| -> i32 {
        let at = tiles::to_pos(&room, tile);

        swings
            .iter()
            .fold(99, |least, h| least.min(h.pos.range(&at).unwrap_or(99)))
    };

    let here = tiles::of_pos(&creep.pos);

    let wish = if swings.is_empty() {
        let at = aim(sight, order, centre, creep);
        toward(sight, creep, &at, engine::RANGED_RANGE)
    } else {
        let steps: Vec<Tile> = tiles::around(here)
            .iter()
            .copied()
            .filter(|t| !tiles::on_edge(*t) && sight.atlas.passable(&room, *t))
            .collect();

        let step = steps
            .iter()
            .copied()
            .min_by_key(|t| (-gap(*t), *t))
            .filter(|t| gap(*t) > gap(here));

        match step {
            Some(t) if plan::can_move(creep, mover_of(creep).factor) => step_to(creep, t),
            _ => hold(sight, creep),
        }
    };

    (strike(sight, order, creep), wish)
}

/// The raid's strikes onto `at` next tick; none outside the raid's room, which
/// costs a raid a tick to follow into.
fn threat(raid: &[Creep], at: &Pos) -> i32 {
    raid.iter()
        .map(|h| {
            let shot = if struck(BodyPart::RangedAttack, h, at) {
                engine::RANGED_ATTACK_POWER * live(BodyPart::RangedAttack, h)
            } else {
                0
            };

            let swing = if struck(BodyPart::Attack, h, at) {
                engine::ATTACK_POWER * live(BodyPart::Attack, h)
            } else {
                0
            };

            shot + swing
        })
        .sum()
}

/// A Doomed body, whose full hits fall within the line, fights on: stepping
/// out cannot save it.
fn rescued(raid: &[Creep], members: &[&Creep], creep: &Creep) -> bool {
    let focus = raid
        .iter()
        .map(|h| {
            engine::RANGED_ATTACK_POWER * live(BodyPart::RangedAttack, h)
                + engine::ATTACK_POWER * live(BodyPart::Attack, h)
        })
        .sum::<i32>()
        - members
            .iter()
            .filter(|m| {
                m.pos.within(engine::MELEE_RANGE, &creep.pos)
                    && (m.name == creep.name || m.hits == m.hits_max)
            })
            .map(|m| engine::HEAL_POWER * live(BodyPart::Heal, m))
            .sum::<i32>();

    let ticks_out = 1 + raid
        .iter()
        .flat_map(|h| [reach(BodyPart::RangedAttack, h), reach(BodyPart::Attack, h)])
        .flatten()
        .fold(0, i32::max);

    focus > 0 && creep.hits < ticks_out * focus && creep.hits_max > ticks_out * focus
}

/// Only while a mate fights on: the gate never empties the line.
pub fn exposed(sight: &Sight, raid_room: &str, members: &[&Creep], creep: &Creep) -> bool {
    let raid = hostiles_in(sight, raid_room);

    rescued(raid, members, creep)
        && members
            .iter()
            .any(|m| m.name != creep.name && !rescued(raid, members, m))
}

/// The tiles out of the raid's next reach, by ring, and the one beside the most mates.
struct Safety<'a> {
    atlas: &'a Atlas,
    room: &'a str,
    raid_room: &'a str,
    raid: &'a [Creep],
    members: &'a [&'a Creep],
    creep: &'a Creep,
}

impl Safety<'_> {
    /// A body crosses an exit by stepping onto it, not by standing on it.
    fn crosses(at: (Tile, Option<Tile>)) -> bool {
        at.1.is_some() && tiles::on_edge(at.0)
    }

    fn safe(&self, at: (Tile, Option<Tile>)) -> bool {
        self.room != self.raid_room
            || Safety::crosses(at)
            || threat(self.raid, &tiles::to_pos(self.room, at.0)) == 0
    }

    fn apart(&self, tile: Tile) -> i32 {
        self.members
            .iter()
            .filter(|m| m.name != self.creep.name && live(BodyPart::Heal, m) > 0)
            .map(|m| m.pos.range(&tiles::to_pos(self.room, tile)).unwrap_or(9))
            .sum()
    }

    fn search(
        &self,
        ring: Vec<(Tile, Option<Tile>)>,
        seen: BTreeSet<Tile>,
        depth: i32,
    ) -> Option<Tile> {
        let safe: Vec<(Tile, Option<Tile>)> =
            ring.iter().copied().filter(|at| self.safe(*at)).collect();

        match safe.as_slice() {
            [] if depth == 0 || ring.is_empty() => None,
            [] => {
                let mut next: Vec<(Tile, Option<Tile>)> = Vec::new();

                for at in ring.iter().copied().filter(|at| !Safety::crosses(*at)) {
                    for n in tiles::around(at.0) {
                        if !seen.contains(n) && self.atlas.passable(self.room, *n) {
                            next.push((*n, at.1.or(Some(*n))));
                        }
                    }
                }

                let mut distinct: Vec<(Tile, Option<Tile>)> = Vec::new();

                for at in next {
                    if !distinct.iter().any(|(t, _)| *t == at.0) {
                        distinct.push(at);
                    }
                }

                let mut seen = seen;

                for (t, _) in &distinct {
                    seen.insert(*t);
                }

                self.search(distinct, seen, depth - 1)
            }
            found => {
                let mut best: Option<Tile> = None;
                let mut best_key = None;

                for at in found {
                    let key = (self.apart(at.0), at.0);

                    if best_key.is_none_or(|b| key < b) {
                        best = at.1;
                        best_key = Some(key);
                    }
                }

                best
            }
        }
    }
}

/// To the nearest tile out of the raid's next reach (an exit hands the body to
/// the next room), beside the most mates that heal it.
pub fn step_back(sight: &mut Sight, raid_room: &str, members: &[&Creep], creep: &Creep) -> Wish {
    let room = creep.pos.room.clone();
    let here = tiles::of_pos(&creep.pos);

    let found = {
        let safety = Safety {
            atlas: &*sight.atlas,
            room: &room,
            raid_room,
            raid: hostiles_in(sight, raid_room),
            members,
            creep,
        };

        safety.search(
            vec![(here, None)],
            BTreeSet::from([here]),
            2 * engine::RANGED_RANGE,
        )
    };

    match found {
        Some(t) if plan::can_move(creep, mover_of(creep).factor) => step_to(creep, t),
        _ => hold(sight, creep),
    }
}

/// To a shot's range of the group, as one.
pub fn blob(sight: &mut Sight, centre: &Pos, members: &[&Creep]) -> Vec<Wish> {
    blob::wishes(
        sight.atlas,
        &target_at(centre, engine::RANGED_RANGE),
        WEIGHT,
        members,
    )
}

/// A medic's seat beside a dismantler: open ground off every tile beside the
/// rival's line, where a body would be struck from a rampart.
fn seat(atlas: &Atlas, line: &[Pos], room: &str, t: Tile) -> bool {
    !tiles::on_edge(t)
        && atlas.passable(room, t)
        && !line
            .iter()
            .any(|p| p.within(engine::MELEE_RANGE, &tiles::to_pos(room, t)))
}

pub fn seats_at(atlas: &Atlas, line: &[Pos], room: &str, work: Tile) -> i32 {
    tiles::around(work)
        .iter()
        .filter(|t| seat(atlas, line, room, **t))
        .count() as i32
}

/// A siege's breach as its members read it this tick.
pub struct Front {
    pub breach: Pos,
    pub wall: String,
    pub line: Vec<Pos>,
    pub towers: Vec<Pos>,
    /// What the towers deal a body working the breach.
    pub strike: i32,
}

fn nearest_tower(front: &Front, at: &Pos) -> i32 {
    front
        .towers
        .iter()
        .fold(99, |least, t| least.min(t.range(at).unwrap_or(99)))
}

/// Assumed, as the scripted rival does: towers aim at the nearest of the set.
pub fn focus_of<'a>(front: &Front, members: &[&'a Creep]) -> Option<&'a Creep> {
    members
        .iter()
        .copied()
        .min_by_key(|m| (nearest_tower(front, &m.pos), &m.name))
}

pub fn siege(
    sight: &mut Sight,
    order: &[String],
    front: &Front,
    members: &[&Creep],
    creep: &Creep,
) -> (Option<CreepAct>, Wish) {
    let room = creep.pos.room.clone();
    let here = tiles::of_pos(&creep.pos);

    let dismantles = |c: &Creep| c.body.iter().any(|p| p.kind.is_work());

    let net = front.strike
        - engine::HEAL_POWER * members.iter().map(|m| live(BodyPart::Heal, m)).sum::<i32>();

    let relieved = |c: &Creep| live(BodyPart::Work, c) == 0 || c.hits < 2 * net;

    let at_breach = |c: &Creep| c.pos.within(engine::MELEE_RANGE, &front.breach);

    let dismantlers: Vec<&Creep> = members.iter().copied().filter(|c| dismantles(c)).collect();

    if dismantles(creep) {
        if relieved(creep)
            && dismantlers
                .iter()
                .any(|d| d.name != creep.name && !relieved(d))
        {
            let off: Vec<Tile> = tiles::around(here)
                .iter()
                .copied()
                .filter(|t| {
                    !tiles::on_edge(*t)
                        && sight.atlas.passable(&room, *t)
                        && !tiles::to_pos(&room, *t).within(engine::MELEE_RANGE, &front.breach)
                })
                .collect();

            let off = off
                .iter()
                .copied()
                .min_by_key(|t| (-(nearest_tower(front, &tiles::to_pos(&room, *t))), *t));

            (
                None,
                match off {
                    Some(t) if plan::can_move(creep, mover_of(creep).factor) => step_to(creep, t),
                    _ => hold(sight, creep),
                },
            )
        } else if at_breach(creep) {
            (
                Some(CreepAct::Dismantle(front.wall.clone())),
                hold(sight, creep),
            )
        } else {
            (
                None,
                toward(sight, creep, &front.breach, engine::MELEE_RANGE),
            )
        }
    } else {
        let at_breach_room: Vec<&Creep> = dismantlers
            .iter()
            .copied()
            .filter(|d| d.pos.room == front.breach.room)
            .collect();

        let anchor = focus_of(front, &at_breach_room).or_else(|| {
            dismantlers
                .iter()
                .copied()
                .min_by_key(|d| (d.pos.range(&front.breach), &d.name))
        });

        let wish = match anchor {
            None => toward(sight, creep, &front.breach, engine::RANGED_RANGE),
            Some(d) => {
                let mut seats: Vec<Tile> = tiles::around(tiles::of_pos(&d.pos))
                    .iter()
                    .copied()
                    .filter(|t| seat(&*sight.atlas, &front.line, &d.pos.room, *t))
                    .collect();
                seats.sort_unstable();

                let mut medics: Vec<String> = members
                    .iter()
                    .filter(|m| !dismantles(m))
                    .map(|m| m.name.clone())
                    .collect();
                medics.sort();

                let sit = medics
                    .iter()
                    .position(|n| n == &creep.name)
                    .and_then(|at| seats.get(at).copied());

                match sit {
                    Some(t) if d.pos.room == room && t == here => hold(sight, creep),
                    Some(t) => toward(sight, creep, &tiles::to_pos(&d.pos.room, t), 0),
                    None => toward(sight, creep, &d.pos, engine::MELEE_RANGE),
                }
            }
        };

        (strike(sight, order, creep), wish)
    }
}

pub fn hold_fast(sight: &mut Sight, order: &[String], creep: &Creep) -> (Option<CreepAct>, Wish) {
    (strike(sight, order, creep), hold(sight, creep))
}

/// Onto the nearest of our ramparts, striking what comes; where none stands,
/// where it stands.
pub fn rampart(sight: &mut Sight, order: &[String], creep: &Creep) -> (Option<CreepAct>, Wish) {
    let ramparts: Vec<Pos> = sight
        .ramparts
        .get(&creep.pos.room)
        .cloned()
        .unwrap_or_default();

    let wish = if ramparts.contains(&creep.pos) {
        hold(sight, creep)
    } else {
        let nearest = ramparts
            .iter()
            .min_by_key(|p| (creep.pos.range(p).unwrap_or(99), p.x, p.y))
            .cloned();

        match nearest {
            Some(p) => toward(sight, creep, &p, 0),
            None => hold(sight, creep),
        }
    };

    (strike(sight, order, creep), wish)
}

pub fn retreat(sight: &mut Sight, creep: &Creep) -> (Option<CreepAct>, Wish) {
    let room = creep.pos.room.clone();
    let here: Vec<Pos> = sight
        .homes
        .iter()
        .filter(|p| p.room == room)
        .cloned()
        .collect();

    let home: Option<Pos> = if here.is_empty() {
        let tree = sight.atlas.tree(tick::REACH_HOPS, &room);

        let routed: Vec<(i32, Pos)> = sight
            .homes
            .iter()
            .filter_map(|p| {
                tree.route(&room, &p.room)
                    .map(|rooms| (rooms.len() as i32, p.clone()))
            })
            .collect();

        routed
            .into_iter()
            .min_by(|(a_hops, a), (b_hops, b)| {
                (a_hops, &a.room, a.x, a.y).cmp(&(b_hops, &b.room, b.x, b.y))
            })
            .map(|(_, p)| p)
    } else {
        here.into_iter().min_by_key(|p| (p.x, p.y))
    };

    (
        None,
        match home {
            Some(p) => toward(sight, creep, &p, 2),
            None => hold(sight, creep),
        },
    )
}

/// v1's heal reflex: a hurt healer heals itself, else the deepest wound beside
/// it, else one within three at the ranged rate; each heal counts off what it
/// closes. The engine keeps one of a swing and a heal, and one of a shot and a
/// ranged heal: a swinging body does not heal, a shooting one heals beside it.
/// A heal lands in the tick's sum with the strikes, so what is `incoming` is owed.
pub fn heals(
    healers: &[&Creep],
    patients: &[Creep],
    incoming: &BTreeMap<String, i32>,
    swinging: &BTreeSet<String>,
    shooting: &BTreeSet<String>,
) -> Vec<Intent> {
    let mut owed: BTreeMap<String, i32> = patients
        .iter()
        .map(|p| {
            (
                p.name.clone(),
                p.hits_max - p.hits + incoming.get(&p.name).copied().unwrap_or(0),
            )
        })
        .collect();

    let mut acts: Vec<Intent> = Vec::new();

    let mut healers = healers.to_vec();
    healers.sort_by(|a, b| a.name.cmp(&b.name));
    for healer in healers {
        let parts = live(BodyPart::Heal, healer);

        if parts > 0 && !swinging.contains(&healer.name) {
            let patient: Option<(&Creep, i32)> = if owed.get(&healer.name).copied().unwrap_or(0) > 0
            {
                Some((healer, 0))
            } else {
                let near = if shooting.contains(&healer.name) {
                    engine::MELEE_RANGE
                } else {
                    engine::RANGED_RANGE
                };

                let hurt: Vec<(&Creep, i32)> = patients
                    .iter()
                    .filter_map(|p| match healer.pos.range(&p.pos) {
                        Some(r) if r <= near && owed.get(&p.name).copied().unwrap_or(0) > 0 => {
                            Some((p, r))
                        }
                        _ => None,
                    })
                    .collect();

                hurt.into_iter().min_by_key(|(p, r)| {
                    (
                        *r > engine::MELEE_RANGE,
                        -owed.get(&p.name).copied().unwrap_or(0),
                        p.name.clone(),
                    )
                })
            };

            match patient {
                Some((p, r)) if r <= engine::MELEE_RANGE => {
                    if let Some(o) = owed.get_mut(&p.name) {
                        *o -= parts * engine::HEAL_POWER;
                    }

                    acts.push(Intent::Creep(
                        healer.name.clone(),
                        CreepAct::HealCreep(p.id.clone()),
                    ));
                }
                Some((p, _)) => {
                    if let Some(o) = owed.get_mut(&p.name) {
                        *o -= parts * engine::RANGED_HEAL_POWER;
                    }

                    acts.push(Intent::Creep(
                        healer.name.clone(),
                        CreepAct::RangedHealCreep(p.id.clone()),
                    ));
                }
                None => (),
            }
        }
    }

    acts
}
