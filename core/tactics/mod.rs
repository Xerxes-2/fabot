/// What a fight body does this tick (`ARCHITECTURE.md` §6.4): a step read off
/// the plan and what stands in its room, and the strike reflex. From v1's
/// Emitter and Resolver step functions. Also which tactic a kind picks (§6.3).
use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};

use crate::atlas::{Atlas, Mover, Target};
use crate::declarations::{self, TacticRule};
use crate::grid::{self, Footprint};
use crate::intents::CreepAct;
use crate::mv::{Wish, blob, plan};
use crate::tiles::Tile;
use crate::vocabulary::Part;
use crate::world::{Creep, Id, Pos, RoomName};
use crate::{engine, tiles};

/// A phase's own tactic (pursue, retreat) is the phase's; this is what a kind
/// commits to, a price per candidate (§6.3).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Tactic {
    Kite,
    Close,
    /// On our ramparts: a home is a post the raid comes to.
    Hold,
    /// A blob is a set that walks in as one, then fights by the tactic it names.
    BlobKite,
    #[serde(rename = "blob")]
    BlobClose,
}

pub fn name(tactic: Tactic) -> &'static str {
    match tactic {
        Tactic::Kite => "kite",
        Tactic::Close => "close",
        Tactic::Hold => "hold",
        Tactic::BlobKite => "blob-kite",
        Tactic::BlobClose => "blob",
    }
}

pub fn by_name(name: &str) -> Option<Tactic> {
    use Tactic::*;
    [Kite, Close, Hold, BlobKite, BlobClose]
        .into_iter()
        .find(|t| self::name(*t) == name)
}

#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Ruling {
    pub forbid: BTreeSet<Tactic>,
    pub pin: Option<Tactic>,
}

pub fn ruling(rules: &[TacticRule], room: RoomName, rivals: &BTreeSet<&str>) -> Ruling {
    let applies = |r: &TacticRule| {
        r.room.is_none_or(|x| x == room) && r.rival.as_deref().is_none_or(|x| rivals.contains(&x))
    };

    let mut out = Ruling::default();

    for rule in rules.iter().filter(|r| applies(r)) {
        out.forbid
            .extend(rule.forbid.iter().filter_map(|n| by_name(n)));

        out.pin = rule.pin.as_deref().and_then(by_name).or(out.pin);
    }

    out
}

pub fn checked(rules: Vec<TacticRule>) -> (Vec<TacticRule>, Vec<String>) {
    let joins = |a: &TacticRule, b: &TacticRule| {
        fn same<T: PartialEq>(x: Option<T>, y: Option<T>) -> bool {
            x.zip(y).is_none_or(|(x, y)| x == y)
        }
        same(a.rival.as_ref(), b.rival.as_ref()) && same(a.room, b.room)
    };

    let mut kept = Vec::new();
    let mut refused = Vec::new();

    for rule in &rules {
        let unknown = rule
            .pin
            .iter()
            .chain(&rule.forbid)
            .find(|name| by_name(name).is_none());

        let pin = rule.pin.as_deref().and_then(by_name);

        let contradicts = rules
            .iter()
            .filter(|other| joins(rule, other))
            .flat_map(|other| other.forbid.iter().filter_map(|n| by_name(n)))
            .any(|forbidden| Some(forbidden) == pin);

        match (unknown, pin) {
            (Some(name), _) => refused.push(format!("tactics {name}")),
            (None, Some(pin)) if contradicts => {
                refused.push(format!("tactics pin {}", name(pin)));
            }
            _ => kept.push(rule.clone()),
        }
    }

    (kept, refused)
}

/// One candidate as its kind's price finds it (§6.3): the bodies it fields and
/// what it wins against what it spends, both in energy.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Offer {
    pub tactic: Tactic,
    pub bodies: i32,
    pub value: i32,
    pub cost: i32,
}

impl Offer {
    pub fn net(&self) -> i32 {
        self.value - self.cost
    }
}

/// The offer that pays most; ties to the fewest bodies, then to the kind's
/// declared candidate order.
pub fn best(offers: &[Offer]) -> Option<&Offer> {
    let ranked = |o: &Offer| (o.net(), std::cmp::Reverse(o.bodies));
    offers
        .iter()
        .reduce(|b, o| if ranked(o) > ranked(b) { o } else { b })
}

/// A pinned tactic wins where it is still offered, else the best pays.
pub fn select(offers: &[Offer], pin: Option<Tactic>) -> Option<&Offer> {
    pin.and_then(|pinned| offers.iter().find(|o| o.tactic == pinned))
        .or_else(|| best(offers))
}

pub struct Sight<'a> {
    pub atlas: &'a mut Atlas,
    pub hostiles: &'a BTreeMap<RoomName, Vec<Creep>>,
    /// The hostiles a stance lets the reflex strike, and what a volley spares.
    pub struck: BTreeSet<Id>,
    pub spared: BTreeMap<RoomName, Vec<Pos>>,
    pub ramparts: BTreeMap<RoomName, Vec<Pos>>,
    pub musters: Vec<Pos>,
    pub avoid: &'a BTreeSet<RoomName>,
}

/// Above every economy wish in the room's one arbitration: a fight step is
/// not shoved for a haul.
pub const WEIGHT: i32 = 1000;

fn hostiles_in<'a>(sight: &Sight<'a>, room: RoomName) -> &'a [Creep] {
    sight.hostiles.get(&room).map_or(&[], Vec::as_slice)
}

fn ranked<'a>(sight: &Sight<'a>, order: &[Id], creep: &Creep) -> Vec<&'a Creep> {
    let near = hostiles_in(sight, creep.pos.room)
        .iter()
        .filter(|h| sight.struck.contains(&h.id));

    let mut first: Vec<&Creep> = order
        .iter()
        .filter_map(|id| near.clone().find(|h| &h.id == id))
        .collect();

    let mut rest: Vec<&Creep> = near.filter(|h| !order.contains(&h.id)).collect();
    rest.sort_by_key(|h| {
        (
            h.live(Part::Attack) + h.live(Part::RangedAttack) == 0,
            &h.id,
        )
    });

    first.extend(rest);
    first
}

/// Every body that strikes, in any phase and engaged or not: a swing and a
/// shot at the first in the kill order in reach, a volley where it lands more.
pub fn strike(sight: &Sight, order: &[Id], creep: &Creep, armed: bool) -> Vec<CreepAct> {
    let targets: Vec<&Creep> = ranked(sight, order, creep)
        .into_iter()
        .filter(|h| !armed || h.live(Part::Attack) > 0 || h.live(Part::RangedAttack) > 0)
        .collect();
    let within = |r: i32| targets.iter().filter(move |h| creep.pos.within(r, &h.pos));

    let melee = within(engine::MELEE_RANGE)
        .next()
        .filter(|_| creep.live(Part::Attack) > 0)
        .map(|h| CreepAct::AttackCreep(h.id));

    let volley: i32 = within(engine::RANGED_RANGE)
        .filter_map(|h| creep.pos.range(&h.pos))
        .map(engine::mass_attack_power_at)
        .sum();
    let spares = sight
        .spared
        .get(&creep.pos.room)
        .is_some_and(|s| s.iter().any(|p| creep.pos.within(engine::RANGED_RANGE, p)));

    let ranged = match within(engine::RANGED_RANGE).next() {
        _ if creep.live(Part::RangedAttack) == 0 => None,
        Some(_) if volley > engine::RANGED_ATTACK_POWER && !spares => {
            Some(CreepAct::RangedMassAttack)
        }
        head => head.map(|h| CreepAct::RangedAttackCreep(h.id)),
    };

    melee.into_iter().chain(ranged).collect()
}

fn mover_of(creep: &Creep) -> Mover {
    Mover {
        footprint: Footprint::One,
        factor: grid::factor_of(creep),
    }
}

fn target_at(sight: &Sight, at: &Pos, range: i32) -> Target {
    Target {
        room: at.room,
        tiles: vec![tiles::of_pos(at)],
        range,
        avoid: sight.avoid.clone(),
        max_hops: declarations::SIEGE_HOPS,
    }
}

pub fn toward(sight: &mut Sight, creep: &Creep, at: &Pos, range: i32) -> Wish {
    plan::toward(
        sight.atlas,
        creep,
        mover_of(creep),
        &target_at(sight, at, range),
        WEIGHT,
    )
}

pub fn hold(sight: &Sight, creep: &Creep) -> Wish {
    plan::stay(&*sight.atlas, creep, mover_of(creep).factor, WEIGHT)
}

fn aim(sight: &Sight, order: &[Id], centre: &Pos, creep: &Creep) -> Pos {
    ranked(sight, order, creep)
        .first()
        .map_or(*centre, |h| h.pos)
}

pub fn close(sight: &mut Sight, order: &[Id], centre: &Pos, creep: &Creep) -> Wish {
    let at = aim(sight, order, centre, creep);
    toward(sight, creep, &at, engine::MELEE_RANGE)
}

fn reach_next_tick(kind: Part, h: &Creep) -> Option<i32> {
    let ranged = kind == Part::RangedAttack;
    let range = if ranged {
        engine::RANGED_RANGE
    } else {
        engine::MELEE_RANGE
    };
    (h.live(kind) > 0).then(|| range + h.live(Part::Move).min(1))
}

fn struck(kind: Part, h: &Creep, at: &Pos) -> bool {
    reach_next_tick(kind, h).is_some_and(|r| at.within(r, &h.pos))
}

/// A tired body's step waits where it stands: the engine refuses it.
fn step_to(creep: &Creep, tile: Tile) -> Wish {
    let moves = plan::can_move(creep, mover_of(creep).factor);
    let at = tiles::of_pos(&creep.pos);
    Wish {
        creep: creep.name.clone(),
        footprint: Footprint::One,
        at,
        head: if moves { tile } else { at },
        onward: None,
        tail: Vec::new(),
        working: Vec::new(),
        weight: WEIGHT,
        moves,
    }
}

/// v1's kiteStep without its threat maps: off every swing's next reach, else
/// to a shot's range of `at`, or of the mark the kill order puts first.
pub fn kite(
    sight: &mut Sight,
    at: Option<&Pos>,
    order: &[Id],
    centre: &Pos,
    creep: &Creep,
) -> Wish {
    let room = creep.pos.room;

    let swings: Vec<&Creep> = hostiles_in(sight, room)
        .iter()
        .filter(|h| struck(Part::Attack, h, &creep.pos))
        .collect();

    let gap = |tile: Tile| -> i32 {
        let at = tiles::to_pos(room, tile);

        swings
            .iter()
            .fold(99, |least, h| least.min(h.pos.range(&at).unwrap_or(99)))
    };

    let here = tiles::of_pos(&creep.pos);

    if swings.is_empty() {
        let at = at
            .copied()
            .unwrap_or_else(|| aim(sight, order, centre, creep));
        toward(sight, creep, &at, engine::RANGED_RANGE)
    } else {
        let step = (tiles::around(here))
            .iter()
            .copied()
            .filter(|t| !tiles::on_edge(*t) && sight.atlas.passable(room, *t))
            .min_by_key(|t| (-gap(*t), *t))
            .filter(|t| gap(*t) > gap(here));

        step.map_or_else(|| hold(sight, creep), |t| step_to(creep, t))
    }
}

/// The raid's strikes onto `at` next tick; none outside the raid's room, which
/// costs a raid a tick to follow into.
fn threat(raid: &[Creep], at: &Pos) -> i32 {
    raid.iter()
        .map(|h| {
            let shot = if struck(Part::RangedAttack, h, at) {
                engine::RANGED_ATTACK_POWER * h.live(Part::RangedAttack)
            } else {
                0
            };

            let swing = if struck(Part::Attack, h, at) {
                engine::ATTACK_POWER * h.live(Part::Attack)
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
            engine::RANGED_ATTACK_POWER * h.live(Part::RangedAttack)
                + engine::ATTACK_POWER * h.live(Part::Attack)
        })
        .sum::<i32>()
        - members
            .iter()
            .filter(|m| {
                m.pos.within(engine::MELEE_RANGE, &creep.pos)
                    && (m.name == creep.name || m.hits == m.hits_max)
            })
            .map(|m| engine::HEAL_POWER * m.live(Part::Heal))
            .sum::<i32>();

    let ticks_out = 1 + raid
        .iter()
        .flat_map(|h| {
            [
                reach_next_tick(Part::RangedAttack, h),
                reach_next_tick(Part::Attack, h),
            ]
        })
        .flatten()
        .fold(0, i32::max);

    focus > 0 && creep.hits < ticks_out * focus && creep.hits_max > ticks_out * focus
}

/// Only while a mate fights on: the gate never empties the line.
pub fn exposed(sight: &Sight, raid_room: RoomName, members: &[&Creep], creep: &Creep) -> bool {
    let raid = hostiles_in(sight, raid_room);

    rescued(raid, members, creep)
        && members
            .iter()
            .any(|m| m.name != creep.name && !rescued(raid, members, m))
}

/// The tiles out of the raid's next reach, by ring, and the one beside the most mates.
struct Safety<'a> {
    atlas: &'a Atlas,
    room: RoomName,
    raid_room: RoomName,
    raid: &'a [Creep],
    members: &'a [&'a Creep],
    creep: &'a Creep,
}

impl Safety<'_> {
    fn safe(&self, at: (Tile, Option<Tile>)) -> bool {
        self.room != self.raid_room
            || at.1.is_some() && tiles::on_edge(at.0)
            || threat(self.raid, &tiles::to_pos(self.room, at.0)) == 0
    }

    fn apart(&self, tile: Tile) -> i32 {
        self.members
            .iter()
            .filter(|m| m.name != self.creep.name && m.live(Part::Heal) > 0)
            .map(|m| m.pos.range(&tiles::to_pos(self.room, tile)).unwrap_or(9))
            .sum()
    }

    fn search(
        &self,
        ring: &[(Tile, Option<Tile>)],
        mut seen: BTreeSet<Tile>,
        depth: i32,
    ) -> Option<Tile> {
        if let Some(at) = (ring.iter())
            .filter(|at| self.safe(**at))
            .min_by_key(|at| (self.apart(at.0), at.0))
        {
            return at.1;
        }
        if depth == 0 || ring.is_empty() {
            return None;
        }
        let mut next: Vec<(Tile, Option<Tile>)> = Vec::new();
        let open = |at: &&(Tile, Option<Tile>)| at.1.is_none() || !tiles::on_edge(at.0);
        for at in ring.iter().filter(open) {
            for n in tiles::around(at.0) {
                if !seen.contains(n) && self.atlas.passable(self.room, *n) {
                    next.push((*n, at.1.or(Some(*n))));
                }
            }
        }
        next.retain(|(t, _)| seen.insert(*t));
        self.search(&next, seen, depth - 1)
    }
}

/// To the nearest tile out of the raid's next reach (an exit hands the body to
/// the next room), beside the most mates that heal it.
pub fn step_back(
    sight: &mut Sight,
    raid_room: RoomName,
    members: &[&Creep],
    creep: &Creep,
) -> Wish {
    let room = creep.pos.room;
    let here = tiles::of_pos(&creep.pos);

    let found = {
        let safety = Safety {
            atlas: &*sight.atlas,
            room,
            raid_room,
            raid: hostiles_in(sight, raid_room),
            members,
            creep,
        };

        safety.search(
            &[(here, None)],
            BTreeSet::from([here]),
            2 * engine::RANGED_RANGE,
        )
    };

    found.map_or_else(|| hold(sight, creep), |t| step_to(creep, t))
}

pub fn blob(
    sight: &mut Sight,
    centre: &Pos,
    range: i32,
    members: &[&Creep],
    fortified: &BTreeSet<RoomName>,
) -> Vec<Wish> {
    blob::wishes(
        sight.atlas,
        &target_at(sight, centre, range),
        WEIGHT,
        members,
        fortified,
    )
}

/// A medic's seat beside a dismantler: open ground off every tile beside the
/// rival's line, where a body would be struck from a rampart.
fn seat(atlas: &Atlas, line: &[Pos], room: RoomName, t: Tile) -> bool {
    !tiles::on_edge(t)
        && atlas.passable(room, t)
        && !line
            .iter()
            .any(|p| p.within(engine::MELEE_RANGE, &tiles::to_pos(room, t)))
}

pub fn seats_at(atlas: &Atlas, line: &[Pos], room: RoomName, work: Tile) -> i32 {
    tiles::around(work)
        .iter()
        .filter(|t| seat(atlas, line, room, **t))
        .count() as i32
}

pub struct Front {
    pub breach: Pos,
    /// None where the breach is his controller: tapped, never struck.
    pub wall: Option<Id>,
    pub line: Vec<Pos>,
    pub towers: Vec<Pos>,
    pub struck: Option<Id>,
    pub breach_strike: i32,
    /// The rival's sources outside its ramparts, where pickets stand, and
    /// how many each holds against the towers.
    pub posts: Vec<(Pos, usize)>,
}

pub fn picket_of(creep: &Creep) -> bool {
    creep.body.iter().any(|p| p.kind == Part::RangedAttack)
}

pub fn claims(creep: &Creep) -> bool {
    creep.body.iter().any(|p| p.kind == Part::Claim)
}

/// Seated beside its post, off the rival's line: there the rival harvests.
pub fn picket(sight: &mut Sight, front: &Front, members: &[&Creep], creep: &Creep) -> Wish {
    // By slot, which a successor keeps, so a landing never reshuffles posts.
    let slot = |c: &Creep| {
        c.name
            .rsplit('-')
            .next()
            .and_then(|s| s.parse::<i32>().ok())
    };
    let mut pickets: Vec<&Creep> = members.iter().copied().filter(|c| picket_of(c)).collect();
    pickets.sort_by_cached_key(|&c| (slot(c), c.name.as_str()));

    let at = pickets
        .iter()
        .position(|c| c.name == creep.name)
        .unwrap_or(0);
    let mut before = 0;
    let Some((post, seat_at)) = front.posts.iter().find_map(|(post, n)| {
        let here = (at >= before && at < before + n).then(|| (post, at - before));
        before += n;
        here
    }) else {
        return hold(sight, creep);
    };

    let mut seats: Vec<Tile> = tiles::around(tiles::of_pos(post))
        .iter()
        .copied()
        .filter(|t| seat(&*sight.atlas, &front.line, post.room, *t))
        .collect();
    seats.sort_unstable();

    match seats.get(seat_at) {
        Some(t) if creep.pos == tiles::to_pos(post.room, *t) => hold(sight, creep),
        Some(t) => toward(sight, creep, &tiles::to_pos(post.room, *t), 0),
        None => toward(sight, creep, post, engine::MELEE_RANGE),
    }
}

fn nearest_tower(front: &Front, at: &Pos) -> i32 {
    front
        .towers
        .iter()
        .fold(99, |least, t| least.min(t.range(at).unwrap_or(99)))
}

/// Seen, not assumed: a rival's towers may focus the softest, not the nearest.
pub fn focus_of<'a>(front: &Front, members: &[&'a Creep]) -> Option<&'a Creep> {
    let spared = |m: &Creep| front.struck.as_ref() != Some(&m.id);
    (members.iter().copied()).min_by_key(|m| (spared(m), nearest_tower(front, &m.pos), &m.name))
}

/// The edge a drain bounces through (`ARCHITECTURE.md` §6.2): `bounce`'s
/// mirror is his.
pub struct Gate {
    pub room: RoomName,
    pub from: RoomName,
    pub bounce: Tile,
    pub rest: Tile,
}
pub fn drain(sight: &mut Sight, gate: &Gate, creep: &Creep) -> Wish {
    let at = tiles::of_pos(&creep.pos);
    let there = |t: Tile| tiles::to_pos(gate.from, t);
    let strikes = creep.live(Part::Attack) > 0 || creep.live(Part::RangedAttack) > 0;

    // The set waits beside his room, off the pair's tiles: the entry is the plan's.
    if strikes || creep.live(Part::Work) > 0 {
        let wait = there(tiles::index(engine::ROOM_SIDE / 2, engine::ROOM_SIDE / 2));
        return toward(sight, creep, &wait, engine::ROOM_SIDE / 2);
    }

    if creep.live(Part::Heal) > 0 {
        return match seat_of(&*sight.atlas, gate) {
            Some(seat) if at == seat && creep.pos.room == gate.from => step_to(creep, at),
            Some(seat) => toward(sight, creep, &there(seat), 0),
            None => step_to(creep, at),
        };
    }

    if creep.pos.room == gate.room {
        return step_to(creep, at);
    }
    if creep.hits < engine::TOWER_POWER_ATTACK / 2 || at != gate.rest {
        return toward(sight, creep, &there(gate.rest), 0);
    }
    step_to(creep, gate.bounce)
}

fn seat_of(atlas: &Atlas, gate: &Gate) -> Option<Tile> {
    (tiles::around(gate.rest).iter())
        .filter(|t| !tiles::on_edge(**t) && tiles::range(**t, gate.bounce) <= 1)
        .filter(|t| atlas.passable(gate.from, **t))
        .min()
        .copied()
}

pub fn siege(
    sight: &mut Sight,
    front: &Front,
    members: &[&Creep],
    creep: &Creep,
) -> (Option<CreepAct>, Wish) {
    let room = creep.pos.room;
    let here = tiles::of_pos(&creep.pos);

    let dismantles = |c: &Creep| c.body.iter().any(|p| p.kind == Part::Work);

    let net = front.breach_strike
        - engine::HEAL_POWER * members.iter().map(|m| m.live(Part::Heal)).sum::<i32>();

    let relieved = |c: &Creep| c.live(Part::Work) == 0 || c.hits < 2 * net;

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
                        && sight.atlas.passable(room, *t)
                        && !tiles::to_pos(room, *t).within(engine::MELEE_RANGE, &front.breach)
                })
                .collect();

            let off = off
                .iter()
                .copied()
                .min_by_key(|t| (-(nearest_tower(front, &tiles::to_pos(room, *t))), *t));

            (
                None,
                off.map_or_else(|| hold(sight, creep), |t| step_to(creep, t)),
            )
        } else if at_breach(creep) {
            (
                front.wall.map(CreepAct::Dismantle),
                toward(sight, creep, &front.breach, engine::MELEE_RANGE),
            )
        } else {
            // The walk sees no bodies: a tile another dismantler holds reads as the way in.
            let tiles: Vec<Tile> = tiles::around(tiles::of_pos(&front.breach))
                .iter()
                .copied()
                .filter(|t| {
                    let at = tiles::to_pos(front.breach.room, *t);
                    !tiles::on_edge(*t)
                        && sight.atlas.passable(at.room, *t)
                        && dismantlers.iter().all(|d| d.pos != at)
                })
                .collect();
            let target = Target {
                tiles,
                ..target_at(sight, &front.breach, 0)
            };
            (
                None,
                plan::toward(sight.atlas, creep, mover_of(creep), &target, WEIGHT),
            )
        }
    } else {
        // Beside the body his towers strike: there the plan pooled the heal.
        let struck = |m: &Creep| m.name != creep.name && front.struck.as_ref() == Some(&m.id);
        let focus: Vec<&Creep> = (members.iter().copied())
            .filter(|m| struck(m) || (dismantles(m) && m.pos.room == front.breach.room))
            .collect();
        let anchor = focus_of(front, &focus).or_else(|| {
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
                    .filter(|t| seat(&*sight.atlas, &front.line, d.pos.room, *t))
                    .collect();
                seats.sort_unstable();

                let mut medics: Vec<String> = members
                    .iter()
                    .filter(|m| !dismantles(m) && m.name != d.name)
                    .map(|m| m.name.clone())
                    .collect();
                medics.sort();

                let sit = medics
                    .iter()
                    .position(|n| n == &creep.name)
                    .and_then(|at| seats.get(at).copied());

                match sit {
                    Some(t) if d.pos.room == room && t == here => hold(sight, creep),
                    Some(t) => toward(sight, creep, &tiles::to_pos(d.pos.room, t), 0),
                    None => toward(sight, creep, &d.pos, engine::MELEE_RANGE),
                }
            }
        };

        (None, wish)
    }
}

pub fn rampart(sight: &mut Sight, creep: &Creep) -> Wish {
    let ramparts: Vec<Pos> = sight
        .ramparts
        .get(&creep.pos.room)
        .cloned()
        .unwrap_or_default();

    if ramparts.contains(&creep.pos) {
        hold(sight, creep)
    } else {
        let nearest = ramparts
            .iter()
            .min_by_key(|p| (creep.pos.range(p).unwrap_or(99), p.x, p.y))
            .copied();

        match nearest {
            Some(p) => toward(sight, creep, &p, 0),
            None => hold(sight, creep),
        }
    }
}

pub fn retreat(sight: &mut Sight, creep: &Creep) -> Wish {
    let room = creep.pos.room;
    let here = (sight.musters.iter()).filter(|p| p.room == room);
    let home = here.min_by_key(|p| (p.x, p.y)).copied().or_else(|| {
        let onto: Vec<RoomName> = sight.musters.iter().map(|p| p.room).collect();
        let tree = (sight.atlas).tree_to(&BTreeSet::new(), declarations::SIEGE_HOPS, room, &onto);
        (sight.musters.iter())
            .filter_map(|p| tree.crossings(room, p.room).map(|n| (n, *p)))
            .min_by_key(|(hops, p)| (*hops, p.room, p.x, p.y))
            .map(|(_, p)| p)
    });
    let Some(home) = home else {
        return hold(sight, creep);
    };
    // Waits as a body with no work, so any walker shoves it.
    if !creep.pos.within(declarations::MUSTER_RANGE, &home) {
        return toward(sight, creep, &home, declarations::MUSTER_RANGE);
    }
    hold(sight, creep)
}

/// v1's heal reflex: a hurt healer heals itself, else the deepest wound beside
/// it, else one within three at the ranged rate; each heal counts off what it
/// closes. The engine keeps one of a swing and a heal, and one of a shot and a
/// ranged heal: a swinging body does not heal, a shooting one heals beside it.
/// A heal lands in the tick's sum with the strikes, so what is `incoming` is owed.
/// Each healer heals the patient in its reach nearest death after what strikes
/// it, then owed most, and that heal comes off what it is owed.
pub fn allot<'p>(
    healers: &[&Creep],
    patients: &[&'p Creep],
    owed: &mut BTreeMap<String, i32>,
    shooting: &dyn Fn(&str) -> bool,
) -> Vec<(String, &'p Creep, bool)> {
    let mut healers = healers.to_vec();
    healers.sort_by(|a, b| a.name.cmp(&b.name));
    let mut out = Vec::new();
    for healer in healers {
        let parts = healer.live(Part::Heal);
        let near = if shooting(&healer.name) {
            engine::MELEE_RANGE
        } else {
            engine::RANGED_RANGE
        };
        let patient = patients
            .iter()
            .copied()
            .filter(|p| owed.get(&p.name).copied().unwrap_or(0) > 0)
            .filter_map(|p| {
                healer
                    .pos
                    .range(&p.pos)
                    .filter(|r| *r <= near)
                    .map(|r| (p, r))
            })
            .min_by_key(|(p, r)| {
                let left = p.hits_max - owed[&p.name];
                (
                    *r > engine::MELEE_RANGE,
                    left,
                    -owed[&p.name],
                    p.name.clone(),
                )
            });
        if let (true, Some((p, r))) = (parts > 0, patient) {
            let beside = r <= engine::MELEE_RANGE;
            let power = if beside {
                engine::HEAL_POWER
            } else {
                engine::RANGED_HEAL_POWER
            };
            *owed.entry(p.name.clone()).or_insert(0) -= parts * power;
            out.push((healer.name.clone(), p, beside));
        }
    }
    out
}

/// Each tile a body may step to and the damage netted there past the heal
/// `allot` lands on it, its mates and it owed what strikes where they stand.
pub fn nets(
    atlas: &Atlas,
    threat: &tiles::Table<i32>,
    mates: &[&Creep],
    c: &Creep,
) -> Vec<(Tile, i32)> {
    let here = tiles::of_pos(&c.pos);
    let healers: Vec<&Creep> = mates.iter().copied().chain([c]).collect();
    std::iter::once(here)
        .chain(
            tiles::around(here)
                .iter()
                .copied()
                .filter(|t| atlas.passable(c.pos.room, *t)),
        )
        .map(|t| {
            let moved = Creep {
                pos: tiles::to_pos(c.pos.room, t),
                ..c.clone()
            };
            let patients: Vec<&Creep> = mates.iter().copied().chain([&moved]).collect();
            let mut owed: BTreeMap<String, i32> = patients
                .iter()
                .map(|p| {
                    (
                        p.name.clone(),
                        p.hits_max - p.hits + threat[tiles::of_pos(&p.pos)],
                    )
                })
                .collect();
            let heal: i32 = allot(&healers, &patients, &mut owed, &|_| true)
                .iter()
                .filter(|(_, p, _)| p.name == c.name)
                .filter_map(|(h, _, _)| healers.iter().find(|m| m.name == *h))
                .map(|h| engine::HEAL_POWER * h.live(Part::Heal))
                .sum();
            (t, threat[t] - heal)
        })
        .collect()
}

/// No tile a body may step to is one it would stand three ticks on: an event
/// for its engagement, which a tactic cannot answer.
pub fn cornered(atlas: &Atlas, threat: &tiles::Table<i32>, mates: &[&Creep], c: &Creep) -> bool {
    nets(atlas, threat, mates, c)
        .iter()
        .all(|(t, net)| tiles::on_edge(*t) || net * 3 >= c.hits)
}

pub fn heals(
    healers: &[&Creep],
    patients: &[Creep],
    incoming: &BTreeMap<String, i32>,
    swinging: &BTreeSet<String>,
    shooting: &BTreeSet<String>,
) -> Vec<(String, CreepAct)> {
    let mut owed: BTreeMap<String, i32> = patients
        .iter()
        .map(|p| {
            let more = incoming.get(&p.name).copied().unwrap_or(0);
            (p.name.clone(), p.hits_max - p.hits + more)
        })
        .collect();
    let healers: Vec<&Creep> = healers
        .iter()
        .copied()
        .filter(|h| !swinging.contains(&h.name))
        .collect();
    let patients: Vec<&Creep> = patients.iter().collect();
    allot(&healers, &patients, &mut owed, &|n| shooting.contains(n))
        .into_iter()
        .map(|(healer, p, beside)| {
            let act = if beside {
                CreepAct::HealCreep(p.id)
            } else {
                CreepAct::RangedHealCreep(p.id)
            };
            (healer, act)
        })
        .collect()
}

/// A target's worth a hit, and whether a mass volley strikes it (one with an
/// owner: the engine's RMA passes over a container).
pub struct Mark {
    pub id: Id,
    pub pos: Pos,
    pub worth: i32,
    pub hits: i32,
    pub massed: bool,
    pub shielded: bool,
    /// It leaves the room in a step: a strike that lands now, never a step.
    pub fleeting: bool,
    /// Each tile it may stand on next tick, its own first; None where no
    /// shot lands.
    pub next: Vec<Option<Pos>>,
}

/// §6.4's Hunt, after ibex: risk at the margin and a three-tick lethal veto,
/// so survival is never outvoted; scored only in `contact`, the whole set at
/// once, as per-body scoring out of it oscillated. A volley never fires
/// beside a body it spares. Also each tile's score.
pub fn hunt(
    sight: &mut Sight,
    threat: &tiles::Table<i32>,
    marks: &[Mark],
    mates: &[&Creep],
    (centre, contact): (&Pos, bool),
    c: &Creep,
) -> (Option<CreepAct>, Wish, Vec<(Tile, i64)>) {
    let room = c.pos.room;
    let spared = sight.spared.get(&room).cloned().unwrap_or_default();
    let parts = c.live(Part::RangedAttack);
    let worth_of = |m: &Mark, damage: i32| {
        i64::from(damage.min(m.hits)) * i64::from(m.worth) / i64::from(m.hits.max(1))
    };
    // A hit's worth from `at`, now or over the tiles a mark may stand on next
    // tick: the step taken now is the next tick's reach, and a shot is
    // resolved before the mark steps (v1's glue).
    let share = |m: &Mark, at: &Pos, next: bool, hit: &dyn Fn(i32) -> i32| -> i64 {
        let now = [(!m.shielded).then_some(m.pos)];
        let tiles = if next { &m.next[..] } else { &now[..] };
        let n = tiles.len().max(1) as i64;
        let hits = tiles.iter().flatten().filter_map(|p| at.range(p));
        hits.map(|r| worth_of(m, hit(r))).sum::<i64>() / n
    };
    let shot = |r: i32| {
        if r <= engine::RANGED_RANGE {
            engine::RANGED_ATTACK_POWER * parts
        } else {
            0
        }
    };
    let landed = |at: &Pos, next: bool| -> (i64, Option<CreepAct>) {
        let single = (marks.iter())
            .map(|m| (share(m, at, next, &shot), m.id))
            .filter(|(w, _)| *w > 0)
            .max();
        let mass: i64 = if spared.iter().any(|p| at.within(engine::RANGED_RANGE, p)) {
            0
        } else {
            (marks.iter().filter(|m| m.massed))
                .map(|m| share(m, at, next, &|r| parts * engine::mass_attack_power_at(r)))
                .sum()
        };
        match single {
            Some((w, id)) if parts > 0 && w >= mass => (w, Some(CreepAct::RangedAttackCreep(id))),
            _ if mass > 0 => (mass, Some(CreepAct::RangedMassAttack)),
            _ => (0, None),
        }
    };
    let swing = engine::ATTACK_POWER * c.live(Part::Attack);
    let swung = |at: &Pos| -> i64 {
        let hit = |r: i32| if r <= engine::MELEE_RANGE { swing } else { 0 };
        (marks.iter())
            .map(|m| share(m, at, true, &hit))
            .max()
            .unwrap_or(0)
    };
    let strike = landed(&c.pos, false).1;
    // A swing lands from no shot's range: each body closes to its own reach.
    let reach = if parts > 0 {
        engine::RANGED_RANGE
    } else {
        engine::MELEE_RANGE
    };
    let mut goal = target_at(sight, centre, reach);
    let targets: Vec<Tile> = (marks.iter())
        .filter(|m| m.worth > 0 && !m.fleeting)
        .map(|m| tiles::of_pos(&m.pos))
        .collect();
    if !targets.is_empty() {
        goal.tiles = targets;
    }
    if !contact {
        let on = plan::toward(sight.atlas, c, mover_of(c), &goal, WEIGHT);
        return (strike, on, Vec::new());
    }
    let cost = i64::from(engine::body_cost(
        &c.body.iter().map(|p| p.kind).collect::<Vec<_>>(),
    ));
    let field = sight.atlas.field(mover_of(c), &goal, &c.pos);
    let approach = |t: Tile| {
        -field
            .as_ref()
            .and_then(|f| f.ticks(t))
            .unwrap_or(i32::MAX / 2)
    };
    let nets = nets(&*sight.atlas, threat, mates, c);
    let open: Vec<Tile> = nets.iter().map(|(t, _)| *t).collect();
    let net = |t: Tile| {
        nets.iter()
            .find(|(o, _)| *o == t)
            .map_or(i32::MAX, |(_, n)| *n)
    };
    // Onto a mark beside it that may step: it follows the mark if it steps
    // and stays beside it if not.
    let glue = (marks.iter())
        .filter(|m| !m.shielded && m.next.len() > 1 && c.pos.range(&m.pos) == Some(1))
        .map(|m| (tiles::of_pos(&m.pos), threat[tiles::of_pos(&m.pos)]));
    let scored: Vec<(Tile, i64)> = (nets.iter().copied())
        .chain(glue)
        .filter(|(t, net)| !tiles::on_edge(*t) && net * 3 < c.hits)
        .map(|(t, net)| {
            let risk = i64::from(net.max(0)) * cost / i64::from(threat[t].max(c.hits / 3).max(1));
            let at = tiles::to_pos(room, t);
            (t, landed(&at, true).0 + swung(&at) - risk)
        })
        .collect();
    // Every tile lethal: the least deadly, an exit among them, until the
    // engagement, told so (`cornered`), re-prices.
    let best = scored
        .iter()
        .max_by_key(|(t, score)| (*score, approach(*t), std::cmp::Reverse(*t)))
        .map(|(t, _)| *t)
        .or_else(|| open.iter().copied().min_by_key(|t| (net(*t), *t)));
    let wish = best.map_or_else(|| hold(sight, c), |t| step_to(c, t));
    (strike, wish, scored)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn rule(
        room: Option<&str>,
        rival: Option<&str>,
        pin: Option<&str>,
        forbid: &[&str],
    ) -> TacticRule {
        TacticRule {
            rival: rival.map(str::to_string),
            room: room.map(RoomName::named),
            pin: pin.map(str::to_string),
            forbid: forbid.iter().map(ToString::to_string).collect(),
        }
    }

    #[test]
    fn a_rule_matches_every_field_it_sets_and_the_last_pin_wins() {
        let rules = [
            rule(None, Some("rival"), Some("kite"), &[]),
            rule(Some("W1N1"), None, None, &["blob"]),
            rule(Some("W1N1"), Some("rival"), Some("hold"), &[]),
        ];
        let rivals: BTreeSet<&str> = ["rival"].into_iter().collect();

        let here = ruling(&rules, RoomName::named("W1N1"), &rivals);
        assert_eq!(here.pin, Some(Tactic::Hold));
        assert!(here.forbid.contains(&Tactic::BlobClose));

        let elsewhere = ruling(&rules, RoomName::named("W2N2"), &rivals);
        assert_eq!(elsewhere.pin, Some(Tactic::Kite));
        assert!(elsewhere.forbid.is_empty());

        let stranger: BTreeSet<&str> = ["other"].into_iter().collect();
        assert_eq!(ruling(&rules, RoomName::named("W1N1"), &stranger).pin, None);
    }

    #[test]
    fn a_rule_a_name_no_tactic_has_or_a_pin_a_rule_that_joins_it_forbids_never_applies() {
        let (kept, refused) = checked(vec![
            rule(None, None, None, &["bounce"]),
            rule(Some("W1N1"), None, Some("kite"), &["kite"]),
            rule(Some("W2N2"), None, Some("close"), &[]),
        ]);

        assert_eq!(kept.len(), 1);
        assert_eq!(refused.len(), 2);

        // A room-wide rule forbids what a rival's rule pins: neither names the other.
        let (kept, refused) = checked(vec![
            rule(Some("W1N1"), Some("rival"), Some("kite"), &[]),
            rule(Some("W1N1"), None, None, &["kite"]),
        ]);

        assert_eq!(kept.len(), 1);
        assert_eq!(refused.len(), 1);
    }

    fn offer(tactic: Tactic, bodies: i32, value: i32, cost: i32) -> Offer {
        Offer {
            tactic,
            bodies,
            value,
            cost,
        }
    }

    #[test]
    fn the_best_offer_pays_most_then_fields_fewest_then_keeps_the_kinds_order() {
        let rich = [
            offer(Tactic::Close, 3, 900, 400),
            offer(Tactic::Kite, 3, 200, 100),
        ];
        assert_eq!(best(&rich).map(|o| o.tactic), Some(Tactic::Close));

        let few = [
            offer(Tactic::Close, 4, 500, 200),
            offer(Tactic::Kite, 2, 500, 200),
        ];
        assert_eq!(best(&few).map(|o| o.tactic), Some(Tactic::Kite));

        let first = [
            offer(Tactic::Hold, 2, 500, 200),
            offer(Tactic::Close, 2, 500, 200),
        ];
        assert_eq!(best(&first).map(|o| o.tactic), Some(Tactic::Hold));
        assert_eq!(best(&[]), None);
    }

    #[test]
    fn every_tactic_names_itself_and_only_itself() {
        for tactic in [
            Tactic::Kite,
            Tactic::Close,
            Tactic::Hold,
            Tactic::BlobKite,
            Tactic::BlobClose,
        ] {
            assert_eq!(by_name(name(tactic)), Some(tactic));
        }
        assert_eq!(by_name("bounce"), None);
    }

    #[test]
    fn a_pin_wins_where_it_is_offered_and_falls_through_where_it_cannot_fight() {
        let offers = [
            offer(Tactic::Close, 3, 900, 400),
            offer(Tactic::Kite, 3, 200, 100),
        ];
        assert_eq!(
            select(&offers, Some(Tactic::Kite)).map(|o| o.tactic),
            Some(Tactic::Kite)
        );
        assert_eq!(
            select(&offers, Some(Tactic::Hold)).map(|o| o.tactic),
            Some(Tactic::Close)
        );
        assert_eq!(select(&[], Some(Tactic::Kite)), None);
    }

    #[test]
    fn a_tactic_is_written_and_read_by_its_name() {
        use Tactic::*;
        for t in [Kite, Close, Hold, BlobKite, BlobClose] {
            assert_eq!(serde_json::to_value(t).unwrap(), name(t));
            assert_eq!(serde_json::from_value::<Tactic>(name(t).into()).unwrap(), t);
        }
    }
}
