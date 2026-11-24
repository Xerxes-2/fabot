/// What a fight body does this tick (`ARCHITECTURE.md` §6.4): a step read off
/// the plan and what stands in its room, and the strike reflex. From v1's
/// Emitter and Resolver step functions. Also which tactic a kind picks (§6.3).
use std::collections::{BTreeMap, BTreeSet};

use serde::{Deserialize, Serialize};
use strum::{EnumString, IntoStaticStr};

use fabot_atlas::{Atlas, Mover, Target};
use fabot_base::declarations::{self, TacticRule};
use fabot_base::grid::{self, Footprint};
use fabot_base::intents::CreepAct;
use fabot_base::tiles::Tile;
use fabot_base::vocabulary::Part;
use fabot_base::wish::Wish;
use fabot_base::world::{self, Creep, Id, Pos, RoomName};
use fabot_base::{engine, tiles};
use fabot_mv::{blob, plan};

/// A phase's own tactic (pursue, retreat) is the phase's; this is what a kind
/// commits to, a price per candidate (§6.3).
#[derive(
    Clone,
    Copy,
    Debug,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Serialize,
    Deserialize,
    EnumString,
    IntoStaticStr,
)]
#[cfg_attr(test, derive(strum::EnumIter))]
// Read borrowed, as Memory's `list` reads it: an owned string is refused.
#[serde(into = "&'static str", try_from = "&str")]
#[strum(serialize_all = "kebab-case")]
pub enum Tactic {
    Kite,
    Close,
    /// On our ramparts: a home is a post the raid comes to.
    Hold,
    /// A blob is a set that walks in as one, then fights by the tactic it names.
    BlobKite,
    #[strum(serialize = "blob")]
    BlobClose,
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
            .extend(rule.forbid.iter().filter_map(|n| n.parse::<Tactic>().ok()));

        out.pin = rule.pin.as_deref().and_then(|n| n.parse().ok()).or(out.pin);
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
            .find(|name| name.parse::<Tactic>().is_err());

        let pin = rule.pin.as_deref().and_then(|n| n.parse::<Tactic>().ok());

        let contradicts = rules
            .iter()
            .filter(|other| joins(rule, other))
            .flat_map(|other| other.forbid.iter().filter_map(|n| n.parse().ok()))
            .any(|forbidden| Some(forbidden) == pin);

        match (unknown, pin) {
            (Some(name), _) => refused.push(format!("tactics {name}")),
            (None, Some(pin)) if contradicts => {
                let pin: &str = pin.into();
                refused.push(format!("tactics pin {pin}"));
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
    plan::stay(&*sight.atlas, creep, mover_of(creep).factor, WEIGHT, None)
}

/// `enter` ranks no mark outside the centre's room: a mark moving there swings the
/// route between borders, and the room's middle may be walled in.
fn aim(sight: &mut Sight, order: &[Id], centre: &Pos, enter: bool, c: &Creep, range: i32) -> Wish {
    let half = engine::ROOM_SIDE / 2;
    let (at, range) = match ranked(sight, order, c).first() {
        _ if enter && c.pos.room != centre.room => (Pos::new(centre.room, half, half), half),
        Some(h) if !c.swings() && h.live(Part::Attack) > 0 => (h.pos, engine::RANGED_RANGE),
        first => (first.map_or(*centre, |h| h.pos), range),
    };
    let (target, foes) = (target_at(sight, &at, range), hostiles_in(sight, c.pos.room));
    let walk = (mover_of(c), &target, WEIGHT);
    if c.swings() {
        plan::toward(sight.atlas, c, walk.0, walk.1, walk.2)
    } else {
        plan::kited(sight.atlas, c, walk, foes)
    }
}

pub fn close(sight: &mut Sight, order: &[Id], centre: &Pos, enter: bool, creep: &Creep) -> Wish {
    aim(sight, order, centre, enter, creep, engine::MELEE_RANGE)
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
            .map(|m| engine::heal_at(world::power(m, Part::Heal), engine::MELEE_RANGE))
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

pub fn engage(
    sight: &mut Sight,
    (tactic, order): (Tactic, &[Id]),
    (centre, enter): (&Pos, bool),
    (members, fighters): (&[&Creep], &[&Creep]),
    c: &Creep,
) -> Wish {
    match (tactic, c.swings()) {
        (Tactic::BlobClose, false) if exposed(sight, centre.room, members, c) => {
            step_back(sight, centre.room, members, c)
        }
        (Tactic::Kite | Tactic::BlobKite, false) => {
            aim(sight, order, centre, enter, c, engine::RANGED_RANGE)
        }
        (Tactic::Hold, _) => rampart(sight, fighters, c),
        _ => close(sight, order, centre, enter, c),
    }
}

/// A blob of bodies that do not swing, bound for a swing's range, walks only those with no tile
/// beside or under them in a swing's reach (`plan::swung`); the rest get no wish from it.
pub fn blob(
    sight: &mut Sight,
    centre: &Pos,
    range: i32,
    members: &[&Creep],
    fortified: &BTreeSet<RoomName>,
) -> Vec<Wish> {
    let near = |c: &Creep| {
        let swung = plan::swung(sight.atlas, c, hostiles_in(sight, c.pos.room));
        let beside = tiles::within(1, tiles::of_pos(&c.pos));
        swung.is_some_and(|d| beside.iter().any(|t| d.contains(*t)))
    };
    let swingers = members.iter().any(|c| c.swings());
    let members: Vec<&Creep> = (members.iter().copied())
        .filter(|c| swingers || range >= engine::RANGED_RANGE || !near(c))
        .collect();
    if members.is_empty() {
        return Vec::new();
    }
    let target = target_at(sight, centre, range);
    blob::wishes(sight.atlas, &target, WEIGHT, &members, fortified)
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
pub fn picket(sight: &mut Sight, front: &Front, members: &[(u16, &Creep)], creep: &Creep) -> Wish {
    // By slot, which a successor keeps, so a landing never reshuffles posts.
    let mut pickets: Vec<(u16, &Creep)> = members.to_vec();
    pickets.retain(|(_, c)| picket_of(c));
    pickets.sort_by_key(|&(slot, c)| (slot, c.name.as_str()));

    let at = pickets
        .iter()
        .position(|(_, c)| c.name == creep.name)
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
        - members
            .iter()
            .map(|m| engine::heal_at(world::power(m, Part::Heal), engine::MELEE_RANGE))
            .sum::<i32>();

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

/// One body a rampart, as Exchange's Hold stance seats them: shoving a body off
/// its rampart trades the tile with it every tick, each bare in turn.
pub fn rampart(sight: &mut Sight, ours: &[&Creep], creep: &Creep) -> Wish {
    let taken = |p: &Pos| *p != creep.pos && ours.iter().any(|m| m.pos == *p);
    let nearest = (sight.ramparts.get(&creep.pos.room).into_iter().flatten())
        .filter(|p| !taken(p))
        .min_by_key(|p| (creep.pos.range(p).unwrap_or(99), p.x, p.y))
        .copied();

    match nearest {
        Some(p) if p != creep.pos => toward(sight, creep, &p, 0),
        _ => hold(sight, creep),
    }
}

/// v1's guard outlived its raid where it stood, to meet the next one there.
pub fn rest(sight: &mut Sight, creep: &Creep, outposts: &BTreeSet<RoomName>) -> Wish {
    let (room, at) = (creep.pos.room, tiles::of_pos(&creep.pos));
    match outposts
        .contains(&room)
        .then(|| sight.atlas.idle(room, at, REST_RANGE))
    {
        Some(Some(t)) if t == at => hold(sight, creep),
        Some(Some(t)) => toward(sight, creep, &tiles::to_pos(room, t), 0),
        _ => retreat(sight, creep),
    }
}

const REST_RANGE: i32 = 8;

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

/// v1's heal reflex. The engine keeps one of a swing and a heal, and one of a
/// shot and a ranged heal: a swinging body does not heal, a shooting one heals
/// beside it. A heal lands in the tick's sum with the strikes, so what is
/// `incoming` is owed. Each healer comes with its `world::power` of HEAL.
pub fn allot<'p>(
    healers: &[(&Creep, i32)],
    patients: &[&'p Creep],
    owed: &mut BTreeMap<String, i32>,
    shooting: &dyn Fn(&str) -> bool,
) -> Vec<(String, &'p Creep, bool, i32)> {
    let mut healers = healers.to_vec();
    healers.sort_by(|a, b| a.0.name.cmp(&b.0.name));
    let mut out = Vec::new();
    for (healer, power) in healers {
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
        if let (true, Some((p, r))) = (power > 0, patient) {
            let heal = engine::heal_at(power, r);
            *owed.entry(p.name.clone()).or_insert(0) -= heal;
            out.push((healer.name.clone(), p, r <= engine::MELEE_RANGE, heal));
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
    let healers: Vec<(&Creep, i32)> = (mates.iter().copied().chain([c]))
        .map(|m| (m, world::power(m, Part::Heal)))
        .collect();
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
                .filter(|(_, p, _, _)| p.name == c.name)
                .map(|(_, _, _, heal)| heal)
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
    let healers: Vec<(&Creep, i32)> = healers
        .iter()
        .copied()
        .filter(|h| !swinging.contains(&h.name))
        .map(|h| (h, world::power(h, Part::Heal)))
        .collect();
    let patients: Vec<&Creep> = patients.iter().collect();
    allot(&healers, &patients, &mut owed, &|n| shooting.contains(n))
        .into_iter()
        .map(|(healer, p, beside, _)| {
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
    use strum::IntoEnumIterator;

    use super::*;

    fn rule(
        room: Option<&str>,
        rival: Option<&str>,
        pin: Option<&str>,
        forbid: &[&str],
    ) -> TacticRule {
        TacticRule {
            rival: rival.map(str::to_string),
            room: room.map(|r| r.parse::<RoomName>().unwrap()),
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

        let here = ruling(&rules, RoomName::W1N1, &rivals);
        assert_eq!(here.pin, Some(Tactic::Hold));
        assert!(here.forbid.contains(&Tactic::BlobClose));

        let elsewhere = ruling(&rules, "W2N2".parse::<RoomName>().unwrap(), &rivals);
        assert_eq!(elsewhere.pin, Some(Tactic::Kite));
        assert!(elsewhere.forbid.is_empty());

        let stranger: BTreeSet<&str> = ["other"].into_iter().collect();
        assert_eq!(ruling(&rules, RoomName::W1N1, &stranger).pin, None);
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
        for tactic in Tactic::iter() {
            let name: &str = tactic.into();
            assert_eq!(name.parse(), Ok(tactic));
        }
        assert!("bounce".parse::<Tactic>().is_err());
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

    fn creep(id: &str, owner: fabot_base::world::Owner, at: (i32, i32), parts: &[Part]) -> Creep {
        let part = |kind| fabot_base::world::BodyPart {
            kind,
            hits: 100,
            boost: None,
        };
        Creep {
            id: id.into(),
            name: id.into(),
            owner,
            pos: Pos::new(RoomName::W1N1, at.0, at.1),
            body: parts.iter().copied().map(part).collect(),
            hits: 100 * parts.len() as i32,
            hits_max: 100 * parts.len() as i32,
            ticks_to_live: 1000,
            fatigue: 0,
            store: fabot_base::world::Store::empty(),
        }
    }

    /// A raider's body and, by hand, the ticks its step onto plain holds it; None without MOVE.
    const RAIDERS: [(&[Part], Option<i32>); 4] = [
        (&[Part::Attack, Part::Move], Some(1)),
        (&[Part::RangedAttack, Part::Move], Some(1)),
        (&[Part::Attack], None),
        (
            &[Part::Attack, Part::Attack, Part::Attack, Part::Move],
            Some(3),
        ),
    ];

    /// What a ranger does at (25, 25), `heavy` with a second RANGED_ATTACK on its one MOVE,
    /// `tired` with that fatigue, under `tactic` (a blob of its own under `BlobClose`), swamp on
    /// `swamp`, against `RAIDERS` at the offsets given.
    struct Stepped {
        /// A tile it stands on or beside no swing strikes by the tick it could be on it and gone.
        out: bool,
        /// Its step lands on a tile a swing does.
        reached: bool,
        /// The tiles `plan::swung` marks, and those v1's rule marks worked out here.
        marked: (BTreeSet<Tile>, BTreeSet<Tile>),
    }

    fn stepped(
        (tactic, heavy, tired): (Tactic, bool, i32),
        raid: &[(i32, i32, usize)],
        swamp: &[(i32, i32)],
    ) -> Option<Stepped> {
        use fabot_base::world::{Owner, Room, World, terrain};
        const ROOM: RoomName = RoomName::W1N1;
        let legs: &[Part] = if heavy {
            &[Part::RangedAttack, Part::RangedAttack, Part::Move]
        } else {
            &[Part::RangedAttack, Part::Move]
        };
        let ranger = Creep {
            fatigue: tired,
            ..creep("r", Owner::Ours, (25, 25), legs)
        };
        let raiders: Vec<(Creep, Option<i32>)> = (raid.iter().enumerate())
            .filter(|(_, (dx, dy, _))| (*dx, *dy) != (0, 0))
            .map(|(i, (dx, dy, k))| {
                let (body, step) = RAIDERS[*k];
                let at = (25 + dx, 25 + dy);
                (creep(&format!("h{i}"), Owner::Invader, at, body), step)
            })
            .collect();
        let hostiles: Vec<Creep> = raiders.iter().map(|(h, _)| h.clone()).collect();
        let mut bits = vec![0; 2500];
        for (dx, dy) in swamp {
            bits[tiles::index(25 + dx, 25 + dy).index()] = 2;
        }
        let world = World {
            rooms: vec![Room {
                terrain: terrain::leak(bits.clone()),
                ..Room::empty(ROOM)
            }],
            creeps: vec![ranger.clone()],
            time: 1,
            me: String::new(),
            cpu_limit: 20.0,
            bucket: 10_000,
            spawns: Vec::new(),
            attacks: Vec::new(),
            destroyed: Vec::new(),
            our_sites: 0,
            rooms_allowed: 1,
            closed: Default::default(),
            rules: fabot_base::world::Rules::vanilla(),
        };
        let mut atlas = Atlas::of_world(&world);
        let marked = plan::swung(&atlas, &ranger, &hostiles);
        let foes = BTreeMap::from([(ROOM, hostiles.clone())]);
        let mut sight = Sight {
            atlas: &mut atlas,
            hostiles: &foes,
            struck: hostiles.iter().map(|h| h.id).collect(),
            spared: BTreeMap::new(),
            ramparts: BTreeMap::new(),
            musters: Vec::new(),
            avoid: &BTreeSet::new(),
        };
        let order: Vec<Id> = hostiles.iter().map(|h| h.id).collect();
        let (on, at, set) = (
            (tactic, &order[..]),
            (&hostiles.first()?.pos, true),
            &[&ranger][..],
        );
        let walked = (tactic == Tactic::BlobClose)
            .then(|| blob(&mut sight, at.0, engine::MELEE_RANGE, set, &BTreeSet::new()))
            .and_then(|mut w| w.pop());
        let wish = walked.unwrap_or_else(|| engage(&mut sight, on, at, (set, set), &ranger));
        // v1's rule worked out here. The ranger leaves a tile `d` off once rested, a plain step
        // a tile before and the step onto it held (its legs read by `factor_of`/`step_ticks`),
        // never sooner than the tick after; a raider strikes it once it has closed to beside.
        let here = tiles::of_pos(&ranger.pos);
        let legs = grid::factor_of(&ranger);
        let ground = |t: Tile| match bits[t.index()] {
            2 => engine::SWAMP_WEIGHT,
            _ => engine::PLAIN_WEIGHT,
        };
        let rest = (tired + 2 * legs.moves - 1) / (2 * legs.moves);
        let step = |w| grid::step_ticks(legs, w).unwrap_or(1);
        let leaves = |t: Tile| match tiles::range(here, t) {
            0 => rest.max(1),
            d => rest + (d - 1) * step(engine::PLAIN_WEIGHT) + step(ground(t)),
        };
        let strikes = |(h, steps): &(Creep, Option<i32>), t: Tile| match (
            h.pos.range(&tiles::to_pos(ROOM, t)).unwrap_or(99),
            steps,
        ) {
            (r, _) if r <= engine::MELEE_RANGE => Some(0),
            (r, Some(ticks)) => Some((r - 2) * ticks + 1),
            _ => None,
        };
        let swings = raiders.iter().filter(|(h, _)| h.live(Part::Attack) > 0);
        let struck = |t: Tile| {
            (swings.clone()).any(|raider| strikes(raider, t).is_some_and(|s| s <= leaves(t)))
        };
        let near = tiles::within(2 * engine::RANGED_RANGE + 1, here);
        let mut beside = std::iter::once(here).chain(tiles::around(here).iter().copied());
        Some(Stepped {
            out: beside.any(|t| !struck(t)),
            reached: struck(wish.head),
            marked: (
                near.iter()
                    .copied()
                    .filter(|t| marked.as_ref().is_some_and(|m| m.contains(*t)))
                    .collect(),
                near.iter().copied().filter(|t| struck(*t)).collect(),
            ),
        })
    }

    const WALKS: &[Tactic] = &[Tactic::Kite, Tactic::Close, Tactic::BlobClose];

    proptest::proptest! {
        #![proptest_config(fabot_base::testing::config(300))]

        #[test]
        fn a_body_that_does_not_swing_steps_into_no_swings_next_reach_while_a_tile_beside_lies_out(
            tactic in proptest::sample::select(WALKS),
            heavy in proptest::bool::ANY,
            tired in proptest::sample::select(&[0, 0, 2, 4][..]),
            raid in proptest::collection::vec((-5..=5i32, -5..=5i32, 0..4usize), 1..=3),
            swamp in proptest::collection::vec((-2..=2i32, -2..=2i32), 0..=6),
        ) {
            if let Some(s) = stepped((tactic, heavy, tired), &raid, &swamp) {
                proptest::prop_assert_eq!(&s.marked.0, &s.marked.1);
                proptest::prop_assert!(tired > 0 || !(s.out && s.reached));
            }
        }
    }

    #[test]
    fn a_tactic_is_written_and_read_by_its_name() {
        for t in Tactic::iter() {
            let name: &str = t.into();
            let value = serde_json::to_value(t).unwrap();
            assert_eq!(value, name);
            assert_eq!(Tactic::deserialize(&value).unwrap(), t);
        }
    }
}
