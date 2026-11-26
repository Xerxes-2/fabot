/// `Defend` (`ARCHITECTURE.md` §6.2): a phase moves on an event or a deadline,
/// never a clock; `step` closes it with None.
use std::borrow::Borrow;
use std::collections::{BTreeMap, BTreeSet};

use fabot_atlas::Atlas;
use fabot_base::{
    declarations, engine,
    intents::{Action, SpawnAct},
    tiles::{self},
    vocabulary::{Part, part_count_in},
    wish::{Idle, Wish},
    world::{
        Controller, Creep, Fallen, Id, Oven, Owner, Pos, RoomName, Spawn, Structure, StructureType,
        World, spent,
    },
};
use fabot_exchange::{self as exchange, Outcome};
use fabot_tactics::{self as tactics, Tactic};

use super::super::{
    scene::{Asked, Hunting, Scene},
    shape::{Cast, Engagement, Group, Objective, Phase, Slot, enter},
};

/// What a `Defend` may fight as, in the order its ties fall to (§6.3).
pub const CANDIDATES: &[Tactic] =
    &[Tactic::Hold, Tactic::Close, Tactic::Kite, Tactic::BlobKite, Tactic::BlobClose];

pub struct Facts {
    pub time: i32,
    pub group: Option<Group>,
    pub standing: BTreeSet<String>,
    pub aging_out: BTreeSet<String>,
    pub in_reach: bool,
    pub home: bool,
    /// It defends a home, which it never leaves to its towers alone.
    pub held: bool,
    pub asked: bool,
    pub on_our_ground: bool,
    pub returned: bool,
    pub clear: bool,
    pub armed_in: bool,
    pub remembered: bool,
}

/// A home's Defend never disengages: it stops asking, holds and is unanswered.
/// Neither arm is priced, so the offers behind the last price go with it.
pub fn give_up(held: bool, time: i32, e: &Engagement) -> Engagement {
    let mut next = if held {
        Engagement { unanswered: true, ..e.clone() }
    } else {
        Engagement { ours: BTreeMap::new(), ..enter(Phase::Disengaging, time, e) }
    };
    next.plan.offers.clear();
    next
}

pub(crate) fn free(c: &Creep) -> bool {
    declarations::fights(&c.name)
        && !spent(&c.body)
        && c.ticks_to_live > engine::AGED_OUT_TICKS_TO_LIVE
}

/// Held by no answer: disarmed, or cast by a Defend standing given up and no open sweep's to take.
fn released((scene, atlas): (&Scene, &Atlas), fights: Fights, c: &Creep) -> bool {
    let (decided, casts, grounds) = fights;
    // An id is reused once closed: a body asked before this Defend opened is not its.
    let gave_up = |(asked, id): (i32, i32)| {
        (decided.iter()).any(|e| {
            e.id == id
                && e.opened <= asked
                && e.objective == Objective::Defend
                && e.phase == Phase::Disengaging
        })
    };
    let drafted = (decided.iter()).any(|e| e.ours.contains_key(&c.name));
    let taken = |e: &Engagement| {
        let ground = (scene.in_sight(e.room)).zip(grounds.get(&e.room));
        e.objective == Objective::Sweep
            && ground.is_some_and(|(r, g)| scene.takes_in(atlas, (e, casts), c, (g, r)))
    };
    let given_up = cast_of(&c.name).is_some_and(gave_up) && !(free(c) && decided.iter().any(taken));
    !drafted && declarations::fights(&c.name) && (spent(&c.body) || given_up)
}

/// The engagements decided, their casts and the sweeps' grounds.
pub(crate) type Fights<'a> = (&'a [Engagement], &'a [Cast], &'a BTreeMap<RoomName, Hunting>);

/// A released body walks home to a spawn and is recycled there; any other rests.
pub(crate) fn idle(sight: &mut tactics::Sight, scene: &Scene, fights: Fights, c: &Creep) -> Wish {
    let spawn = (scene.world.working_spawns()).find(|s| s.pos.room == c.pos.room);
    match (released((scene, sight.atlas), fights, c), spawn) {
        (true, Some(s)) => tactics::toward(sight, c, &s.pos, engine::MELEE_RANGE),
        (true, None) => tactics::retreat(sight, c),
        (false, _) => tactics::rest(sight, c, &scene.worked).idling(Idle::Unengaged),
    }
}

pub(crate) fn recycles(scene: &Scene, atlas: &Atlas, fights: Fights) -> Vec<Action> {
    let spawns = || scene.world.working_spawns();
    let beside = |c: &Creep| spawns().find(|s| s.pos.within(engine::MELEE_RANGE, &c.pos));
    (scene.world.creeps.iter())
        .filter(|c| released((scene, atlas), fights, c))
        .filter_map(|c| Some(Action::Spawn(beside(c)?.id, SpawnAct::RecycleCreep(c.id))))
        .collect()
}

pub fn stands_alone(time: i32, e: &Engagement) -> Engagement {
    if e.phase == Phase::Forming && !e.ours.is_empty() {
        enter(Phase::Approaching, time, e)
    } else {
        e.clone()
    }
}

/// A forming set none of whose casts has begun is past its deadline (§6.2) once
/// their spawns began another body since the ask and the landing priced passed:
/// a home's, as a raid there strikes as it stands, and a sweep's.
pub(crate) fn overdue(scene: &Scene, e: &Engagement) -> bool {
    let casts: Vec<&Cast> = (scene.spawning.iter()).filter(|c| c.engagement == e.id).collect();
    let asked = casts.iter().filter_map(|c| cast_of(&c.name)).map(|(t, _)| t).max();
    let spawns: Vec<&Spawn> = (scene.world.working_spawns())
        .filter(|s| casts.iter().any(|c| c.room == s.pos.room))
        .collect();
    let lands = exchange::spawned(casts.iter().map(|c| c.body.len()), spawns.len() as i32);
    let landing = lands.into_iter().max().unwrap_or(0);
    let ovens: Vec<&Oven> = spawns.iter().filter_map(|s| s.spawning.as_ref()).collect();
    let short = |s: &&&Spawn| {
        let bank = scene.world.room(s.pos.room).map_or(0, |r| r.energy_available);
        (casts.iter()).any(|c| c.room == s.pos.room && engine::body_cost(&c.body) > bank)
    };
    let idle = spawns.iter().any(|s| s.spawning.is_none() && short(&s));
    let begun = |c: &&Cast| ovens.iter().any(|o| o.name == c.name);

    let home = scene.homes.contains(&e.room) && !e.unanswered && e.ours.is_empty();

    (home || e.objective == Objective::Sweep)
        && e.phase == Phase::Forming
        && !casts.iter().any(begun)
        && asked.is_some_and(|at| passed_over(scene.world.time, at, landing, (&ovens, idle)))
}

/// Its asks dropped, a home's set past its deadline holds its towers alone; a
/// sweep goes on only if its standing bodies beat its room as they stand.
pub(crate) fn late(
    asked: &mut Asked,
    (time, stands): (i32, bool),
    e: &Engagement,
) -> Option<Engagement> {
    asked.casts.retain(|c| c.engagement != e.id);
    match e.objective {
        Objective::Sweep => stands.then(|| stands_alone(time, e)),
        _ => Some(give_up(true, time, e)),
    }
}

/// A cast is named for the tick it was asked, which its deadline reads (§6.2).
pub(crate) fn cast_name(time: i32, engagement: i32, slot: Slot) -> String {
    format!("{}{time}-{engagement}-{slot}", declarations::FIGHT_PREFIX)
}

pub(crate) fn cast_of(name: &str) -> Option<(i32, i32)> {
    let mut parts = name.strip_prefix(declarations::FIGHT_PREFIX)?.split('-');
    Some((parts.next()?.parse().ok()?, parts.next()?.parse().ok()?))
}

/// A body its spawn began before the ask was in its price; one begun after, or a
/// spawn idle short of a cast's energy, passed the set over.
fn passed_over(time: i32, since: i32, landing: i32, (ovens, idle): (&[&Oven], bool)) -> bool {
    let began = |o: &&&Oven| time - (engine::spawn_ticks(o.body.len()) - o.remaining);
    time > since + landing && (idle || ovens.iter().any(|o| began(&o) > since))
}

/// Its losses past the budget priced as it committed: a Defend's and a sweep's.
pub fn beaten(e: &Engagement) -> bool {
    e.lost > e.budget
}

/// Past its first win's ticks from opening, a raid out of reach of the ramparts over our
/// structures and struck by nothing of ours does not come to the post: re-read each re-price.
pub(crate) fn off_post(scene: &Scene, e: &Engagement) -> bool {
    let (Some(Outcome::Win(ticks, _)), Some(room)) = (e.predicted, scene.in_sight(e.room)) else {
        return false;
    };
    let ours = |id: &Id| {
        (scene.world.creeps.iter()).any(|c| c.id == *id)
            || (room.structures.iter()).any(|s| s.id == *id && s.owner.is_ours())
    };
    let raid = |id: &Id| (scene.by_id.get(&e.group)).is_some_and(|g| g.members.contains(id));
    let struck = (scene.world.attacks.iter()).any(|(by, at)| raid(at) && ours(by));
    let kept = |r: &&Structure| r.kind != StructureType::Rampart && r.owner.is_ours();
    let post: Vec<Pos> = (room.structures.iter())
        .filter(|r| r.kind == StructureType::Rampart && r.owner.is_ours())
        .filter(|r| room.structures.iter().filter(kept).any(|s| s.pos == r.pos))
        .map(|r| r.pos)
        .collect();
    let members = (scene.by_id.get(&e.group).into_iter()).flat_map(|g| &g.members);
    let mut at = members.filter_map(|m| scene.hostile_at.get(m));
    let near = at.any(|h| post.iter().any(|p| p.within(engine::RANGED_RANGE, h)));
    scene.world.time > e.opened + ticks && !near && !struck
}

/// The event: a Hold past its deadline whose raid never came to the post re-prices off it.
pub(crate) fn unvisited(scene: &Scene, e: &Engagement) -> bool {
    let fights = matches!(e.phase, Phase::Approaching | Phase::Fighting) && !e.unanswered;
    e.plan.tactic == Tactic::Hold && fights && off_post(scene, e)
}

/// Or on the edge tile facing it next door, as a bounce over the border leaves nothing.
pub(crate) fn on_ground(scene: &Scene, centre: &Pos) -> bool {
    let across = tiles::room_toward(centre.room, tiles::exit_of(tiles::of_pos(centre)));
    scene.defended.contains(&centre.room) || across.is_some_and(|r| scene.defended.contains(&r))
}

pub(crate) fn returned(scene: &Scene, was: &[Group], e: &Engagement) -> bool {
    let on = |g: &Group| on_ground(scene, &g.centre);
    scene.by_id.get(&e.group).is_some_and(on) && !(was.iter()).any(|g| g.id == e.group && on(g))
}

pub(crate) fn answers(e: &Engagement, time: i32, first: bool) -> bool {
    e.phase != Phase::Forming || e.since == time || first || e.unanswered
}

const OUTPOST_LOSSES: i32 = 2;

pub fn step(facts: &Facts, e: &Engagement) -> Option<Engagement> {
    let e = &super::pruned(e, &facts.standing, &facts.aging_out);

    let toward = |phase: Phase| Some(enter(phase, facts.time, e));
    let quit = || Some(give_up(facts.held, facts.time, e));

    let spent = e.lost > e.budget + if facts.held { 0 } else { OUTPOST_LOSSES };

    // Closed while the raid stands, it reopens on it with a fresh budget.
    if e.phase == Phase::Disengaging {
        return match (facts.home && !facts.armed_in, facts.returned && !spent) {
            (true, _) => None,
            (_, true) => toward(Phase::Forming),
            _ => Some(e.clone()),
        };
    }

    // Losing sight is not the raid leaving (owner's call); a home is always in sight.
    if facts.group.is_none() && (facts.held || facts.clear) {
        return None;
    }

    // Closed, a raid still remembered would reopen it with a fresh budget;
    // Forming has already re-priced it and found nothing to cast.
    if facts.group.is_none() && e.ours.is_empty() && !facts.asked {
        let again = facts.remembered && e.phase != Phase::Forming && e.lost < OUTPOST_LOSSES;
        return again.then(|| enter(Phase::Forming, facts.time, e)).or_else(quit);
    }

    if facts.group.is_some() && spent {
        return quit();
    }

    let left = facts.group.as_ref().is_some_and(|g| g.room() != e.room);

    if e.phase == Phase::Forming && left && !facts.on_our_ground {
        return quit();
    }

    if e.phase == Phase::Forming && !facts.asked && !e.ours.is_empty() {
        return toward(Phase::Approaching);
    }

    if facts.group.is_none() {
        return Some(e.clone());
    }

    if matches!(e.phase, Phase::Approaching | Phase::Fighting) && left && facts.on_our_ground {
        return toward(Phase::Pursuing);
    }

    if matches!(e.phase, Phase::Approaching | Phase::Fighting) && left {
        return quit();
    }

    if e.phase == Phase::Approaching && facts.in_reach {
        return toward(Phase::Fighting);
    }

    if e.phase == Phase::Pursuing && !facts.on_our_ground {
        return quit();
    }

    Some(e.clone())
}

pub struct Home {
    pub controller: Controller,
    pub towered: bool,
    /// The unanswered Defend's group standing in the home; empty while answered.
    pub raid: Vec<Creep>,
    pub hostiles: Vec<Creep>,
    pub ours: Vec<Structure>,
    pub bodies: Vec<Creep>,
    pub time: i32,
    pub opened: i32,
    pub fallen: Vec<(Pos, Fallen)>,
}

/// A tap is range 1 from where the claimer stood at the tick's start and it
/// steps one tile a tick; one tile more covers a skipped tick.
const CLAIMER_REACH: i32 = engine::MELEE_RANGE + 2;

/// Two 16-CARRY haulers: a home that never holds them casts its hands again itself.
const REFILLING_FLOOR: i32 = 32;

fn reaches(structure: bool, step: i32, raid: &[&Creep], at: &Pos) -> bool {
    raid.iter().any(|h| {
        let wrecks = structure && h.live(Part::Work) > 0;
        let range = engine::strike_reach(|p| h.live(p) > 0 || p == Part::Attack && wrecks);
        range > 0 && h.pos.within(range + step, at)
    })
}

const KILL_DRIFT: i32 = 3;

fn half(lost: i32, whole: i32) -> bool {
    whole > 0 && 2 * lost >= whole
}

/// v1's refiller held a Refill, which is Economy's to know; a hauler's body stands in.
fn hand(body: &[impl Borrow<Part>], pos: &Pos) -> bool {
    part_count_in(body, Part::Carry) > 0
        && part_count_in(body, Part::Work) == 0
        && !tiles::on_edge(tiles::of_pos(pos))
}

fn razed(home: &Home) -> bool {
    let raid: Vec<&Creep> = home.raid.iter().filter(|h| h.owner != Owner::Invader).collect();

    let covered: BTreeSet<Pos> =
        home.ours.iter().filter(|s| s.kind == StructureType::Rampart).map(|s| s.pos).collect();

    let share = |kind: StructureType| -> (i32, i32) {
        let all: Vec<&Structure> = home.ours.iter().filter(|s| s.kind == kind).collect();

        let lost = all
            .iter()
            .filter(|s| !covered.contains(&s.pos) && reaches(true, 0, &raid, &s.pos))
            .count();

        (lost as i32, all.len() as i32)
    };

    let hands: Vec<&Creep> = (home.bodies.iter()).filter(|c| hand(&c.body, &c.pos)).collect();

    let struck = |c: &Creep| {
        c.hits < c.hits_max && !covered.contains(&c.pos) && reaches(false, 0, &raid, &c.pos)
    };

    // A raider steps a tile a tick from its kill; past a few it is not told from a stranger.
    let fell = |(pos, f): &&(Pos, Fallen)| {
        f.died >= home.opened
            && f.lived().is_some()
            && reaches(false, KILL_DRIFT.min(home.time - f.died), &raid, pos)
    };
    // Two of the dig fallen within a tombstone's life: a raid camping a
    // source kills faster than the spawn regrows, and no hand ever falls.
    let culled = (home.fallen.iter().filter(fell))
        .filter(|(_, f)| part_count_in(&f.body, Part::Work) > 0)
        .count()
        >= 2;
    let killed: i32 = (home.fallen.iter().filter(fell))
        .filter(|(pos, f)| hand(&f.body, pos))
        .map(|(_, f)| part_count_in(&f.body, Part::Carry))
        .sum();

    let left: i32 =
        hands.iter().filter(|c| !struck(c)).map(|c| part_count_in(&c.body, Part::Carry)).sum();
    let whole: i32 =
        hands.iter().map(|c| part_count_in(&c.body, Part::Carry)).sum::<i32>() + killed;
    let (spawn_lost, spawn_all) = share(StructureType::Spawn);
    let (bank_lost, bank_all) = share(StructureType::Extension);

    !raid.is_empty()
        && (half(spawn_lost, spawn_all)
            || bank_lost >= 2 && half(bank_lost, bank_all)
            || left < whole && whole >= REFILLING_FLOOR && left < REFILLING_FLOOR
            || culled)
}

pub(crate) fn keeps(kind: StructureType) -> bool {
    use StructureType::*;
    matches!(kind, Spawn | Tower | Storage | Terminal)
}

/// An unanswered raid is one the towers and ramparts are left to; the stock is
/// spent where they hold nothing, or what cannot move or regrow is falling.
fn last_resort(home: &Home) -> bool {
    let raid: Vec<&Creep> = home.raid.iter().collect();

    !home.raid.is_empty()
        && (!home.towered
            || home
                .ours
                .iter()
                .any(|s| keeps(s.kind) && s.hits < s.hits_max && reaches(true, 0, &raid, &s.pos))
            || razed(home))
}

pub(crate) fn running(world: &World) -> bool {
    world.rooms.iter().any(|r| r.controller.as_ref().is_some_and(|c| c.our_safe_mode() > 0))
}

pub fn safe_mode(running: bool, homes: &[Home]) -> Option<Id> {
    let claimer =
        |c: &Controller, h: &Creep| h.live(Part::Claim) > 0 && h.pos.within(CLAIMER_REACH, &c.pos);

    if running {
        None
    } else {
        homes
            .iter()
            .find(|h| {
                h.controller.safe_mode_fireable()
                    && (h.hostiles.iter().any(|c| claimer(&h.controller, c)) || last_resort(h))
            })
            .map(|h| h.controller.id)
    }
}

pub fn opens(
    priority: impl Fn(RoomName) -> Option<f64>,
    armed: impl Fn(&Id) -> bool,
    time: i32,
    targeted: &BTreeSet<RoomName>,
    groups: &[Group],
) -> Vec<(Group, f64)> {
    let mut ranked: Vec<(Group, f64)> = groups
        .iter()
        .filter(|g| g.last_seen == time && g.members.iter().any(&armed))
        .filter_map(|g| Some((g.clone(), priority(g.room())?)))
        .collect();
    ranked.sort_by(|(a, a_priority), (b, b_priority)| {
        b_priority
            .partial_cmp(a_priority)
            .unwrap_or(std::cmp::Ordering::Equal)
            .then(a.id.cmp(&b.id))
    });

    let mut targeted = targeted.clone();
    let mut opened: Vec<(Group, f64)> = Vec::new();

    for (g, priority) in ranked {
        if targeted.contains(&g.room()) {
            continue;
        }

        opened.push((g.clone(), priority));
        targeted.insert(g.room());
    }

    opened
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use fabot_base::world::{BodyPart, Store};
    use proptest::{collection::vec, prelude::*, sample::select};

    use super::*;
    use crate::shape::Objective;

    fn at(x: i32, y: i32) -> Pos {
        Pos::new(RoomName::W1N1, x, y)
    }

    fn creep(owner: Owner, kinds: &[Part], pos: Pos, hurt: bool) -> Creep {
        let body: Vec<BodyPart> =
            kinds.iter().map(|k| BodyPart { kind: *k, hits: 100, boost: None }).collect();
        Creep {
            id: format!("{},{}", pos.x, pos.y).as_str().into(),
            name: format!("{},{}", pos.x, pos.y),
            owner,
            pos,
            hits: if hurt { 50 } else { 100 } * kinds.len() as i32,
            hits_max: 100 * kinds.len() as i32,
            body,
            ticks_to_live: 1000,
            fatigue: 0,
            store: Store::default(),
        }
    }

    struct Dead {
        carry: i32,
        ticks_left: i32,
        died: i32,
    }

    const NOW: i32 = 1000;
    const OPENED: i32 = NOW - 10;

    fn slain(carry: i32) -> Dead {
        Dead { carry, ticks_left: 1000, died: NOW - 1 }
    }

    fn home(raider: Owner, haulers: &[(i32, i32, bool)], fallen: &[Dead]) -> Home {
        let raider = creep(raider, &[Part::RangedAttack, Part::Move], at(25, 25), false);

        let bodies: Vec<Creep> = haulers
            .iter()
            .enumerate()
            .map(|(i, (carry, dx, hurt))| {
                let mut kinds = vec![Part::Carry; *carry as usize];
                kinds.push(Part::Move);
                creep(Owner::Ours, &kinds, at(22 + dx, 23 + i as i32), *hurt)
            })
            .collect();

        let fallen: Vec<(Pos, Fallen)> = fallen
            .iter()
            .take(5)
            .enumerate()
            .map(|(i, dead)| {
                let mut body = vec![Part::Carry; dead.carry as usize];
                body.push(Part::Move);
                let fell = Fallen { body, ticks_to_live: dead.ticks_left, died: dead.died };
                (at(26, 23 + i as i32), fell)
            })
            .collect();

        Home {
            controller: Controller {
                id: "c".into(),
                pos: at(10, 10),
                owner: Owner::Ours,
                level: 5,
                progress: 0,
                progress_total: 1,
                ticks_to_downgrade: 100000,
                reservation: None,
                sign: None,
                safe_mode: 0,
                safe_mode_available: 1,
                safe_mode_cooldown: 0,
                upgrade_blocked: 0,
            },
            towered: true,
            raid: vec![raider.clone()],
            hostiles: vec![raider],
            ours: Vec::new(),
            bodies,
            time: NOW,
            opened: OPENED,
            fallen,
        }
    }

    fn rival(haulers: &[(i32, i32, bool)], fallen: &[Dead]) -> Home {
        home(Owner::Stranger("rival".to_string()), haulers, fallen)
    }

    /// Up to four haulers in a column across the raider's reach: CARRY 1–25 each, a step off it.
    fn haulers(spec: &[(i32, i32, bool)]) -> Vec<(i32, i32, bool)> {
        spec.iter().take(4).map(|(c, dx, hurt)| (1 + c.abs() % 25, dx.abs() % 7, *hurt)).collect()
    }

    /// Arms at what `haulers` folds to the least and most carry and step.
    fn spec() -> impl Strategy<Value = Vec<(i32, i32, bool)>> {
        let wide =
            || prop_oneof![1 => Just(0), 1 => Just(6), 1 => Just(24), 5 => -100_000..=100_000];
        vec((wide(), wide(), any::<bool>()), 0..5)
    }

    const CASES: u32 = 256;
    const PHASES: [Phase; 5] =
        [Phase::Forming, Phase::Approaching, Phase::Fighting, Phase::Pursuing, Phase::Disengaging];

    /// The last prefix sum under the floor, and so how many haulers it takes.
    fn under(carry: &[i32]) -> usize {
        let mut sum = 0;
        let mut last = 0;
        for (i, c) in carry.iter().enumerate() {
            sum += c;
            if sum < REFILLING_FLOOR {
                last = i + 1;
            }
        }
        last
    }

    fn oven(parts: usize, remaining: i32) -> Oven {
        let part = BodyPart { kind: Part::Move, hits: 100, boost: None };
        Oven { name: "economy".into(), remaining, body: vec![part; parts] }
    }

    #[test]
    fn a_spawn_on_a_body_begun_before_the_set_committed_never_passes_it_over() {
        // A 40-part hauler with 120 ticks left as a 108-tick set commits at 0.
        let since = 0;
        for time in since + 1..=since + 120 {
            let on = oven(40, 120 - time);
            assert!(!passed_over(time, since, 108, (&[&on], false)), "t{time}");
        }
        assert!(
            !passed_over(121, since, 108, (&[], false)),
            "a spawn come free waits on the queue"
        );
        assert!(!passed_over(100, since, 108, (&[&oven(10, 29)], false)), "inside the landing");
        assert!(
            passed_over(122, since, 108, (&[&oven(10, 29)], false)),
            "begun after it, past the landing"
        );
        assert!(
            passed_over(121, since, 108, (&[], true)),
            "a spawn short of the cast's energy, past the landing"
        );
    }

    #[test]
    fn a_forming_defend_casts_only_at_an_answer() {
        let forming = Engagement {
            phase: Phase::Forming,
            since: 10,
            predicted: Some(Outcome::Lose),
            unanswered: false,
            ..crate::scene::fresh(1, Objective::Defend, RoomName::W1N1, 0, Tactic::Close)
        };
        assert!(answers(&forming, 10, false), "the tick it began forming");
        assert!(!answers(&forming, 11, false), "a re-price while it forms");
        assert!(answers(&forming, 11, true), "its first answer");
        let starved = Engagement { unanswered: true, ..forming };
        assert!(answers(&starved, 11, false), "one it stood unanswered on");
    }

    #[test]
    fn a_cast_name_gives_back_the_tick_and_engagement_it_was_asked_for() {
        for (time, engagement, slot) in [(0, 1, 0), (27808, 12, 3), (i32::MAX, i32::MAX, Slot::MAX)]
        {
            let name = cast_name(time, engagement, slot);
            assert!(declarations::fights(&name));
            assert_eq!(cast_of(&name), Some((time, engagement)));
        }
        assert_eq!(cast_of("harvester-10-Spawn1"), None);
    }

    proptest! {
        #![proptest_config(fabot_base::testing::config(CASES))]

        #[test]
        fn a_carrier_in_reach_that_is_not_struck_never_spends_the_safe_mode(spec in spec()) {
            let mut unhurt: Vec<(i32, i32, bool)> = vec![(16, 0, false)];
            unhurt.extend(haulers(&spec).into_iter().map(|(c, dx, _)| (c, dx, false)));

            prop_assert_eq!(safe_mode(false, &[rival(&unhurt, &[])]), None);
        }

        #[test]
        fn a_raid_striking_every_hauler_of_a_home_at_the_floor_spends_it(spec in spec()) {
            let mut struck: Vec<(i32, i32, bool)> = vec![(16, 0, true), (16, 1, true)];
            struck.extend(haulers(&spec));
            let struck: Vec<(i32, i32, bool)> = struck
                .into_iter()
                .take(4)
                .map(|(c, dx, _)| (c, dx, true))
                .collect();

            prop_assert!(safe_mode(false, &[rival(&struck, &[])]).is_some());
        }

        #[test]
        fn a_home_that_never_held_the_floor_never_spends_it_on_its_carriers(spec in spec()) {
            let all = haulers(&spec);
            let carry: Vec<i32> = all.iter().map(|(c, _, _)| *c).collect();
            let keep = under(&carry);
            let under: Vec<(i32, i32, bool)> = all.into_iter().take(keep).collect();

            prop_assert_eq!(safe_mode(false, &[rival(&under, &[])]), None);
        }

        #[test]
        fn haulers_aged_out_beside_a_raid_never_spend_it(spec in spec(), aged in 0..6usize) {
            let unhurt: Vec<(i32, i32, bool)> = haulers(&spec)
                .into_iter()
                .map(|(c, dx, _)| (c, dx, false))
                .collect();

            let fallen: Vec<Dead> = (0..2 + aged)
                .map(|i| Dead {
                    carry: 16,
                    ticks_left: (i % 2) as i32 * engine::AGED_OUT_TICKS_TO_LIVE,
                    died: NOW - 1,
                })
                .collect();

            prop_assert_eq!(safe_mode(false, &[rival(&unhurt, &fallen)]), None);
        }

        #[test]
        fn haulers_killed_and_standing_under_the_floor_together_never_spend_it(
            spec in spec(),
            killed in spec(),
        ) {
            let mut all: Vec<(bool, (i32, i32, bool))> =
                haulers(&killed).into_iter().map(|h| (true, h)).collect();
            all.extend(haulers(&spec).into_iter().map(|h| (false, h)));

            let carry: Vec<i32> = all.iter().map(|(_, (c, _, _))| *c).collect();
            let keep = under(&carry);
            let under: Vec<(bool, (i32, i32, bool))> = all.into_iter().take(keep).collect();

            let standing: Vec<(i32, i32, bool)> = under
                .iter()
                .filter(|(dead, _)| !dead)
                .map(|(_, (c, dx, _))| (*c, *dx, true))
                .collect();
            let fallen: Vec<Dead> = under
                .iter()
                .filter(|(dead, _)| *dead)
                .map(|(_, (c, _, _))| slain(*c))
                .collect();

            prop_assert_eq!(safe_mode(false, &[rival(&standing, &fallen)]), None);
        }

        #[test]
        fn a_raid_killing_two_diggers_spends_it_and_one_never(n in 1..=3usize) {
            let mut h = rival(&[], &(0..n).map(|_| slain(1)).collect::<Vec<_>>());
            for (_, f) in &mut h.fallen {
                f.body.push(Part::Work);
            }

            prop_assert_eq!(safe_mode(false, &[h]).is_some(), n >= 2);
        }

        #[test]
        fn haulers_killed_beside_an_invader_raid_never_spend_it(spec in spec(), killed in spec()) {
            let fallen: Vec<Dead> = haulers(&killed)
                .into_iter()
                .map(|(c, _, _)| slain(c))
                .collect();

            let home = home(Owner::Invader, &haulers(&spec), &fallen);

            prop_assert_eq!(safe_mode(false, &[home]), None);
        }

        #[test]
        fn haulers_invaders_killed_before_a_player_raid_opened_never_spend_it(
            spec in spec(),
            earlier in prop_oneof![1 => Just(0), 1 => Just(u16::MAX), 6 => any::<u16>()],
        ) {
            let before = |carry: i32| Dead {
                carry,
                ticks_left: 1000,
                died: OPENED - 1 - i32::from(earlier),
            };

            let mut fallen = vec![before(16), before(16)];
            fallen.extend(haulers(&spec).into_iter().map(|(c, _, _)| before(c)));

            prop_assert_eq!(safe_mode(false, &[rival(&[], &fallen)]), None);
        }

        #[test]
        fn an_outpost_defend_beaten_past_its_bound_holds_while_the_raid_stands(
            phase in select(PHASES.to_vec()),
            lost in 1..=OUTPOST_LOSSES + 6,
            budget in 0..=3,
            (west, grouped) in any::<(bool, bool)>(),
            [in_reach, home, asked, on_our_ground, clear, remembered] in any::<[bool; 6]>(),
            returned in any::<bool>(),
        ) {
            let room = if west { "W1N1" } else { "W2N1" };
            let group = grouped.then(|| Group {
                id: 1,
                members: BTreeSet::from(["raider".into()]),
                last_seen: 0,
                centre: Pos::new(room.parse::<RoomName>().unwrap(), 25, 25),
            });
            let e = Engagement {
                phase,
                lost,
                budget,
                unanswered: false,
                ..crate::scene::fresh(1, Objective::Defend, RoomName::W1N1, 0, Tactic::Close)
            };
            let facts = Facts {
                time: 1,
                group,
                standing: BTreeSet::new(),
                aging_out: BTreeSet::new(),
                in_reach,
                home,
                held: false,
                asked,
                on_our_ground,
                returned,
                clear,
                armed_in: true,
                remembered,
            };

            let spent = e.lost > e.budget + OUTPOST_LOSSES;
            let back = e.phase == Phase::Disengaging && returned && !spent;
            let next = step(&facts, &e);
            if back || e.phase == Phase::Disengaging || facts.group.is_some() && spent {
                let next = next.as_ref().expect("a raid still standing keeps its room held");
                if back {
                    prop_assert_eq!((next.phase, next.since, next.budget), (Phase::Forming, 1, e.budget));
                } else {
                    prop_assert_eq!(next.phase, Phase::Disengaging);
                }
            }
            let formed = next.filter(|n| n.phase == Phase::Forming);
            if facts.group.is_some() && !back && let Some(next) = formed {
                prop_assert_eq!(next.since, e.since, "a seen raid never re-forms");
            }
        }

        #[test]
        fn a_blind_idle_outpost_defend_re_forms_while_its_raid_is_remembered(
            phase in select(PHASES[..4].to_vec()),
            lost in 0..=OUTPOST_LOSSES,
            budget in 0..=3,
            (unanswered, stands) in any::<(bool, bool)>(),
            [in_reach, home, asked, on_our_ground, armed_in, remembered] in any::<[bool; 6]>(),
        ) {
            let e = Engagement {
                phase,
                ours: BTreeMap::from([("fight-1-1-0".to_string(), 0)]),
                lost,
                budget,
                unanswered,
                ..crate::scene::fresh(1, Objective::Defend, RoomName::W1N1, 0, Tactic::Close)
            };
            let standing: BTreeSet<String> = e.ours.keys().filter(|_| stands).cloned().collect();
            let facts = Facts {
                time: 1,
                group: None,
                standing: standing.clone(),
                aging_out: BTreeSet::new(),
                in_reach,
                home,
                held: false,
                asked,
                on_our_ground,
                returned: false,
                clear: false,
                armed_in,
                remembered,
            };

            let next = step(&facts, &e).expect("a blind outpost Defend keeps its budget");
            let idle = standing.is_empty() && !facts.asked;
            let again = facts.remembered && e.phase != Phase::Forming && next.lost < OUTPOST_LOSSES;
            prop_assert_eq!(next.phase == Phase::Disengaging, idle && !again);
            let formed = next.phase == Phase::Forming && next.since == 1;
            prop_assert_eq!(formed, idle && again);
        }
    }
}
