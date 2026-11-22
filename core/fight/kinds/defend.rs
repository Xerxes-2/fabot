/// `Defend` (`ARCHITECTURE.md` §6.2): a phase moves on an event, never a clock;
/// `step` closes it with None.
use std::collections::BTreeSet;

use super::super::shape::{Engagement, Group, Phase, enter};
use fabot_base::engine;
use fabot_base::tiles::{self};
use fabot_base::vocabulary::Part;
use fabot_base::world::{
    self, Controller, Creep, Fallen, Id, Owner, Pos, RoomName, Structure, StructureType,
};
use fabot_tactics::Tactic;

/// What a `Defend` may fight as, in the order its ties fall to (§6.3).
pub const CANDIDATES: &[Tactic] = &[
    Tactic::Hold,
    Tactic::Close,
    Tactic::Kite,
    Tactic::BlobKite,
    Tactic::BlobClose,
];

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
    pub clear: bool,
    pub raided: bool,
    pub remembered: bool,
}

/// A home's Defend never disengages: it stops asking, holds and is unanswered.
/// Neither arm is priced, so the offers behind the last price go with it.
pub fn give_up(held: bool, time: i32, e: &Engagement) -> Engagement {
    let mut next = if held {
        Engagement {
            unanswered: true,
            ..e.clone()
        }
    } else {
        enter(Phase::Disengaging, time, e)
    };
    next.plan.offers.clear();
    next
}

pub fn stands_alone(time: i32, e: &Engagement) -> Engagement {
    if e.phase == Phase::Forming && !e.ours.is_empty() {
        enter(Phase::Approaching, time, e)
    } else {
        e.clone()
    }
}

const OUTPOST_LOSSES: i32 = 2;

pub fn step(facts: &Facts, e: &Engagement) -> Option<Engagement> {
    let mut next = e.clone();
    next.ours.retain(|n, _| facts.standing.contains(n));
    next.lost = e.lost + (e.ours.len() - next.ours.len()) as i32;
    next.ours.retain(|n, _| !facts.aging_out.contains(n));
    let e = &next;

    let toward = |phase: Phase| Some(enter(phase, facts.time, e));
    let quit = || Some(give_up(facts.held, facts.time, e));

    // Closed while the raid stands, it reopens on it with a fresh budget.
    if e.phase == Phase::Disengaging {
        return if facts.home && !facts.raided {
            None
        } else {
            Some(e.clone())
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
        return again
            .then(|| enter(Phase::Forming, facts.time, e))
            .or_else(quit);
    }

    if facts.group.is_some() && e.lost > e.budget {
        return if facts.held || e.lost >= OUTPOST_LOSSES {
            quit()
        } else {
            toward(Phase::Forming)
        };
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
    pub fallen: Vec<Fallen>,
}

/// A tap is range 1 from where the claimer stood at the tick's start and it
/// steps one tile a tick; one tile more covers a skipped tick.
const CLAIMER_REACH: i32 = engine::MELEE_RANGE + 2;

/// Two 16-CARRY haulers: a home that never holds them casts its hands again itself.
const REFILLING_FLOOR: i32 = 32;

fn reaches(structure: bool, step: i32, raid: &[&Creep], at: &Pos) -> bool {
    raid.iter().any(|h| {
        let range = if h.live(Part::RangedAttack) > 0 {
            Some(engine::RANGED_RANGE)
        } else if h.live(Part::Attack) > 0 || structure && h.live(Part::Work) > 0 {
            Some(engine::MELEE_RANGE)
        } else {
            None
        };

        range.is_some_and(|range| h.pos.within(range + step, at))
    })
}

const KILL_DRIFT: i32 = 3;

fn half(lost: i32, whole: i32) -> bool {
    whole > 0 && 2 * lost >= whole
}

fn carry(c: &Creep) -> i32 {
    world::cast(&c.body, Part::Carry)
}

/// v1's refiller held a Refill, which is Economy's to know; a hauler's body stands in.
fn hand(c: &Creep) -> bool {
    carry(c) > 0
        && !c.body.iter().any(|p| p.kind == Part::Work)
        && !tiles::on_edge(tiles::of_pos(&c.pos))
}

fn razed(home: &Home) -> bool {
    let raid: Vec<&Creep> = home
        .raid
        .iter()
        .filter(|h| h.owner != Owner::Invader)
        .collect();

    let covered: BTreeSet<Pos> = home
        .ours
        .iter()
        .filter(|s| s.kind == StructureType::Rampart)
        .map(|s| s.pos)
        .collect();

    let share = |kind: StructureType| -> (i32, i32) {
        let all: Vec<&Structure> = home.ours.iter().filter(|s| s.kind == kind).collect();

        let lost = all
            .iter()
            .filter(|s| !covered.contains(&s.pos) && reaches(true, 0, &raid, &s.pos))
            .count();

        (lost as i32, all.len() as i32)
    };

    let hands: Vec<&Creep> = home.bodies.iter().filter(|c| hand(c)).collect();

    let struck = |c: &Creep| {
        c.hits < c.hits_max && !covered.contains(&c.pos) && reaches(false, 0, &raid, &c.pos)
    };

    // A raider steps a tile a tick from its kill; past a few it is not told from a stranger.
    let fell = |f: &&Fallen| {
        f.died >= home.opened
            && f.creep.owner.is_ours()
            && f.creep.ticks_to_live > engine::AGED_OUT_TICKS_TO_LIVE
            && reaches(
                false,
                KILL_DRIFT.min(home.time - f.died),
                &raid,
                &f.creep.pos,
            )
    };
    // Two of the dig fallen within a tombstone's life: a raid camping a
    // source kills faster than the spawn regrows, and no hand ever falls.
    let dug = |c: &Creep| c.body.iter().any(|p| p.kind == Part::Work);
    let culled = home
        .fallen
        .iter()
        .filter(fell)
        .filter(|f| dug(&f.creep))
        .count()
        >= 2;
    let killed: Vec<&Creep> = (home.fallen.iter().filter(fell))
        .filter(|f| hand(&f.creep))
        .map(|f| &f.creep)
        .collect();

    let left: i32 = hands.iter().filter(|c| !struck(c)).map(|c| carry(c)).sum();
    let whole: i32 =
        hands.iter().map(|c| carry(c)).sum::<i32>() + killed.iter().map(|c| carry(c)).sum::<i32>();
    let (spawn_lost, spawn_all) = share(StructureType::Spawn);
    let (bank_lost, bank_all) = share(StructureType::Extension);

    !raid.is_empty()
        && (half(spawn_lost, spawn_all)
            || bank_lost >= 2 && half(bank_lost, bank_all)
            || left < whole && whole >= REFILLING_FLOOR && left < REFILLING_FLOOR
            || culled)
}

fn keeps(kind: StructureType) -> bool {
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
    use super::*;
    use crate::shape::Objective;
    use fabot_base::world::{BodyPart, Store};
    use std::collections::BTreeMap;

    fn at(x: i32, y: i32) -> Pos {
        Pos::new(RoomName::W1N1, x, y)
    }

    fn creep(owner: Owner, kinds: &[Part], pos: Pos, hurt: bool) -> Creep {
        let body: Vec<BodyPart> = kinds
            .iter()
            .map(|k| BodyPart {
                kind: *k,
                hits: 100,
                boost: None,
            })
            .collect();
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
            store: Store::empty(),
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
        Dead {
            carry,
            ticks_left: 1000,
            died: NOW - 1,
        }
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

        let fallen: Vec<Fallen> = fallen
            .iter()
            .take(5)
            .enumerate()
            .map(|(i, dead)| {
                let mut kinds = vec![Part::Carry; dead.carry as usize];
                kinds.push(Part::Move);
                let mut gone = creep(Owner::Ours, &kinds, at(26, 23 + i as i32), false);
                gone.ticks_to_live = dead.ticks_left;
                Fallen {
                    creep: gone,
                    died: dead.died,
                }
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
        spec.iter()
            .take(4)
            .map(|(c, dx, hurt)| (1 + c.abs() % 25, dx.abs() % 7, *hurt))
            .collect()
    }

    struct Rng(u64);

    impl Rng {
        fn next(&mut self) -> u64 {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            self.0
        }

        fn below(&mut self, n: u64) -> u64 {
            self.next() % n
        }

        fn between(&mut self, lo: i32, hi: i32) -> i32 {
            lo + self.below((hi - lo + 1) as u64) as i32
        }

        fn flip(&mut self) -> bool {
            self.below(2) == 0
        }

        fn spec(&mut self) -> Vec<(i32, i32, bool)> {
            let n = self.below(5) as usize;
            (0..n)
                .map(|_| {
                    (
                        self.between(-100_000, 100_000),
                        self.between(-100_000, 100_000),
                        self.flip(),
                    )
                })
                .collect()
        }

        fn flags(&mut self) -> Vec<bool> {
            let n = self.below(6) as usize;
            (0..n).map(|_| self.flip()).collect()
        }

        fn word(&mut self) -> u16 {
            self.below(1 << 16) as u16
        }
    }

    const DRAWS: usize = 100;

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

    #[test]
    fn a_carrier_in_reach_that_is_not_struck_never_spends_the_safe_mode() {
        let mut rng = Rng(0x9e37_79b9_7f4a_7c15);

        for _ in 0..DRAWS {
            let spec = rng.spec();
            let mut unhurt: Vec<(i32, i32, bool)> = vec![(16, 0, false)];
            unhurt.extend(haulers(&spec).into_iter().map(|(c, dx, _)| (c, dx, false)));

            assert_eq!(safe_mode(false, &[rival(&unhurt, &[])]), None);
        }
    }

    #[test]
    fn a_raid_striking_every_hauler_of_a_home_at_the_floor_spends_it() {
        let mut rng = Rng(0x2545_f491_4f6c_dd1d);

        for _ in 0..DRAWS {
            let spec = rng.spec();
            let mut struck: Vec<(i32, i32, bool)> = vec![(16, 0, true), (16, 1, true)];
            struck.extend(haulers(&spec));
            let struck: Vec<(i32, i32, bool)> = struck
                .into_iter()
                .take(4)
                .map(|(c, dx, _)| (c, dx, true))
                .collect();

            assert!(safe_mode(false, &[rival(&struck, &[])]).is_some());
        }
    }

    #[test]
    fn a_home_that_never_held_the_floor_never_spends_it_on_its_carriers() {
        let mut rng = Rng(0x853c_49e6_748f_ea9b);

        for _ in 0..DRAWS {
            let all = haulers(&rng.spec());
            let carry: Vec<i32> = all.iter().map(|(c, _, _)| *c).collect();
            let keep = under(&carry);
            let under: Vec<(i32, i32, bool)> = all.into_iter().take(keep).collect();

            assert_eq!(safe_mode(false, &[rival(&under, &[])]), None);
        }
    }

    #[test]
    fn haulers_aged_out_beside_a_raid_never_spend_it() {
        let mut rng = Rng(0x1b03_7f4a_7c15_9e37);

        for _ in 0..DRAWS {
            let spec = rng.spec();
            let aged = rng.flags();
            let unhurt: Vec<(i32, i32, bool)> = haulers(&spec)
                .into_iter()
                .map(|(c, dx, _)| (c, dx, false))
                .collect();

            let fallen: Vec<Dead> = (0..2 + aged.len())
                .map(|_| Dead {
                    carry: 16,
                    ticks_left: engine::AGED_OUT_TICKS_TO_LIVE,
                    died: NOW - 1,
                })
                .collect();

            assert_eq!(safe_mode(false, &[rival(&unhurt, &fallen)]), None);
        }
    }

    #[test]
    fn haulers_killed_and_standing_under_the_floor_together_never_spend_it() {
        let mut rng = Rng(0x94d0_49bb_1331_11eb);

        for _ in 0..DRAWS {
            let spec = rng.spec();
            let killed = rng.spec();
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

            assert_eq!(safe_mode(false, &[rival(&standing, &fallen)]), None);
        }
    }

    #[test]
    fn a_raid_killing_two_diggers_spends_it_and_one_never() {
        let mut rng = Rng(0x5851_f42d_4c95_7f2d);

        for _ in 0..DRAWS {
            let n = 1 + rng.below(3) as usize;
            let mut h = rival(&[], &(0..n).map(|_| slain(1)).collect::<Vec<_>>());
            for f in &mut h.fallen {
                f.creep.body.push(BodyPart {
                    kind: Part::Work,
                    hits: 100,
                    boost: None,
                });
            }

            assert_eq!(safe_mode(false, &[h]).is_some(), n >= 2);
        }
    }

    #[test]
    fn haulers_killed_beside_an_invader_raid_never_spend_it() {
        let mut rng = Rng(0xc0ff_ee00_1234_5678);

        for _ in 0..DRAWS {
            let spec = rng.spec();
            let killed = rng.spec();
            let fallen: Vec<Dead> = haulers(&killed)
                .into_iter()
                .map(|(c, _, _)| slain(c))
                .collect();

            let home = home(Owner::Invader, &haulers(&spec), &fallen);

            assert_eq!(safe_mode(false, &[home]), None);
        }
    }

    #[test]
    fn haulers_invaders_killed_before_a_player_raid_opened_never_spend_it() {
        let mut rng = Rng(0x0bad_c0de_dead_beef);

        for _ in 0..DRAWS {
            let spec = rng.spec();
            let earlier = i32::from(rng.word());
            let before = |carry: i32| Dead {
                carry,
                ticks_left: 1000,
                died: OPENED - 1 - earlier,
            };

            let mut fallen = vec![before(16), before(16)];
            fallen.extend(haulers(&spec).into_iter().map(|(c, _, _)| before(c)));

            assert_eq!(safe_mode(false, &[rival(&[], &fallen)]), None);
        }
    }

    #[test]
    fn an_outpost_defend_beaten_past_its_bound_holds_while_the_raid_stands() {
        let mut rng = Rng(0x5eed_0854_0ff1_ce00);
        let phases = [
            Phase::Forming,
            Phase::Approaching,
            Phase::Fighting,
            Phase::Pursuing,
            Phase::Disengaging,
        ];

        for _ in 0..DRAWS {
            let room = if rng.flip() { "W1N1" } else { "W2N1" };
            let group = rng.flip().then(|| Group {
                id: 1,
                members: BTreeSet::from(["raider".into()]),
                last_seen: 0,
                centre: Pos::new(room.parse::<RoomName>().unwrap(), 25, 25),
            });
            let e = Engagement {
                phase: phases[rng.below(phases.len() as u64) as usize],
                lost: rng.between(OUTPOST_LOSSES, OUTPOST_LOSSES + 2),
                budget: rng.between(0, 3),
                unanswered: false,
                ..crate::scene::fresh(1, Objective::Defend, RoomName::W1N1, 0, Tactic::Close)
            };
            let facts = Facts {
                time: 1,
                group,
                standing: BTreeSet::new(),
                aging_out: BTreeSet::new(),
                in_reach: rng.flip(),
                home: rng.flip(),
                held: false,
                asked: rng.flip(),
                on_our_ground: rng.flip(),
                clear: rng.flip(),
                raided: true,
                remembered: rng.flip(),
            };

            let beaten = facts.group.is_some() && e.lost > e.budget;
            if e.phase == Phase::Disengaging || beaten {
                let next = step(&facts, &e).expect("a raid still standing keeps its room held");
                assert_eq!(next.phase, Phase::Disengaging);
            }
        }
    }

    #[test]
    fn a_blind_idle_outpost_defend_re_forms_while_its_raid_is_remembered() {
        let mut rng = Rng(0x0b11_d0ef_e2d5_0000);
        let phases = [
            Phase::Forming,
            Phase::Approaching,
            Phase::Fighting,
            Phase::Pursuing,
        ];

        for _ in 0..DRAWS {
            let e = Engagement {
                phase: phases[rng.below(phases.len() as u64) as usize],
                ours: BTreeMap::from([("fight-1-1-0".to_string(), 0)]),
                lost: rng.between(0, OUTPOST_LOSSES),
                budget: rng.between(0, 3),
                unanswered: rng.flip(),
                ..crate::scene::fresh(1, Objective::Defend, RoomName::W1N1, 0, Tactic::Close)
            };
            let standing: BTreeSet<String> =
                e.ours.keys().filter(|_| rng.flip()).cloned().collect();
            let facts = Facts {
                time: 1,
                group: None,
                standing: standing.clone(),
                aging_out: BTreeSet::new(),
                in_reach: rng.flip(),
                home: rng.flip(),
                held: false,
                asked: rng.flip(),
                on_our_ground: rng.flip(),
                clear: false,
                raided: rng.flip(),
                remembered: rng.flip(),
            };

            let next = step(&facts, &e).expect("a blind outpost Defend keeps its budget");
            let idle = standing.is_empty() && !facts.asked;
            let again = facts.remembered && e.phase != Phase::Forming && next.lost < OUTPOST_LOSSES;
            assert_eq!(next.phase == Phase::Disengaging, idle && !again);
            let formed = next.phase == Phase::Forming && next.since == 1;
            assert_eq!(formed, idle && again);
        }
    }
}
