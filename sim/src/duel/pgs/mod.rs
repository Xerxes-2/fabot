//! `sim duel --rollout R`: Portfolio Greedy Search (Churchill & Buro 2013)
//! over each body's script, the duel's own world the forward model, a
//! rollout scored by each side's living bodies and hits left.
use std::collections::BTreeMap;
use std::time::Instant;

use krebs::world::{RoomsMeta, TerrainStore, World};
use krebs_pathfinder::PathFinder;
use screeps::Part;
use serde_json::{Value, json};

use super::{Duel, Orders, Sample, Side, Thing, View};

pub const FLAGS: [(&str, u64); 6] = [
    ("--rollout", 0),
    ("--replan", 10),
    ("--rounds", 1),
    ("--ours", 1),
    ("--theirs", 0),
    ("--check-fork", 0),
];

/// krebs's `World` holds no `Clone`, so a fork copies its fields by hand: a field krebs adds is
/// missed silently until `--check-fork` sees a forked duel end otherwise.
fn fork(d: &Duel) -> Duel {
    let w = &d.world;
    let (meta, terrain) = (RoomsMeta::new(Vec::new()), TerrainStore::new(Vec::new()));
    let mut f = World::new(
        w.seed,
        w.clock,
        (*w.compat).clone(),
        meta,
        terrain,
        w.rooms.clone(),
        w.users.clone(),
    );
    (f.time, f.next_seq, f.compat, f.config) =
        (w.time, w.next_seq, w.compat.clone(), w.config.clone());
    (f.meta, f.terrain, f.access) = (w.meta.clone(), w.terrain.clone(), w.access.clone());
    (f.market, f.env, f.mods) = (w.market.clone(), w.env.clone(), w.mods.clone());
    f.ids.set_counter(w.ids.counter());
    w.active.list().iter().for_each(|&r| f.active.insert(r));
    f.reindex();
    let (first, now) = (d.first.clone(), d.now.clone());
    Duel {
        world: f,
        first,
        now,
        ..*d
    }
}

/// The emitter's duel forked every tick; whether it ends as the unforked one does.
pub fn check_fork(s: &Sample, ticks: u64, seed: u64, pf: &mut PathFinder) -> Value {
    let mut d = Duel::new(s, seed);
    let emit = |o: &mut Orders, c: &Thing, v: &View| o.act(c, v);
    while d.t < ticks && {
        d = fork(&d);
        d.tick(&mut d.engine(), pf, [&emit, &emit])
    } {}
    json!({ "same": d.result() == super::duel(s, ticks, seed, pf) })
}

/// Whom a body fires on, and the goals it steps toward, or away from when `flee`.
struct Aim<'v> {
    target: &'v Thing,
    goals: Vec<(&'v Thing, u32)>,
    flee: bool,
}

impl<'v> Aim<'v> {
    fn toward(target: &'v Thing, range: Option<u32>) -> Option<Aim<'v>> {
        let goals = range.map_or_default(|r| vec![(target, r)]);
        Some(Aim {
            target,
            goals,
            flee: false,
        })
    }
}

fn close<'v>(c: &Thing, v: &View<'v>) -> Option<Aim<'v>> {
    let target = c.closest(&v.foes)?;
    Aim::toward(target, (c.range(target) > 1).then_some(1))
}

fn kite<'v>(c: &Thing, v: &View<'v>) -> Option<Aim<'v>> {
    let target = c.closest(&v.foes)?;
    let armed = |f: &&&Thing| f.has(Part::Attack) || f.has(Part::RangedAttack);
    let near: Vec<_> = (v.foes.iter().filter(armed))
        .filter(|f| c.range(f) <= 2)
        .map(|f| (*f, 3))
        .collect();
    if near.is_empty() {
        return Aim::toward(target, (c.range(target) > 3).then_some(3));
    }
    Some(Aim {
        target,
        goals: near,
        flee: true,
    })
}

fn focus<'v>(c: &Thing, v: &View<'v>) -> Option<Aim<'v>> {
    let target = (v.foes.iter().copied()).min_by(|a, b| a.hits.total_cmp(&b.hits))?;
    let reach = if c.has(Part::Attack) { 1 } else { 3 };
    Aim::toward(target, (c.range(target) > reach).then_some(reach as u32))
}

fn hold<'v>(c: &Thing, v: &View<'v>) -> Option<Aim<'v>> {
    Aim::toward(c.closest(&v.foes)?, None)
}

fn retreat<'v>(c: &Thing, v: &View<'v>) -> Option<Aim<'v>> {
    let target = c.closest(&v.foes)?;
    if !v.foes.iter().any(|f| c.range(f) <= 4) {
        return Aim::toward(target, None);
    }
    let goals = v.foes.iter().map(|f| (*f, 5)).collect();
    Some(Aim {
        target,
        goals,
        flee: true,
    })
}

/// A body's script by name: none aims, the emitter acts on its own.
#[derive(Clone, Copy)]
struct Script(
    &'static str,
    Option<for<'v> fn(&Thing, &View<'v>) -> Option<Aim<'v>>>,
);

impl PartialEq for Script {
    fn eq(&self, other: &Script) -> bool {
        self.0 == other.0
    }
}

const PORTFOLIO: [Script; 6] = [
    Script("Emit", None),
    Script("Close", Some(close)),
    Script("Kite", Some(kite)),
    Script("Focus", Some(focus)),
    Script("Hold", Some(hold)),
    Script("Retreat", Some(retreat)),
];

pub struct Search {
    pub rollout: u64,
    pub replan: u64,
    pub rounds: u64,
    /// Whether each side, by `Side`, searches; one that does not keeps the emitter.
    pub sides: [bool; 2],
}

impl Orders<'_> {
    fn play(&mut self, script: Script, c: &Thing, v: &View) {
        let Script(_, Some(aim)) = script else {
            return self.act(c, v);
        };
        let Some(Aim {
            target,
            goals,
            flee,
        }) = aim(c, v)
        else {
            return;
        };
        let within = |r: i32| {
            let near = v.foes.iter().copied().filter(|f| c.range(f) <= r);
            (c.range(target) <= r)
                .then_some(target)
                .or_else(|| near.min_by(|a, b| a.hits.total_cmp(&b.hits)))
        };
        if let Some(f) = within(1).filter(|_| c.has(Part::Attack)) {
            self.put(c, "attack", Orders::at(f));
        } else if c.has(Part::Heal) && c.hurt() {
            self.put(c, "heal", Orders::at(c));
        } else if c.has(Part::Heal) {
            self.mend(c, &v.allies);
        }
        if c.has(Part::RangedAttack) {
            if v.foes.iter().filter(|f| c.range(f) <= 1).count() >= 2 {
                self.put(c, "rangedMassAttack", json!({}));
            } else if let Some(f) = within(3) {
                self.put(c, "rangedAttack", json!({ "id": f.id }));
            }
        }
        if !goals.is_empty() {
            self.step(c, &goals, flee);
        }
    }
}

/// Each side's bodies, by their place in its first roll, and the script each plays.
type Assignment = [(Vec<String>, Vec<Script>); 2];

impl Duel {
    fn tick_assigned(&mut self, pf: &mut PathFinder, assignment: &Assignment) -> bool {
        let side = |side: Side| {
            move |o: &mut Orders, c: &Thing, v: &View| {
                let (ids, scripts) = &assignment[side as usize];
                let at = ids
                    .iter()
                    .position(|id| *id == c.id)
                    .expect("one of the side's");
                o.play(scripts[at], c, v);
            }
        };
        self.tick(
            &mut self.engine(),
            pf,
            [&side(Side::Ours), &side(Side::Theirs)],
        )
    }

    /// Each side's mean body worth, a living body half its share and the rest its hits left; ours less theirs.
    fn worth(&self) -> f64 {
        let side = |side: Side| {
            let (first, now) = (&self.first[side as usize], &self.now[side as usize]);
            let left = |(id, h0): (&String, &f64)| now.get(id).map_or(0.0, |h| 0.5 + 0.5 * h / h0);
            first.iter().map(left).sum::<f64>() / first.len() as f64
        };
        side(Side::Ours) - side(Side::Theirs)
    }

    /// Our living bodies by their place in the first roll, none before contact.
    fn engaged(&self) -> impl Iterator<Item = usize> + '_ {
        let ours = self.first[Side::Ours as usize].keys().enumerate();
        let alive = ours.filter(|(_, id)| {
            self.contact.is_some() && self.now[Side::Ours as usize].contains_key(*id)
        });
        alive.map(|(k, _)| k)
    }
}

fn rollout(d: &Duel, pf: &mut PathFinder, assignment: &Assignment, horizon: u64) -> f64 {
    let mut d = fork(d);
    while d.t < horizon && d.tick_assigned(pf, assignment) {}
    d.read();
    d.worth()
}

/// `side`'s best assignment found from the one it plays, the other's held, and the rollouts it took.
fn improve(
    d: &Duel,
    pf: &mut PathFinder,
    assignment: &Assignment,
    side: Side,
    search: &Search,
    ticks: u64,
) -> (Vec<Script>, u64) {
    let horizon = (d.t + search.rollout).min(ticks);
    let sign = if side == Side::Ours { 1.0 } else { -1.0 };
    let mut score = |s: &[Script]| {
        let mut tried = assignment.clone();
        tried[side as usize].1 = s.to_vec();
        sign * rollout(d, pf, &tried, horizon)
    };
    let last = &assignment[side as usize].1;
    let mut best = (score(last), last.clone());
    let mut rollouts = 1;
    let mut better = |best: &mut (f64, Vec<Script>), s: Vec<Script>, rollouts: &mut u64| {
        *rollouts += 1;
        let v = score(&s);
        if v > best.0 + 1e-9 {
            *best = (v, s);
        }
    };
    for p in PORTFOLIO {
        if last.iter().any(|s| *s != p) {
            better(&mut best, vec![p; last.len()], &mut rollouts);
        }
    }
    for _ in 0..search.rounds {
        for b in 0..last.len() {
            let held = best.1[b];
            for p in PORTFOLIO.into_iter().filter(|p| *p != held) {
                let mut s = best.1.clone();
                s[b] = p;
                better(&mut best, s, &mut rollouts);
            }
        }
    }
    (best.1, rollouts)
}

/// Adds to the duel's result the ticks each of our bodies ran each script after contact, the
/// assignments as they changed, the decisions, the rollouts and their milliseconds.
pub fn duel(s: &Sample, ticks: u64, seed: u64, pf: &mut PathFinder, search: &Search) -> Value {
    let mut d = Duel::new(s, seed);
    let mut assignment: Assignment = [Side::Ours, Side::Theirs].map(|side| {
        let ids: Vec<String> = d.first[side as usize].keys().cloned().collect();
        let scripts = vec![PORTFOLIO[0]; ids.len()];
        (ids, scripts)
    });
    // The engine's AI drives an Invader: no script of theirs to search.
    let invaded = s.theirs.bodies.iter().any(|b| b.invader);
    let searches = |k: &Side| search.sides[*k as usize] && !(invaded && *k == Side::Theirs);
    let sides = [Side::Theirs, Side::Ours].into_iter().filter(searches);
    let sides: Vec<Side> = sides.collect();
    let mut ran: BTreeMap<String, BTreeMap<String, u64>> = BTreeMap::new();
    let (mut decisions, mut rollouts, mut ms, mut changes) = (0, 0, 0.0, Vec::new());
    while d.t < ticks {
        if d.t.is_multiple_of(search.replan) {
            let began = Instant::now();
            for &side in &sides {
                let (best, n) = improve(&d, pf, &assignment, side, search, ticks);
                (assignment[side as usize].1, rollouts) = (best, rollouts + n);
            }
            decisions += 1;
            ms += began.elapsed().as_secs_f64() * 1000.0;
            if changes.last().is_none_or(|(_, last)| *last != assignment) {
                changes.push((d.t, assignment.clone()));
            }
        }
        if !d.tick_assigned(pf, &assignment) {
            break;
        }
        for k in d.engaged() {
            *ran.entry(k.to_string())
                .or_default()
                .entry(assignment[Side::Ours as usize].1[k].0.to_string())
                .or_default() += 1;
        }
    }
    let mut out = d.result();
    out["scripts"] = json!(ran);
    let named = |s: &[Script]| s.iter().map(|s| s.0).collect::<Vec<_>>();
    out["plans"] = json!(
        changes
            .iter()
            .map(|(t, p)| json!([t, named(&p[0].1), named(&p[1].1)]))
            .collect::<Vec<_>>()
    );
    out["decisions"] = json!(decisions);
    out["rollouts"] = json!(rollouts);
    out["ms"] = json!(ms);
    out
}
