//! Invariants of the tick runner (`ARCHITECTURE.md` §2): a module's fault
//! costs that module alone, a switched-off module does nothing, and the
//! accounting covers every module.
use std::cell::Cell;
use std::collections::BTreeMap;

use super::Dice;
use crate::core::atlas::Atlas;
use crate::core::intents::{CreepAct, Intent};
use crate::core::state::State;
use crate::core::tick::{self, Context, Module, Outcome, TickResult};
use crate::core::world::World;

const NAMES: [&str; 8] = ["m0", "m1", "m2", "m3", "m4", "m5", "m6", "m7"];

/// Module `I` says its name once, as creep `cI`.
fn say<const I: usize>(_: &mut Context, state: &State) -> Result<(Vec<Intent>, State), String> {
    Ok((
        vec![Intent::Creep(
            format!("c{I}"),
            CreepAct::Say(NAMES[I].to_string()),
        )],
        state.clone(),
    ))
}

fn broke<const I: usize>(_: &mut Context, _: &State) -> Result<(Vec<Intent>, State), String> {
    Err(format!("{} broke", NAMES[I]))
}

macro_rules! modules {
    ($($i:literal)*) => {
        fn module_of(i: usize, throws: bool) -> Module {
            let run = match (i, throws) {
                $(($i, false) => say::<$i>, ($i, true) => broke::<$i>,)*
                _ => unreachable!(),
            };
            Module { name: NAMES[i], run }
        }
    };
}
modules!(0 1 2 3 4 5 6 7);

fn throws_of(dice: &mut Dice) -> Vec<bool> {
    (0..dice.between(0, NAMES.len() as i32))
        .map(|_| dice.chance(2))
        .collect()
}

fn run(throws: &[bool], state: State) -> TickResult {
    let world = World {
        time: 1,
        me: String::new(),
        cpu_limit: 20.0,
        bucket: 10_000,
        rooms: Vec::new(),
        spawns: Vec::new(),
        creeps: Vec::new(),
        our_sites: 0,
        rooms_allowed: 1,
    };
    let mut atlas = Atlas::of_world(&world);
    let declarations = tick::undeclared();
    let sightings = BTreeMap::new();
    let mut context = Context {
        world: &world,
        atlas: &mut atlas,
        declarations: &declarations,
        sightings: &sightings,
        intel: &BTreeMap::new(),
    };
    let modules: Vec<Module> = throws
        .iter()
        .enumerate()
        .map(|(i, &t)| module_of(i, t))
        .collect();
    // A clock that advances one unit a reading.
    let t = Cell::new(0.0);
    let clock = || {
        t.set(t.get() + 1.0);
        t.get()
    };
    tick::run(clock, &modules, &mut context, state)
}

/// What a module said.
fn said(intents: &[Intent]) -> Vec<(String, String)> {
    intents
        .iter()
        .filter_map(|i| match i {
            Intent::Creep(name, CreepAct::Say(message)) => Some((name.clone(), message.clone())),
            _ => None,
        })
        .collect()
}

#[test]
fn a_throw_costs_its_own_modules_intents_and_no_others() {
    let mut dice = Dice::new(51);
    for _ in 0..100 {
        let throws = throws_of(&mut dice);
        let result = run(&throws, State::empty());
        let expected: Vec<(String, String)> = throws
            .iter()
            .enumerate()
            .filter(|(_, t)| !**t)
            .map(|(i, _)| (format!("c{i}"), NAMES[i].to_string()))
            .collect();
        assert_eq!(said(&result.intents), expected, "{throws:?}");
    }
}

#[test]
fn every_module_is_reported_once_in_order_faults_named_then_the_settling() {
    let mut dice = Dice::new(52);
    for _ in 0..100 {
        let throws = throws_of(&mut dice);
        let result = run(&throws, State::empty());
        assert_eq!(result.reports.len(), throws.len() + 1);
        assert_eq!(result.reports.last().unwrap().module, "settle");
        for (report, (i, &t)) in result.reports.iter().zip(throws.iter().enumerate()) {
            assert_eq!(report.module, NAMES[i]);
            match &report.outcome {
                Outcome::Faulted(message) => assert!(t && *message == format!("m{i} broke")),
                Outcome::Ran => assert!(!t),
                Outcome::Off => panic!("{} was not switched off", NAMES[i]),
            }
        }
    }
}

#[test]
fn a_switched_off_module_runs_nothing_and_spends_nothing() {
    let mut dice = Dice::new(53);
    for _ in 0..100 {
        let mut throws = throws_of(&mut dice);
        throws.truncate(NAMES.len() - 1);
        throws.insert(0, false);
        let off = dice.below(throws.len() as i32) as usize;
        let mut state = State::empty();
        state.off.insert(NAMES[off].to_string());
        let result = run(&throws, state);
        let report = &result.reports[off];
        assert_eq!(report.outcome, Outcome::Off);
        assert_eq!(report.cpu, 0.0);
        assert!(!said(&result.intents).contains(&(format!("c{off}"), NAMES[off].to_string())));
    }
}
