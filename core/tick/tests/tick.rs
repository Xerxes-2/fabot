//! Invariants of the tick runner: a switched-off module does nothing, and
//! the accounting covers every module.
use std::cell::Cell;
use std::collections::BTreeMap;

use crate::state::State;
use crate::tick::{self, Module, TickResult};
use fabot_atlas::{self as atlas, Atlas, Context};
use fabot_base::declarations;
use fabot_base::intents::{Action, CreepAct, ModuleOutput};
use fabot_base::world::World;
use proptest::prelude::*;
use proptest::sample::Index;

const CASES: u32 = 256;
const NAMES: [&str; 8] = ["m0", "m1", "m2", "m3", "m4", "m5", "m6", "m7"];

/// Module `I` says its name once, as creep `cI`.
fn say<const I: usize>(_: &mut Context, _: &mut State) -> ModuleOutput {
    vec![Action::Creep(
        format!("c{I}"),
        CreepAct::Say(NAMES[I].to_string()),
    )]
    .into()
}

const SAYS: [tick::ModuleRun; 8] = [
    say::<0>, say::<1>, say::<2>, say::<3>, say::<4>, say::<5>, say::<6>, say::<7>,
];

fn run(count: usize, state: State) -> TickResult {
    let world = World {
        time: 1,
        me: String::new(),
        cpu_limit: 20.0,
        bucket: 10_000,
        rooms: Vec::new(),
        spawns: Vec::new(),
        creeps: Vec::new(),
        attacks: Vec::new(),
        destroyed: Vec::new(),
        our_sites: 0,
        rooms_allowed: 1,
        closed: Default::default(),
        rules: fabot_base::world::Rules::vanilla(),
    };
    let mut atlas = Atlas::of_world(&world);
    let declarations = declarations::Declarations::default();
    let sightings = BTreeMap::new();
    let derived = atlas::derive(&world, &mut atlas);
    let mut context = Context {
        world: &world,
        atlas: &mut atlas,
        declarations: &declarations,
        sightings: &sightings,
        intel: &BTreeMap::new(),
        derived: &derived,
        trace: false,
    };
    let modules: Vec<Module> = (0..count)
        .map(|i| Module {
            name: NAMES[i],
            run: SAYS[i],
        })
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
fn said(intents: &[Action]) -> Vec<(String, String)> {
    intents
        .iter()
        .filter_map(|i| match i {
            Action::Creep(name, CreepAct::Say(message)) => Some((name.clone(), message.clone())),
            _ => None,
        })
        .collect()
}

proptest! {
    #![proptest_config(fabot_base::testing::config(CASES))]

    #[test]
    fn every_module_is_reported_once_in_order_then_the_settling(count in 0..=NAMES.len()) {
        let result = run(count, State::default());
        prop_assert_eq!(result.reports.len(), count + 1);
        prop_assert_eq!(result.reports.last().map(|r| r.module), Some("settle"));
        for (report, name) in result.reports.iter().zip(&NAMES[..count]) {
            prop_assert_eq!(report.module, *name);
            prop_assert!(report.cpu > 0.0);
        }
    }

    #[test]
    fn a_switched_off_module_runs_nothing_and_spends_nothing(count in 1..=NAMES.len(), off in any::<Index>()) {
        let off = off.index(count);
        let mut state = State::default();
        state.off.insert(NAMES[off].to_string());
        let result = run(count, state);
        let report = &result.reports[off];
        prop_assert_eq!(report.cpu, 0.0);
        let spoke = (format!("c{off}"), NAMES[off].to_string());
        prop_assert!(!said(&result.settled.actions).contains(&spoke));
    }
}
