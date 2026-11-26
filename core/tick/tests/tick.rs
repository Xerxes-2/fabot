//! Invariants of the tick runner: a switched-off module does nothing, and
//! the accounting covers every module.
use std::{cell::Cell, collections::BTreeMap};

use fabot_atlas::{self as atlas, Atlas, Context};
use fabot_base::{
    declarations,
    intents::{Action, CreepAct, ModuleOutput},
    world::World,
};
use proptest::{prelude::*, sample::Index};

use crate::{
    state::State,
    tick::{self, Module, TickResult},
};

const CASES: u32 = 256;
const NAMES: [&str; 8] = ["m0", "m1", "m2", "m3", "m4", "m5", "m6", "m7"];

/// Module `I` says its name once, as creep `cI`.
fn say<const I: usize>(_: &mut Context, _: &mut State) -> ModuleOutput {
    let say = CreepAct::Say(NAMES[I].to_string());
    ModuleOutput { actions: vec![Action::Creep(format!("c{I}"), say)], ..ModuleOutput::default() }
}

const SAYS: [tick::ModuleRun; 8] =
    [say::<0>, say::<1>, say::<2>, say::<3>, say::<4>, say::<5>, say::<6>, say::<7>];

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
    let modules: Vec<Module> =
        (0..count).map(|i| Module { name: NAMES[i], run: SAYS[i] }).collect();
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

/// The engine refuses an act of a structure past its level's allowance
/// (ERR_RCL_NOT_ENOUGH): a colony fallen a level orders none, in any module.
#[test]
fn no_module_orders_a_structure_past_its_level() {
    use fabot_base::{
        declarations::{Declared, Outpost},
        testing,
        world::{Id, Pos, RoomName, Spawn, StructureType, terrain},
    };
    for level in [7, 8] {
        let home: RoomName = "W2N2".parse().unwrap();
        let unseen: RoomName = "W3N2".parse().unwrap();
        let plain = terrain::leak(vec![0; 2500]).unwrap();
        let at = |y| Pos::new(home, 25, y);
        let mut room = testing::ours(home, level, plain);
        room.energy_available = 12_900;
        room.energy_capacity = 12_900;
        let mut world = testing::world(r#"{"rooms":[]}"#);
        for (i, y) in [5, 15, 20].into_iter().enumerate() {
            let id = format!("s{i}");
            room.structures.push(testing::built(&id, StructureType::Spawn, at(y)));
            world.spawns.push(Spawn {
                id: id.as_str().into(),
                name: id,
                pos: at(y),
                spawning: None,
            });
        }
        room.structures.push(testing::built("o", StructureType::Observer, at(40)));
        world.rooms.push(room);
        let declared = Declared { id: Id::MISREAD, pos: Pos::new(unseen, 25, 25) };
        let outpost = Outpost {
            reserved: true,
            room: unseen,
            controller: declared.clone(),
            sources: vec![declared],
        };
        let declarations =
            declarations::Declarations { outposts: vec![outpost], ..Default::default() };
        let mut intel = BTreeMap::new();
        let decided = tick::decide(
            (|| 0.0, 0.0),
            &mut Vec::new(),
            (&world, false),
            (declarations, State::default()),
            (BTreeMap::new(), Atlas::default()),
            &mut intel,
            (&|_| Some(plain), [(home, plain.get_bits()), (unseen, plain.get_bits())]),
        );
        let working: Vec<Id> = world.rules.working(&world.rooms[0]).map(|s| s.id).collect();
        let ordered: Vec<Id> = (decided.result.settled.actions.iter())
            .filter_map(|a| match a {
                Action::Spawn(id, _) | Action::Observe(id, _) => Some(*id),
                _ => None,
            })
            .collect();
        let spawned = decided.result.settled.actions.iter().any(|a| matches!(a, Action::Spawn(..)));
        assert!(spawned, "level {level}: nothing cast");
        assert!(ordered.iter().all(|id| working.contains(id)), "level {level}: {ordered:?}");
        assert_eq!(ordered.contains(&"o".into()), level == 8, "{ordered:?}");
    }
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
