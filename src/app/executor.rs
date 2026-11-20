//! `Executor` turns each intent into one API call (`ARCHITECTURE.md` §1).
use js_sys::{Array, Object};
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::{JsCast, JsValue};

use crate::app::world;
use fabot_base::intents::{
    Action, ControllerAct, CreepAct, LabAct, LinkAct, Refusal, RoomAct, SpawnAct, TerminalAct,
    TowerAct,
};
use fabot_base::vocabulary;
use fabot_base::world::Id;

const NOT_FOUND: i32 = -5;
const THREW: i32 = -100;

// Typed imports, not `Reflect::apply`: one seam call each. The engine returns
// a number or `undefined`, which reads NaN and casts to 0.
#[wasm_bindgen]
extern "C" {
    type Actor;
    #[wasm_bindgen(method, catch, js_name = move)]
    fn move_(this: &Actor, direction: i32) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch)]
    fn say(this: &Actor, message: &str) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch)]
    fn harvest(this: &Actor, target: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch)]
    fn transfer(this: &Actor, target: &JsValue, resource: &str) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch)]
    fn withdraw(this: &Actor, target: &JsValue, resource: &str) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch)]
    fn pickup(this: &Actor, target: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch)]
    fn drop(this: &Actor, resource: &str) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch)]
    fn build(this: &Actor, target: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch)]
    fn repair(this: &Actor, target: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = upgradeController)]
    fn upgrade_controller(this: &Actor, target: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = reserveController)]
    fn reserve_controller(this: &Actor, target: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = attackController)]
    fn attack_controller(this: &Actor, target: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = claimController)]
    fn claim_controller(this: &Actor, target: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch)]
    fn attack(this: &Actor, target: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = rangedAttack)]
    fn ranged_attack(this: &Actor, target: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = rangedMassAttack)]
    fn ranged_mass_attack(this: &Actor) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch)]
    fn heal(this: &Actor, target: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = rangedHeal)]
    fn ranged_heal(this: &Actor, target: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch)]
    fn dismantle(this: &Actor, target: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = createConstructionSite)]
    fn create_construction_site(this: &Actor, x: i32, y: i32, kind: &str) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch)]
    fn destroy(this: &Actor) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = activateSafeMode)]
    fn activate_safe_mode(this: &Actor) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch)]
    fn unclaim(this: &Actor) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = transferEnergy)]
    fn transfer_energy(this: &Actor, target: &JsValue, amount: i32) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = runReaction)]
    fn run_reaction(this: &Actor, a: &JsValue, b: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = boostCreep)]
    fn boost_creep(this: &Actor, creep: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch)]
    fn send(this: &Actor, resource: &str, amount: i32, room: &str) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = observeRoom)]
    fn observe_room(this: &Actor, room: &str) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = recycleCreep)]
    fn recycle_creep(this: &Actor, creep: &JsValue) -> Result<f64, JsValue>;
    #[wasm_bindgen(method, catch, js_name = spawnCreep)]
    fn spawn_creep(
        this: &Actor,
        body: &Array,
        name: &str,
        options: &SpawnOptions,
    ) -> Result<f64, JsValue>;

    type SpawnOptions;
    #[wasm_bindgen(method, setter, js_name = energyStructures)]
    fn set_energy_structures(this: &SpawnOptions, structures: &Array);
}

fn outcome(verb: &str, result: Result<f64, JsValue>) -> (String, i32) {
    match result {
        Ok(code) => (verb.to_string(), code as i32),
        Err(_) => ("threw".to_string(), THREW),
    }
}

fn actor(v: &JsValue) -> &Actor {
    v.unchecked_ref()
}

fn on(verb: &str, target: &Id, f: impl FnOnce(&JsValue) -> Result<f64, JsValue>) -> (String, i32) {
    match world::object_by_id(target) {
        None => (verb.to_string(), NOT_FOUND),
        Some(t) => outcome(verb, f(&t)),
    }
}

fn act(intent: &Action) -> (String, i32) {
    match intent {
        Action::Creep(name, act) => {
            let Some(creep) = world::creep_by_name(name) else {
                return ("creep".to_string(), NOT_FOUND);
            };
            let creep = actor(&creep);
            let verb: &str = act.into();
            match act {
                CreepAct::Move(direction) => outcome(verb, creep.move_(*direction)),
                CreepAct::Say(message) => outcome(verb, creep.say(message)),
                CreepAct::Harvest(id) => on(verb, id, |t| creep.harvest(t)),
                CreepAct::Transfer(id, resource) => on(verb, id, |t| {
                    creep.transfer(t, &vocabulary::resource_name(resource))
                }),
                CreepAct::Withdraw(id, resource) => on(verb, id, |t| {
                    creep.withdraw(t, &vocabulary::resource_name(resource))
                }),
                CreepAct::Pickup(id) => on(verb, id, |t| creep.pickup(t)),
                CreepAct::Drop(resource) => {
                    outcome(verb, creep.drop(&vocabulary::resource_name(resource)))
                }
                CreepAct::Build(id) => on(verb, id, |t| creep.build(t)),
                CreepAct::Repair(id) => on(verb, id, |t| creep.repair(t)),
                CreepAct::Upgrade(id) => on(verb, id, |t| creep.upgrade_controller(t)),
                CreepAct::Reserve(id) => on(verb, id, |t| creep.reserve_controller(t)),
                CreepAct::Unreserve(id) => on(verb, id, |t| creep.attack_controller(t)),
                CreepAct::ClaimController(id) => on(verb, id, |t| creep.claim_controller(t)),
                CreepAct::AttackCreep(id) => on(verb, id, |t| creep.attack(t)),
                CreepAct::RangedAttackCreep(id) => on(verb, id, |t| creep.ranged_attack(t)),
                CreepAct::RangedMassAttack => outcome(verb, creep.ranged_mass_attack()),
                CreepAct::HealCreep(id) => on(verb, id, |t| creep.heal(t)),
                CreepAct::RangedHealCreep(id) => on(verb, id, |t| creep.ranged_heal(t)),
                CreepAct::Dismantle(id) => on(verb, id, |t| creep.dismantle(t)),
            }
        }
        Action::Room(name, RoomAct::CreateSite(x, y, kind)) => {
            let verb = format!("site:{}", kind.as_ref());
            match world::room_by_name(*name) {
                None => (verb, NOT_FOUND),
                Some(room) => {
                    let site = actor(&room).create_construction_site(*x, *y, kind.as_ref());
                    outcome(&verb, site)
                }
            }
        }
        Action::Room(_, RoomAct::Destroy(id)) => on("destroy", id, |s| actor(s).destroy()),
        Action::Tower(id, act) => match world::object_by_id(id) {
            None => ("tower".to_string(), NOT_FOUND),
            Some(tower) => {
                let tower = actor(&tower);
                match act {
                    TowerAct::Attack(target) => on("attack", target, |t| tower.attack(t)),
                    TowerAct::Heal(target) => on("heal", target, |t| tower.heal(t)),
                    TowerAct::Repair(target) => on("repair", target, |t| tower.repair(t)),
                }
            }
        },
        Action::Controller(id, ControllerAct::ActivateSafeMode) => {
            on("safeMode", id, |c| actor(c).activate_safe_mode())
        }
        Action::Controller(id, ControllerAct::Unclaim) => on("unclaim", id, |c| actor(c).unclaim()),
        Action::Link(id, LinkAct::TransferEnergy(target, amount)) => {
            match world::object_by_id(id) {
                None => ("link".to_string(), NOT_FOUND),
                Some(link) => on("link", target, |t| actor(&link).transfer_energy(t, *amount)),
            }
        }
        Action::Lab(id, LabAct::RunReaction(a, b)) => match (
            world::object_by_id(id),
            world::object_by_id(a),
            world::object_by_id(b),
        ) {
            (Some(lab), Some(a), Some(b)) => {
                outcome("runReaction", actor(&lab).run_reaction(&a, &b))
            }
            _ => ("runReaction".to_string(), NOT_FOUND),
        },
        Action::Lab(id, LabAct::BoostCreep(creep)) => {
            match (world::object_by_id(id), world::object_by_id(creep)) {
                (Some(lab), Some(creep)) => outcome("boostCreep", actor(&lab).boost_creep(&creep)),
                _ => ("boostCreep".to_string(), NOT_FOUND),
            }
        }
        Action::Terminal(id, TerminalAct::Send(resource, amount, room)) => on("send", id, |t| {
            let name = vocabulary::resource_name(resource);
            actor(t).send(&name, *amount, &room.to_string())
        }),
        Action::Observe(id, room) => on("observeRoom", id, |o| {
            actor(o).observe_room(&room.to_string())
        }),
        Action::Spawn(id, SpawnAct::RecycleCreep(creep)) => {
            match (world::object_by_id(id), world::object_by_id(creep)) {
                (Some(spawn), Some(creep)) => {
                    outcome("recycle", actor(&spawn).recycle_creep(&creep))
                }
                _ => ("recycle".to_string(), NOT_FOUND),
            }
        }
        Action::Spawn(id, SpawnAct::SpawnCreep(body, name, drawn)) => on("spawn", id, |spawn| {
            let parts: Array = body.iter().map(|p| JsValue::from(*p)).collect();
            let options: SpawnOptions = Object::new().unchecked_into();
            if !drawn.is_empty() {
                let structures: Array = drawn.iter().filter_map(world::object_by_id).collect();
                options.set_energy_structures(&structures);
            }
            actor(spawn).spawn_creep(&parts, name, &options)
        }),
    }
}

/// One bad intent costs itself, never the rest of the tick's.
pub fn run(intents: &[Action]) -> Vec<Refusal> {
    (intents.iter())
        .filter_map(|intent| {
            let (act, code) = act(intent);
            (code != 0).then(|| Refusal {
                actor: intent.actor(),
                act,
                code,
            })
        })
        .collect()
}
