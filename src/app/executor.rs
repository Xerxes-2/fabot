//! `Executor` turns each intent into one API call (`ARCHITECTURE.md` §1).
use js_sys::{Array, Object};
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::{JsCast, JsValue};

use crate::app::world;
use fabot_core::intents::{
    ControllerAct, CreepAct, Intent, LabAct, LinkAct, Refusal, RoomAct, SpawnAct, TerminalAct,
    TowerAct,
};
use fabot_core::vocabulary;

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

fn on(verb: &str, target: &str, f: impl FnOnce(&JsValue) -> Result<f64, JsValue>) -> (String, i32) {
    match world::object_by_id(target) {
        None => (verb.to_string(), NOT_FOUND),
        Some(t) => outcome(verb, f(&t)),
    }
}

fn act(intent: &Intent) -> (String, i32) {
    match intent {
        Intent::Creep(name, act) => {
            let Some(creep) = world::creep_by_name(name) else {
                return ("creep".to_string(), NOT_FOUND);
            };
            let creep = actor(&creep);
            match act {
                CreepAct::Move(direction) => outcome("move", creep.move_(*direction)),
                CreepAct::Say(message) => outcome("say", creep.say(message)),
                CreepAct::Harvest(id) => on("harvest", id, |t| creep.harvest(t)),
                CreepAct::Transfer(id, resource) => on("transfer", id, |t| {
                    creep.transfer(t, &vocabulary::resource_name(resource))
                }),
                CreepAct::Withdraw(id, resource) => on("withdraw", id, |t| {
                    creep.withdraw(t, &vocabulary::resource_name(resource))
                }),
                CreepAct::Pickup(id) => on("pickup", id, |t| creep.pickup(t)),
                CreepAct::Drop(resource) => {
                    outcome("drop", creep.drop(&vocabulary::resource_name(resource)))
                }
                CreepAct::Build(id) => on("build", id, |t| creep.build(t)),
                CreepAct::Repair(id) => on("repair", id, |t| creep.repair(t)),
                CreepAct::Upgrade(id) => on("upgrade", id, |t| creep.upgrade_controller(t)),
                CreepAct::Reserve(id) => on("reserve", id, |t| creep.reserve_controller(t)),
                CreepAct::Unreserve(id) => on("unreserve", id, |t| creep.attack_controller(t)),
                CreepAct::ClaimController(id) => on("claim", id, |t| creep.claim_controller(t)),
                CreepAct::AttackCreep(id) => on("attack", id, |t| creep.attack(t)),
                CreepAct::RangedAttackCreep(id) => {
                    on("rangedAttack", id, |t| creep.ranged_attack(t))
                }
                CreepAct::RangedMassAttack => {
                    outcome("rangedMassAttack", creep.ranged_mass_attack())
                }
                CreepAct::HealCreep(id) => on("heal", id, |t| creep.heal(t)),
                CreepAct::RangedHealCreep(id) => on("rangedHeal", id, |t| creep.ranged_heal(t)),
                CreepAct::Dismantle(id) => on("dismantle", id, |t| creep.dismantle(t)),
            }
        }
        Intent::Room(name, RoomAct::CreateSite(x, y, kind)) => {
            let verb = format!("site:{}", kind.name());
            match world::room_by_name(*name) {
                None => (verb, NOT_FOUND),
                Some(room) => {
                    let site = actor(&room).create_construction_site(*x, *y, &kind.name());
                    outcome(&verb, site)
                }
            }
        }
        Intent::Room(_, RoomAct::Destroy(id)) => match world::object_by_id(id) {
            None => ("destroy".to_string(), NOT_FOUND),
            Some(s) => outcome("destroy", actor(&s).destroy()),
        },
        Intent::Tower(id, act) => match world::object_by_id(id) {
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
        Intent::Controller(id, ControllerAct::ActivateSafeMode) => match world::object_by_id(id) {
            None => ("safeMode".to_string(), NOT_FOUND),
            Some(controller) => outcome("safeMode", actor(&controller).activate_safe_mode()),
        },
        Intent::Controller(id, ControllerAct::Unclaim) => match world::object_by_id(id) {
            None => ("unclaim".to_string(), NOT_FOUND),
            Some(controller) => outcome("unclaim", actor(&controller).unclaim()),
        },
        Intent::Link(id, LinkAct::TransferEnergy(target, amount)) => {
            match world::object_by_id(id) {
                None => ("link".to_string(), NOT_FOUND),
                Some(link) => on("link", target, |t| actor(&link).transfer_energy(t, *amount)),
            }
        }
        Intent::Lab(id, LabAct::RunReaction(a, b)) => match (
            world::object_by_id(id),
            world::object_by_id(a),
            world::object_by_id(b),
        ) {
            (Some(lab), Some(a), Some(b)) => {
                outcome("runReaction", actor(&lab).run_reaction(&a, &b))
            }
            _ => ("runReaction".to_string(), NOT_FOUND),
        },
        Intent::Lab(id, LabAct::BoostCreep(creep)) => {
            match (world::object_by_id(id), world::object_by_id(creep)) {
                (Some(lab), Some(creep)) => outcome("boostCreep", actor(&lab).boost_creep(&creep)),
                _ => ("boostCreep".to_string(), NOT_FOUND),
            }
        }
        Intent::Terminal(id, TerminalAct::Send(resource, amount, room)) => {
            match world::object_by_id(id) {
                None => ("send".to_string(), NOT_FOUND),
                Some(t) => {
                    let name = vocabulary::resource_name(resource);
                    outcome("send", actor(&t).send(&name, *amount, &room.to_string()))
                }
            }
        }
        Intent::Observe(id, room) => match world::object_by_id(id) {
            None => ("observeRoom".to_string(), NOT_FOUND),
            Some(observer) => outcome(
                "observeRoom",
                actor(&observer).observe_room(&room.to_string()),
            ),
        },
        Intent::Queue(_, _) | Intent::Wish(_, _) => ("unsettled".to_string(), NOT_FOUND),
        Intent::Trace(creep, scored) => {
            let tiles: Vec<String> = scored
                .iter()
                .map(|(p, score)| format!("[\"{}\",{},{},{score}]", p.room, p.x, p.y))
                .collect();
            let line = format!(r#"{{"creep":"{creep}","tiles":[{}]}}"#, tiles.join(","));
            crate::app::wire::trace(&line);
            ("trace".to_string(), 0)
        }
        Intent::Count(_, _) | Intent::Short(_, _, _) | Intent::Held(_) => ("count".to_string(), 0),
        Intent::Spawn(id, SpawnAct::RecycleCreep(creep)) => {
            match (world::object_by_id(id), world::object_by_id(creep)) {
                (Some(spawn), Some(creep)) => {
                    outcome("recycle", actor(&spawn).recycle_creep(&creep))
                }
                _ => ("recycle".to_string(), NOT_FOUND),
            }
        }
        Intent::Spawn(id, SpawnAct::SpawnCreep(body, name, drawn)) => match world::object_by_id(id)
        {
            None => ("spawn".to_string(), NOT_FOUND),
            Some(spawn) => {
                let parts = Array::new();
                for part in body {
                    parts.push(&JsValue::from(*part));
                }
                let options: SpawnOptions = Object::new().unchecked_into();
                if !drawn.is_empty() {
                    let structures: Array = drawn
                        .iter()
                        .filter_map(|id| world::object_by_id(id))
                        .collect();
                    options.set_energy_structures(&structures);
                }
                outcome("spawn", actor(&spawn).spawn_creep(&parts, name, &options))
            }
        },
    }
}

/// One bad intent costs itself, never the rest of the tick's.
pub fn run(intents: &[Intent]) -> Vec<Refusal> {
    let mut out = Vec::new();
    for intent in intents {
        let (act_name, code) = act(intent);
        if code != 0 {
            out.push(Refusal {
                actor: intent.actor(),
                act: act_name,
                code,
            });
        }
    }
    out
}
