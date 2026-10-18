//! `Executor` turns each intent into one API call (`ARCHITECTURE.md` §1).
use crate::app::js::field;
use js_sys::{Array, Function, Reflect};
use wasm_bindgen::{JsCast, JsValue};

use crate::app::world;
use crate::core::intents::{
    ControllerAct, CreepAct, Intent, LabAct, LinkAct, RoomAct, SpawnAct, TerminalAct, TowerAct,
};
use crate::core::vocabulary;

#[derive(Clone, Debug)]
pub struct Refusal {
    pub actor: String,
    pub act: String,
    pub code: i32,
}

const NOT_FOUND: i32 = -5;
const THREW: i32 = -100;

fn args(values: &[JsValue]) -> Array {
    let array = Array::new();
    for v in values {
        array.push(v);
    }
    array
}

fn call(obj: &JsValue, verb: &str, values: &[JsValue]) -> Result<i32, ()> {
    let function = field(obj, verb);
    let result: Result<JsValue, JsValue> =
        Reflect::apply(function.unchecked_ref::<Function>(), obj, &args(values));
    match result {
        Ok(v) => Ok(v.as_f64().map_or(0, |n| n as i32)),
        Err(_) => Err(()),
    }
}

fn num(n: i32) -> JsValue {
    JsValue::from(n)
}

fn on(verb: &str, target: &str, f: impl FnOnce(&JsValue) -> Result<i32, ()>) -> (String, i32) {
    match world::object_by_id(target) {
        None => (verb.to_string(), NOT_FOUND),
        Some(t) => match f(&t) {
            Ok(code) => (verb.to_string(), code),
            Err(()) => ("threw".to_string(), THREW),
        },
    }
}

fn act(intent: &Intent) -> (String, i32) {
    match intent {
        Intent::Creep(name, act) => {
            let Some(creep) = world::creep_by_name(name) else {
                return ("creep".to_string(), NOT_FOUND);
            };
            let verb = |n: &str| n.to_string();
            match act {
                CreepAct::Move(direction) => match call(&creep, "move", &[num(*direction)]) {
                    Ok(code) => (verb("move"), code),
                    Err(()) => (verb("threw"), THREW),
                },
                CreepAct::Say(message) => {
                    match call(&creep, "say", &[JsValue::from_str(message)]) {
                        Ok(code) => (verb("say"), code),
                        Err(()) => (verb("threw"), THREW),
                    }
                }
                CreepAct::Harvest(id) => on("harvest", id, |t| {
                    call(&creep, "harvest", std::slice::from_ref(t))
                }),
                CreepAct::Transfer(id, resource) => on("transfer", id, |t| {
                    call(
                        &creep,
                        "transfer",
                        &[
                            t.clone(),
                            JsValue::from_str(&vocabulary::resource_name(resource)),
                        ],
                    )
                }),
                CreepAct::Withdraw(id, resource) => on("withdraw", id, |t| {
                    call(
                        &creep,
                        "withdraw",
                        &[
                            t.clone(),
                            JsValue::from_str(&vocabulary::resource_name(resource)),
                        ],
                    )
                }),
                CreepAct::Pickup(id) => on("pickup", id, |t| {
                    call(&creep, "pickup", std::slice::from_ref(t))
                }),
                CreepAct::Build(id) => on("build", id, |t| {
                    call(&creep, "build", std::slice::from_ref(t))
                }),
                CreepAct::Repair(id) => on("repair", id, |t| {
                    call(&creep, "repair", std::slice::from_ref(t))
                }),
                CreepAct::Upgrade(id) => on("upgrade", id, |t| {
                    call(&creep, "upgradeController", std::slice::from_ref(t))
                }),
                CreepAct::Reserve(id) => on("reserve", id, |t| {
                    call(&creep, "reserveController", std::slice::from_ref(t))
                }),
                CreepAct::Unreserve(id) => on("unreserve", id, |t| {
                    call(&creep, "attackController", std::slice::from_ref(t))
                }),
                CreepAct::ClaimController(id) => on("claim", id, |t| {
                    call(&creep, "claimController", std::slice::from_ref(t))
                }),
                CreepAct::AttackCreep(id) => on("attack", id, |t| {
                    call(&creep, "attack", std::slice::from_ref(t))
                }),
                CreepAct::RangedAttackCreep(id) => on("rangedAttack", id, |t| {
                    call(&creep, "rangedAttack", std::slice::from_ref(t))
                }),
                CreepAct::RangedMassAttack => match call(&creep, "rangedMassAttack", &[]) {
                    Ok(code) => (verb("rangedMassAttack"), code),
                    Err(()) => (verb("threw"), THREW),
                },
                CreepAct::HealCreep(id) => on("heal", id, |t| {
                    call(&creep, "heal", std::slice::from_ref(t))
                }),
                CreepAct::RangedHealCreep(id) => on("rangedHeal", id, |t| {
                    call(&creep, "rangedHeal", std::slice::from_ref(t))
                }),
                CreepAct::Dismantle(id) => on("dismantle", id, |t| {
                    call(&creep, "dismantle", std::slice::from_ref(t))
                }),
            }
        }
        Intent::Room(name, RoomAct::CreateSite(x, y, kind)) => {
            let verb = format!("site:{}", kind.name());
            match world::room_by_name(name) {
                None => (verb, NOT_FOUND),
                Some(room) => match call(
                    &room,
                    "createConstructionSite",
                    &[num(*x), num(*y), JsValue::from_str(&kind.name())],
                ) {
                    Ok(code) => (verb, code),
                    Err(()) => ("threw".to_string(), THREW),
                },
            }
        }
        Intent::Tower(id, act) => match world::object_by_id(id) {
            None => ("tower".to_string(), NOT_FOUND),
            Some(tower) => match act {
                TowerAct::Attack(target) => on("attack", target, |t| {
                    call(&tower, "attack", std::slice::from_ref(t))
                }),
                TowerAct::Heal(target) => on("heal", target, |t| {
                    call(&tower, "heal", std::slice::from_ref(t))
                }),
            },
        },
        Intent::Controller(id, ControllerAct::ActivateSafeMode) => match world::object_by_id(id) {
            None => ("safeMode".to_string(), NOT_FOUND),
            Some(controller) => match call(&controller, "activateSafeMode", &[]) {
                Ok(code) => ("safeMode".to_string(), code),
                Err(()) => ("threw".to_string(), THREW),
            },
        },
        Intent::Link(id, LinkAct::TransferEnergy(target, amount)) => {
            match world::object_by_id(id) {
                None => ("link".to_string(), NOT_FOUND),
                Some(link) => on("link", target, |t| {
                    call(&link, "transferEnergy", &[t.clone(), num(*amount)])
                }),
            }
        }
        Intent::Lab(id, LabAct::RunReaction(a, b)) => match (
            world::object_by_id(id),
            world::object_by_id(a),
            world::object_by_id(b),
        ) {
            (Some(lab), Some(a), Some(b)) => match call(&lab, "runReaction", &[a, b]) {
                Ok(code) => ("runReaction".to_string(), code),
                Err(()) => ("threw".to_string(), THREW),
            },
            _ => ("runReaction".to_string(), NOT_FOUND),
        },
        Intent::Terminal(id, TerminalAct::Send(resource, amount, room)) => {
            match world::object_by_id(id) {
                None => ("send".to_string(), NOT_FOUND),
                Some(t) => {
                    let name = JsValue::from_str(&vocabulary::resource_name(resource));
                    match call(&t, "send", &[name, num(*amount), JsValue::from_str(room)]) {
                        Ok(code) => ("send".to_string(), code),
                        Err(()) => ("threw".to_string(), THREW),
                    }
                }
            }
        }
        Intent::Observe(id, room) => match world::object_by_id(id) {
            None => ("observeRoom".to_string(), NOT_FOUND),
            Some(observer) => match call(&observer, "observeRoom", &[JsValue::from_str(room)]) {
                Ok(code) => ("observeRoom".to_string(), code),
                Err(()) => ("threw".to_string(), THREW),
            },
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
        Intent::Count(_, _) => ("count".to_string(), 0),
        Intent::Spawn(id, SpawnAct::SpawnCreep(body, name)) => match world::object_by_id(id) {
            None => ("spawn".to_string(), NOT_FOUND),
            Some(spawn) => {
                let parts = Array::new();
                for part in body {
                    parts.push(&JsValue::from_str(vocabulary::part_name(*part)));
                }
                match call(
                    &spawn,
                    "spawnCreep",
                    &[parts.into(), JsValue::from_str(name)],
                ) {
                    Ok(code) => ("spawn".to_string(), code),
                    Err(()) => ("threw".to_string(), THREW),
                }
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
