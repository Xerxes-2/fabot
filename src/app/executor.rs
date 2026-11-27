//! `Executor` hands the tick's intents to `exec.js` in one call, which makes
//! each its API call (`ARCHITECTURE.md` §1).
use std::fmt::{Display, Write};

use fabot_base::{
    intents::{
        Action, ControllerAct, CreepAct, LabAct, LinkAct, Refusal, RoomAct, SpawnAct, TerminalAct,
        TowerAct,
    },
    world::Id,
};
use fabot_observe::metrics;
use wasm_bindgen::prelude::wasm_bindgen;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(js_name = __fabotExec)]
    fn exec(ints: &[i32], strs: &str) -> Vec<i32>;
}

const CREEP: i32 = 0;
const ROOM: i32 = 1;
const OBJECT: i32 = 2;
const INT: i32 = 0;
const TEXT: i32 = 1;
const BY_ID: i32 = 2;
const TEXTS: i32 = 3;
const DRAWN: i32 = 4;
const DOORS: i32 = 5;
const GONE: i32 = 1;
const THREW: i32 = 2;
const ERR_INVALID_ARGS: i32 = -10;

/// Each string NUL-ended in `strs`, named in `ints` by its index; `argc` is
/// where the current record counts its arguments.
#[derive(Default)]
struct Pack {
    ints: Vec<i32>,
    strs: String,
    words: i32,
    argc: usize,
    nul: bool,
    invalid: Vec<usize>,
}

impl Pack {
    fn word(&mut self, x: impl Display) -> i32 {
        let start = self.strs.len();
        let _ = write!(self.strs, "{x}");
        self.nul |= self.strs[start..].contains('\0');
        self.strs.push('\0');
        self.words += 1;
        self.words - 1
    }

    fn call(&mut self, kind: i32, actor: impl Display, method: &str) -> &mut Self {
        let (actor, method) = (self.word(actor), self.word(method));
        self.ints.extend([kind, actor, method, 0]);
        self.argc = self.ints.len() - 1;
        self
    }

    fn arg(&mut self, tag: i32, x: i32) -> &mut Self {
        self.ints[self.argc] += 1;
        self.ints.extend([tag, x]);
        self
    }

    fn int(&mut self, x: i32) -> &mut Self {
        self.arg(INT, x)
    }

    fn text(&mut self, x: impl Display) -> &mut Self {
        let word = self.word(x);
        self.arg(TEXT, word)
    }

    fn by_id(&mut self, id: &Id) -> &mut Self {
        let word = self.word(id);
        self.arg(BY_ID, word)
    }

    fn list<T: Display>(&mut self, tag: i32, xs: &[T]) -> &mut Self {
        self.arg(tag, xs.len() as i32);
        for x in xs {
            let word = self.word(x);
            self.ints.push(word);
        }
        self
    }

    /// A NUL inside a string would shift every later index, so that intent goes
    /// out as a lookup of nothing and comes back invalid.
    fn intent(&mut self, at: usize, intent: &Action) {
        let mark = (self.ints.len(), self.strs.len(), self.words);
        self.nul = false;
        self.pack(intent);
        if self.nul {
            self.ints.truncate(mark.0);
            self.strs.truncate(mark.1);
            self.words = mark.2;
            self.call(OBJECT, "", "");
            self.invalid.push(at);
        }
    }

    fn pack(&mut self, intent: &Action) {
        match intent {
            Action::Creep(name, act) => {
                let call = self.call(CREEP, name, act.method());
                match act {
                    CreepAct::Move(direction) => call.int(*direction),
                    CreepAct::Say(message) => call.text(message),
                    CreepAct::Transfer(id, resource) | CreepAct::Withdraw(id, resource) => {
                        call.by_id(id).text(resource)
                    }
                    CreepAct::Drop(resource) => call.text(resource),
                    CreepAct::Sign(id, text) => call.by_id(id).text(text),
                    CreepAct::RangedMassAttack => call,
                    CreepAct::Harvest(id)
                    | CreepAct::Pickup(id)
                    | CreepAct::Build(id)
                    | CreepAct::Repair(id)
                    | CreepAct::Upgrade(id)
                    | CreepAct::Reserve(id)
                    | CreepAct::Unreserve(id)
                    | CreepAct::ClaimController(id)
                    | CreepAct::AttackCreep(id)
                    | CreepAct::RangedAttackCreep(id)
                    | CreepAct::HealCreep(id)
                    | CreepAct::RangedHealCreep(id)
                    | CreepAct::Dismantle(id) => call.by_id(id),
                }
            }
            Action::Room(name, RoomAct::CreateSite(x, y, kind)) => {
                (self.call(ROOM, name, "createConstructionSite").int(*x).int(*y)).text(kind)
            }
            Action::Room(_, RoomAct::Destroy(id)) => self.call(OBJECT, id, "destroy"),
            Action::Room(_, RoomAct::RemoveSite(id)) => self.call(OBJECT, id, "remove"),
            Action::Tower(id, TowerAct::Attack(t)) => self.call(OBJECT, id, "attack").by_id(t),
            Action::Tower(id, TowerAct::Heal(t)) => self.call(OBJECT, id, "heal").by_id(t),
            Action::Tower(id, TowerAct::Repair(t)) => self.call(OBJECT, id, "repair").by_id(t),
            Action::Controller(id, ControllerAct::ActivateSafeMode) => {
                self.call(OBJECT, id, "activateSafeMode")
            }
            Action::Controller(id, ControllerAct::Unclaim) => self.call(OBJECT, id, "unclaim"),
            Action::Link(id, LinkAct::TransferEnergy(target, amount)) => {
                (self.call(OBJECT, id, "transferEnergy").by_id(target)).int(*amount)
            }
            Action::Lab(id, LabAct::RunReaction(a, b)) => {
                self.call(OBJECT, id, "runReaction").by_id(a).by_id(b)
            }
            Action::Lab(id, LabAct::BoostCreep(creep)) => {
                self.call(OBJECT, id, "boostCreep").by_id(creep)
            }
            Action::Terminal(id, TerminalAct::Send { resource, amount, to }) => {
                (self.call(OBJECT, id, "send").text(resource)).int(*amount).text(to)
            }
            Action::Observe(id, room) => self.call(OBJECT, id, "observeRoom").text(room),
            Action::Spawn(id, SpawnAct::RecycleCreep(creep)) => {
                self.call(OBJECT, id, "recycleCreep").by_id(creep)
            }
            Action::Spawn(id, SpawnAct::SpawnCreep(body, name, drawn, doors)) => {
                (self.call(OBJECT, id, "spawnCreep").list(TEXTS, body))
                    .text(name)
                    .list(DRAWN, drawn)
                    .list(DOORS, doors)
            }
        };
    }
}

/// One bad intent costs itself, never the rest of the tick's.
pub fn run(intents: &[Action]) -> Vec<Refusal> {
    let mut pack = Pack::default();
    (intents.iter().enumerate()).for_each(|(at, intent)| pack.intent(at, intent));
    let refused = exec(&pack.ints, &pack.strs);
    (refused.as_chunks::<3>().0.iter())
        .map(|&[at, code, tag]| {
            let intent = &intents[at as usize];
            let (code, tag) = if pack.invalid.contains(&(at as usize)) {
                (ERR_INVALID_ARGS, 0)
            } else {
                (code, tag)
            };
            Refusal {
                actor: intent.actor(),
                act: match tag {
                    THREW => "threw".to_string(),
                    _ => metrics::verb(intent, tag == GONE).into_owned(),
                },
                code,
            }
        })
        .collect()
}
