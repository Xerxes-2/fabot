//! `Memory`'s text and what it holds: `v2` is ours, keys of others are carried
//! through in place, and v1's and the engine's `creeps` are dropped.
use std::collections::BTreeMap;

use serde::Deserialize;
use serde::de::DeserializeOwned;
use serde_json::{Map, Value, json};

use super::economy::EconomyState;
use super::economy::task::{Assignment, of_task_id, task_id};
use super::fight::shape::FightState;
use super::state::State;
use super::strategy::StrategyState;
use super::vocabulary::{self, Resource};
use super::world::{Id, Pos, RoomName};
use crate::declarations::{
    ColonyPlan, Declarations, Declared, Flow, Node, Outpost, Stage, undeclared,
};

fn assignment(held: &str) -> Assignment {
    let (slot, task) = held.split_once('|').unwrap_or((held, ""));
    Assignment {
        slot: slot.to_string(),
        task: of_task_id(task),
    }
}

pub struct Memory {
    /// As the tick found it: the recorder reads it, and `written` skips a text
    /// equal to it.
    pub text: String,
    root: Map<String, Value>,
    /// Text `JSON.parse` may take but serde refuses is never written over.
    unread: bool,
}

impl Memory {
    pub fn of_text(text: String) -> Memory {
        let (mut root, unread) = match serde_json::from_str(&text) {
            Ok(Value::Object(root)) => (root, false),
            _ => (Map::new(), !text.is_empty()),
        };
        for theirs in ["fabot", "creeps"] {
            root.shift_remove(theirs);
        }
        let v2 = root.entry("v2").or_insert(Value::Null);
        if !v2.is_object() {
            *v2 = json!({});
        }
        if !v2["switches"].is_object() {
            v2["switches"] = json!({});
        }
        Memory { text, root, unread }
    }

    fn v2(&self) -> &Value {
        &self.root["v2"]
    }

    pub fn state(&self) -> State {
        let v2 = self.v2();
        let fight = &v2["fight"];
        let entries = |key| v2[key].as_object().into_iter().flatten();
        State {
            off: (entries("switches"))
                .filter(|(_, on)| on.as_bool() == Some(false))
                .map(|(name, _)| name.clone())
                .collect(),
            economy: EconomyState {
                assignments: (entries("assignments"))
                    .filter_map(|(name, held)| Some((name.clone(), assignment(held.as_str()?))))
                    .collect(),
                ..EconomyState::default()
            },
            fight: FightState {
                groups: list(fight, "groups"),
                engagements: list(fight, "engagements"),
                casts: list(fight, "casts"),
                due: list(fight, "due"),
            },
            strategy: StrategyState {
                expansion: Deserialize::deserialize(&v2["expansion"]).unwrap_or_default(),
                ..StrategyState::default()
            },
            ..State::default()
        }
    }

    /// The switches and declarations are the operator's and are left as they are.
    pub fn written(&mut self, state: &State) -> Option<String> {
        if self.unread {
            return None;
        }
        let v2 = &mut self.root["v2"];
        let held = |a: &Assignment| a.slot.clone() + "|" + &a.task.as_ref().map_or_default(task_id);
        v2["assignments"] = json!(
            (state.economy.assignments.iter())
                .map(|(name, a)| (name, held(a)))
                .collect::<BTreeMap<_, _>>()
        );
        v2["fight"] = json!({
            "groups": state.fight.groups,
            "engagements": state.fight.engagements,
            "casts": state.fight.casts,
            "due": state.fight.due,
        });
        v2["expansion"] = json!(state.strategy.expansion);
        // Unchanged, `RawMemory.set` would only copy the text across the seam.
        serde_json::to_string(&self.root)
            .ok()
            .filter(|text| *text != self.text)
    }

    pub fn recording(&self) -> bool {
        self.v2()["switches"]["recorder"].as_bool() == Some(true)
    }

    pub fn declarations(&self) -> Declarations {
        let declared = &self.v2()["declarations"];
        let abandon: Vec<RoomName> = list(declared, "abandon");
        Declarations {
            reserve_lifetimes: match declared["reserve"].as_f64() {
                Some(n) if n >= 0.0 => n,
                _ => undeclared().reserve_lifetimes,
            },
            outposts: outposts(declared, "outposts"),
            flows: flows(&declared["labs"]),
            claims: outposts(declared, "claims"),
            allies: list(declared, "allies"),
            enemies: list(declared, "enemies"),
            sieges: list(declared, "sieges"),
            colonies: (abandon.into_iter())
                .map(|room| ColonyPlan {
                    room,
                    stage: Stage::Abandon,
                    target: 0,
                    reserve: 0,
                    rampart_hits: 0,
                    ring: false,
                    owes: false,
                    pile: None,
                    muster: None,
                })
                .collect(),
            avoid: list(declared, "avoid"),
            tactics: list(declared, "tactics"),
        }
    }
}

fn list<T: DeserializeOwned, C: FromIterator<T> + Default>(v: &Value, key: &str) -> C {
    v[key].as_array().map_or_default(|items| {
        (items.iter())
            .filter_map(|item| T::deserialize(item).ok())
            .collect()
    })
}

/// A source or controller declared in its outpost's room.
#[derive(Deserialize)]
struct At {
    id: Id,
    x: i32,
    y: i32,
}

#[derive(Deserialize)]
struct Site {
    room: RoomName,
    controller: At,
    sources: Vec<At>,
}

fn outposts(declared: &Value, key: &str) -> Vec<Outpost> {
    let sites: Vec<Site> = list(declared, key);
    let at = |room, at: At| Declared {
        id: at.id,
        pos: Pos::new(room, at.x, at.y),
    };
    (sites.into_iter())
        .map(|site| Outpost {
            room: site.room,
            controller: at(site.room, site.controller),
            sources: (site.sources.into_iter())
                .map(|s| at(site.room, s))
                .collect(),
        })
        .collect()
}

/// `{ home, targets: { compound: stock } }`.
fn flows(labs: &Value) -> Vec<Flow> {
    let Some(home) = labs["home"].as_str().and_then(RoomName::parse) else {
        return Vec::new();
    };
    let targets = labs["targets"].as_object().into_iter().flatten();
    (targets.filter_map(|(name, stock)| match vocabulary::resource_of_name(name) {
        what @ Resource::Compound(_) => Some(Flow {
            what,
            to: Node::Colony(home),
            stock: stock.as_f64()? as i32,
        }),
        _ => None,
    }))
    .collect()
}
