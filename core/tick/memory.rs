use std::collections::BTreeMap;

use fabot_base::{
    declarations::{ColonyPlan, Declarations, Declared, Outpost, Stage},
    tuning::Tuning,
    world::{Id, Pos, RoomName},
};
use fabot_economy::{
    EconomyState,
    task::{Assignment, of_task_id, task_id},
};
use fabot_fight::shape::FightState;
use fabot_strategy::StrategyState;
use serde::{Deserialize, de::DeserializeOwned};
use serde_json::{Map, Value, json};

use super::state::State;

fn assignment(held: &str) -> Assignment {
    let (slot, task) = held.split_once('|').unwrap_or((held, ""));
    Assignment { slot: slot.to_string(), task: of_task_id(task) }
}

pub struct Memory {
    /// As the tick found it: the recorder reads it, and `written` skips a text
    /// equal to it.
    pub text: String,
    v2: Value,
    /// Text `JSON.parse` may take but serde refuses is never written over.
    unread: bool,
}

impl Memory {
    pub fn of_text(text: String) -> Memory {
        let (mut root, unread) = match serde_json::from_str(&text) {
            Ok(Value::Object(root)) => (root, false),
            _ => (Map::new(), !text.is_empty()),
        };
        let mut v2 =
            (root.shift_remove("v2")).filter(Value::is_object).unwrap_or_else(|| json!({}));
        if !v2["switches"].is_object() {
            v2["switches"] = json!({});
        }
        Memory { text, v2, unread }
    }

    pub fn state(&self) -> State {
        let v2 = &self.v2;
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
                closed: Vec::new(),
            },
            strategy: StrategyState {
                expansion: Deserialize::deserialize(&v2["expansion"]).unwrap_or_default(),
                piles: Deserialize::deserialize(&v2["piles"]).unwrap_or_default(),
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
        let v2 = &mut self.v2;
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
        });
        v2["expansion"] = json!(state.strategy.expansion);
        v2["piles"] = json!(state.strategy.piles);
        // Unchanged, `RawMemory.set` would only copy the text across the seam.
        serde_json::to_string(&BTreeMap::from([("v2", &self.v2)]))
            .ok()
            .filter(|text| *text != self.text)
    }

    pub fn recording(&self) -> bool {
        self.v2["switches"]["recorder"].as_bool() == Some(true)
    }

    /// A tuning serde refuses or out of range is a fault, and every key keeps
    /// its default; so is the reserve declared where it was, so no deploy
    /// changes it unseen.
    pub fn declarations(&self) -> (Declarations, Vec<String>) {
        let declared = &self.v2["declarations"];
        let abandon: Vec<RoomName> = list(declared, "abandon");
        let tuned = match &self.v2["tuning"] {
            Value::Null => Ok(Tuning::default()),
            t => {
                Tuning::deserialize(t).map_err(|e| e.to_string()).and_then(|t| match t.outside() {
                    Some(key) => Err(format!("tuning.{} is out of its range", camel(key))),
                    None => Ok(t),
                })
            }
        };
        let (tuning, mut faults) =
            tuned.map_or_else(|e| (Tuning::default(), vec![e]), |t| (t, Vec::new()));
        if !declared["reserve"].is_null() {
            faults.push("declarations.reserve is read no more: set tuning.reserveLifetimes".into());
        }
        let declarations = Declarations {
            tuning,
            outposts: outposts(declared, "outposts"),
            claims: outposts(declared, "claims"),
            allies: list(declared, "allies"),
            enemies: list(declared, "enemies"),
            colonies: (abandon.into_iter())
                .map(|room| ColonyPlan {
                    room,
                    stage: Stage::Abandon,
                    target: 0,
                    reserve: 0,
                    rampart_hits: 0,
                    ring: false,
                    owes: false,
                    fights: false,
                    pile: None,
                    muster: None,
                })
                .collect(),
            avoid: list(declared, "avoid"),
            tactics: list(declared, "tactics"),
            operations: Vec::new(),
            says_idle: declared["debug"]["idle"].as_bool() == Some(true),
        };
        (declarations, faults)
    }
}

/// `rename_all = "camelCase"`'s name of a field, as an unknown key's fault spells them.
fn camel(snake: &str) -> String {
    let mut words = snake.split('_');
    let first = words.next().unwrap_or_default().to_string();
    first + &words.map(|w| w[..1].to_uppercase() + &w[1..]).collect::<String>()
}

fn list<T: DeserializeOwned, C: FromIterator<T> + Default>(v: &Value, key: &str) -> C {
    v[key].as_array().map_or_default(|items| {
        (items.iter()).filter_map(|item| T::deserialize(item).ok()).collect()
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
    let at = |room, at: At| Declared { id: at.id, pos: Pos::new(room, at.x, at.y) };
    (sites.into_iter())
        .map(|site| Outpost {
            reserved: true,
            room: site.room,
            controller: at(site.room, site.controller),
            sources: (site.sources.into_iter()).map(|s| at(site.room, s)).collect(),
        })
        .collect()
}
