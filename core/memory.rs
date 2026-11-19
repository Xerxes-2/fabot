//! `Memory`'s text and what it holds: `v2` is ours, keys of others are carried
//! through in place, and v1's and the engine's `creeps` are dropped.
use std::collections::BTreeSet;

use serde::de::DeserializeOwned;
use serde::{Deserialize, Serialize, Serializer};
use serde_json::{Map, Value, json};

use super::exchange::{Losses, Outcome};
use super::fight::shape::{Group, Objective};
use super::state::State;
use super::tactics::{Tactic, by_name, name};
use super::tick::{ColonyPlan, Declarations, Declared, Flow, Node, Outpost, Stage, undeclared};
use super::vocabulary::{self, Resource};
use super::world::{Id, Pos, RoomName};

pub struct Memory {
    /// As the tick found it, for the recorder.
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
            assignments: (entries("assignments"))
                .filter_map(|(name, id)| Some((name.clone(), id.as_str()?.to_string())))
                .collect(),
            groups: list(fight, "groups"),
            engagements: list(fight, "engagements"),
            casts: list(fight, "casts"),
            due: list(fight, "due"),
            expansion: Deserialize::deserialize(&v2["expansion"]).unwrap_or_default(),
            ..State::default()
        }
    }

    /// The switches and declarations are the operator's and are left as they are.
    pub fn written(&mut self, state: &State) -> Option<String> {
        if self.unread {
            return None;
        }
        let v2 = &mut self.root["v2"];
        v2["assignments"] = json!(state.assignments);
        v2["fight"] = json!({
            "groups": state.groups,
            "engagements": state.engagements,
            "casts": state.casts,
            "due": state.due,
        });
        v2["expansion"] = json!(state.expansion);
        serde_json::to_string(&self.root).ok()
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

/// A group as Memory holds it, its room its centre's.
#[derive(Serialize, Deserialize)]
pub struct Seen {
    #[serde(flatten)]
    centre: Pos,
    id: i32,
    seen: i32,
    members: BTreeSet<Id>,
}

impl From<Seen> for Group {
    fn from(s: Seen) -> Group {
        Group {
            id: s.id,
            members: s.members,
            room: s.centre.room,
            last_seen: s.seen,
            centre: s.centre,
        }
    }
}

impl From<Group> for Seen {
    fn from(g: Group) -> Seen {
        Seen {
            centre: g.centre,
            id: g.id,
            seen: g.last_seen,
            members: g.members,
        }
    }
}

/// An objective as Memory holds it: a Defend has no kind.
#[derive(Serialize, Deserialize)]
pub struct Kind {
    #[serde(skip_serializing_if = "Option::is_none")]
    kind: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    target: Option<Id>,
}

impl From<Kind> for Objective {
    fn from(k: Kind) -> Objective {
        match (k.kind.as_deref(), k.target) {
            (Some("siege"), Some(target)) => Objective::Siege(target),
            (Some("sweep"), _) => Objective::Sweep,
            _ => Objective::Defend,
        }
    }
}

impl From<Objective> for Kind {
    fn from(o: Objective) -> Kind {
        let (kind, target) = match o {
            Objective::Defend => (None, None),
            Objective::Siege(target) => (Some("siege"), Some(target)),
            Objective::Sweep => (Some("sweep"), None),
        };
        Kind {
            kind: kind.map(str::to_string),
            target,
        }
    }
}

/// `JSON.stringify`'s number: a whole one has no `.0`, so Memory's text
/// survives the round trip.
pub fn js_number<S: Serializer>(n: &f64, s: S) -> Result<S::Ok, S::Error> {
    if n.fract() == 0.0 && n.abs() < 1e18 {
        s.serialize_i64(*n as i64)
    } else {
        s.serialize_f64(*n)
    }
}

/// As `npm run exchange` writes a prediction.
#[derive(Clone, Copy, Serialize, Deserialize)]
#[serde(tag = "kind", rename_all = "lowercase")]
pub enum Stored {
    Win { ticks: i32, dead: i32, hits: i32 },
    Kite { ticks: i32 },
    Lose,
}

impl From<Stored> for Outcome {
    fn from(s: Stored) -> Outcome {
        match s {
            Stored::Win { ticks, dead, hits } => Outcome::Win(ticks, Losses { dead, hits }),
            Stored::Kite { ticks } => Outcome::Kite(ticks),
            Stored::Lose => Outcome::Lose,
        }
    }
}

impl From<Outcome> for Stored {
    fn from(o: Outcome) -> Stored {
        match o {
            Outcome::Win(ticks, Losses { dead, hits }) => Stored::Win { ticks, dead, hits },
            Outcome::Kite(ticks) => Stored::Kite { ticks },
            Outcome::Lose => Stored::Lose,
        }
    }
}

/// A body as Memory holds it, a part a letter.
pub mod letters {
    use crate::vocabulary::{ALL_BODY_PARTS, Part, part_letter};
    use serde::{Deserialize, Deserializer, Serializer, de::Error};

    pub fn serialize<S: Serializer>(body: &[Part], s: S) -> Result<S::Ok, S::Error> {
        s.collect_str(&body.iter().map(|p| part_letter(*p)).collect::<String>())
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Vec<Part>, D::Error> {
        let letters = String::deserialize(d)?;
        let part = |c| ALL_BODY_PARTS.into_iter().find(|p| part_letter(*p) == c);
        (letters.chars())
            .map(|c| part(c).ok_or_else(|| D::Error::custom("no part")))
            .collect()
    }
}

impl Serialize for Tactic {
    fn serialize<S: Serializer>(&self, s: S) -> Result<S::Ok, S::Error> {
        s.serialize_str(name(*self))
    }
}

impl<'de> Deserialize<'de> for Tactic {
    fn deserialize<D: serde::Deserializer<'de>>(d: D) -> Result<Self, D::Error> {
        let name = String::deserialize(d)?;
        by_name(&name).ok_or_else(|| serde::de::Error::custom("no tactic"))
    }
}
