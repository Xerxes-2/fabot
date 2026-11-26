//! The boost chain's sample keys, read by `soak:labs`. A cluster's product is
//! the rise of an output lab whose cooldown is set anew that tick: a reverse
//! reaction or an unboost sets one too, but raises no output.
use std::collections::BTreeMap;

use fabot_base::{
    boost::{ORES, Roles, ores_in, roles_of},
    world::{Id, Pos, RoomName},
};
use krebs::{
    atom,
    doc::{Atom, Obj, Val},
    utils::{calc_rooms_distance, calc_terminal_energy_cost},
};
use screeps::{LAB_BOOST_MINERAL, LAB_MINERAL_CAPACITY, LAB_REACTION_AMOUNT, ResourceType};
use serde_json::{Value, json};

use crate::view::{EVENT_HARVEST, Room, View, num, obj, store, text, xy};

type Tally = BTreeMap<String, f64>;

#[derive(Default)]
struct Cluster {
    produced: Tally,
    consumed: Tally,
    /// What our bodies here were boosted with, in mineral the labs spent.
    boosted: Tally,
    /// Ticks the labs stood in each state, toward the last reaction they ran:
    /// `carried` is a reagent short while a body here carries it.
    ticks: BTreeMap<&'static str, u64>,
    inputs: Option<[ResourceType; 2]>,
}

#[derive(Default)]
pub struct Labs {
    /// Each lab's cooldown end and what it held, at the last tick.
    was: BTreeMap<String, (f64, Option<(String, f64)>)>,
    /// Each of our bodies' boosted parts, by compound.
    boosts: BTreeMap<String, Tally>,
    clusters: BTreeMap<String, Cluster>,
    fee_of: Tally,
    sends: Tally,
    /// Per route, so churn (a resource out and back) shows apart from a chain's one-way flow.
    routes: BTreeMap<(String, String, String), f64>,
    /// By ore: what our extractors dug, and what our terminals sent to rooms not ours, in ore.
    dug: Tally,
    away: Tally,
}

fn mineral(o: &Obj) -> Option<(String, f64)> {
    let store = obj(o, atom!("store"))?;
    store
        .iter()
        .find(|(r, n)| r.as_str() != "energy" && n.as_f64() > Some(0.0))
        .map(|(r, n)| (r.as_str().to_string(), n.as_f64().unwrap_or(0.0)))
}

/// The ores `amount` of `resource` was reacted from.
fn add_ores(to: &mut Tally, resource: &str, amount: f64) {
    let Ok(resource) = resource.parse::<ResourceType>() else {
        return;
    };
    for (ore, n) in ORES.iter().zip(ores_in(resource)) {
        if n > 0 {
            *to.entry(ore.to_string()).or_default() += amount * f64::from(n);
        }
    }
}

/// Every mineral and compound in our stores, stores nobody's, piles, tombstones and our bodies.
fn stock_of(room: &Room, us: &str) -> Tally {
    let mut out = Tally::new();
    for &(kind, o) in &room.objects {
        let resource = text(o, atom!("resourceType"));
        if kind == "energy" && resource != "energy" {
            *out.entry(resource.into()).or_default() += num(o, Atom::intern(resource));
        } else if kind != "energy" && [us, ""].contains(&text(o, atom!("user"))) {
            for (r, n) in obj(o, atom!("store")).into_iter().flat_map(Obj::iter) {
                if r.as_str() != "energy" {
                    *out.entry(r.as_str().into()).or_default() += n.as_f64().unwrap_or(0.0);
                }
            }
        }
    }
    out
}

fn labs_of<'a>(room: &'a Room, us: &str) -> (Vec<&'a Obj>, Option<Roles>) {
    let labs: Vec<&Obj> = room.of("lab").filter(|o| text(o, atom!("user")) == us).collect();
    let tiles: Vec<(Id, Pos)> = labs
        .iter()
        .map(|o| {
            let (x, y) = xy(o);
            (Id::of_engine(text(o, atom!("_id"))), Pos::new(RoomName::SIM, x as i32, y as i32))
        })
        .collect();
    (labs, roles_of(&tiles))
}

fn role(roles: Option<&Roles>, o: &Obj) -> &'static str {
    let id = Id::of_engine(text(o, atom!("_id")));
    match roles {
        Some(r) if r.reagents.0 == id || r.reagents.1 == id => "reagent",
        Some(r) if r.outputs.contains(&id) => "output",
        _ => "",
    }
}

impl Labs {
    pub fn account(&mut self, room: &Room, time: f64, us: &str) {
        for e in room.events().filter(|e| e.kind == EVENT_HARVEST) {
            let target = e.data["targetId"].as_str().unwrap_or_default();
            if let Some(ore) = room.of("mineral").find(|m| text(m, atom!("_id")) == target) {
                let amount = e.data["amount"].as_f64().unwrap_or(0.0);
                *self.dug.entry(text(ore, atom!("mineralType")).into()).or_default() += amount;
            }
        }
        let cluster = self.clusters.entry(room.name.into()).or_default();
        for c in room.of("creep").filter(|c| text(c, atom!("user")) == us) {
            let mut parts = Tally::new();
            if let Some(Val::Arr(body)) = c.get(atom!("body")) {
                for part in body.iter().filter_map(Val::as_obj) {
                    if let Some(boost) = part.get(atom!("boost")).and_then(Val::as_str) {
                        *parts.entry(boost.into()).or_default() += 1.0;
                    }
                }
            }
            let had =
                self.boosts.insert(text(c, atom!("_id")).into(), parts.clone()).unwrap_or_default();
            for (boost, n) in parts {
                let rise = n - had.get(&boost).copied().unwrap_or(0.0);
                if rise > 0.0 {
                    *cluster.boosted.entry(boost).or_default() +=
                        rise * f64::from(LAB_BOOST_MINERAL);
                }
            }
        }

        let (labs, roles) = labs_of(room, us);
        let mut cooling = false;
        for &o in &labs {
            let until = num(o, atom!("cooldownTime"));
            let held = mineral(o);
            let was = self.was.insert(text(o, atom!("_id")).into(), (until, held.clone()));
            if role(roles.as_ref(), o) != "output" {
                continue;
            }
            cooling |= until > time;
            let Some(((was_until, was_held), (compound, amount))) = was.zip(held) else {
                continue;
            };
            let before = was_held.filter(|h| h.0 == compound).map_or(0.0, |h| h.1);
            if was_until == until || until <= time || amount <= before {
                continue;
            }
            let rise = amount - before;
            *cluster.produced.entry(compound.clone()).or_default() += rise;
            if let Some(inputs) =
                compound.parse::<ResourceType>().ok().and_then(ResourceType::reaction_components)
            {
                for r in inputs {
                    *cluster.consumed.entry(r.to_string()).or_default() += rise;
                }
                cluster.inputs = Some(inputs);
            }
        }

        let Some(roles) = roles else {
            return;
        };
        let lab = |id: Id| labs.iter().find(|o| Id::of_engine(text(o, atom!("_id"))) == id);
        let reagents =
            [roles.reagents.0, roles.reagents.1].map(|id| lab(id).and_then(|o| mineral(o)));
        let stands = |m: &Option<(String, f64)>| {
            m.as_ref().is_some_and(|m| m.1 >= f64::from(LAB_REACTION_AMOUNT))
        };
        let missing: Vec<Option<String>> = match cluster.inputs {
            Some(inputs) => inputs
                .iter()
                .map(ToString::to_string)
                .filter(|r| {
                    !reagents.iter().any(|m| stands(m) && m.as_ref().is_some_and(|m| m.0 == *r))
                })
                .map(Some)
                .collect(),
            None => reagents.iter().filter(|m| !stands(m)).map(|_| None).collect(),
        };
        let carried = room
            .of("creep")
            .filter(|c| text(c, atom!("user")) == us)
            .any(|c| missing.iter().flatten().any(|r| store(c, Atom::intern(r)) > 0.0));
        let full = roles.outputs.iter().all(|id| {
            lab(*id)
                .and_then(|o| mineral(o))
                .is_some_and(|m| m.1 > f64::from(LAB_MINERAL_CAPACITY - LAB_REACTION_AMOUNT))
        });
        let state = match missing.len() {
            _ if cooling => "busy",
            _ if full => "full",
            0 => "stocked",
            _ if carried => "carried",
            2 => "bare",
            _ => "short",
        };
        *cluster.ticks.entry(state).or_default() += 1;
    }

    pub fn sent(&mut self, v: &View, resource: &str, from: &str, to: &str, amount: f64) {
        let range = calc_rooms_distance(from, to, false, 0.0);
        *self.fee_of.entry(resource.into()).or_default() +=
            calc_terminal_energy_cost(amount, range);
        *self.sends.entry(resource.into()).or_default() += 1.0;
        *self.routes.entry((resource.into(), from.into(), to.into())).or_default() += amount;
        if !v.index(to).is_some_and(|i| v.rooms[i].of("controller").any(|c| v.ours(c))) {
            add_ores(&mut self.away, resource, amount);
        }
    }

    /// `rooms` with each followed room's lab keys and stock added.
    pub fn rooms(&self, mut rooms: Value, v: &View) -> Value {
        let cools = |o| num(o, atom!("cooldownTime")) - v.k.time as f64;
        for (name, row) in rooms.as_object_mut().into_iter().flatten() {
            let room = v.room(name);
            let (labs, roles) = labs_of(room, v.us);
            let labs = labs.iter().map(|o| {
                let id = text(o, atom!("_id"));
                json!([id, xy(o), mineral(o), cools(o), role(roles.as_ref(), o)])
            });
            let row = row.as_object_mut().expect("a json! object");
            row.insert("labs".into(), json!(labs.collect::<Vec<_>>()));
            row.insert("stockOf".into(), json!(stock_of(room, v.us)));
            if let Some(c) = self.clusters.get(name) {
                for (k, value) in [
                    ("produced", json!(c.produced)),
                    ("consumed", json!(c.consumed)),
                    ("boosted", json!(c.boosted)),
                    ("labTicks", json!(c.ticks)),
                    ("inputs", json!(c.inputs.map(|i| i.map(|r| r.to_string())))),
                ] {
                    row.insert(k.into(), value);
                }
            }
        }
        rooms
    }

    /// The fees by resource, and by ore what was dug, held, sent away and boosted with.
    pub fn totals(&self, v: &View, rooms: &[String]) -> [(&'static str, Value); 7] {
        let (mut held, mut boosted) = (Tally::new(), Tally::new());
        for name in rooms {
            for (r, n) in stock_of(v.room(name), v.us) {
                add_ores(&mut held, &r, n);
            }
        }
        for (r, n) in self.clusters.values().flat_map(|c| &c.boosted) {
            add_ores(&mut boosted, r, *n);
        }
        // The lesser way of each pair: a chain's one-way flow counts nothing.
        let mut back = Tally::new();
        for ((r, from, to), n) in self.routes.iter().filter(|((_, f, t), _)| f < t) {
            let other = self.routes.get(&(r.clone(), to.clone(), from.clone()));
            *back.entry(r.clone()).or_default() += n.min(other.copied().unwrap_or(0.0));
        }
        [
            ("feeOf", json!(self.fee_of)),
            ("sendsOf", json!(self.sends)),
            ("reversedOf", json!(back)),
            ("oreDug", json!(self.dug)),
            ("oreHeld", json!(held)),
            ("oreAway", json!(self.away)),
            ("oreBoosted", json!(boosted)),
        ]
    }
}
