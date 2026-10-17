//! `Memory.v2` is ours; v1's keys are dropped on the first tick that sees them,
//! since their parse is what every tick's first touch of `Memory` pays.
use std::collections::{BTreeMap, BTreeSet};

use js_sys::{Array, Object, Reflect};
use screeps::raw_memory;
use wasm_bindgen::{JsCast, JsValue};

use super::js::{field, int_of};
use crate::core::exchange::{Losses, Outcome};
use crate::core::fight::shape::{Cast, Engagement, Group, Objective, Phase, Plan, Tactic};
use crate::core::state::State;
use crate::core::tick::{Declarations, Declared, Outpost, undeclared};
use crate::core::vocabulary;
use crate::core::world::{Pos, World};

/// Read by `npm run observe` and `scripts/capture`.
pub const OBSERVE_SEGMENT: u8 = 1;

fn set_field(o: &JsValue, name: &str, value: &JsValue) {
    let _ = Reflect::set(o, &JsValue::from_str(name), value);
}

fn drop_field(o: &JsValue, name: &str) {
    let object = Object::from(o.clone());
    let _ = Reflect::delete_property(&object, &JsValue::from_str(name));
}

fn is_object(v: &JsValue) -> bool {
    v.is_object() && !v.is_null()
}

fn string_of(v: &JsValue) -> Option<String> {
    v.as_string()
}

fn array_of(v: &JsValue) -> Option<Vec<JsValue>> {
    if !Array::is_array(v) {
        return None;
    }
    let array: Array = v.clone().into();
    Some((0..array.length()).map(|i| array.get(i)).collect())
}

fn list_of<T>(parse: impl Fn(&JsValue) -> Option<T>, v: &JsValue) -> Vec<T> {
    array_of(v)
        .map(|items| items.iter().filter_map(parse).collect())
        .unwrap_or_default()
}

fn pos_of(v: &JsValue) -> Option<Pos> {
    match (
        string_of(&field(v, "room")),
        int_of(&field(v, "x")),
        int_of(&field(v, "y")),
    ) {
        (Some(room), Some(x), Some(y)) => Some(Pos::new(room, x, y)),
        _ => None,
    }
}

fn pos_obj(p: &Pos) -> JsValue {
    let o = Object::new();
    set_field(o.as_ref(), "room", &JsValue::from_str(&p.room));
    set_field(o.as_ref(), "x", &JsValue::from_f64(p.x as f64));
    set_field(o.as_ref(), "y", &JsValue::from_f64(p.y as f64));
    o.into()
}

/// The engine hands out a new `Memory` object a tick; a held reference would
/// write the last tick's.
fn memory() -> JsValue {
    Reflect::get(&js_sys::global(), &JsValue::from_str("Memory")).unwrap_or(JsValue::UNDEFINED)
}

fn root() -> JsValue {
    let memory = memory();
    let fabot = field(&memory, "fabot");
    if is_object(&fabot) {
        drop_field(&memory, "fabot");
    }
    let v2 = field(&memory, "v2");
    if is_object(&v2) {
        v2
    } else {
        let fresh = Object::new();
        let switches = Object::new();
        set_field(fresh.as_ref(), "switches", switches.as_ref());
        set_field(&memory, "v2", fresh.as_ref());
        fresh.into()
    }
}

fn switches() -> JsValue {
    let v2 = root();
    let switches = field(&v2, "switches");
    if is_object(&switches) {
        switches
    } else {
        let fresh = Object::new();
        set_field(&v2, "switches", fresh.as_ref());
        fresh.into()
    }
}

fn group_of(o: &JsValue) -> Option<Group> {
    match (
        int_of(&field(o, "id")),
        int_of(&field(o, "seen")),
        pos_of(o),
    ) {
        (Some(id), Some(seen), Some(centre)) => Some(Group {
            id,
            members: list_of(string_of, &field(o, "members"))
                .into_iter()
                .collect(),
            room: centre.room.clone(),
            last_seen: seen,
            centre,
        }),
        _ => None,
    }
}

fn group_obj(g: &Group) -> JsValue {
    let o = pos_obj(&g.centre);
    set_field(&o, "id", &JsValue::from_f64(g.id as f64));
    set_field(&o, "seen", &JsValue::from_f64(g.last_seen as f64));
    let members = Array::new();
    for m in &g.members {
        members.push(&JsValue::from_str(m));
    }
    set_field(&o, "members", members.as_ref());
    o
}

fn phase_name(phase: Phase) -> &'static str {
    match phase {
        Phase::Forming => "forming",
        Phase::Approaching => "approaching",
        Phase::Fighting => "fighting",
        Phase::Pursuing => "pursuing",
        Phase::Disengaging => "disengaging",
    }
}

fn phase_of(name: &str) -> Option<Phase> {
    Some(match name {
        "forming" => Phase::Forming,
        "approaching" => Phase::Approaching,
        "fighting" => Phase::Fighting,
        "pursuing" => Phase::Pursuing,
        "disengaging" => Phase::Disengaging,
        _ => return None,
    })
}

fn tactic_name(tactic: &Tactic) -> &'static str {
    match tactic {
        Tactic::Kite => "kite",
        Tactic::Close => "close",
        Tactic::Hold => "hold",
        Tactic::Blob(fights) => match **fights {
            Tactic::Kite => "blob-kite",
            _ => "blob",
        },
    }
}

fn tactic_of(name: &str) -> Option<Tactic> {
    Some(match name {
        "kite" => Tactic::Kite,
        "close" => Tactic::Close,
        "hold" => Tactic::Hold,
        "blob" => Tactic::Blob(Box::new(Tactic::Close)),
        "blob-kite" => Tactic::Blob(Box::new(Tactic::Kite)),
        _ => return None,
    })
}

/// As `npm run exchange` writes a prediction.
fn outcome_obj(outcome: &Outcome) -> JsValue {
    let o = Object::new();
    match outcome {
        Outcome::Win(ticks, losses) => {
            set_field(o.as_ref(), "kind", &JsValue::from_str("win"));
            set_field(o.as_ref(), "ticks", &JsValue::from_f64(*ticks as f64));
            set_field(o.as_ref(), "dead", &JsValue::from_f64(losses.dead as f64));
            set_field(o.as_ref(), "hits", &JsValue::from_f64(losses.hits as f64));
        }
        Outcome::Kite(ticks) => {
            set_field(o.as_ref(), "kind", &JsValue::from_str("kite"));
            set_field(o.as_ref(), "ticks", &JsValue::from_f64(*ticks as f64));
        }
        Outcome::Lose => {
            set_field(o.as_ref(), "kind", &JsValue::from_str("lose"));
        }
    }
    o.into()
}

fn outcome_of(o: &JsValue) -> Option<Outcome> {
    let count = |name: &str| int_of(&field(o, name)).unwrap_or(0);
    match string_of(&field(o, "kind")).as_deref() {
        Some("win") => Some(Outcome::Win(
            count("ticks"),
            Losses {
                dead: count("dead"),
                hits: count("hits"),
            },
        )),
        Some("kite") => Some(Outcome::Kite(count("ticks"))),
        Some("lose") => Some(Outcome::Lose),
        _ => None,
    }
}

fn engagement_of(o: &JsValue) -> Option<Engagement> {
    let phase = phase_of(&string_of(&field(o, "phase"))?)?;
    let since = int_of(&field(o, "since")).unwrap_or(0);
    let objective = match (string_of(&field(o, "kind")), string_of(&field(o, "target"))) {
        (Some(kind), Some(target)) if kind == "siege" => Objective::Siege(target),
        _ => Objective::Defend,
    };
    Some(Engagement {
        id: int_of(&field(o, "id"))?,
        objective,
        group: int_of(&field(o, "group"))?,
        room: string_of(&field(o, "room"))?,
        ours: list_of(string_of, &field(o, "ours")).into_iter().collect(),
        plan: Plan {
            tactic: tactic_of(&string_of(&field(o, "tactic")).unwrap_or_default())
                .unwrap_or(Tactic::Close),
            order: list_of(string_of, &field(o, "order")),
        },
        phase,
        // Saved before it was kept: its phase's start is the latest it can have opened.
        opened: int_of(&field(o, "opened")).unwrap_or(since),
        since,
        lost: int_of(&field(o, "lost")).unwrap_or(0),
        budget: int_of(&field(o, "budget")).unwrap_or(0),
        priority: field(o, "priority").as_f64().unwrap_or(0.0),
        unanswered: field(o, "unanswered").as_bool() == Some(true),
        capacity: int_of(&field(o, "capacity")).unwrap_or(0),
        predicted: {
            let predicted = field(o, "predicted");
            if predicted.is_undefined() || predicted.is_null() {
                None
            } else {
                outcome_of(&predicted)
            }
        },
    })
}

fn engagement_obj(e: &Engagement) -> JsValue {
    let o = Object::new();
    if let Objective::Siege(target) = &e.objective {
        set_field(o.as_ref(), "kind", &JsValue::from_str("siege"));
        set_field(o.as_ref(), "target", &JsValue::from_str(target));
    }
    set_field(o.as_ref(), "id", &JsValue::from_f64(e.id as f64));
    set_field(o.as_ref(), "group", &JsValue::from_f64(e.group as f64));
    set_field(o.as_ref(), "room", &JsValue::from_str(&e.room));
    let ours = Array::new();
    for name in &e.ours {
        ours.push(&JsValue::from_str(name));
    }
    set_field(o.as_ref(), "ours", ours.as_ref());
    set_field(
        o.as_ref(),
        "tactic",
        &JsValue::from_str(tactic_name(&e.plan.tactic)),
    );
    let order = Array::new();
    for id in &e.plan.order {
        order.push(&JsValue::from_str(id));
    }
    set_field(o.as_ref(), "order", order.as_ref());
    set_field(o.as_ref(), "phase", &JsValue::from_str(phase_name(e.phase)));
    set_field(o.as_ref(), "opened", &JsValue::from_f64(e.opened as f64));
    set_field(o.as_ref(), "since", &JsValue::from_f64(e.since as f64));
    set_field(o.as_ref(), "lost", &JsValue::from_f64(e.lost as f64));
    set_field(o.as_ref(), "budget", &JsValue::from_f64(e.budget as f64));
    set_field(o.as_ref(), "priority", &JsValue::from_f64(e.priority));
    set_field(o.as_ref(), "unanswered", &JsValue::from_bool(e.unanswered));
    set_field(
        o.as_ref(),
        "capacity",
        &JsValue::from_f64(e.capacity as f64),
    );
    match &e.predicted {
        Some(outcome) => set_field(o.as_ref(), "predicted", &outcome_obj(outcome)),
        None => set_field(o.as_ref(), "predicted", &JsValue::NULL),
    }
    o.into()
}

fn cast_of(o: &JsValue) -> Option<Cast> {
    let body = string_of(&field(o, "body"))?;
    let parts: Vec<_> = body.chars().filter_map(part_of_letter).collect();
    if parts.len() != body.chars().count() {
        return None;
    }
    Some(Cast {
        engagement: int_of(&field(o, "engagement"))?,
        name: string_of(&field(o, "name"))?,
        room: string_of(&field(o, "room"))?,
        body: parts,
    })
}

fn part_of_letter(letter: char) -> Option<vocabulary::BodyPart> {
    vocabulary::ALL_BODY_PARTS
        .iter()
        .copied()
        .find(|p| vocabulary::part_letter(*p) == letter)
}

fn cast_obj(c: &Cast) -> JsValue {
    let o = Object::new();
    set_field(
        o.as_ref(),
        "engagement",
        &JsValue::from_f64(c.engagement as f64),
    );
    set_field(o.as_ref(), "name", &JsValue::from_str(&c.name));
    set_field(o.as_ref(), "room", &JsValue::from_str(&c.room));
    let body: String = c.body.iter().map(|p| vocabulary::part_letter(*p)).collect();
    set_field(o.as_ref(), "body", &JsValue::from_str(&body));
    o.into()
}

/// The first touch of `Memory` is the engine's parse, so this is the read the
/// tick's "memory" phase times.
pub fn read_state() -> State {
    let s = switches();
    let held = field(&root(), "assignments");
    let fight = field(&root(), "fight");

    State {
        off: {
            let mut off = BTreeSet::new();
            for (name, value) in entries(&s) {
                if value.as_bool() == Some(false) {
                    off.insert(name);
                }
            }
            off
        },
        assignments: if is_object(&held) {
            entries(&held)
                .into_iter()
                .filter_map(|(name, value)| string_of(&value).map(|id| (name, id)))
                .collect()
        } else {
            BTreeMap::new()
        },
        layout: BTreeMap::new(),
        groups: list_of(group_of, &field(&fight, "groups")),
        engagements: list_of(engagement_of, &field(&fight, "engagements")),
        casts: list_of(cast_of, &field(&fight, "casts")),
    }
}

fn entries(o: &JsValue) -> Vec<(String, JsValue)> {
    let array = Object::entries(&Object::from(o.clone()));
    (0..array.length())
        .filter_map(|i| array.get(i).dyn_into::<Array>().ok())
        .filter_map(|pair| {
            let key = pair.get(0).as_string()?;
            Some((key, pair.get(1)))
        })
        .collect()
}

/// The switches are the operator's and are left as they are.
pub fn write_state(state: &State) {
    let root = root();
    let assignments = Object::new();
    for (name, id) in &state.assignments {
        set_field(assignments.as_ref(), name, &JsValue::from_str(id));
    }
    set_field(&root, "assignments", assignments.as_ref());

    let fight = Object::new();
    let groups = Array::new();
    for g in &state.groups {
        groups.push(&group_obj(g));
    }
    let engagements = Array::new();
    for e in &state.engagements {
        engagements.push(&engagement_obj(e));
    }
    let casts = Array::new();
    for c in &state.casts {
        casts.push(&cast_obj(c));
    }
    set_field(fight.as_ref(), "groups", groups.as_ref());
    set_field(fight.as_ref(), "engagements", engagements.as_ref());
    set_field(fight.as_ref(), "casts", casts.as_ref());
    set_field(&root, "fight", fight.as_ref());
}

fn declared_at(room: &str, o: &JsValue) -> Option<Declared> {
    Some(Declared {
        id: string_of(&field(o, "id"))?,
        pos: Pos::new(room, int_of(&field(o, "x"))?, int_of(&field(o, "y"))?),
    })
}

fn outpost_of(o: &JsValue) -> Option<Outpost> {
    let room = string_of(&field(o, "room"))?;
    let sources = array_of(&field(o, "sources"))?;
    let parsed: Vec<Declared> = sources
        .iter()
        .filter_map(|s| declared_at(&room, s))
        .collect();
    let controller = declared_at(&room, &field(o, "controller"))?;
    if parsed.len() != sources.len() {
        return None;
    }
    Some(Outpost {
        room,
        controller,
        sources: parsed,
    })
}

pub fn read_declarations() -> Declarations {
    let declared = field(&root(), "declarations");
    Declarations {
        reserve_lifetimes: match field(&declared, "reserve").as_f64() {
            Some(n) if n >= 0.0 => n,
            _ => undeclared().reserve_lifetimes,
        },
        outposts: array_of(&field(&declared, "outposts"))
            .map(|outposts| outposts.iter().filter_map(outpost_of).collect())
            .unwrap_or_default(),
    }
}

pub fn recording() -> bool {
    field(&switches(), "recorder").as_bool() == Some(true)
}

#[derive(Clone, Debug)]
pub struct Fault {
    pub tick: i32,
    pub where_: String,
    pub message: String,
}

#[derive(Clone, Debug)]
pub struct Observed {
    pub tick: i32,
    pub version: String,
    pub bucket: i32,
    pub total: f64,
    pub phases: Vec<(String, f64)>,
    pub counts: Vec<(String, i32)>,
    pub off: Vec<String>,
    pub faults: Vec<Fault>,
    pub refusals: Vec<crate::app::executor::Refusal>,
}

pub fn write_observe(o: &Observed) {
    let root = Object::new();
    set_field(root.as_ref(), "tick", &JsValue::from_f64(o.tick as f64));
    set_field(root.as_ref(), "version", &JsValue::from_str(&o.version));
    set_field(root.as_ref(), "bucket", &JsValue::from_f64(o.bucket as f64));
    set_field(root.as_ref(), "cpu", &JsValue::from_f64(o.total));

    let phases = Array::new();
    for (name, ms) in &o.phases {
        let pair = Array::new();
        pair.push(&JsValue::from_str(name));
        pair.push(&JsValue::from_f64(*ms));
        phases.push(&pair);
    }
    set_field(root.as_ref(), "phases", phases.as_ref());

    let counts = Array::new();
    for (name, n) in &o.counts {
        let pair = Array::new();
        pair.push(&JsValue::from_str(name));
        pair.push(&JsValue::from_f64(*n as f64));
        counts.push(&pair);
    }
    set_field(root.as_ref(), "counts", counts.as_ref());

    let off = Array::new();
    for name in &o.off {
        off.push(&JsValue::from_str(name));
    }
    set_field(root.as_ref(), "off", off.as_ref());

    let faults = Array::new();
    for f in &o.faults {
        let fault = Object::new();
        set_field(fault.as_ref(), "tick", &JsValue::from_f64(f.tick as f64));
        set_field(fault.as_ref(), "where", &JsValue::from_str(&f.where_));
        set_field(fault.as_ref(), "message", &JsValue::from_str(&f.message));
        faults.push(fault.as_ref());
    }
    set_field(root.as_ref(), "faults", faults.as_ref());

    let refusals = Array::new();
    for r in &o.refusals {
        let refusal = Object::new();
        set_field(refusal.as_ref(), "actor", &JsValue::from_str(&r.actor));
        set_field(refusal.as_ref(), "act", &JsValue::from_str(&r.act));
        set_field(refusal.as_ref(), "code", &JsValue::from_f64(r.code as f64));
        refusals.push(refusal.as_ref());
    }
    set_field(root.as_ref(), "refusals", refusals.as_ref());

    let text: String = js_sys::JSON::stringify(root.as_ref())
        .ok()
        .and_then(|text| text.as_string())
        .unwrap_or_else(|| "{}".to_string());
    write_segment(OBSERVE_SEGMENT, &text);
}

pub fn write_segment(id: u8, text: &str) {
    let segments = raw_memory::segments();
    segments.set(id, text.to_string());
}

/// The recorder's ring: the last five ticks' `World` and Memory size.
pub fn record(time: i32, world: &World) -> Option<String> {
    super::record::record(time, world)
}

/// The bundle's banner, set by `scripts/rust.mjs` on the global.
pub fn version() -> String {
    js_sys::Reflect::get(&js_sys::global(), &JsValue::from_str("__FABOT_VERSION__"))
        .ok()
        .and_then(|v| v.as_string())
        .unwrap_or_else(|| "unbuilt".to_string())
}
