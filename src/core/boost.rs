use std::collections::BTreeSet;

use super::atlas::Mover;
use super::engine;
use super::grid::{self, Footprint};
use super::intents::{Intent, LabAct};
use super::mv;
use super::tick::{Context, target_at};
use super::vocabulary::{self, Boost, Compound, Resource};
use super::world::{self, Pos, RoomName, World};

/// A cast name's last token names the compound its body walks to a lab for.
pub fn of_name(name: &str) -> Option<Compound> {
    match vocabulary::resource_of_name(name.rsplit('-').next()?) {
        Resource::Compound(compound) => Some(compound),
        _ => None,
    }
}

/// The lab a body asking for `compound` is boosted from: one with room for it
/// holding the most of it, ties to the lower id; a lab holding another mineral
/// only when none has room, its drain making room.
pub fn booster<'a>(
    world: &'a World,
    room: RoomName,
    compound: Compound,
) -> Option<&'a world::Structure> {
    let held = |s: &world::Structure| s.store.amount_of(&Resource::Compound(compound));
    let most = |a: &&'a world::Structure, b: &&'a world::Structure| {
        held(a).cmp(&held(b)).then_with(|| b.id.cmp(&a.id))
    };

    (labs_in(world, room))
        .filter(|l| holds_room_for(l, compound))
        .max_by(most)
        .or_else(|| labs_in(world, room).max_by(most))
}

fn labs_in(world: &World, room: RoomName) -> impl Iterator<Item = &world::Structure> {
    (world.rooms.iter())
        .filter(move |r| r.name == room)
        .flat_map(|r| r.structures.iter())
        .filter(|s| s.kind == world::Kind::Lab && s.owner.is_ours())
}

/// Over the stores a haul fills: labs, terminal, storage and bodies' loads.
pub fn home_holds(world: &World, room: RoomName, compound: Compound) -> (i32, i32) {
    let held = |s: &world::Store| {
        (
            s.amount_of(&Resource::Compound(compound)),
            s.amount_of(&Resource::Energy),
        )
    };
    let stores = (world.rooms.iter())
        .filter(|r| r.name == room)
        .flat_map(|r| r.structures.iter())
        .filter(|s| {
            s.owner.is_ours() && (s.kind.is_lab() || s.kind.is_terminal() || s.kind.is_storage())
        })
        .map(|s| held(&s.store))
        .chain(
            (world.creeps.iter())
                .filter(move |c| c.pos.room == room)
                .map(|c| held(&c.store)),
        );

    stores.fold((0, 0), |(compound_, energy), (m, e)| {
        (compound_ + m, energy + e)
    })
}

/// Whether the lab holds the compound or nothing: a lab holds one mineral, so
/// another one in it leaves the compound nowhere to land.
fn holds_room_for(lab: &world::Structure, compound: Compound) -> bool {
    let wanted = Resource::Compound(compound);
    (lab.store.held.iter()).all(|(r, n)| *n <= 0 || r == &wanted || !vocabulary::is_mineral(r))
}

fn can_boost(lab: &world::Structure, compound: Compound) -> bool {
    lab.store.amount_of(&Resource::Compound(compound)) >= engine::BOOST_MINERAL_PER_PART
        && lab.store.amount_of(&Resource::Energy) >= engine::BOOST_ENERGY_PER_PART
}

fn boost_need(creep: &world::Creep, compound: Compound) -> (i32, i32) {
    let name = vocabulary::resource_name(&Resource::Compound(compound));
    let parts = (creep.body.iter())
        .filter(|p| {
            p.boost == Boost::Unboosted
                && vocabulary::boost::of_compound(p.kind, &name) != Boost::Unboosted
        })
        .count() as i32;

    (
        parts * engine::BOOST_MINERAL_PER_PART,
        parts * engine::BOOST_ENERGY_PER_PART,
    )
}

pub struct Waiting<'a> {
    pub lab: &'a world::Structure,
    pub compound: Compound,
    pub mineral: i32,
    pub energy: i32,
}

/// The lab a body waits at this tick, and what its parts still ask of it: one
/// that has room for the compound, while the home holds what the parts ask and
/// can carry it there. Read afresh every tick, so a colony that cannot serve the
/// body costs it the compound and never its raid.
pub fn waiting<'a>(world: &'a World, home: RoomName, creep: &world::Creep) -> Option<Waiting<'a>> {
    let compound = of_name(&creep.name)?;
    let (mineral, energy) = boost_need(creep, compound);
    let (held, fuel) = home_holds(world, home, compound);

    if mineral == 0 || held < mineral || fuel < energy {
        return None;
    }
    let lab = booster(world, home, compound)?;
    // A colony with nothing left to carry the load fights the body unboosted
    // rather than stand it at a lab nothing will fill.
    let carried = can_boost(lab, compound)
        || (world.creeps.iter()).any(|c| {
            c.owner.is_ours()
                && c.pos.room == home
                && c.name != creep.name
                && of_name(&c.name).is_none()
                && c.store.free() > 0
        });
    if !holds_room_for(lab, compound) || !carried {
        return None;
    }

    Some(Waiting {
        lab,
        compound,
        mineral,
        energy,
    })
}

/// A body whose cast asked for a compound and whose parts do not carry it yet
/// is held at the lab and boosted there: its module's acts and wish are
/// dropped for the walk, or the boost itself when it stands beside it.
pub fn stop(context: &mut Context, intents: Vec<Intent>) -> Vec<Intent> {
    let Some((home, _)) = &context.declarations.labs else {
        return intents;
    };
    let home = *home;
    let mut stalls: Vec<(String, Pos, String, Compound)> = Vec::new();

    for creep in (context.world.creeps.iter()).filter(|c| c.owner.is_ours() && c.pos.room == home) {
        if let Some(w) = waiting(context.world, home, creep) {
            stalls.push((creep.name.clone(), w.lab.pos, w.lab.id.clone(), w.compound));
        }
    }

    if stalls.is_empty() {
        return intents;
    }

    let stalling: BTreeSet<&str> = stalls.iter().map(|(name, _, _, _)| name.as_str()).collect();
    let mut out: Vec<Intent> = (intents.into_iter())
        .filter(|i| {
            let actor = i.actor();
            i.is_queue() || !stalling.contains(actor.as_str())
        })
        .collect();

    let mut boostable: BTreeSet<&str> = BTreeSet::new();
    for lab in labs_in(context.world, home) {
        for compound in stalls.iter().map(|(_, _, _, c)| c) {
            if can_boost(lab, *compound) {
                boostable.insert(lab.id.as_str());
            }
        }
    }

    for (name, at, lab, _) in &stalls {
        let Some(creep) = context.world.creeps.iter().find(|c| &c.name == name) else {
            continue;
        };
        if creep.pos.within(1, at) && boostable.contains(lab.as_str()) {
            out.push(Intent::Lab(
                lab.clone(),
                LabAct::BoostCreep(creep.id.clone()),
            ));
            continue;
        }
        let mover = Mover {
            footprint: Footprint::One,
            factor: grid::loaded_factor(&world::body_kinds(creep)),
        };
        out.push(Intent::Wish(
            creep.pos.room,
            mv::plan::toward(context.atlas, creep, mover, &target_at(at, 1), 1),
        ));
    }

    out
}
