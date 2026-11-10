pub mod intel;

use super::intents::Intent;
use super::state::State;
use std::collections::BTreeSet;

use super::engine;
use super::fight::shape::{Cast, Engagement};
use super::tick::{ColonyPlan, Context, Declarations, Stage};
use super::tiles;
use super::world::{RoomName, Rules, World};
use intel::Intel;

pub fn decide(context: &mut Context, state: &mut State) -> Result<Vec<Intent>, String> {
    let mut preyed = intel::preyed(context.intel, context.declarations, context.world.time);
    preyed.retain(|room| context.atlas.has_terrain(*room));
    let (observed, scouts) = intel::sight(
        context.world,
        context.atlas,
        context.declarations,
        context.intel,
        &preyed,
    );
    state.scouts = scouts;
    state.preyed = preyed;
    Ok(observed)
}

/// A rival's room this near raids within a creep life.
const NEAR: i32 = 3;

/// The threat in, a reserve out, as a priced choice would be (§5).
pub fn reserve(rules: &Rules, lifetimes: f64, level: i32, threatened: bool) -> i32 {
    let fire = rules.allowed("tower", level) * engine::TOWER_ENERGY_COST * engine::CREEP_LIFETIME;
    let full = (f64::from(fire) * lifetimes) as i32;
    if threatened { full } else { full / 3 }
}

pub fn plan(
    world: &World,
    owned: &BTreeSet<RoomName>,
    declarations: &Declarations,
    intel: &Intel,
    (engagements, casts): (&[Engagement], &[Cast]),
) -> Vec<ColonyPlan> {
    let rival = |name: &str| name != world.me && !declarations.allies.contains(name);
    let rivals: Vec<RoomName> = (intel.iter())
        .filter(|(_, row)| row.level > 0 && row.owner.as_deref().is_some_and(rival))
        .map(|(room, _)| *room)
        .collect();
    let fought = |room: RoomName| engagements.iter().any(|e| e.room == room);
    (owned.iter().filter_map(|r| world.room(*r)))
        .filter_map(|room| {
            let level = room.controller.as_ref()?.level;
            let near = rivals
                .iter()
                .any(|r| tiles::rooms_apart(*r, room.name) <= NEAR);
            // A fight's committed casts are paid before any surplus.
            let asked: i32 = (casts.iter())
                .filter(|c| c.room == room.name)
                .map(|c| engine::body_cost(&c.body))
                .sum();
            let threatened = near || fought(room.name) || asked > 0;
            let kept = reserve(
                &world.rules,
                declarations.reserve_lifetimes,
                level,
                threatened,
            );
            Some(ColonyPlan {
                room: room.name,
                stage: Stage::Exploit,
                target: level,
                reserve: kept + asked,
            })
        })
        .collect()
}
