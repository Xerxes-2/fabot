pub mod intel;

use super::intents::Intent;
use super::state::State;
use super::tick::{ColonyPlan, Context, Stage};
use super::world::{RoomName, World};

pub fn decide(context: &mut Context, state: &State) -> Result<(Vec<Intent>, State), String> {
    let mut preyed = intel::preyed(context.intel, context.declarations, context.world.time);
    preyed.retain(|room| context.atlas.has_terrain(*room));
    let (observed, scouts) = intel::sight(
        context.world,
        context.atlas,
        context.declarations,
        context.intel,
        &preyed,
    );
    Ok((
        observed,
        State {
            scouts,
            preyed,
            ..state.clone()
        },
    ))
}

pub fn plan(world: &World, owned: &std::collections::BTreeSet<RoomName>) -> Vec<ColonyPlan> {
    (owned.iter().filter_map(|r| world.room(*r)))
        .filter_map(|room| {
            Some(ColonyPlan {
                room: room.name,
                stage: Stage::Exploit,
                target: room.controller.as_ref()?.level,
            })
        })
        .collect()
}
