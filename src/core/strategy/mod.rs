pub mod intel;

use super::intents::Intent;
use super::state::State;
use super::tick::Context;

pub fn decide(context: &mut Context, state: &State) -> Result<(Vec<Intent>, State), String> {
    let preyed = intel::preyed(context.intel, context.declarations, context.world.time);
    let (observed, scouts) =
        intel::sight(context.world, context.declarations, context.intel, &preyed);
    Ok((
        observed,
        State {
            scouts,
            preyed,
            ..state.clone()
        },
    ))
}
