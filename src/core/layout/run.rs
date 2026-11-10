use crate::core::engine;
use crate::core::intents::{Intent, RoomAct};
use crate::core::state::State;
use crate::core::tick::{Context, Module};
use crate::core::world::Rules;

use super::derive;
use super::remote;
use super::sites;
use super::towers;

pub fn decide_over(rules: &Rules, colonies: &[derive::Colony], budget: i32) -> Vec<Intent> {
    let mut budget = budget;
    let mut intents: Vec<Intent> = Vec::new();

    for colony in colonies {
        if let Some(plan) = &colony.plan
            && budget > 0
            && colony.census.pending < sites::SITES_IN_FLIGHT
            && sites::may_place(rules, &colony.census, colony.target)
        {
            for p in sites::place(rules, &colony.census, colony.target, plan, budget) {
                budget -= 1;
                intents.push(Intent::Room(
                    colony.room,
                    RoomAct::CreateSite(p.x, p.y, p.kind),
                ));
            }
        }

        intents.extend(towers::fire(
            &colony.towers,
            &colony.hostiles,
            &colony.ours,
            &colony.struck,
        ));
    }

    intents
}

pub fn decide(ctx: &mut Context, state: &mut State) -> Result<Vec<Intent>, String> {
    let world = ctx.world;
    let (colonies, standing) = derive::colonies(
        world,
        &ctx.derived.owned,
        &ctx.declarations.colonies,
        &state.layout,
    );

    let budget = engine::MAX_CONSTRUCTION_SITES - world.our_sites;
    let home = decide_over(&world.rules, &colonies, budget);

    let placed: i32 = home
        .iter()
        .map(|i| match i {
            Intent::Room(_, RoomAct::CreateSite(_, _, _)) => 1,
            _ => 0,
        })
        .sum();

    let sites = remote::sites(ctx.atlas, world, ctx.derived, &home, budget - placed);

    let mut intents = home;
    intents.extend(sites);

    state.layout = standing;
    Ok(intents)
}

pub fn layout() -> Module {
    Module {
        name: "layout",
        run: decide,
    }
}
