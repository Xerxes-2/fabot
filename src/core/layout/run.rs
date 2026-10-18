use crate::core::engine;
use crate::core::intents::{Intent, RoomAct};
use crate::core::state::State;
use crate::core::tick::{self, Context, Module};

use super::derive;
use super::remote;
use super::sites;
use super::towers;

pub fn decide_over(colonies: &[derive::Colony], budget: i32) -> Vec<Intent> {
    let mut budget = budget;
    let mut intents: Vec<Intent> = Vec::new();

    for colony in colonies {
        if let Some(plan) = &colony.plan
            && budget > 0
            && colony.census.pending < sites::SITES_IN_FLIGHT
            && sites::may_place(&colony.census, colony.level)
        {
            for p in sites::place(&colony.census, colony.level, plan, budget) {
                budget -= 1;
                intents.push(Intent::Room(
                    colony.room.clone(),
                    RoomAct::CreateSite(p.x, p.y, p.kind),
                ));
            }
        }

        intents.extend(towers::fire(&colony.towers, &colony.hostiles, &colony.ours));
    }

    intents
}

pub fn decide(ctx: &mut Context, state: &State) -> Result<(Vec<Intent>, State), String> {
    let world = ctx.world;
    let (mut colonies, standing) = derive::colonies(world, &state.layout);
    for c in &mut colonies {
        if ctx.declarations.abandon.contains(&c.room) {
            c.plan = None;
        }
    }

    let budget = engine::MAX_CONSTRUCTION_SITES - world.our_sites;
    let home = decide_over(&colonies, budget);

    let placed: i32 = home
        .iter()
        .map(|i| match i {
            Intent::Room(_, RoomAct::CreateSite(_, _, _)) => 1,
            _ => 0,
        })
        .sum();

    let worked = tick::worked(ctx);
    let sites = remote::sites(ctx.atlas, world, &worked, &home, budget - placed);

    let mut intents = home;
    intents.extend(sites);

    Ok((
        intents,
        State {
            layout: standing,
            ..state.clone()
        },
    ))
}

pub fn layout() -> Module {
    Module {
        name: "layout",
        run: decide,
    }
}
