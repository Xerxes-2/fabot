use crate::engine;
use crate::intents::{Intent, RoomAct};
use crate::state::State;
use crate::tick::{Context, Module};
use crate::world::Rules;

use super::derive;
use super::remote;
use super::sites;
use super::towers;

pub fn decide_over(rules: &Rules, colonies: &[derive::Colony], mut budget: i32) -> Vec<Intent> {
    let mut intents: Vec<Intent> = Vec::new();

    for colony in colonies {
        if let Some(plan) = &colony.plan
            && budget > 0
            && colony.census.paying < sites::SITES_IN_FLIGHT
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

        // The engine refuses a destroy while a hostile creep is in the room.
        intents.extend(
            (colony.buried.iter().chain(&colony.astray))
                .filter(|_| colony.hostiles.is_empty())
                .map(|id| Intent::Room(colony.room, RoomAct::Destroy(*id))),
        );
        intents.extend(towers::fire(
            &colony.towers,
            &colony.hostiles,
            &colony.ours,
            &colony.struck,
            &colony.ramparts,
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
        std::mem::take(&mut state.layout),
    );

    let budget = engine::MAX_CONSTRUCTION_SITES - world.our_sites;
    let home = decide_over(&world.rules, &colonies, budget);

    let placed = (home.iter())
        .filter(|i| matches!(i, Intent::Room(_, RoomAct::CreateSite(..))))
        .count() as i32;

    let piles = |room| {
        (ctx.declarations.colonies.iter())
            .find(|p| p.room == room)?
            .pile
    };
    let sites = remote::sites(
        ctx.atlas,
        world,
        ctx.derived,
        (&home, &piles),
        &colonies
            .iter()
            .filter_map(|c| c.plan.as_ref())
            .collect::<Vec<_>>(),
        budget - placed,
    );

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
