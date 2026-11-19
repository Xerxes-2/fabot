use crate::atlas::Context;
use crate::engine;
use crate::intents::{Action, ModuleOutput, RoomAct};
use crate::world::Rules;

use super::derive;
use super::remote;
use super::shape::LayoutState;
use super::sites;
use super::towers;

pub fn decide_over(rules: &Rules, colonies: &[derive::Colony], mut budget: i32) -> Vec<Action> {
    let mut intents: Vec<Action> = Vec::new();

    for colony in colonies {
        if let Some(plan) = &colony.plan
            && budget > 0
            && colony.census.paying < sites::SITES_IN_FLIGHT
            && sites::may_place(rules, &colony.census, colony.target)
        {
            for p in sites::place(rules, &colony.census, colony.target, plan, budget) {
                budget -= 1;
                intents.push(Action::Room(
                    colony.room,
                    RoomAct::CreateSite(p.x, p.y, p.kind),
                ));
            }
        }

        // The engine refuses a destroy while a hostile creep is in the room.
        intents.extend(
            (colony.buried.iter().chain(&colony.astray))
                .filter(|_| colony.hostiles.is_empty())
                .map(|id| Action::Room(colony.room, RoomAct::Destroy(*id))),
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

pub fn decide(ctx: &mut Context, standing: &mut LayoutState) -> Result<ModuleOutput, String> {
    let world = ctx.world;
    let (colonies, kept) = derive::colonies(
        world,
        &ctx.derived.owned,
        &ctx.declarations.colonies,
        std::mem::take(standing),
    );

    let budget = engine::MAX_CONSTRUCTION_SITES - world.our_sites;
    let home = decide_over(&world.rules, &colonies, budget);

    let placed = (home.iter())
        .filter(|i| matches!(i, Action::Room(_, RoomAct::CreateSite(..))))
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

    *standing = kept;
    Ok(intents.into())
}
