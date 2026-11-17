pub mod build;
pub mod cut;
pub mod derive;
pub mod ground;
pub mod path;
pub mod place;
pub mod plan;
pub mod remote;
pub mod run;
pub mod score;
pub mod shape;
pub mod sites;
pub mod towers;

#[cfg(test)]
pub(crate) mod fixtures;

pub use plan::RING_LEVEL;
pub use run::decide;
pub use shape::{Item, Plan, Standing};

/// A room before anything stands in it, its controller ours at `level`: what
/// the plan tool and `sim/` plan a fresh colony from.
#[cfg(feature = "tools")]
pub fn bare(
    name: crate::world::RoomName,
    terrain: Vec<u8>,
    (controller, sources, minerals): (
        crate::world::Pos,
        &[crate::world::Pos],
        &[crate::world::Pos],
    ),
    level: i32,
) -> crate::world::Room<'static> {
    use crate::world::{Controller, Mineral, Owner, Room, Source};
    Room {
        name,
        terrain: crate::world::terrain::leak(terrain),
        controller: Some(Controller {
            id: "controller".into(),
            pos: controller,
            owner: Owner::Ours,
            level,
            progress: 0,
            progress_total: 0,
            ticks_to_downgrade: 0,
            reservation: None,
            safe_mode: 0,
            safe_mode_available: 0,
            safe_mode_cooldown: 0,
            upgrade_blocked: 0,
        }),
        energy_available: 0,
        energy_capacity: 0,
        sources: (sources.iter().enumerate())
            .map(|(i, pos)| Source {
                id: format!("source{i}").as_str().into(),
                pos: *pos,
                energy: 0,
                energy_capacity: 0,
                ticks_to_regeneration: 0,
            })
            .collect(),
        minerals: (minerals.iter().enumerate())
            .map(|(i, pos)| Mineral {
                id: format!("mineral{i}").as_str().into(),
                pos: *pos,
                resource: crate::vocabulary::Resource::Energy,
                amount: 0,
            })
            .collect(),
        structures: Vec::new(),
        sites: Vec::new(),
        piles: Vec::new(),
        remains: Vec::new(),
        others: Vec::new(),
    }
}

/// A room's plan and the tile it puts the first spawn on.
pub fn first_spawn(
    rules: &crate::world::Rules,
    room: &crate::world::Room,
) -> Option<(Plan, crate::tiles::Tile)> {
    let plan = plan::of_ground(rules, &ground::of_room(room)?)?;
    let tile = plan
        .items
        .iter()
        .find(|i| i.kind == crate::world::Kind::Spawn)?
        .tile;
    Some((plan, tile))
}
