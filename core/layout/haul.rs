//! What a plan's energy walks before its tunnels stand: each source's post to
//! the hub and the hub to the upgrade container.
use fabot_base::{
    engine,
    grid::{self, FatigueFactor},
    tiles::{self, Tile},
    vocabulary::Part,
    world::{Built as _, StructureType},
};

use crate::{
    ground::Ground,
    path::{Goal, Search},
    place::{BLOCKED, SWAMP, WALL},
    shape::Plan,
};

/// The share of all energy carried on from the hub to the controller.
const UPGRADED: f64 = 0.5;

/// Each walk in ticks, None where none arrives.
pub struct Walks {
    pub posts: Vec<Option<u32>>,
    pub controller: Option<u32>,
}

impl Walks {
    /// The CARRY a source's yield fills over each walk there and back.
    pub fn haul(&self) -> Option<f64> {
        let n = self.posts.len() as f64;
        let posts: u32 = self.posts.iter().copied().sum::<Option<u32>>()?;
        let walked = f64::from(posts) + UPGRADED * n * f64::from(self.controller?);
        Some(engine::carry_parts(2.0 * walked * f64::from(engine::HELD_OUTPUT_PER_TICK)))
    }
}

/// A loaded hauler of one CARRY a MOVE.
fn pace() -> FatigueFactor {
    grid::loaded_factor(&[Part::Carry, Part::Move])
}

fn ticks(weight: i32) -> u32 {
    grid::step_ticks(pace(), weight).map_or(u32::MAX, |t| t as u32)
}

/// Planned roads walked, but none through rock, a structure in the way.
pub fn matrix(ground: &Ground, plan: &Plan) -> Vec<u8> {
    let road = ticks(engine::ROAD_WEIGHT) as u8;
    let mut m = vec![0; tiles::COUNT];
    for i in plan.items.iter().chain(&plan.ring) {
        let t = i.tile.index();
        if !i.kind.walkable() || (i.kind == StructureType::Road && ground.terrain[t] == WALL) {
            m[t] = BLOCKED;
        } else if i.kind == StructureType::Road && m[t] == 0 {
            m[t] = road;
        }
    }
    m
}

pub fn walker<'a>(ground: &'a Ground, matrix: &'a [u8]) -> Search<'a> {
    let (plain, swamp) = (ticks(engine::PLAIN_WEIGHT), ticks(engine::SWAMP_WEIGHT));
    Search { terrain: &ground.terrain, matrix, plain, swamp }
}

/// None without a controller or a post's container.
pub fn walks(ground: &Ground, plan: &Plan) -> Option<Walks> {
    let m = matrix(ground, plan);
    let walk = walker(ground, &m);
    let tick = |t: &Tile| match (m[t.index()], ground.terrain[t.index()]) {
        (0, SWAMP) => walk.swamp,
        (0, _) => walk.plain,
        (road, _) => u32::from(road),
    };
    let ticks = |from: Tile| {
        // A plan whose centre a structure takes is unloaded beside it.
        let range = u32::from(m[plan.anchor.index()] == BLOCKED);
        let found = walk.run(from, &[Goal { tile: plan.anchor, range }]);
        (!found.incomplete).then(|| found.path.iter().map(tick).sum())
    };
    let containers = || plan.items.iter().filter(|i| i.kind == StructureType::Container);
    let beside =
        |s: &Tile| containers().filter(|i| tiles::range(i.tile, *s) == 1).min_by_key(|i| i.tile);
    let post = |s: &Tile| beside(s).map(|i| ticks(i.tile));
    let posts = ground.sources.iter().map(post).collect::<Option<_>>()?;
    let controller = ground.controller?;
    let buffer = containers().filter(|i| ground.buffers(i.tile));
    let buffer = buffer.min_by_key(|i| (tiles::range(i.tile, controller), i.tile))?;
    Some(Walks { posts, controller: ticks(buffer.tile) })
}
