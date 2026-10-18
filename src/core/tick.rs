/// A throw in one module costs that module this tick and nothing else
/// (`ARCHITECTURE.md` §2). The clock is passed in to keep `decide` deterministic.
use std::collections::{BTreeMap, BTreeSet};

use crate::core::HashMap;

use super::atlas::{Atlas, Mover, Target, Tree};
use super::engine;
use super::grid::{self, FatigueFactor, Footprint};
use super::intents::{Ask, CreepAct, Intent, Rank, SpawnAct};
use super::mv;
use super::state::State;
use super::strategy::intel::Intel;
use super::tiles::{self, Tile};
use super::vocabulary::{BodyPart, Compound};
use super::world::{Pos, Sightings, World};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declared {
    pub id: String,
    pub pos: Pos,
}

/// What is known of a room before it is seen.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Outpost {
    pub room: String,
    pub controller: Declared,
    pub sources: Vec<Declared>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declarations {
    /// The creep lifetimes of tower fire a colony's reserve holds.
    pub reserve_lifetimes: f64,
    pub outposts: Vec<Outpost>,
    /// Declared as an outpost is, so a claimer walks to one unseen.
    pub claims: Vec<Outpost>,
    /// The one colony whose labs react, and the stock of each compound it
    /// makes toward.
    pub labs: Option<(String, BTreeMap<Compound, i32>)>,
    pub allies: BTreeSet<String>,
    pub enemies: BTreeSet<String>,
    /// The rival rooms whose towers and economy are razed.
    pub sieges: Vec<String>,
    /// Our colonies being emptied before the owner unclaims them by hand.
    pub abandon: Vec<String>,
    /// Rooms no fight body walks through on its way: a rival's towers.
    pub avoid: BTreeSet<String>,
}

pub fn undeclared() -> Declarations {
    Declarations {
        reserve_lifetimes: 2.0,
        outposts: Vec::new(),
        claims: Vec::new(),
        labs: None,
        allies: BTreeSet::new(),
        enemies: BTreeSet::new(),
        sieges: Vec::new(),
        abandon: Vec::new(),
        avoid: BTreeSet::new(),
    }
}

pub struct Context<'a> {
    pub world: &'a World,
    pub atlas: &'a mut Atlas,
    pub declarations: &'a Declarations,
    pub sightings: &'a Sightings,
    pub intel: &'a Intel,
    /// The harness's trace is wanted (`Intent::Trace`); never live.
    pub trace: bool,
}

/// A body cast under this name is Fight's for its life, so no creep is in
/// both Fight and Economy (`ARCHITECTURE.md` §2).
pub const FIGHT_PREFIX: &str = "fight-";

pub fn fights(name: &str) -> bool {
    name.starts_with(FIGHT_PREFIX)
}

/// The route search needs a bound; nothing is worked further than this.
pub const REACH_HOPS: i32 = 3;

/// A declared siege's room stands further off than any room worked.
pub const SIEGE_HOPS: i32 = 10;

pub fn target_at(pos: &Pos, range: i32) -> Target {
    Target {
        room: pos.room.clone(),
        tiles: vec![tiles::of_pos(pos)],
        range,
        avoid: BTreeSet::new(),
        max_hops: REACH_HOPS,
    }
}

/// Ticks and crossings are not one unit.
pub fn nearest(
    atlas: &mut Atlas,
    route: impl Fn(&Target, &str) -> Option<Vec<String>>,
    homes: &[Pos],
    can: impl Fn(usize) -> bool,
    at: &Pos,
) -> Option<usize> {
    let walker = Mover {
        footprint: Footprint::One,
        factor: grid::loaded_factor(&[BodyPart::Move]),
    };

    let least =
        |atlas: &mut Atlas, measure: &dyn Fn(&mut Atlas, &Pos) -> Option<i32>| -> Option<usize> {
            let mut best: Option<usize> = None;
            let mut best_d = i32::MAX;
            for (i, home) in homes.iter().enumerate() {
                if can(i)
                    && let Some(d) = measure(atlas, home)
                    && d < best_d
                {
                    best = Some(i);
                    best_d = d;
                }
            }
            best
        };

    least(atlas, &|atlas, home| {
        let t = target_at(home, 1);
        atlas.ticks_to(&route, walker, &t, at)
    })
    .or_else(|| {
        least(atlas, &|_atlas, home| {
            route(&target_at(at, 0), &home.room)
                .map(|rooms| Atlas::crossing_ticks() * (rooms.len() as i32 - 1))
        })
    })
}

pub fn colonies(world: &World) -> Vec<String> {
    world
        .rooms
        .iter()
        .filter(|r| {
            r.controller.as_ref().is_some_and(|c| c.owner.is_ours())
                && world.spawns.iter().any(|s| s.pos.room == r.name)
        })
        .map(|r| r.name.clone())
        .collect()
}

/// The declared outposts a colony's spawn reaches and no hostile seen there
/// outlives (`ARCHITECTURE.md` §13, M1b): a stood-down room is no ground.
pub fn worked(context: &Context) -> Vec<Outpost> {
    let trees: Vec<(String, Tree)> = colonies(context.world)
        .into_iter()
        .map(|home| {
            let tree = context.atlas.tree(REACH_HOPS, &home);
            (home, tree)
        })
        .collect();

    let raised = context.declarations.claims.iter().filter(|c| {
        context.world.room(&c.room).is_some_and(|r| {
            r.controller.as_ref().is_some_and(|k| k.owner.is_ours())
                && !context.world.spawns.iter().any(|s| s.pos.room == c.room)
        })
    });

    context
        .declarations
        .outposts
        .iter()
        .chain(raised)
        .filter(|o| {
            super::world::sightings::unarmed(context.sightings, context.world.time, &o.room)
                && trees
                    .iter()
                    .any(|(home, tree)| home == &o.room || tree.contains(&o.room))
        })
        .cloned()
        .collect()
}

pub type ModuleRun = fn(&mut Context, &State) -> Result<(Vec<Intent>, State), String>;

pub struct Module {
    pub name: &'static str,
    pub run: ModuleRun,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Outcome {
    Ran,
    Off,
    Faulted(String),
}

#[derive(Clone, Debug)]
pub struct Report {
    pub module: &'static str,
    pub cpu: f64,
    pub outcome: Outcome,
}

pub struct TickResult {
    pub intents: Vec<Intent>,
    pub state: State,
    pub reports: Vec<Report>,
}

fn by_room<T: Clone>(pairs: &[(String, T)]) -> Vec<(String, Vec<T>)> {
    let mut groups: HashMap<String, Vec<T>> = HashMap::default();
    let mut rooms: Vec<String> = Vec::new();
    for (room, value) in pairs {
        if !groups.contains_key(room) {
            groups.insert(room.clone(), Vec::new());
            rooms.push(room.clone());
        }
        groups.get_mut(room).unwrap().push(value.clone());
    }
    rooms
        .into_iter()
        .map(|room| {
            let values = groups.remove(&room).unwrap();
            (room, values)
        })
        .collect()
}

/// Each room's spawn queue: floors, then the fight's asks, then the
/// economy's, on its idle spawns while the bank pays; a fight ask the bank
/// cannot pay yet holds the economy's behind it, one its capacity never pays
/// holds nothing.
pub fn queue(world: &World, asks: &[(String, Ask)]) -> Vec<Intent> {
    let rank = |a: &Ask| -> i32 {
        match a.rank {
            Rank::Floor => 0,
            Rank::Fight => 1,
            Rank::Economy => 2,
        }
    };

    fn serve(
        idle: &[&super::world::Spawn],
        capacity: i32,
        bank: i32,
        held: bool,
        asks: &[Ask],
    ) -> Vec<Intent> {
        let mut out = Vec::new();
        let mut bank = bank;
        let mut held = held;
        let mut idle_at = 0;
        for a in asks {
            if idle_at >= idle.len() {
                break;
            }
            if held && a.rank.is_economy() {
                break;
            }
            if engine::body_cost(&a.body) <= bank {
                out.push(Intent::Spawn(
                    idle[idle_at].id.clone(),
                    SpawnAct::SpawnCreep(a.body.clone(), a.name.clone()),
                ));
                bank -= engine::body_cost(&a.body);
                idle_at += 1;
            } else {
                held = held || (a.rank.is_fight() && engine::body_cost(&a.body) <= capacity);
            }
        }
        out
    }

    let mut out = Vec::new();
    for (room, mut asks) in by_room(asks) {
        asks.sort_by_key(rank);
        if let Some(r) = world.room(&room) {
            let idle: Vec<&super::world::Spawn> = world
                .spawns
                .iter()
                .filter(|s| s.pos.room == room && s.spawning.is_none())
                .collect();
            out.extend(serve(
                &idle,
                r.energy_capacity,
                r.energy_available,
                false,
                &asks,
            ));
        }
    }
    out
}

/// One arbitration per room over every module's wishes (§3's `Move`); a creep
/// of ours that wishes nothing holds its tile.
pub fn arbitrate(context: &Context, wishes: &[(String, mv::Wish)]) -> Vec<Intent> {
    let mut wished: BTreeSet<String> = BTreeSet::new();
    let groups = by_room(wishes);
    let mut scratch = mv::arbitrate::scratch();
    let mut out = Vec::new();

    for (_, ws) in &groups {
        for w in ws {
            wished.insert(w.creep.clone());
        }
    }

    for (room, ws) in groups {
        let held: Vec<Tile> = context
            .world
            .creeps
            .iter()
            .filter(|c| c.pos.room == room && !wished.contains(&c.name))
            .map(|c| tiles::of_pos(&c.pos))
            .collect();
        let steps = mv::arbitrate::arbitrate_with(
            &mut scratch,
            |tile| context.atlas.passable(&room, tile),
            &held,
            &ws,
        );
        out.extend(
            steps
                .into_iter()
                .map(|(name, direction)| Intent::Creep(name, CreepAct::Move(direction))),
        );
    }
    out
}

fn settle(context: &Context, intents: Vec<Intent>) -> Vec<Intent> {
    let mut asks = Vec::new();
    let mut wishes = Vec::new();
    for i in &intents {
        match i {
            Intent::Queue(room, ask) => asks.push((room.clone(), ask.clone())),
            Intent::Wish(room, wish) => wishes.push((room.clone(), wish.clone())),
            _ => (),
        }
    }
    let mut out = queue(context.world, &asks);
    out.extend(
        intents
            .into_iter()
            .filter(|i| !i.is_queue() && !i.is_wish()),
    );
    out.extend(arbitrate(context, &wishes));
    out
}

pub fn run(
    clock: impl Fn() -> f64,
    modules: &[Module],
    context: &mut Context,
    state: State,
) -> TickResult {
    let mut intents: Vec<Intent> = Vec::new();
    let mut state = state;
    let mut reports: Vec<Report> = Vec::new();

    for m in modules {
        if state.off.contains(m.name) {
            reports.push(Report {
                module: m.name,
                cpu: 0.0,
                outcome: Outcome::Off,
            });
            continue;
        }
        let start = clock();
        let (produced, next, outcome) = match (m.run)(context, &state) {
            Ok((produced, next)) => (produced, next, Outcome::Ran),
            Err(error) => (Vec::new(), state.clone(), Outcome::Faulted(error)),
        };
        reports.push(Report {
            module: m.name,
            cpu: clock() - start,
            outcome,
        });
        intents.extend(produced);
        state = next;
    }

    let start = clock();

    // A fault settling the fight's asks and wishes costs only the fight's.
    let is_fight = |i: &Intent| -> bool {
        match i {
            Intent::Queue(_, ask) => ask.rank.is_fight(),
            Intent::Wish(_, wish) => fights(&wish.creep),
            _ => false,
        }
    };

    let (settled, outcome) = if let Ok(settled) =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            settle(context, intents.clone())
        })) {
        (settled, Outcome::Ran)
    } else {
        let rest: Vec<Intent> = intents.iter().filter(|i| !is_fight(i)).cloned().collect();
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| settle(context, rest))) {
            Ok(settled) => (settled, Outcome::Faulted("settle".to_string())),
            Err(_) => (
                intents
                    .into_iter()
                    .filter(|i| !i.is_queue() && !i.is_wish())
                    .collect(),
                Outcome::Faulted("settle".to_string()),
            ),
        }
    };

    reports.push(Report {
        module: "settle",
        cpu: clock() - start,
        outcome,
    });

    TickResult {
        intents: settled,
        state,
        reports,
    }
}

/// The modules `Main` runs, in this order.
pub const MODULES: [Module; 4] = [
    Module {
        name: "strategy",
        run: super::strategy::decide,
    },
    Module {
        name: "fight",
        run: super::fight::decide,
    },
    Module {
        name: "economy",
        run: super::economy::decide,
    },
    Module {
        name: "layout",
        run: super::layout::decide,
    },
];

pub fn module_list() -> &'static [Module] {
    &MODULES
}

/// A `Target` for a creep's own tile, as `Strategy` and `Layout` build one.
pub fn target_of(pos: &Pos, range: i32) -> Target {
    target_at(pos, range)
}

pub fn walker_of(body: &[BodyPart]) -> Mover {
    Mover {
        footprint: Footprint::One,
        factor: grid::loaded_factor(body),
    }
}

pub fn factor_loaded(body: &[BodyPart]) -> FatigueFactor {
    grid::loaded_factor(body)
}

pub fn pairs_by_room<T: Clone>(pairs: &[(String, T)]) -> Vec<(String, Vec<T>)> {
    by_room(pairs)
}

pub type Effects = BTreeMap<String, String>;
