use std::collections::BTreeSet;

use super::loot::{fallen, garrison};
use super::{Atlas, Mover, Target, Tree};
use fabot_base::declarations::{ColonyPlan, Declarations, Outpost, REACH_HOPS, Stage};
use fabot_base::grid::{self, Footprint};
use fabot_base::intel::Intel;
use fabot_base::tiles::{self, TileSet};
use fabot_base::vocabulary::{Part, ResourceType};
use fabot_base::world::{
    Creep, Owner, Pos, Room, RoomName, Sightings, StructureType, World, hostiles,
};
use fabot_base::{boost, engine};

pub struct Context<'a> {
    pub world: &'a World<'a>,
    pub atlas: &'a mut Atlas,
    pub declarations: &'a Declarations,
    pub sightings: &'a Sightings,
    pub intel: &'a Intel,
    pub derived: &'a Derived,
    /// The harness's trace is wanted (`Telemetry.traces`); never live.
    pub trace: bool,
}

#[inline]
pub fn target_at(pos: &Pos, range: i32) -> Target {
    Target {
        room: pos.room,
        tiles: vec![tiles::of_pos(pos)],
        range,
        avoid: BTreeSet::new(),
        max_hops: REACH_HOPS,
    }
}

pub fn nearest<'t>(
    atlas: &mut Atlas,
    trees: &dyn Fn(RoomName) -> Option<&'t Tree>,
    homes: &[Pos],
    can: impl Fn(usize) -> bool,
    at: &Pos,
) -> Option<usize> {
    let walker = Mover {
        footprint: Footprint::One,
        factor: grid::loaded_factor(&[Part::Move]),
    };

    let least =
        |atlas: &mut Atlas, measure: &dyn Fn(&mut Atlas, &Pos) -> Option<i32>| -> Option<usize> {
            (homes.iter().enumerate())
                .filter(|(i, _)| can(*i))
                .filter_map(|(i, home)| Some((measure(atlas, home)?, i)))
                .filter(|(d, _)| *d < i32::MAX)
                .min_by_key(|(d, _)| *d)
                .map(|(_, i)| i)
        };

    least(atlas, &|atlas, home| {
        let t = target_at(home, 1);
        match trees(home.room) {
            Some(tree) if home.room != at.room => atlas.ticks_on(tree, walker, &t, at),
            _ => atlas.ticks_to(walker, &t, at),
        }
    })
    .or_else(|| {
        least(atlas, &|atlas, home| {
            atlas.ticks_toward(REACH_HOPS, home, at)
        })
    })
}

pub fn plan_of<'a>(
    declarations: &'a Declarations,
    plans: &'a [ColonyPlan],
    room: RoomName,
) -> Option<&'a ColonyPlan> {
    (declarations.colonies.iter().chain(plans)).find(|p| p.room == room)
}

/// The colony a claim is served from, its claimer, raising and pioneers alike
/// (§5), so the claim holds that one alone (§7.2): the nearest stocked, else
/// the nearest whose bank casts a claimer, so no claim waits on a storage.
pub fn sponsor(
    atlas: &mut Atlas,
    derived: &Derived,
    given_up: &dyn Fn(RoomName) -> bool,
    at: &Pos,
) -> Option<usize> {
    let spawns: Vec<Pos> = derived.homes.iter().map(|h| h.spawn).collect();
    let claimer = engine::body_cost(&[Part::Claim, Part::Move]);
    let pays = |i: usize| {
        let room = derived.colonies[i];
        room != at.room && !given_up(room) && derived.homes[i].capacity >= claimer
    };
    let stocked = |i: usize| pays(i) && derived.homes[i].stocked;
    let trees = |room| derived.tree(room);
    nearest(atlas, &trees, &spawns, stocked, at)
        .or_else(|| nearest(atlas, &trees, &spawns, pays, at))
}

/// The colony that serves a worked room, for Economy's haul and Strategy's
/// defence alike (§5): a claim its sponsor, an outpost the nearest on foot
/// that banks the haul. `planned`: a colony's merged plan.
pub fn server<'p>(
    atlas: &mut Atlas,
    derived: &Derived,
    planned: &dyn Fn(RoomName) -> Option<&'p ColonyPlan>,
    (at, claim): (&Pos, bool),
) -> Option<usize> {
    let given_up = |room| planned(room).is_some_and(|p| p.stage == Stage::Abandon);
    if claim {
        return sponsor(atlas, derived, &given_up, at);
    }
    let banks = |i: usize| {
        let (room, home) = (derived.colonies[i], &derived.homes[i]);
        !given_up(room) && planned(room).is_some_and(|p| home.banks(p))
    };
    let spawns: Vec<Pos> = derived.homes.iter().map(|h| h.spawn).collect();
    nearest(atlas, &|room| derived.tree(room), &spawns, banks, at)
}

/// A colony's facts a sponsor is picked by, `Derived.colonies`' order.
#[derive(Debug)]
pub struct Home {
    pub spawn: Pos,
    pub stocked: bool,
    pub capacity: i32,
}

#[derive(Debug)]
pub struct Derived {
    pub owned: BTreeSet<RoomName>,
    /// Every room GCL allows is owned.
    pub capped: bool,
    /// The owned rooms a spawn stands in, in `World.rooms`' order.
    pub colonies: Vec<RoomName>,
    /// The declared outposts and raised claims a colony reaches, whose work
    /// no foe stands down (`ARCHITECTURE.md` §13, M1b).
    pub worked: Vec<Outpost>,
    /// What a body outside a fight walks round, per room with any.
    pub danger: Vec<(RoomName, TileSet)>,
    /// What a harvester walks round, its seat and the walk to it: the strikes no guard holds (v1).
    pub exposed: Vec<(RoomName, TileSet)>,
    pub trees: Vec<(RoomName, Tree)>,
    pub homes: Vec<Home>,
    /// The colonies whose labs react and boost (`boost::clusters`), their
    /// boosting read before Strategy plans, as Economy's labs are.
    pub boosts: Vec<(RoomName, boost::Boosting)>,
}

/// A colony banks its haul from here (§5): on its storage, else on its hub's pile.
pub const BANK_LEVEL: i32 = 2;

impl Home {
    pub fn banks(&self, plan: &ColonyPlan) -> bool {
        plan.target >= BANK_LEVEL && (self.stocked || plan.pile.is_some())
    }
}

impl Derived {
    pub fn danger(&self, room: RoomName, seated: bool) -> Option<&TileSet> {
        let sets = if seated { &self.exposed } else { &self.danger };
        (sets.iter()).find_map(|(r, d)| (*r == room).then_some(d))
    }

    pub fn home(&self, room: RoomName) -> Option<&Home> {
        (self.colonies.iter().zip(&self.homes)).find_map(|(c, h)| (*c == room).then_some(h))
    }

    #[inline]
    pub fn tree(&self, home: RoomName) -> Option<&Tree> {
        (self.trees.iter()).find_map(|(h, t)| (*h == home).then_some(t))
    }
}

/// A keeper's strikes the Atlas walls already; a tower strikes the whole
/// room, its reach taken where its fire is above its weakest. None in a room
/// we own: its refills keep the towers firing, and the strikes they take are
/// what calls the safe mode.
fn danger(world: &World, owned: &BTreeSet<RoomName>, guarded: bool) -> Vec<(RoomName, TileSet)> {
    let reaches = |r: &Room| {
        let guards = guards(world, r.name);
        let creeps = (r.hostiles())
            .filter(|c| hostiles::foe(c) && hostiles::strikes(c) && !garrison(r, c))
            .filter(|c| !(guarded && held(&guards, c)))
            .map(|c| (tiles::of_pos(&c.pos), hostiles::reach(c)));
        let towers = (r.structures.iter())
            .filter(|s| {
                s.kind == StructureType::Tower
                    && s.owner.is_theirs()
                    && !(s.owner == Owner::Invader && fallen(r))
                    && s.store.amount_of(ResourceType::Energy) >= engine::TOWER_ENERGY_COST
            })
            .map(|s| (tiles::of_pos(&s.pos), engine::TOWER_FALLOFF_RANGE));
        creeps.chain(towers).collect::<Vec<_>>()
    };
    (world.rooms.iter())
        .filter(|r| !owned.contains(&r.name))
        .filter_map(|r| {
            let reaches = reaches(r);
            let set: TileSet = (reaches.iter())
                .flat_map(|(at, reach)| tiles::within(*reach, *at))
                .collect();
            (!reaches.is_empty()).then_some((r.name, set))
        })
        .collect()
}

pub fn derive(world: &World, atlas: &mut Atlas) -> Derived {
    let owned = hostiles::owned(&world.rooms);
    let spawned = |room: RoomName| world.working_spawns().any(|s| s.pos.room == room);
    let colonies: Vec<RoomName> = (world.rooms.iter())
        .map(|r| r.name)
        .filter(|room| owned.contains(room) && spawned(*room))
        .collect();

    let trees: Vec<(RoomName, Tree)> = (colonies.iter())
        .map(|home| (*home, atlas.tree(&BTreeSet::new(), REACH_HOPS, *home)))
        .collect();
    let homes = (colonies.iter())
        .filter_map(|home| {
            let room = world.room(*home)?;
            Some(Home {
                spawn: world.working_spawns().find(|s| s.pos.room == *home)?.pos,
                stocked: (room.structures.iter())
                    .any(|s| s.kind == StructureType::Storage && s.owner.is_ours()),
                capacity: room.energy_capacity,
            })
        })
        .collect();
    Derived {
        danger: danger(world, &owned, false),
        exposed: danger(world, &owned, true),
        homes,
        capped: owned.len() as i32 >= world.rooms_allowed,
        owned,
        colonies,
        worked: Vec::new(),
        trees,
        boosts: Vec::new(),
    }
}

pub fn worked(
    world: &World,
    sightings: &Sightings,
    declarations: &Declarations,
    derived: &Derived,
) -> Vec<Outpost> {
    let owned = &derived.owned;
    let spawned = |room: RoomName| derived.colonies.contains(&room);
    let raised =
        (declarations.claims.iter()).filter(|c| owned.contains(&c.room) && !spawned(c.room));
    (declarations.outposts.iter())
        .filter(|o| !owned.contains(&o.room))
        .chain(raised)
        .filter(|o| {
            open(world, sightings, owned, o.room)
                && (derived.trees.iter()).any(|(_, tree)| tree.contains(o.room))
        })
        .cloned()
        .collect()
}

fn guards<'w>(world: &'w World, room: RoomName) -> Vec<&'w Creep> {
    (world.creeps.iter())
        .filter(|c| c.pos.room == room)
        .collect()
}

/// v1's guard: a body of ours striking as far as the raider, and in reach of it, holds it off the work.
fn held(guards: &[&Creep], foe: &Creep) -> bool {
    let range = |c: &Creep| engine::strike_reach(|p| c.live(p) > 0);
    (guards.iter()).any(|g| {
        range(g) > 0 && range(g) >= range(foe) && g.pos.within(hostiles::reach(g), &foe.pos)
    })
}

/// Whether an outpost's work is open (`ARCHITECTURE.md` §13, M1b): a foe
/// stands down the work it can reach and no more.
fn open(world: &World, sightings: &Sightings, owned: &BTreeSet<RoomName>, room: RoomName) -> bool {
    let Some(seen) = world.room(room) else {
        return fabot_base::world::sightings::unarmed(sightings, world.time, room);
    };

    let core = seen.invader_core().is_some();
    let foes: Vec<Creep> = seen.hostiles().cloned().collect();
    let guards = guards(world, room);
    let work: Vec<&Pos> = seen
        .sources
        .iter()
        .map(|s| &s.pos)
        .chain(
            seen.structures
                .iter()
                .filter(|s| s.kind == StructureType::Container)
                .map(|s| &s.pos),
        )
        .collect();

    !core
        && !foes.iter().any(|c| {
            // An armed Invader only ever walks to the work, so it stands the
            // room down on sight; a player's only the work it can strike.
            hostiles::armed(owned, foes.iter(), c)
                && (c.owner == Owner::Invader
                    || !held(&guards, c)
                        && work.iter().any(|at| c.pos.within(hostiles::reach(c), at)))
        })
}
