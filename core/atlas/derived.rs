use std::collections::BTreeSet;

use super::{Atlas, Mover, Target, Tree};
use fabot_base::declarations::{Declarations, Outpost, REACH_HOPS};
use fabot_base::engine;
use fabot_base::grid::{self, Footprint};
use fabot_base::intel::Intel;
use fabot_base::tiles::{self, TileSet};
use fabot_base::vocabulary::{Part, Resource};
use fabot_base::world::{Creep, Kind, Owner, Pos, Room, RoomName, Sightings, World, hostiles};

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

/// Ticks and crossings are not one unit. `trees` holds the homes' rooms'.
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
        least(atlas, &|_atlas, home| {
            (trees(home.room)?.crossings(at.room, home.room)).map(|n| Atlas::CROSSING_TICKS * n)
        })
    })
}

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
    pub trees: Vec<(RoomName, Tree)>,
}

impl Derived {
    pub fn danger(&self, room: RoomName) -> Option<&TileSet> {
        (self.danger.iter()).find_map(|(r, d)| (*r == room).then_some(d))
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
fn danger(world: &World, owned: &BTreeSet<RoomName>) -> Vec<(RoomName, TileSet)> {
    let reaches = |r: &Room| {
        let creeps = (r.hostiles())
            .filter(|c| c.owner != Owner::Keeper && hostiles::strikes(c))
            .map(|c| (tiles::of_pos(&c.pos), hostiles::reach(c)));
        let towers = (r.structures.iter())
            .filter(|s| {
                s.kind == Kind::Tower
                    && s.owner.is_theirs()
                    && !s.owner.is_ally()
                    && s.store.amount_of(&Resource::Energy) >= engine::TOWER_ENERGY_COST
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
    let spawned = |room: RoomName| world.spawns.iter().any(|s| s.pos.room == room);
    let colonies: Vec<RoomName> = (world.rooms.iter())
        .map(|r| r.name)
        .filter(|room| owned.contains(room) && spawned(*room))
        .collect();

    let trees: Vec<(RoomName, Tree)> = (colonies.iter())
        .map(|home| (*home, atlas.tree(&BTreeSet::new(), REACH_HOPS, *home)))
        .collect();
    Derived {
        danger: danger(world, &owned),
        capped: owned.len() as i32 >= world.rooms_allowed,
        owned,
        colonies,
        worked: Vec::new(),
        trees,
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

/// Whether an outpost's work is open (`ARCHITECTURE.md` §13, M1b): a foe
/// stands down the work it can reach and no more.
fn open(world: &World, sightings: &Sightings, owned: &BTreeSet<RoomName>, room: RoomName) -> bool {
    let Some(seen) = world.room(room) else {
        return fabot_base::world::sightings::unarmed(sightings, world.time, room);
    };

    let core = seen
        .structures
        .iter()
        .any(|s| s.kind == Kind::InvaderCore && s.level > 0);
    let foes: Vec<Creep> = seen.hostiles().cloned().collect();
    let work: Vec<&Pos> = seen
        .sources
        .iter()
        .map(|s| &s.pos)
        .chain(
            seen.structures
                .iter()
                .filter(|s| s.kind == Kind::Container)
                .map(|s| &s.pos),
        )
        .collect();

    !core
        && !foes.iter().any(|c| {
            // An Invader's raid only ever walks to the work, so it stands the
            // room down on sight; a player's only the work it can strike.
            c.owner == Owner::Invader
                || (hostiles::armed(owned, &foes, c)
                    && work.iter().any(|at| c.pos.within(hostiles::reach(c), at)))
        })
}
