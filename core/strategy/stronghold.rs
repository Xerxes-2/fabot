/// The stronghold Fight takes next (§7.3), and the rooms Economy hauls its loot from.
use std::cmp::Reverse;
use std::collections::BTreeSet;

use fabot_atlas::Atlas;
use fabot_atlas::Derived;
use fabot_base::declarations::{FAR_HOPS, OpKind, Operation};
use fabot_base::engine;
use fabot_base::intel::Intel;
use fabot_base::world::{RoomName, World};
use fabot_fight::kinds::stronghold;
use fabot_fight::shape::{Cast, Engagement, Objective};

/// L4's fortifier and its garrison's reach are not priced yet; L5 is out (owner).
const MOST: i32 = 3;

/// Its loot is declared with it, so the haul is cast as the core falls.
pub fn rows(
    world: &World,
    intel: &Intel,
    (derived, avoid): (&Derived, &BTreeSet<RoomName>),
    (engagements, casts): (&[Engagement], &[Cast]),
    atlas: &Atlas,
) -> Vec<Operation> {
    // Priced once (owner): no rank or collapse displaces a cast set's row.
    let committed: Vec<RoomName> = (engagements.iter())
        .filter(|e| e.objective == Objective::Stronghold && !e.unanswered)
        .map(|e| e.room)
        .collect();
    // Out of sight, the loot as last seen: a body there sees it taken.
    let unseen = (intel.iter()).filter(|(room, row)| {
        world.room(**room).is_none()
            && row
                .loot
                .as_ref()
                .is_some_and(|(c, l)| *c > world.time && !l.is_empty())
    });
    let fallen: Vec<RoomName> = (world.rooms.iter())
        .filter(|r| fabot_atlas::fallen_with_loot(r))
        .map(|r| r.name)
        .chain(unseen.map(|(room, _)| *room))
        .collect();
    let standing =
        |(room, row): (&RoomName, &fabot_base::intel::Row)| Some((*room, row.stronghold?));
    let candidates: Vec<_> = intel.iter().filter_map(standing).collect();
    let mut taken = None;
    let mut reached: Vec<RoomName> = Vec::new();
    if !(candidates.is_empty() && fallen.is_empty()) {
        let colonies: Vec<(RoomName, i32)> = (derived.colonies.iter())
            .filter_map(|c| Some((*c, world.room(*c)?.energy_capacity)))
            .collect();
        let spawning: Vec<Cast> = (casts.iter())
            .filter(|c| !world.creeps.iter().any(|o| o.name == c.name))
            .cloned()
            .collect();
        let stocks = fabot_fight::casting::cluster_boosts(&derived.boosts, &spawning, engagements);
        // Fight's walks: one carried tree a room, its crossings from each colony.
        let near = |room: RoomName| {
            let tree = atlas.tree(avoid, FAR_HOPS, room);
            let walk = |c| Some(engine::ROOM_SIDE * tree.crossings(c, room)?);
            stronghold::priced_from(&colonies, walk, &stocks)
        };
        // Fight takes one at a time: none is priced while a set is cast.
        let mut ready: Vec<_> = (candidates.into_iter().filter(|_| committed.is_empty()))
            .filter(|(_, s)| s.level <= MOST && s.deploys_at <= world.time)
            .filter_map(|(room, s)| Some((Reverse(s.level), near(room)?, room, s)))
            .collect();
        ready.sort_by_key(|(level, (_, _, _, walk), room, _)| (*level, *walk, *room));
        taken = (ready.into_iter()).find_map(|(_, (_, stock, capacity, walk), room, s)| {
            let left = s.collapses_at - world.time;
            stronghold::take(&s, left, (&stock, capacity, walk)).map(|_| room)
        });
        reached = fallen.into_iter().filter(|r| near(*r).is_some()).collect();
    }
    let rooms: BTreeSet<RoomName> = committed.into_iter().chain(taken).chain(reached).collect();
    let row = |kind| {
        move |room: &RoomName| Operation {
            kind,
            room: *room,
            priority: 0.0,
        }
    };
    (rooms.iter().map(row(OpKind::Stronghold)))
        .chain(rooms.iter().map(row(OpKind::Pickup)))
        .collect()
}
