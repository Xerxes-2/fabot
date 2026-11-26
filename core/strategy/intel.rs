use std::collections::{BTreeMap, BTreeSet};

use fabot_atlas::Atlas;
use fabot_base::{
    declarations::{self, Declarations, Declared},
    engine,
    intel::{Intel, Row, STALE, Scout},
    intents::Action,
    stronghold::{Stronghold, raw},
    tiles,
    vocabulary::Part,
    world::{Owner, Room, RoomName, Rules, Structure, StructureType, World},
};

pub fn step(world: &World, atlas: &Atlas, intel: &mut Intel) {
    for room in &world.rooms {
        let c = room.controller.as_ref();
        let row = Row {
            seen: world.time,
            owner: c.and_then(|c| c.owner.username(&world.me)),
            level: c.map_or(0, |c| c.level),
            reservation: c
                .and_then(|c| c.reservation.as_ref())
                .and_then(|(owner, left)| Some((owner.username(&world.me)?, world.time + left))),
            // An ally's body is never hunted, so it never hides an enemy's.
            worker: room
                .hostiles()
                .filter(|h| h.owner != Owner::Invader)
                .find(|h| h.body.iter().any(|p| p.kind == Part::Work))
                .and_then(|h| h.owner.username(&world.me)),
            lairs: Some(Atlas::posts(room)),
            controller: c.map(|c| Declared { id: c.id, pos: c.pos }),
            sources: Some(
                (room.sources.iter()).map(|s| Declared { id: s.id, pos: s.pos }).collect(),
            ),
            contained: (room.sources.iter()).all(|s| {
                (room.structures.iter())
                    .any(|c| c.kind == StructureType::Container && c.pos.within(1, &s.pos))
            }),
            mineral: (room.minerals.first())
                .and_then(|m| Some((m.resource?, Some(tiles::of_pos(&m.pos))))),
            stronghold: room.invader_core().map(|s| Stronghold {
                level: s.level,
                deploys_at: world.time + s.deploy,
                collapses_at: world.time + s.collapse,
                loot: worth(&world.rules, room, atlas, s.level),
            }),
            towered: (world.rules.towers(room)).any(|s| s.owner.is_theirs()),
            // Its collapse ends its loot unseen; walled loot stands while a wall is reachable.
            loot: fabot_atlas::fallen_with_loot(room).then(|| {
                let last = intel.get(&room.name);
                let collapse = (last.and_then(|r| r.stronghold.map(|s| s.collapses_at)))
                    .or(last.and_then(|r| Some(r.loot.as_ref()?.0)))
                    .unwrap_or(world.time);
                let breaks = fabot_atlas::loot_wall(room, atlas).is_some();
                let walled =
                    |at: &Declared| !fabot_atlas::walls_of(room, atlas, &at.pos).is_empty();
                let loot = (fabot_atlas::loot_in(room).into_iter())
                    .map(|(at, r, n)| (walled(&at), at, r, n))
                    .filter(|(w, ..)| breaks || !w)
                    .map(|(w, at, r, n)| (at, r, n, w));
                (collapse, loot.collect())
            }),
        };
        intel.insert(room.name, row);
    }
}

/// Loot no set reaches is none: Strategy and Fight price only what they can take.
fn worth(rules: &Rules, room: &Room, atlas: &Atlas, level: i32) -> i32 {
    let held = fabot_atlas::loot_in(room);
    if held.is_empty() || !fabot_fight::kinds::stronghold::reached(rules, room, atlas, level) {
        return 0;
    }
    let held = held.into_iter();
    held.map(|(_, r, n)| raw(r) * f64::from(n)).sum::<f64>() as i32
}

fn declared(declarations: &Declarations) -> impl Iterator<Item = &RoomName> {
    declarations.claims.iter().chain(&declarations.outposts).map(|o| &o.room)
}

pub fn preyed(intel: &Intel, declarations: &Declarations, time: i32) -> Vec<RoomName> {
    let enemy =
        |user: &Option<String>| user.as_ref().is_some_and(|u| declarations.enemies.contains(u));
    let mut rooms: Vec<(i32, &RoomName)> = intel
        .iter()
        .filter(|(room, _)| !declarations.avoid.contains(*room))
        .filter(|(_, r)| r.owner.is_none() || !r.towered)
        .filter(|(_, r)| {
            enemy(&r.owner)
                || enemy(&r.worker)
                || r.reservation
                    .as_ref()
                    .is_some_and(|(u, lapse)| *lapse > time && declarations.enemies.contains(u))
        })
        .map(|(room, r)| (r.seen, room))
        .collect();
    rooms.sort();
    rooms.into_iter().map(|(_, room)| *room).collect()
}

/// Rooms a rival raising §5's reserve could hold and the remotes outposts
/// picks from, so from RCL1; by route too, as one walled off by terrain raids no sooner than a far one.
/// A guarded room, in no tree, sits one past its neighbour so a scout can clear its row.
pub fn near<'a>(
    (world, atlas): (&'a World, &'a Atlas),
    (intel, declarations): (&'a Intel, &'a Declarations),
    home: RoomName,
) -> impl Iterator<Item = (RoomName, i32)> + 'a {
    let me = &world.me;
    let guarded = |room: &RoomName| intel.get(room).is_some_and(|r| r.guarded(me, declarations));
    let tree = atlas.tree(&BTreeSet::new(), super::NEAR, home);
    (-super::NEAR..=super::NEAR)
        .flat_map(move |dx| (-super::NEAR..=super::NEAR).map(move |dy| home.offset(dx, dy)))
        .flatten()
        .filter(|room| tiles::has_controller(room.coords()))
        .filter(|room| atlas.has_terrain(*room) && !world.closed.contains(room))
        .filter_map(move |room| {
            let beside = || {
                let sides = [(1, 0), (-1, 0), (0, 1), (0, -1)];
                let near = (sides.into_iter())
                    .filter_map(|(dx, dy)| tree.crossings(room.offset(dx, dy)?, home))
                    .min()?;
                Some(near + 1).filter(|hops| *hops <= super::NEAR)
            };
            let hops =
                (tree.crossings(room, home)).or_else(|| guarded(&room).then(beside).flatten())?;
            Some((room, hops))
        })
}

pub fn threat_radius(
    (world, atlas): (&World, &Atlas),
    (intel, declarations): (&Intel, &Declarations),
    owned: &BTreeSet<RoomName>,
) -> BTreeMap<RoomName, i32> {
    (owned.iter())
        .flat_map(|home| near((world, atlas), (intel, declarations), *home))
        .filter(|(room, _)| !owned.contains(room))
        .fold(BTreeMap::new(), |mut radius, (room, hops)| {
            let at = radius.entry(room).or_insert(hops);
            *at = (*at).min(hops);
            radius
        })
}

fn sees(observer: RoomName, room: RoomName) -> bool {
    tiles::rooms_apart(observer, room) <= engine::OBSERVER_RANGE
}

/// Every declared or threat-radius room no free observer reaches is scouted (§7.1).
pub fn sight(
    world: &World,
    atlas: &Atlas,
    declarations: &Declarations,
    intel: &Intel,
    (preyed, radius): (&[RoomName], &BTreeMap<RoomName, i32>),
) -> (Vec<Action>, Vec<Scout>) {
    let mut free: Vec<_> = world
        .rooms
        .iter()
        .flat_map(|r| world.rules.working(r))
        .filter(|s| s.kind == StructureType::Observer && s.owner.is_ours())
        .collect();
    let mut stale: Vec<(i32, &RoomName)> = intel
        .iter()
        .filter(|(_, r)| world.time - r.seen > STALE)
        .map(|(room, r)| (r.seen, room))
        .collect();
    stale.sort();

    let observe = |free: &mut Vec<&Structure>, room: &RoomName| -> Option<Action> {
        if !atlas.has_terrain(*room) {
            return None;
        }
        let (at, _) = free
            .iter()
            .enumerate()
            .filter(|(_, o)| sees(o.pos.room, *room))
            .min_by_key(|(_, o)| (tiles::rooms_apart(o.pos.room, *room), o.id))?;
        Some(Action::Observe(free.remove(at).id, *room))
    };

    // A sweep's choice needs a look while no fight body hunts a candidate,
    // one a tick none was seen in, so one observer starves nothing.
    let looked = (preyed.iter()).any(|r| intel.get(r).is_some_and(|row| row.seen >= world.time));
    let hunting = (world.creeps.iter())
        .any(|c| declarations::fights(&c.name) && preyed.contains(&c.pos.room));
    let eyes: Vec<RoomName> = free.iter().map(|o| o.pos.room).collect();
    let sweep = preyed.first().filter(|_| !hunting && !looked);
    let mut observed: Vec<Action> = sweep.and_then(|r| observe(&mut free, r)).into_iter().collect();
    let covered = |observed: &[Action], room: &RoomName| {
        (observed.iter()).any(|i| matches!(i, Action::Observe(_, r) if r == room))
    };
    // A declared room no row neighbours never joins the frontier, and no scout is cast to it.
    let unseen = declared(declarations).filter(|r| !intel.contains_key(*r));
    for room in unseen.chain(stale.into_iter().map(|(_, r)| r)) {
        if !covered(&observed, room)
            && let Some(intent) = observe(&mut free, room)
        {
            observed.push(intent);
        }
    }
    // Never seen, beside a room seen: an unseen world has no row to go stale.
    let mut frontier: Vec<RoomName> = intel
        .keys()
        .flat_map(|room| [(1, 0), (-1, 0), (0, 1), (0, -1)].map(|(dx, dy)| room.offset(dx, dy)))
        .flatten()
        .filter(|room| !intel.contains_key(room))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();
    let n = frontier.len().max(1);
    frontier.rotate_left(world.time as usize % n);
    observed.extend(frontier.iter().filter_map(|r| observe(&mut free, r)));

    let seen = |room: &RoomName| intel.get(room).map(|r| r.seen);
    // Rings nearest first: a nearer rival raids sooner, and age alone strays.
    let ring =
        (radius.iter()).filter(|(room, _)| !intel.contains_key(*room)).map(|(_, hops)| *hops).min();
    let fresh = |row: &Row| row.guarded(&world.me, declarations) && world.time - row.seen <= STALE;
    let near = (radius.iter())
        .filter(|(room, hops)| intel.contains_key(*room) || Some(**hops) == ring)
        .filter(|(room, _)| !intel.get(*room).is_some_and(fresh))
        .map(|(room, _)| room);
    let mut rooms: Vec<&RoomName> = declared(declarations)
        .chain(near)
        .filter(|room| !covered(&observed, room))
        .filter(|room| !(atlas.has_terrain(**room) && eyes.iter().any(|e| sees(*e, **room))))
        .collect();
    rooms.sort_by_cached_key(|room| (seen(room), *room));
    rooms.dedup();
    let scouts = rooms
        .into_iter()
        .map(|room| Scout {
            room: *room,
            age: world.time - seen(room).unwrap_or(0),
            cast: seen(room).is_none_or(|at| world.time - at > STALE),
        })
        .collect();
    (observed, scouts)
}
