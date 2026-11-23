use std::collections::{BTreeMap, BTreeSet};

use crate::shape::{Engagement, Group, Objective, Phase};
use crate::tracker;
use fabot_base::world::{Creep, Id, Owner, RoomName, StructureType, World, hostiles};

pub fn armed_and_opening(
    world: &World,
    owned: &BTreeSet<RoomName>,
    (worked, homes): (&BTreeSet<RoomName>, &BTreeSet<RoomName>),
    hostiles: &[Creep],
    cores: &BTreeSet<Id>,
) -> (BTreeSet<Id>, BTreeSet<Id>) {
    let stronghold: BTreeSet<RoomName> = (world.rooms.iter())
        .filter(|r| {
            (r.structures.iter()).any(|s| s.kind == StructureType::InvaderCore && s.level > 0)
        })
        .map(|r| r.name)
        .collect();
    // An outpost has no towers to answer a body that works it (§7.2).
    let outpost = |r: &RoomName| worked.contains(r) && !homes.contains(r);
    let works = |h: &Creep| outpost(&h.pos.room) && hostiles::works(h);
    let armed: BTreeSet<Id> = (hostiles.iter())
        .filter(|h| hostiles::foe(h))
        .filter(|h| cores.contains(&h.id) || hostiles::armed(owned, hostiles, h) || works(h))
        .map(|h| h.id)
        .collect();
    // A stronghold's garrison stands down unfought; `armed` keeps it for every other reader.
    let garrison = |h: &Creep| h.owner == Owner::Invader && stronghold.contains(&h.pos.room);
    let opens: BTreeSet<Id> = (hostiles.iter())
        .filter(|h| armed.contains(&h.id) && !garrison(h))
        .map(|h| h.id)
        .collect();
    (armed, opens)
}

/// A Defend left open holds its room: a newcomer inside it is its target
/// now, with a fresh budget, where its own group is not there (a home's,
/// so a second raid is not left to towers) or was lost from sight.
pub fn newcomers(
    engagements: &mut [Engagement],
    events: &[tracker::Event],
    by_id: &BTreeMap<i32, Group>,
    homes: &BTreeSet<RoomName>,
    opens: &BTreeSet<Id>,
    time: i32,
) {
    for e in engagements {
        if !matches!(e.objective, Objective::Defend) || e.phase == Phase::Disengaging {
            continue;
        }

        let in_room =
            |id| (by_id.get(&id)).is_some_and(|g| g.room() == e.room && g.last_seen == time);

        let gone = if homes.contains(&e.room) {
            !in_room(e.group)
        } else {
            !by_id.contains_key(&e.group)
        };

        let newcomer = events.iter().find_map(|event| match event {
            tracker::Event::Newcomer(id)
                if gone && in_room(*id) && by_id[id].members.iter().any(|m| opens.contains(m)) =>
            {
                Some(*id)
            }
            _ => None,
        });

        if let Some(id) = newcomer {
            e.group = id;
            e.opened = time;
            e.budget = e.lost;
            e.unanswered = true;
            e.predicted = None;
        }
    }
}
