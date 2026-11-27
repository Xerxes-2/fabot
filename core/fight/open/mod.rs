use std::collections::{BTreeMap, BTreeSet};

use fabot_base::world::{Creep, Id, RoomName, hostiles};

use crate::{
    shape::{Engagement, Group, Objective, Phase, Plan},
    tracker,
};

pub fn armed_and_opening(
    owned: &BTreeSet<RoomName>,
    (worked, homes): (&BTreeSet<RoomName>, &BTreeSet<RoomName>),
    hostiles: &[Creep],
    cores: &BTreeSet<Id>,
) -> (BTreeSet<Id>, BTreeSet<Id>) {
    // An outpost has no towers to answer a body that works it (§7.2).
    let outpost = |r: &RoomName| worked.contains(r) && !homes.contains(r);
    let works = |h: &Creep| outpost(&h.pos.room) && hostiles::works(h);
    let armed: BTreeSet<Id> = (hostiles.iter())
        .filter(|h| cores.contains(&h.id) || hostiles::armed(owned, hostiles.iter(), h) || works(h))
        .map(|h| h.id)
        .collect();
    let opens: BTreeSet<Id> = (hostiles.iter())
        .filter(|h| armed.contains(&h.id) && !fabot_atlas::cast_by_core(h))
        .map(|h| h.id)
        .collect();
    (armed, opens)
}

/// Only a raid with a body its kill order lacks grew: our own losses are none (§6.2).
pub fn grew<'a>(order: &[Id], mut raid: impl Iterator<Item = &'a Id>) -> bool {
    raid.any(|id| !order.contains(id))
}

/// A waiting set wins each growth into its kill order once, or answers it anew (§6.2).
pub fn waited(e: &Engagement, order: Vec<Id>, wins: impl FnOnce() -> bool) -> (Engagement, bool) {
    if !grew(&e.plan.order, order.iter()) {
        (e.clone(), false)
    } else if wins() {
        (Engagement { plan: Plan { order, ..e.plan.clone() }, ..e.clone() }, false)
    } else {
        (anew(e), true)
    }
}

pub fn anew(e: &Engagement) -> Engagement {
    Engagement { budget: e.lost, unanswered: true, predicted: None, ..e.clone() }
}

/// A Defend left open holds its room: a newcomer inside it is its target
/// now, with a fresh budget, where its own group is not there (a home's, so
/// a second raid is not left to towers), was lost from sight or shot disarmed.
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

        let raids = (by_id.get(&e.group))
            .is_some_and(|g| g.last_seen < time || g.members.iter().any(|m| opens.contains(m)));
        let gone = !raids || homes.contains(&e.room) && !in_room(e.group);

        let newcomer = events.iter().find_map(|event| match event {
            tracker::Event::Newcomer(id)
                if gone && in_room(*id) && by_id[id].members.iter().any(|m| opens.contains(m)) =>
            {
                Some(*id)
            }
            _ => None,
        });

        if let Some(id) = newcomer {
            *e = Engagement { group: id, ..anew(e) };
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn only_a_body_the_order_lacks_grows_a_raid() {
        let order: Vec<Id> = ["a", "b"].map(Id::from).to_vec();
        let grown: Vec<Id> = ["a", "c"].map(Id::from).to_vec();
        assert!(!grew(&order, order.iter()), "a set shrunk by its losses, raid unchanged");
        assert!(!grew(&order, order[..1].iter()), "a raid that lost a body");
        assert!(grew(&order, grown.iter()), "a raid with a body its order lacks");
    }

    #[test]
    fn each_growth_past_what_the_set_wins_is_answered_once() {
        let ids = |s: &[&str]| s.iter().map(|&i| Id::from(i)).collect::<Vec<Id>>();
        let mut e = crate::scene::fresh(
            1,
            Objective::Defend,
            RoomName::W1N1,
            0,
            fabot_tactics::Tactic::Close,
        );
        (e.plan.order, e.lost, e.budget, e.unanswered) = (ids(&["a", "b"]), 1, 0, false);
        assert!(!waited(&e, ids(&["a", "b"]), || false).1, "our losses, raid unchanged");
        assert!(!waited(&e, ids(&["a"]), || false).1, "a raid that lost a body");
        let (anew, fresh) = waited(&e, ids(&["a", "b", "c"]), || false);
        assert!(fresh && anew.budget == e.lost && anew.unanswered, "grown and losing");
        let (kept, fresh) = waited(&e, ids(&["a", "b", "c"]), || true);
        assert!(!fresh && kept.plan.order.len() == 3, "grown and still winning");
        assert!(!waited(&kept, ids(&["a", "b", "c"]), || false).1, "a later death, no growth");
    }
}
