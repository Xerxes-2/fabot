//! What a fight did, set against its prediction at the close (§10.1); never decided on.
use std::collections::BTreeSet;

use serde::{Deserialize, Serialize};

use crate::scene::Scene;
use crate::shape::{Engagement, Group, Objective};
use fabot_base::engine;
use fabot_base::world::{Creep, Id};

#[derive(Clone, Debug, Default, PartialEq, Eq, Serialize, Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub struct Actual {
    /// One of ours first within 3 of its rival, or its rival first struck.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub contact: Option<i32>,
    /// The last sighting of a rival body whose death was seen.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub seen: Option<i32>,
    /// A rival body went from sight with no word of its death.
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub escaped: bool,
    #[serde(skip_serializing_if = "BTreeSet::is_empty")]
    pub rivals: BTreeSet<String>,
    /// Of the bodies cast for it: a free body it drafts is not charged.
    pub energy: i32,
    /// Our dead in the rooms a sweep moved on from, which its `lost` no longer holds.
    pub left_dead: i32,
    #[serde(skip_serializing_if = "std::ops::Not::not")]
    pub gave_up: bool,
}

impl Actual {
    pub fn is_empty(&self) -> bool {
        *self == Actual::default()
    }
}

/// A priced engagement as it closed, its dead counted to the close; `left`: not
/// every rival death was seen.
#[derive(Clone, Debug, PartialEq)]
pub struct Closed {
    pub engagement: Engagement,
    pub left: bool,
    pub safe: bool,
}

/// Its rival as last tick's groups held it: a Defend's group, else whoever stood in its room.
fn raid<'g>(raids: &'g [Group], e: &Engagement) -> impl Iterator<Item = &'g Group> + use<'g> {
    let (defends, group, room) = (e.objective == Objective::Defend, e.group, e.room);
    (raids.iter()).filter(move |g| g.id == group || (!defends && g.room() == room))
}

/// Reads its rival's bodies gone from sight each tick; writes on events only.
pub fn account(facts: Facts, was: &[Engagement], decided: &mut [Engagement]) {
    for e in decided {
        step(facts, was.iter().find(|w| w.id == e.id), e);
    }
}

pub type Facts<'a> = (&'a Scene<'a>, &'a [Group], &'a BTreeSet<Id>);

fn step((scene, raids, fallen): Facts, prior: Option<&Engagement>, e: &mut Engagement) {
    let world = scene.world;
    let sighted = raid(raids, e).filter(|g| g.last_seen == world.time - 1);
    let gone = (sighted.flat_map(|g| &g.members))
        .filter(|m| !scene.hostile_at.contains_key(m))
        .map(|m| fallen.contains(m))
        .collect::<Vec<_>>();
    let razed = matches!(e.objective, Objective::Siege(target) if fallen.contains(&target));
    if razed || gone.contains(&true) {
        e.actual.seen = Some(world.time - 1);
    }
    e.actual.escaped |= gone.contains(&false);

    let ours = scene.members_of(e);
    if e.actual.contact.is_none() {
        let group = scene.by_id.get(&e.group);
        let theirs: Vec<&Creep> = (scene.foes.values().flatten())
            .filter(|h| match e.objective {
                Objective::Defend => group.is_some_and(|g| g.members.contains(&h.id)),
                _ => h.pos.room == e.room,
            })
            .collect();
        let reached =
            |h: &&Creep| h.hits < h.hits_max || ours.iter().any(|o| o.pos.within(3, &h.pos));
        if theirs.iter().any(reached) {
            e.actual.contact = Some(world.time);
            let named = theirs.iter().filter_map(|h| h.owner.user());
            let room = (e.objective != Objective::Defend).then(|| scene.rivals_of(e.room));
            (e.actual.rivals).extend(named.chain(room.into_iter().flatten()).map(str::to_string));
        }
    }

    let cast = scene.landed.iter().filter(|c| c.engagement == e.id);
    e.actual.energy += cast.map(|c| engine::body_cost(&c.body)).sum::<i32>();
    e.actual.gave_up |= prior.is_some_and(|w| !w.unanswered) && e.unanswered;
}

/// The priced engagements `decided` drops, with their closing tick's events and dead.
pub fn closed(facts: Facts, open: &[Engagement], decided: &[Engagement]) -> Vec<Closed> {
    let (scene, raids, _) = facts;
    (open.iter())
        .filter(|e| e.predicted.is_some() && !decided.iter().any(|d| d.id == e.id))
        .map(|e| {
            let mut engagement = e.clone();
            step(facts, Some(e), &mut engagement);
            let dead = (e.ours.keys()).filter(|n| !scene.ours.contains_key(n.as_str()));
            engagement.lost += dead.count() as i32;
            let stands = (raid(raids, e).flat_map(|g| &g.members))
                .chain(&e.plan.order)
                .any(|m| scene.hostile_at.contains_key(m));
            let controller = scene.in_sight(e.room).and_then(|r| r.controller.as_ref());
            let actual = &engagement.actual;
            Closed {
                left: stands || actual.escaped || actual.seen.is_none(),
                safe: controller.is_some_and(|c| c.safe_mode > 0),
                engagement,
            }
        })
        .collect()
}
