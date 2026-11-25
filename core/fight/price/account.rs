//! What a fight did, set against its prediction at the close (§10.1); never decided on.
use std::collections::{BTreeMap, BTreeSet};

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
    /// Rival bodies unseen with no word of their death, by last sighting, for a creep's lifetime.
    #[serde(skip_serializing_if = "BTreeMap::is_empty")]
    pub unseen: BTreeMap<Id, i32>,
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

/// An engagement as it closed, its dead counted to the close; `left`: not
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
    let (dead, gone): (Vec<&Id>, Vec<&Id>) = (sighted.flat_map(|g| &g.members))
        .filter(|m| !scene.hostile_at.contains_key(m))
        .partition(|m| fallen.contains(m));
    let razed = matches!(e.objective, Objective::Siege(target) if fallen.contains(&target));
    if razed || !dead.is_empty() {
        e.actual.seen = Some(world.time - 1);
    }
    let tombstoned = (e.actual.unseen.iter()).filter(|(m, _)| fallen.contains(*m));
    e.actual.seen = e.actual.seen.max(tombstoned.map(|(_, last)| *last).max());
    let unseen = &mut e.actual.unseen;
    unseen.retain(|m, last| {
        let back = scene.hostile_at.contains_key(m) || fallen.contains(m);
        !back && world.time - *last < engine::CREEP_LIFETIME
    });
    unseen.extend(gone.into_iter().map(|m| (*m, world.time - 1)));

    let ours = scene.members_of(e);
    if e.actual.contact.is_none() {
        let group = scene.by_id.get(&e.group);
        let theirs: Vec<&Creep> = (scene.foes.values().flatten())
            .filter(|h| match e.objective {
                Objective::Defend => group.is_some_and(|g| g.members.contains(&h.id)),
                _ => h.pos.room == e.room,
            })
            .collect();
        let shot = |h: &Creep| (ours.iter()).any(|o| o.pos.within(engine::RANGED_RANGE, &h.pos));
        let reached = |h: &&Creep| h.hits < h.hits_max || shot(h);
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

/// The engagements `decided` drops, with their closing tick's events and dead.
pub fn closed(facts: Facts, open: &[Engagement], decided: &[Engagement]) -> Vec<Closed> {
    let (scene, raids, _) = facts;
    (open.iter())
        .filter(|e| !decided.iter().any(|d| d.id == e.id))
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
                left: stands || !actual.unseen.is_empty() || actual.seen.is_none(),
                safe: controller.is_some_and(|c| c.safe_mode > 0),
                engagement,
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scene::fresh;
    use fabot_base::world::{Pos, RoomName, Rules, Sightings, World};
    use fabot_exchange::Outcome;
    use fabot_tactics::Tactic;

    fn on_tick<T>(time: i32, hostile_at: BTreeMap<Id, Pos>, f: impl FnOnce(&Scene) -> T) -> T {
        let world = World {
            time,
            me: String::new(),
            cpu_limit: 20.0,
            bucket: 10_000,
            rooms: Vec::new(),
            spawns: Vec::new(),
            creeps: Vec::new(),
            our_sites: 0,
            rooms_allowed: 1,
            closed: BTreeSet::new(),
            attacks: Vec::new(),
            destroyed: Vec::new(),
            rules: Rules::default(),
        };
        let (sightings, avoid) = (Sightings::new(), BTreeSet::new());
        f(&Scene {
            world: &world,
            sightings: &sightings,
            intel: &Default::default(),
            remembered: BTreeMap::new(),
            avoid: &avoid,
            rules: &[],
            walks: BTreeMap::new(),
            homes: BTreeSet::new(),
            defended: BTreeSet::new(),
            musters: Vec::new(),
            worked: BTreeSet::new(),
            foes: BTreeMap::new(),
            cores: BTreeSet::new(),
            by_id: BTreeMap::new(),
            hostile_at,
            ours: BTreeMap::new(),
            armed: BTreeSet::new(),
            landed: Vec::new(),
            spawning: Vec::new(),
            boosts: BTreeMap::new(),
            boosting: &[],
        })
    }

    fn defend(predicted: Option<Outcome>) -> Engagement {
        Engagement {
            group: 1,
            predicted,
            ..fresh(1, Objective::Defend, RoomName::W1N1, 0, Tactic::Close)
        }
    }

    /// A Defend on one raid body over (time, in sight, its death seen) ticks,
    /// and whether it would close left on the last.
    fn play(ticks: &[(i32, bool, bool)]) -> (Engagement, bool) {
        let (x, at) = (
            Id::parse("a1").unwrap(),
            fabot_base::tiles::middle(RoomName::W1N1),
        );
        let (mut e, mut left, mut was_in_sight) = (defend(Some(Outcome::Lose)), true, false);
        for &(time, in_sight, fell) in ticks {
            let hostile_at = in_sight.then_some((x, at)).into_iter().collect();
            on_tick(time, hostile_at, |scene| {
                let raids = [Group {
                    centre: at,
                    id: 1,
                    last_seen: time - 1,
                    members: BTreeSet::from([x]),
                }];
                let raids = &raids[..usize::from(was_in_sight)];
                let fallen = fell.then_some(x).into_iter().collect();
                let facts = (scene, raids, &fallen);
                let was = [e.clone()];
                left = closed(facts, &was, &[])[0].left;
                account(facts, &was, std::slice::from_mut(&mut e));
            });
            was_in_sight = in_sight;
        }
        (e, left)
    }

    #[test]
    fn a_raid_body_back_in_sight_before_its_seen_death_closes_won() {
        let ticks = [
            (1, true, false),
            (2, false, false),
            (3, true, false),
            (4, false, true),
        ];
        assert!(!play(&ticks).1);
    }

    #[test]
    fn a_raid_body_dead_out_of_sight_closes_won_on_its_tombstone() {
        let ticks = [
            (1, true, false),
            (2, false, false),
            (3, false, false),
            (4, false, true),
        ];
        let (e, left) = play(&ticks[..3]);
        assert!(left && !e.actual.unseen.is_empty());
        assert!(!play(&ticks).1);
    }

    #[test]
    fn a_raid_body_unseen_a_creeps_lifetime_is_no_longer_unseen() {
        let unseen = |last| play(&[(1, true, false), (2, false, false), (last, false, false)]).0;
        assert!(!unseen(engine::CREEP_LIFETIME).actual.unseen.is_empty());
        assert!(unseen(1 + engine::CREEP_LIFETIME).actual.unseen.is_empty());
    }

    #[test]
    fn an_unpriced_close_counts_its_closing_ticks_dead() {
        let e = Engagement {
            ours: BTreeMap::from([("a".to_string(), 0)]),
            ..defend(None)
        };
        let lost = on_tick(1, BTreeMap::new(), |scene| {
            let closes = closed((scene, &[], &BTreeSet::new()), &[e], &[]);
            closes.iter().map(|c| c.engagement.lost).collect::<Vec<_>>()
        });
        assert_eq!(lost, [1]);
    }
}
