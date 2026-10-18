use std::collections::BTreeMap;

use crate::core::engine;
use crate::core::intents::Intent;
use crate::core::tick::Declarations;
use crate::core::tiles;
use crate::core::vocabulary::BodyPart;
use crate::core::world::{Owner, World};

/// A room as it stood when last seen, by username: a stance is the
/// declarations' and changes without a sight.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Row {
    pub seen: i32,
    pub owner: Option<String>,
    pub level: i32,
    /// The reserver and the tick its hold lapses, so it ends unseen.
    pub reservation: Option<(String, i32)>,
    /// Whose body seen there carries WORK or CARRY: another's economy.
    pub worker: Option<String>,
}

pub type Intel = BTreeMap<String, Row>;

/// The owner's: a rival's hold turns over on a reservation's lapse or a
/// controller's downgrade, slower than a creep life, which one scout's renews.
pub const STALE: i32 = engine::CREEP_LIFETIME;

/// `Owner::of_user`'s inverse: a row outlives the stance it was seen under.
fn user(me: &str, owner: &Owner) -> Option<String> {
    match owner {
        Owner::Ours => Some(me.to_string()),
        Owner::Ally(user) | Owner::Enemy(user) | Owner::Stranger(user) => Some(user.clone()),
        Owner::Invader => Some(engine::INVADER_USER.to_string()),
        Owner::Keeper => Some(engine::KEEPER_USER.to_string()),
        Owner::Nobody => None,
    }
}

pub fn step(world: &World, intel: &mut Intel) {
    for room in &world.rooms {
        let c = room.controller.as_ref();
        let row = Row {
            seen: world.time,
            owner: c.and_then(|c| user(&world.me, &c.owner)),
            level: c.map_or(0, |c| c.level),
            reservation: c
                .and_then(|c| c.reservation.as_ref())
                .and_then(|(owner, left)| Some((user(&world.me, owner)?, world.time + left))),
            // An ally's body is never hunted, so it never hides an enemy's.
            worker: room
                .hostiles()
                .find(|h| {
                    h.body
                        .iter()
                        .any(|p| matches!(p.kind, BodyPart::Work | BodyPart::Carry))
                })
                .and_then(|h| user(&world.me, &h.owner)),
        };
        match intel.get_mut(&room.name) {
            Some(held) => *held = row,
            None => {
                intel.insert(room.name.clone(), row);
            }
        }
    }
}

fn declared(declarations: &Declarations) -> impl Iterator<Item = &String> {
    declarations
        .claims
        .iter()
        .chain(&declarations.outposts)
        .map(|o| &o.room)
        .chain(&declarations.sieges)
}

/// A declared room a scout serves, stalest first; `cast` while its row is
/// past `STALE`: a living scout patrols, a new one is cast only for a stale row.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scout {
    pub room: String,
    pub cast: bool,
}

/// Sieges first, then the stalest rows, each to its nearest free observer;
/// every declared room no observer covers is scouted (`ARCHITECTURE.md` §7.1).
pub fn sight(
    world: &World,
    declarations: &Declarations,
    intel: &Intel,
) -> (Vec<Intent>, Vec<Scout>) {
    let mut free: Vec<_> = world
        .rooms
        .iter()
        .flat_map(|r| r.structures.iter())
        .filter(|s| s.kind.is_observer() && s.owner.is_ours())
        .collect();
    let mut stale: Vec<(i32, &String)> = intel
        .iter()
        .filter(|(_, r)| world.time - r.seen > STALE)
        .map(|(room, r)| (r.seen, room))
        .collect();
    stale.sort();

    let mut observe = |room: &String| -> Option<Intent> {
        let (at, observer) = free
            .iter()
            .enumerate()
            .filter(|(_, o)| tiles::rooms_apart(&o.pos.room, room) <= engine::OBSERVER_RANGE)
            .min_by_key(|(_, o)| (tiles::rooms_apart(&o.pos.room, room), o.id.clone()))?;
        let id = observer.id.clone();
        free.remove(at);
        Some(Intent::Observe(id, room.clone()))
    };

    let mut observed: Vec<Intent> = declarations
        .sieges
        .iter()
        .filter_map(&mut observe)
        .collect();
    let covered = |observed: &[Intent], room: &String| {
        observed
            .iter()
            .any(|i| matches!(i, Intent::Observe(_, r) if r == room))
    };
    for (_, room) in stale {
        if !covered(&observed, room)
            && let Some(intent) = observe(room)
        {
            observed.push(intent);
        }
    }

    let seen = |room: &String| intel.get(room).map(|r| r.seen);
    let mut rooms: Vec<&String> = declared(declarations)
        .filter(|room| !covered(&observed, room))
        .collect();
    rooms.sort_by_key(|room| (seen(room), *room));
    rooms.dedup();
    let scouts = rooms
        .into_iter()
        .map(|room| Scout {
            room: room.clone(),
            cast: seen(room).is_some_and(|at| world.time - at > STALE),
        })
        .collect();
    (observed, scouts)
}

const NONE: &str = "-";

/// One line a room, `room seen owner level reserver lapse worker`, `-` for
/// none: a segment holds thousands.
pub fn to_text(intel: &Intel) -> String {
    let name = |user: &Option<String>| user.clone().unwrap_or_else(|| NONE.to_string());
    let mut out = String::new();
    for (room, r) in intel {
        let (reserver, lapse) = match &r.reservation {
            Some((user, lapse)) => (user.as_str(), lapse.to_string()),
            None => (NONE, NONE.to_string()),
        };
        out.push_str(&format!(
            "{room} {} {} {} {reserver} {lapse} {}\n",
            r.seen,
            name(&r.owner),
            r.level,
            name(&r.worker)
        ));
    }
    out
}

pub fn of_text(text: &str) -> Intel {
    let name = |s: &str| (s != NONE).then(|| s.to_string());
    text.lines()
        .filter_map(|line| {
            let f: Vec<&str> = line.split(' ').collect();
            if f.len() != 7 {
                return None;
            }
            let row = Row {
                seen: f[1].parse().ok()?,
                owner: name(f[2]),
                level: f[3].parse().ok()?,
                reservation: name(f[4]).zip(f[5].parse().ok()),
                worker: name(f[6]),
            };
            Some((f[0].to_string(), row))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::tests::Dice;

    #[test]
    fn a_row_reads_back_as_written() {
        let mut dice = Dice::new(7);
        let user = |dice: &mut Dice| {
            dice.chance(2)
                .then(|| dice.pick(&["a", "b_c", "d-e", "Invader"]).to_string())
        };
        for _ in 0..300 {
            let row = Row {
                seen: dice.between(0, 9_999_999),
                owner: user(&mut dice),
                level: dice.between(0, 8),
                reservation: user(&mut dice).map(|u| (u, dice.between(0, 9_999_999))),
                worker: user(&mut dice),
            };
            let intel: Intel = [("W1N1".to_string(), row)].into();
            assert_eq!(of_text(&to_text(&intel)), intel);
        }
    }
}
