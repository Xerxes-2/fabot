use std::collections::BTreeMap;

use crate::core::atlas::tiles::Tile;
use crate::core::atlas::{Atlas, Post};
use crate::core::engine;
use crate::core::intents::Intent;
use crate::core::tick::{self, Declarations};
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
    /// Whose economy the room is; a hauler only passes, so WORK alone marks it.
    pub worker: Option<String>,
    /// None on a row written before posts were read: unknown, not none.
    pub lairs: Option<Vec<Post>>,
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
                .find(|h| h.body.iter().any(|p| p.kind == BodyPart::Work))
                .and_then(|h| user(&world.me, &h.owner)),
            lairs: Some(Atlas::posts(room)),
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

pub fn preyed(intel: &Intel, declarations: &Declarations, time: i32) -> Vec<String> {
    let enemy = |user: &Option<String>| {
        user.as_ref()
            .is_some_and(|u| declarations.enemies.contains(u))
    };
    let mut rooms: Vec<(i32, &String)> = intel
        .iter()
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
    rooms.into_iter().map(|(_, room)| room.clone()).collect()
}

/// Every declared room no observer covers is scouted (`ARCHITECTURE.md` §7.1).
pub fn sight(
    world: &World,
    declarations: &Declarations,
    intel: &Intel,
    preyed: &[String],
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

    // A sweep's choice needs a look while no fight body hunts a candidate,
    // one a tick none was seen in, so one observer starves nothing.
    let looked = (preyed.iter()).any(|r| intel.get(r).is_some_and(|row| row.seen >= world.time));
    let hunting =
        (world.creeps.iter()).any(|c| tick::fights(&c.name) && preyed.contains(&c.pos.room));
    let mut observed: Vec<Intent> = (declarations.sieges.iter())
        .chain(preyed.first().filter(|_| !hunting && !looked))
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
    // Never seen, beside a room seen: an unseen world has no row to go stale.
    let mut frontier: Vec<String> = intel
        .keys()
        .filter_map(|room| tiles::room_coords(room))
        .flat_map(|(x, y)| [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)])
        .map(|(x, y)| tiles::room_name(x, y))
        .filter(|room| !intel.contains_key(room))
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .collect();
    let n = frontier.len().max(1);
    frontier.rotate_left(world.time as usize % n);
    observed.extend(frontier.iter().filter_map(&mut observe));

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

/// One line a room, `room seen owner level reserver lapse worker lairs`, `-`
/// for none, `lairs` each lair's tile and its resource's as `lair.resource,…`
/// (`.` alone for none): a segment holds thousands.
pub fn to_text(intel: &Intel) -> String {
    let name = |user: &Option<String>| user.clone().unwrap_or_else(|| NONE.to_string());
    let mut out = String::new();
    for (room, r) in intel {
        let (reserver, lapse) = match &r.reservation {
            Some((user, lapse)) => (user.as_str(), lapse.to_string()),
            None => (NONE, NONE.to_string()),
        };
        out.push_str(&format!(
            "{room} {} {} {} {reserver} {lapse} {} {}\n",
            r.seen,
            name(&r.owner),
            r.level,
            name(&r.worker),
            r.lairs
                .as_ref()
                .map_or(NONE.to_string(), |posts| posts_text(posts))
        ));
    }
    out
}

fn posts_text(posts: &[Post]) -> String {
    let post = |(lair, resource): &Post| format!("{}.{}", lair.index(), resource.index());
    match posts.iter().map(post).collect::<Vec<_>>().join(",") {
        none if none.is_empty() => ".".to_string(),
        posts => posts,
    }
}

/// A count, written before posts were, is none only at zero.
fn posts_of(text: &str) -> Option<Vec<Post>> {
    let tile = |t: &str| {
        t.parse::<u16>()
            .ok()
            .filter(|i| usize::from(*i) < tiles::COUNT)
    };
    let post = |p: &str| {
        let (lair, resource) = p.split_once('.')?;
        Some((
            Tile::of_index(tile(lair)?.into()),
            Tile::of_index(tile(resource)?.into()),
        ))
    };
    match text {
        "." | "0" => Some(Vec::new()),
        posts if posts.contains('.') => posts.split(',').map(post).collect(),
        _ => None,
    }
}

pub fn of_text(text: &str) -> Intel {
    let name = |s: &str| (s != NONE).then(|| s.to_string());
    text.lines()
        .filter_map(|line| {
            let f: Vec<&str> = line.split(' ').collect();
            if !(7..=8).contains(&f.len()) {
                return None;
            }
            let row = Row {
                seen: f[1].parse().ok()?,
                owner: name(f[2]),
                level: f[3].parse().ok()?,
                reservation: name(f[4]).zip(f[5].parse().ok()),
                worker: name(f[6]),
                lairs: f.get(7).and_then(|p| posts_of(p)),
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
                lairs: dice.chance(2).then(|| {
                    let tile = |dice: &mut Dice| Tile::of_index(dice.below(2500) as usize);
                    (0..dice.below(5))
                        .map(|_| (tile(&mut dice), tile(&mut dice)))
                        .collect()
                }),
            };
            let intel: Intel = [("W1N1".to_string(), row)].into();
            assert_eq!(of_text(&to_text(&intel)), intel);
        }
    }
}
