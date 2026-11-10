use std::collections::{BTreeMap, BTreeSet};

use crate::core::atlas::tiles::Tile;
use crate::core::atlas::{Atlas, Post};
use crate::core::engine;
use crate::core::intents::Intent;
use crate::core::tick::{self, Declarations, Declared};
use crate::core::tiles;
use crate::core::vocabulary::BodyPart;
use crate::core::world::{Owner, Pos, RoomName, World};

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
    pub controller: Option<Declared>,
    /// None on a row written before sources were read: unknown, not none.
    pub sources: Option<Vec<Declared>>,
    pub contained: bool,
}

pub type Intel = BTreeMap<RoomName, Row>;

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
            controller: c.map(|c| Declared {
                id: c.id.clone(),
                pos: c.pos,
            }),
            sources: Some(
                (room.sources.iter())
                    .map(|s| Declared {
                        id: s.id.clone(),
                        pos: s.pos,
                    })
                    .collect(),
            ),
            contained: (room.sources.iter()).all(|s| {
                (room.structures.iter()).any(|c| c.kind.is_container() && c.pos.within(1, &s.pos))
            }),
        };
        match intel.get_mut(&room.name) {
            Some(held) => *held = row,
            None => {
                intel.insert(room.name, row);
            }
        }
    }
}

fn declared(declarations: &Declarations) -> impl Iterator<Item = &RoomName> {
    declarations
        .claims
        .iter()
        .chain(&declarations.outposts)
        .map(|o| &o.room)
        .chain(&declarations.sieges)
}

/// A room a scout serves; `cast` while its row is past `STALE` or missing: a
/// living scout patrols, a new one is cast only for a stale row.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scout {
    pub room: RoomName,
    pub age: i32,
    pub cast: bool,
}

pub fn preyed(intel: &Intel, declarations: &Declarations, time: i32) -> Vec<RoomName> {
    let enemy = |user: &Option<String>| {
        user.as_ref()
            .is_some_and(|u| declarations.enemies.contains(u))
    };
    let mut rooms: Vec<(i32, &RoomName)> = intel
        .iter()
        .filter(|(room, _)| !declarations.avoid.contains(*room))
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

/// Rooms a rival raising §5's reserve could hold, scouted from a colony a tower
/// guards; by route too, as one walled off by terrain raids no sooner than a far one.
pub fn threat_radius(
    world: &World,
    atlas: &Atlas,
    owned: &BTreeSet<RoomName>,
) -> BTreeMap<RoomName, i32> {
    let towered = (owned.iter().filter_map(|r| world.room(*r)))
        .filter(|room| {
            let level = room.controller.as_ref().map_or(0, |c| c.level);
            world.rules.allowed("tower", level) > 0
        })
        .map(|room| {
            (
                room.name,
                atlas.tree(&BTreeSet::new(), super::NEAR, room.name),
            )
        });
    towered
        .flat_map(|(home, tree)| {
            let (x, y) = home.coords();
            (-super::NEAR..=super::NEAR)
                .flat_map(move |dx| (-super::NEAR..=super::NEAR).map(move |dy| (x + dx, y + dy)))
                .filter(|c| tiles::has_controller(*c))
                .map(|(x, y)| RoomName::new(x, y))
                .filter(|room| atlas.has_terrain(*room))
                .filter_map(move |room| Some((room, tree.crossings(room, home)?)))
        })
        .filter(|(room, _)| !owned.contains(room) && !world.closed.contains(room))
        .fold(BTreeMap::new(), |mut radius, (room, hops)| {
            let at = radius.entry(room).or_insert(hops);
            *at = (*at).min(hops);
            radius
        })
}

/// Every declared or threat-radius room no observer covers is scouted (§7.1).
pub fn sight(
    world: &World,
    atlas: &Atlas,
    declarations: &Declarations,
    intel: &Intel,
    (preyed, radius): (&[RoomName], &BTreeMap<RoomName, i32>),
) -> (Vec<Intent>, Vec<Scout>) {
    let mut free: Vec<_> = world
        .rooms
        .iter()
        .flat_map(|r| r.structures.iter())
        .filter(|s| s.kind.is_observer() && s.owner.is_ours())
        .collect();
    let mut stale: Vec<(i32, &RoomName)> = intel
        .iter()
        .filter(|(_, r)| world.time - r.seen > STALE)
        .map(|(room, r)| (r.seen, room))
        .collect();
    stale.sort();

    let mut observe = |room: &RoomName| -> Option<Intent> {
        if !atlas.has_terrain(*room) {
            return None;
        }
        let (at, observer) = free
            .iter()
            .enumerate()
            .filter(|(_, o)| tiles::rooms_apart(o.pos.room, *room) <= engine::OBSERVER_RANGE)
            .min_by_key(|(_, o)| (tiles::rooms_apart(o.pos.room, *room), o.id.clone()))?;
        let id = observer.id.clone();
        free.remove(at);
        Some(Intent::Observe(id, *room))
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
    let covered = |observed: &[Intent], room: &RoomName| {
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
    let mut frontier: Vec<RoomName> = intel
        .keys()
        .map(|room| room.coords())
        .flat_map(|(x, y)| [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)])
        .map(|(x, y)| RoomName::new(x, y))
        .filter(|room| !intel.contains_key(room))
        .collect::<std::collections::BTreeSet<_>>()
        .into_iter()
        .collect();
    let n = frontier.len().max(1);
    frontier.rotate_left(world.time as usize % n);
    observed.extend(frontier.iter().filter_map(&mut observe));

    let seen = |room: &RoomName| intel.get(room).map(|r| r.seen);
    // Rings nearest first: a nearer rival raids sooner, and age alone strays.
    let ring = (radius.iter())
        .filter(|(room, _)| !intel.contains_key(*room))
        .map(|(_, hops)| *hops)
        .min();
    let near = (radius.iter())
        .filter(|(room, hops)| intel.contains_key(*room) || Some(**hops) == ring)
        .map(|(room, _)| room);
    let mut rooms: Vec<&RoomName> = declared(declarations)
        .chain(near)
        .filter(|room| !covered(&observed, room))
        .collect();
    rooms.sort_by_key(|room| (seen(room), *room));
    rooms.dedup();
    let scouts = rooms
        .into_iter()
        .map(|room| {
            // Read every tick, as an observer would.
            let siege = declarations.sieges.contains(room);
            Scout {
                room: *room,
                age: world.time - seen(room).filter(|_| !siege).unwrap_or(0),
                cast: siege || seen(room).is_none_or(|at| world.time - at > STALE),
            }
        })
        .collect();
    (observed, scouts)
}

const NONE: &str = "-";

/// One line a room, `room seen owner level reserver lapse worker lairs
/// controller sources contained`, `-` for none, `lairs` each lair's tile and its
/// resource's as `lair.resource,…` and `sources` each as `id@tile,…` (`.`
/// alone for none): a segment holds thousands.
pub fn to_text(intel: &Intel) -> String {
    use std::fmt::Write;
    fn name(user: &Option<String>) -> &str {
        user.as_deref().unwrap_or(NONE)
    }
    let mut out = String::new();
    for (room, r) in intel {
        let _ = write!(out, "{room} {} {} {} ", r.seen, name(&r.owner), r.level);
        let _ = match &r.reservation {
            Some((user, lapse)) => write!(out, "{user} {lapse}"),
            None => write!(out, "{NONE} {NONE}"),
        };
        let _ = write!(out, " {} ", name(&r.worker));
        match r.lairs.as_deref() {
            None => out.push_str(NONE),
            Some([]) => out.push('.'),
            Some(posts) => {
                for (i, (lair, resource)) in posts.iter().enumerate() {
                    let comma = if i == 0 { "" } else { "," };
                    let _ = write!(out, "{comma}{}.{}", lair.index(), resource.index());
                }
            }
        }
        let _ = match &r.controller {
            Some(c) => write!(out, " {}@{}", c.id, c.pos.index()),
            None => write!(out, " {NONE}"),
        };
        match r.sources.as_deref() {
            None => out.push_str(" -"),
            Some([]) => out.push_str(" ."),
            Some(sources) => {
                for (i, s) in sources.iter().enumerate() {
                    let sep = if i == 0 { ' ' } else { ',' };
                    let _ = write!(out, "{sep}{}@{}", s.id, s.pos.index());
                }
            }
        }
        out.push_str(if r.contained { " 1" } else { " 0" });
        out.push('\n');
    }
    out
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

fn site_of(room: RoomName, text: &str) -> Option<Declared> {
    let (id, at) = text.split_once('@')?;
    let at: i32 = at
        .parse()
        .ok()
        .filter(|i| (0..tiles::COUNT as i32).contains(i))?;
    Some(Declared {
        id: id.to_string(),
        pos: Pos::new(room, at % 50, at / 50),
    })
}

pub fn of_text(text: &str) -> Intel {
    let name = |s: &str| (s != NONE).then(|| s.to_string());
    text.lines()
        .filter_map(|line| {
            let f: Vec<&str> = line.split(' ').collect();
            if !(7..=11).contains(&f.len()) {
                return None;
            }
            let room = RoomName::parse(f[0])?;
            let sources = match f.get(9).copied() {
                None | Some(NONE) => None,
                Some(".") => Some(Vec::new()),
                Some(list) => Some(
                    (list.split(','))
                        .map(|s| site_of(room, s))
                        .collect::<Option<_>>()?,
                ),
            };
            let row = Row {
                seen: f[1].parse().ok()?,
                owner: name(f[2]),
                level: f[3].parse().ok()?,
                reservation: name(f[4]).zip(f[5].parse().ok()),
                worker: name(f[6]),
                lairs: f.get(7).and_then(|p| posts_of(p)),
                controller: f.get(8).and_then(|c| site_of(room, c)),
                sources,
                contained: f.get(10) == Some(&"1"),
            };
            Some((room, row))
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
        let site = |dice: &mut Dice| Declared {
            id: format!("{:x}", dice.between(0, 9_999_999)),
            pos: Pos::new(
                RoomName::named("W1N1"),
                dice.between(0, 49),
                dice.between(0, 49),
            ),
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
                controller: dice.chance(2).then(|| site(&mut dice)),
                sources: dice
                    .chance(2)
                    .then(|| (0..dice.below(3)).map(|_| site(&mut dice)).collect()),
                contained: dice.chance(2),
            };
            let intel: Intel = [(RoomName::named("W1N1"), row)].into();
            assert_eq!(of_text(&to_text(&intel)), intel);
        }
    }
}
