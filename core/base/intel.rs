use std::borrow::Cow;
use std::collections::BTreeMap;

use crate::declarations::{Declarations, Declared, Outpost};
use crate::engine;
use crate::tiles::{self, Post, Tile};
use crate::vocabulary::{Resource, resource_name, resource_of_name};
use crate::world::{Controller, Id, Owner, Pos, Room, RoomName, Source, World, push_int};

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
    /// None on a row written before minerals were read, or a room without one.
    pub mineral: Option<Resource>,
    /// An Invader stronghold stands: a core past level 0.
    pub stronghold: bool,
}

pub type Intel = BTreeMap<RoomName, Row>;

/// The owner's: a rival's hold turns over on a reservation's lapse or a
/// controller's downgrade, slower than a creep life, which one scout's renews.
pub const STALE: i32 = engine::CREEP_LIFETIME;

/// A room out of sight stands as it was last seen, or as nobody's.
pub fn blind(
    outpost: &Outpost,
    intel: &Intel,
    world: &World,
    declarations: &Declarations,
) -> Room<'static> {
    let time = world.time;
    let row = intel.get(&outpost.room);
    let owner = |user: &String| Owner::of_user(user.clone(), &world.me, declarations);
    Room {
        name: outpost.room,
        terrain: None,
        controller: Some(Controller {
            id: outpost.controller.id,
            pos: outpost.controller.pos,
            owner: row
                .and_then(|r| r.owner.as_ref())
                .map_or(Owner::Nobody, owner),
            level: row.map_or(0, |r| r.level),
            progress: 0,
            progress_total: 0,
            ticks_to_downgrade: 0,
            reservation: row
                .and_then(|r| r.reservation.as_ref())
                .filter(|(_, lapse)| *lapse > time)
                .map(|(user, lapse)| (owner(user), lapse - time)),
            safe_mode: 0,
            safe_mode_available: 0,
            safe_mode_cooldown: 0,
            upgrade_blocked: 0,
        }),
        energy_available: 0,
        energy_capacity: 0,
        sources: outpost
            .sources
            .iter()
            .map(|s| Source {
                id: s.id,
                pos: s.pos,
                energy: 0,
                energy_capacity: 0,
                ticks_to_regeneration: 0,
            })
            .collect(),
        minerals: Vec::new(),
        structures: Vec::new(),
        sites: Vec::new(),
        piles: Vec::new(),
        remains: Vec::new(),
        others: Vec::new(),
    }
}

/// A room a scout serves; `cast` while its row is past `STALE` or missing: a
/// living scout patrols, a new one is cast only for a stale row.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Scout {
    pub room: RoomName,
    pub age: i32,
    pub cast: bool,
}

const NONE: &str = "-";

/// One line a room, `room seen owner level reserver lapse worker lairs
/// controller sources contained mineral stronghold`, `-` for none, `lairs` each lair's tile and its
/// resource's as `lair.resource,…` and `sources` each as `id@tile,…` (`.`
/// alone for none): a segment holds thousands.
pub fn to_text(intel: &Intel) -> String {
    use std::fmt::Write;
    fn name(user: &Option<String>) -> &str {
        user.as_deref().unwrap_or(NONE)
    }
    fn id_at(out: &mut String, sep: &str, id: Id, at: &Pos) {
        out.extend([sep, &id.text(), "@"]);
        push_int(out, at.index());
    }
    let mut out = String::new();
    for (room, r) in intel {
        let _ = write!(out, "{room} ");
        push_int(&mut out, r.seen);
        out.extend([" ", name(&r.owner), " "]);
        push_int(&mut out, r.level);
        match &r.reservation {
            Some((user, lapse)) => {
                out.extend([" ", user, " "]);
                push_int(&mut out, *lapse);
            }
            None => out.extend([" ", NONE, " ", NONE]),
        }
        out.extend([" ", name(&r.worker), " "]);
        match r.lairs.as_deref() {
            None => out.push_str(NONE),
            Some([]) => out.push('.'),
            Some(posts) => {
                for (i, (lair, resource)) in posts.iter().enumerate() {
                    out.push_str(if i == 0 { "" } else { "," });
                    push_int(&mut out, lair.index() as i32);
                    out.push('.');
                    push_int(&mut out, resource.index() as i32);
                }
            }
        }
        match &r.controller {
            Some(c) => id_at(&mut out, " ", c.id, &c.pos),
            None => out.extend([" ", NONE]),
        }
        match r.sources.as_deref() {
            None => out.push_str(" -"),
            Some([]) => out.push_str(" ."),
            Some(sources) => {
                for (i, s) in sources.iter().enumerate() {
                    id_at(&mut out, if i == 0 { " " } else { "," }, s.id, &s.pos);
                }
            }
        }
        out.push_str(if r.contained { " 1" } else { " 0" });
        out.extend([
            " ",
            r.mineral
                .as_ref()
                .map_or(Cow::Borrowed(NONE), resource_name)
                .as_ref(),
        ]);
        out.push_str(if r.stronghold { " 1" } else { " 0" });
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
        id: Id::parse(id)?,
        pos: Pos::new(room, at % 50, at / 50),
    })
}

pub fn of_text(text: &str) -> Intel {
    let name = |s: &str| (s != NONE).then(|| s.to_string());
    text.lines()
        .filter_map(|line| {
            let f: Vec<&str> = line.split(' ').collect();
            if !(7..=13).contains(&f.len()) {
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
                mineral: f
                    .get(11)
                    .filter(|m| **m != NONE)
                    .map(|m| resource_of_name(m)),
                stronghold: f.get(12) == Some(&"1"),
            };
            Some((room, row))
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::Dice;

    #[test]
    fn push_int_writes_as_display() {
        for n in [i32::MIN, -10, -1, 0, 9, 10, 4096, i32::MAX] {
            let mut out = String::new();
            push_int(&mut out, n);
            assert_eq!(out, n.to_string());
        }
    }

    #[test]
    fn a_row_reads_back_as_written() {
        let mut dice = Dice::new(7);
        let user = |dice: &mut Dice| {
            dice.chance(2)
                .then(|| dice.pick(&["a", "b_c", "d-e", "Invader"]).to_string())
        };
        let site = |dice: &mut Dice| Declared {
            id: format!("{:x}", dice.between(0, 9_999_999)).as_str().into(),
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
                mineral: dice
                    .chance(2)
                    .then(|| resource_of_name(dice.pick(&["H", "O", "U", "L", "K", "Z", "X"]))),
                stronghold: dice.chance(2),
            };
            let intel: Intel = [(RoomName::named("W1N1"), row)].into();
            assert_eq!(of_text(&to_text(&intel)), intel);
        }
    }
}
