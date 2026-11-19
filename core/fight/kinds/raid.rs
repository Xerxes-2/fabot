/// `Raid` (`ARCHITECTURE.md` §6.2): a ranged blob in a room no barrier of his
/// walls off, on the line his towers fire into the heal it pools there.
use std::collections::{BTreeMap, BTreeSet};

use super::super::shape::{Engagement, Objective};
use super::super::{casting, price};
use super::siege;
use fabot_atlas::Atlas;
use fabot_base::engine;
use fabot_base::intents::CreepAct;
use fabot_base::tiles::{self, Tile};
use fabot_base::vocabulary::{Part, Resource};
use fabot_base::wish::Wish;
use fabot_base::world::{Creep, Id, Kind, Pos, Room, Structure};
use fabot_exchange::{self as exchange, Fighter, Ground, Losses, Outcome};
use fabot_tactics::{self as tactics, Sight, Tactic};

/// A siege whose set shoots its way in: a ranged blob, no dismantler.
pub fn raids(e: &Engagement) -> bool {
    matches!(e.objective, Objective::Siege(_)) && e.plan.tactic == Tactic::BlobKite
}

pub fn charged(room: &Room) -> Vec<Pos> {
    (room.structures.iter())
        .filter(|s| s.kind == Kind::Tower && !s.owner.is_ours())
        .filter(|s| s.store.amount_of(&Resource::Energy) >= engine::TOWER_ENERGY_COST)
        .map(|s| s.pos)
        .collect()
}

/// His bodies that strike back, at what they deal.
pub fn guards(room: &Room) -> Vec<(Pos, i32, i32)> {
    (room.hostiles())
        .map(|h| {
            let f = casting::fighter_of(h);

            (h.pos, f.attack, f.ranged)
        })
        .filter(|(_, swing, shot)| *swing > 0 || *shot > 0)
        .collect()
}

pub fn breached(room: &Room, passable: impl Fn(Tile) -> bool, goal: &Pos) -> bool {
    let walls: BTreeSet<Tile> = room
        .structures
        .iter()
        .filter(|s| matches!(s.kind, Kind::Rampart | Kind::Wall))
        .map(|s| tiles::of_pos(&s.pos))
        .collect();
    let open = |t: Tile| !walls.contains(&t) && passable(t);
    let mut seen = [false; tiles::COUNT];
    let mut edge: Vec<Tile> = Vec::new();

    for tile in (0..tiles::COUNT).map(Tile::of_index) {
        if tiles::on_edge(tile) && open(tile) {
            seen[tile.index()] = true;
            edge.push(tile);
        }
    }

    let aim = tiles::of_pos(goal);

    while let Some(tile) = edge.pop() {
        if tiles::range(tile, aim) <= engine::MELEE_RANGE {
            return true;
        }

        let next: Vec<Tile> = (tiles::around(tile))
            .iter()
            .copied()
            .filter(|n| open(*n) && !seen[n.index()])
            .collect();

        for n in next {
            seen[n.index()] = true;
            edge.push(n);
        }
    }

    false
}

/// His charged tower first: it takes the most strike off the set per hit spent.
fn next<'a>(room: &'a Room, members: &[&Creep]) -> Option<&'a Structure> {
    let towers = charged(room);

    (room.structures.iter())
        .filter(|s| siege::razed(&s.kind) && s.owner.is_theirs())
        .min_by_key(|s| {
            let removes = if towers.contains(&s.pos) {
                engine::tower_attack_at(0)
            } else {
                0
            };

            (
                std::cmp::Reverse(removes * 1000 / s.hits.max(1)),
                near(members, &s.pos),
                s.id,
            )
        })
}

pub fn target(room: &Room, members: &[&Creep]) -> Option<Id> {
    next(room, members).map(|s| s.id).or_else(|| {
        room.controller
            .as_ref()
            .filter(|c| c.owner.is_theirs())
            .map(|c| c.id)
    })
}

/// Where the blob goes and how near: a shot off his structure, and onto his
/// site, whose `move` onto the tile stomps it.
pub fn mark(room: &Room, members: &[&Creep]) -> Option<(Pos, i32)> {
    if let Some(s) = next(room, members) {
        return Some((s.pos, engine::RANGED_RANGE));
    }
    let site = (room.sites.iter())
        .filter(|s| s.owner.is_theirs())
        .min_by_key(|s| (near(members, &s.pos), s.id));

    if let Some(site) = site {
        return Some((site.pos, 0));
    }
    room.controller
        .as_ref()
        .map(|k| (k.pos, engine::RANGED_RANGE))
}

fn near(members: &[&Creep], at: &Pos) -> i32 {
    (members.iter())
        .filter_map(|m| m.pos.range(at))
        .min()
        .unwrap_or(engine::ROOM_SIDE)
}

/// The structure the set is priced on: the one his towers and armed bodies
/// strike hardest a body beside it, since they strike the same set whether it
/// razes that one first or last.
pub fn hardest<'a>(room: &'a Room) -> Option<&'a Structure> {
    let towers = charged(room);
    let guards = guards(room);

    (room.structures.iter())
        .filter(|s| siege::razed(&s.kind) && s.owner.is_theirs())
        .max_by_key(|s| {
            (
                exchange::tower_strike(&towers, &s.pos) + exchange::guard_strike(&guards, &s.pos),
                s.id,
            )
        })
}

/// The set's price: a way to within a shot of `goal` no step of which his
/// towers and armed bodies out-strike the heal the set pools there.
pub fn plan(
    atlas: &Atlas,
    room: &Room,
    goal: &Pos,
    from: &Pos,
    set: &[Fighter],
    standing: &[&Creep],
) -> Option<(Vec<Id>, Outcome)> {
    let weights = atlas.weights(room.name)?;
    let gait = price::gait(set)?;
    let ground = Ground {
        room: room.name,
        weights,
        barriers: BTreeMap::new(),
    };
    let sources = price::entries(atlas, &ground, gait, from, set, standing);

    let ring = |t: Tile| tactics::seats_at(atlas, &[], room.name, t);
    let plan = exchange::breach_plan(
        &ground,
        gait,
        &charged(room),
        &guards(room),
        set,
        ring(tiles::of_pos(goal)),
        ring,
        0,
        &sources,
        &tiles::within(engine::RANGED_RANGE, tiles::of_pos(goal)),
    )?;

    Some((
        Vec::new(),
        Outcome::Win(plan.1, Losses { dead: 0, hits: 0 }),
    ))
}

/// Eight bodies heal one from beside it and eight more within three: the price
/// buys no set that lands more heal on the struck body.
const SETS: usize = 16;

pub fn reinforce<T>(
    capacity: i32,
    fielded: &[casting::Body],
    mut priced: impl FnMut(&[casting::Body]) -> Option<T>,
) -> Option<(Vec<casting::Body>, T)> {
    let raider = casting::most(
        capacity,
        &[Part::RangedAttack, Part::Move, Part::Heal, Part::Move],
    );

    (2..=SETS).find_map(|n| {
        let mut set = fielded.to_vec();
        set.extend(std::iter::repeat_n(
            raider.clone(),
            n.saturating_sub(fielded.len()),
        ));
        priced(&set).map(|plan| (set[fielded.len().min(n)..].to_vec(), plan))
    })
}

/// As near the mark as the line lets it, where his towers and armed bodies do
/// not out-strike the heal the set lands on a body there; a wounded body steps
/// to where they net it least.
pub fn dance(sight: &mut Sight, room: &Room, members: &[&Creep], c: &Creep) -> Wish {
    let target = mark(room, members);
    let towers = charged(room);
    let guards = guards(room);
    let healers: Vec<(&Creep, i32)> = (members.iter())
        .filter(|m| m.name != c.name)
        .map(|m| (*m, m.live(Part::Heal)))
        .collect();
    let heal_at = |at: &Pos| -> i32 {
        (healers.iter())
            .map(|(m, parts)| match m.pos.range(at) {
                Some(r) if r <= engine::MELEE_RANGE => engine::HEAL_POWER * parts,
                Some(r) if r <= engine::RANGED_RANGE => engine::RANGED_HEAL_POWER * parts,
                _ => 0,
            })
            .sum()
    };
    let net = |tile: Tile| -> i32 {
        let at = tiles::to_pos(room.name, tile);
        let strike = exchange::tower_strike(&towers, &at) + exchange::guard_strike(&guards, &at);

        strike - heal_at(&at)
    };
    let hurt = c.hits < c.hits_max;
    let Some((target, stop)) = target else {
        return tactics::hold(sight, c);
    };
    // The walk his walls route it round: the line only holds the last step, and
    // a body no mate heals holds it too, as his towers strike it alone.
    let kept = (healers.iter()).any(|(m, _)| {
        m.pos
            .range(&c.pos)
            .is_some_and(|r| r <= engine::RANGED_RANGE)
    });

    if !hurt && kept {
        return tactics::toward(sight, c, &target, stop);
    }

    let here = tiles::of_pos(&c.pos);
    let near = |tile: Tile| {
        tiles::to_pos(room.name, tile)
            .range(&target)
            .unwrap_or(engine::ROOM_SIDE)
    };
    // Off the ring a body draws no heal and, as his towers' mark, dies there
    // whatever they deal it: it walks back in beside its mates.
    let mate = |tile: Tile| {
        (healers.iter())
            .map(|(m, _)| tiles::range(tile, tiles::of_pos(&m.pos)))
            .min()
            .unwrap_or(engine::ROOM_SIDE)
    };
    let step = std::iter::once(here)
        .chain(
            (tiles::around(here))
                .iter()
                .copied()
                .filter(|t| !tiles::on_edge(*t) && sight.atlas.passable(room.name, *t)),
        )
        .min_by_key(|t| {
            let at = tiles::to_pos(room.name, *t);

            (heal_at(&at) == 0, net(*t), mate(*t), near(*t))
        });

    match step {
        Some(t) if t != here => tactics::toward(sight, c, &tiles::to_pos(room.name, t), 0),
        _ => tactics::hold(sight, c),
    }
}

/// A charged tower in reach first, else the thing dearest a hit; a volley where
/// the bare ones take more.
pub fn volley(sight: &Sight, room: &Room, c: &Creep) -> Option<CreepAct> {
    let spares = (sight.spared.get(&c.pos.room))
        .is_some_and(|s| s.iter().any(|p| c.pos.within(engine::RANGED_RANGE, p)));
    let guns = c.live(Part::RangedAttack);
    if guns == 0 {
        return None;
    }
    let sources: Vec<Pos> = room.sources.iter().map(|s| s.pos).collect();
    let theirs = || room.structures.iter().filter(|s| s.owner.is_theirs());
    let cover = |s: &Structure| theirs().find(|r| r.kind == Kind::Rampart && r.pos == s.pos);
    let reach = |at: &Pos| c.pos.range(at).filter(|r| *r <= engine::RANGED_RANGE);

    // A mark: id, range, hits it takes, what its loss costs, charged, bare.
    let mut marks: Vec<(Id, i32, i32, i32, bool, bool)> = Vec::new();

    for s in theirs().filter(|s| siege::razed(&s.kind)) {
        let Some(r) = reach(&s.pos) else { continue };
        let struck = cover(s).unwrap_or(s);
        let charged = s.kind == Kind::Tower
            && s.store.amount_of(&Resource::Energy) >= engine::TOWER_ENERGY_COST;

        marks.push((
            struck.id,
            r,
            s.hits + cover(s).map_or(0, |w| w.hits),
            siege::taken(s, &sources),
            charged,
            cover(s).is_none(),
        ));
    }

    let dearest = |a: &(Id, i32, i32, i32, bool, bool), b: &(Id, i32, i32, i32, bool, bool)| {
        let dear = |m: &(Id, i32, i32, i32, bool, bool)| f64::from(m.3) / f64::from(m.2.max(1));

        dear(a).total_cmp(&dear(b)).then(b.0.cmp(&a.0))
    };
    let struck =
        (marks.iter().find(|m| m.4)).or_else(|| marks.iter().max_by(|a, b| dearest(a, b)))?;
    let volley: i32 = (marks.iter())
        .filter(|m| m.5)
        .map(|m| m.2.min(guns * engine::mass_attack_power_at(m.1)))
        .sum();

    if !spares && volley > struck.2.min(guns * engine::RANGED_ATTACK_POWER) {
        Some(CreepAct::RangedMassAttack)
    } else {
        Some(CreepAct::RangedAttackCreep(struck.0))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A raid is the fewest raiders whose price walks in, and never fewer than
    /// two however cheap the price is.
    #[test]
    fn a_raid_is_the_fewest_raiders_whose_price_walks_in() {
        let priced = |want: usize| move |set: &[casting::Body]| (set.len() == want).then_some(());

        for want in [2, 7, 16] {
            let (set, ()) = reinforce(12_900, &[], priced(want)).unwrap();

            assert_eq!(set.len(), want);
        }
    }
}
