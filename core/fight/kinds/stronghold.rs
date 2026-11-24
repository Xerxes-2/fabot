/// `Stronghold` (`ARCHITECTURE.md` §6.2): an Invader core shot dead from out
/// of its garrison's reach, then the ramparts over its loot; `step` closes it
/// with None once its room holds neither.
use std::collections::{BTreeMap, BTreeSet};

use super::super::casting;
use super::super::price;
use super::super::scene::{Asked, Scene, fresh, middle};
use super::super::shape::{Engagement, Objective, Phase, Plan, enter};
use fabot_atlas::Atlas;
use fabot_base::boost::Stock;
use fabot_base::declarations::{Declarations, OpKind, REACH_HOPS};
use fabot_base::engine;
use fabot_base::intents::CreepAct;
use fabot_base::tiles;
use fabot_base::vocabulary::{Part, ResourceType};
use fabot_base::wish::Wish;
use fabot_base::world::{Creep, Id, Owner, Pos, Room, RoomName, Structure, StructureType};
use fabot_exchange::stronghold::{self as bunker, Bunker};
use fabot_exchange::{Outcome, Side, Terrain};
use fabot_tactics::{self as tactics, Sight, Tactic};

/// Its bodies stand beside the one the towers strike, eight at most.
const COPIES: i32 = 8;

pub(crate) fn step(
    scene: &Scene,
    declarations: &Declarations,
    standing: &BTreeSet<String>,
    e: &Engagement,
) -> Option<Engagement> {
    let time = scene.world.time;
    let mut next = e.clone();
    next.ours.retain(|n, _| standing.contains(n));
    next.lost = e.lost + (e.ours.len() - next.ours.len()) as i32;
    let living = |n: &String| {
        (scene.ours.get(n.as_str()))
            .is_some_and(|c| c.ticks_to_live > engine::AGED_OUT_TICKS_TO_LIVE)
    };
    next.ours.retain(|n, _| living(n));
    let asked = scene.spawning.iter().any(|c| c.engagement == e.id);
    let declared = declarations.priority(OpKind::Stronghold, e.room).is_some();
    let held = scene
        .in_sight(e.room)
        .is_none_or(|r| r.invader_core().is_some() || !loot(r).is_empty());
    let known = (scene.intel.get(&e.room)).is_some_and(|r| r.stronghold.is_some());
    let arrived = scene.members_of(&next).iter().any(|c| c.pos.room == e.room);
    let gone = next.ours.is_empty() && !asked && !(next.unanswered && known);
    if !declared || !held || gone {
        None
    } else if e.phase == Phase::Forming && !asked && !next.ours.is_empty() {
        Some(enter(Phase::Approaching, time, &next))
    } else if e.phase == Phase::Approaching && arrived {
        Some(enter(Phase::Fighting, time, &next))
    } else {
        Some(next)
    }
}

fn terrain() -> Terrain {
    Terrain {
        weight: engine::PLAIN_WEIGHT,
        run_off: engine::ROOM_SIDE / 2,
    }
}

/// One stronghold at a time, on the row Strategy ranks first.
pub(crate) fn opens(
    scene: &Scene,
    declarations: &Declarations,
    ranked: &[(Engagement, bool)],
    first: i32,
) -> Option<(Engagement, bool)> {
    let held = |room| ranked.iter().any(|(e, _)| e.room == room);
    let standing = |room| (scene.intel.get(&room)).is_some_and(|r| r.stronghold.is_some());
    if ranked
        .iter()
        .any(|(e, _)| e.objective == Objective::Stronghold)
    {
        return None;
    }
    let row = (declarations.operations.iter())
        .find(|o| o.kind == OpKind::Stronghold && !held(o.room) && standing(o.room))?;
    let id = ranked.iter().fold(first, |most, (e, _)| most.max(e.id + 1));
    let e = fresh(
        id,
        Objective::Stronghold,
        row.room,
        scene.world.time,
        Tactic::Kite,
    );
    let priority = row.priority;
    Some((Engagement { priority, ..e }, false))
}

/// Priced off the level intel read, unseen: its towers and ramparts are the
/// engine's; each tick while unanswered, as the boosts asks ahead of it drew
/// this tick may be back the next.
pub(crate) fn priced(
    scene: &Scene,
    atlas: &Atlas,
    asked: &mut Asked,
    e: &Engagement,
) -> Engagement {
    if !e.unanswered {
        return e.clone();
    }
    let colonies: Vec<(RoomName, i32)> = (scene.colonies_of(atlas, e).iter())
        .map(|r| (r.name, r.energy_capacity))
        .collect();
    let walk = |c| scene.walk_to(atlas, e, c);
    let Some((_, stock, capacity, walk)) = priced_from(&colonies, walk, &asked.boosts) else {
        return e.clone();
    };
    let Some(level) = (scene.intel.get(&e.room)).and_then(|r| Some(r.stronghold?.level)) else {
        return e.clone();
    };
    let Some(Taken {
        set,
        boosted,
        won,
        cost,
        worth,
    }) = take(level, (&stock, capacity, Some(walk)))
    else {
        return Engagement {
            capacity,
            ..e.clone()
        };
    };
    let offers = vec![price::offer(Tactic::Kite, &set, worth, cost)];
    let asking = casting::Asking {
        bodies: &set,
        boosted,
    };
    let boosts = scene.ask(atlas, e, asking, asked);
    Engagement {
        plan: Plan {
            boosts,
            tactic: Tactic::Kite,
            order: Vec::new(),
            offers,
        },
        budget: e.lost,
        unanswered: false,
        capacity,
        predicted: Some(won),
        ..e.clone()
    }
}

/// What a stronghold is priced against, Strategy's row and Fight's open
/// alike: the colony that casts and boosts its set (`Scene::nearest`'s
/// rule: nearest by `walk`, within reach), its bank's capacity, its stock net
/// of what other casts reserve (`stocks`), and that walk.
pub fn priced_from(
    colonies: &[(RoomName, i32)],
    walk: impl Fn(RoomName) -> Option<i32>,
    stocks: &casting::Boosts,
) -> Option<(RoomName, Stock, i32, i32)> {
    let (walk, colony, capacity) = (colonies.iter())
        .filter_map(|(c, capacity)| Some((walk(*c)?, *c, *capacity)))
        .filter(|(walk, ..)| *walk <= engine::ROOM_SIDE * REACH_HOPS)
        .min()?;
    let stock = stocks.get(&colony).cloned().unwrap_or_default();
    Some((colony, stock, capacity, walk))
}

pub struct Taken {
    pub set: Vec<casting::Body>,
    pub boosted: bool,
    pub won: Outcome,
    pub cost: i32,
    pub worth: i32,
}

/// The cheapest set a colony holding `stock` and banking `capacity` casts
/// `walk` tiles off that takes a stronghold of `level` for less than its loot.
pub fn take(level: i32, (stock, capacity, walk): (&Stock, i32, Option<i32>)) -> Option<Taken> {
    let bunker = Bunker::of_level(level);
    let ours = Side {
        cast_walk: walk,
        together: true,
        ..Side::default()
    };
    let raider = &[Part::RangedAttack, Part::Move, Part::Heal, Part::Move];
    let bodies: Vec<casting::Body> = (bunker::kit(bunker.strike, stock, capacity).into_iter())
        .chain(Some(casting::most(capacity, raider)).filter(|b| !b.is_empty()))
        .collect();
    let worth = bunker::loot(level);
    let (set, boosted, won, cost) =
        bunker::cheapest_kill(&ours, (&bodies, COPIES), stock, &bunker, terrain())?;
    (cost < worth).then_some(Taken {
        set,
        boosted,
        won,
        cost,
        worth,
    })
}

fn invader(s: &Structure, kind: StructureType) -> bool {
    s.kind == kind && s.owner == Owner::Invader
}

fn rampart_at<'a>(room: &'a Room, at: &Pos) -> Option<&'a Structure> {
    (room.structures.iter()).find(|s| invader(s, StructureType::Rampart) && s.pos == *at)
}

fn loot(room: &Room) -> Vec<Pos> {
    let held = |store: &fabot_base::world::Store| {
        (store.held.iter()).any(|(r, n)| *r != ResourceType::Energy && *n > 0)
    };
    let containers = (room.structures.iter())
        .filter(|s| s.kind == StructureType::Container && held(&s.store))
        .map(|s| s.pos);
    (room.remains.iter())
        .filter(|r| held(&r.store))
        .map(|r| r.pos)
        .chain(containers)
        .collect()
}

/// A stronghold's ramparts stand to its collapse, its core gone or not.
pub fn fallen_with_loot(room: &Room) -> bool {
    let rampart = |s: &Structure| invader(s, StructureType::Rampart);
    room.structures.iter().any(rampart) && room.invader_core().is_none() && !loot(room).is_empty()
}

/// The core's own rampart, then one over loot or over a walkable side of
/// loot walled in on every side: the fewest hits, then the most it frees.
fn mark(room: &Room, atlas: &Atlas) -> Option<(Id, Pos)> {
    let of = |s: &Structure| (s.id, s.pos);
    if let Some(c) = room.invader_core() {
        return Some(of(rampart_at(room, &c.pos).unwrap_or(c)));
    }
    // A fallen core's garrison strikes nothing and stands where it was: off
    // a rampart it is shot, as it walls a loot's side.
    let bare = (room.hostiles())
        .filter(|h| h.owner == Owner::Invader && rampart_at(room, &h.pos).is_none());
    if let Some(h) = bare.min_by_key(|h| (h.hits, h.id)) {
        return Some((h.id, h.pos));
    }
    let walkable = |p: &Pos| {
        (room.structures.iter())
            .all(|s| s.pos != *p || s.walkable || s.kind == StructureType::Rampart)
    };
    let open = |p: &Pos| {
        walkable(p) && atlas.passable(room.name, tiles::of_pos(p)) && rampart_at(room, p).is_none()
    };
    let mut walls: BTreeMap<(Id, Pos), (i32, i32)> = BTreeMap::new();
    let mut wall = |s: &Structure| walls.entry(of(s)).or_insert((s.hits, 0)).1 -= 1;
    for at in loot(room) {
        rampart_at(room, &at).into_iter().for_each(&mut wall);
        let around: Vec<Pos> = (tiles::around(tiles::of_pos(&at)).iter())
            .map(|t| tiles::to_pos(room.name, *t))
            .collect();
        if !around.iter().any(open) {
            let sides = around.iter().filter(|p| walkable(p));
            sides
                .filter_map(|p| rampart_at(room, p))
                .for_each(&mut wall);
        }
    }
    Some(walls.into_iter().min_by_key(|(at, rank)| (*rank, *at))?.0)
}

/// Out of the garrison's reach first: it never leaves its ramparts.
fn post(room: &Room, atlas: &Atlas, mark: &Pos, (c, mates): (&Creep, &[Pos])) -> Option<Pos> {
    let ramparts: Vec<Pos> = (room.structures.iter())
        .filter(|s| invader(s, StructureType::Rampart))
        .map(|s| s.pos)
        .collect();
    let reach = (room.hostiles())
        .filter(|h| h.owner == Owner::Invader && ramparts.contains(&h.pos))
        .map(|h| fabot_exchange::fighter::best_range(&casting::fighter_of(h)))
        .max();
    let towers = super::raid::charged(room);
    tiles::within(engine::RANGED_RANGE, tiles::of_pos(mark))
        .into_iter()
        .filter(|t| !tiles::on_edge(*t) && atlas.passable(room.name, *t))
        .map(|t| tiles::to_pos(room.name, t))
        .filter(|p| !ramparts.contains(p) && !mates.contains(p))
        .min_by_key(|p| {
            let reached = reach.is_some_and(|r| ramparts.iter().any(|w| w.within(r, p)));
            let near = c.pos.range(p).unwrap_or(engine::ROOM_SIDE);
            let strike = fabot_exchange::tower_strike(&towers, p);
            (reached, strike, near, p.x, p.y)
        })
}

/// Healed before it lands, as a heal behind it lets the next strike through
/// the TOUGH it broke; a drained tower counts, refilled and fired in a tick.
pub(crate) fn incoming<'a>(
    scene: &'a Scene,
    decided: &'a [Engagement],
) -> impl Iterator<Item = (String, i32)> + 'a {
    (decided.iter())
        .filter(|e| e.objective == Objective::Stronghold)
        .filter_map(|e| Some((scene.in_sight(e.room)?, e)))
        .filter(|(room, _)| room.invader_core().is_some())
        .flat_map(|(room, e)| {
            let towers: Vec<Pos> = scene.towers_in(room).iter().map(|t| t.pos).collect();
            let here = scene
                .members_of(e)
                .into_iter()
                .filter(|c| c.pos.room == room.name);
            here.map(move |c| {
                (
                    c.name.clone(),
                    fabot_exchange::tower_strike(&towers, &c.pos),
                )
            })
        })
}

pub(crate) fn act(
    sight: &mut Sight,
    scene: &Scene,
    e: &Engagement,
    c: &Creep,
) -> Option<(Option<CreepAct>, Wish)> {
    let working = matches!(e.phase, Phase::Approaching | Phase::Fighting);
    if e.objective != Objective::Stronghold || !working {
        return None;
    }
    let mates: Vec<Pos> = (scene.members_of(e).iter())
        .filter(|m| m.name != c.name)
        .map(|m| m.pos)
        .collect();
    Some(walked(sight, scene.in_sight(e.room), e, (c, &mates)))
}

fn walked(
    sight: &mut Sight,
    room: Option<&Room>,
    e: &Engagement,
    (c, mates): (&Creep, &[Pos]),
) -> (Option<CreepAct>, Wish) {
    let Some(room) = room else {
        return (
            None,
            tactics::toward(sight, c, &middle(e.room), engine::ROOM_SIDE / 2 - 2),
        );
    };
    let Some((mark, at)) = mark(room, sight.atlas) else {
        return (None, tactics::hold(sight, c));
    };
    let shot = (c.pos.within(engine::RANGED_RANGE, &at) && c.live(Part::RangedAttack) > 0)
        .then_some(CreepAct::RangedAttackCreep(mark));
    let wish = match post(room, sight.atlas, &at, (c, mates)) {
        Some(p) if p == c.pos => tactics::hold(sight, c),
        Some(p) => tactics::toward(sight, c, &p, 0),
        None => tactics::toward(sight, c, &at, engine::RANGED_RANGE),
    };
    (shot, wish)
}

#[cfg(test)]
mod tests {
    use super::*;
    use fabot_base::boost::Held;

    /// A colony nearer on the map but further on foot neither casts nor
    /// lends its boosts: the set is priced from the one that boosts it.
    #[test]
    fn a_stronghold_is_priced_from_the_colony_nearest_on_foot() {
        let (grid_near, walk_near) = (RoomName::W1N1, RoomName::new(-3, -3));
        let held = Held {
            compound: fabot_base::vocabulary::ResourceType::CatalyzedGhodiumAlkalide,
            parts: 30,
            in_labs: 0,
        };
        let stocks: casting::Boosts = [(
            grid_near,
            Stock {
                held: vec![held],
                energy: 100,
            },
        )]
        .into();
        let walk = |c| Some(if c == grid_near { 3 } else { 2 } * engine::ROOM_SIDE);
        let colonies = [(grid_near, 5600), (walk_near, 5600)];
        let (colony, stock, _, walked) = priced_from(&colonies, walk, &stocks).unwrap();
        assert_eq!((colony, walked), (walk_near, 2 * engine::ROOM_SIDE));
        assert_eq!(stock, Stock::default());
        let even = |_| Some(2 * engine::ROOM_SIDE);
        assert_eq!(
            priced_from(&colonies, even, &stocks).unwrap().0,
            grid_near.min(walk_near)
        );
        let far = |_| Some((REACH_HOPS + 1) * engine::ROOM_SIDE);
        assert!(priced_from(&colonies, far, &stocks).is_none());
    }
}
