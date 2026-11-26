/// `Stronghold` (`ARCHITECTURE.md` §6.2): an Invader core struck dead from out
/// of its garrison's reach by a recipe's set, then the garrison off its ramparts.
use std::cmp::Reverse;
use std::collections::{BTreeMap, BTreeSet};

use Part::{Attack, Heal, RangedAttack, Tough};
use ResourceType::{GhodiumAlkalide, KeaniumAlkalide, LemergiumAlkalide, LemergiumOxide};
use fabot_atlas::Atlas;
use fabot_base::{
    boost::{Act::Fight, Stock, strength, takes},
    declarations::{Declarations, OpKind, REACH_HOPS},
    engine,
    intents::CreepAct,
    stronghold::{self as bunker, Stronghold, raw},
    tiles::{self, TileSet},
    vocabulary::{Part, ResourceType, multiple, part_count_in},
    wish::Wish,
    world::{Creep, Id, Pos, Room, RoomName, Rules, Structure},
};
use fabot_tactics::{self as tactics, Sight, Tactic};

use super::{
    super::{
        casting, price,
        scene::{Asked, Scene, fresh},
        shape::{Engagement, Objective, Phase, Plan, enter},
    },
    square::{self, BLOCK},
};

/// Each body's copies and its parts, `HEAD` order: the struck body loses
/// its TOUGH, then its MOVE, before a weapon or a HEAL.
type Bodies = &'static [(usize, [usize; 5])];
const HEAD: [Part; 5] = [Tough, Part::Move, Attack, RangedAttack, Heal];

/// `sized`: its first body's ATTACK and MOVE as `take` prices it cheapest.
pub struct Recipe {
    bodies: Bodies,
    boosts: &'static [(Part, ResourceType)],
    sized: bool,
}

/// By level, the recipes a colony casts the first of: S11's leaders' kits run
/// on krebs's towers. L1: a bare ATTACK beside three 17H17M (612 HEAL holds the
/// tower's 600), striking 30 a part to RANGED's 10. L2/L3: four on T1/T2 boosts,
/// the two within a shot of the core holding the guns, as the garrison's reach
/// leaves the ring three off it, on GHO2 TOUGH, as HEAL alone held by 4%.
const RECIPES: [&[Recipe]; 4] = [
    &[],
    &[Recipe { bodies: &[(1, [0, 0, 0, 0, 0]), (3, [0, 17, 0, 0, 17])], boosts: &[], sized: true }],
    &[Recipe {
        bodies: SPLIT,
        boosts: &[
            (Tough, GhodiumAlkalide),
            (RangedAttack, KeaniumAlkalide),
            (Heal, LemergiumOxide),
        ],
        sized: false,
    }],
    &[Recipe {
        bodies: SPLIT,
        boosts: &[
            (Tough, GhodiumAlkalide),
            (RangedAttack, KeaniumAlkalide),
            (Heal, LemergiumAlkalide),
        ],
        sized: false,
    }],
];
const SPLIT: Bodies = &[(2, [6, 25, 0, 12, 7]), (2, [0, 18, 0, 0, 18])];

impl Recipe {
    fn set(&self, attack: usize) -> Vec<casting::Body> {
        let body = |parts: &[usize; 5]| {
            let b = (HEAD.iter().zip(parts)).flat_map(|(p, n)| std::iter::repeat_n(*p, *n));
            engine::with_move_last(b.collect())
        };
        let sized = |i: usize, parts: &[usize; 5]| match i == 0 && self.sized {
            true => [parts[0], attack, attack, parts[3], parts[4]],
            false => *parts,
        };
        (self.bodies.iter().enumerate())
            .flat_map(|(i, (n, parts))| std::iter::repeat_n(body(&sized(i, parts)), *n))
            .collect()
    }

    fn sizes(&self) -> std::ops::RangeInclusive<usize> {
        if self.sized { 1..=engine::MAX_BODY_PARTS / 2 } else { 0..=0 }
    }

    fn boosted<'a>(
        &'a self,
        set: &'a [casting::Body],
    ) -> impl Iterator<Item = (Part, ResourceType, i32)> + 'a {
        (self.boosts.iter()).map(move |(part, tier)| {
            let parts = set.iter().map(|b| part_count_in(b, *part)).sum();
            (*part, *tier, parts)
        })
    }

    fn casts(&self, set: &[casting::Body], stock: &Stock, capacity: i32) -> bool {
        let held = |part: Part, tier: ResourceType| -> i32 {
            (stock.held.iter())
                .filter(|h| takes(Fight, part, h.compound))
                .filter(|h| strength(Fight, part, h.compound) >= strength(Fight, part, tier))
                .map(|h| h.parts)
                .sum()
        };
        let parts: i32 = self.boosted(set).map(|(.., n)| n).sum();
        set.iter().all(|b| engine::body_cost(b) <= capacity)
            && (self.boosted(set)).all(|(part, tier, n)| held(part, tier) >= n)
            && stock.energy >= parts
    }

    fn kill(&self, set: &[casting::Body], level: usize) -> Option<i32> {
        let boost = |part| {
            (self.boosted(set).find(|(p, ..)| *p == part))
                .map_or(1, |(p, tier, _)| multiple(p, Some(tier)))
        };
        let reach = reach(set.iter().map(|b| part_count_in(b, Attack) > 0));
        let strike = |b: &casting::Body| {
            let (a, r) = (part_count_in(b, Attack), part_count_in(b, RangedAttack));
            engine::strike_at(a * boost(Attack), r * boost(RangedAttack), reach)
        };
        // A block may hold only its near two in reach of the core.
        let mut strikes: Vec<i32> = set.iter().map(strike).collect();
        strikes.sort_unstable_by_key(|s| Reverse(*s));
        let shot: i32 = strikes.iter().take(BLOCK / 2).sum();
        let hits = bunker::STRONGHOLD_RAMPART_HITS[level] + bunker::INVADER_CORE_HITS;
        (shot > 0).then(|| (hits + shot - 1) / shot)
    }

    /// Each body at the share of its life `used` takes, as a body that wins
    /// lives on (owner), and each boost whole at its raw worth.
    fn cost(&self, set: &[casting::Body], used: i32) -> i32 {
        let bodies: i32 = set.iter().map(|b| engine::body_cost(b)).sum();
        let boosts: f64 = (self.boosted(set))
            .map(|(_, tier, n)| {
                f64::from(n * engine::BOOST_MINERAL_PER_PART) * raw(tier)
                    + f64::from(n * engine::BOOST_ENERGY_PER_PART)
            })
            .sum();
        bodies * used / engine::CREEP_LIFETIME + boosts as i32
    }
}

fn reach(mut swings: impl Iterator<Item = bool>) -> i32 {
    if swings.any(|s| s) { engine::MELEE_RANGE } else { engine::RANGED_RANGE }
}

pub(crate) fn step(
    scene: &Scene,
    declarations: &Declarations,
    standing: &BTreeSet<String>,
    aging_out: &BTreeSet<String>,
    e: &Engagement,
) -> Option<Engagement> {
    let time = scene.world.time;
    let next = super::pruned(e, standing, aging_out);
    let asked = scene.spawning.iter().any(|c| c.engagement == e.id);
    let declared = declarations.priority(OpKind::Stronghold, e.room).is_some();
    let held = (scene.in_sight(e.room))
        .is_none_or(|r| r.invader_core().is_some() || bare(r).next().is_some());
    let known = (scene.intel.get(&e.room)).is_some_and(|r| r.stronghold.is_some());
    let members = scene.members_of(&next);
    // The set goes in once each body stands in the room: a body in alone takes the towers alone.
    let gathered = !members.is_empty() && members.iter().all(|c| c.pos.room == e.room);
    let gone = next.ours.is_empty() && !asked && !(next.unanswered && known);
    if !declared || !held || gone {
        None
    } else if e.phase == Phase::Forming && !asked && !next.ours.is_empty() {
        Some(enter(Phase::Approaching, time, &next))
    } else if e.phase == Phase::Approaching && gathered {
        Some(enter(Phase::Fighting, time, &next))
    } else {
        Some(next)
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
    if ranked.iter().any(|(e, _)| e.objective == Objective::Stronghold) {
        return None;
    }
    let row = (declarations.operations.iter())
        .find(|o| o.kind == OpKind::Stronghold && !held(o.room) && standing(o.room))?;
    let id = ranked.iter().fold(first, |most, (e, _)| most.max(e.id + 1));
    let e = fresh(id, Objective::Stronghold, row.room, scene.world.time, Tactic::Kite);
    let priority = row.priority;
    Some((Engagement { priority, ..e }, false))
}

/// Priced off the level and loot intel read, unseen; each tick while
/// unanswered, as the boosts asks ahead of it drew this tick may be back the next.
pub(crate) fn priced(
    scene: &Scene,
    atlas: &Atlas,
    asked: &mut Asked,
    e: &Engagement,
) -> Engagement {
    if !e.unanswered {
        return e.clone();
    }
    let colonies: Vec<(RoomName, i32)> =
        (scene.colonies_of(atlas, e).iter()).map(|r| (r.name, r.energy_capacity)).collect();
    let walk = |c| scene.walk_to(atlas, e, c);
    let Some((_, stock, capacity, walk)) = priced_from(&colonies, walk, &asked.boosts) else {
        return e.clone();
    };
    let Some(standing) = (scene.intel.get(&e.room)).and_then(|r| r.stronghold) else {
        return e.clone();
    };
    let left = standing.collapses_at - scene.world.time;
    let Some(Taken { set, boosted, cost, worth }) = take(&standing, left, (&stock, capacity, walk))
    else {
        return Engagement { capacity, ..e.clone() };
    };
    let offers = vec![price::offer(Tactic::Kite, &set, worth, cost)];
    let asking = casting::Asking { bodies: &set, boosted };
    let boosts = scene.ask(atlas, e, asking, asked);
    Engagement {
        plan: Plan { boosts, tactic: Tactic::Kite, order: Vec::new(), offers },
        budget: e.lost,
        unanswered: false,
        capacity,
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
    pub cost: i32,
    pub worth: i32,
}

/// The first recipe for `standing`'s level the colony casts, if it kills within its
/// bodies' life, a life before the collapse, for no more than the loot (§6.2).
pub fn take(
    standing: &Stronghold,
    left: i32,
    (stock, capacity, walk): (&Stock, i32, i32),
) -> Option<Taken> {
    let level = usize::try_from(standing.level).ok()?;
    let recipes = RECIPES.get(level)?;
    let worth = standing.loot + bunker::STRONGHOLD_CORE_DENSITY[level];
    // A body waits at home on the set's last spawn: one spawn, at worst.
    let finishes = |set: &[casting::Body], used: i32| {
        let spawned: i32 = set.iter().map(|b| engine::spawn_ticks(b.len())).sum();
        used + spawned <= engine::CREEP_LIFETIME && used + engine::CREEP_LIFETIME <= left
    };
    let priced = |r: &'static Recipe| {
        (r.sizes().map(|n| r.set(n)))
            .filter(|set| r.casts(set, stock, capacity))
            .filter_map(|set| {
                let used = walk + r.kill(&set, level)?;
                let cost = r.cost(&set, used);
                finishes(&set, used).then_some((r, set, cost))
            })
            .min_by_key(|(.., cost)| *cost)
    };
    let (recipe, set, cost) = recipes.iter().find_map(priced)?;
    (standing.loot > 0 && cost <= worth).then_some(Taken {
        boosted: !recipe.boosts.is_empty(),
        set,
        cost,
        worth,
    })
}

/// A fallen core's garrison strikes nothing and stands where it was: off a
/// rampart it is shot, as it walls a loot's side.
fn bare<'a>(room: &'a Room) -> impl Iterator<Item = &'a Creep> {
    (room.hostiles()).filter(|h| {
        fabot_atlas::garrison(room, h) && fabot_atlas::invader_rampart_at(room, &h.pos).is_none()
    })
}

fn mark(room: &Room) -> Option<(Id, Pos)> {
    let of = |s: &Structure| (s.id, s.pos);
    if let Some(c) = room.invader_core() {
        return Some(of(fabot_atlas::invader_rampart_at(room, &c.pos).unwrap_or(c)));
    }
    let h = bare(room).min_by_key(|h| (h.hits, h.id))?;
    Some((h.id, h.pos))
}

/// The block a set stands in: a square its square walks to from the border,
/// the most within `reach` of the mark, the least tower strike, the nearest.
struct Ground {
    mark: (Id, Pos),
    walls: TileSet,
    block: [Pos; BLOCK],
}

fn ground(rules: &Rules, room: &Room, atlas: &Atlas, reach: i32, set: &[&Creep]) -> Option<Ground> {
    let (id, at) = mark(room)?;
    let closed = square::closed(room);
    let walls = square::walls(room, atlas, &closed);
    let entered = square::from_border(atlas, &walls, room.name)?;
    let towers = rules.charged_towers(room);
    let corners = (-reach - 1..=reach).flat_map(|dx| (-reach - 1..=reach).map(move |dy| (dx, dy)));
    let block = corners
        .filter_map(|(dx, dy)| square::at(room, atlas, &closed, (at.x + dx, at.y + dy)))
        .filter(|b| entered.ticks(tiles::of_pos(&b[0])).is_some())
        .filter(|b| b.iter().filter(|p| p.within(reach, &at)).count() >= BLOCK / 2)
        .min_by_key(|b| {
            let striking = b.iter().filter(|p| p.within(reach, &at)).count();
            let strike: i32 = b.iter().map(|p| fabot_exchange::tower_strike(&towers, p)).sum();
            (Reverse(striking), strike, square::far(set, &b[0]), b[0].x, b[0].y)
        })?;
    Some(Ground { mark: (id, at), walls, block })
}

/// Whether `level`'s first recipe has a block its square walks to.
pub fn reached(rules: &Rules, room: &Room, atlas: &Atlas, level: i32) -> bool {
    let recipe = usize::try_from(level).ok().and_then(|l| RECIPES.get(l)?.first());
    let swings = recipe.map(|r| r.set(1).into_iter().map(|b| part_count_in(&b, Attack) > 0));
    swings.is_some_and(|s| ground(rules, room, atlas, reach(s), &[]).is_some())
}

/// Healed before it lands, on the body nearest the core the towers strike, the
/// one struck last among ties; a drained tower counts, refilled and fired in a tick.
pub(crate) fn incoming<'a>(
    scene: &'a Scene,
    decided: &'a [Engagement],
) -> impl Iterator<Item = (String, i32)> + 'a {
    (decided.iter()).filter(|e| e.objective == Objective::Stronghold).filter_map(|e| {
        let room = scene.in_sight(e.room)?;
        let core = room.invader_core()?;
        let towers = scene.towers_in(room);
        let fired: BTreeSet<&Id> = towers.iter().map(|t| &t.id).collect();
        let last = (scene.world.attacks.iter()).find(|(by, _)| fired.contains(by));
        let here = scene.members_of(e).into_iter().filter(|c| c.pos.room == room.name);
        let struck = here.min_by_key(|c| {
            let range = c.pos.range(&core.pos).unwrap_or(engine::ROOM_SIDE);
            (range, last.is_none_or(|(_, at)| *at != c.id), c.name.clone())
        })?;
        let at: Vec<Pos> = towers.iter().map(|t| t.pos).collect();
        let strike = fabot_exchange::tower_strike(&at, &struck.pos);
        Some((struck.name.clone(), strike))
    })
}

/// The set's wishes, once a set: out of a square it forms one round its body
/// nearest the mark, and a square steps as one.
pub(crate) fn walks(sight: &mut Sight, scene: &Scene, e: &Engagement) -> Vec<Wish> {
    let set = scene.members_of(e);
    let fighting = e.objective == Objective::Stronghold && e.phase == Phase::Fighting;
    let Some(room) = scene.in_sight(e.room).filter(|_| fighting && !set.is_empty()) else {
        return Vec::new();
    };
    let reach = reach(set.iter().map(|c| c.live(Attack) > 0));
    let Some(g) = ground(sight.rules, room, sight.atlas, reach, &set) else {
        return Vec::new();
    };
    let at = g.mark.1;
    let stand = |sight: &mut Sight, c: &Creep, p: Option<Pos>| match p {
        Some(p) if p != c.pos => tactics::toward(sight, c, &p, 0),
        _ => tactics::hold(sight, c),
    };
    let next = |from: &Pos| square::step(sight.atlas, &g.walls, &set, (from, &g.block[0]));
    let rested = set.iter().all(|m| m.fatigue == 0);
    let around = |m: &&Creep| m.pos.within(2, &g.block[0]);
    let lead = (set.iter().copied()).min_by_key(|m| (m.pos.range(&at), &m.name));
    let formed = || {
        lead.and_then(|l| {
            let corners = [(0, 0), (-1, 0), (0, -1), (-1, -1)];
            (corners.iter())
                .filter_map(|(dx, dy)| {
                    square::at(room, sight.atlas, &|_| false, (l.pos.x + dx, l.pos.y + dy))
                })
                .filter(|f| !g.walls.contains(tiles::of_pos(&f[0])) && next(&f[0]).is_some())
                .min_by_key(|f| {
                    (f.iter().map(|p| square::far(&set, p)).sum::<i32>(), f[0].x, f[0].y)
                })
        })
    };
    let seat = |tiles: &[Pos; BLOCK], c: &Creep| square::seat(tiles, &at, &set, c);
    match (set.iter().all(around), square::corner(&set)) {
        (true, _) => set.iter().map(|c| stand(sight, c, seat(&g.block, c))).collect(),
        (_, Some(corner)) if rested => match next(&corner) {
            Some(d) => (set.iter())
                .map(|c| match tiles::neighbour(tiles::of_pos(&c.pos), d) {
                    Some(t) => tactics::step_to(c, t),
                    None => tactics::hold(sight, c),
                })
                .collect(),
            None => set.iter().map(|c| stand(sight, c, seat(&g.block, c))).collect(),
        },
        (_, Some(_)) => set.iter().map(|c| tactics::hold(sight, c)).collect(),
        (_, None) => {
            let tiles = formed().unwrap_or(g.block);
            set.iter().map(|c| stand(sight, c, seat(&tiles, c))).collect()
        }
    }
}

pub(crate) fn act(
    sight: &mut Sight,
    scene: &Scene,
    (e, middles, walking): (&Engagement, &BTreeMap<RoomName, Pos>, &BTreeMap<String, Wish>),
    c: &Creep,
) -> Option<(Option<CreepAct>, Wish)> {
    let working = matches!(e.phase, Phase::Approaching | Phase::Fighting);
    if e.objective != Objective::Stronghold || !working {
        return None;
    }
    let room = scene.in_sight(e.room).filter(|_| e.phase == Phase::Fighting);
    let strike = room.and_then(mark).and_then(|(id, at)| {
        let swings = c.live(Attack) > 0 && c.pos.within(engine::MELEE_RANGE, &at);
        let shoots = c.live(RangedAttack) > 0 && c.pos.within(engine::RANGED_RANGE, &at);
        (swings.then_some(CreepAct::AttackCreep(id)))
            .or(shoots.then_some(CreepAct::RangedAttackCreep(id)))
    });
    let wish = match (walking.get(&c.name), room) {
        (Some(wish), _) => wish.clone(),
        (None, Some(_)) => tactics::hold(sight, c),
        (None, None) => tactics::toward(sight, c, &middles[&e.room], engine::ROOM_SIDE / 2 - 2),
    };
    Some((strike, wish))
}

#[cfg(test)]
mod tests {
    use fabot_base::boost::Held;

    use super::*;

    fn stock(held: &[(ResourceType, i32)]) -> Stock {
        let held = (held.iter())
            .map(|(compound, parts)| Held { compound: *compound, parts: *parts, in_labs: 0 })
            .collect();
        Stock { held, energy: 1000 }
    }

    /// A stronger compound stands in for its tier; a weaker one, or too
    /// little, casts nothing.
    #[test]
    fn a_recipe_casts_on_its_tier_or_stronger() {
        use ResourceType::*;
        let l3 = &RECIPES[3][0];
        let set = l3.set(0);
        let casts = |held: &[(ResourceType, i32)]| l3.casts(&set, &stock(held), 5600);
        let tier = [(GhodiumAlkalide, 12), (KeaniumAlkalide, 24), (LemergiumAlkalide, 50)];
        assert!(casts(&tier));
        let t3 = [
            (CatalyzedGhodiumAlkalide, 12),
            (CatalyzedKeaniumAlkalide, 24),
            (CatalyzedLemergiumAlkalide, 50),
        ];
        assert!(casts(&t3));
        assert!(!casts(&[tier[0], tier[1], (LemergiumOxide, 50)]));
        assert!(!casts(&[(GhodiumOxide, 12), tier[1], tier[2]]));
        assert!(!casts(&[tier[0], (KeaniumAlkalide, 23), tier[2]]));
        assert!(!l3.casts(&set, &stock(&tier), 4600));
    }

    /// A stronghold is taken only by a set its colony casts, for no more than
    /// its loot: unstocked, L2's and L3's boosted recipes cast nothing, and
    /// L1's bare set outprices a few hundred loot.
    #[test]
    fn an_unstocked_colony_takes_a_stronghold_only_where_its_bare_set_pays() {
        use ResourceType::*;
        let taken = |level, loot, held: &[(ResourceType, i32)]| {
            let standing = Stronghold { level, deploys_at: 0, collapses_at: 50_000, loot };
            take(&standing, 50_000, (&stock(held), 5_600, 100)).is_some()
        };
        let tier = [(GhodiumAlkalide, 50), (KeaniumAlkalide, 50), (LemergiumAlkalide, 50)];
        for level in 1..=3 {
            assert!(!taken(level, 700, &[]), "L{level}");
        }
        assert!(!taken(2, 1_000_000, &[]) && !taken(3, 1_000_000, &[]));
        assert!(taken(1, 1_000_000, &[]) && taken(3, 1_000_000, &tier));
    }

    /// A colony nearer on the map but further on foot neither casts nor
    /// lends its boosts: the set is priced from the one that boosts it.
    #[test]
    fn a_stronghold_is_priced_from_the_colony_nearest_on_foot() {
        let (grid_near, walk_near) = (RoomName::W1N1, RoomName::new(-3, -3));
        let held = Held { compound: ResourceType::CatalyzedGhodiumAlkalide, parts: 30, in_labs: 0 };
        let stocks: casting::Boosts = [(grid_near, Stock { held: vec![held], energy: 100 })].into();
        let walk = |c| Some(if c == grid_near { 3 } else { 2 } * engine::ROOM_SIDE);
        let colonies = [(grid_near, 5600), (walk_near, 5600)];
        let (colony, stock, _, walked) = priced_from(&colonies, walk, &stocks).unwrap();
        assert_eq!((colony, walked), (walk_near, 2 * engine::ROOM_SIDE));
        assert_eq!(stock, Stock::default());
        let even = |_| Some(2 * engine::ROOM_SIDE);
        assert_eq!(priced_from(&colonies, even, &stocks).unwrap().0, grid_near.min(walk_near));
        let far = |_| Some((REACH_HOPS + 1) * engine::ROOM_SIDE);
        assert!(priced_from(&colonies, far, &stocks).is_none());
    }
}
