use std::collections::BTreeSet;

use fabot_atlas::Atlas;
use fabot_base::engine;
use fabot_base::tiles::{self, Tile};
use fabot_base::world::{Creep, Kind, Pos, Room, Rules, Structure, terrain};

use super::plan::{controller_link, haul_level, sized};
use super::shape::{Item, Plan};

/// A level's first batch of extensions, and few enough that the builders'
/// energy goes to the plan's first items before its later ones.
pub const SITES_IN_FLIGHT: i32 = 5;

/// A site of any other kind under a creep is placed but not built while the
/// creep stays.
fn standable(kind: &Kind) -> bool {
    matches!(kind, Kind::Road | Kind::Container | Kind::Rampart)
}

fn kinds() -> i32 {
    Kind::Other(String::new()).ordinal() + 1
}

/// `(tile, ticks a hauler needs to the source, the tiles a post may stand on)`.
type Post = (Tile, Option<(Tile, Vec<Tile>)>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Placement {
    pub x: i32,
    pub y: i32,
    pub kind: Kind,
}

/// A `byte` per tile, not a `bool`: a `bool[]` costs `Tiles.count` element
/// writes a room a tick, where a typed array is zeroed by its allocation.
#[derive(Clone, Debug)]
pub struct Census {
    pub solid: [u8; tiles::COUNT],
    pub road: [u8; tiles::COUNT],
    pub rampart: [u8; tiles::COUNT],
    pub site: [u8; tiles::COUNT],
    pub occupied: [u8; tiles::COUNT],
    pub walkable: [u8; tiles::COUNT],
    /// Tiles whose container's rock, else controller, has one already: one
    /// standing off the plan's tile is kept, never doubled.
    pub served: [u8; tiles::COUNT],
    pub rocked: Vec<(Tile, Tile)>,
    /// None once a link serves the source: one standing off the plan's tile is
    /// kept, never doubled.
    pub posts: Vec<Post>,
    pub buffer: Option<(Tile, Tile)>,
    /// A solid site there waits on its upgrader as under any creep.
    pub seat: [u8; tiles::COUNT],
    /// Every owner's, and sites: CONTROLLER_STRUCTURES counts them all. By
    /// `Kind.ordinal`.
    pub held: Vec<i32>,
    /// One under a creep is not counted, so it never holds back the rest.
    pub pending: i32,
    pub paying: i32,
}

pub fn census_of(room: &Room, ours: &[Creep]) -> Census {
    let mut solid = [0u8; tiles::COUNT];
    let mut road = [0u8; tiles::COUNT];
    let mut rampart = [0u8; tiles::COUNT];
    let mut site = [0u8; tiles::COUNT];
    let mut held = vec![0i32; kinds() as usize];
    let mut occupied = [0u8; tiles::COUNT];
    let mut walkable = [0u8; tiles::COUNT];

    for (t, w) in walkable.iter_mut().enumerate() {
        if room.terrain.map_or(0, |terrain| terrain.get_bits()[t]) & terrain::WALL == 0 {
            *w = 1;
        }
    }

    for c in ours.iter().chain(room.hostiles()) {
        occupied[tiles::of_pos(&c.pos)] = 1;
    }

    for s in &room.structures {
        let t = tiles::of_pos(&s.pos);

        if s.kind == Kind::Road {
            road[t] = 1;
        } else if s.kind == Kind::Rampart {
            rampart[t] = 1;
        } else {
            solid[t] = 1;
        }

        if !s.walkable {
            walkable[t] = 0;
        }

        held[s.kind.ordinal() as usize] += 1;
    }

    let mut pending = 0;
    let mut paying = 0;

    for s in &room.sites {
        let t = tiles::of_pos(&s.pos);
        site[t] = 1;
        held[s.kind.ordinal() as usize] += 1;

        if !s.kind.walkable() {
            walkable[t] = 0;
        }

        if s.owner.is_ours() && (standable(&s.kind) || occupied[t] == 0) {
            pending += 1;
            paying += i32::from(s.kind.pays_back());
        }
    }

    let standing = |ordinal: i32| -> Vec<Pos> {
        (room.structures.iter().map(|s| (&s.kind, s.pos)))
            .chain(room.sites.iter().map(|s| (&s.kind, s.pos)))
            .filter(|(kind, _)| kind.ordinal() == ordinal)
            .map(|(_, pos)| pos)
            .collect()
    };

    let containers = standing(Kind::Container.ordinal());

    let rocks: Vec<Pos> = room
        .sources
        .iter()
        .map(|s| s.pos)
        .chain(room.minerals.iter().map(|m| m.pos))
        .collect();

    let mut served = [0u8; tiles::COUNT];

    if let Some(c) = &room.controller
        && containers.iter().any(|p| Atlas::is_buffer(room, p))
    {
        for t in tiles::within(engine::UPGRADE_RANGE, tiles::of_pos(&c.pos)) {
            served[t] = 1;
        }
    }

    let mut rocked: Vec<(Tile, Tile)> = Vec::new();
    for rock in &rocks {
        for p in containers.iter().filter(|p| p.within(1, rock)) {
            rocked.push((tiles::of_pos(rock), tiles::of_pos(p)));
            for t in tiles::within(1, tiles::of_pos(rock)) {
                served[t] = 1;
            }
        }
    }

    for rock in &rocks {
        walkable[tiles::of_pos(rock)] = 0;
    }

    if let Some(c) = &room.controller {
        walkable[tiles::of_pos(&c.pos)] = 0;
    }

    let mut seat = [0u8; tiles::COUNT];

    let buffer: Option<(Tile, &Structure)> = (room.controller.as_ref())
        .zip(Atlas::buffer(room))
        .map(|(c, b)| (tiles::of_pos(&c.pos), b));

    if let Some((c, b)) = buffer {
        for t in Atlas::upgrader_seats(|t: Tile| walkable[t] > 0, c, tiles::of_pos(&b.pos)) {
            seat[t] = 1;
        }
    }

    let links = standing(Kind::Link.ordinal());

    let unheld = |t: Tile| -> bool {
        walkable[t] > 0
            && solid[t] == 0
            && road[t] == 0
            && site[t] == 0
            && tiles::x_of(t).min(tiles::y_of(t)) >= 2
            && tiles::x_of(t).max(tiles::y_of(t)) < tiles::SIDE - 2
    };

    let mut posts: Vec<Post> = Vec::new();

    for s in &room.sources {
        if links.iter().any(|p| p.within(2, &s.pos)) {
            posts.push((tiles::of_pos(&s.pos), None));
        } else if let Some((p, _)) = Atlas::post_of(room, &s.pos) {
            let post = tiles::of_pos(&p);
            let beside = tiles::around(post)
                .iter()
                .copied()
                .filter(|t| unheld(*t))
                .collect();
            posts.push((tiles::of_pos(&s.pos), Some((post, beside))));
        }
    }

    Census {
        solid,
        road,
        rampart,
        site,
        occupied,
        walkable,
        served,
        rocked,
        posts,
        buffer: buffer
            .filter(|(_, b)| b.kind == Kind::Container)
            .map(|(c, b)| (c, tiles::of_pos(&b.pos))),
        seat,
        held,
        pending,
        paying,
    }
}

/// Whether a structure on `t` parts the tiles beside it but across `post`,
/// where a miner works and steps aside for no walker. Beside, not the whole
/// room: a walk round the room prices dearer than a step through the miner.
fn closes(census: &Census, post: Tile, t: Tile) -> bool {
    let beside: Vec<Tile> = tiles::around(t)
        .iter()
        .copied()
        .filter(|n| *n != post && census.walkable[*n] > 0)
        .collect();

    let mut reached: Vec<Tile> = beside.iter().take(1).copied().collect();
    let mut i = 0;

    while i < reached.len() {
        for n in &beside {
            if tiles::range(*n, reached[i]) == 1 && !reached.contains(n) {
                reached.push(*n);
            }
        }

        i += 1;
    }

    reached.len() < beside.len()
}

fn held_of(census: &Census, kind: &Kind) -> i32 {
    census.held[kind.ordinal() as usize]
}

/// Below the haul's level no road or container is due, so only a kind short
/// of its allowance can be placed; false lets the caller skip the plan.
pub fn may_place(rules: &Rules, census: &Census, level: i32) -> bool {
    let mut kinds = vec![Kind::Link, Kind::Extractor];
    kinds.extend(sized());

    level >= haul_level(rules)
        || kinds
            .iter()
            .any(|kind| held_of(census, kind) < rules.allowed(kind, level))
}

pub fn posted(plan: &Plan, t: Tile) -> bool {
    (plan.items.iter()).any(|i| i.kind == Kind::Container && i.tile == t)
}

/// Each rock and our container beside it off the plan's post: seated before
/// the room had a plan, it gives way to the post the bays and the link stand
/// around.
pub fn astray(plan: &Plan, census: &Census) -> Vec<(Tile, Tile)> {
    (census.rocked.iter())
        .filter(|(_, c)| !posted(plan, *c))
        .copied()
        .collect()
}

pub fn placeable(item: &Item, level: i32) -> bool {
    item.level <= level && (item.kind != Kind::Lab || engine::topped_out(level))
}

/// The engine stacks a road only with a rampart, any other kind only with a
/// road or a rampart.
pub fn place(
    rules: &Rules,
    census: &Census,
    level: i32,
    plan: &Plan,
    budget: i32,
) -> Vec<Placement> {
    let mut placed: Vec<Placement> = Vec::new();
    let mut paid = census.paying;
    let mut added = vec![0i32; kinds() as usize];
    let added_of = |added: &Vec<i32>, kind: &Kind| -> i32 { added[kind.ordinal() as usize] };

    let mut taken: BTreeSet<Tile> = BTreeSet::new();

    let mut planned = [0u8; tiles::COUNT];
    let mut solid = [0u8; tiles::COUNT];
    // A rampart over a structure due by this level waits for it to stand; a
    // later one would hold a ring tile open until it stands.
    let mut under = [0u8; tiles::COUNT];

    for i in &plan.items {
        planned[i.tile] = 1;

        if !i.kind.walkable() {
            solid[i.tile] = 1;
        }
        if i.kind != Kind::Road && i.kind != Kind::Rampart && placeable(i, level) {
            under[i.tile] = 1;
        }
    }

    // A source's link stands beside the post its miner works from, which
    // another bot may have set off the plan's container tile; on the plan's
    // own, where the plan puts it unless it parts the tiles round it. The roads the
    // plan drew there lead to that unbuilt container, and the tile farthest
    // from the anchor lies behind the miner, off the way in. The last open
    // tile is the miner's way in.
    // A claim's first spawn may stand in a harvest bay, where a pioneer digs:
    // its site waits under the creep and is built once it steps off.
    let founding = |kind: &Kind| *kind == Kind::Spawn && held_of(census, &Kind::Spawn) == 0;
    let tile_of = |item: &Item| -> Option<Tile> {
        let post = census
            .posts
            .iter()
            .find(|(rock, _)| item.kind == Kind::Link && tiles::range(item.tile, *rock) <= 2);

        match (post, census.buffer) {
            (Some((_, None)), _) => None,
            (None, Some((c, b)))
                if item.kind == Kind::Link && census.seat[item.tile] > 0 && !posted(plan, b) =>
            {
                let walkable = |t: Tile| census.walkable[t] > 0;
                let open = |t: Tile| {
                    walkable(t)
                        && census.solid[t] == 0
                        && census.road[t] == 0
                        && census.site[t] == 0
                        && planned[t] == 0
                };

                controller_link(
                    walkable,
                    open,
                    |t: Tile| t == item.tile || solid[t] == 0,
                    c,
                    b,
                )
            }
            (Some((_, Some((post, beside)))), _)
                if closes(census, *post, item.tile)
                    || (!posted(plan, *post) && tiles::range(item.tile, *post) != 1) =>
            {
                let head = (beside.iter().copied())
                    .filter(|t| {
                        plan.items
                            .iter()
                            .all(|i| i.tile != *t || i.kind == Kind::Road)
                            && !closes(census, *post, *t)
                    })
                    .min_by_key(|t| {
                        (
                            plan.items.iter().any(|i| i.tile == *t),
                            -(tiles::range(*t, plan.anchor)),
                            *t,
                        )
                    });
                if beside.len() > 1 { head } else { None }
            }
            _ => Some(item.tile),
        }
    };

    let astray = astray(plan, census);
    // Past the container's own level a room keeps what stands rather than
    // rebuild it.
    let replaces = |t: Tile| {
        level <= haul_level(rules)
            && (census.rocked.iter())
                .filter(|(rock, _)| tiles::range(*rock, t) == 1)
                .all(|rc| astray.contains(rc))
    };

    for item in plan.items.iter().filter(|i| placeable(i, level)) {
        match tile_of(item) {
            Some(t)
                if (placed.len() as i32) < budget
                    && if item.kind.pays_back() {
                        paid
                    } else {
                        census.pending + placed.len() as i32
                    } < SITES_IN_FLIGHT
                    && census.site[t] == 0
                    && (if item.kind == Kind::Rampart {
                        (census.solid[t] > 0 || under[t] == 0) && census.rampart[t] == 0
                    } else {
                        census.solid[t] == 0
                    })
                    && (standable(&item.kind)
                        || census.occupied[t] == 0
                        || founding(&item.kind))
                    && !taken.contains(&t)
                    && (item.kind != Kind::Container || census.served[t] == 0 || replaces(t))
                    && (item.kind != Kind::Road || census.road[t] == 0)
                    && held_of(census, &item.kind) + added_of(&added, &item.kind)
                        < rules.allowed(&item.kind, level) =>
            {
                taken.insert(t);
                paid += i32::from(item.kind.pays_back());
                added[item.kind.ordinal() as usize] = added_of(&added, &item.kind) + 1;

                placed.push(Placement {
                    x: tiles::x_of(t),
                    y: tiles::y_of(t),
                    kind: item.kind.clone(),
                });
            }
            _ => (),
        }
    }

    placed
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fixtures as fx;
    use fabot_base::world::{Owner, Structure};

    #[test]
    fn no_controller_links_site_lands_on_a_seat_of_a_buffer_another_bot_set() {
        let mut rng = fx::Rng::new(10);

        for _ in 0..fx::MAX_TEST {
            let colony = fx::colony(&mut rng);
            let pick = rng.choose(-1_000, 1_000);

            let Some((ground, plan)) = fx::plan_of(&colony) else {
                continue;
            };

            let Some(controller) = ground.controller else {
                continue;
            };

            let rocks: Vec<Tile> = ground
                .sources
                .iter()
                .chain(ground.minerals.iter())
                .copied()
                .collect();

            let off_plan: Vec<Tile> = tiles::within(engine::UPGRADE_RANGE, controller)
                .into_iter()
                .filter(|t| {
                    ground.terrain[*t] != 1
                        && rocks.iter().all(|r| tiles::range(*r, *t) > 1)
                        && plan
                            .items
                            .iter()
                            .all(|i| i.tile != *t || i.kind == Kind::Road)
                })
                .collect();

            if off_plan.is_empty() {
                continue;
            }

            let buffer = { i64::from(pick) % off_plan.len() as i64 }.unsigned_abs() as usize;
            let buffer = off_plan[buffer];

            let mut room = colony.room.clone();
            room.structures.push(fx::structure(
                "buffer",
                Kind::Container,
                Owner::Ours,
                buffer,
            ));

            let census = census_of(&room, &[]);

            for i in plan.items.iter().filter(|i| i.kind == Kind::Link) {
                let mut one = plan.clone();
                one.items = vec![i.clone()];

                for p in place(
                    &Rules::vanilla(),
                    &census,
                    engine::CONTROLLER_MAX_LEVEL,
                    &one,
                    1,
                ) {
                    let t = tiles::index(p.x, p.y);

                    assert!(
                        tiles::range(t, buffer) > 1
                            || tiles::range(t, controller) > engine::UPGRADE_RANGE
                            || ground.sources.iter().any(|r| tiles::range(*r, t) <= 2)
                    );
                }
            }
        }
    }

    #[test]
    fn sites_keep_to_the_level_the_allowance_and_the_caps() {
        let mut rng = fx::Rng::new(11);

        for _ in 0..fx::MAX_TEST {
            let case = fx::census_case(&mut rng);

            let Some((_, plan)) = fx::plan_of(&case.colony) else {
                continue;
            };

            let pick = |ranks: &[i32]| -> Vec<Item> {
                let mut distinct: Vec<i32> = Vec::new();
                for r in ranks {
                    if !distinct.contains(r) {
                        distinct.push(*r);
                    }
                }

                distinct
                    .into_iter()
                    .filter(|r| *r >= 0 && (*r as usize) < plan.items.len())
                    .map(|r| plan.items[r as usize].clone())
                    .collect()
            };

            let built = pick(&case.built);

            let pending: Vec<Item> = pick(&case.pending)
                .into_iter()
                .filter(|i| !built.iter().any(|b| b.tile == i.tile))
                .collect();

            let mut room = case.colony.room.clone();
            room.structures.extend(built.iter().map(|i| {
                fx::structure(&format!("b{}", i.tile), i.kind.clone(), Owner::Ours, i.tile)
            }));
            room.sites = pending
                .iter()
                .map(|i| fx::site(&format!("x{}", i.tile), i.kind.clone(), i.tile))
                .collect();

            let counted = census_of(&room, &[]);
            let rules = Rules::vanilla();
            let placed = place(&rules, &counted, case.level, &plan, case.budget);

            let held = |kind: &Kind| -> i32 {
                (room.structures.iter().filter(|s| s.kind == *kind).count()
                    + room.sites.iter().filter(|s| s.kind == *kind).count()) as i32
            };

            let tiles_placed: Vec<Tile> = placed.iter().map(|p| tiles::index(p.x, p.y)).collect();

            let paid = |kinds: &mut dyn Iterator<Item = &Kind>| {
                kinds.filter(|k| k.pays_back()).count() as i32
            };
            let paying = paid(&mut room.sites.iter().map(|s| &s.kind));
            let paying_after = paying + paid(&mut placed.iter().map(|p| &p.kind));
            let after = room.sites.len() as i32 + placed.len() as i32;

            let solid = |s: &Structure| s.kind != Kind::Road && s.kind != Kind::Rampart;

            // What a kind may go down over: a rampart only over a standing
            // structure no rampart covers yet; a road shares a rampart, any
            // other kind a road or a rampart.
            let clear = |kind: &Kind, here: &[Structure]| -> bool {
                if *kind == Kind::Rampart {
                    here.iter().any(solid) && !here.iter().any(|s| s.kind == Kind::Rampart)
                } else {
                    here.iter().all(|s| {
                        s.kind == Kind::Rampart || (s.kind == Kind::Road && *kind != Kind::Road)
                    })
                }
            };

            let standing = |t: Tile| -> Vec<Structure> {
                room.structures
                    .iter()
                    .filter(|s| tiles::of_pos(&s.pos) == t)
                    .cloned()
                    .collect()
            };

            // An item the rules admit after what was placed: its level
            // reached, its kind short, its tile clear.
            let admitted = |i: &Item| -> bool {
                let t = i.tile;
                let added = placed.iter().filter(|q| q.kind == i.kind).count() as i32;

                placeable(i, case.level)
                    && !room.sites.iter().any(|x| tiles::of_pos(&x.pos) == t)
                    && !tiles_placed.contains(&t)
                    && !(i.kind == Kind::Container && counted.served[t] > 0)
                    && clear(&i.kind, &standing(t))
                    && held(&i.kind) + added < rules.allowed(&i.kind, case.level)
            };

            assert!(may_place(&rules, &counted, case.level) || placed.is_empty());
            let full = |i: &Item| {
                (placed.len() as i32) >= case.budget
                    || if i.kind.pays_back() {
                        paying_after
                    } else {
                        after
                    } >= SITES_IN_FLIGHT
            };
            assert!((placed.len() as i32) <= case.budget.max(0));
            assert!(paying_after <= SITES_IN_FLIGHT.max(paying));
            assert!((placed.iter().enumerate()).all(|(i, p)| {
                p.kind.pays_back() || room.sites.len() + i < SITES_IN_FLIGHT as usize
            }));
            assert!(plan.items.iter().all(|i| !admitted(i) || full(i)));
            assert_eq!(fx::distinct(&tiles_placed), tiles_placed);

            for p in &placed {
                let t = tiles::index(p.x, p.y);

                assert!(!room.sites.iter().any(|x| tiles::of_pos(&x.pos) == t));
                assert!(clear(&p.kind, &standing(t)));
            }

            for p in &placed {
                let added = placed.iter().filter(|q| q.kind == p.kind).count() as i32;

                assert!(held(&p.kind) + added <= rules.allowed(&p.kind, case.level));
            }

            for p in &placed {
                assert!(plan.items.iter().any(|i| {
                    i.tile == tiles::index(p.x, p.y) && i.kind == p.kind && i.level <= case.level
                }));
            }
        }
    }

    #[test]
    fn every_ring_tile_holds_a_rampart_from_the_lowest_ring_level() {
        let rules = Rules::vanilla();
        for colony in fx::samples(14).take(fx::MAX_TEST) {
            let Some((_, mut plan)) = fx::plan_of(&colony) else {
                continue;
            };
            let level = plan.ring.iter().map(|i| i.level).min().unwrap_or(0);
            let ring: Vec<Tile> = (plan.ring.iter())
                .filter(|i| i.kind == Kind::Rampart)
                .map(|i| i.tile)
                .collect();
            plan.items.append(&mut plan.ring);
            let mut room = colony.room.clone();
            loop {
                let placed = place(&rules, &census_of(&room, &[]), level, &plan, i32::MAX);
                if placed.is_empty() {
                    break;
                }
                for p in placed {
                    let t = tiles::index(p.x, p.y);
                    room.structures.push(fx::structure(
                        &format!("s{t}.{}", p.kind.ordinal()),
                        p.kind,
                        Owner::Ours,
                        t,
                    ));
                }
            }
            for t in ring {
                assert!(
                    (room.structures.iter())
                        .any(|s| s.kind == Kind::Rampart && tiles::of_pos(&s.pos) == t),
                    "{t} open"
                );
            }
        }
    }

    #[test]
    fn no_lab_is_placed_below_the_top_level() {
        let rules = Rules::vanilla();
        for colony in fx::samples(14).take(fx::MAX_TEST) {
            let Some((_, mut plan)) = fx::plan_of(&colony) else {
                continue;
            };
            plan.items.retain(|i| i.kind == Kind::Lab);
            for level in 6..=engine::CONTROLLER_MAX_LEVEL {
                let placed = place(
                    &rules,
                    &census_of(&colony.room, &[]),
                    level,
                    &plan,
                    i32::MAX,
                );
                assert_eq!(
                    placed.is_empty(),
                    level < engine::CONTROLLER_MAX_LEVEL,
                    "{level}"
                );
            }
        }
    }

    #[test]
    fn a_container_off_the_plans_post_gives_way_to_the_plans_own() {
        let rules = Rules::vanilla();
        for colony in fx::samples(16).take(fx::MAX_TEST) {
            let Some((ground, plan)) = fx::plan_of(&colony) else {
                continue;
            };
            for rock in &ground.sources {
                let Some(post) = (plan.items.iter())
                    .find(|i| i.kind == Kind::Container && tiles::range(i.tile, *rock) == 1)
                else {
                    continue;
                };
                let Some(stray) = tiles::around(*rock)
                    .iter()
                    .copied()
                    .find(|t| ground.terrain[*t] != 1 && !tiles::on_edge(*t) && !posted(&plan, *t))
                else {
                    continue;
                };
                let mut room = colony.room.clone();
                room.structures.push(fx::structure(
                    "stray",
                    Kind::Container,
                    Owner::Nobody,
                    stray,
                ));
                let mut one = plan.clone();
                one.items = vec![post.clone()];
                let placed = place(&rules, &census_of(&room, &[]), haul_level(&rules), &one, 1);
                assert_eq!(placed.len(), 1, "{} beside {rock} not placed", post.tile);
            }
        }
    }
}
