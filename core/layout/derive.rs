use std::collections::{BTreeMap, BTreeSet};

use fabot_base::{
    declarations::{ColonyPlan, Stage},
    tiles::{self, Tile, TileSet},
    vocabulary::ResourceType,
    world::{Creep, Id, Pos, RoomName, Site, Structure, StructureType, World},
};

use super::{
    ground, plan,
    shape::Standing,
    sites::{self, Census},
};

pub struct Colony {
    pub room: RoomName,
    pub target: i32,
    pub plan: Option<super::shape::Plan>,
    pub census: Census,
    pub towers: Vec<Structure>,
    pub hostiles: Vec<Creep>,
    /// Not a road or a container: anything walking by stands beside one.
    pub struck: Vec<Pos>,
    pub ours: Vec<Creep>,
    pub ramparts: Vec<Structure>,
    pub buried: Vec<Id>,
    pub astray: Vec<Id>,
    pub unpaved: Vec<Id>,
}

/// `ARCHITECTURE.md` §5: Hivemind's search spread over ticks.
const CENTRES_PER_TICK: usize = 4;

pub fn colonies(
    world: &World,
    owned: &BTreeSet<RoomName>,
    plans: &[ColonyPlan],
    mut standing: BTreeMap<RoomName, Standing>,
) -> (Vec<Colony>, BTreeMap<RoomName, Standing>) {
    let mut next: BTreeMap<RoomName, Standing> = BTreeMap::new();
    let mut found: Vec<Colony> = Vec::new();
    let mut budget = CENTRES_PER_TICK;

    for room in owned.iter().filter_map(|name| world.room(*name)) {
        let Some(colony) = plans.iter().find(|p| p.room == room.name) else {
            continue;
        };

        // Terrain, sources, minerals and the controller never move, so the
        // key leaves them out.
        let key = ground::obstacles(room);

        let held = standing.remove(&room.name);
        let cold = held.is_none();
        let held = held.filter(|s| s.key == key);
        let ground = (held.as_ref())
            .is_none_or(|s| s.drawing.is_some())
            .then(|| ground::of_room(room))
            .flatten();
        let mut kept = held.unwrap_or_else(|| Standing {
            key,
            plan: None,
            drawing: ground.as_ref().map(plan::Drawing::of),
            cold,
        });
        if let (Some(d), Some(g)) = (&mut kept.drawing, &ground)
            && let plan::Drawn::Done(done) = d.advance(&world.rules, g, &mut budget)
        {
            kept.plan = done;
            kept.drawing = None;
        }

        let plan = match colony.stage {
            Stage::Abandon => None,
            Stage::Exploit => kept.plan.clone().map(|mut plan| {
                if colony.ring {
                    plan.items.append(&mut plan.ring);
                }
                plan
            }),
        };
        next.insert(room.name, kept);

        let ours: Vec<Creep> =
            world.creeps.iter().filter(|creep| creep.pos.room == room.name).cloned().collect();

        let mut towers: Vec<Structure> = (world.rules.towers(room))
            .filter(|s| s.owner.is_ours() && fabot_base::world::charged(s))
            .cloned()
            .collect();

        towers.sort_by_key(|a| a.id);

        let blocked: TileSet = (room.structures.iter())
            .filter(|s| (s.owner.is_ours() || s.kind == StructureType::Wall) && !s.walkable)
            .map(|s| tiles::of_pos(&s.pos))
            .collect();
        let mut census = sites::census_of(room, &ours);
        let stock = room.storage().map(|s| s.store.amount_of(ResourceType::Energy));
        census.stocked = stock.is_some_and(|n| n >= colony.reserve);
        let astray: Vec<Tile> = match &plan {
            Some(plan) => (sites::astray(plan, &census).into_iter())
                .filter(|(rock, _)| {
                    (census.rocked.iter())
                        .any(|(r, p)| r == rock && sites::posted(plan, *p) && census.site[*p] == 0)
                })
                .map(|(_, c)| c)
                .chain(
                    (room.structures.iter())
                        .filter(|w| w.kind == StructureType::Wall && w.hits > 0)
                        .map(|w| tiles::of_pos(&w.pos))
                        .filter(|t| !tiles::on_edge(*t) && !plan.walls(*t)),
                )
                .collect(),
            None => Vec::new(),
        };

        let owed: TileSet = plan.iter().flat_map(|p| sites::unpaved(p, &census)).collect();
        let road = |s: &&Site| s.kind == StructureType::Road && s.owner.is_ours();
        found.push(Colony {
            room: room.name,
            target: colony.target,
            plan,
            census,
            towers,
            hostiles: room.hostiles().cloned().collect(),
            struck: room
                .structures
                .iter()
                .filter(|s| {
                    s.owner.is_ours()
                        && s.kind != StructureType::Road
                        && s.kind != StructureType::Container
                })
                .map(|s| &s.pos)
                .chain(ours.iter().map(|c| &c.pos))
                .copied()
                .collect(),
            ours,
            ramparts: (room.structures.iter())
                .filter(|s| s.kind == StructureType::Rampart && s.owner.is_ours())
                .cloned()
                .collect(),
            buried: (room.structures.iter())
                .filter(|r| {
                    r.kind == StructureType::Road && blocked.contains(tiles::of_pos(&r.pos))
                })
                .map(|r| r.id)
                .collect(),
            astray: (room.structures.iter())
                .filter(|c| {
                    matches!(c.kind, StructureType::Container | StructureType::Wall)
                        && astray.contains(&tiles::of_pos(&c.pos))
                })
                .map(|c| c.id)
                .collect(),
            unpaved: (room.sites.iter().filter(road))
                .filter_map(|s| owed.contains(tiles::of_pos(&s.pos)).then_some(s.id))
                .collect(),
        });
    }

    (found, next)
}

#[cfg(test)]
mod tests {
    use fabot_base::{
        intents::{Action, RoomAct},
        tiles,
        world::{Built, Owner, StructureType, hostiles},
    };

    use super::*;
    use crate::{fixtures as fx, shape::Plan};

    fn agree(a: &Option<Plan>, b: &Option<Plan>) -> bool {
        match (a, b) {
            (Some(x), Some(y)) => fx::same(x, y),
            (None, None) => true,
            _ => false,
        }
    }

    /// Sampled real rooms built whole to their plan, a road laid under every
    /// structure: the engine's constructed wall is nobody's.
    #[test]
    fn every_road_under_our_solid_structure_or_a_wall_is_buried_and_no_other() {
        let rules = fabot_base::world::Rules::vanilla();
        let mut buried = 0;
        for real in fabot_base::testing::real_rooms().step_by(100) {
            let Some(plan) =
                ground::of_room(&real.room()).and_then(|g| plan::of_ground(&rules, &g))
            else {
                continue;
            };
            let mut world = fabot_base::testing::world(r#"{"rooms":[]}"#);
            world.rooms.push(real.room());
            let spawn = plan.items.iter().find(|i| i.kind == StructureType::Spawn);
            let spawn = spawn.expect("a spawn").clone();
            fabot_base::testing::colonize(&mut world, real.name, 8, spawn.tile);
            let at = |t: Tile| tiles::to_pos(real.name, t);
            let stand = |kind: StructureType, owner, t: Tile| Structure {
                owner,
                ..fabot_base::testing::built(&format!("{:.2}{t}", format!("{kind:?}")), kind, at(t))
            };
            let (mut laid, mut solid) = (BTreeSet::new(), BTreeSet::new());
            for i in plan.items.iter().chain(&plan.ring) {
                let owner = if i.kind == StructureType::Wall { Owner::Nobody } else { Owner::Ours };
                if !i.kind.walkable() {
                    solid.insert(i.tile);
                }
                if i.kind != StructureType::Road && *i != spawn {
                    world.rooms[0].structures.push(stand(i.kind, owner, i.tile));
                }
                laid.insert(i.tile);
            }
            for t in laid {
                world.rooms[0].structures.push(stand(StructureType::Road, Owner::Ours, t));
            }
            let owned = BTreeSet::from([real.name]);
            let (found, _) = colonies(&world, &owned, &fx::exploited(&owned), BTreeMap::new());
            let held: BTreeSet<Tile> = (world.rooms[0].structures.iter())
                .filter(|s| found[0].buried.contains(&s.id))
                .map(|s| tiles::of_pos(&s.pos))
                .collect();
            assert_eq!(held, solid, "{}", real.name);
            let destroyed = (crate::run::decide_over((&rules, None), &found, 0).into_iter())
                .filter(|a| matches!(a, Action::Room(_, RoomAct::Destroy(_))))
                .count();
            assert_eq!(destroyed, found[0].buried.len() + found[0].astray.len());
            buried += solid.len();
        }
        assert!(buried > 0);
    }

    /// A road site laid before the ring was declared holds its rampart off
    /// below the reserve, where no builder takes it.
    #[test]
    fn every_road_site_on_an_unwalled_ring_tile_is_removed_and_no_other() {
        let mut removed = 0;
        for real in fabot_base::testing::real_rooms().step_by(200) {
            let mut world = fabot_base::testing::world(r#"{"rooms":[]}"#);
            world.rooms.push(real.room());
            let terrain = world.rooms[0].terrain;
            let open = |t: Tile| terrain.is_some_and(|g| g.get_bits()[t.index()] == 0);
            let spawn =
                tiles::all().find(|t| open(*t) && !tiles::on_edge(*t)).expect("open ground");
            fabot_base::testing::colonize(&mut world, real.name, 8, spawn);
            let standing = settled(&world, BTreeMap::new());
            let Some(plan) = standing[&real.name].plan.clone() else {
                continue;
            };
            let ring = plan.ring.iter().filter(|i| i.kind == StructureType::Rampart);
            let mut owed = BTreeSet::new();
            for (k, i) in ring.enumerate() {
                let (t, pos) = (i.tile, tiles::to_pos(real.name, i.tile));
                let id = format!("r{t}");
                if k % 2 == 0 {
                    let rampart = fabot_base::testing::built(&id, StructureType::Rampart, pos);
                    world.rooms[0].structures.push(rampart);
                } else {
                    owed.insert(Id::from(id.as_str()));
                }
                let (kind, owner) = (StructureType::Road, Owner::Ours);
                let site = fabot_base::world::Site {
                    id: id.as_str().into(),
                    kind,
                    pos,
                    owner,
                    progress: 0,
                    progress_total: 300,
                };
                world.rooms[0].sites.push(site);
            }
            let owned = BTreeSet::from([real.name]);
            let mut plans = fx::exploited(&owned);
            plans[0].ring = true;
            let (found, _) = colonies(&world, &owned, &plans, standing);
            assert_eq!(found[0].unpaved.iter().copied().collect::<BTreeSet<Id>>(), owed);
            removed += owed.len();
        }
        assert!(removed > 0);
    }

    /// An older plan's walls stand where the plan now leaves an exit's gap
    /// (E9S9's 40,6 and 41,6): each wall off the plan is razed, none on it.
    #[test]
    fn every_wall_off_the_plan_is_astray_and_none_on_it() {
        use fabot_base::testing;
        let walled =
            testing::world(include_str!("../../scripts/scenario/worlds/walled-exit-e9s9.json"));
        let real: Vec<testing::RealRoom> = testing::real_rooms().step_by(200).collect();
        let rooms = [walled.rooms[0].clone()].into_iter().chain(real.iter().map(|r| r.room()));
        let (mut rng, mut razed) = (fx::Rng::new(19), 0);
        for room in rooms {
            let name = room.name;
            let mut world = testing::world(r#"{"rooms":[]}"#);
            world.rooms.push(room);
            let terrain = world.rooms[0].terrain;
            let open = |t: Tile| terrain.is_some_and(|g| g.get_bits()[t.index()] == 0);
            let spawn =
                tiles::all().find(|t| open(*t) && !tiles::on_edge(*t)).expect("open ground");
            testing::colonize(&mut world, name, 8, spawn);
            let standing = settled(&world, BTreeMap::new());
            let Some(plan) = standing[&name].plan.clone() else {
                continue;
            };
            let taken =
                |t: Tile| t == spawn || plan.items.iter().chain(&plan.ring).any(|i| i.tile == t);
            let random =
                (0..20).map(|_| Tile::of_index(rng.choose(0, tiles::COUNT as i32 - 1) as usize));
            let incident = [tiles::index(40, 6), tiles::index(41, 6)];
            let off: BTreeSet<Tile> = (incident.into_iter().chain(random))
                .filter(|t| open(*t) && !tiles::on_edge(*t) && !taken(*t))
                .collect();
            let on = plan.ring.iter().filter(|i| i.kind == StructureType::Wall).map(|i| i.tile);
            for t in off.iter().copied().chain(on) {
                let wall =
                    testing::built(&format!("w{t}"), StructureType::Wall, tiles::to_pos(name, t));
                let wall = Structure { owner: Owner::Nobody, ..wall };
                world.rooms[0].structures.push(wall);
            }
            let owned = BTreeSet::from([name]);
            let (found, _) = colonies(&world, &owned, &fx::exploited(&owned), standing);
            let astray: BTreeSet<Tile> = (world.rooms[0].structures.iter())
                .filter(|s| found[0].astray.contains(&s.id))
                .map(|s| tiles::of_pos(&s.pos))
                .collect();
            assert_eq!(astray, off, "{name}");
            razed += off.len();
        }
        assert!(razed > 0);
    }

    fn settled(
        world: &World,
        standing: BTreeMap<RoomName, Standing>,
    ) -> BTreeMap<RoomName, Standing> {
        let owned = hostiles::owned(&world.rooms);
        let plans = fx::exploited(&owned);
        let mut standing = standing;
        loop {
            let (_, next) = colonies(world, &owned, &plans, standing.clone());
            standing = next;
            if standing.values().all(|s| s.drawing.is_none()) {
                return standing;
            }
        }
    }

    #[test]
    fn a_standing_plan_is_kept_while_its_key_holds_and_drawn_afresh_when_the_ground_moves() {
        let mut rng = fx::Rng::new(12);

        for _ in 0..4 {
            let colony = fx::colony(&mut rng);
            let wall = rng.choose(0, tiles::COUNT as i32 - 1);
            let world = fx::world_of(colony.room.clone(), Vec::new(), Vec::new());
            let standing = settled(&world, BTreeMap::new());
            let plan_of =
                |s: &BTreeMap<RoomName, Standing>| s.values().next().and_then(|s| s.plan.clone());

            let owned = hostiles::owned(&world.rooms);
            let (_, kept) = colonies(&world, &owned, &fx::exploited(&owned), standing.clone());
            assert!(agree(&plan_of(&standing), &plan_of(&kept)));

            let mut ringed = colony.room.clone();
            ringed.structures.push(fx::structure(
                "r",
                StructureType::Wall,
                Owner::Nobody,
                tiles::index(25, 25),
            ));
            let ringed = fx::world_of(ringed, Vec::new(), Vec::new());
            let (_, kept) = colonies(&ringed, &owned, &fx::exploited(&owned), standing.clone());
            assert!(agree(&plan_of(&standing), &plan_of(&kept)));

            for edge in [tiles::index(0, 25), tiles::index(25, 49)] {
                let mut edged = colony.room.clone();
                let wall = fx::structure("e", StructureType::Wall, Owner::Nobody, edge);
                edged.structures.push(wall);
                assert_ne!(ground::obstacles(&edged), ground::obstacles(&colony.room));
            }

            let mut walled = colony.room.clone();
            walled.structures.push(fx::structure(
                "w",
                StructureType::Spawn,
                Owner::Stranger("x".into()),
                Tile::of_index(wall.unsigned_abs() as usize % tiles::COUNT),
            ));
            let walled = fx::world_of(walled, Vec::new(), Vec::new());
            let moved = settled(&walled, standing);
            let fresh = settled(&walled, BTreeMap::new());
            assert!(agree(&plan_of(&moved), &plan_of(&fresh)));
        }
    }
}
