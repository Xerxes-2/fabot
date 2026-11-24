//! `sim duel --real`: our bodies fight as a home's Defend (`core/fight/act.rs`)
//! with the sample's tactic: the raid's group and kill order, Approaching until
//! one of ours stands in reach of it, then the same formation, walk, reflexes
//! and arbitrated moves. Our towers stay the duel's stand-in. A Defend has no
//! siege front and raids nothing: no heal is owed ahead and the strike is unarmed.
use std::collections::{BTreeMap, BTreeSet};

use fabot_atlas::{Atlas, Context, derive};
use fabot_base::engine::RANGED_RANGE;
use fabot_base::intents::{Action, CreepAct};
use fabot_base::world::{Creep, Id, Sightings, StructureType};
use fabot_fight::shape::{Group, Phase};
use fabot_fight::tracker::{self, LOST_GROUP_TICKS};
use fabot_fight::{casting, formation, fortified, reflexes};
use fabot_tactics::{self as tactics, Tactic};
use krebs::world::World as Krebs;
use screeps::LocalRoomTerrain;

use crate::act::{self, Stored};
use crate::read;
use crate::view::View;

#[derive(Clone)]
pub struct Real {
    tactic: Tactic,
    atlas: Atlas,
    groups: Vec<Group>,
    order: Vec<Id>,
    phase: Phase,
}

impl Real {
    pub fn new(tactic: Tactic) -> Real {
        Real {
            tactic,
            atlas: Atlas::default(),
            groups: Vec::new(),
            order: Vec::new(),
            phase: Phase::Approaching,
        }
    }

    pub fn orders(&mut self, k: &Krebs) -> Stored {
        let (v, declarations) = (
            View::of(k),
            fabot_base::declarations::Declarations::default(),
        );
        let terrain = LocalRoomTerrain::new_from_bits(Box::new(*k.terrain.raw(0)));
        let mut world = read::world(&v, &declarations);
        world.rooms[0].terrain = Some(&terrain);
        let (room, t) = (world.rooms[0].name, world.time);
        let terrains = [(room, terrain.get_bits())];
        let mut atlas = Atlas::following(std::mem::take(&mut self.atlas), &world, [], terrains);
        let derived = derive(&world, &mut atlas);
        let hostiles: Vec<Creep> = world.rooms[0].hostiles().cloned().collect();
        let owned = &derived.owned;
        (self.groups, _) = tracker::step(LOST_GROUP_TICKS, 0, owned, &self.groups, &hostiles, t);
        let group = self.groups.first().expect("a raid stands");
        if self.order.is_empty() {
            self.order = casting::kill_order(&hostiles);
        }
        let ours: Vec<&Creep> = world.creeps.iter().collect();
        let raid = hostiles.iter().filter(|h| group.members.contains(&h.id));
        if (raid.clone()).any(|h| ours.iter().any(|c| c.pos.within(RANGED_RANGE, &h.pos))) {
            self.phase = Phase::Fighting;
        }

        let (foes, avoid) = (BTreeMap::from([(room, hostiles.clone())]), BTreeSet::new());
        let ramparts = (world.rooms[0].structures.iter())
            .filter(|s| s.kind == StructureType::Rampart && s.owner.is_ours());
        let mut sight = tactics::Sight {
            atlas: &mut atlas,
            hostiles: &foes,
            struck: hostiles.iter().map(|h| h.id).collect(),
            spared: BTreeMap::new(),
            ramparts: BTreeMap::from([(room, ramparts.map(|s| s.pos).collect())]),
            musters: Vec::new(),
            avoid: &avoid,
        };

        let (on, walk) = (
            (self.tactic, &self.order[..]),
            (self.phase, false, true, false),
        );
        let centre = (group.centre, RANGED_RANGE);
        let fortified = fortified(&world, &[]);
        let blob = formation(&mut sight, on, walk, ours.clone(), centre, &fortified);
        let mut walking: BTreeMap<String, _> =
            blob.into_iter().map(|w| (w.creep.clone(), w)).collect();
        let (mut wishes, mut chosen) = (Vec::new(), BTreeMap::<String, Vec<CreepAct>>::new());
        for c in &ours {
            let wish = walking.remove(&c.name).unwrap_or_else(|| {
                let set = (&ours[..], &ours[..]);
                tactics::engage(&mut sight, on, (&group.centre, true), set, c)
            });
            wishes.push((room, wish));
            chosen.insert(c.name.clone(), tactics::strike(&sight, on.1, c, false));
        }
        let mut actions: Vec<Action> =
            reflexes(chosen, (&ours, &world.creeps), &BTreeMap::new()).collect();
        let context = Context {
            world: &world,
            atlas: &mut atlas,
            declarations: &declarations,
            sightings: &Sightings::default(),
            intel: &fabot_base::intel::Intel::default(),
            derived: &derived,
            trace: false,
        };
        actions.extend(fabot_core::tick::arbitrate(&context, &wishes).0);
        self.atlas = atlas;
        act::run(&v, &actions).0
    }
}
