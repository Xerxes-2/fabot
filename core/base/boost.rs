use super::declarations::Declarations;
use super::engine;
use super::vocabulary::{self, ResourceType};
use super::world::{self, Id, Pos, RoomName, StructureType, World};

/// A cast name's last token names the compound its body walks to a lab for.
pub fn of_name(name: &str) -> Option<ResourceType> {
    let compound: ResourceType = name.rsplit('-').next()?.parse().ok()?;
    compound.reaction_components().map(|_| compound)
}

/// The lab a body asking for `compound` is boosted from: one with room for it
/// holding the most of it, ties to the lower id; when none has room, Hivemind's
/// (`boost-manager.ts`): no reagent lab, the one holding least of another
/// mineral, so its drain is the shortest.
pub fn booster<'a>(
    world: &'a World,
    room: RoomName,
    compound: ResourceType,
) -> Option<&'a world::Structure> {
    let held = |s: &world::Structure| s.store.amount_of(compound);
    let most = |a: &&'a world::Structure, b: &&'a world::Structure| {
        held(a).cmp(&held(b)).then_with(|| b.id.cmp(&a.id))
    };
    let all: Vec<(Id, Pos)> = labs_in(world, room).map(|l| (l.id, l.pos)).collect();
    let reagents = roles_of(&all).map(|r| r.reagents);
    let reagent = |l: &world::Structure| reagents.is_some_and(|(a, b)| l.id == a || l.id == b);
    let other = |l: &world::Structure| {
        (l.store.held.iter())
            .filter(|(r, _)| vocabulary::is_mineral(*r))
            .map(|(_, n)| *n)
            .sum::<i32>()
    };

    (labs_in(world, room))
        .filter(|l| holds_room_for(l, compound))
        .max_by(most)
        .or_else(|| (labs_in(world, room)).min_by_key(|l| (reagent(l), other(l), l.id)))
}

/// Most labs, ties to the first by name: one still building never takes it.
pub fn cluster(world: &World, declarations: &Declarations) -> Option<RoomName> {
    (world::hostiles::owned(&world.rooms).into_iter())
        .filter(|room| !declarations.abandoning(*room))
        .map(|room| (labs_in(world, room).count(), std::cmp::Reverse(room)))
        .filter(|(labs, _)| *labs > 0)
        .max()
        .map(|(_, room)| room.0)
}

pub fn labs_in<'a>(world: &'a World, room: RoomName) -> impl Iterator<Item = &'a world::Structure> {
    (world.rooms.iter())
        .filter(move |r| r.name == room)
        .flat_map(|r| r.structures.iter())
        .filter(|s| s.kind == world::StructureType::Lab && s.owner.is_ours())
}

pub fn home_holds(world: &World, room: RoomName, compound: ResourceType) -> (i32, i32) {
    let held = |s: &world::Store| (s.amount_of(compound), s.amount_of(ResourceType::Energy));
    let stores = (world.rooms.iter())
        .filter(|r| r.name == room)
        .flat_map(|r| r.structures.iter())
        .filter(|s| {
            s.owner.is_ours()
                && matches!(
                    s.kind,
                    StructureType::Lab | StructureType::Terminal | StructureType::Storage
                )
        })
        .map(|s| held(&s.store))
        .chain(
            (world.creeps.iter())
                .filter(move |c| c.pos.room == room)
                .map(|c| held(&c.store)),
        );

    stores.fold((0, 0), |(compound_, energy), (m, e)| {
        (compound_ + m, energy + e)
    })
}

/// A lab holds one mineral: another in it leaves the compound nowhere to land.
fn holds_room_for(lab: &world::Structure, compound: ResourceType) -> bool {
    (lab.store.held.iter()).all(|(r, n)| *n <= 0 || *r == compound || !vocabulary::is_mineral(*r))
}

pub fn can_boost(lab: &world::Structure, compound: ResourceType) -> bool {
    lab.store.amount_of(compound) >= engine::BOOST_MINERAL_PER_PART
        && lab.store.amount_of(ResourceType::Energy) >= engine::BOOST_ENERGY_PER_PART
}

fn boost_need(creep: &world::Creep, compound: ResourceType) -> (i32, i32) {
    let parts = (creep.body.iter())
        .filter(|p| p.boost.is_none() && vocabulary::takes(p.kind, compound))
        .count() as i32;

    (
        parts * engine::BOOST_MINERAL_PER_PART,
        parts * engine::BOOST_ENERGY_PER_PART,
    )
}

pub struct Waiting<'a> {
    pub lab: &'a world::Structure,
    pub compound: ResourceType,
    pub mineral: i32,
    pub energy: i32,
}

/// The lab a body waits at this tick, and what its parts still ask of it: one
/// that has room for the compound, while the home holds what the parts ask and
/// can carry it there. Read afresh every tick, so a colony that cannot serve the
/// body costs it the compound and never its raid.
pub fn waiting<'a>(world: &'a World, home: RoomName, creep: &world::Creep) -> Option<Waiting<'a>> {
    let compound = of_name(&creep.name)?;
    let (mineral, energy) = boost_need(creep, compound);
    let (held, fuel) = home_holds(world, home, compound);

    if mineral == 0 || held < mineral || fuel < energy {
        return None;
    }
    let lab = booster(world, home, compound)?;
    // A colony with no body to carry the load fights the body unboosted
    // rather than stand it at a lab nothing will fill, even while loaded.
    let carried = can_boost(lab, compound)
        || (world.creeps.iter()).any(|c| {
            c.owner.is_ours()
                && c.pos.room == home
                && c.name != creep.name
                && of_name(&c.name).is_none()
                && c.store.capacity > 0
        });
    if !holds_room_for(lab, compound) || !carried {
        return None;
    }

    Some(Waiting {
        lab,
        compound,
        mineral,
        energy,
    })
}

/// The two reagent labs, and every lab within reach of both as an output.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Roles {
    pub reagents: (Id, Id),
    pub outputs: Vec<Id>,
}

/// Off the labs' tiles alone, so they hold from tick to tick: the pair the
/// most others stand within reach of both of, ties to the lower ids.
pub fn roles_of(labs: &[(Id, Pos)]) -> Option<Roles> {
    let mut sorted: Vec<&(Id, Pos)> = labs.iter().collect();
    sorted.sort_by_key(|a| a.0);
    let reaches = |a: &Pos, b: &Pos| a.within(screeps::LAB_REACTION_RANGE as i32, b);

    let mut best: Option<Roles> = None;
    for (i, (a, at_a)) in sorted.iter().map(|l| (&l.0, &l.1)).enumerate() {
        for (b, at_b) in sorted.iter().skip(i + 1).map(|l| (&l.0, &l.1)) {
            let outputs: Vec<Id> = sorted
                .iter()
                .filter(|(id, at)| id != a && id != b && reaches(at, at_a) && reaches(at, at_b))
                .map(|(id, _)| *id)
                .collect();
            if best
                .as_ref()
                .map_or(!outputs.is_empty(), |r| outputs.len() > r.outputs.len())
            {
                best = Some(Roles {
                    reagents: (*a, *b),
                    outputs,
                });
            }
        }
    }
    best
}
