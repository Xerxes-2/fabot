use super::declarations::{self, Declarations};
use super::engine;
use super::vocabulary::{self, Part, RESOURCES_ALL, ResourceType, takes};
use super::world::{self, Id, Pos, RoomName, StructureType, World};

/// A lab a carrier fills from the storage takes 85 ticks, measured; a
/// compound already in a lab costs a tick or two, taken as none.
const LAB_FILL: i32 = 85;

/// The lab wait the Exchange prices a cast for, and the age past which a body
/// waits no longer (owner): it leaves with what it has.
pub fn lab_wait(fills: usize) -> i32 {
    LAB_FILL * fills as i32
}

/// A colony fills one boost lab at a time: the fills queued before a body's own.
#[derive(Clone, Copy, Default)]
pub struct LabQueue(pub usize);

impl LabQueue {
    pub fn wait_with(self, fills: usize) -> i32 {
        lab_wait(self.0 + fills)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Held {
    pub compound: ResourceType,
    pub parts: i32,
    pub in_labs: i32,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Stock {
    pub held: Vec<Held>,
    pub energy: i32,
}

impl Stock {
    pub fn of(world: &World, home: RoomName) -> Stock {
        let held = (RESOURCES_ALL.iter().copied())
            .filter(|c| vocabulary::ALL_BODY_PARTS.iter().any(|p| takes(*p, *c)))
            .map(|c| Held {
                compound: c,
                parts: home_holds(world, home, c) / engine::BOOST_MINERAL_PER_PART,
                in_labs: labs_in(world, home)
                    .map(|l| l.store.amount_of(c))
                    .sum::<i32>()
                    / engine::BOOST_MINERAL_PER_PART,
            })
            .filter(|h| h.parts > 0)
            .collect();
        Stock {
            held,
            energy: home_holds(world, home, ResourceType::Energy) / engine::BOOST_ENERGY_PER_PART,
        }
    }
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Visits {
    pub compounds: Vec<ResourceType>,
    pub fills: usize,
}

/// Spent from `stock` as the body's visits spend it, so neither pricing nor
/// the cast counts one stock or one energy twice. Part types go by
/// `vocabulary::BOOSTS` rank: a colony short of stock runs short of the
/// lowest-ranked.
pub fn boosted_by(body: &[Part], stock: &mut Stock) -> Visits {
    let strength = |p: Part, c: ResourceType| {
        (100.0 * f64::from(vocabulary::multiple(p, Some(c))) / vocabulary::damage_ratio(p, Some(c)))
            as i32
    };
    let rank = |p: Part| {
        (vocabulary::BOOSTS.iter())
            .filter(|(c, _)| takes(p, *c))
            .map(|(_, r)| *r)
            .max()
    };
    let mut order: Vec<Part> = (vocabulary::ALL_BODY_PARTS.iter().copied())
        .filter(|p| body.contains(p) && stock.held.iter().any(|h| takes(*p, h.compound)))
        .collect();
    order.sort_by_key(|p| std::cmp::Reverse(rank(*p)));

    let mut visits = Visits::default();
    for p in order {
        let n = vocabulary::part_count_in(body, p);
        let fuel = stock.energy;
        let best = (stock.held.iter_mut())
            .filter(|h| n <= fuel && takes(p, h.compound) && n <= h.parts)
            .max_by_key(|h| strength(p, h.compound));
        if let Some(h) = best {
            h.parts -= n;
            stock.energy -= n;
            visits.fills += usize::from(h.in_labs < n);
            h.in_labs = (h.in_labs - n).max(0);
            visits.compounds.push(h.compound);
        }
    }
    visits
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Load {
    pub compound: ResourceType,
    pub mineral: i32,
    pub energy: i32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Wait {
    pub creep: Id,
    pub lab: Id,
    pub at: Pos,
    pub load: Load,
    pub stands: bool,
    pub carried: bool,
    pub left: i32,
}

#[derive(Clone, Debug, Default, PartialEq)]
pub struct Boosting {
    pub waits: Vec<Wait>,
    pub left: Stock,
}

/// Read afresh each tick, nothing kept: of the bodies a forming set's plan
/// boosts here and no fight has touched, oldest first, as pricing spends a
/// set in cast order, each still within its lab wait takes its share.
pub fn boosting(world: &World, room: RoomName, boosted: &[&str]) -> Boosting {
    let ours = |c: &&world::Creep| c.owner.is_ours() && c.pos.room == room;
    let carrier = (world.creeps.iter())
        .filter(ours)
        .any(|c| !declarations::fights(&c.name) && c.store.capacity > 0);
    let mut bodies: Vec<&world::Creep> = (world.creeps.iter())
        .filter(ours)
        .filter(|c| c.hits == c.hits_max && boosted.contains(&c.name.as_str()))
        .collect();
    bodies.sort_by(|a, b| (a.ticks_to_live, &a.name).cmp(&(b.ticks_to_live, &b.name)));
    let mut left = Stock::of(world, room);
    let mut waits: Vec<Wait> = Vec::new();
    let mut queue = LabQueue::default();

    for c in bodies {
        let bare: Vec<Part> = (c.body.iter())
            .filter(|p| p.boost.is_none())
            .map(|p| p.kind)
            .collect();
        let on: Vec<ResourceType> = c.body.iter().filter_map(|p| p.boost).collect();
        let had = (0..on.len())
            .filter(|i| !on[..*i].contains(&on[*i]))
            .count();
        let mut share = left.clone();
        let by = boosted_by(&bare, &mut share);
        let age = engine::lifetime(c.body.iter().map(|p| p.kind)) - c.ticks_to_live;
        // A fill for each compound it took and for its next, one come early too.
        let own = had + by.fills.max(usize::from(!by.compounds.is_empty()));
        let bound = queue.wait_with(own);
        let Some(&compound) = by.compounds.first().filter(|_| age <= bound) else {
            continue;
        };
        let taken: Vec<(Id, ResourceType)> =
            waits.iter().map(|w| (w.lab, w.load.compound)).collect();
        let Some(lab) = booster(world, room, compound, &taken) else {
            continue;
        };
        left = share;
        queue.0 += by.fills;
        let (mineral, energy) = boost_need(c, compound);
        waits.push(Wait {
            creep: c.id,
            lab: lab.id,
            at: lab.pos,
            load: Load {
                compound,
                mineral,
                energy,
            },
            // No carrier: the body fights unboosted rather than stand at a lab nothing fills.
            stands: holds_room_for(lab, compound) && (can_boost(lab, compound) || carrier),
            carried: can_boost(lab, compound) || carrier,
            left: queue.wait_with(0).min(bound - age),
        });
    }
    Boosting { waits, left }
}

/// The lab a body asking for `compound` is boosted from: one with room for it
/// holding the most of it, ties to the lower id; when none has room, Hivemind's
/// (`boost-manager.ts`): no reagent lab, the one holding least of another
/// mineral, so its drain is the shortest.
fn booster<'a>(
    world: &'a World,
    room: RoomName,
    compound: ResourceType,
    taken: &[(Id, ResourceType)],
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

    let free =
        || labs_in(world, room).filter(|l| !taken.iter().any(|t| t.0 == l.id && t.1 != compound));

    (free().filter(|l| holds_room_for(l, compound)).max_by(most))
        .or_else(|| free().min_by_key(|l| (reagent(l), other(l), l.id)))
}

/// Most labs first, ties by name: one still building its labs picks a reaction last.
pub fn clusters(world: &World, declarations: &Declarations) -> Vec<RoomName> {
    let mut labs: Vec<(usize, std::cmp::Reverse<RoomName>)> =
        (world::hostiles::owned(&world.rooms).into_iter())
            .filter(|room| !declarations.abandoning(*room))
            .map(|room| (labs_in(world, room).count(), std::cmp::Reverse(room)))
            .filter(|(labs, _)| *labs > 0)
            .collect();
    labs.sort_by(|a, b| b.cmp(a));
    labs.into_iter().map(|(_, room)| room.0).collect()
}

pub fn labs_in<'a>(world: &'a World, room: RoomName) -> impl Iterator<Item = &'a world::Structure> {
    (world.rooms.iter())
        .filter(move |r| r.name == room)
        .flat_map(|r| world.rules.working(r))
        .filter(|s| s.kind == world::StructureType::Lab && s.owner.is_ours())
}

/// A load between a store and a lab is still the room's.
pub fn home_holds(world: &World, room: RoomName, compound: ResourceType) -> i32 {
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
        .map(|s| s.store.amount_of(compound))
        .chain(
            (world.creeps.iter())
                .filter(move |c| c.pos.room == room)
                .map(|c| c.store.amount_of(compound)),
        );

    stores.sum()
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

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Roles {
    pub reagents: (Id, Id),
    pub outputs: Vec<Id>,
}

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
