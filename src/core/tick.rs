/// A throw in one module costs that module this tick and nothing else
/// (`ARCHITECTURE.md` §2). The clock is passed in to keep `decide` deterministic.
use std::collections::{BTreeMap, BTreeSet};

use crate::core::HashMap;

use super::atlas::{Atlas, Mover, Target, Tree};
use super::engine;
use super::grid::{self, Footprint};
use super::intents::{Ask, CreepAct, Intent, LabAct, Rank, SpawnAct};
use super::mv;
use super::state::State;
use super::strategy::intel::Intel;
use super::tactics::{self, Tactic};
use super::tiles::{self, Tile};
use super::vocabulary::{BodyPart, Boost, Compound, Resource};
use super::world::{Creep, Owner, Pos, RoomName, Sightings, World};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Declared {
    pub id: String,
    pub pos: Pos,
}

/// What is known of a room before it is seen.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Outpost {
    pub room: RoomName,
    pub controller: Declared,
    pub sources: Vec<Declared>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TacticRule {
    pub rival: Option<String>,
    pub room: Option<RoomName>,
    pub pin: Option<String>,
    pub forbid: Vec<String>,
}

/// What the rules say of one engagement (`ARCHITECTURE.md` §6.3): the tactics
/// none lets it fight, and the last matching rule's pin.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Ruling {
    pub forbid: BTreeSet<Tactic>,
    pub pin: Option<Tactic>,
}

pub fn ruling(rules: &[TacticRule], room: RoomName, rivals: &BTreeSet<&str>) -> Ruling {
    let applies = |r: &TacticRule| {
        r.room.is_none_or(|x| x == room) && r.rival.as_deref().is_none_or(|x| rivals.contains(&x))
    };

    let mut out = Ruling::default();

    for rule in rules.iter().filter(|r| applies(r)) {
        out.forbid
            .extend(rule.forbid.iter().filter_map(|n| tactics::by_name(n)));

        if let Some(pin) = rule.pin.as_deref().and_then(tactics::by_name) {
            out.pin = Some(pin);
        }
    }

    out
}

/// A name no tactic has, or a pin those rules forbid, is dropped for the
/// operator (§7.2).
fn checked(rules: Vec<TacticRule>) -> (Vec<TacticRule>, Vec<String>) {
    let joins = |a: &TacticRule, b: &TacticRule| {
        fn same<T: PartialEq>(x: Option<T>, y: Option<T>) -> bool {
            x.zip(y).is_none_or(|(x, y)| x == y)
        }
        same(a.rival.as_ref(), b.rival.as_ref()) && same(a.room, b.room)
    };

    let mut kept = Vec::new();
    let mut refused = Vec::new();

    for rule in &rules {
        let unknown = rule
            .pin
            .iter()
            .chain(&rule.forbid)
            .find(|name| tactics::by_name(name).is_none());

        let pin = rule.pin.as_deref().and_then(tactics::by_name);

        let contradicts = rules
            .iter()
            .filter(|other| joins(rule, other))
            .flat_map(|other| other.forbid.iter().filter_map(|n| tactics::by_name(n)))
            .any(|forbidden| Some(forbidden) == pin);

        match (unknown, pin) {
            (Some(name), _) => refused.push(format!("tactics {name}")),
            (None, Some(pin)) if contradicts => {
                refused.push(format!("tactics pin {}", tactics::name(pin)));
            }
            _ => kept.push(rule.clone()),
        }
    }

    (kept, refused)
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declarations {
    /// The creep lifetimes of tower fire a colony's reserve holds.
    pub reserve_lifetimes: f64,
    pub outposts: Vec<Outpost>,
    /// Declared as an outpost is, so a claimer walks to one unseen.
    pub claims: Vec<Outpost>,
    /// The one colony whose labs react, and the stock of each compound it
    /// makes toward.
    pub labs: Option<(RoomName, BTreeMap<Compound, i32>)>,
    pub allies: BTreeSet<String>,
    pub enemies: BTreeSet<String>,
    /// The rival rooms whose towers and economy are razed.
    pub sieges: Vec<RoomName>,
    /// Our colonies being emptied before the owner unclaims them by hand.
    pub abandon: Vec<RoomName>,
    /// Rooms no fight body walks through on its way: a rival's towers.
    pub avoid: BTreeSet<RoomName>,
    /// The operator's tactic rules, per rival and room (§6.3).
    pub tactics: Vec<TacticRule>,
}

/// What `Declarations::refuse` dropped: the rooms the engine keeps a creep out
/// of, and the tactic rules no name answers to.
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct Refused {
    pub rooms: Vec<RoomName>,
    pub rules: Vec<String>,
}

impl Declarations {
    /// A room the engine never lets a creep into serves nothing, and a tactic
    /// rule no name answers to never applies; both dropped, for the operator.
    pub fn refuse(&mut self, closed: &BTreeSet<RoomName>) -> Refused {
        let mut refused = Refused::default();
        let mut open = |room: &RoomName| {
            let open = !closed.contains(room);
            if !open {
                refused.rooms.push(*room);
            }
            open
        };
        self.outposts.retain(|o| open(&o.room));
        self.claims.retain(|o| open(&o.room));
        self.sieges.retain(|room| open(room));

        let (kept, rules) = checked(std::mem::take(&mut self.tactics));
        self.tactics = kept;
        refused.rules = rules;

        refused
    }

    /// A declared siege's owner, as intel last saw it, is an enemy (owner's call).
    pub fn besieging(mut self, intel: &Intel) -> Declarations {
        let owners: Vec<String> = (self.sieges.iter())
            .filter_map(|room| intel.get(room)?.owner.clone())
            .collect();
        self.enemies.extend(owners);
        self
    }
}

pub fn undeclared() -> Declarations {
    Declarations {
        reserve_lifetimes: 2.0,
        outposts: Vec::new(),
        claims: Vec::new(),
        labs: None,
        allies: BTreeSet::new(),
        enemies: BTreeSet::new(),
        sieges: Vec::new(),
        abandon: Vec::new(),
        avoid: BTreeSet::new(),
        tactics: Vec::new(),
    }
}

pub struct Context<'a> {
    pub world: &'a World,
    pub atlas: &'a mut Atlas,
    pub declarations: &'a Declarations,
    pub sightings: &'a Sightings,
    pub intel: &'a Intel,
    /// The harness's trace is wanted (`Intent::Trace`); never live.
    pub trace: bool,
}

/// A body cast under this name is Fight's for its life, so no creep is in
/// both Fight and Economy (`ARCHITECTURE.md` §2).
pub const FIGHT_PREFIX: &str = "fight-";

pub fn fights(name: &str) -> bool {
    name.starts_with(FIGHT_PREFIX)
}

/// The route search needs a bound; nothing is worked further than this.
pub const REACH_HOPS: i32 = 3;

/// A declared siege's room stands further off than any room worked.
pub const SIEGE_HOPS: i32 = 10;

pub fn target_at(pos: &Pos, range: i32) -> Target {
    Target {
        room: pos.room,
        tiles: vec![tiles::of_pos(pos)],
        range,
        avoid: BTreeSet::new(),
        max_hops: REACH_HOPS,
    }
}

/// Ticks and crossings are not one unit. `trees` holds the homes' rooms'.
pub fn nearest<'t>(
    atlas: &mut Atlas,
    trees: &dyn Fn(RoomName) -> Option<&'t Tree>,
    homes: &[Pos],
    can: impl Fn(usize) -> bool,
    at: &Pos,
) -> Option<usize> {
    let walker = Mover {
        footprint: Footprint::One,
        factor: grid::loaded_factor(&[BodyPart::Move]),
    };

    let least =
        |atlas: &mut Atlas, measure: &dyn Fn(&mut Atlas, &Pos) -> Option<i32>| -> Option<usize> {
            let mut best: Option<usize> = None;
            let mut best_d = i32::MAX;
            for (i, home) in homes.iter().enumerate() {
                if can(i)
                    && let Some(d) = measure(atlas, home)
                    && d < best_d
                {
                    best = Some(i);
                    best_d = d;
                }
            }
            best
        };

    least(atlas, &|atlas, home| {
        let t = target_at(home, 1);
        match trees(home.room) {
            Some(tree) if home.room != at.room => atlas.ticks_on(tree, walker, &t, at),
            _ => atlas.ticks_to(walker, &t, at),
        }
    })
    .or_else(|| {
        least(atlas, &|_atlas, home| {
            (trees(home.room)?.crossings(at.room, home.room)).map(|n| Atlas::crossing_ticks() * n)
        })
    })
}

pub fn colonies(world: &World) -> Vec<RoomName> {
    world
        .rooms
        .iter()
        .filter(|r| {
            r.controller.as_ref().is_some_and(|c| c.owner.is_ours())
                && world.spawns.iter().any(|s| s.pos.room == r.name)
        })
        .map(|r| r.name)
        .collect()
}

/// Whether an outpost's work is open (`ARCHITECTURE.md` §13, M1b): a foe
/// stands down the work it can reach and no more.
fn open(context: &Context, owned: &BTreeSet<RoomName>, room: RoomName) -> bool {
    let Some(seen) = context.world.room(room) else {
        return super::world::sightings::unarmed(context.sightings, context.world.time, room);
    };

    let core = seen
        .structures
        .iter()
        .any(|s| s.kind.is_invader_core() && s.level > 0);
    let foes: Vec<Creep> = seen.hostiles().cloned().collect();
    let work: Vec<&Pos> = seen
        .sources
        .iter()
        .map(|s| &s.pos)
        .chain(
            seen.structures
                .iter()
                .filter(|s| s.kind.is_container())
                .map(|s| &s.pos),
        )
        .collect();

    !core
        && !foes.iter().any(|c| {
            // An Invader's raid only ever walks to the work, so it stands the
            // room down on sight; a player's only the work it can strike.
            c.owner == Owner::Invader
                || (super::world::hostiles::armed(owned, &foes, c)
                    && work
                        .iter()
                        .any(|at| c.pos.within(super::world::hostiles::reach(c), at)))
        })
}

/// The declared outposts a colony's spawn reaches and whose work no foe stands
/// down (`ARCHITECTURE.md` §13, M1b): a stood-down room is no ground.
pub fn worked(context: &Context) -> Vec<Outpost> {
    let owned = super::world::hostiles::owned(&context.world.rooms);
    let trees: Vec<(RoomName, Tree)> = colonies(context.world)
        .into_iter()
        .map(|home| {
            let tree = context.atlas.tree(&BTreeSet::new(), REACH_HOPS, home);
            (home, tree)
        })
        .collect();

    let raised = context.declarations.claims.iter().filter(|c| {
        context.world.room(c.room).is_some_and(|r| {
            r.controller.as_ref().is_some_and(|k| k.owner.is_ours())
                && !context.world.spawns.iter().any(|s| s.pos.room == c.room)
        })
    });

    context
        .declarations
        .outposts
        .iter()
        .chain(raised)
        .filter(|o| {
            open(context, &owned, o.room)
                && trees
                    .iter()
                    .any(|(home, tree)| home == &o.room || tree.contains(o.room))
        })
        .cloned()
        .collect()
}

pub type ModuleRun = fn(&mut Context, &State) -> Result<(Vec<Intent>, State), String>;

pub struct Module {
    pub name: &'static str,
    pub run: ModuleRun,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Outcome {
    Ran,
    Off,
    Faulted(String),
}

#[derive(Clone, Debug)]
pub struct Report {
    pub module: &'static str,
    pub cpu: f64,
    pub outcome: Outcome,
}

pub struct TickResult {
    pub intents: Vec<Intent>,
    pub state: State,
    pub reports: Vec<Report>,
}

fn by_room<T: Clone>(pairs: &[(RoomName, T)]) -> Vec<(RoomName, Vec<T>)> {
    let mut groups: HashMap<RoomName, Vec<T>> = HashMap::default();
    let mut rooms: Vec<RoomName> = Vec::new();
    for (room, value) in pairs {
        if !groups.contains_key(room) {
            groups.insert(*room, Vec::new());
            rooms.push(*room);
        }
        groups.get_mut(room).unwrap().push(value.clone());
    }
    rooms
        .into_iter()
        .map(|room| {
            let values = groups.remove(&room).unwrap();
            (room, values)
        })
        .collect()
}

/// Each room's spawn queue: floors, then the fight's asks, then the
/// economy's, on its idle spawns while the bank pays; a fight ask the bank
/// cannot pay yet holds the economy's behind it, one its capacity never pays
/// holds nothing.
pub fn queue(world: &World, asks: &[(RoomName, Ask)]) -> Vec<Intent> {
    let rank = |a: &Ask| -> i32 {
        match a.rank {
            Rank::Floor => 0,
            Rank::Fight => 1,
            Rank::Economy => 2,
        }
    };

    fn serve(
        idle: &[&super::world::Spawn],
        capacity: i32,
        bank: i32,
        held: bool,
        asks: &[Ask],
    ) -> Vec<Intent> {
        let mut out = Vec::new();
        let mut bank = bank;
        let mut held = held;
        let mut idle_at = 0;
        for a in asks {
            if idle_at >= idle.len() {
                break;
            }
            if held && a.rank.is_economy() {
                break;
            }
            if engine::body_cost(&a.body) <= bank {
                out.push(Intent::Spawn(
                    idle[idle_at].id.clone(),
                    SpawnAct::SpawnCreep(a.body.clone(), a.name.clone()),
                ));
                bank -= engine::body_cost(&a.body);
                idle_at += 1;
            } else {
                held = held || (a.rank.is_fight() && engine::body_cost(&a.body) <= capacity);
            }
        }
        out
    }

    let mut out = Vec::new();
    for (room, mut asks) in by_room(asks) {
        asks.sort_by_key(rank);
        if let Some(r) = world.room(room) {
            let idle: Vec<&super::world::Spawn> = world
                .spawns
                .iter()
                .filter(|s| s.pos.room == room && s.spawning.is_none())
                .collect();
            out.extend(serve(
                &idle,
                r.energy_capacity,
                r.energy_available,
                false,
                &asks,
            ));
        }
    }
    out
}

/// One arbitration per room over every module's wishes (§3's `Move`); a creep
/// of ours that wishes nothing holds its tile.
pub fn arbitrate(context: &Context, wishes: &[(RoomName, mv::Wish)]) -> Vec<Intent> {
    let mut wished: BTreeSet<String> = BTreeSet::new();
    let groups = by_room(wishes);
    let mut scratch = mv::arbitrate::scratch();
    let mut out = Vec::new();

    for (_, ws) in &groups {
        for w in ws {
            wished.insert(w.creep.clone());
        }
    }

    for (room, ws) in groups {
        // A wish onto a hostile's tile follows it: the engine moves the body
        // there only once the hostile steps off, so it holds its tile.
        let theirs: BTreeSet<Tile> = (context.world.room(room).into_iter())
            .flat_map(|r| r.hostiles().map(|c| tiles::of_pos(&c.pos)))
            .collect();
        let (glued, ws): (Vec<mv::Wish>, Vec<mv::Wish>) =
            (ws.into_iter()).partition(|w| w.head != w.at && theirs.contains(&w.head));
        out.extend(glued.iter().map(|w| {
            Intent::Creep(
                w.creep.clone(),
                CreepAct::Move(tiles::direction(w.at, w.head)),
            )
        }));
        let mut held: Vec<Tile> = context
            .world
            .creeps
            .iter()
            .filter(|c| c.pos.room == room && !wished.contains(&c.name))
            .map(|c| tiles::of_pos(&c.pos))
            .collect();
        held.extend(glued.iter().map(|w| w.at));
        let steps = mv::arbitrate::arbitrate_with(
            &mut scratch,
            |tile| context.atlas.passable(room, tile) || context.atlas.is_struck(room, tile),
            &held,
            &ws,
        );
        out.extend(
            steps
                .into_iter()
                .map(|(name, direction)| Intent::Creep(name, CreepAct::Move(direction))),
        );
    }
    out
}

/// The compound a cast name asks for, read off its last token: a body cast
/// with a compound walks to the lab before it takes its work.
pub fn boost_of_name(name: &str) -> Option<Compound> {
    match super::vocabulary::resource_of_name(name.rsplit('-').next()?) {
        Resource::Compound(compound) => Some(compound),
        _ => None,
    }
}

/// The lab a body asking for `compound` is boosted from: one with room for it
/// holding the most of it, ties to the lower id; a lab holding another mineral
/// only when none has room, its drain making room.
pub fn booster<'a>(
    world: &'a World,
    room: RoomName,
    compound: Compound,
) -> Option<&'a super::world::Structure> {
    let held = |s: &super::world::Structure| s.store.amount_of(&Resource::Compound(compound));
    let most = |a: &&'a super::world::Structure, b: &&'a super::world::Structure| {
        held(a).cmp(&held(b)).then_with(|| b.id.cmp(&a.id))
    };

    (labs_in(world, room))
        .filter(|l| holds_room_for(l, compound))
        .max_by(most)
        .or_else(|| labs_in(world, room).max_by(most))
}

fn labs_in(world: &World, room: RoomName) -> impl Iterator<Item = &super::world::Structure> {
    (world.rooms.iter())
        .filter(move |r| r.name == room)
        .flat_map(|r| r.structures.iter())
        .filter(|s| s.kind == super::world::Kind::Lab && s.owner.is_ours())
}

/// What the home holds of a compound and of energy, over the stores a haul
/// fills: its labs, terminal, storage and the loads on its bodies.
pub fn home_holds(world: &World, room: RoomName, compound: Compound) -> (i32, i32) {
    let held = |s: &super::world::Store| {
        (
            s.amount_of(&Resource::Compound(compound)),
            s.amount_of(&Resource::Energy),
        )
    };
    let stores = (world.rooms.iter())
        .filter(|r| r.name == room)
        .flat_map(|r| r.structures.iter())
        .filter(|s| {
            s.owner.is_ours() && (s.kind.is_lab() || s.kind.is_terminal() || s.kind.is_storage())
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

/// Whether the lab holds the compound or nothing: a lab holds one mineral, so
/// another one in it leaves the compound nowhere to land.
fn holds_room_for(lab: &super::world::Structure, compound: Compound) -> bool {
    let wanted = Resource::Compound(compound);
    (lab.store.held.iter())
        .all(|(r, n)| *n <= 0 || r == &wanted || !super::vocabulary::is_mineral(r))
}

/// Whether the lab can boost a part now: a boost spends one of each a part.
fn can_boost(lab: &super::world::Structure, compound: Compound) -> bool {
    lab.store.amount_of(&Resource::Compound(compound)) >= engine::BOOST_MINERAL_PER_PART
        && lab.store.amount_of(&Resource::Energy) >= engine::BOOST_ENERGY_PER_PART
}

/// The mineral and energy a body's unboosted parts ask of `compound`.
fn boost_need(creep: &super::world::Creep, compound: Compound) -> (i32, i32) {
    let name = super::vocabulary::resource_name(&Resource::Compound(compound));
    let parts = (creep.body.iter())
        .filter(|p| {
            p.boost == Boost::Unboosted
                && super::vocabulary::boost::of_compound(p.kind, &name) != Boost::Unboosted
        })
        .count() as i32;

    (
        parts * engine::BOOST_MINERAL_PER_PART,
        parts * engine::BOOST_ENERGY_PER_PART,
    )
}

/// What a body cast with a compound still asks of the lab it walks to: the
/// lab, the compound, and the mineral and energy its unboosted parts need.
pub struct Waiting<'a> {
    pub lab: &'a super::world::Structure,
    pub compound: Compound,
    pub mineral: i32,
    pub energy: i32,
}

/// The lab a body waits at this tick, and what its parts still ask of it: one
/// that has room for the compound, while the home holds what the parts ask and
/// can carry it there. Read afresh every tick, so a colony that cannot serve the
/// body costs it the compound and never its raid.
pub fn waiting<'a>(
    world: &'a World,
    home: RoomName,
    creep: &super::world::Creep,
) -> Option<Waiting<'a>> {
    let compound = boost_of_name(&creep.name)?;
    let (mineral, energy) = boost_need(creep, compound);
    let (held, fuel) = home_holds(world, home, compound);

    if mineral == 0 || held < mineral || fuel < energy {
        return None;
    }
    let lab = booster(world, home, compound)?;
    // A colony with nothing left to carry the load fights the body unboosted
    // rather than stand it at a lab nothing will fill.
    let carried = can_boost(lab, compound)
        || (world.creeps.iter()).any(|c| {
            c.owner.is_ours()
                && c.pos.room == home
                && c.name != creep.name
                && boost_of_name(&c.name).is_none()
                && c.store.free() > 0
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

/// A body whose cast asked for a compound and whose parts do not carry it yet
/// is held at the lab and boosted there: its module's acts and wish are
/// dropped for the walk, or the boost itself when it stands beside it.
fn boost_stop(context: &mut Context, intents: Vec<Intent>) -> Vec<Intent> {
    let Some((home, _)) = &context.declarations.labs else {
        return intents;
    };
    let home = *home;
    let mut stalls: Vec<(String, Pos, String, Compound)> = Vec::new();

    for creep in (context.world.creeps.iter()).filter(|c| c.owner.is_ours() && c.pos.room == home) {
        if let Some(w) = waiting(context.world, home, creep) {
            stalls.push((creep.name.clone(), w.lab.pos, w.lab.id.clone(), w.compound));
        }
    }

    if stalls.is_empty() {
        return intents;
    }

    let stalling: BTreeSet<&str> = stalls.iter().map(|(name, _, _, _)| name.as_str()).collect();
    let mut out: Vec<Intent> = (intents.into_iter())
        .filter(|i| {
            let actor = i.actor();
            i.is_queue() || !stalling.contains(actor.as_str())
        })
        .collect();

    let mut boostable: BTreeSet<&str> = BTreeSet::new();
    for lab in labs_in(context.world, home) {
        for compound in stalls.iter().map(|(_, _, _, c)| c) {
            if can_boost(lab, *compound) {
                boostable.insert(lab.id.as_str());
            }
        }
    }

    for (name, at, lab, _) in &stalls {
        let Some(creep) = context.world.creeps.iter().find(|c| &c.name == name) else {
            continue;
        };
        if creep.pos.within(1, at) && boostable.contains(lab.as_str()) {
            out.push(Intent::Lab(
                lab.clone(),
                LabAct::BoostCreep(creep.id.clone()),
            ));
            continue;
        }
        let mover = Mover {
            footprint: Footprint::One,
            factor: grid::loaded_factor(&super::world::body_kinds(creep)),
        };
        out.push(Intent::Wish(
            creep.pos.room,
            mv::plan::toward(context.atlas, creep, mover, &target_at(at, 1), 1),
        ));
    }

    out
}

fn settle(context: &mut Context, intents: Vec<Intent>) -> Vec<Intent> {
    let intents = boost_stop(context, intents);
    let mut asks = Vec::new();
    let mut wishes = Vec::new();
    for i in &intents {
        match i {
            Intent::Queue(room, ask) => asks.push((*room, ask.clone())),
            Intent::Wish(room, wish) => wishes.push((*room, wish.clone())),
            _ => (),
        }
    }
    let mut out = queue(context.world, &asks);
    out.extend(
        intents
            .into_iter()
            .filter(|i| !i.is_queue() && !i.is_wish()),
    );
    out.extend(arbitrate(context, &wishes));
    out
}

pub fn run(
    clock: impl Fn() -> f64,
    modules: &[Module],
    context: &mut Context,
    state: State,
) -> TickResult {
    let mut intents: Vec<Intent> = Vec::new();
    let mut state = state;
    let mut reports: Vec<Report> = Vec::new();

    for m in modules {
        if state.off.contains(m.name) {
            reports.push(Report {
                module: m.name,
                cpu: 0.0,
                outcome: Outcome::Off,
            });
            continue;
        }
        let start = clock();
        let (produced, next, outcome) = match (m.run)(context, &state) {
            Ok((produced, next)) => (produced, next, Outcome::Ran),
            Err(error) => (Vec::new(), state.clone(), Outcome::Faulted(error)),
        };
        reports.push(Report {
            module: m.name,
            cpu: clock() - start,
            outcome,
        });
        intents.extend(produced);
        state = next;
    }

    let start = clock();

    // A fault settling the fight's asks and wishes costs only the fight's.
    let is_fight = |i: &Intent| -> bool {
        match i {
            Intent::Queue(_, ask) => ask.rank.is_fight(),
            Intent::Wish(_, wish) => fights(&wish.creep),
            _ => false,
        }
    };

    let (settled, outcome) = if let Ok(settled) =
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
            settle(context, intents.clone())
        })) {
        (settled, Outcome::Ran)
    } else {
        let rest: Vec<Intent> = intents.iter().filter(|i| !is_fight(i)).cloned().collect();
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| settle(context, rest))) {
            Ok(settled) => (settled, Outcome::Faulted("settle".to_string())),
            Err(_) => (
                intents
                    .into_iter()
                    .filter(|i| !i.is_queue() && !i.is_wish())
                    .collect(),
                Outcome::Faulted("settle".to_string()),
            ),
        }
    };

    reports.push(Report {
        module: "settle",
        cpu: clock() - start,
        outcome,
    });

    TickResult {
        intents: settled,
        state,
        reports,
    }
}

/// The modules `Main` runs, in this order.
pub const MODULES: [Module; 4] = [
    Module {
        name: "strategy",
        run: super::strategy::decide,
    },
    Module {
        name: "fight",
        run: super::fight::decide,
    },
    Module {
        name: "economy",
        run: super::economy::decide,
    },
    Module {
        name: "layout",
        run: super::layout::decide,
    },
];

pub fn module_list() -> &'static [Module] {
    &MODULES
}

/// A `Target` for a creep's own tile, as `Strategy` and `Layout` build one.
pub fn target_of(pos: &Pos, range: i32) -> Target {
    target_at(pos, range)
}

pub type Effects = BTreeMap<String, String>;

#[cfg(test)]
mod tests {
    use super::*;

    fn rule(
        room: Option<&str>,
        rival: Option<&str>,
        pin: Option<&str>,
        forbid: &[&str],
    ) -> TacticRule {
        TacticRule {
            rival: rival.map(str::to_string),
            room: room.map(RoomName::named),
            pin: pin.map(str::to_string),
            forbid: forbid.iter().map(ToString::to_string).collect(),
        }
    }

    #[test]
    fn a_rule_matches_every_field_it_sets_and_the_last_pin_wins() {
        let rules = [
            rule(None, Some("rival"), Some("kite"), &[]),
            rule(Some("W1N1"), None, None, &["blob"]),
            rule(Some("W1N1"), Some("rival"), Some("hold"), &[]),
        ];
        let rivals: BTreeSet<&str> = ["rival"].into_iter().collect();

        let here = ruling(&rules, RoomName::named("W1N1"), &rivals);
        assert_eq!(here.pin, Some(Tactic::Hold));
        assert!(here.forbid.contains(&Tactic::BlobClose));

        let elsewhere = ruling(&rules, RoomName::named("W2N2"), &rivals);
        assert_eq!(elsewhere.pin, Some(Tactic::Kite));
        assert!(elsewhere.forbid.is_empty());

        let stranger: BTreeSet<&str> = ["other"].into_iter().collect();
        assert_eq!(ruling(&rules, RoomName::named("W1N1"), &stranger).pin, None);
    }

    #[test]
    fn a_rule_a_name_no_tactic_has_or_a_pin_a_rule_that_joins_it_forbids_never_applies() {
        let (kept, refused) = checked(vec![
            rule(None, None, None, &["bounce"]),
            rule(Some("W1N1"), None, Some("kite"), &["kite"]),
            rule(Some("W2N2"), None, Some("close"), &[]),
        ]);

        assert_eq!(kept.len(), 1);
        assert_eq!(refused.len(), 2);

        // A room-wide rule forbids what a rival's rule pins: neither names the other.
        let (kept, refused) = checked(vec![
            rule(Some("W1N1"), Some("rival"), Some("kite"), &[]),
            rule(Some("W1N1"), None, None, &["kite"]),
        ]);

        assert_eq!(kept.len(), 1);
        assert_eq!(refused.len(), 1);
    }
}
