/// A throw in one module costs that module this tick and nothing else
/// (`ARCHITECTURE.md` §2). The clock is passed in to keep `decide` deterministic.
use std::collections::{BTreeMap, BTreeSet};

use crate::core::HashMap;

use screeps::LocalRoomTerrain;

use super::atlas::{Atlas, Mover, Target, Tree};
use super::engine;
use super::grid::{self, Footprint};
use super::intents::{Ask, CreepAct, Intent, Rank, SpawnAct};
use super::mv;
use super::state::State;
use super::strategy::{
    self,
    intel::{self, Intel},
};
use super::tactics::{self, Tactic};
use super::tiles::{self, Tile, TileSet};
use super::vocabulary::{Compound, Part, Resource};
use super::world::{
    Creep, Kind, Owner, Pos, Room, RoomName, Sightings, Structure, World, hostiles,
};

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

#[derive(Clone, Debug, PartialEq, Eq, serde::Deserialize)]
pub struct TacticRule {
    pub rival: Option<String>,
    pub room: Option<RoomName>,
    pub pin: Option<String>,
    #[serde(default)]
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Stage {
    Exploit,
    /// Emptied, razed and unclaimed.
    Abandon,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ColonyPlan {
    pub room: RoomName,
    pub stage: Stage,
    pub target: i32,
    /// The stock its economy keeps before it spends a surplus.
    pub reserve: i32,
    pub rampart_hits: i32,
    pub ring: bool,
    pub owes: bool,
    /// Where its haul is banked while no storage stands (`ARCHITECTURE.md` §5).
    pub pile: Option<Pos>,
    /// Where its free fight bodies wait (`Plan::muster`).
    pub muster: Option<Pos>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Node {
    Colony(RoomName),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Flow {
    pub what: Resource,
    pub to: Node,
    pub stock: i32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declarations {
    /// The creep lifetimes of tower fire a colony's reserve holds.
    pub reserve_lifetimes: f64,
    pub outposts: Vec<Outpost>,
    /// Declared as an outpost is, so a claimer walks to one unseen.
    pub claims: Vec<Outpost>,
    pub allies: BTreeSet<String>,
    pub enemies: BTreeSet<String>,
    /// The rival rooms whose towers and economy are razed.
    pub sieges: Vec<RoomName>,
    pub colonies: Vec<ColonyPlan>,
    pub flows: Vec<Flow>,
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

    pub fn planned(
        mut self,
        (plans, outposts, claim): (Vec<ColonyPlan>, Vec<Outpost>, Option<Outpost>),
    ) -> Declarations {
        (self.claims).extend(claim.filter(|c| !self.claims.iter().any(|o| o.room == c.room)));
        let declared: BTreeSet<RoomName> = self.colonies.iter().map(|p| p.room).collect();
        (self.colonies).extend(plans.into_iter().filter(|p| !declared.contains(&p.room)));
        let declared: BTreeSet<RoomName> = self.outposts.iter().map(|o| o.room).collect();
        (self.outposts).extend(outposts.into_iter().filter(|o| !declared.contains(&o.room)));
        self
    }

    pub fn abandoning(&self, room: RoomName) -> bool {
        (self.colonies.iter()).any(|p| p.room == room && p.stage == Stage::Abandon)
    }

    pub fn labs(&self) -> Option<(RoomName, BTreeMap<Compound, i32>)> {
        let Node::Colony(home) = self.flows.first()?.to;
        let targets = (self.flows.iter())
            .filter_map(|f| match f.what {
                Resource::Compound(c) if f.to == Node::Colony(home) && f.stock > 0 => {
                    Some((c, f.stock))
                }
                _ => None,
            })
            .collect();
        Some((home, targets))
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
        allies: BTreeSet::new(),
        enemies: BTreeSet::new(),
        sieges: Vec::new(),
        colonies: Vec::new(),
        flows: Vec::new(),
        avoid: BTreeSet::new(),
        tactics: Vec::new(),
    }
}

pub struct Context<'a> {
    pub world: &'a World<'a>,
    pub atlas: &'a mut Atlas,
    pub declarations: &'a Declarations,
    pub sightings: &'a Sightings,
    pub intel: &'a Intel,
    pub derived: &'a Derived,
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

/// A free fight body holds within this of its colony's muster.
pub const MUSTER_RANGE: i32 = 2;

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
        factor: grid::loaded_factor(&[Part::Move]),
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

pub struct Derived {
    pub owned: BTreeSet<RoomName>,
    /// Every room GCL allows is owned.
    pub capped: bool,
    /// The owned rooms a spawn stands in, in `World.rooms`' order.
    pub colonies: Vec<RoomName>,
    /// The declared outposts and raised claims a colony reaches, whose work
    /// no foe stands down (`ARCHITECTURE.md` §13, M1b).
    pub worked: Vec<Outpost>,
    /// What a body outside a fight walks round, per room with any.
    pub danger: Vec<(RoomName, TileSet)>,
    pub trees: Vec<(RoomName, Tree)>,
}

impl Derived {
    pub fn danger(&self, room: RoomName) -> Option<&TileSet> {
        (self.danger.iter()).find_map(|(r, d)| (*r == room).then_some(d))
    }
}

/// A keeper's strikes the Atlas walls already; a tower strikes the whole
/// room, its reach taken where its fire is above its weakest. None in a room
/// we own: its refills keep the towers firing, and the strikes they take are
/// what calls the safe mode.
fn danger(world: &World, owned: &BTreeSet<RoomName>) -> Vec<(RoomName, TileSet)> {
    let reaches = |r: &Room| {
        let creeps = (r.hostiles())
            .filter(|c| c.owner != Owner::Keeper && hostiles::strikes(c))
            .map(|c| (tiles::of_pos(&c.pos), hostiles::reach(c)));
        let towers = (r.structures.iter())
            .filter(|s| {
                s.kind.is_tower()
                    && s.owner.is_theirs()
                    && !s.owner.is_ally()
                    && s.store.amount_of(&Resource::Energy) >= engine::TOWER_ENERGY_COST
            })
            .map(|s| (tiles::of_pos(&s.pos), engine::TOWER_FALLOFF_RANGE));
        creeps.chain(towers).collect::<Vec<_>>()
    };
    (world.rooms.iter())
        .filter(|r| !owned.contains(&r.name))
        .filter_map(|r| {
            let reaches = reaches(r);
            let mut set = TileSet::default();
            for (at, reach) in &reaches {
                tiles::within(*reach, *at).into_iter().for_each(|t| {
                    set.insert(t);
                });
            }
            (!reaches.is_empty()).then_some((r.name, set))
        })
        .collect()
}

pub fn derive(
    world: &World,
    atlas: &mut Atlas,
    declarations: &Declarations,
    sightings: &Sightings,
) -> Derived {
    let owned = hostiles::owned(&world.rooms);
    let spawned = |room: RoomName| world.spawns.iter().any(|s| s.pos.room == room);
    let colonies: Vec<RoomName> = (world.rooms.iter())
        .map(|r| r.name)
        .filter(|room| owned.contains(room) && spawned(*room))
        .collect();

    let trees: Vec<(RoomName, Tree)> = (colonies.iter())
        .map(|home| (*home, atlas.tree(&BTreeSet::new(), REACH_HOPS, *home)))
        .collect();
    let mut derived = Derived {
        danger: danger(world, &owned),
        capped: owned.len() as i32 >= world.rooms_allowed,
        owned,
        colonies,
        worked: Vec::new(),
        trees,
    };
    derived.worked = worked(world, sightings, declarations, &derived);
    derived
}

pub fn worked(
    world: &World,
    sightings: &Sightings,
    declarations: &Declarations,
    derived: &Derived,
) -> Vec<Outpost> {
    let owned = &derived.owned;
    let spawned = |room: RoomName| derived.colonies.contains(&room);
    let raised =
        (declarations.claims.iter()).filter(|c| owned.contains(&c.room) && !spawned(c.room));
    (declarations.outposts.iter())
        .chain(raised)
        .filter(|o| {
            open(world, sightings, owned, o.room)
                && (derived.trees.iter())
                    .any(|(home, tree)| home == &o.room || tree.contains(o.room))
        })
        .cloned()
        .collect()
}

/// Whether an outpost's work is open (`ARCHITECTURE.md` §13, M1b): a foe
/// stands down the work it can reach and no more.
fn open(world: &World, sightings: &Sightings, owned: &BTreeSet<RoomName>, room: RoomName) -> bool {
    let Some(seen) = world.room(room) else {
        return super::world::sightings::unarmed(sightings, world.time, room);
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
                || (hostiles::armed(owned, &foes, c)
                    && work.iter().any(|at| c.pos.within(hostiles::reach(c), at)))
        })
}

/// A module writes its rows of the state only once it cannot fault: a fault
/// leaves the state as it found it.
pub type ModuleRun = fn(&mut Context, &mut State) -> Result<Vec<Intent>, String>;

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

fn by_room<'a, T>(pairs: &[(RoomName, &'a T)]) -> Vec<(RoomName, Vec<&'a T>)> {
    let mut groups: HashMap<RoomName, Vec<&T>> = HashMap::default();
    let mut rooms: Vec<RoomName> = Vec::new();
    for &(room, value) in pairs {
        groups
            .entry(room)
            .or_insert_with(|| {
                rooms.push(room);
                Vec::new()
            })
            .push(value);
    }
    rooms
        .into_iter()
        .map(|room| {
            let values = groups.remove(&room).unwrap();
            (room, values)
        })
        .collect()
}

/// Hivemind's order: the bays, which their harvesters refill, first. None
/// without a bay, so the engine keeps its own.
fn drawn(r: &Room, spawn: &Pos) -> Vec<String> {
    let in_bay = Atlas::in_bay(r);
    let bay = |s: &&Structure| in_bay(&s.kind, &s.pos);
    let ours = (r.structures.iter()).filter(|s| s.owner.is_ours());
    let mut energy: Vec<&Structure> = ours
        .filter(|s| s.kind.is_spawn() || s.kind == Kind::Extension)
        .collect();
    if !energy.iter().any(bay) {
        return Vec::new();
    }
    energy.sort_by_key(|s| (!bay(s), !s.kind.is_spawn(), spawn.range(&s.pos)));
    energy.into_iter().map(|s| s.id.clone()).collect()
}

/// Each room's spawn queue: floors, then the fight's asks, then the
/// economy's, on its idle spawns while the bank pays; a fight ask the bank
/// cannot pay yet holds the economy's behind it, one its capacity never pays
/// holds nothing.
pub fn queue(world: &World, asks: &[(RoomName, &Ask)]) -> Vec<Intent> {
    let rank = |a: &Ask| match a.rank {
        Rank::Floor => 0,
        Rank::Fight => 1,
        Rank::Economy => 2,
    };

    let mut out = Vec::new();
    for (room, mut asks) in by_room(asks) {
        asks.sort_by_key(|a| rank(a));
        let Some(r) = world.room(room) else {
            continue;
        };
        let mut idle = (world.spawns.iter()).filter(|s| s.pos.room == room && s.spawning.is_none());
        let mut bank = r.energy_available;
        let mut held = false;
        for a in &asks {
            if held && a.rank.is_economy() {
                break;
            }
            let cost = engine::body_cost(&a.body);
            if cost > bank {
                held = held || (a.rank.is_fight() && cost <= r.energy_capacity);
                continue;
            }
            let Some(spawn) = idle.next() else {
                break;
            };
            out.push(Intent::Spawn(
                spawn.id.clone(),
                SpawnAct::SpawnCreep(a.body.clone(), a.name.clone(), drawn(r, &spawn.pos)),
            ));
            bank -= cost;
        }
    }
    out
}

/// One arbitration per room over every module's wishes (§3's `Move`); a creep
/// of ours that wishes nothing holds its tile.
pub fn arbitrate(context: &Context, wishes: &[(RoomName, &mv::Wish)]) -> Vec<Intent> {
    let mut wished: BTreeSet<&str> = BTreeSet::new();
    let groups = by_room(wishes);
    let mut scratch = mv::arbitrate::scratch();
    let mut out = Vec::new();

    for (_, ws) in &groups {
        for w in ws {
            wished.insert(&w.creep);
        }
    }

    for (room, ws) in groups {
        // A wish onto a hostile's tile follows it: the engine moves the body
        // there only once the hostile steps off, so it holds its tile.
        let theirs: BTreeSet<Tile> = (context.world.room(room).into_iter())
            .flat_map(|r| r.hostiles().map(|c| tiles::of_pos(&c.pos)))
            .collect();
        let (glued, ws): (Vec<&mv::Wish>, Vec<&mv::Wish>) =
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
            .filter(|c| c.pos.room == room && !wished.contains(c.name.as_str()))
            .map(|c| tiles::of_pos(&c.pos))
            .collect();
        held.extend(glued.iter().map(|w| w.at));
        let steps = mv::arbitrate::arbitrate_with(
            &mut scratch,
            |tile| context.atlas.passable(room, tile) || context.atlas.is_struck(room, tile),
            &held,
            &ws,
        );
        let stepped: BTreeSet<&str> = steps.iter().map(|(name, _)| name.as_str()).collect();
        out.extend(
            (ws.iter())
                .filter(|w| w.moves && !w.stays() && !stepped.contains(w.creep.as_str()))
                .map(|w| Intent::Held(w.creep.clone())),
        );
        out.extend(
            steps
                .into_iter()
                .map(|(name, direction)| Intent::Creep(name, CreepAct::Move(direction))),
        );
    }
    out
}

fn settle(context: &mut Context, intents: &[Intent]) -> Vec<Intent> {
    let intents = super::boost::stop(context, intents);
    let mut asks = Vec::new();
    let mut wishes = Vec::new();
    for i in intents.iter() {
        match i {
            Intent::Queue(room, ask) => asks.push((*room, ask)),
            Intent::Wish(room, wish) => wishes.push((*room, wish)),
            _ => (),
        }
    }
    let mut out = queue(context.world, &asks);
    out.extend(
        (intents.iter())
            .filter(|i| !i.is_queue() && !i.is_wish())
            .cloned(),
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
        let (produced, outcome) = match (m.run)(context, &mut state) {
            Ok(produced) => (produced, Outcome::Ran),
            Err(error) => (Vec::new(), Outcome::Faulted(error)),
        };
        reports.push(Report {
            module: m.name,
            cpu: clock() - start,
            outcome,
        });
        intents.extend(produced);
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
        std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| settle(context, &intents)))
    {
        (settled, Outcome::Ran)
    } else {
        let rest: Vec<Intent> = intents.iter().filter(|i| !is_fight(i)).cloned().collect();
        match std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| settle(context, &rest))) {
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

/// A tick decided: what the driver executes and writes, and what it keeps.
pub struct Decided {
    pub result: TickResult,
    pub declarations: Declarations,
    pub derived: Derived,
    pub refused: Refused,
    pub sightings: Sightings,
    pub atlas: Atlas,
}

/// From the world read to the intents settled. `began` is when the `world`
/// phase began, before the driver read it.
pub fn decide<'t>(
    (clock, began): (impl Fn() -> f64, f64),
    phases: &mut Vec<(String, f64)>,
    (world, trace): (&World, bool),
    (mut declarations, state): (Declarations, State),
    (sightings, atlas): (Sightings, Atlas),
    intel: &'t mut Intel,
    (terrain, terrains): (
        &dyn Fn(RoomName) -> Option<&'t LocalRoomTerrain>,
        impl IntoIterator<Item = (RoomName, &'t [u8; tiles::COUNT])>,
    ),
) -> Decided {
    let mut state = state;
    let refused = declarations.refuse(&world.closed);
    let sightings = super::world::sightings::step(world, sightings);
    intel::step(world, intel);
    phases.push(("world".to_string(), clock() - began));

    let start = clock();
    let seen = (intel.iter()).map(|(room, row)| (*room, row.lairs.as_deref(), row.stronghold));
    let mut atlas = Atlas::following(atlas, world, seen, terrains);
    phases.push(("atlas".to_string(), clock() - start));

    let start = clock();
    let mut derived = derive(world, &mut atlas, &declarations, &sightings);
    phases.push(("derive".to_string(), clock() - start));
    let plans = strategy::plan(
        world,
        &derived,
        &declarations,
        intel,
        (&state.engagements, &state.casts, &state.due),
        (&state.layout, &state.spare),
        (terrain, &mut state.expansion),
    );
    let declarations = declarations.planned(plans);
    derived.worked = worked(world, &sightings, &declarations, &derived);

    let mut context = Context {
        world,
        atlas: &mut atlas,
        declarations: &declarations,
        sightings: &sightings,
        intel,
        derived: &derived,
        trace,
    };
    let result = run(clock, &MODULES, &mut context, state);
    for report in &result.reports {
        phases.push((report.module.to_string(), report.cpu));
    }
    Decided {
        result,
        declarations,
        derived,
        refused,
        sightings,
        atlas,
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
