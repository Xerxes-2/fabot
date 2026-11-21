/// A colony's flows as slots: each slot casts one body and is the work that
/// body does, so what is cast and what is offered are one list.
use std::cell::RefCell;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Display;

use fabot_atlas::{self as atlas, Atlas, Mover, Target};
use fabot_base::HashMap;
use fabot_base::declarations::{self, Declarations};
use fabot_base::engine;
use fabot_base::grid::{self, Footprint};
use fabot_base::vocabulary::{Part, part_count_in};
use fabot_base::world::{Controller, Id, Kind, Pos, RoomName, Spawn};

use super::bodies::{self, Leg, Pattern};
use super::derived::{Body, Derived, Ground, SiteFact, dig_work, drawn_for};
use super::task::Assignment;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Job {
    Dig(Id),
    /// The colony's carriers: its sources' and outposts' dig home, the
    /// storage's stock to its sinks, and every ore load.
    Fill,
    /// Builds, repairs, and upgrades where no buffer stands; `true` builds
    /// before it upgrades.
    Work(bool),
    Upgrade,
    Reserve(Id),
    Claim(Id),
    Scout(RoomName),
}

impl Job {
    /// A seated job holds one body; extras of the others still work.
    pub fn seated(&self) -> bool {
        !matches!(self, Job::Fill | Job::Work(_))
    }

    pub fn pattern(&self) -> Pattern {
        match self {
            Job::Dig(_) => Pattern::Harvester,
            Job::Fill => Pattern::Hauler,
            Job::Work(_) => Pattern::Worker,
            Job::Upgrade => Pattern::Upgrader,
            Job::Reserve(_) | Job::Claim(_) => Pattern::Claimer,
            Job::Scout(_) => Pattern::Scout,
        }
    }
}

#[derive(Clone, Debug)]
pub struct Slot {
    pub id: String,
    pub colony: usize,
    pub room: RoomName,
    pub job: Job,
    pub seat: Pos,
    pub body: Vec<Part>,
    /// The slot whose body digs what this one carries.
    pub fed_by: Option<String>,
    /// Spends the surplus: cast only while the colony's digs stand.
    pub spends: bool,
    pub rank: i32,
    /// Its outpost is stood down: its body keeps it, and waits at home.
    pub closed: bool,
    pub home_walk: i32,
}

impl Slot {
    pub fn new(
        ci: usize,
        own: bool,
        job: Job,
        seat: Pos,
        key: impl Display,
        k: usize,
        body: Vec<Part>,
    ) -> Slot {
        let pattern: &str = job.pattern().into();
        Slot {
            id: format!("{pattern}:{key}:{k}"),
            colony: ci,
            room: seat.room,
            rank: rank_of(&job, own),
            job,
            seat,
            body,
            fed_by: None,
            spends: false,
            closed: false,
            home_walk: 0,
        }
    }

    /// A lost controller loses the room; a lost road costs more than its repair.
    pub fn draws_reserve(&self) -> bool {
        matches!((self.rank, &self.job), (DEADLINE, _) | (MEND, Job::Work(_)))
    }

    pub fn digs(&self) -> bool {
        self.rank == CREW
    }
}

/// The ticks from `a` to `b`: in one room its range, else its crossings off
/// a tree `Derived` grew from either end, a search only without one.
pub fn distance(atlas: &Atlas, derived: &Derived, a: &Pos, b: &Pos) -> Option<i32> {
    if let Some(r) = a.range(b) {
        return Some(r);
    }
    let tree = (derived.route(b.room)).or_else(|| derived.route(a.room));
    let crossed = match tree {
        Some(tree) => tree.crossings(a.room, b.room),
        None => (atlas.route(&BTreeSet::new(), declarations::REACH_HOPS, a.room, b.room))
            .map(|r| r.len() as i32 - 1),
    };
    crossed.map(|n| Atlas::CROSSING_TICKS * n)
}

pub type PairTicks = RefCell<HashMap<(RoomName, RoomName), Option<i32>>>;

pub fn distance_memo(
    atlas: &Atlas,
    derived: &Derived,
    kept: &PairTicks,
    a: &Pos,
    b: &Pos,
) -> Option<i32> {
    if a.room == b.room {
        return a.range(b);
    }
    *(kept.borrow_mut().entry((a.room, b.room))).or_insert_with(|| distance(atlas, derived, a, b))
}

pub fn ticks_to(
    atlas: &Atlas,
    derived: &Derived,
    mover: Mover,
    target: &Target,
    from: &Pos,
) -> Option<i32> {
    match derived.route(target.room) {
        Some(tree)
            if from.room != target.room
                && target.avoid.is_empty()
                && target.max_hops == declarations::REACH_HOPS =>
        {
            atlas.ticks_on(tree, mover, target, from)
        }
        _ => atlas.ticks_to(mover, target, from),
    }
}

fn walk(atlas: &Atlas, derived: &Derived, parts: &[Part], from: &Pos, to: &Pos) -> Option<i32> {
    let mover = Mover {
        footprint: Footprint::One,
        factor: grid::loaded_factor(parts),
    };
    ticks_to(atlas, derived, mover, &atlas::target_at(to, 1), from)
}

/// A room out of sight floods nothing: its walk is its crossings, unroaded.
fn leg(atlas: &Atlas, derived: &Derived, from: &Pos, to: &Pos) -> Option<Leg> {
    use Part::{Carry, Move};
    let walked = || {
        Some(Leg {
            even: walk(atlas, derived, &[Carry, Move], from, to)?,
            roaded: walk(atlas, derived, &[Carry, Carry, Move], from, to)?,
            empty: walk(atlas, derived, &[Move], to, from)?,
        })
    };
    walked().or_else(|| {
        let d = distance(atlas, derived, from, to)?;
        Some(Leg {
            even: d,
            roaded: 2 * d,
            empty: d,
        })
    })
}

/// The haulers that carry each flow, a rate a tick over its leg: one body
/// shape for them all, chosen on their walks weighted by what each carries.
fn haulers(flows: &[(f64, Leg)], capacity: i32) -> Vec<Vec<Part>> {
    let weighed = |at: fn(&Leg) -> i32| {
        let sum: f64 = flows
            .iter()
            .map(|(rate, leg)| rate * f64::from(at(leg)))
            .sum();
        sum.round() as i32
    };
    let ratio = bodies::carry_per_move(Leg {
        even: weighed(|l| l.even),
        roaded: weighed(|l| l.roaded),
        empty: weighed(|l| l.empty),
    });
    let ticks: f64 = (flows.iter())
        .map(|(rate, leg)| {
            let loaded = if ratio == 2 { leg.roaded } else { leg.even };
            rate * f64::from(2 + loaded + leg.empty)
        })
        .sum();
    let carry = (ticks / f64::from(engine::CARRY_PART_CAPACITY)).ceil() as i32;
    let Some(most) = bodies::hauler(ratio, engine::MAX_BODY_PARTS as i32, capacity) else {
        return Vec::new();
    };
    // Bodies are sized to the bank: the share over the flow carries what a
    // body loses waiting on a container or a sink.
    let per = part_count_in(&most, Part::Carry).max(1);
    vec![most; ((carry + per - 1) / per) as usize]
}

fn sponsor(atlas: &mut Atlas, derived: &Derived, at: &Pos) -> Option<usize> {
    let given_up = |room| (derived.grounds.iter()).any(|g| g.room == room && g.abandoned);
    atlas::sponsor(atlas, derived.ours, &given_up, at)
}

/// Per colony, the colony that sends its pioneers while it is young.
fn sponsors(atlas: &mut Atlas, derived: &Derived) -> Vec<Option<usize>> {
    (derived.colonies.iter().enumerate())
        .map(|(cj, c)| {
            let g = &derived.grounds[cj];
            let young = c.controller.level <= STORAGE_LEVEL && !g.has_storage() && !g.abandoned;
            young.then(|| sponsor(atlas, derived, &c.controller.pos))?
        })
        .collect()
}

fn server(
    atlas: &mut Atlas,
    (derived, declarations): (&Derived, &Declarations),
    at: (&Pos, bool),
) -> Option<usize> {
    let planned = |room| (declarations.colonies.iter()).find(|p| p.room == room);
    atlas::server(atlas, derived.ours, &planned, at)
}

pub fn owners(atlas: &mut Atlas, of: (&Derived, &Declarations)) -> Vec<Option<usize>> {
    let derived = of.0;
    (derived.grounds.iter().enumerate())
        .map(|(gi, ground)| {
            if gi < derived.colonies.len() {
                return Some(gi);
            }
            let claim = (ground.controller.as_ref()).is_some_and(|c| c.owner.is_ours());
            let at = (ground.controller.as_ref().map(|c| c.pos))
                .or_else(|| ground.sources.first().map(|s| s.pos))?;
            server(atlas, of, (&at, claim))
        })
        .collect()
}

/// The engine drops what a part adds at the cap; one claim life short of it
/// outlasts the successor's walk.
pub fn claim_parts_for(held: i32) -> i32 {
    let ceil = |n: i32, d: i32| (n + d - 1) / d;
    let short = 0.max(engine::RESERVATION_CAP - held - engine::CLAIM_LIFETIME);
    1 + ceil(
        ceil(short, engine::CLAIM_LIFETIME),
        engine::RESERVE_PER_CLAIM,
    )
}

fn upkeep(body: &[Part]) -> f64 {
    f64::from(engine::body_cost(body)) / f64::from(engine::lifetime(body.iter().copied()))
}

/// Over `h` ticks a pile loses `h / 2D` of itself to decay and the bodies cast
/// to spend it idle a life less `h` at `per` an energy: least at √(2·D·per·life).
fn drain(per: f64) -> f64 {
    let life = f64::from(engine::CREEP_LIFETIME);
    (2.0 * f64::from(engine::ENERGY_DECAY) * per * life)
        .sqrt()
        .min(life)
}

/// A body that digs and holds on a site builds it with its dig (`run::raising`).
pub fn raised_by<'a>(g: &'a Ground, b: &Body) -> Option<&'a SiteFact> {
    (g.sites.iter()).find(|s| b.work > 0 && b.carry > 0 && s.pos == b.pos)
}

/// Half the timer: a level-up lands inside it, and safe mode's grace stays banked.
pub fn inside_deadline(c: &Controller) -> bool {
    c.level > 0 && c.ticks_to_downgrade <= engine::controller_downgrade(c.level) / 2
}

/// Owner, 2026-11-13: past the last level upgrading only feeds GCL, so the
/// controller is fed only to keep its level.
pub fn fed(c: &Controller) -> bool {
    !engine::topped_out(c.level) || inside_deadline(c)
}

/// An upgrader brought its loads waits by the controller's feed (`jobs::supply`).
fn upgrade_pace(atlas: &Atlas, derived: &Derived, ci: usize, body: &[Part]) -> f64 {
    let power = engine::UPGRADE_DRAIN_PER_WORK;
    let g = &derived.grounds[ci];
    if g.banks_on_pile() && g.feed.is_some() {
        f64::from(power)
    } else {
        worker_pace(atlas, derived, ci, body, power)
    }
}

/// What a WORK of a worker spends a tick at `power` a WORK while working:
/// its load lasts so long, then it walks to its energy and back.
fn worker_pace(atlas: &Atlas, derived: &Derived, ci: usize, body: &[Part], power: i32) -> f64 {
    let (colony, ground) = (&derived.colonies[ci], &derived.grounds[ci]);
    let work = part_count_in(body, Part::Work).max(1);
    let fetches: Vec<(Pos, i32)> = match (&ground.buffer, ground.bank()) {
        (Some(b), _) => vec![(b.pos, 1)],
        (None, Some(s)) => vec![(s.pos, 1)],
        (None, None) => (ground.sources.iter())
            .map(|s| (s.post.unwrap_or(s.pos), s.dug.max(1)))
            .collect(),
    };
    let empty: Vec<Part> = body.iter().copied().filter(|p| *p != Part::Carry).collect();
    let at = &colony.controller.pos;
    let (mut weighted, mut weights) = (0, 0);
    for (fetch, w) in &fetches {
        if let (Some(there), Some(back)) = (
            walk(atlas, derived, body, fetch, at),
            walk(atlas, derived, &empty, fetch, at),
        ) {
            weighted += w * (there + back);
            weights += w;
        }
    }
    let load = f64::from(part_count_in(body, Part::Carry) * engine::CARRY_PART_CAPACITY);
    let working = load / f64::from(work * power);
    let trip = if weights == 0 {
        f64::from(engine::CREEP_LIFETIME)
    } else {
        f64::from(weighted) / f64::from(weights)
    };
    f64::from(power) * working / (working + trip)
}

/// Bodies' upkeep, all and home's own steady share: `spare` reads only the
/// latter, or each outpost chosen would move the price it was chosen at.
#[derive(Default)]
struct Upkeep {
    all: f64,
    own: f64,
}

impl Upkeep {
    fn add(&mut self, all: f64, own: f64) {
        self.all += all;
        self.own += own;
    }
}

/// Carried flows, all and home's own steady share, split as `Upkeep` is.
#[derive(Default)]
struct Flows {
    all: Vec<(f64, Leg)>,
    own: Vec<(f64, Leg)>,
}

impl Flows {
    fn push(&mut self, all: f64, own: f64, leg: Leg) {
        self.all.push((all, leg));
        self.own.push((own, leg));
    }
}

struct Plan<'a> {
    atlas: &'a Atlas,
    derived: &'a Derived<'a>,
    held: &'a BTreeMap<String, Assignment>,
    out: Vec<Slot>,
    spare: BTreeMap<RoomName, f64>,
}

impl Plan<'_> {
    fn push(
        &mut self,
        ci: usize,
        job: Job,
        seat: Pos,
        key: impl Display,
        k: usize,
        body: Vec<Part>,
    ) -> &mut Slot {
        let own = seat.room == self.derived.colonies[ci].room;
        self.out.push(Slot::new(ci, own, job, seat, key, k, body));
        let pushed = self.out.len() - 1;
        &mut self.out[pushed]
    }

    fn colony(&mut self, ci: usize, owners: &[Option<usize>], sponsors: &[Option<usize>]) {
        let (atlas, derived) = (self.atlas, self.derived);
        let (colony, ground) = (&derived.colonies[ci], &derived.grounds[ci]);
        let capacity = colony.bank_capacity;
        let life = f64::from(engine::CREEP_LIFETIME);
        let storage = ground.stores.iter().find(|s| s.owned);
        let bank = ground.bank();
        let home = colony.spawns[0].pos;
        let drop = bank.map_or(home, |s| s.pos);

        let worker = bodies::sized(Pattern::Worker, MOST, capacity);
        if ground.abandoned {
            let mut own = Vec::new();
            if !ground.shipment.is_empty() {
                let body = bodies::hauler(2, engine::MAX_BODY_PARTS as i32, capacity);
                let body = body.unwrap_or_default();
                own.push(body.clone());
                self.push(ci, Job::Fill, drop, colony.room, 0, body);
            }
            if inside_deadline(&colony.controller) {
                own.extend(worker.clone());
                self.worker(ci, colony.room, 0, (false, false), FLOOR, MOST);
            }
            self.spare(ci, &own);
            return;
        }

        let mut fixed = Upkeep::default();
        let mut dug = 0.0;
        let mut far_dug = 0.0;
        let mut owed = 0;
        let mut payback = 0;
        let mut repairs = false;
        let mut mends = false;
        let mut crews = 0;
        let mut digs: Vec<String> = Vec::new();
        let mut local = Flows::default();
        let mut remote: Vec<(f64, Leg)> = Vec::new();
        let mut far: Vec<String> = Vec::new();
        let fed = fed(&colony.controller);
        let mut drink: i32 = (derived.bodies.iter().chain(derived.ovens.iter()))
            .filter(|b| b.pos.room == colony.room && b.work > 0 && b.carry > 0)
            .filter(|b| b.pattern != Pattern::Harvester)
            .map(|b| b.work * engine::UPGRADE_DRAIN_PER_WORK)
            .sum::<i32>()
            .min(bodies::upgradable_at(colony.controller.level));

        for (gi, g) in derived.grounds.iter().enumerate() {
            if owners[gi] != Some(ci) {
                continue;
            }
            let own = gi == ci;
            let raised = !own && g.controller.as_ref().is_some_and(|c| c.owner.is_ours());
            // Owner, 2026-11-12: the tower is rushed, so an outpost's sites wait for it.
            if own || raised || colony.controller.level >= TOWER_LEVEL {
                owed += g.sites.iter().map(|s| s.left).sum::<i32>();
                payback += (g.sites.iter())
                    .filter(|s| drawn_for(&s.kind))
                    .map(|s| s.left)
                    .sum::<i32>();
            }
            repairs |= !g.repairs.is_empty();
            let haul = derived.haul(owners, gi);
            let at_risk = g.repairs.iter().any(|r| r.rebuild > 0 && r.at_risk(haul));
            mends |= own && at_risk;
            if raised {
                // A claim without its spawn is raised by workers sent from here.
                for k in 0..PIONEERS {
                    let cost = self.worker(ci, g.room, k, (true, false), PIONEER, MOST);
                    fixed.add(cost, cost);
                }
                continue;
            }
            // Hivemind's mine builder.
            let paves = g.sites.iter().any(|s| s.kind == Kind::Road);
            if !own && (at_risk || paves) && storage.is_some() {
                let mender = self.worker(ci, g.room, 0, (false, false), MENDER, MENDER_WORK);
                fixed.add(mender, 0.0);
            }
            for s in &g.sources {
                let body = if own {
                    bodies::home_harvester(dig_work(s.rate), capacity)
                } else {
                    bodies::sized(Pattern::Harvester, dig_work(s.rate), capacity)
                };
                // An outpost out of sight shows no post: its harvester walks
                // to the source, where the post comes into view.
                let body = body.filter(|_| s.post.is_some() || !own);
                let rate = body.as_ref().map_or(0, |b| {
                    s.rate
                        .min(part_count_in(b, Part::Work) * engine::HARVEST_PER_WORK)
                });
                // A crew digs what the post's harvester leaves of a source
                // whose container does not stand, and builds it, a worker
                // digging a third of its life; not knowing its seats is no crew.
                if s.container.is_none() && own {
                    let seats = s.seats.map_or(0, |n| n - i32::from(s.post.is_some()));
                    let crew = worker.as_ref().map_or(1, |w| {
                        part_count_in(w, Part::Work) * engine::HARVEST_PER_WORK / 3
                    });
                    crews += seats.min((s.rate - rate + crew - 1) / crew.max(1));
                }
                let Some(body) = body else { continue };
                let post = s.post.unwrap_or(s.pos);
                let cost = upkeep(&body);
                fixed.add(cost, if own { cost } else { 0.0 });
                // No store holds an outpost's dig raising its site. At home it still
                // sizes the spend: those workers dig the free seats themselves.
                let raises = !own
                    && (derived.bodies.iter()).any(|b| {
                        b.pos == post
                            && b.pattern == Pattern::Harvester
                            && raised_by(g, b).is_some()
                    });
                let dig_id = format!("harvester:{}:0", s.id);
                if own {
                    digs.push(dig_id.clone());
                }
                self.push(ci, Job::Dig(s.id), post, s.id, 0, body);
                if raises {
                    continue;
                }
                dug += f64::from(rate);
                if !own {
                    far_dug += f64::from(rate);
                }
                // A source link sends a hub what the controller's link has no room for
                // (`run::link_send`); alone, that link drinks it all below the last level.
                let hub = g.links.iter().any(|l| l.role.is_hub());
                let (linked, steady) = match (&s.link, &g.receiver) {
                    (Some(_), _) if hub => (rate, 0),
                    (Some(_), Some(_)) => (
                        rate.min(drink),
                        rate * i32::from(engine::topped_out(colony.controller.level)),
                    ),
                    _ => (0, rate),
                };
                drink -= linked;
                let rate = rate - linked;
                if rate.max(steady) <= 0 {
                    continue;
                }
                // A backlog is carried off within the ticks its source fills a
                // container, or the dig overflows; never faster than the dig, so
                // the haulers stay bounded by what the source yields.
                let waiting: i32 = (g.stores.iter())
                    .filter(|st| s.container.is_some() && s.container == st.id.object())
                    .map(|st| st.energy)
                    .chain(
                        g.piles
                            .iter()
                            .filter(|p| p.pos.within(1, &post))
                            .map(|p| p.amount),
                    )
                    .sum();
                let fills = engine::CONTAINER_CAPACITY / engine::HELD_OUTPUT_PER_TICK;
                let backlog = (waiting / fills).min(rate);
                let to =
                    bank.map_or_else(|| ground.buffer.as_ref().map_or(home, |b| b.pos), |s| s.pos);
                let Some(leg) = leg(atlas, derived, &post, &to) else {
                    continue;
                };
                if own {
                    local.push(f64::from(rate + backlog), f64::from(steady), leg);
                } else {
                    remote.push((f64::from(rate + backlog), leg));
                    far.push(dig_id.clone());
                }
            }
            match &g.controller {
                Some(c) if !own && g.reservable && !claimed(derived, c.id) => {
                    let parts = claim_parts_for(g.our_reservation);
                    if let Some(body) =
                        bodies::sized(Pattern::Claimer, parts, capacity).filter(|_| {
                            capacity >= engine::body_cost(bodies::block_of(Pattern::Claimer))
                        })
                    {
                        fixed.add(upkeep(&body), 0.0);
                        self.push(ci, Job::Reserve(c.id), c.pos, c.id, 0, body);
                    }
                }
                _ => (),
            }
        }

        for cj in (0..sponsors.len()).filter(|cj| sponsors[*cj] == Some(ci)) {
            // Past its tower's level they build: upgrading, they drank what its builders draw.
            let builds = derived.colonies[cj].controller.level >= TOWER_LEVEL;
            for k in 0..PIONEERS {
                let room = derived.colonies[cj].room;
                let cost = self.worker(ci, room, k, (builds, false), PIONEER, MOST);
                fixed.add(cost, cost);
            }
        }

        let mut mined = 0.0;
        for m in &ground.minerals {
            let Some(post) = m.post.filter(|_| m.container.is_some() && m.amount > 0) else {
                continue;
            };
            if let Some(body) = bodies::sized(Pattern::Harvester, bodies::MINER_WORK, capacity) {
                let cost = upkeep(&body);
                fixed.add(cost, cost);
                mined +=
                    f64::from(part_count_in(&body, Part::Work) * engine::MINERAL_HARVEST_PER_WORK)
                        / f64::from(engine::MINERAL_HARVEST_CYCLE);
                self.push(ci, Job::Dig(m.id), post, m.id, 0, body).rank = MINE;
            }
        }

        let kept = f64::from(colony.reserve);
        // Without a storage what waits in the containers is the stock, spent
        // within the ticks a source fills one, or it overflows.
        let held: i32 = (ground.stores.iter())
            .filter(|s| s.source_dug > 0 || s.kind.is_buffer())
            .map(|s| s.energy)
            .sum();
        let fills = f64::from(engine::CONTAINER_CAPACITY / engine::HELD_OUTPUT_PER_TICK);
        let banked = match (ground.stock(), bank) {
            (Some(stock), _) => (f64::from(stock) - kept) / life,
            (None, Some(pile)) => {
                let pace = |w: &Vec<Part>| {
                    let work = f64::from(part_count_in(w, Part::Work));
                    upkeep(w) / work / upgrade_pace(atlas, derived, ci, w)
                };
                let piled = f64::from(pile.energy);
                let decay = (piled / f64::from(engine::ENERGY_DECAY)).ceil();
                piled / drain(worker.as_ref().map_or(1.0, pace)) - decay
            }
            (None, None) => 0.0,
        };
        let stocked = banked
            + if storage.is_none() {
                f64::from(held) / fills
            } else {
                0.0
            };
        // A worker draws only a full load past the reserve from one store
        // (`jobs::fuel`), a payback site's builder past none: short of it the
        // surplus is the hand-fed upgraders'.
        // With no reserve kept the containers and piles are a worker's too.
        let load = worker.as_deref().map_or(0, |w| {
            part_count_in(w, Part::Carry) * engine::CARRY_PART_CAPACITY
        });
        let drawn = |kept: i32| {
            storage.is_none()
                || kept <= 0
                || (ground.stores.iter()).any(|s| s.owned && s.spare(kept) >= load)
        };
        let past_reserve = drawn(colony.reserve);
        let draws = past_reserve || drawn(ground.kept_from_builders(colony.reserve));
        let owed = if past_reserve { owed } else { payback };
        let floor = match () {
            _ if !draws => i32::from(inside_deadline(&colony.controller) || mends),
            _ if owed > 0 || repairs => 2,
            _ => 1,
        };
        let worker_upkeep = worker.as_deref().map_or(0.0, upkeep);
        fixed.add(
            f64::from(floor + crews) * worker_upkeep,
            f64::from(KEPT_FLOOR + crews) * worker_upkeep,
        );
        let carried: Vec<(f64, Leg)> = local.all.iter().chain(&remote).copied().collect();
        let shapes = haulers(&carried, capacity);
        fixed.add(shapes.iter().map(|b| upkeep(b)).sum(), 0.0);
        let Upkeep {
            all: fixed,
            own: own_upkeep,
        } = fixed;
        // With no reserve to draw down, spend sized to dig no carrier brings
        // home drinks the bank that would cast those carriers.
        let carry = |flows: &[(f64, Leg)]| {
            (haulers(flows, capacity).iter())
                .map(|b| part_count_in(b, Part::Carry))
                .sum::<i32>()
        };
        let outposts = far_id(colony.room);
        let shape = shapes.into_iter().next().unwrap_or_default();
        let far_digs = (self.out.iter()).filter(|s| far.contains(&s.id));
        let trip = round_trip(atlas, derived, ci, &shape, far_digs);
        // An oven carries nothing yet.
        let standing: i32 = (derived.bodies.iter())
            .filter(|b| (self.held.get(&b.name)).is_some_and(|a| a.slot.starts_with(&outposts)))
            .filter(|b| serves_far(b, trip))
            .map(|b| b.carry)
            .sum();
        let hauled = f64::from(standing) / f64::from(carry(&remote).max(1));
        let dug = match storage {
            Some(_) => dug,
            None => dug - far_dug * (1.0 - hauled.clamp(0.0, 1.0)),
        };
        // Negative below the reserve, so no spend is written below it.
        let surplus = (dug - fixed + stocked).max(0.0);
        let upgradable = bodies::upgradable_at(colony.controller.level);
        // A swamp is five plains at either pace, so two paces part road from off it.
        let upgrader = (ground.buffer.as_ref())
            .and_then(|b| {
                let even = walk(atlas, derived, &[Part::Carry, Part::Move], &home, &b.pos)?;
                let roaded = walk(
                    atlas,
                    derived,
                    &[Part::Carry, Part::Carry, Part::Move],
                    &home,
                    &b.pos,
                )?;
                bodies::upgrader(2 * even - roaded, roaded - even, upgradable, capacity)
            })
            .filter(|b| {
                !ground.buffer_seats.is_empty()
                    && bodies::standing(part_count_in(b, Part::Work), part_count_in(b, Part::Carry))
            });
        // A structure pays only once it stands, so a level is built out before
        // it is upgraded; a road backlog never stops the controller.
        let works = match () {
            _ if !draws => 0.0,
            _ if !past_reserve => f64::from(payback) / life,
            _ if !fed => surplus,
            _ if upgradable < i32::MAX => (surplus - f64::from(upgradable)).max(0.0),
            _ if payback > 0 => surplus,
            _ if storage.is_none() => ((dug - fixed) / 2.0).clamp(0.0, surplus),
            _ => stocked.clamp(0.0, surplus),
        };
        let build = works.min(f64::from(owed) / life);
        let mended: f64 = (derived.grounds.iter().enumerate())
            .filter(|(gi, _)| owners[*gi] == Some(ci))
            .flat_map(|(gi, g)| {
                let haul = if gi == ci {
                    derived.haul(owners, gi)
                } else {
                    0
                };
                (g.repairs.iter())
                    .map(move |r| r.loss(haul) + f64::from(r.hits_max - r.hits) / life)
            })
            .sum::<f64>()
            / f64::from(engine::REPAIR_HITS_PER_ENERGY);
        let mend = (works - build).max(0.0).min(mended);

        let mut drunk = 0.0;
        let mut upgraded = if fed { surplus - build - mend } else { 0.0 };
        if let Some(body) = &upgrader {
            let work = part_count_in(body, Part::Work);
            let most =
                (ground.buffer_seats.len() as i32).min(upgradable.saturating_add(work - 1) / work);
            // A standing upgrader's body is paid for: only its drink is owed.
            let standing = (derived.bodies.iter().chain(derived.ovens.iter()))
                .filter(|b| b.pattern == Pattern::Upgrader && b.pos.room == colony.room)
                .count() as i32;
            let kept = ((upgraded / f64::from(work)).round() as i32).min(standing);
            let new =
                ((upgraded - f64::from(kept * work)) / (f64::from(work) + upkeep(body))).max(0.0);
            let n = (kept + new.floor() as i32).clamp(0, most);
            for (k, &seat) in ground.buffer_seats.iter().enumerate().take(n as usize) {
                let slot = self.push(ci, Job::Upgrade, seat, colony.room, k, body.clone());
                slot.spends = true;
                if banked > 0.0 {
                    slot.rank = STOCKED;
                }
            }
            drunk = f64::from(n * work.min(upgradable));
            upgraded = 0.0;
        }

        let upgraded = upgraded.min(f64::from(upgradable) - drunk).max(0.0);
        let (builders, upgraders) = worker.as_ref().map_or((0, 0), |body| {
            let work = f64::from(part_count_in(body, Part::Work).max(1));
            let per = upkeep(body) / work;
            let bodies = |spend: f64, pace: f64| (spend / (pace + per) / work).ceil() as i32;
            let pace = |power: i32| worker_pace(atlas, derived, ci, body, power);
            (
                bodies(build, pace(engine::BUILD_POWER))
                    + bodies(
                        mend,
                        pace(engine::REPAIR_POWER / engine::REPAIR_HITS_PER_ENERGY),
                    ),
                bodies(upgraded, upgrade_pace(atlas, derived, ci, body)),
            )
        });
        let builders = builders.max(i32::from(owed > 0 || mend > 0.0));
        let all_build = !fed || (payback > 0 && upgradable == i32::MAX);
        let upgraders = upgraders.max(i32::from(
            fed && past_reserve && drunk == 0.0 && storage.is_some(),
        ));
        // No more workers than the controller has seats in reach: one past
        // them stands in the others' way and spends nothing.
        let spenders = (builders + upgraders).min(ground.upgrade_seats.unwrap_or(i32::MAX));
        let spenders = if draws { spenders } else { 0 };
        for k in 0..(floor.max(spenders) + crews) {
            let rank = match k {
                0 if inside_deadline(&colony.controller) => DEADLINE,
                0 if mends && !draws => MEND,
                0 if mends => STOCKED,
                0 if floor > 0 => STOCKED,
                k if k < floor => FLOOR,
                k if k < floor + crews => CREW,
                _ => SPEND,
            };
            let spends = rank == SPEND;
            let rank = match rank {
                FLOOR | SPEND if banked > 0.0 && storage.is_none() => STOCKED,
                rank => rank,
            };
            let builds = (all_build || k < builders) && rank != DEADLINE;
            self.worker(ci, colony.room, k as usize, (builds, spends), rank, MOST);
        }

        // The dig, the ring's upkeep, the controller's drink and the ore, each
        // over its own walk to or from the bank.
        if let Some(s) = bank {
            let ring = leg(atlas, derived, &s.pos, &home);
            let feed = ground.feed.as_ref().map_or(home, |f| f.pos);
            let fed = leg(atlas, derived, &s.pos, &feed);
            let ore = (ground.minerals.iter())
                .find_map(|m| m.post)
                .and_then(|p| leg(atlas, derived, &p, &s.pos));
            // The ring refills at the pace its spawns draw it, fight casts too.
            let drawn: f64 = (colony.spawns.iter())
                .filter_map(|s| s.spawning.as_ref())
                .map(|o| {
                    let kinds: Vec<Part> = o.body.iter().map(|p| p.kind).collect();
                    f64::from(engine::body_cost(&kinds))
                        / f64::from(engine::SPAWN_TICKS_PER_PART * kinds.len().max(1) as i32)
                })
                .sum();
            let brought = |at: &Pos| {
                (ground.banks_on_pile())
                    .then(|| leg(atlas, derived, &s.pos, at))
                    .flatten()
            };
            for (rate, own, leg) in [
                (fixed.max(drawn), own_upkeep, ring),
                (build + mend, 0.0, brought(&home)),
                (upgraded, 0.0, brought(&colony.controller.pos)),
                (drunk, 0.0, fed),
                (mined, mined, ore),
            ] {
                if let Some(leg) = leg {
                    local.push(rate, own, leg);
                }
            }
        }
        // The carriers home's flows need come first, then the outposts',
        // each cast once an outpost's dig stands.
        let home_carriers = haulers(&local.all, capacity).len();
        let mut carriers = haulers(&[&local.all[..], &remote].concat(), capacity);
        let spawn_time = f64::from(engine::CREEP_LIFETIME) * colony.spawns.len() as f64;
        let mut spent = (self.out.iter().filter(|s| s.colony == ci))
            .map(|s| spawn_ticks(&s.body))
            .chain(carriers.iter().take(home_carriers).map(|b| spawn_ticks(b)))
            .sum::<f64>();
        let far_carriers = (carriers.iter().skip(home_carriers))
            .take_while(|b| {
                spent += spawn_ticks(b);
                spent <= spawn_time
            })
            .count();
        carriers.truncate(home_carriers + far_carriers);
        if carriers.is_empty() && storage.is_some() {
            carriers.extend(bodies::hauler(2, engine::MAX_BODY_PARTS as i32, capacity));
        }
        let first_far = home_carriers.max(1);
        for (k, body) in carriers.into_iter().enumerate() {
            let slot = self.push(ci, Job::Fill, drop, colony.room, k, body);
            if let Some(j) = k.checked_sub(first_far) {
                slot.id = format!("{outposts}{j}");
                slot.rank = OUTPOST_HAUL;
                slot.fed_by = Some(far.join(","));
            } else if k > 0 || storage.is_none() {
                slot.fed_by = Some(digs.join(","));
            }
        }

        for slot in self.out.iter_mut().filter(|s| s.colony == ci && s.spends) {
            slot.fed_by = Some(digs.join(","));
        }
        let workers = (KEPT_FLOOR + crews) as usize;
        let mut own = haulers(&local.own, capacity);
        if let Some(w) = worker {
            own.extend(std::iter::repeat_n(w, workers));
        }
        self.spare(ci, &own);
    }

    fn spare(&mut self, ci: usize, own: &[Vec<Part>]) {
        let home = self.derived.colonies[ci].room;
        // A claim's pioneers outrank the far haul, so the outposts they leave
        // unhauled are given up until the claim stands.
        let kept: f64 = (self.out.iter())
            .filter(|s| s.colony == ci && !s.spends && s.job != Job::Fill)
            .filter(|s| s.rank == PIONEER || (s.room == home && !matches!(s.job, Job::Work(_))))
            .map(|s| &s.body)
            .chain(own)
            .map(|b| spawn_ticks(b))
            .sum();
        let life = f64::from(engine::CREEP_LIFETIME);
        let spawns = self.derived.colonies[ci].spawns.len() as f64;
        self.spare.insert(home, life * spawns - kept);
    }

    fn worker(
        &mut self,
        ci: usize,
        room: RoomName,
        k: usize,
        (builds, spends): (bool, bool),
        rank: i32,
        work: i32,
    ) -> f64 {
        let capacity = self.derived.colonies[ci].bank_capacity;
        let Some(body) = bodies::sized(Pattern::Worker, work, capacity) else {
            return 0.0;
        };
        let cost = upkeep(&body);
        let seat = (self.derived.grounds.iter())
            .find(|g| g.room == room)
            .and_then(|g| g.controller.as_ref().map(|c| c.pos))
            .unwrap_or_else(|| Pos::new(room, 25, 25));
        let caster = self.derived.colonies[ci].room;
        let key = if caster == room {
            room.to_string()
        } else {
            format!("{room}<{caster}")
        };
        let slot = self.push(ci, Job::Work(builds), seat, &key, k, body);
        slot.rank = rank;
        slot.spends = spends;
        cost
    }
}

/// A claim's controller is its claimer's, never a reserver's as well.
fn claimed(derived: &Derived, controller: Id) -> bool {
    derived.claims.iter().any(|c| c.id == controller)
}

fn errand(derived: &Derived, ci: usize, job: Job, seat: Pos, key: &str) -> Option<Slot> {
    let body = bodies::sized(job.pattern(), 1, derived.colonies[ci].bank_capacity)?;
    Some(Slot::new(ci, false, job, seat, key, 0, body))
}

fn scouter(atlas: &mut Atlas, derived: &Derived, at: &Pos) -> Option<usize> {
    let homes: Vec<Pos> = derived.colonies.iter().map(|c| c.spawns[0].pos).collect();
    let block = engine::body_cost(bodies::block_of(Pattern::Scout));
    let can =
        |ci: usize| !derived.grounds[ci].abandoned && derived.colonies[ci].bank_capacity >= block;
    atlas::nearest(atlas, &|room| derived.route(room), &homes, can, at)
}

pub fn plan(
    atlas: &mut Atlas,
    (derived, declarations): (&Derived, &Declarations),
    owners: &[Option<usize>],
    held: &BTreeMap<String, Assignment>,
) -> (Vec<Slot>, BTreeMap<RoomName, f64>) {
    let sponsors = sponsors(atlas, derived);
    let mut plan = Plan {
        atlas: &*atlas,
        derived,
        held,
        out: Vec::new(),
        spare: BTreeMap::new(),
    };
    for ci in 0..derived.colonies.len() {
        plan.colony(ci, owners, &sponsors);
    }
    let (mut out, spare) = (plan.out, plan.spare);
    // A stood-down outpost keeps its dig and reserver slots, never cast, so
    // its bodies hold them and come back when it reopens.
    for o in &derived.closed {
        let at = o.controller.pos;
        let Some(ci) = server(atlas, (derived, declarations), (&at, false)) else {
            continue;
        };
        let jobs = (o.sources.iter())
            .map(|s| (Job::Dig(s.id), s.pos, s.id))
            .chain((!claimed(derived, o.controller.id)).then_some((
                Job::Reserve(o.controller.id),
                at,
                o.controller.id,
            )));
        for (job, seat, key) in jobs {
            out.push(Slot {
                closed: true,
                ..Slot::new(ci, false, job, seat, key, 0, Vec::new())
            });
        }
    }
    for c in &derived.claims {
        let ci = sponsor(atlas, derived, &c.pos);
        out.extend(ci.and_then(|ci| errand(derived, ci, Job::Claim(c.id), c.pos, &c.id.text())));
    }
    // A scout takes whichever room its walk picks, so a slot is a count, one
    // a colony, keyed by it so a living scout keeps it however rooms reorder.
    let mut scouts = derived.scouts.clone();
    scouts.sort_by_key(|(_, age, cast)| (!*cast, -age));
    let mut held = BTreeSet::new();
    for (at, age, cast) in &scouts {
        if held.len() == derived.colonies.len() {
            break;
        }
        let ci = scouter(atlas, derived, at);
        let Some(mut slot) = ci.and_then(|ci| errand(derived, ci, Job::Scout(at.room), *at, ""))
        else {
            continue;
        };
        if held.insert(slot.colony) {
            let scout: &str = Pattern::Scout.into();
            slot.id = format!("{scout}:{}:0", derived.colonies[slot.colony].room);
            if !cast {
                slot.body.clear();
            }
            // A row never written reads as old as the world: the reserve
            // (§5) cannot be read until it is, so its scout goes before any spend.
            if *age >= derived.time {
                slot.rank = SIGHT;
            }
            out.push(slot);
        }
    }
    for s in &mut out {
        s.home_walk = walk_to(atlas, derived, s.colony, &s.body, &s.seat).unwrap_or(0);
    }
    debug_assert_eq!(shared_ids(&out), 0);
    (out, spare)
}

/// A controller inside its downgrade deadline is worked before any other
/// slot is cast or walked to.
const DEADLINE: i32 = -1;
/// Below the reserve a lost road costs more than the energy that mends it.
const MEND: i32 = 3;
/// The ore is RCL6's payoff: the outposts take what spawn time it leaves.
const MINE: i32 = 3;
/// Cast before the outposts' haul (§5).
const STOCKED: i32 = 4;
/// Ahead of the far haul, as Hivemind's mine builder: its recasts hold every spawn.
const MENDER: i32 = 5;
const OUTPOST_HAUL: i32 = 6;
const MOST: i32 = engine::MAX_BODY_PARTS as i32;
/// A load mends two plain roads whole.
const MENDER_WORK: i32 = 2;
const FLOOR: i32 = 7;
/// What `spare` charges for home's floor whatever it is this tick: a stock
/// about its reserve and the outposts' sites and wear move the floor.
const KEPT_FLOOR: i32 = 2;
const CREW: i32 = 8;
/// After the sponsor's own dig and carriers, before its outposts (The International's).
const PIONEER: i32 = 2;
const PIONEERS: usize = 4;
const TOWER_LEVEL: i32 = 3;
pub const STORAGE_LEVEL: i32 = 4;
const SPEND: i32 = 10;
const SIGHT: i32 = 3;

/// Cast order: a colony's own dig and its carriers first, the pioneers it
/// sponsors, then its outposts' reservers, whose bodies see them, their dig,
/// mender and haul, its floor of workers, and the surplus last.
fn rank_of(job: &Job, own: bool) -> i32 {
    match (job, own) {
        (Job::Dig(_), true) => 0,
        (Job::Fill, _) => 1,
        (Job::Reserve(_) | Job::Claim(_), _) => SIGHT,
        (Job::Scout(_), _) => 12,
        (Job::Dig(_), false) => 4,
        (Job::Work(_), _) => SPEND,
        (Job::Upgrade, _) => 11,
    }
}

fn spawn_ticks(body: &[Part]) -> f64 {
    f64::from(engine::SPAWN_TICKS_PER_PART * body.len() as i32 * engine::CREEP_LIFETIME)
        / f64::from(engine::lifetime(body.iter().copied()))
}

pub fn far(slot: &Slot) -> bool {
    slot.job == Job::Fill && slot.rank == OUTPOST_HAUL
}

/// Numbered apart from home's, so a pool that shrinks never turns an outpost's
/// slot into a home one under its body.
fn far_id(home: RoomName) -> String {
    let hauler: &str = Pattern::Hauler.into();
    format!("{hauler}:{home}:far:")
}

/// An id is its holders' key.
pub fn shared_ids(slots: &[Slot]) -> i32 {
    (slots.len() - slots.iter().map(|s| &s.id).collect::<BTreeSet<_>>().len()) as i32
}

/// A carrier parked at home serves no flow while an outpost slot waits for one.
pub fn parked(slots: &[Slot], holders: &Holders) -> i32 {
    let held = |s: &Slot| holders.get(s.id.as_str()).map_or(0, Vec::len);
    let carriers = |ci: usize, outpost: bool| {
        (slots.iter()).filter(move |s| s.colony == ci && s.job == Job::Fill && far(s) == outpost)
    };
    let colonies: BTreeSet<usize> = slots.iter().map(|s| s.colony).collect();
    (colonies.into_iter())
        .filter(|&ci| {
            carriers(ci, false).any(|s| held(s) > 1) && carriers(ci, true).any(|s| held(s) == 0)
        })
        .count() as i32
}

fn walk_to(atlas: &Atlas, derived: &Derived, ci: usize, body: &[Part], to: &Pos) -> Option<i32> {
    let home = derived.colonies[ci].spawns[0].pos;
    walk(atlas, derived, body, &home, to).or_else(|| distance(atlas, derived, &home, to))
}

/// Dispatch may send a carrier to any of its outposts' digs, so the farthest
/// sets the trip; a dig it cannot price leaves the trip unknown.
fn round_trip<'a>(
    atlas: &Atlas,
    derived: &Derived,
    ci: usize,
    body: &[Part],
    digs: impl IntoIterator<Item = &'a Slot>,
) -> Option<i32> {
    let trips: Option<Vec<i32>> = (digs.into_iter())
        .map(|d| walk_to(atlas, derived, ci, body, &d.seat).map(|w| 2 * w))
        .collect();
    trips?.into_iter().max()
}

/// A carrier's CARRY counts toward the outposts' flow only while it can bring
/// a load home past its successor's spawning; an unknown trip counts none.
fn serves_far(body: &Body, trip: Option<i32>) -> bool {
    trip.is_some_and(|t| !expires(body, t))
}

/// Home dig is worth a recast: the bank that cast this body has grown; and a
/// home carrier under half its slot's shape, one cast on an empty bank.
fn outgrown(derived: &Derived, body: &Body, slot: &Slot) -> bool {
    let home = slot.room == derived.colonies[slot.colony].room;
    let Job::Dig(id) = &slot.job else {
        return matches!(slot.job, Job::Fill)
            && home
            && body.pattern == Pattern::Hauler
            && 2 * body.cast_carry < part_count_in(&slot.body, Part::Carry);
    };
    if body.pattern != Pattern::Harvester || !home {
        return false;
    }
    let rate = (derived.grounds.iter())
        .filter(|g| g.room == slot.room)
        .flat_map(|g| g.sources.iter())
        .find(|s| &s.id == id)
        .map_or(0, |s| s.rate);
    let dug = |work: i32| (work * engine::HARVEST_PER_WORK).min(rate);
    dug(body.cast_work) < dug(part_count_in(&slot.body, Part::Work))
}

fn expires(body: &Body, home_walk: i32) -> bool {
    body.ticks_to_live <= body.size * engine::SPAWN_TICKS_PER_PART + home_walk
}

pub type Holders<'a> = BTreeMap<&'a str, Vec<&'a Body>>;

/// Each body's slot: the one it held while that slot stands, else the
/// nearest of its pattern no body holds, else, for a job that takes extras,
/// the nearest of its pattern. Last, the outgrown bodies whose successors
/// stand at their seats.
pub fn assign<'a>(
    atlas: &Atlas,
    derived: &'a Derived,
    slots: &'a [Slot],
    held: &BTreeMap<String, Assignment>,
) -> (BTreeMap<String, usize>, Holders<'a>, BTreeSet<String>) {
    let by_id: BTreeMap<&str, usize> = slots
        .iter()
        .enumerate()
        .map(|(i, s)| (s.id.as_str(), i))
        .collect();
    let mut out: BTreeMap<String, usize> = BTreeMap::new();
    let mut holders: Holders = BTreeMap::new();
    let mut all: Vec<&Body> = derived.bodies.iter().chain(derived.ovens.iter()).collect();
    all.sort_by_key(|b| {
        (b.pattern == Pattern::Hauler).then_some(std::cmp::Reverse(b.ticks_to_live))
    });
    let mut loose: Vec<&Body> = Vec::new();
    for b in &all {
        let kept = (held.get(&b.name))
            .and_then(|a| by_id.get(a.slot.as_str()))
            .filter(|i| slots[**i].job.pattern() == b.pattern)
            .filter(|i| {
                let s = &slots[**i];
                s.job != Job::Fill
                    || outgrown(derived, b, s)
                    || expires(b, s.home_walk)
                    || !holders.contains_key(s.id.as_str())
            });
        match kept {
            Some(i) => {
                out.insert(b.name.clone(), *i);
                holders.entry(slots[*i].id.as_str()).or_default().push(b);
            }
            None => loose.push(b),
        }
    }
    let stands = |b: &Body, s: &Slot| !expires(b, s.home_walk) && !outgrown(derived, b, s);
    let mut taken: Vec<bool> = (slots.iter())
        .map(|s| (holders.get(s.id.as_str())).is_some_and(|h| h.iter().any(|h| stands(h, s))))
        .collect();
    let trips: Vec<Option<i32>> = (slots.iter())
        .map(|s| {
            let ids = s.fed_by.as_deref().filter(|_| far(s)).unwrap_or_default();
            let digs = (ids.split(',')).filter_map(|id| by_id.get(id).map(|&d| &slots[d]));
            far(s)
                .then(|| round_trip(atlas, derived, s.colony, &s.body, digs))
                .flatten()
        })
        .collect();
    let kept = PairTicks::default();
    for b in loose {
        if b.pattern == Pattern::Scout {
            let free = (slots.iter().enumerate()).find(|(_, s)| {
                s.job.pattern() == b.pattern && !holders.contains_key(s.id.as_str())
            });
            if let Some((i, s)) = free {
                out.insert(b.name.clone(), i);
                holders.entry(s.id.as_str()).or_default().push(b);
            }
            continue;
        }
        // A mender is a body of its own size: a home's loose worker taking it
        // left that home's work, and a young one's.
        let near = |open: bool| {
            (slots.iter().enumerate())
                .filter(|(i, s)| {
                    s.job.pattern() == b.pattern
                        && (if open { !taken[*i] } else { !s.job.seated() })
                })
                .filter(|(_, s)| s.rank != MENDER || b.size == s.body.len() as i32)
                .filter(|(i, s)| !outgrown(derived, b, s) && (!far(s) || serves_far(b, trips[*i])))
                .filter_map(|(i, s)| {
                    let first = (s.body.is_empty(), s.rank != DEADLINE);
                    Some((
                        first,
                        distance_memo(atlas, derived, &kept, &b.pos, &s.seat)?,
                        i,
                    ))
                })
                .min()
                .map(|(_, _, i)| i)
        };
        if let Some(i) = near(true).or_else(|| near(false)) {
            out.insert(b.name.clone(), i);
            holders.entry(slots[i].id.as_str()).or_default().push(b);
            taken[i] |= stands(b, &slots[i]);
        }
    }
    let relieved = |s: &Slot, h: &[&Body]| {
        h.iter()
            .any(|b| !outgrown(derived, b, s) && b.pos.within(1, &s.seat))
    };
    for s in slots {
        if let Some(h) = holders.get_mut(s.id.as_str())
            && relieved(s, h)
        {
            for b in h.iter().filter(|b| outgrown(derived, b, s)) {
                out.remove(&b.name);
            }
            h.retain(|b| !outgrown(derived, b, s));
        }
    }
    let retired = (all.iter())
        .filter(|b| !out.contains_key(&b.name))
        .filter(|b| {
            slots.iter().any(|s| {
                outgrown(derived, b, s)
                    && holders.get(s.id.as_str()).is_some_and(|h| relieved(s, h))
            })
        })
        .map(|b| b.name.clone())
        .collect();
    (out, holders, retired)
}

#[derive(Clone, Debug)]
pub struct Cast {
    pub colony: RoomName,
    pub body: Vec<Part>,
    pub name: String,
    pub floor: bool,
    pub slot: String,
}

/// Every open slot whose flow stands, in order on each colony's idle spawns
/// while the bank pays; the first it cannot pay waits for the bank. With no
/// harvester or no carrier of its own, a colony casts one at the bank it has.
pub fn casts(
    derived: &Derived,
    slots: &[Slot],
    holders: &Holders,
) -> (Vec<Cast>, Vec<(RoomName, Pattern, i32)>) {
    let open = |s: &Slot| {
        holders.get(s.id.as_str()).is_none_or(|h| {
            h.iter()
                .all(|b| expires(b, s.home_walk) || outgrown(derived, b, s))
        })
    };
    let held = |id: &str| holders.get(id).is_some_and(|h| !h.is_empty());
    let fed = |s: &Slot| {
        // A carrier waits on any dig it carries, a spender on every one.
        (s.fed_by.as_deref()).is_none_or(|ids| {
            let mut ids = ids.split(',').filter(|id| !id.is_empty());
            if far(s) { ids.any(held) } else { ids.all(held) }
        })
    };

    let mut casts = Vec::new();
    let mut short: BTreeMap<(RoomName, Pattern), i32> = BTreeMap::new();
    for (ci, colony) in derived.colonies.iter().enumerate() {
        let mut idle: Vec<&Spawn> = colony
            .spawns
            .iter()
            .filter(|s| s.spawning.is_none())
            .collect();
        let mut bank = colony.bank;
        let cast =
            |slot: &Slot, body: Vec<Part>, floor: bool, idle: &mut Vec<&Spawn>, bank: &mut i32| {
                let spawn = idle.remove(0);
                *bank -= engine::body_cost(&body);
                Cast {
                    colony: colony.room,
                    name: bodies::cast_name(slot.job.pattern(), derived.time, &spawn.name),
                    body,
                    floor,
                    slot: slot.id.clone(),
                }
            };
        let mut mine: Vec<&Slot> = slots.iter().filter(|s| s.colony == ci).collect();
        mine.sort_by_key(|s| s.rank);

        // The floors: nothing digs the bank full, or nothing carries it.
        let anchor = mine
            .iter()
            .find(|s| matches!(s.job, Job::Dig(_)) && s.room == colony.room);
        let supply = (mine.iter())
            .find(|s| s.job == Job::Fill && fed(s))
            .or_else(|| {
                mine.iter()
                    .find(|s| matches!(s.job, Job::Work(_)) && s.room == colony.room)
            });
        let sources = &derived.grounds[ci].sources;
        let digging = (mine.iter()).any(|s| {
            matches!(&s.job, Job::Dig(id) if sources.iter().any(|x| &x.id == id)) && held(&s.id)
        });
        let carrying = (mine.iter()).any(|s| {
            matches!(s.job, Job::Fill | Job::Work(_)) && s.room == colony.room && held(&s.id)
        });
        if let Some(s) =
            anchor.filter(|_| !digging && bank < colony.bank_capacity && !idle.is_empty())
            && let Job::Dig(id) = &s.job
            && let Some(rate) = sources.iter().find(|x| &x.id == id).map(|x| x.rate)
            && let Some(body) = bodies::home_harvester(dig_work(rate), bank)
            && engine::body_cost(&body) <= bank
        {
            casts.push(cast(s, body, true, &mut idle, &mut bank));
        }
        if let Some(s) = supply.filter(|_| !carrying && !idle.is_empty())
            && let Some(body) = bodies::sized(s.job.pattern(), engine::MAX_BODY_PARTS as i32, bank)
        {
            casts.push(cast(s, body, true, &mut idle, &mut bank));
        }

        // A body past its own slot's one stands in for an open slot of its
        // pool, so casts are slots less standing bodies.
        let mut spare: BTreeMap<(RoomName, Pattern, bool), usize> = BTreeMap::new();
        for s in mine.iter().filter(|s| !s.job.seated()) {
            let standing = holders
                .get(s.id.as_str())
                .map_or(0, |h| h.iter().filter(|b| !expires(b, s.home_walk)).count());
            *spare.entry((s.room, s.job.pattern(), far(s))).or_default() +=
                standing.saturating_sub(1);
        }
        let mut waiting = false;
        for s in mine {
            // A spend body cast now holds a spawn the fight's ask waits on.
            if colony.owes && s.spends
                || !open(s)
                || !fed(s)
                || s.body.is_empty()
                || casts.iter().any(|c| c.slot == s.id)
            {
                continue;
            }
            if let Some(n) = spare
                .get_mut(&(s.room, s.job.pattern(), far(s)))
                .filter(|n| **n > 0)
            {
                *n -= 1;
                continue;
            }
            if waiting || idle.is_empty() || engine::body_cost(&s.body) > bank {
                waiting = true;
                *short.entry((colony.room, s.job.pattern())).or_default() += 1;
                continue;
            }
            // The colony's own dig and carriers fill the bank a fight's cast is
            // paid from, so they queue ahead of it at any bank.
            let floor = s.rank <= rank_of(&Job::Fill, true);
            casts.push(cast(s, s.body.clone(), floor, &mut idle, &mut bank));
        }
    }
    let short = short.into_iter().map(|((r, p), n)| (r, p, n)).collect();
    (casts, short)
}
