/// A colony's flows as slots: each slot casts one body and is the work that
/// body does, so what is cast and what is offered are one list.
use std::collections::{BTreeMap, BTreeSet};

use crate::core::atlas::{Atlas, Mover, Target};
use crate::core::engine;
use crate::core::grid::{self, Footprint};
use crate::core::tick;
use crate::core::vocabulary::{BodyPart, part_count_in};
use crate::core::world::{Pos, RoomName};

use super::bodies::{self, Leg, Pattern};
use super::derived::{Body, Derived, dig_work};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Job {
    Dig(String),
    /// The colony's carriers: its sources' and outposts' dig home, the
    /// storage's stock to its sinks, and every ore load.
    Fill,
    /// Builds, repairs, and upgrades where no buffer stands; `true` builds
    /// before it upgrades.
    Work(bool),
    Upgrade,
    Reserve(String),
    Claim(String),
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
    pub body: Vec<BodyPart>,
    /// The slot whose body digs what this one carries.
    pub fed_by: Option<String>,
    /// Spends the surplus: cast only while the colony's digs stand.
    pub spends: bool,
    pub rank: i32,
    /// Its outpost is stood down: its body keeps it, and waits at home.
    pub closed: bool,
}

impl Slot {
    /// A lost controller loses the room; a lost road costs more than its repair.
    pub fn draws_reserve(&self) -> bool {
        matches!((self.rank, &self.job), (DEADLINE, _) | (MEND, Job::Work(_)))
    }
}

/// The ticks from `a` to `b`: in one room its range, else its crossings off
/// a tree `Derived` grew from either end, a search only without one.
pub fn distance(atlas: &Atlas, derived: &Derived, a: &Pos, b: &Pos) -> Option<i32> {
    if let Some(r) = a.range(b) {
        return Some(r);
    }
    let tree = (derived.routes.get(&b.room)).or_else(|| derived.routes.get(&a.room));
    let crossed = match tree {
        Some(tree) => tree.crossings(a.room, b.room),
        None => (atlas.route(&BTreeSet::new(), tick::REACH_HOPS, a.room, b.room))
            .map(|r| r.len() as i32 - 1),
    };
    crossed.map(|n| Atlas::crossing_ticks() * n)
}

pub fn ticks_to(
    atlas: &Atlas,
    derived: &Derived,
    mover: Mover,
    target: &Target,
    from: &Pos,
) -> Option<i32> {
    match derived.routes.get(&target.room) {
        Some(tree)
            if from.room != target.room
                && target.avoid.is_empty()
                && target.max_hops == tick::REACH_HOPS =>
        {
            atlas.ticks_on(tree, mover, target, from)
        }
        _ => atlas.ticks_to(mover, target, from),
    }
}

fn walk(atlas: &Atlas, derived: &Derived, parts: &[BodyPart], from: &Pos, to: &Pos) -> Option<i32> {
    let mover = Mover {
        footprint: Footprint::One,
        factor: grid::loaded_factor(parts),
    };
    ticks_to(atlas, derived, mover, &tick::target_at(to, 1), from)
}

/// A room out of sight floods nothing: its walk is its crossings, unroaded.
fn leg(atlas: &Atlas, derived: &Derived, from: &Pos, to: &Pos) -> Option<Leg> {
    use BodyPart::{Carry, Move};
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
fn haulers(flows: &[(f64, Leg)], capacity: i32) -> Vec<Vec<BodyPart>> {
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
    let per = part_count_in(&most, BodyPart::Carry).max(1);
    vec![most; ((carry + per - 1) / per) as usize]
}

/// Per colony, the stocked colony that sends its pioneers while it is young.
fn sponsors(atlas: &mut Atlas, derived: &Derived) -> Vec<Option<usize>> {
    let homes: Vec<Pos> = derived.colonies.iter().map(|c| c.spawns[0].pos).collect();
    let banks = |ci: usize| !derived.grounds[ci].abandoned && derived.grounds[ci].has_storage();
    (derived.colonies.iter().enumerate())
        .map(|(cj, c)| {
            let young = c.controller.level < TOWER_LEVEL && !derived.grounds[cj].has_storage();
            let can = |ci: usize| ci != cj && banks(ci);
            let at = c.controller.pos;
            young.then(|| tick::nearest(atlas, &|r| derived.routes.get(&r), &homes, can, &at))?
        })
        .collect()
}

fn banks(derived: &Derived, ci: usize) -> bool {
    !derived.grounds[ci].abandoned && derived.grounds[ci].bank().is_some()
}

/// The colony that serves each ground: its own colony, else the nearest that
/// banks the haul (`ARCHITECTURE.md` §5).
pub fn owners(atlas: &mut Atlas, derived: &Derived) -> Vec<Option<usize>> {
    let homes: Vec<Pos> = derived.colonies.iter().map(|c| c.spawns[0].pos).collect();
    let mut out = Vec::new();
    for (gi, ground) in derived.grounds.iter().enumerate() {
        if gi < derived.colonies.len() {
            out.push(Some(gi));
            continue;
        }
        let at = (ground.controller.as_ref().map(|c| c.pos))
            .or_else(|| ground.sources.first().map(|s| s.pos));
        out.push(at.and_then(|at| {
            tick::nearest(
                atlas,
                &|room| derived.routes.get(&room),
                &homes,
                |ci| banks(derived, ci),
                &at,
            )
        }));
    }
    out
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

fn upkeep(body: &[BodyPart]) -> f64 {
    let life = if body.contains(&BodyPart::Claim) {
        engine::CLAIM_LIFETIME
    } else {
        engine::CREEP_LIFETIME
    };
    f64::from(engine::body_cost(body)) / f64::from(life)
}

/// Over `h` ticks a pile loses `h / 2D` of itself to decay and the bodies cast
/// to spend it idle a life less `h` at `per` an energy: least at √(2·D·per·life).
fn drain(per: f64) -> f64 {
    let life = f64::from(engine::CREEP_LIFETIME);
    (2.0 * f64::from(engine::ENERGY_DECAY) * per * life)
        .sqrt()
        .min(life)
}

/// Half the timer: a level-up lands inside it, and safe mode's grace stays banked.
pub fn inside_deadline(c: &crate::core::world::Controller) -> bool {
    c.level > 0 && c.ticks_to_downgrade <= engine::controller_downgrade(c.level) / 2
}

/// What a WORK of a worker spends a tick at `power` a WORK while working:
/// its load lasts so long, then it walks to its energy and back.
fn worker_pace(atlas: &Atlas, derived: &Derived, ci: usize, body: &[BodyPart], power: i32) -> f64 {
    let (colony, ground) = (&derived.colonies[ci], &derived.grounds[ci]);
    let work = part_count_in(body, BodyPart::Work).max(1);
    let fetches: Vec<(Pos, i32)> = match (&ground.buffer, ground.bank()) {
        (Some(b), _) => vec![(b.pos, 1)],
        (None, Some(s)) => vec![(s.pos, 1)],
        (None, None) => (ground.sources.iter())
            .map(|s| (s.post.unwrap_or(s.pos), s.dug.max(1)))
            .collect(),
    };
    let empty: Vec<BodyPart> = body.iter().copied().filter(|p| !p.is_carry()).collect();
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
    let load = f64::from(part_count_in(body, BodyPart::Carry) * engine::CARRY_PART_CAPACITY);
    let working = load / f64::from(work * power);
    let trip = if weights == 0 {
        f64::from(engine::CREEP_LIFETIME)
    } else {
        f64::from(weighted) / f64::from(weights)
    };
    f64::from(power) * working / (working + trip)
}

struct Plan<'a> {
    atlas: &'a Atlas,
    derived: &'a Derived,
    out: Vec<Slot>,
}

impl Plan<'_> {
    #[allow(clippy::too_many_arguments)]
    fn push(
        &mut self,
        ci: usize,
        room: RoomName,
        job: Job,
        seat: Pos,
        key: &str,
        k: usize,
        body: Vec<BodyPart>,
    ) {
        let tag = bodies::name_of(job.pattern());
        let own = room == self.derived.colonies[ci].room;
        self.out.push(Slot {
            id: format!("{tag}:{key}:{k}"),
            colony: ci,
            room,
            rank: rank_of(&job, own),
            job,
            seat,
            body,
            fed_by: None,
            spends: false,
            closed: false,
        });
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

        if ground.abandoned {
            if !ground.shipment.is_empty() {
                let body = bodies::hauler(2, engine::MAX_BODY_PARTS as i32, capacity);
                self.push(
                    ci,
                    colony.room,
                    Job::Fill,
                    drop,
                    &colony.room.to_string(),
                    0,
                    body.unwrap_or_default(),
                );
            }
            if inside_deadline(&colony.controller) {
                self.worker(ci, colony.room, 0, false, FLOOR);
            }
            return;
        }

        let mut fixed = 0.0;
        let worker = bodies::sized(Pattern::Worker, engine::MAX_BODY_PARTS as i32, capacity);
        let mut dug = 0.0;
        let mut far_dug = 0.0;
        let mut owed = 0;
        let mut payback = 0;
        let mut repairs = false;
        let mut mends = false;
        let mut crews = 0;
        let mut digs: Vec<String> = Vec::new();
        let mut local: Vec<(f64, Leg)> = Vec::new();
        let mut remote: Vec<(f64, Leg)> = Vec::new();
        let mut far: Vec<String> = Vec::new();
        let mut drink: i32 = (derived.bodies.iter().chain(derived.ovens.iter()))
            .filter(|b| b.pos.room == colony.room && b.work > 0 && b.carry > 0)
            .filter(|b| !b.pattern.is_harvester())
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
                    .filter(|s| s.kind.pays_back())
                    .map(|s| s.left)
                    .sum::<i32>();
            }
            repairs |= !g.repairs.is_empty();
            let haul = derived.haul(owners, gi);
            mends |= own && g.repairs.iter().any(|r| r.rebuild > 0 && r.at_risk(haul));
            if raised {
                // A claim without its spawn is raised by workers sent from here.
                for k in 0..PIONEERS {
                    self.worker(ci, g.room, k, true, PIONEER);
                }
                continue;
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
                        .min(part_count_in(b, BodyPart::Work) * engine::HARVEST_PER_WORK)
                });
                // A crew digs what the post's harvester leaves of a source
                // whose container does not stand, and builds it, a worker
                // digging a third of its life; not knowing its seats is no crew.
                if s.container.is_none() && own {
                    let seats = s.seats.map_or(0, |n| n - i32::from(s.post.is_some()));
                    let crew = worker.as_ref().map_or(1, |w| {
                        part_count_in(w, BodyPart::Work) * engine::HARVEST_PER_WORK / 3
                    });
                    crews += seats.min((s.rate - rate + crew - 1) / crew.max(1));
                }
                let Some(body) = body else { continue };
                let post = s.post.unwrap_or(s.pos);
                fixed += upkeep(&body);
                dug += f64::from(rate);
                if !own {
                    far_dug += f64::from(rate);
                }
                let dig_id = format!("harvester:{}:0", s.id);
                if own {
                    digs.push(dig_id.clone());
                }
                self.push(ci, g.room, Job::Dig(s.id.clone()), post, &s.id, 0, body);
                // A source link carries the dig to a hub without limit, to the
                // controller's link as fast as the room's bodies drink it.
                let linked = match (&s.link, &g.receiver) {
                    (Some(_), Some(r)) if r.role.is_hub() => rate,
                    (Some(_), Some(_)) => rate.min(drink),
                    _ => 0,
                };
                drink -= linked;
                let rate = rate - linked;
                // An outpost's dig piles on its post until the container
                // stands, and is hauled from there; at home the crews take it.
                if s.container.is_none() && own || rate <= 0 {
                    continue;
                }
                // A backlog is carried off within the ticks its source fills a
                // container, or the dig overflows; never faster than the dig, so
                // the haulers stay bounded by what the source yields.
                let waiting: i32 = (g.stores.iter())
                    .filter(|st| s.container.as_ref() == Some(&st.id))
                    .map(|st| st.energy)
                    .chain(
                        g.piles
                            .iter()
                            .filter(|p| p.pos.within(1, &post))
                            .map(|p| p.amount),
                    )
                    .sum();
                let fills = engine::CONTAINER_CAPACITY / engine::HELD_OUTPUT_PER_TICK;
                let rate = rate + (waiting / fills).min(rate);
                let to =
                    bank.map_or_else(|| ground.buffer.as_ref().map_or(home, |b| b.pos), |s| s.pos);
                let Some(leg) = leg(atlas, derived, &post, &to) else {
                    continue;
                };
                if own {
                    local.push((f64::from(rate), leg));
                } else {
                    remote.push((f64::from(rate), leg));
                    far.push(dig_id.clone());
                }
            }
            match &g.controller {
                Some(c) if !own && g.reservable => {
                    let parts = claim_parts_for(g.our_reservation);
                    if let Some(body) =
                        bodies::sized(Pattern::Claimer, parts, capacity).filter(|_| {
                            capacity >= engine::body_cost(bodies::block_of(Pattern::Claimer))
                        })
                    {
                        fixed += upkeep(&body);
                        self.push(
                            ci,
                            g.room,
                            Job::Reserve(c.id.clone()),
                            c.pos,
                            &c.id,
                            0,
                            body,
                        );
                    }
                }
                _ => (),
            }
        }

        for cj in (0..sponsors.len()).filter(|cj| sponsors[*cj] == Some(ci)) {
            for k in 0..PIONEERS {
                self.worker(ci, derived.colonies[cj].room, k, false, PIONEER);
            }
        }

        let mut mined = 0.0;
        for m in &ground.minerals {
            let Some(post) = m.post.filter(|_| m.container.is_some() && m.amount > 0) else {
                continue;
            };
            if let Some(body) = bodies::sized(Pattern::Harvester, bodies::MINER_WORK, capacity) {
                fixed += upkeep(&body);
                mined += f64::from(
                    part_count_in(&body, BodyPart::Work) * engine::MINERAL_HARVEST_PER_WORK,
                ) / f64::from(engine::MINERAL_HARVEST_CYCLE);
                self.push(
                    ci,
                    colony.room,
                    Job::Dig(m.id.clone()),
                    post,
                    &m.id,
                    0,
                    body,
                );
                self.out.last_mut().expect("pushed").rank = MINE;
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
                let pace = |w: &Vec<BodyPart>| {
                    let work = f64::from(part_count_in(w, BodyPart::Work));
                    upkeep(w)
                        / work
                        / worker_pace(atlas, derived, ci, w, engine::UPGRADE_DRAIN_PER_WORK)
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
            part_count_in(w, BodyPart::Carry) * engine::CARRY_PART_CAPACITY
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
        fixed += f64::from(floor) * worker.as_deref().map_or(0.0, upkeep);
        let carried: Vec<(f64, Leg)> = local.iter().chain(&remote).copied().collect();
        fixed += haulers(&carried, capacity)
            .iter()
            .map(|b| upkeep(b))
            .sum::<f64>();
        // With no reserve to draw down, spend sized to dig no carrier brings
        // home drinks the bank that would cast those carriers.
        let carry = |flows: &[(f64, Leg)]| {
            (haulers(flows, capacity).iter())
                .map(|b| part_count_in(b, BodyPart::Carry))
                .sum::<i32>()
        };
        let names: Vec<&str> = colony.spawns.iter().map(|s| s.name.as_str()).collect();
        let standing: i32 = (derived.bodies.iter().chain(derived.ovens.iter()))
            .filter(|b| b.pattern.is_hauler())
            .filter(|b| bodies::spawn_of_name(&b.name).is_some_and(|n| names.contains(&n)))
            .map(|b| b.carry)
            .sum();
        let hauled = f64::from(standing - carry(&local)) / f64::from(carry(&remote).max(1));
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
                let even = walk(
                    atlas,
                    derived,
                    &[BodyPart::Carry, BodyPart::Move],
                    &home,
                    &b.pos,
                )?;
                let roaded = walk(
                    atlas,
                    derived,
                    &[BodyPart::Carry, BodyPart::Carry, BodyPart::Move],
                    &home,
                    &b.pos,
                )?;
                bodies::upgrader(2 * even - roaded, roaded - even, upgradable, capacity)
            })
            .filter(|b| {
                !ground.buffer_seats.is_empty()
                    && bodies::standing(
                        part_count_in(b, BodyPart::Work),
                        part_count_in(b, BodyPart::Carry),
                    )
            });
        // A structure pays only once it stands, so a level is built out before
        // it is upgraded; a road backlog never stops the controller.
        let works = match () {
            _ if !draws => 0.0,
            _ if !past_reserve => f64::from(payback) / life,
            _ if upgradable < i32::MAX => (surplus - f64::from(upgradable)).max(0.0),
            _ if payback > 0 => surplus,
            _ => ((dug - fixed) / 2.0).clamp(0.0, surplus),
        };
        let build = works.min(f64::from(owed) / life);
        // Repair is its own flow: what decays a tick and what is missing over
        // a life, out of the sites' share.
        let mended: f64 = (derived.grounds.iter().enumerate())
            .filter(|(gi, _)| owners[*gi] == Some(ci))
            .flat_map(|(gi, g)| {
                let haul = derived.haul(owners, gi);
                (g.repairs.iter())
                    .map(move |r| r.loss(haul) + f64::from(r.hits_max - r.hits) / life)
            })
            .sum::<f64>()
            / f64::from(engine::REPAIR_HITS_PER_ENERGY);
        let mend = (works - build).max(0.0).min(mended);

        let mut drunk = 0.0;
        let mut upgraded = surplus - build - mend;
        if let Some(body) = &upgrader {
            let work = part_count_in(body, BodyPart::Work);
            let most =
                (ground.buffer_seats.len() as i32).min(upgradable.saturating_add(work - 1) / work);
            // A standing upgrader's body is paid for: only its drink is owed.
            let standing = (derived.bodies.iter().chain(derived.ovens.iter()))
                .filter(|b| b.pattern.is_upgrader() && b.pos.room == colony.room)
                .count() as i32;
            let kept = ((upgraded / f64::from(work)).round() as i32).min(standing);
            let new =
                ((upgraded - f64::from(kept * work)) / (f64::from(work) + upkeep(body))).max(0.0);
            let n = (kept + new.floor() as i32).clamp(0, most);
            for k in 0..n as usize {
                let seat = ground.buffer_seats[k];
                self.push(
                    ci,
                    colony.room,
                    Job::Upgrade,
                    seat,
                    &colony.room.to_string(),
                    k,
                    body.clone(),
                );
                let slot = self.out.last_mut().expect("pushed");
                slot.spends = true;
                if banked > 0.0 {
                    slot.rank = STOCKED;
                }
            }
            drunk = f64::from(n * work.min(upgradable));
            upgraded = 0.0;
        }

        let (builders, upgraders) = worker.as_ref().map_or((0, 0), |body| {
            let work = f64::from(part_count_in(body, BodyPart::Work).max(1));
            let per = upkeep(body) / work;
            let bodies = |spend: f64, power: i32| {
                let pace = worker_pace(atlas, derived, ci, body, power);
                (spend / (pace + per) / work).ceil() as i32
            };
            (
                bodies(build, engine::BUILD_POWER)
                    + bodies(mend, engine::REPAIR_POWER / engine::REPAIR_HITS_PER_ENERGY),
                bodies(upgraded, engine::UPGRADE_DRAIN_PER_WORK),
            )
        });
        let builders = builders.max(i32::from(owed > 0 || mend > 0.0));
        let all_build = payback > 0 && upgradable == i32::MAX;
        let upgraders = upgraders.max(i32::from(past_reserve && drunk == 0.0 && storage.is_some()));
        // No more workers than the controller has seats in reach: one past
        // them stands in the others' way and spends nothing.
        let spenders = (builders + upgraders).min(ground.upgrade_seats.unwrap_or(i32::MAX));
        let spenders = if draws { spenders } else { 0 };
        for k in 0..(floor.max(spenders) + crews) {
            let rank = match k {
                0 if inside_deadline(&colony.controller) => DEADLINE,
                0 if mends && !draws => MEND,
                k if k < floor => FLOOR,
                k if k < floor + crews => CREW,
                _ => SPEND,
            };
            let builds = (all_build || k < builders) && rank != DEADLINE;
            self.worker(ci, colony.room, k as usize, builds, rank);
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
                    let kinds: Vec<BodyPart> = o.body.iter().map(|p| p.kind).collect();
                    f64::from(engine::body_cost(&kinds))
                        / f64::from(engine::SPAWN_TICKS_PER_PART * kinds.len().max(1) as i32)
                })
                .sum();
            for (rate, leg) in [(fixed.max(drawn), ring), (drunk, fed), (mined, ore)] {
                local.extend(leg.map(|leg| (rate, leg)));
            }
        }
        // The carriers home's flows need come first, then the outposts',
        // each cast once an outpost's dig stands.
        let home_carriers = haulers(&local, capacity).len();
        local.extend(remote);
        let mut carriers = haulers(&local, capacity);
        let ticks = |b: &Vec<BodyPart>| {
            let life = if b.contains(&BodyPart::Claim) {
                engine::CLAIM_LIFETIME
            } else {
                engine::CREEP_LIFETIME
            };
            f64::from(engine::SPAWN_TICKS_PER_PART * b.len() as i32 * engine::CREEP_LIFETIME)
                / f64::from(life)
        };
        let spawn_time = f64::from(engine::CREEP_LIFETIME) * colony.spawns.len() as f64;
        let mut spent = (self.out.iter().filter(|s| s.colony == ci))
            .map(|s| ticks(&s.body))
            .chain(carriers.iter().take(home_carriers).map(ticks))
            .sum::<f64>();
        let far_carriers = (carriers.iter().skip(home_carriers))
            .take_while(|b| {
                spent += ticks(b);
                spent <= spawn_time
            })
            .count();
        carriers.truncate(home_carriers + far_carriers);
        if carriers.is_empty() && storage.is_some() {
            carriers.extend(bodies::hauler(2, engine::MAX_BODY_PARTS as i32, capacity));
        }
        let room = colony.room.to_string();
        for (k, body) in carriers.into_iter().enumerate() {
            self.push(ci, colony.room, Job::Fill, drop, &room, k, body);
            let slot = self.out.last_mut().expect("pushed");
            if k >= home_carriers.max(1) {
                slot.rank = OUTPOST_HAUL;
                slot.fed_by = Some(far.join(","));
            } else if k > 0 || storage.is_none() {
                slot.fed_by = Some(digs.join(","));
            }
        }

        for slot in self.out.iter_mut().filter(|s| s.colony == ci && s.spends) {
            slot.fed_by = Some(digs.join(","));
        }
    }

    fn worker(&mut self, ci: usize, room: RoomName, k: usize, builds: bool, rank: i32) {
        let capacity = self.derived.colonies[ci].bank_capacity;
        let Some(body) = bodies::sized(Pattern::Worker, engine::MAX_BODY_PARTS as i32, capacity)
        else {
            return;
        };
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
        self.push(ci, room, Job::Work(builds), seat, &key, k, body);
        let slot = self.out.last_mut().expect("pushed");
        slot.rank = rank;
        slot.spends = rank == SPEND;
    }
}

fn errand(atlas: &mut Atlas, derived: &Derived, job: Job, seat: Pos, key: &str) -> Option<Slot> {
    let pattern = job.pattern();
    let homes: Vec<Pos> = derived.colonies.iter().map(|c| c.spawns[0].pos).collect();
    let can = |ci: usize| {
        !derived.grounds[ci].abandoned
            && derived.colonies[ci].bank_capacity >= engine::body_cost(bodies::block_of(pattern))
    };
    let ci = tick::nearest(atlas, &|room| derived.routes.get(&room), &homes, can, &seat)?;
    let body = bodies::sized(pattern, 1, derived.colonies[ci].bank_capacity)?;
    Some(Slot {
        id: format!("{}:{key}:0", bodies::name_of(pattern)),
        rank: rank_of(&job, false),
        colony: ci,
        room: seat.room,
        job,
        seat,
        body,
        fed_by: None,
        spends: false,
        closed: false,
    })
}

pub fn plan(atlas: &mut Atlas, derived: &Derived, owners: &[Option<usize>]) -> Vec<Slot> {
    let sponsors = sponsors(atlas, derived);
    let mut plan = Plan {
        atlas: &*atlas,
        derived,
        out: Vec::new(),
    };
    for ci in 0..derived.colonies.len() {
        plan.colony(ci, owners, &sponsors);
    }
    let mut out = plan.out;
    // A stood-down outpost keeps its dig and reserver slots, never cast, so
    // its bodies hold them and come back when it reopens.
    let homes: Vec<Pos> = derived.colonies.iter().map(|c| c.spawns[0].pos).collect();
    for o in &derived.closed {
        let at = o.controller.pos;
        let Some(ci) = tick::nearest(
            atlas,
            &|r| derived.routes.get(&r),
            &homes,
            |ci| banks(derived, ci),
            &at,
        ) else {
            continue;
        };
        let jobs = (o.sources.iter())
            .map(|s| (Job::Dig(s.id.clone()), s.pos, s.id.clone()))
            .chain(std::iter::once((
                Job::Reserve(o.controller.id.clone()),
                at,
                o.controller.id.clone(),
            )));
        for (job, seat, key) in jobs {
            out.push(Slot {
                id: format!("{}:{key}:0", bodies::name_of(job.pattern())),
                colony: ci,
                room: o.room,
                rank: rank_of(&job, false),
                job,
                seat,
                body: Vec::new(),
                fed_by: None,
                spends: false,
                closed: true,
            });
        }
    }
    for c in &derived.claims {
        out.extend(errand(
            atlas,
            derived,
            Job::Claim(c.id.clone()),
            c.pos,
            &c.id,
        ));
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
        let Some(mut slot) = errand(atlas, derived, Job::Scout(at.room), *at, "") else {
            continue;
        };
        if held.insert(slot.colony) {
            slot.id = format!(
                "{}:{}:0",
                bodies::name_of(Pattern::Scout),
                derived.colonies[slot.colony].room
            );
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
    out
}

/// A controller inside its downgrade deadline is worked before any other
/// slot is cast or walked to.
const DEADLINE: i32 = -1;
/// Below the reserve a lost road costs more than the energy that mends it.
const MEND: i32 = 3;
/// The ore is RCL6's payoff: the outposts take what spawn time it leaves.
const MINE: i32 = 3;
/// Upgraders while the stock stands past the reserve (§5).
const STOCKED: i32 = 4;
const OUTPOST_HAUL: i32 = 5;
const FLOOR: i32 = 6;
const CREW: i32 = 8;
const PIONEER: i32 = 9;
const PIONEERS: usize = 4;
/// Below the level its first tower stands at, a colony is raised by its
/// nearest stocked neighbour's workers too.
const TOWER_LEVEL: i32 = 3;
const SPEND: i32 = 10;
const SIGHT: i32 = 3;

/// Cast order: a colony's own dig and its carriers first, then its outposts'
/// reservers, whose bodies see them, their dig and haul, its floor of
/// workers, and the surplus last.
fn rank_of(job: &Job, own: bool) -> i32 {
    match (job, own) {
        (Job::Dig(_), true) => 0,
        (Job::Fill, _) => 2,
        (Job::Reserve(_) | Job::Claim(_), _) => SIGHT,
        (Job::Scout(_), _) => 12,
        (Job::Dig(_), false) => 4,
        (Job::Work(_), _) => SPEND,
        (Job::Upgrade, _) => 11,
    }
}

pub fn far(slot: &Slot) -> bool {
    slot.job == Job::Fill && slot.rank == OUTPOST_HAUL
}

pub fn expiring(atlas: &Atlas, derived: &Derived, body: &Body, slot: &Slot) -> bool {
    expires(body, home_walk(atlas, derived, slot))
}

fn home_walk(atlas: &Atlas, derived: &Derived, slot: &Slot) -> i32 {
    let home = derived.colonies[slot.colony].spawns[0].pos;
    (walk(atlas, derived, &slot.body, &home, &slot.seat))
        .or_else(|| distance(atlas, derived, &home, &slot.seat))
        .unwrap_or(0)
}

/// Home dig is worth a recast: the bank that cast this body has grown.
fn outgrown(derived: &Derived, body: &Body, slot: &Slot) -> bool {
    let Job::Dig(id) = &slot.job else {
        return false;
    };
    let rate = (derived.grounds.iter())
        .filter(|g| g.room == slot.room)
        .flat_map(|g| g.sources.iter())
        .find(|s| &s.id == id)
        .map_or(0, |s| s.rate);
    let dug = |work: i32| (work * engine::HARVEST_PER_WORK).min(rate);
    body.pattern.is_harvester()
        && slot.room == derived.colonies[slot.colony].room
        && dug(body.work) < dug(part_count_in(&slot.body, BodyPart::Work))
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
    held: &BTreeMap<String, String>,
) -> (BTreeMap<String, usize>, Holders<'a>, BTreeSet<String>) {
    let by_id: BTreeMap<&str, usize> = slots
        .iter()
        .enumerate()
        .map(|(i, s)| (s.id.as_str(), i))
        .collect();
    let mut out: BTreeMap<String, usize> = BTreeMap::new();
    let mut holders: Holders = BTreeMap::new();
    let all: Vec<&Body> = derived.bodies.iter().chain(derived.ovens.iter()).collect();
    let mut loose: Vec<&Body> = Vec::new();
    for b in &all {
        let kept = (held.get(&b.name))
            .and_then(|v| by_id.get(slot_of(v)))
            .filter(|i| slots[**i].job.pattern() == b.pattern);
        match kept {
            Some(i) => {
                out.insert(b.name.clone(), *i);
                holders.entry(slots[*i].id.as_str()).or_default().push(b);
            }
            None => loose.push(b),
        }
    }
    let walks: Vec<i32> = if loose.iter().all(|b| b.pattern.is_scout()) {
        Vec::new()
    } else {
        slots.iter().map(|s| home_walk(atlas, derived, s)).collect()
    };
    for b in loose {
        if b.pattern.is_scout() {
            let free = (slots.iter().enumerate()).find(|(_, s)| {
                s.job.pattern() == b.pattern && !holders.contains_key(s.id.as_str())
            });
            if let Some((i, s)) = free {
                out.insert(b.name.clone(), i);
                holders.entry(s.id.as_str()).or_default().push(b);
            }
            continue;
        }
        let near = |open: bool| {
            (slots.iter().enumerate())
                .filter(|(_, s)| s.job.pattern() == b.pattern && !outgrown(derived, b, s))
                .filter(|(i, s)| {
                    let taken = holders.get(s.id.as_str()).is_some_and(|h| {
                        h.iter()
                            .any(|h| !expires(h, walks[*i]) && !outgrown(derived, h, s))
                    });
                    if open { !taken } else { !s.job.seated() }
                })
                .filter_map(|(i, s)| {
                    let first = (s.body.is_empty(), s.rank != DEADLINE);
                    Some((first, distance(atlas, derived, &b.pos, &s.seat)?, i))
                })
                .min()
                .map(|(_, d, i)| (d, i))
                .map(|(_, i)| i)
        };
        if let Some(i) = near(true).or_else(|| near(false)) {
            out.insert(b.name.clone(), i);
            holders.entry(slots[i].id.as_str()).or_default().push(b);
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

/// An assignment is `slot|task`; the task is what the body did last tick.
pub fn slot_of(held: &str) -> &str {
    held.split_once('|').map_or(held, |(s, _)| s)
}

pub fn task_of(held: &str) -> Option<&str> {
    held.split_once('|')
        .map(|(_, t)| t)
        .filter(|t| !t.is_empty())
}

#[derive(Clone, Debug)]
pub struct Cast {
    pub colony: RoomName,
    pub body: Vec<BodyPart>,
    pub name: String,
    pub floor: bool,
    pub slot: String,
}

/// Every open slot whose flow stands, in order on each colony's idle spawns
/// while the bank pays; the first it cannot pay waits for the bank. With no
/// harvester or no carrier of its own, a colony casts one at the bank it has.
pub fn casts(
    atlas: &Atlas,
    derived: &Derived,
    slots: &[Slot],
    holders: &Holders,
) -> (Vec<Cast>, Vec<(RoomName, Pattern, i32)>) {
    let open = |s: &Slot| {
        holders.get(s.id.as_str()).is_none_or(|h| {
            h.iter()
                .all(|b| expiring(atlas, derived, b, s) || outgrown(derived, b, s))
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
        let mut idle: Vec<&crate::core::world::Spawn> = colony
            .spawns
            .iter()
            .filter(|s| s.spawning.is_none())
            .collect();
        let mut bank = colony.bank;
        let cast = |slot: &Slot,
                    body: Vec<BodyPart>,
                    floor: bool,
                    idle: &mut Vec<&crate::core::world::Spawn>,
                    bank: &mut i32| {
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
        let mine: Vec<&Slot> = {
            let mut v: Vec<&Slot> = slots.iter().filter(|s| s.colony == ci).collect();
            v.sort_by_key(|s| s.rank);
            v
        };

        // The floors: nothing digs the bank full, or nothing carries it.
        let anchor = mine
            .iter()
            .find(|s| matches!(s.job, Job::Dig(_)) && s.room == colony.room);
        let supply = (mine.iter()).find(|s| s.job == Job::Fill).or_else(|| {
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
            && let Some(rate) = derived.grounds[ci]
                .sources
                .iter()
                .find(|x| &x.id == id)
                .map(|x| x.rate)
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
        let mut spare: BTreeMap<(RoomName, Pattern), usize> = BTreeMap::new();
        for s in mine.iter().filter(|s| !s.job.seated()) {
            let standing = holders.get(s.id.as_str()).map_or(0, |h| {
                h.iter().filter(|b| !expiring(atlas, derived, b, s)).count()
            });
            *spare.entry((s.room, s.job.pattern())).or_default() += standing.saturating_sub(1);
        }
        let mut waiting = false;
        for s in mine {
            // A spend body cast now holds a spawn the fight's ask waits on.
            if colony.owes && (s.spends || s.rank == STOCKED)
                || !open(s)
                || !fed(s)
                || s.body.is_empty()
                || casts.iter().any(|c| c.slot == s.id)
            {
                continue;
            }
            if let Some(n) = spare
                .get_mut(&(s.room, s.job.pattern()))
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
            // The colony's own carriers refill the bank a fight's cast is paid
            // from: while it stands under half they queue ahead of the cast,
            // or neither is ever cast.
            let floor = s.job == Job::Fill
                && s.rank == rank_of(&Job::Fill, true)
                && 2 * colony.bank < colony.bank_capacity;
            casts.push(cast(s, s.body.clone(), floor, &mut idle, &mut bank));
        }
    }
    let short = short.into_iter().map(|((r, p), n)| (r, p, n)).collect();
    (casts, short)
}
