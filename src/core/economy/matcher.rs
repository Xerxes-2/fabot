/// Knows no Task kinds: what a body may take is `Pool::fits`, what it brings to
/// a capacity `Pool::share`, where a Task ranks its `Priority` and walk.
use std::cmp::Ordering;
use std::collections::BTreeMap;

use crate::core::HashMap;

use super::derived::Body;
use super::pool::{self, Capacity, Offer};
use super::price;

/// Creep name → `task_id`.
pub type Assignments = BTreeMap<String, String>;

/// `floor`: ticks no walk of `travel`'s is shorter than, so an offer whose
/// floor costs more than the best reached is never walked.
pub fn match_all(
    travel: &mut dyn FnMut(&Body, &Offer) -> Option<i32>,
    floor: &dyn Fn(&Body, &Offer) -> i32,
    pool: &[Offer],
    bodies: &[Body],
    assignments: &Assignments,
) -> Assignments {
    let mut matched = Matcher {
        travel,
        floor,
        pool,
        bodies,
        walks: vec![Walk::Unasked; bodies.len() * pool.len()],
        held: vec![0; pool.len()],
        holders: vec![0; pool.len()],
        assigned: vec![None; bodies.len()],
        best_offer: vec![None; bodies.len()],
        best_cost: vec![0; bodies.len()],
        best_walk: vec![0; bodies.len()],
        best_load: vec![0; bodies.len()],
        by_id: pool
            .iter()
            .enumerate()
            .map(|(oi, offer)| (offer.id.as_str(), oi))
            .collect(),
    };
    matched.run(assignments)
}

struct Matcher<'a> {
    travel: &'a mut dyn FnMut(&Body, &Offer) -> Option<i32>,
    floor: &'a dyn Fn(&Body, &Offer) -> i32,
    pool: &'a [Offer],
    bodies: &'a [Body],
    walks: Vec<Walk>,
    held: Vec<i32>,
    holders: Vec<i32>,
    assigned: Vec<Option<usize>>,
    best_offer: Vec<Option<usize>>,
    best_cost: Vec<i32>,
    best_walk: Vec<i32>,
    best_load: Vec<i32>,
    by_id: HashMap<&'a str, usize>,
}

/// `Unreached` also when the body dies first (`Pool::outlives`).
#[derive(Clone, Copy)]
enum Walk {
    Unasked,
    Unreached,
    Ticks(i32),
}

fn before(a: (i32, i32, i32), b: (i32, i32, i32)) -> bool {
    a.0 < b.0 || (a.0 == b.0 && (a.1 < b.1 || (a.1 == b.1 && a.2 < b.2)))
}

impl Matcher<'_> {
    fn walk(&mut self, bi: usize, oi: usize) -> Option<i32> {
        let at = bi * self.pool.len() + oi;

        if let Walk::Unasked = self.walks[at] {
            self.walks[at] = match (self.travel)(&self.bodies[bi], &self.pool[oi]) {
                Some(t) if pool::outlives(&self.bodies[bi], &self.pool[oi], t) => {
                    Walk::Ticks(t.max(0))
                }
                _ => Walk::Unreached,
            };
        }

        match self.walks[at] {
            Walk::Ticks(t) => Some(t),
            _ => None,
        }
    }

    fn seated(&self, oi: usize) -> bool {
        matches!(self.pool[oi].capacity, Capacity::Seated { .. })
    }

    fn garrison(&self, bi: usize, oi: usize) -> bool {
        pool::garrisons(&self.bodies[bi], &self.pool[oi])
    }

    fn posted(&mut self, bi: usize, oi: usize) -> bool {
        self.garrison(bi, oi)
            && match &self.pool[oi].post {
                Some(post) => self.bodies[bi].pos.within(0, post),
                None => false,
            }
    }

    fn ahead(&mut self, oi: usize, a: usize, b: usize) -> bool {
        let wa = self.walk(a, oi);
        let wb = self.walk(b, oi);
        let va = wa.map(|w| pool::seat_value(&self.bodies[a], &self.pool[oi], w));
        let vb = wb.map(|w| pool::seat_value(&self.bodies[b], &self.pool[oi], w));
        let pa = self.posted(a, oi);
        let pb = self.posted(b, oi);

        (pa && !pb)
            || (pa == pb && va > vb)
            || (pa == pb
                && va == vb
                && (wa < wb || (wa == wb && self.bodies[a].name < self.bodies[b].name)))
    }

    /// So a successor walks out while its predecessor still works.
    fn outlasts(&mut self, holder: usize, bi: usize, oi: usize) -> bool {
        let ttl = self.bodies[holder].ticks_to_live;
        self.walk(bi, oi).is_none_or(|w| ttl > w)
    }

    fn admits_over(&mut self, bi: usize, oi: usize, ahead_only: bool) -> bool {
        let mut seated_alike = 0;
        let mut sum = 0;

        for b in 0..self.bodies.len() {
            if self.assigned[b] == Some(oi)
                && b != bi
                && self.outlasts(b, bi, oi)
                && (!ahead_only || self.ahead(oi, b, bi))
            {
                sum += pool::share(&self.bodies[b], &self.pool[oi]);

                if self.garrison(b, oi) == self.garrison(bi, oi) {
                    seated_alike += 1;
                }
            }
        }

        pool::admits(&self.pool[oi], self.garrison(bi, oi), sum, seated_alike)
    }

    fn admits_now(&mut self, bi: usize, oi: usize) -> bool {
        if self.seated(oi) {
            self.admits_over(bi, oi, false)
        } else {
            pool::admits(&self.pool[oi], false, self.held[oi], 0)
        }
    }

    fn admits_ahead(&mut self, bi: usize, oi: usize) -> bool {
        self.admits_over(bi, oi, true)
    }

    fn take(&mut self, bi: usize, oi: usize) {
        self.assigned[bi] = Some(oi);
        self.held[oi] += pool::share(&self.bodies[bi], &self.pool[oi]);
        self.holders[oi] += 1;
    }

    fn release(&mut self, bi: usize) {
        let Some(oi) = self.assigned[bi].take() else {
            return;
        };
        self.held[oi] -= pool::share(&self.bodies[bi], &self.pool[oi]);
        self.holders[oi] -= 1;
    }

    fn open(&mut self, bi: usize, oi: usize) -> bool {
        self.admits_now(bi, oi) || (self.seated(oi) && self.admits_ahead(bi, oi))
    }

    /// Cheapest floor first, so the walks past the best are never flooded; a
    /// tie goes to the offer first in rank and pool order.
    fn score(&mut self, bi: usize, ranked: &[usize]) {
        self.best_offer[bi] = None;
        let body = &self.bodies[bi];
        let mut order: Vec<(i32, usize)> = ranked
            .iter()
            .enumerate()
            .map(|(k, &oi)| {
                let offer = &self.pool[oi];
                (
                    price::walked(offer.priority, (self.floor)(body, offer).max(0)),
                    k,
                )
            })
            .collect();
        order.sort_unstable();

        for (floor, k) in order {
            let oi = ranked[k];
            if self.best_offer[bi].is_some() && floor > self.best_cost[bi] {
                break;
            }
            let offer = &self.pool[oi];

            if pool::fits(&self.bodies[bi], offer) && self.open(bi, oi) {
                let holder = self.holders[oi];
                let earlier =
                    self.best_offer[bi].is_none_or(|b| self.rank_of(oi) < self.rank_of(b));

                if let Some(w) = self.walk(bi, oi)
                    && (self.best_offer[bi].is_none()
                        || before(
                            (price::walked(offer.priority, w), w, holder),
                            (self.best_cost[bi], self.best_walk[bi], self.best_load[bi]),
                        )
                        || earlier
                            && (price::walked(offer.priority, w), w, holder)
                                == (self.best_cost[bi], self.best_walk[bi], self.best_load[bi]))
                {
                    self.best_offer[bi] = Some(oi);
                    self.best_cost[bi] = price::walked(offer.priority, w);
                    self.best_walk[bi] = w;
                    self.best_load[bi] = holder;
                }
            }
        }
    }

    fn outranked(&mut self, bi: usize, oi: usize, deadlines: &[usize]) -> bool {
        deadlines.iter().any(|&d| {
            self.pool[d].priority < self.pool[oi].priority
                && pool::fits(&self.bodies[bi], &self.pool[d])
                && self.walk(bi, d).is_some()
                && self.open(bi, d)
        })
    }

    fn rank_of(&self, oi: usize) -> (i32, usize) {
        (self.pool[oi].priority, oi)
    }

    fn first(&self, a: usize, b: usize) -> bool {
        let ka = (self.best_cost[a], self.best_walk[a], self.best_load[a]);
        let kb = (self.best_cost[b], self.best_walk[b], self.best_load[b]);

        before(ka, kb) || (ka == kb && self.bodies[a].name < self.bodies[b].name)
    }

    fn run(&mut self, assignments: &Assignments) -> Assignments {
        let offers = self.pool.len();

        // Lightest first keeps whole a set the match admitted while nothing
        // changed; a garrison on its post first, so a newcomer never displaces it.
        let mut kept: Vec<(bool, i32, i32, usize, usize)> = Vec::new();

        for bi in 0..self.bodies.len() {
            let Some(id) = assignments.get(&self.bodies[bi].name) else {
                continue;
            };
            let Some(oi) = self.by_id.get(id.as_str()).copied() else {
                continue;
            };

            if pool::fits(&self.bodies[bi], &self.pool[oi])
                && let Some(w) = self.walk(bi, oi)
            {
                let key = if self.seated(oi) {
                    -pool::seat_value(&self.bodies[bi], &self.pool[oi], w)
                } else {
                    pool::share(&self.bodies[bi], &self.pool[oi])
                };
                kept.push((!self.posted(bi, oi), key, w, bi, oi));
            }
        }

        kept.sort_by(|a, b| {
            if a.0 != b.0 {
                a.0.cmp(&b.0)
            } else if a.1 != b.1 {
                a.1.cmp(&b.1)
            } else if a.2 != b.2 {
                a.2.cmp(&b.2)
            } else {
                self.bodies[a.3].name.cmp(&self.bodies[b.3].name)
            }
        });

        // A controller inside its deadline is worked before any held task.
        let deadlines: Vec<usize> = (0..offers)
            .filter(|oi| self.pool[*oi].priority <= price::DEADLINE_RANK)
            .collect();

        for (_, _, _, bi, oi) in kept.clone() {
            if self.admits_now(bi, oi) && !self.outranked(bi, oi, &deadlines) {
                self.take(bi, oi);
            }
        }

        // Best rank first, the pool's order within one, so no offer is walked
        // whose rank alone costs more than the best reached.
        let mut ranked: Vec<usize> = (0..offers).collect();
        ranked.sort_by_key(|oi| (self.pool[*oi].priority, *oi));

        for bi in 0..self.bodies.len() {
            if self.assigned[bi].is_none() {
                self.score(bi, &ranked);
            }
        }

        // So a contested Task goes to the body that reaches it cheapest.
        let mut go = true;

        while go {
            let mut pick: Option<usize> = None;

            for bi in 0..self.bodies.len() {
                if self.assigned[bi].is_none()
                    && self.best_offer[bi].is_some()
                    && (pick.is_none() || self.first(bi, pick.unwrap()))
                {
                    pick = Some(bi);
                }
            }

            let Some((pick, oi)) = pick.and_then(|p| Some((p, self.best_offer[p]?))) else {
                go = false;
                continue;
            };

            self.take(pick, oi);

            let mut released: Vec<usize> = Vec::new();

            if self.seated(oi) {
                let mut seated_here: Vec<usize> = (0..self.bodies.len())
                    .filter(|b| self.assigned[*b] == Some(oi))
                    .collect();

                seated_here.sort_by(|a, b| {
                    if self.ahead(oi, *a, *b) {
                        Ordering::Less
                    } else if self.ahead(oi, *b, *a) {
                        Ordering::Greater
                    } else {
                        Ordering::Equal
                    }
                });

                let mut admitted: Vec<usize> = Vec::new();

                for b in seated_here {
                    let over: Vec<usize> = admitted
                        .iter()
                        .copied()
                        .filter(|a| self.outlasts(*a, b, oi))
                        .collect();

                    let sum: i32 = over
                        .iter()
                        .map(|a| pool::share(&self.bodies[*a], &self.pool[oi]))
                        .sum();

                    let alike = over
                        .iter()
                        .filter(|a| self.garrison(**a, oi) == self.garrison(b, oi))
                        .count() as i32;

                    if pool::admits(&self.pool[oi], self.garrison(b, oi), sum, alike) {
                        admitted.push(b);
                    } else {
                        released.push(b);
                    }
                }

                for b in &released {
                    self.release(*b);
                }
            }

            for bi in 0..self.bodies.len() {
                if self.assigned[bi].is_none()
                    && (self.best_offer[bi] == Some(oi) || !released.is_empty())
                {
                    self.score(bi, &ranked);
                }
            }
        }

        let mut out = Assignments::new();

        for bi in 0..self.bodies.len() {
            if let Some(oi) = self.assigned[bi] {
                out.insert(self.bodies[bi].name.clone(), self.pool[oi].id.clone());
            }
        }

        out
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::core::economy::bodies::Pattern;
    use crate::core::economy::dice::{self, Dice};

    /// A pool, the bodies, a walk per pair (`None` unreachable) and last tick's
    /// assignments, some stale.
    struct Scene {
        bodies: Vec<Body>,
        pool: Vec<Offer>,
        walks: Vec<Option<i32>>,
        assignments: Assignments,
    }

    impl Scene {
        fn travel(&self, body: &Body, offer: &Offer) -> Option<i32> {
            let bi = self
                .bodies
                .iter()
                .position(|b| b.name == body.name)
                .unwrap();
            let oi = self.pool.iter().position(|o| o.id == offer.id).unwrap();
            self.walks[bi * self.pool.len() + oi]
        }
    }

    fn scene(dice: &mut Dice) -> Scene {
        let nb = dice.between(0, 14);
        let no = dice.between(0, 18);
        let bodies: Vec<Body> = (0..nb as usize).map(|i| dice::body(dice, i)).collect();
        let pool: Vec<Offer> = (0..no as usize).map(|i| dice::offer(dice, i)).collect();

        let walks: Vec<Option<i32>> = (0..bodies.len() * pool.len())
            .map(|_| {
                if dice.chance(7) {
                    None
                } else {
                    Some(dice.between(0, 60))
                }
            })
            .collect();

        let ids: Vec<String> = pool.iter().map(|o| o.id.clone()).collect();

        let mut assignments = Assignments::new();

        for body in &bodies {
            let held = match dice.below(7) {
                0 | 1 => None,
                2..=5 => {
                    if ids.is_empty() {
                        None
                    } else {
                        Some(ids[dice.below(ids.len() as i32) as usize].clone())
                    }
                }
                _ => Some("h:gone".to_string()),
            };

            if let Some(id) = held {
                assignments.insert(body.name.clone(), id);
            }
        }

        Scene {
            bodies,
            pool,
            walks,
            assignments,
        }
    }

    fn matched(scene: &Scene, assignments: &Assignments) -> Assignments {
        let mut travel = |body: &Body, offer: &Offer| scene.travel(body, offer);
        match_all(
            &mut travel,
            &|_, _| 0,
            &scene.pool,
            &scene.bodies,
            assignments,
        )
    }

    /// Whether a body lives to take an offer.
    fn reaches(scene: &Scene, body: &Body, offer: &Offer) -> bool {
        scene
            .travel(body, offer)
            .is_some_and(|t| pool::outlives(body, offer, t))
    }

    fn limit_of(capacity: Capacity) -> Option<i32> {
        match capacity {
            Capacity::Unbounded => None,
            Capacity::Bodies(n) | Capacity::Work(n) | Capacity::Load(n) => Some(n),
            Capacity::Seated { work, .. } => Some(work),
        }
    }

    /// Whether `offer` is filled past its last holder: the holders that are
    /// there when each holder arrives, less the largest of them, fit its limit.
    fn within_capacity(scene: &Scene, out: &Assignments, offer: &Offer) -> bool {
        let holders: Vec<&Body> = scene
            .bodies
            .iter()
            .filter(|b| out.get(&b.name) == Some(&offer.id))
            .collect();

        let arrives = |b: &Body| scene.travel(b, offer).unwrap_or(0).max(0);

        match limit_of(offer.capacity) {
            None => true,
            Some(limit) => holders.iter().all(|b| {
                let shares: Vec<i32> = holders
                    .iter()
                    .filter(|a| arrives(a) <= arrives(b) && a.ticks_to_live > arrives(b))
                    .map(|a| pool::share(a, offer))
                    .collect();

                !shares.is_empty()
                    && shares.iter().sum::<i32>() - shares.iter().max().copied().unwrap_or(0)
                        < limit
            }),
        }
    }

    fn capacity_held(scene: &Scene) -> bool {
        let out = matched(scene, &scene.assignments);
        scene
            .pool
            .iter()
            .all(|offer| within_capacity(scene, &out, offer))
    }

    fn scenes(seed: u64) -> Vec<Scene> {
        let mut dice = Dice::new(seed);
        (0..300).map(|_| scene(&mut dice)).collect()
    }

    #[test]
    fn no_capacity_is_filled_past_its_last_holder() {
        for scene in scenes(6954441170866046493) {
            assert!(capacity_held(&scene));
        }
    }

    #[test]
    fn at_no_tick_does_a_seated_task_hold_more_garrisons_than_its_posts_nor_other_bodies_than_its_other_seats()
     {
        for scene in scenes(11) {
            let out = matched(&scene, &scene.assignments);

            for offer in &scene.pool {
                let Capacity::Seated { seats, posts, .. } = offer.capacity else {
                    continue;
                };

                let holders: Vec<&Body> = scene
                    .bodies
                    .iter()
                    .filter(|b| out.get(&b.name) == Some(&offer.id))
                    .collect();

                let arrives = |b: &Body| scene.travel(b, offer).unwrap_or(0).max(0);

                for b in &holders {
                    let present = holders
                        .iter()
                        .filter(|a| {
                            pool::garrisons(a, offer) == pool::garrisons(b, offer)
                                && arrives(a) <= arrives(b)
                                && a.ticks_to_live > arrives(b)
                        })
                        .count() as i32;

                    let room = if pool::garrisons(b, offer) {
                        posts
                    } else {
                        seats - posts
                    };

                    assert!(present <= room, "{} {:?}", offer.id, offer.capacity);
                }
            }
        }
    }

    #[test]
    fn every_assigned_task_is_offered_fits_the_body_and_is_reached_in_its_life() {
        for scene in scenes(12) {
            let out = matched(&scene, &scene.assignments);

            for b in &scene.bodies {
                if let Some(id) = out.get(&b.name) {
                    match scene.pool.iter().find(|o| &o.id == id) {
                        Some(offer) => {
                            assert!(pool::fits(b, offer));
                            assert!(reaches(&scene, b, offer));
                        }
                        None => panic!("{id} is offered"),
                    }
                }
            }
        }
    }

    #[test]
    fn a_match_does_not_depend_on_the_order_of_the_bodies() {
        let mut dice = Dice::new(13);

        for scene in scenes(14) {
            let mut shuffled = scene.bodies.clone();
            for i in (1..shuffled.len()).rev() {
                let j = dice.below(i as i32 + 1) as usize;
                shuffled.swap(i, j);
            }

            let mut travel = |body: &Body, offer: &Offer| scene.travel(body, offer);
            let permuted = match_all(
                &mut travel,
                &|_, _| 0,
                &scene.pool,
                &shuffled,
                &scene.assignments,
            );

            assert_eq!(permuted, matched(&scene, &scene.assignments));
        }
    }

    #[test]
    fn a_match_matched_again_is_unchanged() {
        for scene in scenes(15) {
            let once = matched(&scene, &scene.assignments);
            assert_eq!(matched(&scene, &once), once);
        }
    }

    #[test]
    fn a_valid_assignment_to_an_unbounded_task_is_kept() {
        for scene in scenes(16) {
            let out = matched(&scene, &scene.assignments);

            for b in &scene.bodies {
                if let Some(id) = scene.assignments.get(&b.name)
                    && let Some(offer) = scene.pool.iter().find(|o| &o.id == id)
                    && offer.capacity == Capacity::Unbounded
                    && pool::fits(b, offer)
                    && reaches(&scene, b, offer)
                    && !scene.pool.iter().any(|d| {
                        d.priority <= price::DEADLINE_RANK
                            && d.priority < offer.priority
                            && pool::fits(b, d)
                            && reaches(&scene, b, d)
                    })
                {
                    assert_eq!(out.get(&b.name), Some(id));
                }
            }
        }
    }

    #[test]
    fn a_body_left_unmatched_has_no_offer_it_could_still_take() {
        for scene in scenes(17) {
            let out = matched(&scene, &scene.assignments);

            let held = |offer: &Offer| -> i32 {
                scene
                    .bodies
                    .iter()
                    .map(|b| {
                        if out.get(&b.name) == Some(&offer.id) {
                            pool::share(b, offer)
                        } else {
                            0
                        }
                    })
                    .sum()
            };

            let seated_alike = |body: &Body, offer: &Offer| -> i32 {
                scene
                    .bodies
                    .iter()
                    .filter(|b| {
                        out.get(&b.name) == Some(&offer.id)
                            && pool::garrisons(b, offer) == pool::garrisons(body, offer)
                    })
                    .count() as i32
            };

            for b in &scene.bodies {
                if out.contains_key(&b.name) {
                    continue;
                }

                for o in &scene.pool {
                    let reachable = pool::fits(b, o)
                        && pool::admits(o, pool::garrisons(b, o), held(o), seated_alike(b, o))
                        && reaches(&scene, b, o);

                    assert!(!reachable, "{} could take {}", b.name, o.id);
                }
            }
        }
    }

    #[test]
    fn an_upgrader_fits_only_an_upgrade() {
        for scene in scenes(18) {
            for b in &scene.bodies {
                let upgrader = Body {
                    pattern: Pattern::Upgrader,
                    ..b.clone()
                };

                for o in &scene.pool {
                    assert!(!pool::fits(&upgrader, o) || o.task.is_upgrade(), "{}", o.id);
                }
            }
        }
    }
}
