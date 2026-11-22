/// Who is out there (`ARCHITECTURE.md` §6.1).
use std::collections::{BTreeMap, BTreeSet};

use super::shape::Group;
use fabot_base::world::{Creep, Id, Pos, RoomName, hostiles};

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Event {
    Newcomer(i32),
    /// Some of `from`'s members stand in `into`, apart from its largest fragment.
    Split {
        from: i32,
        into: i32,
    },
    /// `from` ended inside the older `into`.
    Merged {
        into: i32,
        from: i32,
    },
    /// Some of its members are seen nowhere now (dead, or out of sight).
    Left(i32),
}

/// Ranged reach plus one step.
pub const REACH: i32 = 4;

/// The owner's starting value, to be tuned.
pub const LOST_GROUP_TICKS: i32 = 30;

/// A seam's two tiles are one apart.
pub fn world_of(pos: &Pos) -> (i32, i32) {
    let (rx, ry) = pos.room.coords();
    (rx * 50 + pos.x, ry * 50 + pos.y)
}

pub fn linked(a: &Pos, b: &Pos) -> bool {
    if a.room == b.room {
        a.within(REACH, b)
    } else {
        let (ax, ay) = world_of(a);
        let (bx, by) = world_of(b);
        (ax - bx).abs() <= 1 && (ay - by).abs() <= 1
    }
}

fn find(root: &mut [usize], mut i: usize) -> usize {
    loop {
        let up = root[i];
        if up == i {
            return i;
        }
        let grand = root[up];
        root[i] = grand;
        i = grand;
    }
}

fn clusters<'a>(owned: &BTreeSet<RoomName>, hostiles: &'a [Creep]) -> Vec<Vec<&'a Creep>> {
    let mut sorted: Vec<&Creep> = hostiles.iter().collect();
    sorted.sort_by_key(|a| a.id);
    let n = sorted.len();
    let mut root: Vec<usize> = (0..n).collect();
    let armed: Vec<bool> = sorted
        .iter()
        .map(|c| hostiles::armed(owned, hostiles, c))
        .collect();

    for (i, and) in armed.iter().enumerate() {
        if *and {
            for j in i + 1..n {
                if armed[j] && linked(&sorted[i].pos, &sorted[j].pos) {
                    let a = find(&mut root, i);
                    let b = find(&mut root, j);
                    if a != b {
                        root[a.max(b)] = a.min(b);
                    }
                }
            }
        }
    }

    let mut by_root: BTreeMap<usize, usize> = BTreeMap::new();
    let mut order: Vec<Vec<&Creep>> = Vec::new();

    for (i, creep) in sorted.iter().enumerate() {
        let r = find(&mut root, i);
        if let Some(&at) = by_root.get(&r) {
            order[at].push(creep)
        } else {
            by_root.insert(r, order.len());
            order.push(vec![creep]);
        }
    }

    order
}

/// The member nearest the members' mean, so the centre always stands in a seen room.
fn centre(members: &[&Creep]) -> Pos {
    let n = members.len() as i32;
    let mut sx = 0;
    let mut sy = 0;

    for m in members {
        let (x, y) = world_of(&m.pos);
        sx += x;
        sy += y;
    }

    let mut best = members[0].pos;
    let mut best_d = i32::MAX;

    for m in members {
        let (x, y) = world_of(&m.pos);
        let d = (n * x - sx).abs().max((n * y - sy).abs());

        if d < best_d {
            best = m.pos;
            best_d = d;
        }
    }

    best
}

/// `held`: the highest group id an engagement holds; a new id is past it and
/// every group's, so no id is reused while anything holds it.
pub fn step(
    grace: i32,
    held: i32,
    owned: &BTreeSet<RoomName>,
    prev: &[Group],
    hostiles: &[Creep],
    tick: i32,
) -> (Vec<Group>, Vec<Event>) {
    let mut prev = prev.to_vec();
    prev.sort_by_key(|g| g.id);
    let cs = clusters(owned, hostiles);
    let mut owner: BTreeMap<Id, usize> = BTreeMap::new();

    for (gi, g) in prev.iter().enumerate() {
        for m in &g.members {
            owner.insert(*m, gi);
        }
    }

    let overlap: Vec<Vec<i32>> = cs
        .iter()
        .map(|c| {
            let mut o = vec![0i32; prev.len()];

            for m in c {
                if let Some(&gi) = owner.get(&m.id) {
                    o[gi] += 1;
                }
            }

            o
        })
        .collect();

    let best: Vec<Option<usize>> = (0..prev.len())
        .map(|gi| {
            let mut b = None;
            let mut most = 0;

            for (ci, row) in overlap.iter().enumerate() {
                if row[gi] > most {
                    b = Some(ci);
                    most = row[gi];
                }
            }

            b
        })
        .collect();

    let mut next_id = prev.iter().fold(held, |m, g| m.max(g.id)) + 1;

    let mut groups: Vec<Group> = Vec::new();
    let mut events: Vec<Event> = Vec::new();

    for ci in 0..cs.len() {
        let mut kept = None;

        for gi in 0..prev.len() {
            if best[gi] == Some(ci) {
                match kept {
                    None => kept = Some(prev[gi].id),
                    Some(into) => events.push(Event::Merged {
                        into,
                        from: prev[gi].id,
                    }),
                }
            }
        }

        let id = kept.unwrap_or_else(|| {
            next_id += 1;
            next_id - 1
        });

        let mut seen_before = false;

        for gi in 0..prev.len() {
            if overlap[ci][gi] > 0 {
                seen_before = true;

                if best[gi] != Some(ci) {
                    events.push(Event::Split {
                        from: prev[gi].id,
                        into: id,
                    });
                }
            }
        }

        if !seen_before {
            events.push(Event::Newcomer(id));
        }

        let c = centre(&cs[ci]);

        groups.push(Group {
            id,
            members: cs[ci].iter().map(|m| m.id).collect(),
            last_seen: tick,
            centre: c,
        });
    }

    let seen: BTreeSet<&Id> = hostiles.iter().map(|h| &h.id).collect();

    for (g, best) in prev.iter().zip(&best) {
        if best.is_some() && g.members.iter().any(|m| !seen.contains(m)) {
            events.push(Event::Left(g.id));
        }
        if best.is_none() && tick - g.last_seen <= grace {
            groups.push(g.clone());
        }
    }

    groups.sort_by_key(|g| g.id);
    (groups, events)
}

#[cfg(test)]
mod tests {
    use super::*;
    use fabot_base::vocabulary::Part;
    use fabot_base::world::{BodyPart, Owner, Store};

    const GRACE: i32 = 3;

    fn creep(id: &str, pos: Pos, armed: bool) -> Creep {
        Creep {
            id: id.into(),
            name: id.to_string(),
            owner: Owner::Stranger("rival".to_string()),
            pos,
            body: vec![BodyPart {
                kind: if armed {
                    Part::RangedAttack
                } else {
                    Part::Work
                },
                hits: 100,
                boost: None,
            }],
            hits: 100,
            hits_max: 100,
            ticks_to_live: 1000,
            fatigue: 0,
            store: Store::empty(),
        }
    }

    fn at(room: RoomName, x: i32, y: i32) -> Pos {
        Pos::new(room, x, y)
    }

    /// A seeded xorshift stands in for FsCheck's generator: same shapes, one history a draw.
    struct Rng(u64);

    impl Rng {
        fn next(&mut self) -> u64 {
            self.0 ^= self.0 << 13;
            self.0 ^= self.0 >> 7;
            self.0 ^= self.0 << 17;
            self.0
        }

        fn below(&mut self, n: u64) -> u64 {
            self.next() % n
        }

        /// A whole in `lo..=hi`.
        fn between(&mut self, lo: i32, hi: i32) -> i32 {
            lo + self.below((hi - lo + 1) as u64) as i32
        }

        fn pick<T: Clone>(&mut self, of: &[T]) -> T {
            of[self.below(of.len() as u64) as usize].clone()
        }

        /// `Gen.frequency [(yes, true); (no, false)]`.
        fn chance(&mut self, yes: u64, no: u64) -> bool {
            self.below(yes + no) < yes
        }

        /// `Gen.subListOf`.
        fn subset<T: Clone>(&mut self, of: &[T]) -> Vec<T> {
            of.iter().filter(|_| self.chance(1, 1)).cloned().collect()
        }
    }

    fn pool() -> Vec<String> {
        (0..15).map(|i| format!("h{i}")).collect()
    }

    /// Two rooms sharing a seam, and tiles crowded near it so clusters touch.
    fn arrival(rng: &mut Rng, id: &str) -> Creep {
        let room = rng.pick(&[RoomName::W1N1, "W2N1".parse::<RoomName>().unwrap()]);
        let x = rng.pick(&[0, 1, 3, 6, 9, 43, 46, 48, 49]);
        let y = rng.between(0, 12);
        let armed = rng.chance(4, 1);
        creep(id, at(room, x, y), armed)
    }

    /// A tick later: most step a few tiles, some vanish, absent ids may arrive.
    fn next_gen(rng: &mut Rng, now: &[Creep]) -> Vec<Creep> {
        let mut out: Vec<Creep> = Vec::new();

        for c in now {
            let dx = rng.between(-3, 3);
            let dy = rng.between(-3, 3);
            let keep = rng.chance(6, 1);

            if keep {
                let mut stepped = c.clone();
                stepped.pos.x = (c.pos.x + dx).clamp(0, 49);
                stepped.pos.y = (c.pos.y + dy).clamp(0, 49);
                out.push(stepped);
            }
        }

        let absent: Vec<String> = pool()
            .into_iter()
            .filter(|id| now.iter().all(|c| c.id != Id::from(id.as_str())))
            .collect();
        let mut arriving = rng.subset(&absent);
        arriving.truncate(3);

        for id in arriving {
            out.push(arrival(rng, &id));
        }

        out
    }

    /// Groups after one to six ticks, the next tick's hostiles, and that tick.
    fn history(rng: &mut Rng) -> (Vec<Group>, Vec<Creep>, i32) {
        let n = rng.between(1, 6);
        let ids = rng.subset(&pool());
        let mut now: Vec<Creep> = ids.iter().map(|id| arrival(rng, id)).collect();
        let mut groups: Vec<Group> = Vec::new();

        let mut t = 1;
        while t <= n {
            now = next_gen(rng, &now);
            let (next, _) = step(GRACE, 0, &BTreeSet::new(), &groups, &now, t);
            groups = next;
            t += 1;
        }

        (groups, now, t)
    }

    fn seen(tick: i32, groups: &[Group]) -> Vec<&Group> {
        groups.iter().filter(|g| g.last_seen == tick).collect()
    }

    fn distance(a: &Pos, b: &Pos) -> i32 {
        let (ax, ay) = world_of(a);
        let (bx, by) = world_of(b);
        (ax - bx).abs().max((ay - by).abs())
    }

    const DRAWS: usize = 300;

    #[test]
    fn tiles_across_a_seam_link_only_when_adjacent() {
        let mut rng = Rng(0x9e37_79b9_7f4a_7c15);

        for _ in 0..DRAWS {
            let west = 49 - rng.between(0, 2);
            let east = rng.between(0, 2);
            let y = rng.between(1, 47);
            let dy = rng.between(-1, 1);
            let a = at("W2N1".parse::<RoomName>().unwrap(), west, y);
            let b = at(RoomName::W1N1, east, y + dy);
            assert_eq!(linked(&a, &b), (49 - west) + east < 1);
        }
    }

    #[test]
    fn ids_are_stable_under_a_permutation_of_the_input() {
        let mut rng = Rng(0x2545_f491_4f6c_dd1d);

        for _ in 0..DRAWS {
            let (prev, now, tick) = history(&mut rng);
            let mut shuffled_prev = prev.clone();
            let mut shuffled_now = now.clone();

            for i in (1..shuffled_prev.len()).rev() {
                shuffled_prev.swap(i, rng.below(i as u64 + 1) as usize);
            }

            for i in (1..shuffled_now.len()).rev() {
                shuffled_now.swap(i, rng.below(i as u64 + 1) as usize);
            }

            let (once, first) = step(GRACE, 0, &BTreeSet::new(), &prev, &now, tick);
            let (twice, second) = step(
                GRACE,
                0,
                &BTreeSet::new(),
                &shuffled_prev,
                &shuffled_now,
                tick,
            );

            assert_eq!(once, twice);
            assert_eq!(first, second);
        }
    }

    #[test]
    fn every_hostile_stands_in_exactly_one_group_and_no_id_twice() {
        let mut rng = Rng(0x853c_49e6_748f_ea9b);

        for _ in 0..DRAWS {
            let (prev, now, tick) = history(&mut rng);
            let (groups, _) = step(GRACE, 0, &BTreeSet::new(), &prev, &now, tick);
            let ids: Vec<i32> = groups.iter().map(|g| g.id).collect();
            let mut members: Vec<Id> = seen(tick, &groups)
                .iter()
                .flat_map(|g| g.members.iter().copied())
                .collect();
            members.sort();

            let mut distinct = ids.clone();
            distinct.sort_unstable();
            distinct.dedup();
            assert_eq!(distinct, ids);

            let mut hostiles: Vec<Id> = now.iter().map(|c| c.id).collect();
            hostiles.sort();
            assert_eq!(members, hostiles);
        }
    }

    #[test]
    fn a_group_is_one_cluster_reaching_no_other() {
        let mut rng = Rng(0x1b03_7f4a_7c15_9e37);

        for _ in 0..DRAWS {
            let (prev, now, tick) = history(&mut rng);
            let (all, _) = step(GRACE, 0, &BTreeSet::new(), &prev, &now, tick);
            let groups = seen(tick, &all);
            let at: BTreeMap<Id, &Creep> = now.iter().map(|c| (c.id, c)).collect();

            let positions =
                |g: &Group| -> Vec<&Creep> { g.members.iter().map(|m| at[m]).collect() };
            let armed_of = |c: &Creep| hostiles::armed(&BTreeSet::new(), &now, c);

            let narrow = |g: &Group| -> bool {
                let ps = positions(g);
                ps.iter().all(|a| {
                    ps.iter()
                        .all(|b| distance(&a.pos, &b.pos) <= REACH * (ps.len() as i32 - 1))
                })
            };

            let apart = |g: &Group, h: &Group| -> bool {
                g.id == h.id
                    || positions(g).iter().all(|a| {
                        positions(h)
                            .iter()
                            .all(|b| !(armed_of(a) && armed_of(b) && linked(&a.pos, &b.pos)))
                    })
            };

            let connected = |g: &Group| -> bool {
                let ps = positions(g);
                match ps.first() {
                    None => false,
                    Some(first) => {
                        let mut reached: BTreeSet<Id> = BTreeSet::new();
                        reached.insert(first.id);

                        loop {
                            let more: Vec<Id> = ps
                                .iter()
                                .filter(|b| {
                                    !reached.contains(&b.id)
                                        && ps.iter().any(|a| {
                                            reached.contains(&a.id)
                                                && armed_of(a)
                                                && armed_of(b)
                                                && linked(&a.pos, &b.pos)
                                        })
                                })
                                .map(|c| c.id)
                                .collect();

                            if more.is_empty() {
                                break;
                            }

                            reached.extend(more);
                        }

                        reached.len() == ps.len()
                    }
                }
            };

            for g in &groups {
                assert!(narrow(g) && connected(g));

                for h in &groups {
                    assert!(apart(g, h));
                }
            }
        }
    }

    #[test]
    fn a_new_id_is_past_every_id_held() {
        let mut rng = Rng(0x94d0_49bb_1331_11eb);

        for _ in 0..DRAWS {
            let (prev, now, tick) = history(&mut rng);
            let engaged = rng.between(0, 19);
            let held: BTreeSet<i32> = prev.iter().map(|g| g.id).collect();
            let (groups, events) = step(GRACE, engaged, &BTreeSet::new(), &prev, &now, tick);

            for e in &events {
                if let Event::Newcomer(id) = e {
                    assert!(*id > engaged);
                    assert!(held.iter().all(|h| id > h));
                }
            }

            assert!(
                groups
                    .iter()
                    .all(|g| held.contains(&g.id) || g.last_seen == tick)
            );
        }
    }

    #[test]
    fn a_split_keeps_the_id_on_the_largest_fragment() {
        let mut rng = Rng(0xc0ff_ee00_1234_5678);

        for _ in 0..DRAWS {
            let (prev, now, tick) = history(&mut rng);
            let (groups, events) = step(GRACE, 0, &BTreeSet::new(), &prev, &now, tick);
            let by_id: BTreeMap<i32, &Group> = groups.iter().map(|g| (g.id, g)).collect();
            let before: BTreeMap<i32, &BTreeSet<Id>> =
                prev.iter().map(|g| (g.id, &g.members)).collect();

            let share = |g: &Group, members: &BTreeSet<Id>| -> usize {
                g.members.intersection(members).count()
            };

            for e in &events {
                if let Event::Split { from, into } = e {
                    let fragment = share(by_id[into], before[from]);
                    assert!(fragment > 0);

                    if let Some(kept) = by_id.get(from) {
                        assert!(share(kept, before[from]) >= fragment);
                    }
                }
            }
        }
    }

    #[test]
    fn a_group_that_loses_a_member_from_sight_says_so_once() {
        let mut rng = Rng(0x0bad_c0de_dead_beef);

        for _ in 0..DRAWS {
            let (prev, now, tick) = history(&mut rng);
            let (_, events) = step(GRACE, 0, &BTreeSet::new(), &prev, &now, tick);
            let here: BTreeSet<&Id> = now.iter().map(|c| &c.id).collect();

            for g in &prev {
                let seen = g.members.iter().any(|m| here.contains(m));
                let vanished = g.members.iter().any(|m| !here.contains(m));
                let said = events.iter().filter(|e| **e == Event::Left(g.id)).count();
                assert_eq!(said, usize::from(seen && vanished));
            }
        }
    }

    #[test]
    fn a_merge_keeps_the_older_id() {
        let mut rng = Rng(0x1234_5678_9abc_def1);

        for _ in 0..DRAWS {
            let (prev, now, tick) = history(&mut rng);
            let (groups, events) = step(GRACE, 0, &BTreeSet::new(), &prev, &now, tick);
            let ids: BTreeSet<i32> = groups.iter().map(|g| g.id).collect();

            for e in &events {
                if let Event::Merged { into, from } = e {
                    assert!(into < from);
                    assert!(ids.contains(into));
                    assert!(!ids.contains(from));
                }
            }
        }
    }

    #[test]
    fn an_unseen_group_is_kept_for_the_grace_then_dropped() {
        let mut rng = Rng(0xfeed_face_cafe_0001);

        for _ in 0..DRAWS {
            let (prev, now, tick) = history(&mut rng);
            let (groups, _) = step(GRACE, 0, &BTreeSet::new(), &prev, &now, tick);

            assert!(groups.iter().all(|g| tick - g.last_seen <= GRACE));

            for g in &prev {
                assert!(
                    tick - g.last_seen > GRACE
                        || now.iter().any(|c| g.members.contains(&c.id))
                        || groups.iter().any(|k| k.id == g.id)
                );
            }
        }
    }
}
