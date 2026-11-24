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
        a.crow_flies(b) <= 1
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
fn centre<'a>(members: &[&'a Creep]) -> Pos {
    let n = members.len() as i32;
    let (sx, sy) = (members.iter()).fold((0, 0), |(sx, sy), m| {
        let (x, y) = world_of(&m.pos);
        (sx + x, sy + y)
    });
    let off = |m: &Creep| {
        let (x, y) = world_of(&m.pos);
        (n * x - sx).abs().max((n * y - sy).abs())
    };
    let nearer = |a: &'a Creep, b: &'a Creep| if off(b) < off(a) { b } else { a };
    (members.iter().copied()).fold(members[0], nearer).pos
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
    use proptest::bool::weighted;
    use proptest::collection::vec;
    use proptest::prelude::*;
    use proptest::sample::{Index, select};

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

    type Arrival = (RoomName, i32, i32, bool);

    #[derive(Clone, Debug)]
    struct Turn {
        dx: i32,
        dy: i32,
        stays: bool,
        arrives: bool,
        arrival: Arrival,
    }

    const POOL: usize = 15;
    const CASES: u32 = 256;

    /// Two rooms sharing a seam, and tiles crowded near it so clusters touch.
    fn arrival() -> impl Strategy<Value = Arrival> {
        let rooms = vec![RoomName::W1N1, "W2N1".parse::<RoomName>().unwrap()];
        let x = select(vec![0, 1, 3, 6, 9, 43, 46, 48, 49]);
        (select(rooms), x, 0..=12, weighted(0.8))
    }

    /// Who is there first, then one to six ticks of every pool id's turn.
    fn turns() -> impl Strategy<Value = Vec<Vec<Turn>>> {
        let turn = (
            -3..=3,
            -3..=3,
            weighted(6.0 / 7.0),
            any::<bool>(),
            arrival(),
        )
            .prop_map(|(dx, dy, stays, arrives, arrival)| Turn {
                dx,
                dy,
                stays,
                arrives,
                arrival,
            });
        vec(vec(turn, POOL), 2..=7)
    }

    fn arrive(i: usize, (room, x, y, armed): &Arrival) -> (usize, Creep) {
        (i, creep(&format!("h{i}"), at(*room, *x, *y), *armed))
    }

    /// A tick later: most step a few tiles, some vanish, up to three absent ids arrive.
    fn next_gen(turn: &[Turn], now: &[(usize, Creep)]) -> Vec<(usize, Creep)> {
        let mut out: Vec<(usize, Creep)> = (now.iter())
            .filter(|(i, _)| turn[*i].stays)
            .map(|(i, c)| {
                let mut stepped = c.clone();
                stepped.pos.x = (c.pos.x + turn[*i].dx).clamp(0, 49);
                stepped.pos.y = (c.pos.y + turn[*i].dy).clamp(0, 49);
                (*i, stepped)
            })
            .collect();
        let absent = (0..POOL).filter(|i| now.iter().all(|(j, _)| i != j));
        let arriving = absent.filter(|i| turn[*i].arrives).take(3);
        out.extend(arriving.map(|i| arrive(i, &turn[i].arrival)));
        out
    }

    /// Groups after one to six ticks, the next tick's hostiles, and that tick.
    fn history(turns: &[Vec<Turn>]) -> (Vec<Group>, Vec<Creep>, i32) {
        let first = (0..POOL).filter(|i| turns[0][*i].arrives);
        let mut now: Vec<(usize, Creep)> = first.map(|i| arrive(i, &turns[0][i].arrival)).collect();
        let mut groups: Vec<Group> = Vec::new();

        for (t, turn) in (1..).zip(&turns[1..]) {
            let creeps: Vec<Creep> = now.iter().map(|(_, c)| c.clone()).collect();
            groups = step(GRACE, 0, &BTreeSet::new(), &groups, &creeps, t).0;
            now = next_gen(turn, &now);
        }

        let now = now.into_iter().map(|(_, c)| c).collect();
        (groups, now, turns.len() as i32)
    }

    fn seen(tick: i32, groups: &[Group]) -> Vec<&Group> {
        groups.iter().filter(|g| g.last_seen == tick).collect()
    }

    fn shuffled<T: Clone>(of: &[T], order: &[Index]) -> Vec<T> {
        let mut out = of.to_vec();
        for i in (1..out.len()).rev() {
            out.swap(i, order[i % order.len()].index(i + 1));
        }
        out
    }

    proptest! {
        #![proptest_config(fabot_base::testing::config(CASES))]

        #[test]
        fn tiles_across_a_seam_link_only_when_adjacent(
            west in 47..=49,
            east in 0..=2,
            y in 1..=47,
            dy in -1..=1,
        ) {
            let a = at("W2N1".parse::<RoomName>().unwrap(), west, y);
            let b = at(RoomName::W1N1, east, y + dy);
            prop_assert_eq!(linked(&a, &b), (49 - west) + east < 1);
        }

        #[test]
        fn ids_are_stable_under_a_permutation_of_the_input(
            turns in turns(),
            prev_order in vec(any::<Index>(), 32),
            now_order in vec(any::<Index>(), 32),
        ) {
            let (prev, now, tick) = history(&turns);
            let shuffled_prev = shuffled(&prev, &prev_order);
            let shuffled_now = shuffled(&now, &now_order);

            let (once, first) = step(GRACE, 0, &BTreeSet::new(), &prev, &now, tick);
            let (twice, second) = step(
                GRACE,
                0,
                &BTreeSet::new(),
                &shuffled_prev,
                &shuffled_now,
                tick,
            );

            prop_assert_eq!(once, twice);
            prop_assert_eq!(first, second);
        }

        #[test]
        fn every_hostile_stands_in_exactly_one_group_and_no_id_twice(turns in turns()) {
            let (prev, now, tick) = history(&turns);
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
            prop_assert_eq!(distinct, ids);

            let mut hostiles: Vec<Id> = now.iter().map(|c| c.id).collect();
            hostiles.sort();
            prop_assert_eq!(members, hostiles);
        }

        #[test]
        fn a_group_is_one_cluster_reaching_no_other(turns in turns()) {
            let (prev, now, tick) = history(&turns);
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
                        .all(|b| a.pos.crow_flies(&b.pos) <= REACH * (ps.len() as i32 - 1))
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
                prop_assert!(narrow(g) && connected(g));

                for h in &groups {
                    prop_assert!(apart(g, h));
                }
            }
        }

        #[test]
        fn a_new_id_is_past_every_id_held(turns in turns(), engaged in 0..=19) {
            let (prev, now, tick) = history(&turns);
            let held: BTreeSet<i32> = prev.iter().map(|g| g.id).collect();
            let (groups, events) = step(GRACE, engaged, &BTreeSet::new(), &prev, &now, tick);

            for e in &events {
                if let Event::Newcomer(id) = e {
                    prop_assert!(*id > engaged);
                    prop_assert!(held.iter().all(|h| id > h));
                }
            }

            prop_assert!(
                groups
                    .iter()
                    .all(|g| held.contains(&g.id) || g.last_seen == tick)
            );
        }

        #[test]
        fn a_split_keeps_the_id_on_the_largest_fragment(turns in turns()) {
            let (prev, now, tick) = history(&turns);
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
                    prop_assert!(fragment > 0);

                    if let Some(kept) = by_id.get(from) {
                        prop_assert!(share(kept, before[from]) >= fragment);
                    }
                }
            }
        }

        #[test]
        fn a_group_that_loses_a_member_from_sight_says_so_once(turns in turns()) {
            let (prev, now, tick) = history(&turns);
            let (_, events) = step(GRACE, 0, &BTreeSet::new(), &prev, &now, tick);
            let here: BTreeSet<&Id> = now.iter().map(|c| &c.id).collect();

            for g in &prev {
                let seen = g.members.iter().any(|m| here.contains(m));
                let vanished = g.members.iter().any(|m| !here.contains(m));
                let said = events.iter().filter(|e| **e == Event::Left(g.id)).count();
                prop_assert_eq!(said, usize::from(seen && vanished));
            }
        }

        #[test]
        fn a_merge_keeps_the_older_id(turns in turns()) {
            let (prev, now, tick) = history(&turns);
            let (groups, events) = step(GRACE, 0, &BTreeSet::new(), &prev, &now, tick);
            let ids: BTreeSet<i32> = groups.iter().map(|g| g.id).collect();

            for e in &events {
                if let Event::Merged { into, from } = e {
                    prop_assert!(into < from);
                    prop_assert!(ids.contains(into));
                    prop_assert!(!ids.contains(from));
                }
            }
        }

        #[test]
        fn an_unseen_group_is_kept_for_the_grace_then_dropped(turns in turns()) {
            let (prev, now, tick) = history(&turns);
            let (groups, _) = step(GRACE, 0, &BTreeSet::new(), &prev, &now, tick);

            prop_assert!(groups.iter().all(|g| tick - g.last_seen <= GRACE));

            for g in &prev {
                prop_assert!(
                    tick - g.last_seen > GRACE
                        || now.iter().any(|c| g.members.contains(&c.id))
                        || groups.iter().any(|k| k.id == g.id)
                );
            }
        }
    }
}
