/// Fable's `sortBy` and `maxBy` run the projection at every comparison; v1 measured it.
/// No `minBy`: Fable's keeps the last of equal keys and .NET's the first.
use std::cmp::Ordering;

pub fn sorted_by_key<T, K: Clone + Ord>(
    projection: impl Fn(&T) -> K,
    list: &[T],
) -> Vec<T>
where
    T: Clone,
{
    let mut keyed: Vec<(K, T)> = list.iter().map(|item| (projection(item), item.clone())).collect();
    keyed.sort_by(|(a, _), (b, _)| a.cmp(b));
    keyed.into_iter().map(|(_, item)| item).collect()
}

pub fn sorted_by_key_by<T, K>(
    projection: impl Fn(&T) -> K,
    order: impl Fn(&K, &K) -> Ordering,
    list: &[T],
) -> Vec<T>
where
    T: Clone,
{
    let mut keyed: Vec<(K, T)> = list.iter().map(|item| (projection(item), item.clone())).collect();
    keyed.sort_by(|(a, _), (b, _)| order(a, b));
    keyed.into_iter().map(|(_, item)| item).collect()
}

pub fn greatest_by_key<T: Clone, K: Clone>(
    projection: impl Fn(&T) -> K,
    order: impl Fn(&K, &K) -> Ordering,
    list: &[T],
) -> T
where
    T: Clone,
{
    let mut best = list[0].clone();
    let mut best_key = projection(&best);
    for item in &list[1..] {
        let key = projection(item);
        if order(&key, &best_key) == Ordering::Greater {
            best = item.clone();
            best_key = key;
        }
    }
    best
}

pub fn max_by<T: Clone, K: Clone + Ord>(projection: impl Fn(&T) -> K, list: &[T]) -> T {
    greatest_by_key(projection, |a, b| a.cmp(b), list)
}

pub fn sort_by<T, K: Clone + Ord>(projection: impl Fn(&T) -> K, list: &[T]) -> Vec<T>
where
    T: Clone,
{
    sorted_by_key(projection, list)
}

pub fn sort_by_descending<T, K: Clone + Ord>(projection: impl Fn(&T) -> K, list: &[T]) -> Vec<T>
where
    T: Clone,
{
    sorted_by_key_by(projection, |a, b| b.cmp(a), list)
}
