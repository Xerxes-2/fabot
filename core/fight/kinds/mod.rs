pub mod defend;
mod square;
pub mod stronghold;
pub mod sweep;

pub fn pruned(
    e: &crate::shape::Engagement,
    standing: &std::collections::BTreeSet<String>,
    aging_out: &std::collections::BTreeSet<String>,
) -> crate::shape::Engagement {
    let mut next = e.clone();
    next.ours.retain(|n, _| standing.contains(n));
    next.lost = e.lost + (e.ours.len() - next.ours.len()) as i32;
    next.ours.retain(|n, _| !aging_out.contains(n));
    next
}
