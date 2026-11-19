//! The draw every suite samples from.
use crate::vocabulary::{Part, ResourceType, takes};

/// The compound of `tier` (0 for none) that boosts `part` in a fight.
pub fn boosted(part: Part, tier: usize) -> Option<ResourceType> {
    use ResourceType::*;
    const LINES: [[ResourceType; 3]; 6] = [
        [UtriumHydride, UtriumAcid, CatalyzedUtriumAcid],
        [KeaniumOxide, KeaniumAlkalide, CatalyzedKeaniumAlkalide],
        [
            LemergiumOxide,
            LemergiumAlkalide,
            CatalyzedLemergiumAlkalide,
        ],
        [GhodiumOxide, GhodiumAlkalide, CatalyzedGhodiumAlkalide],
        [ZynthiumOxide, ZynthiumAlkalide, CatalyzedZynthiumAlkalide],
        [ZynthiumHydride, ZynthiumAcid, CatalyzedZynthiumAcid],
    ];
    let t = tier.checked_sub(1)?;
    LINES.iter().map(|line| line[t]).find(|c| takes(part, *c))
}

/// A fixed-seed draw: the suites' scenes and worlds
/// are sampled, never replayed (`ARCHITECTURE.md` §10).
pub struct Dice(u64);

impl Dice {
    pub fn new(seed: u64) -> Dice {
        Dice(seed | 1)
    }

    fn next(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x << 13;
        x ^= x >> 7;
        x ^= x << 17;
        self.0 = x;
        x
    }

    pub fn below(&mut self, n: i32) -> i32 {
        (self.next() % n as u64) as i32
    }

    pub fn between(&mut self, lo: i32, hi: i32) -> i32 {
        lo + self.below(hi - lo + 1)
    }

    pub fn chance(&mut self, one_in: i32) -> bool {
        self.below(one_in) == 0
    }

    pub fn pick<T: Copy>(&mut self, xs: &[T]) -> T {
        xs[self.below(xs.len() as i32) as usize]
    }

    pub fn pick_of<T: Clone>(&mut self, xs: &[T]) -> T {
        xs[self.below(xs.len() as i32) as usize].clone()
    }
}
