//! The invariants of the modules without a suite of their own, and the
//! draw every suite samples from.
mod atlas;
mod engine;
mod mv;
mod tick;

/// A fixed-seed draw: the suites' scenes and worlds
/// are sampled, never replayed (`ARCHITECTURE.md` §10).
pub struct Dice(u64);

impl Dice {
    pub fn new(seed: u64) -> Dice {
        Dice(seed | 1)
    }

    pub fn next(&mut self) -> u64 {
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
