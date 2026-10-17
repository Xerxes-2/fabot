use crate::core::grid::Footprint;

/// A wish whose `Head` is `At` stays, and `Tail` lists where it may be shoved.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Wish {
    pub creep: String,
    /// A `Quad` is held where it stands: formation stepping is not
    /// arbitrated.
    pub footprint: Footprint,
    pub at: i32,
    pub head: i32,
    /// A body it shoves off its head is put there last.
    pub onward: Option<i32>,
    /// Best first.
    pub tail: Vec<i32>,
    /// Its work area when it stands inside it, else empty; holds `At` when
    /// not empty.
    pub working: Vec<i32>,
    /// What its head is worth over standing still.
    pub weight: i32,
    pub moves: bool,
}

impl Wish {
    pub fn stays(&self) -> bool {
        self.head == self.at
    }

    pub fn movable(&self) -> bool {
        self.moves && self.footprint == Footprint::One
    }

    /// A body is pushed off its work only by a chain at least two weights
    /// heavier: one weight heavier scores nothing.
    pub fn value(&self, tile: i32) -> i32 {
        let weight = self.weight.max(1);
        let asked = if self.stays() {
            0
        } else if tile == self.head {
            weight
        } else if self.tail.contains(&tile) {
            1
        } else {
            0
        };
        if !self.working.is_empty() && !self.working.contains(&tile) {
            asked - weight - 1
        } else {
            asked
        }
    }
}
