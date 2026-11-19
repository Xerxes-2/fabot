use crate::grid::Footprint;
use crate::tiles::Tile;

/// A wish whose `Head` is `At` stays, and `Tail` lists where it may be shoved.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Wish {
    pub creep: String,
    /// A `Quad` is held where it stands: formation stepping is not
    /// arbitrated.
    pub footprint: Footprint,
    pub at: Tile,
    pub head: Tile,
    /// A body it shoves off its head is put there last.
    pub onward: Option<Tile>,
    /// Best first.
    pub tail: Vec<Tile>,
    /// Its work area when it stands inside it, else empty; holds `At` when
    /// not empty.
    pub working: Vec<Tile>,
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
    #[inline]
    pub fn value(&self, tile: Tile) -> i32 {
        let weight = self.weight.max(1);
        let asked = if self.stays() {
            0
        } else if tile == self.head {
            weight
        } else {
            i32::from(self.tail.contains(&tile))
        };
        if !self.working.is_empty() && !self.working.contains(&tile) {
            asked - weight - 1
        } else {
            asked
        }
    }
}
