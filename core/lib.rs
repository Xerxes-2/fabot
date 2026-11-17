pub mod atlas;
pub mod boost;
pub mod drive;
pub mod economy;
pub mod engine;
pub mod exchange;
pub mod fight;
pub mod intents;
pub mod layout;
pub mod memory;
pub mod mv;
pub mod observe;
pub mod season;
pub mod state;
pub mod strategy;
pub mod tactics;
pub mod tick;
pub mod vocabulary;
pub mod world;

pub use atlas::{grid, tiles};

/// FxHash: on wasm it beat SipHash, foldhash and rapidhash (bench-live), whose
/// 128-bit multiply wasm has no instruction for.
pub type HashMap<K, V> = std::collections::HashMap<K, V, rustc_hash::FxBuildHasher>;

#[cfg(test)]
pub(crate) mod tests;
