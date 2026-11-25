pub mod boost;
pub mod declarations;
pub mod engine;
pub mod grid;
pub mod intel;
pub mod intents;
pub mod stronghold;
#[cfg(any(test, feature = "test-support"))]
pub mod testing;
pub mod tiles;
pub mod tuning;
pub mod vocabulary;
pub mod wire;
pub mod wish;
pub mod world;

/// FxHash: on wasm it beat SipHash, foldhash and rapidhash (bench-live), whose
/// 128-bit multiply wasm has no instruction for.
pub type HashMap<K, V> = std::collections::HashMap<K, V, rustc_hash::FxBuildHasher>;
#[cfg(test)]
mod tests {
    mod boost;
    mod engine;
}
