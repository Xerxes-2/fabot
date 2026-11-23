use crate::vocabulary::{Part, ResourceType, takes};

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

/// Regressions persist beside the source file: proptest's default files them in
/// the parent of the nearest `lib.rs` (`core/`), and warns where none is above.
pub fn config(cases: u32) -> proptest::test_runner::Config {
    use proptest::test_runner::{Config, FileFailurePersistence};
    let beside = FileFailurePersistence::WithSource("proptest-regressions");
    Config {
        failure_persistence: Some(Box::new(beside)),
        ..Config::with_cases(cases)
    }
}
