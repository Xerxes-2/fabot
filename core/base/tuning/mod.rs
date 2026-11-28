macro_rules! tuning {
    ($($field:ident: $kind:ty = $default:expr, $valid:expr;)*) => {
        /// `ARCHITECTURE.md` §4 `tuning`; each is read, with its why, where it decides.
        #[derive(Clone, Copy, Debug, PartialEq, serde::Deserialize)]
        #[serde(default, deny_unknown_fields, rename_all = "camelCase")]
        pub struct Tuning {
            $(pub $field: $kind,)*
        }

        impl Default for Tuning {
            fn default() -> Tuning {
                Tuning {
                    $($field: $default,)*
                }
            }
        }

        impl Tuning {
            pub fn outside(&self) -> Option<&'static str> {
                [$((stringify!($field), ($valid).contains(&self.$field)),)*]
                    .into_iter().find(|(_, ok)| !ok).map(|(field, _)| field)
            }
        }
    };
}

tuning! {
    reserve_lifetimes: f64 = 2.0, 0.0..=100.0;
    quiet_reserve_divisor: f64 = 3.0, 1.0..=100.0;
    rampart_floor: i32 = 10_000, 0..=300_000_000;
    quiet_wall_divisor: f64 = 3.0, 1.0..=100.0;
    sponsor_margin: i32 = 30_000, 0..=1_000_000;
    expand_bucket: i32 = 9_000, 0..=10_000;
    expand_threat_rooms: i32 = 2, 0..=10;
    prune_level: i32 = 5, 0..=8;
    expand_source: f64 = 1.0, 0.0..=10.0;
    expand_near_source: f64 = 0.5, 0.0..=10.0;
    expand_crossing: f64 = 1.0, 0.0..=50.0;
    expand_worth: f64 = 1.0, 0.0..=10.0;
    expand_failure: f64 = 6.0, 0.0..=100.0;
    expand_mineral: f64 = 1.0, 0.0..=10.0;
    expand_swap: f64 = 1.0, 0.0..=10.0;
    expand_scarcity: f64 = 1.0, 0.0..=1.0;
    expand_future_ore: f64 = 0.25, 0.0..=1.0;
    expand_rival_ore: f64 = 0.25, 0.0..=1.0;
    outpost_spend_parts: f64 = 2.0, 0.0..=100.0;
    outpost_cpu_share: f64 = 0.5, 0.01..=1.0;
    outpost_spawn_share: f64 = 0.5, 0.01..=1.0;
    pioneers: usize = 4, 0..=10;
}
