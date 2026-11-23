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
            /// The first field outside its range.
            pub fn outside(&self) -> Option<&'static str> {
                $(
                    if !($valid).contains(&self.$field) {
                        return Some(stringify!($field));
                    }
                )*
                None
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
    outpost_spend_parts: f64 = 2.0, 0.0..=100.0;
    outpost_cpu_share: f64 = 0.5, 0.01..=1.0;
    outpost_spawn_share: f64 = 0.5, 0.01..=1.0;
    pioneers: usize = 4, 0..=10;
}
