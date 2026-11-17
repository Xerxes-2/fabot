//! The soak on krebs, natively: `decide` on krebs's documents each tick,
//! krebs's processor on our intents, `soak-step.mjs`'s files out.
//!
//! Usage: sim --world <world.json> --room <name> --out <dir> [--ticks N] [--every K] [--seed N] [--late K]
mod act;
mod driver;
mod read;
mod sample;
mod view;
mod world;

use std::collections::BTreeMap;
use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::time::Instant;

use fabot_core::world::RoomName;
use krebs::cron::{CronCtx, JOBS};
use krebs::hooks::ProcessorHooks;
use krebs::intents::RoomIntents;
use krebs::processor::{self, TickCtx, global::GlobalCtx};
use krebs::world::World as Krebs;
use serde_json::json;

use view::{View, ms};

/// Season 11's measure of how often `genInvaders` lands inside a room's pass (§10.2).
const INSIDE_PASS: f64 = 0.66;
/// Season 11's tick, at which the 300 s cron fires every 88 ticks.
pub const TICK_MS: u32 = 3410;

struct Args {
    world: String,
    room: String,
    out: String,
    ticks: u64,
    every: u64,
    seed: u64,
    late: u64,
}

fn args() -> Args {
    let mut flags: BTreeMap<String, String> = BTreeMap::new();
    let mut rest = std::env::args().skip(1);
    while let (Some(flag), Some(value)) = (rest.next(), rest.next()) {
        flags.insert(flag, value);
    }
    let usage = "usage: sim --world <world.json> --room <name> --out <dir> [--ticks N] [--every K] [--seed N] [--late K]";
    let need = |flag: &str| (flags.get(flag).cloned()).unwrap_or_else(|| panic!("{flag}: {usage}"));
    let number = |flag: &str, or: u64| {
        (flags.get(flag)).map_or(or, |v| {
            v.parse().unwrap_or_else(|_| panic!("{flag}: {usage}"))
        })
    };
    Args {
        world: need("--world"),
        room: need("--room"),
        out: need("--out"),
        ticks: number("--ticks", 3000),
        every: number("--every", 100),
        seed: number("--seed", 1),
        late: number("--late", 0),
    }
}

/// krebs's `TickLoop::tick` without its runners and publish: P3 over the
/// active rooms and those with intents, P4, P5, the clock, then the official
/// cron jobs, each when its period has passed on the virtual clock, which
/// runs `TICK_MS` a tick. Between ticks the active set
/// holds what the last pass activated, as when the backend's cron lands
/// outside a processing pass; `INSIDE_PASS` stands for the calls that land inside one.
struct Engine {
    hooks: ProcessorHooks,
    cron_last: Vec<i64>,
    /// SplitMix64 off the soak's seed: which active rooms `genInvaders` sees as inactive.
    draw: u64,
    /// Rooms the processor ran, summed over the ticks.
    processed: u64,
}

impl Engine {
    fn next(&mut self) -> f64 {
        self.draw = self.draw.wrapping_add(0x9e37_79b9_7f4a_7c15);
        let mut z = self.draw;
        z = (z ^ (z >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
        z = (z ^ (z >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
        (z ^ (z >> 31)) as f64 / u64::MAX as f64
    }

    /// The active rooms this call lands inside the pass of, taken out of the set until it returns.
    fn hide(&mut self, k: &mut Krebs) -> Vec<u32> {
        let (mut kept, mut hidden) = (Vec::new(), Vec::new());
        for r in k.active.take() {
            if self.next() < INSIDE_PASS {
                hidden.push(r)
            } else {
                kept.push(r)
            }
        }
        kept.into_iter().for_each(|r| k.active.insert(r));
        hidden
    }

    fn step(&mut self, k: &mut Krebs, intents: Vec<(usize, RoomIntents)>) {
        let t = k.time;
        let mut by_room: BTreeMap<u32, RoomIntents> = BTreeMap::new();
        for (room, i) in intents {
            k.active.insert(room as u32);
            by_room.insert(room as u32, i);
        }
        let mut active = k.active.take();
        active.sort_unstable();
        self.processed += active.len() as u64;
        let results = {
            let ctx = TickCtx {
                time: t,
                vnow: k.clock.at(t),
                seed: k.seed,
                compat: &k.compat,
                config: &k.config,
                meta: &k.meta,
                terrain: &k.terrain,
                access: &k.access,
                users: &k.users,
                user_ids: &k.user_ids,
                hooks: &self.hooks,
                order_override: None,
            };
            let empty = || RoomIntents { users: Vec::new() };
            (active.iter())
                .map(|&r| {
                    let intents = by_room.remove(&r).unwrap_or_else(empty);
                    processor::process_room(&ctx, r, &k.rooms[r as usize], intents)
                })
                .collect::<Vec<_>>()
        };
        if let Some(r) = results.iter().find(|r| r.crashed.is_some()) {
            let room = &k.meta.rooms[r.room as usize].name;
            panic!("{room} crashed at {t}: {:?}", r.crashed);
        }
        processor::install(k, results);
        let global = GlobalCtx {
            time: t,
            vnow: k.clock.at(t),
            hooks: &self.hooks,
            order_override: None,
        };
        processor::global::run(k, &global);
        k.time = t + 1;
        let now = k.clock.at(k.time);
        self.cron_last.resize(JOBS.len(), 0);
        for (i, &(name, period, job)) in JOBS.iter().enumerate() {
            if now - self.cron_last[i] > i64::from(period) * 1000 {
                self.cron_last[i] = now;
                let hidden = if name == "genInvaders" {
                    self.hide(k)
                } else {
                    Vec::new()
                };
                job(k, &mut CronCtx::new(k.seed, k.time, now, name)).expect(name);
                hidden.into_iter().for_each(|r| k.active.insert(r));
            }
        }
    }
}

fn main() {
    let args = args();
    let map = world::read(&args.world);
    let seat = map.seat(&args.room);
    eprintln!(
        "colony: {}, spawn at {},{}; output {}",
        args.room, seat.0, seat.1, args.out
    );
    let mut k = map.stand(&args.room, seat, args.seed);
    let terrains = (map.rooms.iter())
        .filter_map(|(name, t)| {
            let bits: Box<[u8; fabot_core::tiles::COUNT]> = t
                .bytes()
                .map(|b| b - b'0')
                .collect::<Vec<_>>()
                .try_into()
                .ok()?;
            Some((
                RoomName::parse(name)?,
                screeps::LocalRoomTerrain::new_from_bits(bits),
            ))
        })
        .collect();

    fs::create_dir_all(&args.out).expect("the output directory");
    let file = |name: &str| {
        BufWriter::new(File::create(format!("{}/{name}", args.out)).expect("an output file"))
    };
    let mut samples = file("samples.jsonl");
    for empty in ["bodies.jsonl", "engagements.jsonl", "decay.jsonl"] {
        file(empty);
    }
    let out = [
        file("faults.jsonl"),
        file("cpu.jsonl"),
        file("metrics.jsonl"),
    ];
    let mut driver = driver::Driver::new(driver::Maps(terrains), out);

    let mut engine = Engine {
        hooks: ProcessorHooks::default(),
        cron_last: Vec::new(),
        draw: args.seed,
        processed: 0,
    };
    let mut sampler = sample::Sampler::new(&args.room);
    let mut levels: Vec<serde_json::Value> = Vec::new();
    let (mut engine_ms, mut sample_ms) = (0.0, 0.0);
    let began = Instant::now();

    // One view a tick: the sampler reads tick t - 1's end, which tick t's bot reads too.
    for t in 0..=args.ticks {
        let v = View::of(&k);
        if let Some(t) = t.checked_sub(1) {
            let at = Instant::now();
            sampler.account(&v);
            let rcl = sampler.level(&v);
            if levels.last().is_none_or(|l| l["rcl"] != rcl) {
                levels.push(json!({ "t": t, "time": k.time - 1, "rcl": rcl }));
            }
            if t % args.every == 0 || t == args.ticks - 1 {
                let mut row = sampler.sample(&v, t, &map);
                row["faults"] = json!(driver.faults.len());
                row["lost"] = json!(0);
                let _ = writeln!(samples, "{row}");
                eprintln!("t{t} rcl{rcl} {} creeps {}", row["progress"], row["creeps"]);
            }
            sample_ms += ms(at);
        }
        if t == args.ticks {
            break;
        }
        let intents = if t >= args.late {
            driver.tick(&v, t, &|| sampler.ledger())
        } else {
            Vec::new()
        };
        drop(v);

        let at = Instant::now();
        engine.step(&mut k, intents);
        engine_ms += ms(at);
    }

    let write = |name: &str, value: serde_json::Value| {
        fs::write(format!("{}/{name}", args.out), format!("{value}\n")).expect("an output file");
    };
    write("levels.json", json!(levels));
    write("refusals.json", json!(driver.refused));
    write("lost.json", json!([]));
    write("slow.json", json!([]));
    let per = |total: f64| total / args.ticks as f64;
    let timing = json!({ "ticks": args.ticks, "wall": began.elapsed().as_millis(), "engine": per(engine_ms),
        "adapter": per(driver.adapter_ms), "decide": per(driver.decide_ms), "sampler": per(sample_ms),
        "rooms": engine.processed as f64 / args.ticks as f64 });
    eprintln!("{timing}");
    write("timing.json", timing);
}
