//! The soak on krebs, natively: `decide` on krebs's documents each tick,
//! krebs's processor on our intents, the files `scripts/scenario/measures.mjs` reads out.
// A tool, not the bot: a panic fails the run that called it.
#![allow(clippy::unwrap_used, clippy::expect_used)]

mod act;
mod badge;
mod capture;
mod driver;
mod duel;
mod engaged;
mod generated;
mod lives;
mod npc;
mod read;
mod reset;
mod rival;
mod sample;
mod seat;
mod view;
mod watch;
mod world;

use std::collections::{BTreeMap, BTreeSet};
use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::time::Instant;

use krebs::cron::{CronCtx, JOBS};
use krebs::hooks::ProcessorHooks;
use krebs::intents::RoomIntents;
use krebs::processor::{self, TickCtx, global::GlobalCtx};
use krebs::world::World as Krebs;
use serde_json::{Value, json};

use view::{View, ms};

#[global_allocator]
static ALLOC: mimalloc::MiMalloc = mimalloc::MiMalloc;

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
    /// `soak.mjs --ended`: the safe mode a respawn starts in is over.
    ended: bool,
    /// What the heap keeps across ticks is computed afresh beside it.
    check: bool,
    reset_every: u64,
    /// Rooms the samples follow beside the declared ones: an outpost Strategy picks is declared nowhere.
    follow: Vec<String>,
    npc: Vec<npc::Due>,
    rivals: Vec<rival::Rival>,
    /// Each seat our bot plays as `{name, room?, declarations?}`; one in `--room` (or `--gen`'s pick) unless given.
    players: Option<Vec<Value>>,
    /// `--watch`'s port and tick in ms.
    watch: Option<(u16, u64)>,
    /// Our badge as the official `{type, color1, color2, color3, param, flip}`.
    badge: Option<serde_json::Value>,
    generated: Option<generated::Spec>,
    capture: Option<String>,
}

fn args() -> Args {
    let mut flags: BTreeMap<String, String> = BTreeMap::new();
    let mut rest = std::env::args().skip(1);
    while let Some(flag) = rest.next() {
        let value = if ["--ended", "--check", "--watch"].contains(&flag.as_str()) {
            String::new()
        } else {
            rest.next().unwrap_or_default()
        };
        flags.insert(flag, value);
    }
    let usage = "usage: sim (--world <world.json> --room <name> | --gen <seed> [--sectors WxH]) --out <dir> [--ticks N] [--every K] [--seed N] [--late K] [--follow A,B] [--npc <[[tick, room, bodies, [a stronghold's level, deploy] or null for a core]]>] [--rivals <[rival::Rival]>] [--players <[{name, room?, declarations?}]>, each after the first out in <dir>/<name>, a room --gen picks unless named] [--ended] [--check] [--reset-every N] [--capture a-b[,c-d]] [--watch [--port N] [--tick-ms N] [--badge <json>]]";
    let need = |flag: &str| (flags.get(flag).cloned()).unwrap_or_else(|| panic!("{flag}: {usage}"));
    let number = |flag: &str, or: u64| (flags.get(flag)).map_or(or, |v| v.parse().expect(usage));
    Args {
        generated: generated::Spec::of(&flags),
        capture: flags.get("--capture").cloned(),
        world: flags.get("--world").cloned().unwrap_or_default(),
        room: flags.get("--room").cloned().unwrap_or_default(),
        out: need("--out"),
        ticks: number("--ticks", 3000),
        every: number("--every", 100),
        seed: number("--seed", 1),
        late: number("--late", 0),
        ended: flags.contains_key("--ended"),
        check: flags.contains_key("--check"),
        reset_every: number("--reset-every", 0),
        follow: (flags.get("--follow").into_iter())
            .flat_map(|f| f.split(','))
            .filter(|r| !r.is_empty())
            .map(str::to_string)
            .collect(),
        watch: (flags.contains_key("--watch"))
            .then(|| (number("--port", 21025) as u16, number("--tick-ms", 50))),
        badge: (flags.get("--badge")).map(|b| serde_json::from_str(b).expect(usage)),
        npc: serde_json::from_str(flags.get("--npc").map_or("[]", String::as_str)).expect(usage),
        rivals: serde_json::from_str(flags.get("--rivals").map_or("[]", String::as_str))
            .expect(usage),
        players: (flags.get("--players")).map(|p| serde_json::from_str(p).expect(usage)),
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
        let (hidden, kept): (Vec<u32>, Vec<u32>) = k
            .active
            .take()
            .into_iter()
            .partition(|_| self.next() < INSIDE_PASS);
        kept.into_iter().for_each(|r| k.active.insert(r));
        hidden
    }

    fn step(&mut self, k: &mut Krebs, intents: Vec<(usize, RoomIntents)>) {
        let t = k.time;
        let mut by_room: BTreeMap<u32, RoomIntents> = BTreeMap::new();
        for (room, i) in intents {
            k.active.insert(room as u32);
            let at = by_room.entry(room as u32);
            (at.or_insert_with(|| RoomIntents { users: Vec::new() })
                .users)
                .extend(i.users);
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
                // The backend logs a job that throws and runs the next (ARCHITECTURE.md §10.2's genDeposits).
                if let Err(e) = job(k, &mut CronCtx::new(k.seed, k.time, now, name)) {
                    eprintln!("{name}: {e}");
                }
                hidden.into_iter().for_each(|r| k.active.insert(r));
            }
        }
    }
}

fn main() {
    if std::env::args().nth(1).as_deref() == Some("duel") {
        return duel::main();
    }
    let args = args();
    let read = || world::read(&args.world);
    let map = (args.generated.as_ref()).map_or_else(read, generated::Spec::world);
    let homes = |wanted: &[_]| (args.generated.as_ref()).map(|g| g.homes(&map, wanted));
    let seats = seat::seats(args.players.clone(), &args.room, homes, &map.memory);
    let room = seats[0].room.clone();
    eprintln!("colony: {room}; output {}", args.out);
    let mut k = map.stand(&seats, args.seed, args.ended, args.badge.clone());
    let mut watch = (args.watch).map(|(port, every)| watch::Watch::open(&k, &room, port, every));
    let terrains: Vec<_> = (map.rooms.iter())
        .filter_map(|(name, t)| {
            let bits: Box<[u8; fabot_base::tiles::COUNT]> = t
                .bytes()
                .map(|b| b - b'0')
                .collect::<Vec<_>>()
                .try_into()
                .ok()?;
            Some((
                name.parse().ok()?,
                screeps::LocalRoomTerrain::new_from_bits(bits),
            ))
        })
        .collect();

    let closed: BTreeSet<_> = map.closed.iter().filter_map(|r| r.parse().ok()).collect();
    let maps = || driver::Maps(terrains.clone(), closed.clone());
    let twin = || reset::Twin::new(args.reset_every, args.check);
    let mut players = seat::players(seats, &args.out, maps, twin);
    players[0].driver.kept.intel = fabot_base::intel::of_text(&map.intel);
    let uids: Vec<String> = players.iter().map(|p| p.seat.uid.clone()).collect();
    let file = |name: &str| {
        BufWriter::new(File::create(format!("{}/{name}", args.out)).expect("an output file"))
    };
    let mut samples = file("samples.jsonl");

    let mut capture = (args.capture.as_ref()).map(|w| capture::Capture::open(w, &args.out, &map));
    let mut rivals = rival::Rivals::new(args.rivals.clone());
    let mut engine = Engine {
        hooks: ProcessorHooks::default(),
        cron_last: Vec::new(),
        draw: args.seed,
        processed: 0,
    };
    let memory: Value = serde_json::from_str(&players[0].seat.memory).unwrap_or_default();
    let declared = |key: &str| -> Vec<String> {
        let sites = memory["v2"]["declarations"][key]
            .as_array()
            .into_iter()
            .flatten();
        sites
            .filter_map(|c| c["room"].as_str().map(str::to_string))
            .collect()
    };
    let followed = [declared("outposts"), args.follow.clone()].concat();
    let mut sampler = sample::Sampler::new(&room, &followed, declared("claims"));
    let mut levels: Vec<serde_json::Value> = Vec::new();
    let (mut engine_ms, mut sample_ms) = (0.0, 0.0);
    let began = Instant::now();

    // One view a tick: the sampler reads tick t - 1's end, which tick t's bot reads too.
    for t in 0..=args.ticks {
        let mut v = View::of(&k);
        if let Some(t) = t.checked_sub(1) {
            let at = Instant::now();
            sampler.account(&v);
            for (p, uid) in players.iter_mut().zip(&uids) {
                v.us = uid;
                p.engaged(&v, t);
            }
            v.us = &uids[0];
            let driver = &players[0].driver;
            let rcl = sampler.level(&v);
            if levels.last().is_none_or(|l| l["rcl"] != rcl) {
                levels.push(json!({ "t": t, "time": k.time - 1, "rcl": rcl }));
            }
            if t % args.every == 0 || t == args.ticks - 1 {
                let mut row = sampler.sample(&v, t, &map);
                row["faults"] = json!(driver.faults.len());
                row["strongholds"] = npc::strongholds(&v, &driver.kept.intel);
                let seen = (driver.kept.intel.iter()).map(|(r, w)| (r.to_string(), w.seen));
                row["seen"] = json!(seen.collect::<BTreeMap<_, _>>());
                let _ = writeln!(samples, "{row}");
                eprintln!("t{t} rcl{rcl} {} creeps {}", row["progress"], row["creeps"]);
                (players.iter().skip(1)).for_each(|p| eprintln!("  {}", p.standing(&v)));
            }
            sample_ms += ms(at);
        }
        if t == args.ticks {
            break;
        }
        let first = &players[0].driver;
        (capture.iter_mut()).for_each(|c| c.tick(t, &v, &first.memory, &first.kept.intel));
        let mut intents = Vec::new();
        for (i, (p, uid)) in (players.iter_mut().zip(&uids).enumerate()).filter(|_| t >= args.late)
        {
            v.us = uid;
            if i == 0 {
                intents = p.driver.tick(&v, t, &|| sampler.ledger());
                sampler.intend(&intents);
            } else {
                intents.extend(p.driver.tick(&v, t, &Value::default));
            }
        }
        intents.extend(rivals.intents(&v, t));
        drop(v);

        let at = Instant::now();
        engine.step(&mut k, intents);
        for (_, room, due) in args.npc.iter().filter(|n| n.0 == t) {
            match due {
                Some(npc::Npc::Wave(bodies)) => npc::wave(&mut k, room, bodies, || engine.next()),
                Some(npc::Npc::Stronghold(level, deploy)) => {
                    npc::stronghold(&mut k, room, *level, *deploy)
                }
                None => npc::core(&mut k, room, || engine.next()),
            }
        }
        rivals.stand(&mut k, t, &room, || engine.next());
        engine_ms += ms(at);
        watch.iter_mut().for_each(|w| w.tick(&k, &players));
    }

    let write = |dir: &str, name: &str, value: Value| {
        fs::write(format!("{dir}/{name}"), format!("{value}\n")).expect("an output file");
    };
    let lives = sampler.lives.close();
    let bodies: String = lives.iter().map(|b| format!("{b}\n")).collect();
    fs::write(format!("{}/bodies.jsonl", args.out), bodies).expect("an output file");
    write(&args.out, "levels.json", json!(levels));
    for p in &players {
        write(&p.dir, "refusals.json", json!(p.driver.refused));
    }
    let driver = &players[0].driver;
    let per = |total: f64| total / args.ticks as f64;
    let timing = json!({ "ticks": args.ticks, "wall": began.elapsed().as_millis(), "engine": per(engine_ms),
        "adapter": per(driver.adapter_ms), "decide": per(driver.decide_ms), "sampler": per(sample_ms),
        "rooms": engine.processed as f64 / args.ticks as f64 });
    eprintln!("{timing}");
    write(&args.out, "timing.json", timing);
    if let Some(w) = watch {
        drop((samples, players));
        w.park();
    }
}
