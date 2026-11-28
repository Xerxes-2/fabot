//! `npm run plan:quality`: every real room's and a gen-world sample's plan
//! read on what its economy pays before tunnels stand, against the best plan
//! of the centres the search tries. A report, never a gate.
use std::fmt::Write;

use fabot_base::{
    tiles::{self, Tile},
    world::{RoomName, Rules, StructureType},
};
use fabot_layout::{
    Plan, build,
    ground::{self, Ground},
    haul::{self, Walks},
    place::{self, SWAMP, WALL},
    plan::{self, Drawing, Drawn, TRIES},
};

/// A post or upgrade container no walk from the hub reaches.
const UNREACHABLE: &str = "unreachable";

struct Read {
    walks: Walks,
    farthest: u32,
    swamp: f64,
    tunnel: f64,
}

fn read(ground: &Ground, plan: &Plan) -> Option<Read> {
    let walks = haul::walks(ground, plan)?;
    let m = haul::matrix(ground, plan);
    let hub = haul::walker(ground, &m).from(plan.anchor);
    let extension = |t: Tile| tiles::around(t).iter().map(|n| hub[*n]).min();
    let farthest = (plan.items.iter())
        .filter(|i| i.kind == StructureType::Extension)
        .filter_map(|i| extension(i.tile).and_then(|d| d.checked_add(2)))
        .max()
        .unwrap_or(0);
    let roads: Vec<Tile> =
        plan.items.iter().filter(|i| i.kind == StructureType::Road).map(|i| i.tile).collect();
    let share = |code: u8| {
        roads.iter().filter(|t| ground.terrain[**t] == code).count() as f64
            / roads.len().max(1) as f64
    };
    Some(Read { walks, farthest, swamp: share(SWAMP), tunnel: share(WALL) })
}

fn tried(rules: &Rules, ground: &Ground) -> Vec<(Tile, bool, Option<Read>)> {
    let (walls, exits) = place::distances(&ground.terrain);
    let centers = build::centers(ground, &walls, &exits);
    (centers.into_iter().take(TRIES))
        .filter_map(|c| build::build(rules, ground, (&walls, &exits), c))
        .map(|b| plan::to_plan(rules, ground, &b))
        .map(|p| (p.anchor, plan::fits(&p), read(ground, &p)))
        .collect()
}

fn line(name: &str, rules: &Rules, ground: &Ground) -> Option<String> {
    let mut budget = usize::MAX;
    let Drawn::Done(Some(plan)) = Drawing::of(ground).advance(rules, ground, &mut budget) else {
        return None;
    };
    let chosen = read(ground, &plan)?;
    let tried = tried(rules, ground);
    let fit = |t: &&(Tile, bool, Option<Read>)| t.1;
    let haul = |t: &(Tile, bool, Option<Read>)| Some((t.0, t.2.as_ref()?.walks.haul()?));
    let best = tried.iter().filter(fit).filter_map(haul).min_by(|a, b| a.1.total_cmp(&b.1));
    let xy = |t: Tile| format!("{},{}", tiles::x_of(t), tiles::y_of(t));
    let walk = |w: &Option<u32>| w.map_or_else(|| UNREACHABLE.to_string(), |w| w.to_string());
    let posts: Vec<String> = chosen.walks.posts.iter().map(walk).collect();
    let hauled = chosen.walks.haul();
    let haul = hauled.map_or_else(|| UNREACHABLE.into(), |h| format!("{h:.1}"));
    let gap = hauled.zip(best).map_or_else(|| "-".into(), |(h, b)| format!("{:.1}", h - b.1));
    Some(format!(
        "{name} centre={} built={} haul={haul} posts={} controller={} farthest={} swamp={:.3} \
         tunnel={:.3} best={} gap={gap}",
        xy(plan.anchor),
        usize::MAX - budget,
        posts.join(","),
        walk(&chosen.walks.controller),
        chosen.farthest,
        chosen.swamp,
        chosen.tunnel,
        best.map_or_else(|| "-".into(), |b| xy(b.0)),
    ))
}

/// `sim gen-rooms`' lines: `seed:name;x,y|x,y;x,y;x,y;terrain`, `-` for none.
fn gen_ground(text: &str) -> Option<(String, Ground)> {
    let f: Vec<&str> = text.split(';').collect();
    let at = |s: &str| {
        let mut xy = s.split(',').map(|v| v.parse::<i32>().ok());
        Some(tiles::index(xy.next()??, xy.next()??))
    };
    let some = |s: &&str| *s != "-";
    let name: RoomName = f.first()?.split(':').nth(1)?.parse().ok()?;
    let mut sources: Vec<Tile> = f.get(1)?.split('|').filter(some).filter_map(at).collect();
    sources.sort_unstable();
    let controller = Some(*f.get(2)?).filter(some).and_then(at);
    let minerals: Vec<Tile> = Some(*f.get(3)?).filter(some).and_then(at).into_iter().collect();
    let mut terrain: Vec<u8> = (f.get(4)?.bytes())
        .map(|b| match b - b'0' {
            v if v & 1 != 0 => WALL,
            v if v & 2 != 0 => SWAMP,
            _ => 0,
        })
        .collect();
    for t in sources.iter().chain(&minerals).chain(&controller) {
        terrain[t.index()] = WALL;
    }
    (terrain.len() == tiles::COUNT)
        .then(|| (f[0].to_string(), Ground { room: name, terrain, sources, minerals, controller }))
}

fn stat<'a>(lines: &'a [String], key: &str) -> Vec<(f64, &'a str)> {
    let value = |l: &'a String| {
        let v = l.split(' ').find_map(|kv| kv.strip_prefix(key)?.strip_prefix('='))?;
        Some((v.parse().ok()?, l.split(' ').next()?))
    };
    let mut values: Vec<(f64, &str)> = lines.iter().filter_map(value).collect();
    values.sort_by(|a, b| b.0.total_cmp(&a.0).then(a.1.cmp(b.1)));
    values
}

#[test]
#[ignore = "a report: npm run plan:quality"]
fn plan_quality() {
    let out = std::env::var("PLAN_QUALITY_OUT").expect("PLAN_QUALITY_OUT: the report's path");
    let rules = Rules::vanilla();
    let real = fabot_base::testing::real_rooms().filter_map(|r| {
        let ground = ground::of_room(&r.room())?;
        Some((ground.room.to_string(), ground))
    });
    let gen_text = std::env::var("PLAN_QUALITY_GEN").map(std::fs::read_to_string);
    let gen_text = gen_text.unwrap_or(Ok(String::new())).expect("PLAN_QUALITY_GEN reads");
    let rooms: Vec<(String, Ground)> =
        real.chain(gen_text.lines().filter_map(gen_ground)).collect();
    let threads = std::thread::available_parallelism().map_or(1, usize::from);
    let mut lines: Vec<String> = std::thread::scope(|s| {
        let chunks = rooms.chunks(rooms.len().div_ceil(threads));
        let read = |c: &'_ [(String, Ground)]| -> Vec<String> {
            c.iter().filter_map(|(n, g)| line(n, &rules, g)).collect()
        };
        let jobs: Vec<_> = chunks.map(|c| s.spawn(move || read(c))).collect();
        jobs.into_iter().flat_map(|j| j.join().expect("a reading thread")).collect()
    });
    lines.sort();
    let mut report = format!("rooms {} read {}\n", rooms.len(), lines.len());
    let cut = |l: &&String| l.contains(&format!("haul={UNREACHABLE}"));
    let cut: Vec<&str> = lines.iter().filter(cut).filter_map(|l| l.split(' ').next()).collect();
    let _ = writeln!(report, "{UNREACHABLE}: {} {}", cut.len(), cut.join(", "));
    for key in ["haul", "gap", "farthest", "swamp", "tunnel", "built"] {
        let values = stat(&lines, key);
        let Some(last) = values.len().checked_sub(1) else {
            let _ = writeln!(report, "{key}: none");
            continue;
        };
        let at = |q: f64| values[((1.0 - q) * last as f64) as usize].0;
        let _ =
            writeln!(report, "{key}: median {:.2} p90 {:.2} p99 {:.2}", at(0.5), at(0.9), at(0.99));
        let worst: Vec<String> = values.iter().take(20).map(|(v, r)| format!("{r} {v}")).collect();
        let _ = writeln!(report, "  worst: {}", worst.join(", "));
    }
    report.push_str("--\n");
    report.push_str(&lines.join("\n"));
    std::fs::write(&out, report + "\n").expect("the report writes");
}
