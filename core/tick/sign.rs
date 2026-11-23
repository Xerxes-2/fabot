use fabot_base::declarations::Declarations;
use fabot_base::intents::{Action, CreepAct};
use fabot_base::world::{Controller, Owner, RoomName, World};

const OWNED: [&str; 15] = [
    "fabot: Rust -> WASM. Zero unsafe, moderate hubris.",
    "Borrow checker approved this outpost. Mutable access denied.",
    "Compiled to WASM so we can lose creeps at native speed.",
    "Used to be F#, now Rust. Still allergic to mutable global state.",
    "fabot: fearless concurrency on a single-threaded 100 CPU isolate.",
    "This room has a single owner. The borrow checker insists.",
    "No garbage collector pauses were harmed in the running of this room.",
    "cargo test passed; the live server disagreed.",
    "Result<Room, Raid>: currently Ok, hoping not to unwrap Err.",
    "WASM binary loaded. Panics forbidden. Refusals logged.",
    "fabot: ownership rules strictly enforced by rustc and two towers.",
    "Zero-cost abstractions, non-zero creep spawn costs.",
    "Lifetime 'a of this creep ends in 1500 ticks. Plan accordingly.",
    "Deterministic decide(): same world in, same intents out, every tick.",
    "fn sign() -> OK. No allocs, no detour, back to hauling.",
];

const RESERVED: [&str; 10] = [
    "[fabot] Room surveyed. Active remote mining in progress.",
    "[fabot] Economic zone. Haulers have right of way.",
    "[fabot] Territory under routine observation and harvest.",
    "[fabot] Outpost claimed for seasonal economy. Neighbours welcome to talk.",
    "[fabot] Active sector. Unarmed scouts welcome; armed guests priced on sight.",
    "[fabot] Automated logistics corridor. Mind the traffic.",
    "[fabot] Reserved remote. Please message before contesting.",
    "[fabot] Energy extraction site. Operating continuously.",
    "[fabot] Local infrastructure maintained here. Keep clear of roads.",
    "[fabot] Seasonal claim in progress. Peaceful border preferred.",
];

const PASSING: [&str; 3] = [
    "[fabot] Logged in Atlas. Terrain cached, routes laid.",
    "[fabot] Scouted and indexed. Passing through.",
    "[fabot] Controller noted. No further action required.",
];

/// FNV-1a of the name: `RoomName`'s order would put neighbours on neighbouring lines.
fn pick(texts: &'static [&'static str], room: RoomName) -> &'static str {
    let hash = (room.to_string().bytes()).fold(0x811c_9dc5_u32, |h, b| {
        (h ^ u32::from(b)).wrapping_mul(0x0100_0193)
    });
    texts[hash as usize % texts.len()]
}

/// The text we want on it, or none where another player holds the room, an
/// ally signed it, or the server did: a novice area's sign hides ours. An
/// outpost's lapsed reservation keeps its text, or each reserver arriving
/// after a lapse would sign it twice.
pub fn wanted(c: &Controller, room: RoomName, outpost: bool) -> Option<&'static str> {
    let texts: &'static [&'static str] = match (&c.owner, &c.reservation) {
        (Owner::Ours, _) => &OWNED,
        (Owner::Nobody, Some((Owner::Ours, _))) => &RESERVED,
        (Owner::Nobody, None) if outpost => &RESERVED,
        (Owner::Nobody, None) => &PASSING,
        _ => return None,
    };
    match &c.sign {
        Some((Owner::Ally(_), _)) => return None,
        Some((Owner::Stranger(u), _)) if u == screeps::SYSTEM_USERNAME => return None,
        _ => {}
    }
    let text = pick(texts, room);
    let signed = matches!(&c.sign, Some((Owner::Ours, t)) if t == text);
    (!signed).then_some(text)
}

/// A body of ours beside a controller signs it where it stands.
pub fn reflex<'a>(
    world: &'a World,
    declarations: &'a Declarations,
) -> impl Iterator<Item = Action> + 'a {
    world.rooms.iter().filter_map(|r| {
        let c = r.controller.as_ref()?;
        let outpost = declarations.outposts.iter().any(|o| o.room == r.name);
        let text = wanted(c, r.name, outpost)?;
        let body = (world.creeps.iter()).find(|b| b.pos.within(1, &c.pos))?;
        Some(Action::Creep(
            body.name.clone(),
            CreepAct::Sign(c.id, text.to_string()),
        ))
    })
}

#[cfg(test)]
mod tests {
    use super::{OWNED, PASSING, RESERVED, wanted};
    use fabot_base::world::{Controller, Owner, Pos, RoomName};

    #[test]
    fn every_text_fits_the_engines_hundred_units() {
        for t in OWNED.iter().chain(&RESERVED).chain(&PASSING) {
            assert!(t.encode_utf16().count() <= 100, "{t}");
        }
    }

    #[test]
    fn a_sign_is_wanted_only_in_rooms_ours_or_nobodys_and_never_over_an_allys_the_servers_or_our_own()
     {
        let room = RoomName::W1N1;
        let them = || Owner::Stranger("x".into());
        let controller = |owner, reservation, sign| Controller {
            id: "c".into(),
            pos: Pos::new(room, 10, 10),
            owner,
            level: 0,
            progress: 0,
            progress_total: 0,
            ticks_to_downgrade: 0,
            reservation,
            sign,
            safe_mode: 0,
            safe_mode_available: 0,
            safe_mode_cooldown: 0,
            upgrade_blocked: 0,
        };
        let theirs = Some((them(), "theirs".to_string()));
        let ally = Some((Owner::Ally("a".into()), "ally".to_string()));
        let ours = |t: &str| Some((Owner::Ours, t.to_string()));
        let system = Some((
            Owner::Stranger(screeps::SYSTEM_USERNAME.into()),
            "novice".to_string(),
        ));
        let held = Some((Owner::Ours, 1000));
        let table = [
            (controller(Owner::Ours, None, theirs.clone()), true),
            (
                controller(Owner::Nobody, held.clone(), theirs.clone()),
                true,
            ),
            (controller(Owner::Nobody, None, theirs.clone()), true),
            (controller(Owner::Nobody, None, ours("old")), true),
            (controller(them(), None, None), false),
            (
                controller(Owner::Nobody, Some((them(), 1000)), theirs),
                false,
            ),
            (
                controller(Owner::Nobody, Some((Owner::Invader, 1000)), None),
                false,
            ),
            (controller(Owner::Nobody, None, ally.clone()), false),
            (controller(Owner::Ours, None, ally), false),
            (controller(Owner::Nobody, None, system), false),
        ];
        for ((c, signs), outpost) in table
            .into_iter()
            .flat_map(|t| [(t.clone(), false), (t, true)])
        {
            assert_eq!(wanted(&c, room, outpost).is_some(), signs, "{c:?}");
            if let Some(text) = wanted(&c, room, outpost) {
                let mut signed = c.clone();
                signed.sign = ours(text);
                assert_eq!(wanted(&signed, room, outpost), None, "{c:?}");
            }
        }
        let lapsed = controller(Owner::Nobody, None, None);
        let reserved = controller(Owner::Nobody, held, None);
        assert_eq!(wanted(&lapsed, room, true), wanted(&reserved, room, true));
    }
}
