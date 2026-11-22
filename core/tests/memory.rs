//! Memory round-trips: what the tick reads, written back unchanged, is the
//! text it read, byte for byte, but for the keys of others it drops.
use crate::memory::Memory;

const OURS: &str = concat!(
    r#"{"other":{"b":1,"a":[1.5,"x",null]},"v2":{"declarations":{"reserve":1.5,"#,
    r#""outposts":[{"room":"W2N1","controller":{"id":"c1","x":3,"y":4},"sources":[{"id":"a1","x":5,"y":6}]}],"#,
    r#""allies":["A"],"tactics":[{"rival":"R","forbid":["kite"]}],"labs":{"home":"W1N1","targets":{"UH":3000,"H":1}}},"#,
    r#""switches":{"economy":false,"recorder":true},"assignments":{"a":"d1|w:a1:energy","b":"u1|"},"#,
    r#""fight":{"groups":[{"room":"W1N1","x":10,"y":11,"id":3,"seen":99,"members":["b1","b2"]}],"#,
    r#""engagements":[{"kind":"siege","target":"d1","id":1,"group":0,"room":"W2N1","ours":["a"],"#,
    r#""tactic":"blob","order":["e1"],"phase":"forming","opened":100,"since":100,"lost":0,"budget":2,"#,
    r#""priority":1,"unanswered":true,"ignored":0,"capacity":300,"#,
    r#""predicted":{"kind":"win","ticks":20,"dead":1,"hits":500}},"#,
    r#"{"id":2,"group":3,"room":"W1N1","ours":[],"tactic":"close","order":[],"phase":"fighting","#,
    r#""opened":90,"since":95,"lost":1,"budget":0,"priority":0.25,"unanswered":false,"ignored":2,"#,
    r#""capacity":0,"predicted":null},{"kind":"sweep","id":4,"group":0,"room":"W3N1","ours":[],"#,
    r#""tactic":"kite","order":[],"phase":"pursuing","opened":1,"since":2,"lost":0,"budget":0,"#,
    r#""priority":-3.5,"unanswered":false,"ignored":0,"capacity":0,"predicted":{"kind":"kite","ticks":7}}],"#,
    r#""casts":[{"engagement":1,"name":"f-1","room":"W1N1","body":"TRMH"}],"due":["W1N1"]},"#,
    r#""expansion":{"room":"W3N1","since":50,"failed":[{"room":"W4N1","since":10}],"abandoned":[]}}}"#
);

#[test]
fn state_written_back_is_the_text_read() {
    let theirs = OURS.replacen(r#""v2":"#, r#""fabot":{"v1":1},"v2":"#, 1);
    let theirs = format!(
        "{}{}",
        &theirs[..theirs.len() - 1],
        r#","creeps":{"dead":{}}}"#
    );
    let mut memory = Memory::of_text(theirs);
    let state = memory.state();
    assert_eq!(
        (state.fight.groups.len(), state.fight.engagements.len()),
        (1, 3)
    );
    assert_eq!(memory.written(&state).as_deref(), Some(OURS));
    let mut memory = Memory::of_text(OURS.to_string());
    assert_eq!(memory.written(&memory.state()), None);
}

#[test]
fn garbled_reads_as_none() {
    for text in [
        "",
        "null",
        "[1]",
        "{\"v2\":7}",
        "{\"v2\":{\"fight\":{\"groups\":[{}]}}}",
        "{\"v2\":",
    ] {
        let mut memory = Memory::of_text(text.to_string());
        let state = memory.state();
        assert!(state.fight.groups.is_empty() && state.economy.assignments.is_empty());
        let declared = memory.declarations();
        assert!(declared.outposts.is_empty());
        let kept = text.is_empty() || text.ends_with('}');
        assert_eq!(memory.written(&state).is_some(), kept);
    }
}

#[test]
fn declarations_read_each_key() {
    let declared = Memory::of_text(OURS.to_string()).declarations();
    assert_eq!(declared.reserve_lifetimes, 1.5);
    assert_eq!(declared.outposts[0].sources[0].pos.x, 5);
    assert_eq!((declared.flows.len(), declared.tactics.len()), (1, 1));
    assert!(declared.allies.contains("A"));
}
