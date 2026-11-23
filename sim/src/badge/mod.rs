use serde_json::{Value, json};

use crate::world::{INVADER, KEEPER, US};

/// As the official API shows them: Invader's own, none for Source Keeper; ours neutral, others off the id.
pub fn put(mut user: Value) -> Value {
    let id = user["_id"].as_str().unwrap_or_default();
    let h = (id.bytes()).fold(5381u64, |h, b| h.wrapping_mul(33) ^ u64::from(b));
    user["badge"] = match id {
        KEEPER => return user,
        INVADER => serde_json::from_str(include_str!("invader-badge.json")).expect("a badge"),
        US => json!({ "type": 1, "color1": "#5a5a5a", "color2": "#9a9a9a", "color3": "#d0d0d0",
            "param": 0, "flip": false }),
        _ => json!({ "type": 1 + h % 24, "color1": h / 24 % 80, "color2": h / 1920 % 80,
            "color3": h / 153_600 % 80, "param": 0, "flip": false }),
    };
    user
}

pub fn ours(mut us: Value, badge: Option<Value>) -> Value {
    if let Some(b) = badge {
        us["badge"] = b;
    }
    us
}
