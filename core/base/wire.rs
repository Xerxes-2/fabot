use serde::Serializer;

/// `JSON.stringify`'s number: a whole one has no `.0`, so Memory's text
/// survives the round trip.
pub fn js_number<S: Serializer>(n: &f64, s: S) -> Result<S::Ok, S::Error> {
    if n.fract() == 0.0 && n.abs() < 1e18 {
        s.serialize_i64(*n as i64)
    } else {
        s.serialize_f64(*n)
    }
}

pub mod letters {
    use serde::{Deserialize, Deserializer, Serializer, de::Error};

    use crate::vocabulary::{ALL_BODY_PARTS, Part, part_letter};

    pub fn serialize<S: Serializer>(body: &[Part], s: S) -> Result<S::Ok, S::Error> {
        s.collect_str(&body.iter().map(|p| part_letter(*p)).collect::<String>())
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(d: D) -> Result<Vec<Part>, D::Error> {
        let letters = String::deserialize(d)?;
        let part = |c| ALL_BODY_PARTS.into_iter().find(|p| part_letter(*p) == c);
        (letters.chars()).map(|c| part(c).ok_or_else(|| D::Error::custom("no part"))).collect()
    }
}
