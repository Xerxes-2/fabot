//! The recorder's ring: a tick's pass from `read.js`, its Memory string and
//! its intel rows, deflated, in base64, over as many ring segments as it
//! takes; `__fabotUnread` in `read.js` stands it again as the bench's capture.
//! Terrain never changes, so `observe capture` reads it from the server.

use std::fmt::Write;

use screeps::raw_memory;

use super::world::Pass;
use fabot_core::world::push_int;

const RING: usize = 5;
const FIRST: usize = 10;
const LIMIT: usize = 100 * 1024;
/// A part's head, `tick part parts `, is shorter than this.
const HEAD: usize = 32;
const LEVEL: u8 = 1;

const ALPHABET: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

fn base64(bytes: &[u8]) -> String {
    let mut out = String::with_capacity(bytes.len().div_ceil(3) * 4);
    for chunk in bytes.chunks(3) {
        let word = chunk.iter().enumerate().fold(0u32, |word, (i, byte)| {
            word | (u32::from(*byte) << (16 - 8 * i))
        });
        for i in 0..4 {
            out.push(if i <= chunk.len() {
                char::from(ALPHABET[((word >> (18 - 6 * i)) & 63) as usize])
            } else {
                '='
            });
        }
    }
    out
}

/// NUL parts the fields: JSON escapes it in Memory, and a name holds none.
fn text(time: i32, cpu_limit: f64, pass: &Pass, memory: &str, intel: &str) -> String {
    let mut text =
        String::with_capacity(memory.len() + intel.len() + pass.strs.len() + pass.ints.len() * 4);
    let _ = write!(text, "{time} {cpu_limit}\0{memory}\0{intel}\0");
    for (i, n) in pass.ints.iter().enumerate() {
        if i > 0 {
            text.push(',');
        }
        push_int(&mut text, *n);
    }
    text.push('\0');
    text.push_str(&pass.strs);
    text
}

/// A tick of `parts` starts at slot `time * parts`, so the newest tick is
/// always whole and the ring keeps the last `RING / parts`.
pub fn record(time: i32, cpu_limit: f64, pass: &Pass, memory: &str, intel: &str) -> Option<String> {
    let text = text(time, cpu_limit, pass, memory, intel);
    let packed = base64(&miniz_oxide::deflate::compress_to_vec(
        text.as_bytes(),
        LEVEL,
    ));
    let room = LIMIT - HEAD;
    let parts = packed.len().div_ceil(room);
    if parts > RING {
        return Some(format!(
            "recorder: tick {time} is {} bytes packed, over the ring",
            packed.len()
        ));
    }
    let segments = raw_memory::segments();
    let first = time.unsigned_abs() as usize * parts;
    for (part, chunk) in packed.as_bytes().chunks(room).enumerate() {
        let id = FIRST + (first + part) % RING;
        let chunk = std::str::from_utf8(chunk).unwrap_or_default();
        segments.set(id as u8, format!("{time} {part} {parts} {chunk}"));
    }
    None
}
