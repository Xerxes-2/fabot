//! The few JsValue reads both the seam's readers want.

use crate::core::HashMap;
use std::cell::RefCell;

use js_sys::Reflect;
use wasm_bindgen::JsValue;

thread_local! {
    /// A property name costs a JS string to read by; the seam reads the same
    /// few hundred thousand a tick, so each literal is built once.
    static KEYS: RefCell<HashMap<&'static str, JsValue>> = RefCell::new(HashMap::default());
}

pub fn warm(names: &[&'static str]) {
    KEYS.with(|keys| {
        let mut keys = keys.borrow_mut();
        for name in names {
            keys.entry(name).or_insert_with(|| JsValue::from_str(name));
        }
    });
}

pub fn field(v: &JsValue, name: &str) -> JsValue {
    KEYS.with(|keys| {
        let keys = keys.borrow();
        match keys.get(name) {
            Some(key) => Reflect::get(v, key).unwrap_or(JsValue::UNDEFINED),
            None => Reflect::get(v, &JsValue::from_str(name)).unwrap_or(JsValue::UNDEFINED),
        }
    })
}

pub fn int_of(v: &JsValue) -> Option<i32> {
    v.as_f64().map(|n| n as i32)
}
