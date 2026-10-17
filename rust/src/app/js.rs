//! The few JsValue reads both the seam's readers want.

use js_sys::Reflect;
use wasm_bindgen::JsValue;

pub fn field(v: &JsValue, name: &str) -> JsValue {
    Reflect::get(v, &JsValue::from_str(name)).unwrap_or(JsValue::UNDEFINED)
}

pub fn int_of(v: &JsValue) -> Option<i32> {
    v.as_f64().map(|n| n as i32)
}

pub fn int_or_zero(v: &JsValue) -> i32 {
    int_of(v).unwrap_or(0)
}
