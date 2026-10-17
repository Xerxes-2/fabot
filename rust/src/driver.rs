use wasm_bindgen::prelude::*;

#[wasm_bindgen(js_name = loop)]
pub fn tick() {
    let time = screeps::game::time();
    let _ = time;
}
