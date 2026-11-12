pub mod driver;
pub mod executor;
pub mod js;
pub mod record;
pub mod wire;
pub mod world;

#[cfg(target_arch = "wasm32")]
fn hook_panics() {
    use wasm_bindgen::JsCast;

    std::panic::set_hook(Box::new(|info| {
        let where_ = info
            .location()
            .map_or_default(|at| format!("{}:{}", at.file(), at.line()));
        let message = info
            .payload()
            .downcast_ref::<&str>()
            .map(std::string::ToString::to_string)
            .or_else(|| info.payload().downcast_ref::<String>().cloned())
            .unwrap_or_default();
        if let Ok(console) = js_sys::Reflect::get(
            &js_sys::global(),
            &wasm_bindgen::JsValue::from_str("console"),
        ) && let Ok(error) =
            js_sys::Reflect::get(&console, &wasm_bindgen::JsValue::from_str("error"))
        {
            let error: &js_sys::Function = error.unchecked_ref();
            let _ = error.call2(
                &console,
                &wasm_bindgen::JsValue::from_str(&format!("fabot panic at {where_}: {message}")),
                &js_sys::Error::new(&message).into(),
            );
        }
    }));
}
