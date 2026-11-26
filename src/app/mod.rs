pub mod driver;
pub mod executor;
pub mod record;
pub mod wire;
pub mod world;

#[cfg(target_arch = "wasm32")]
fn hook_panics() {
    std::panic::set_hook(Box::new(|info| {
        let where_ = info.location().map_or_default(|at| format!("{}:{}", at.file(), at.line()));
        let message = info
            .payload()
            .downcast_ref::<&str>()
            .map(std::string::ToString::to_string)
            .or_else(|| info.payload().downcast_ref::<String>().cloned())
            .unwrap_or_default();
        let _ = wire::console_error(
            &format!("fabot panic at {where_}: {message}"),
            &js_sys::Error::new(&message).into(),
        );
    }));
}
