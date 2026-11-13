# fabot

Screeps seasonal-server bot in Rust, compiled to WASM and bundled into one JS
file.

This is **v2**: `ARCHITECTURE.md` is the design and `AGENTS.md` the working
rules. v1 is on the `v1` bookmark (`git show v1:<path>`), a rollback only.

## Prerequisites

With Nix (recommended — same toolchain as CI, pinned by `flake.lock`):
`nix develop` gives Node.js 24, `wasm-pack` and the Rust toolchain
`rust-toolchain.toml` pins (`.envrc` enables direnv). Without Nix: Node.js >= 24,
rustup (it reads the same file) and `wasm-pack`. Then:

- `npm install`
- `cp .env.example .env` and fill in your Screeps token

## Workflow

```sh
npm test         # cargo fmt --check, line ceilings, clippy, cargo test
npm run build    # wasm-pack → dist/main.js, dist/main.wasm
npm run smoke    # the bundle runs four ticks against a stub game
npm run scenario # the scenario library on xxscreeps (needs `npm run xx -- setup`)
npm run deploy   # build, smoke, upload to the live server
npm run observe -- health
```

`npm run xx` drives [xxscreeps](https://github.com/laverdet/xxscreeps), the
engine v2 verifies against (`ARCHITECTURE.md` §10.2); `run <scenario.json>`
with `--bot dist` runs the built bundle.
