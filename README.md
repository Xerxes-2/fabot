# fabot

Screeps seasonal-server bot in Rust, compiled to WASM and bundled into one JS
file.

This is **v2**: `ARCHITECTURE.md` is the design and `AGENTS.md` the working
rules. v1 lives on the `v1` bookmark (`git show v1:<path>`), a rollback only.

## Prerequisites

With Nix (recommended — same toolchain as CI, pinned by `flake.lock`):
`nix develop` gives Node.js 24, `wasm-pack` and the Rust toolchain
`rust-toolchain.toml` pins (`.envrc` enables direnv). Without Nix: Node.js >= 24,
rustup (it reads the same file) and `wasm-pack`. Then:

- `npm install`
- `cp .env.example .env` and fill in your Screeps token
- `jj ship` (`AGENTS.md`): the `aliases.ship` that `.claude/hooks/session-start.sh` sets, and locally `jj config set --repo claude.session-branch main`

## Workflow

```sh
npm test          # cargo fmt --check, line ceilings, clippy, cargo test
npm run build     # wasm-pack → dist/main.js, dist/main.wasm
npm run smoke     # bundle runs four ticks against a stub game
npm run build:sim # PGO native simulator (`sim/`, Rust port of official engine)
npm run scenario  # scenario library (native `sim`)
npm run soak:many # long-horizon economy soak across seat extremes
npm run start-rooms -- <world.json>  # rank a first room by the bot's own claim rule
npm run deploy    # build, smoke, upload to the live server
npm run observe -- health
```

