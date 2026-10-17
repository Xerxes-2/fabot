# fabot

Screeps seasonal-server bot in Rust, compiled to WASM and bundled into one JS
file. The F#/Fable tree it is ported from (`src/`) still builds: `rust/` is the
tree under change, `src/` the oracle `npm run parity` reads (`ARCHITECTURE.md`
§10.2).

This is **v2**, a rewrite in progress: `ARCHITECTURE.md` is the design and
`AGENTS.md` the working rules. v1 — the bot running live this season — is
on the `v1` bookmark (`git show v1:<path>`), maintenance only.

## Prerequisites

With Nix (recommended — same toolchain as CI, pinned by `flake.lock`):
`nix develop` gives .NET SDK 10, Node.js 24 and the Rust toolchain with
`wasm-pack` (`.envrc` enables direnv). Without Nix: .NET SDK 10, Node.js >= 24,
Rust and `wasm-pack`. Then:

- `dotnet tool restore` (Fable, Fantomas — the F# tree only)
- `npm install`
- `cp .env.example .env` and fill in your Screeps token

## Workflow

```sh
npm test         # format check, line ceilings, dotnet test
npm run test:rust # cargo fmt, clippy, cargo test
npm run build    # Fable → esbuild → dist/main.js
npm run build:rust # wasm-pack → dist/fabot-rs.js
npm run parity -- --a dist/main.js --b dist/fabot-rs.js <capture.jsonl>
npm run smoke    # the bundle runs four ticks against a stub game
npm run scenario # the scenario library on xxscreeps (needs `npm run xx -- setup`)
npm run deploy   # build, smoke, upload — runs v2 live; v2 does nothing yet
npm run observe -- health
```

`npm run xx` drives [xxscreeps](https://github.com/laverdet/xxscreeps), the
engine v2 verifies against (`ARCHITECTURE.md` §10.2). Its `run <slug>` mode
read v1's arena tests and no longer has any to read; `run <scenario.json>`
with `--bot dist` runs the built bundle.
