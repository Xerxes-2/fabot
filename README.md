# fabot

Screeps seasonal-server bot in F#, compiled to JS by [Fable](https://fable.io).

This is **v2**, a rewrite in progress: `ARCHITECTURE.md` is the design and
`AGENTS.md` the working rules. v1 — the bot running live this season — is
on the `v1` bookmark (`git show v1:<path>`), maintenance only.

## Prerequisites

With Nix (recommended — same toolchain as CI, pinned by `flake.lock`):
`nix develop` gives .NET SDK 10 and Node.js 24 (`.envrc` enables direnv).
Without Nix: .NET SDK 10 and Node.js >= 24. Then:

- `dotnet tool restore` (Fable, Fantomas)
- `npm install`
- `cp .env.example .env` and fill in your Screeps token

## Workflow

```sh
npm test         # format check, line ceilings, dotnet test
npm run build    # Fable → esbuild → dist/main.js
npm run smoke    # the bundle runs four ticks against a stub game
npm run deploy   # build, smoke, upload — runs v2 live; v2 does nothing yet
npm run observe -- health
```

`npm run xx` drives [xxscreeps](https://github.com/laverdet/xxscreeps), the
engine v2 verifies against (`ARCHITECTURE.md` §10.2). Its `run <slug>` mode
read v1's arena tests and no longer has any to read; `run <scenario.json>`
with `--bot dist` runs the built bundle.
