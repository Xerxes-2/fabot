# fabot

Screeps seasonal-server bot in F#, compiled to JS via [Fable](https://fable.io).

## Prerequisites

- .NET SDK 10, Node.js >= 22
- `dotnet tool restore` (installs Fable)
- `npm install` (installs esbuild)
- `cp .env.example .env` and fill in your Screeps auth token
  (create one at https://screeps.com/a/#!/account/auth-tokens)

## Workflow

```sh
npm run deploy   # build (Fable → esbuild → dist/main.js) + upload to the server
```

Or separately: `npm run build` / `npm run upload`.

The bundle is a single CommonJS `main` module exporting `loop`, uploaded via
`POST <SCREEPS_API_URL>/api/user/code`. Server contract details and sources:
`docs/research/fable-screeps.md`.
