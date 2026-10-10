#!/usr/bin/env bash
# SessionStart hook, cloud sessions only: the setup that needs the repo checkout
# (the environment's setup script runs before the clone and installs dotnet + jj).
# Idempotent; output goes to stderr so it never lands in the session context.
set -euo pipefail

[ "${CLAUDE_CODE_REMOTE:-}" = "true" ] || exit 0
cd "$CLAUDE_PROJECT_DIR"

{
  [ -d .jj ] || jj git init --colocate
  dotnet tool restore
  dotnet restore
  npm install --no-audit --no-fund
} >&2
