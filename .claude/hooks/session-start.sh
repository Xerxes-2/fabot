#!/usr/bin/env bash
# SessionStart hook, cloud sessions only: the setup that needs the repo checkout
# (the environment's setup script runs before the clone and installs the Rust toolchain, wasm-pack and jj).
# Idempotent; output goes to stderr so it never lands in the session context.
#
# A cloud session may push only the branch it was cloned on, so that branch stands in
# for `main` (AGENTS.md § Cloud sessions): it is recorded as `claude.session-branch`,
# which worktree-create.sh bases subagent workspaces on and scripts/ship.sh pushes.
set -euo pipefail

[ "${CLAUDE_CODE_REMOTE:-}" = "true" ] || exit 0
cd "$CLAUDE_PROJECT_DIR"

{
  # Read before jj colocates: jj then detaches git's HEAD.
  branch=$(git symbolic-ref --quiet --short HEAD || true)

  [ -d .jj ] || jj git init --colocate

  if [ -n "$branch" ]; then
    jj config set --repo claude.session-branch "$branch"
    # The clone writes origin's ref for a branch origin may not have yet; a fetch
    # corrects it, or the first push fails its lease ("stale info").
    jj git fetch -b "$branch" || true
  fi

  # A pushed change is immutable (AGENTS.md): an edit after a push lands in a new one.
  jj config set --repo 'revset-aliases."immutable_heads()"' \
    'builtin_immutable_heads() | remote_bookmarks(remote=exact:"origin")'

  # jj takes git's identity; the working-copy change made before it had none.
  name=$(git config user.name || true)
  email=$(git config user.email || true)
  if [ -n "$name" ] && [ -n "$email" ]; then
    jj config set --repo user.name "$name"
    jj config set --repo user.email "$email"
    [ -n "$(jj log --no-graph -r @ -T 'author.email()')" ] || jj metaedit --update-author @
  fi

  # `jj ship [-r REV]`: the gates, then the session branch pushed (scripts/ship.sh).
  jj config set --repo aliases.ship \
    '["util", "exec", "--", "bash", "-c", "exec \"$JJ_WORKSPACE_ROOT/scripts/ship.sh\" \"$@\"", "ship"]'

  npm install --no-audit --no-fund
} >&2
