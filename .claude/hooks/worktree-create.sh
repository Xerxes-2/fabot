#!/usr/bin/env bash
# WorktreeCreate hook: a jj workspace for a subagent (isolation: "worktree") or a
# --worktree session, inside this repo at .claude/worktrees/<name> (ignored).
# Prints the workspace path on stdout; everything else goes to stderr.
#
# Base, in order of preference:
#   1. the revset in repo config `claude.worktree-base`, when it resolves to exactly
#      one commit (an orchestrator's integration bookmark:
#      `jj config set --repo claude.worktree-base integ`, unset after);
#   2. in a cloud session, its branch (`claude.session-branch`, set by session-start.sh),
#      which stands in for `main` there;
#   3. trunk() (the `main` bookmark);
#   4. a frozen copy of the main workspace's @ (`jj duplicate`, "cc-base: <name>"),
#      never @ itself, which the main workspace keeps rewriting.
#
# Subagent workspaces ("agent-*") are registered under .claude/worktrees/.sessions/
# <session_id>/ so worktree-sweep.sh removes them at SessionEnd: Claude Code does not
# call WorktreeRemove for hook-made subagent worktrees.
#
# Setup of the new workspace: node_modules linked from the main workspace, .env
# copied when present; then, if present and executable,
# .claude/worktree-setup.local.sh <workspace> <main> for anything machine-local.
set -euo pipefail

input=$(cat)
name=$(jq -r '.name // .worktree_name // empty' <<<"$input")
sid=$(jq -r '.session_id // empty' <<<"$input")
[ -n "$name" ] || { echo "worktree-create: no name in input" >&2; exit 1; }

here=$(cd "$(dirname "$0")/../.." && pwd)
main=${here%%/.claude/worktrees/*}
worktrees="$main/.claude/worktrees"
dest="$worktrees/$name"
[ -e "$dest" ] && { echo "worktree-create: $dest already exists" >&2; exit 1; }
mkdir -p "$worktrees"

j() { jj -R "$main" --ignore-working-copy --color never "$@"; }
one() { [ "$(j log --no-graph -r "$1" -T '"x"' 2>/dev/null)" = "x" ]; }

cfg_base=$(j config get claude.worktree-base 2>/dev/null || true)
session_branch=$(j config get claude.session-branch 2>/dev/null || true)
if [ -n "$cfg_base" ] && one "$cfg_base"; then
  base="$cfg_base"
  echo "worktree-create: basing on claude.worktree-base = $cfg_base" >&2
elif [ -n "$session_branch" ] && one "bookmarks(exact:\"$session_branch\")"; then
  base="bookmarks(exact:\"$session_branch\")"
  echo "worktree-create: basing on the session branch $session_branch" >&2
elif one 'trunk() ~ root()'; then
  base='trunk()'
else
  out=$(jj -R "$main" --color never duplicate @ 2>&1)
  base=$(sed -n 's/^Duplicated [0-9a-f]* as \([a-z]*\) .*/\1/p' <<<"$out" | head -1)
  [ -n "$base" ] || { echo "worktree-create: cannot parse duplicate output: $out" >&2; exit 1; }
  jj -R "$main" describe -r "$base" -m "cc-base: $name" >&2
fi

jj -R "$main" workspace add --name "cc-$name" -r "$base" "$dest" >&2

if [[ "$name" == agent-* && -n "$sid" ]]; then
  mkdir -p "$worktrees/.sessions/$sid"
  printf '%s\n' "$dest" >"$worktrees/.sessions/$sid/$name"
fi

# Setup never blocks a subagent: a failure is logged and the workspace still returned.
{
  [ -e "$dest/node_modules" ] || [ ! -d "$main/node_modules" ] || ln -s "$main/node_modules" "$dest/node_modules"
  [ -e "$dest/.env" ] || [ ! -f "$main/.env" ] || cp "$main/.env" "$dest/.env"
  local_setup="$main/.claude/worktree-setup.local.sh"
  if [ -x "$local_setup" ]; then
    "$local_setup" "$dest" "$main" || echo "worktree-create: $local_setup failed ($?)" >&2
  fi
} >&2

echo "$dest"
