#!/usr/bin/env bash
# `jj ship [-r REV]` in a cloud session (the alias .claude/hooks/session-start.sh sets):
# the gates over the working copy, then the session branch (`claude.session-branch`)
# moved to REV (default @) and pushed. A cloud session may push no other branch, so it
# stands in for `main` (AGENTS.md § Cloud sessions); `main` gets the work by PR.
set -euo pipefail

rev=@
while [ $# -gt 0 ]; do
  case "$1" in
    -r) rev=$2; shift 2 ;;
    *) echo "ship: usage: jj ship [-r REV]" >&2; exit 2 ;;
  esac
done

branch=$(jj config get claude.session-branch 2>/dev/null || true)
[ -n "$branch" ] || { echo "ship: no claude.session-branch: not a cloud session" >&2; exit 1; }

# The gates run on the files on disk, which are @'s: REV must hold the same tree.
[ -z "$(jj diff --from "$rev" --to @ --summary)" ] ||
  { echo "ship: $rev differs from @, whose files the gates run on: jj new $rev first" >&2; exit 1; }
[ -n "$(jj log --no-graph -r "$rev" -T 'description')" ] ||
  { echo "ship: $rev has no description" >&2; exit 1; }

npm test
npm run build
npm run smoke

jj bookmark set "$branch" -r "$rev"

# Shipped from @, jj itself starts a new @: what origin holds is immutable.
for wait in 2 4 8 16 0; do
  jj git push -b "$branch" && exit 0
  [ "$wait" -gt 0 ] || break
  echo "ship: push failed, retrying in ${wait}s" >&2
  sleep "$wait"
done
exit 1
