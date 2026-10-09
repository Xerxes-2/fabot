#!/usr/bin/env bash
# SessionEnd hook: remove the subagent workspaces this session created (registered by
# worktree-create.sh), each through worktree-remove.sh.
#
# By hand, from anywhere in the repo:
#   .claude/hooks/worktree-sweep.sh <session_id>   one session's leftovers
#   .claude/hooks/worktree-sweep.sh --all          every registered workspace, live ones too
#   .claude/hooks/worktree-sweep.sh --stale        forget cc-* workspaces whose directory is
#                                                  gone, and drop directories no workspace owns
set -uo pipefail

here=$(cd "$(dirname "$0")/../.." && pwd)
main=${here%%/.claude/worktrees/*}
worktrees="$main/.claude/worktrees"
reg="$worktrees/.sessions"
remove="$main/.claude/hooks/worktree-remove.sh"

stale() {
  local ws dir
  while IFS=$'\t' read -r ws dir; do
    [[ "$ws" == cc-* ]] || continue
    if [ ! -d "$dir" ]; then
      jj -R "$main" --ignore-working-copy workspace forget "$ws" >&2 && echo "forgot $ws (no directory)" >&2
    fi
  done < <(jj -R "$main" --ignore-working-copy workspace list -T 'name ++ "\t" ++ self.root() ++ "\n"' 2>/dev/null)
  for dir in "$worktrees"/*/; do
    [ -d "$dir" ] || continue
    dir=${dir%/}
    jj -R "$main" --ignore-working-copy workspace list -T 'name ++ "\n"' | grep -qx "cc-$(basename "$dir")" \
      || { rm -rf "$dir" && echo "removed $dir (no workspace)" >&2; }
  done
}

case "${1:-}" in
  --stale) stale; exit 0 ;;
  --all) dirs=("$reg"/*/) ;;
  "")
    sid=$(jq -r '.session_id // empty' <<<"$(cat)")
    [ -n "$sid" ] || exit 0
    dirs=("$reg/$sid/")
    ;;
  *) dirs=("$reg/$1/") ;;
esac

rc=0
for d in "${dirs[@]}"; do
  [ -d "$d" ] || continue
  for m in "$d"*; do
    [ -f "$m" ] || continue
    if jq -n --arg p "$(<"$m")" '{worktree_path: $p}' | "$remove"; then
      rm -f "$m"
    else
      echo "worktree-sweep: could not remove $(<"$m") (kept $m)" >&2
      rc=1
    fi
  done
  rmdir "$d" 2>/dev/null
done
exit $rc
