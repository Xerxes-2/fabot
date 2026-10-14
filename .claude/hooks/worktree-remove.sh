#!/usr/bin/env bash
# WorktreeRemove hook, the counterpart of worktree-create.sh; also what
# worktree-sweep.sh feeds each registered workspace to.
# Snapshot the workspace, then:
#   - its @ empty: forget it and abandon @, and its parent too only when that parent is
#     this workspace's own frozen "cc-base:" copy, still mutable, with no other child;
#   - its @ holds work: label it "cc: <name>" if undescribed, forget the workspace. The
#     change stays in the DAG to be shipped or abandoned from the main workspace.
# Then kill what still runs in it (kill-cwd.sh) and delete the directory. Paths
# outside .claude/worktrees/ are left alone.
set -uo pipefail

p=$(jq -r '.worktree_path // empty' <<<"$(cat)")
[ -n "$p" ] && [ -d "$p" ] || exit 0
case "$p" in
  */.claude/worktrees/*) ;;
  *) echo "worktree-remove: $p is not under .claude/worktrees/, left alone" >&2; exit 0 ;;
esac
main=${p%%/.claude/worktrees/*}
name=$(basename "$p")
ws="cc-$name"

j() { jj -R "$main" --ignore-working-copy "$@"; }
if j workspace list -T 'name ++ "\n"' 2>/dev/null | grep -qx "$ws"; then
  jj -R "$p" status >/dev/null 2>&1 # snapshot the subagent's edits
  own_base="$ws@- & description(regex:\"^cc-base: \") & mutable() & ~parents(children($ws@-) ~ $ws@)"
  ids=$(j log -r "$ws@ | ($own_base)" --no-graph -T 'commit_id ++ " "')
  if [ "$(j log -r "$ws@" --no-graph -T 'empty')" = "true" ]; then
    j workspace forget "$ws" >&2
    j abandon $ids >&2
  else
    [ -n "$(j log -r "$ws@" --no-graph -T 'description')" ] || j describe -r "$ws@" -m "cc: $name" >&2
    j workspace forget "$ws" >&2
  fi
fi
"$main/.claude/hooks/kill-cwd.sh" "$p"
rm -rf "$p"
