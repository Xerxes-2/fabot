#!/usr/bin/env bash
# Kill every process whose working directory is <dir> or under it, with its
# descendants: a removed workspace's shells (an `until` loop waiting on a file
# the removal deleted) would otherwise run on forever.
set -uo pipefail
dir=${1%/}
case "$dir" in
  */.claude/worktrees/?*) ;;
  *) exit 0 ;;
esac

spare=" "
p=$$
while [ "$p" -gt 1 ]; do
  spare+="$p "
  p=$(ps -o ppid= -p "$p" | tr -d ' ') || break
done

# The parent first, so a loop cannot start a new child after its children die.
tree() {
  local c kids
  kids=$(pgrep -P "$1")
  kill "$1" 2>/dev/null
  for c in $kids; do tree "$c"; done
}

for p in /proc/[0-9]*; do
  pid=${p#/proc/}
  [[ "$spare" == *" $pid "* ]] && continue
  cwd=$(readlink "$p/cwd" 2>/dev/null) || continue
  case "$cwd" in
    "$dir" | "$dir/"* | "$dir (deleted)") tree "$pid" ;;
  esac
done
exit 0
