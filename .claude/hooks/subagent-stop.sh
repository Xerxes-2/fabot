#!/usr/bin/env bash
# SubagentStop hook: remove a finished subagent's workspace as soon as it holds no work.
# The workspace worktree-create.sh made for agent <id> is .claude/worktrees/agent-<id>
# (Claude Code names the worktree after the agent). Claude Code itself never calls
# WorktreeRemove for hook-made subagent worktrees (2.1.294).
#
# Removed only when, after a snapshot, its @ is empty and the agent has no background
# task still running (SubagentStop also fires when an agent pauses on its own background
# work). A workspace holding work stays: the agent may be resumed to amend it, and its
# change is shipped from the main workspace; worktree-sweep.sh removes it at SessionEnd.
set -uo pipefail

input=$(cat)
id=$(jq -r '.agent_id // empty' <<<"$input")
# background_tasks lists the stopping agent itself as running; only others count.
running=$(jq -r --arg id "$id" '[(.background_tasks // [])[] | select(.id != $id)] | length' <<<"$input" 2>/dev/null || echo 0)
[ -n "$id" ] && [ "$running" = "0" ] || exit 0

here=$(cd "$(dirname "$0")/../.." && pwd)
main=${here%%/.claude/worktrees/*}
dest="$main/.claude/worktrees/agent-$id"
[ -d "$dest" ] || exit 0

jj -R "$dest" status >/dev/null 2>&1 # snapshot the agent's edits
[ "$(jj -R "$main" --ignore-working-copy log -r "cc-agent-$id@" --no-graph -T 'empty' 2>/dev/null)" = "true" ] || exit 0

jq -n --arg p "$dest" '{worktree_path: $p}' | "$main/.claude/hooks/worktree-remove.sh"
rm -f "$main/.claude/worktrees/.sessions/"*/"agent-$id"
exit 0
