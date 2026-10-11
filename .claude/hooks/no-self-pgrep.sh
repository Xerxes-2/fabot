#!/usr/bin/env bash
# A wait loop on `pgrep -f`/`pkill -f` over a command's arguments matches the
# waiting shell's own command line, so it never ends. Refuse it; wait on the
# job itself instead.
cmd=$(jq -r '.tool_input.command // empty')
if grep -Eq '\b(while|until)[[:space:]]+(!?[[:space:]]*)(pgrep|pkill)[[:space:]][^;]*-[a-zA-Z]*f' <<<"$cmd"; then
  echo "Refused: a while/until loop on pgrep -f / pkill -f matches its own shell and never ends. Wait on the job's pid instead: \`cmd & pid=\$!; wait \$pid\`, or for a process you did not start \`kill -0 \$pid\` in the loop. A background Bash command notifies you when it exits." >&2
  exit 2
fi
exit 0
