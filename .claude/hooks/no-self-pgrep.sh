#!/usr/bin/env bash
# A wait loop on `pgrep -f`/`pkill -f` over a command's arguments matches the
# waiting shell's own command line, so it never ends. Refuse it; wait on the
# job itself instead.
cmd=$(jq -r '.tool_input.command // empty')
if grep -Eq '\b(while|until)[[:space:]][^;]*(pgrep|pkill)[[:space:]][^;]*-[a-zA-Z]*f' <<<"$cmd"; then
  echo "Refused: a while/until loop on pgrep -f / pkill -f matches its own shell and never ends. Run the job with run_in_background and end your turn: you are notified when it exits. To wait inside one command, wait on its pid (\`cmd & pid=\$!; wait \$pid\`, or \`kill -0 \$pid\` in the loop)." >&2
  exit 2
fi
exit 0
