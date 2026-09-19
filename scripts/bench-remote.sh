#!/usr/bin/env bash
# Interleaved A/B of two bundles on a quiet remote box (#384).
#
# Why this exists: an A/B of a few percent cannot be measured on a workstation
# that is also running something else. Locally the same bundle measured 3.28
# and 4.74 ms/tick in one evening; on the remote box five runs of one bundle
# span 0.19 ms on 11.8, which is +/- 0.8%. The harness needs only node and the
# built bundle, so the box needs no dotnet.
#
# Usage: scripts/bench-remote.sh <base.js> <fix.js> [pairs] [profile args...]
set -euo pipefail

BASE="$1"; FIX="$2"; PAIRS="${3:-5}"; shift 3 || shift 2
ARGS="${*:-400 10 --scenario reactor --level 7}"
HOST="${FABOT_BENCH_HOST:-OCI-Ubuntu-arm}"
REMOTE="${FABOT_BENCH_DIR:-fabot-bench}"

rsync -az --delete \
  --exclude node_modules --exclude build --exclude .jj --exclude .git \
  --exclude obj --exclude bin --exclude '*.cpuprofile' --exclude dist \
  ./ "$HOST:$REMOTE/"
scp -q "$BASE" "$HOST:$REMOTE/base.js"
scp -q "$FIX" "$HOST:$REMOTE/fix.js"

ssh "$HOST" "bash -s" <<REMOTE_SCRIPT
set -euo pipefail
cd "$REMOTE"
mkdir -p dist
for i in \$(seq 1 $PAIRS); do
  cp base.js dist/main.js
  b=\$(node scripts/profile.mjs $ARGS 2>&1 | grep -oE 'mean [0-9.]+' | head -1 | cut -d' ' -f2)
  cp fix.js dist/main.js
  f=\$(node scripts/profile.mjs $ARGS 2>&1 | grep -oE 'mean [0-9.]+' | head -1 | cut -d' ' -f2)
  echo "pair \$i  base \$b  fix \$f"
done
REMOTE_SCRIPT
