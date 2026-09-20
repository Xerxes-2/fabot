#!/usr/bin/env bash
# ADR citation gate.
#
# Why this exists: a decision's reasoning lives in its ADR and nowhere else.
# Code carries at most a pointer (`// ADR-NNNN`) at the site that implements
# the decision. Restating the reasoning at every site it touches leaves the
# restatements false the day the decision changes.
#
# Usage: scripts/adr-check.sh            gate: fails on a citation of a superseded
#                                         or nonexistent ADR, an ADR with no status,
#                                         a duplicate number, or more citations
#                                         than scripts/adr-ceiling allows
#        scripts/adr-check.sh --report   counts: citations, dead citations, the
#                                         most-cited ADRs, comments/code ratio
#        scripts/adr-check.sh --ratchet  write the current citation count to
#                                         scripts/adr-ceiling (it only goes down)
#        scripts/adr-check.sh --index    list the live ADRs
set -euo pipefail
cd "$(dirname "$0")/.."

ADR_DIR=docs/adr
CEILING_FILE=scripts/adr-ceiling
MODE="${1:-gate}"

code_files() { find src tests \( -name '*.fs' -o -name '*.fsi' \) | sort; }

# One line per citation: <file>:<line>:<nnnn>
citations() {
  { code_files | xargs grep -noE 'ADR[ -]?[0-9]{4}' 2>/dev/null || true; } \
    | sed -E 's/ADR[ -]?([0-9]{4})$/\1/'
}

# One line per ADR: <nnnn>\t<status>\t<title>
# status is the first 15 lines' Status line, lower-cased and stripped to its
# verdict: "accepted", "superseded by NNNN", "amended by ...", or "" if absent.
adr_table() {
  for f in "$ADR_DIR"/[0-9][0-9][0-9][0-9]-*.md; do
    n=$(basename "$f" | cut -c1-4)
    title=$(head -1 "$f" | sed 's/^# //')
    status=$( { head -15 "$f" | grep -E 'Status:' || true; } | head -1 \
      | sed -E 's/.*[Ss]tatus:?\*{0,2}:? *//; s/\*//g; s/[[:space:]]+$//' | tr 'A-Z' 'a-z')
    printf '%s\t%s\t%s\n' "$n" "$status" "$title"
  done
}

dead_numbers() { adr_table | awk -F'\t' '$2 ~ /superseded by [0-9]{4}/ {print $1}'; }
known_numbers() { adr_table | cut -f1 | sort -u; }

case "$MODE" in
  --index)
    adr_table | awk -F'\t' '$2 !~ /superseded by [0-9]{4}/ {printf "%s  %-40s %s\n", $1, $2, $3}'
    ;;
  --report)
    total=$(citations | wc -l)
    dead_numbers | sort -u > /tmp/adr-dead.$$
    known_numbers > /tmp/adr-known.$$
    dead=$(citations | awk -F: 'NR==FNR {d[$1]=1; next} d[$3] {c++} END {print c+0}' /tmp/adr-dead.$$ -)
    unknown=$(citations | awk -F: 'NR==FNR {k[$1]=1; next} !k[$3] {c++} END {print c+0}' /tmp/adr-known.$$ -)
    nostatus=$(adr_table | awk -F'\t' '$2==""' | wc -l)
    ceiling=$(cat "$CEILING_FILE" 2>/dev/null || echo none)
    echo "citations: $total (ceiling $ceiling)"
    echo "dead citations (superseded ADR): $dead"
    echo "citations of a nonexistent ADR: $unknown"
    echo "ADRs without a status: $nostatus / $(adr_table | wc -l)"
    echo "most-cited:"
    citations | cut -d: -f3 | sort | uniq -c | sort -rn | head -10 | awk '{printf "  ADR %s  %s\n", $2, $1}'
    if command -v tokei >/dev/null; then
      tokei src tests -t 'F#' 2>/dev/null | awk '$1=="F#" {printf "F# comments/code: %s / %s = %.2f\n", $5, $4, $5/$4}'
    fi
    rm -f /tmp/adr-dead.$$ /tmp/adr-known.$$
    ;;
  --ratchet)
    total=$(citations | wc -l)
    old=$(cat "$CEILING_FILE" 2>/dev/null || echo "$total")
    if [ "$total" -gt "$old" ]; then
      echo "refusing to raise the ceiling: $old -> $total" >&2; exit 1
    fi
    echo "$total" > "$CEILING_FILE"
    echo "ceiling: $old -> $total"
    ;;
  gate)
    fail=0
    dups=$(known_numbers | wc -l); files=$(adr_table | wc -l)
    if [ "$dups" -ne "$files" ]; then
      echo "duplicate ADR numbers:"; adr_table | cut -f1 | sort | uniq -d | sed 's/^/  /'; fail=1
    fi
    if adr_table | awk -F'\t' '$2==""' | grep -q .; then
      echo "ADRs without a Status in their first 15 lines:"
      adr_table | awk -F'\t' '$2=="" {print "  " $1 "  " $3}'; fail=1
    fi
    dead_numbers | sort -u > /tmp/adr-dead.$$; known_numbers > /tmp/adr-known.$$
    if citations | awk -F: 'NR==FNR {d[$1]=1; next} d[$3] {print "  " $1 ":" $2 "  ADR " $3}' /tmp/adr-dead.$$ - | grep -q .; then
      echo "citations of a superseded ADR:"
      citations | awk -F: 'NR==FNR {d[$1]=1; next} d[$3] {print "  " $1 ":" $2 "  ADR " $3}' /tmp/adr-dead.$$ -; fail=1
    fi
    if citations | awk -F: 'NR==FNR {k[$1]=1; next} !k[$3] {print "  " $1 ":" $2 "  ADR " $3}' /tmp/adr-known.$$ - | grep -q .; then
      echo "citations of a nonexistent ADR:"
      citations | awk -F: 'NR==FNR {k[$1]=1; next} !k[$3] {print "  " $1 ":" $2 "  ADR " $3}' /tmp/adr-known.$$ -; fail=1
    fi
    rm -f /tmp/adr-dead.$$ /tmp/adr-known.$$
    if [ -f "$CEILING_FILE" ]; then
      total=$(citations | wc -l); ceiling=$(cat "$CEILING_FILE")
      if [ "$total" -gt "$ceiling" ]; then
        echo "citations above the ceiling: $total > $ceiling (scripts/adr-ceiling)"; fail=1
      fi
    fi
    [ "$fail" -eq 0 ] && echo "adr-check: ok"
    exit "$fail"
    ;;
  *) echo "unknown mode $MODE" >&2; exit 2 ;;
esac
