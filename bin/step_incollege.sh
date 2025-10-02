#!/usr/bin/env bash
set -euo pipefail

# Simulate stepping through src/InCollege-Input.txt line-by-line.
# It resets data files to a saved baseline before each step so output reflects
# a single continuous session up to the current line, and prints only new output.

ROOT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
SRC_DIR="$ROOT_DIR/src"
SESSION_DIR="$ROOT_DIR/.step_session"

MASTER_INPUT="$SESSION_DIR/master_input"
RUNTIME_INPUT="$SRC_DIR/InCollege-Input.txt"
APP_OUT="$SRC_DIR/InCollege-Output.txt"

baseline_copy() {
  mkdir -p "$SESSION_DIR/baseline"
  cp -f "$SRC_DIR/userinfo.txt" "$SESSION_DIR/baseline/userinfo.txt" 2>/dev/null || :
  cp -f "$SRC_DIR/profiles.txt" "$SESSION_DIR/baseline/profiles.txt" 2>/dev/null || :
  cp -f "$SRC_DIR/profiles.tmp" "$SESSION_DIR/baseline/profiles.tmp" 2>/dev/null || :
  cp -f "$SRC_DIR/profiles.new" "$SESSION_DIR/baseline/profiles.new" 2>/dev/null || :
}

restore_baseline() {
  # Restore saved baseline into working files prior to each run
  cp -f "$SESSION_DIR/baseline/userinfo.txt" "$SRC_DIR/userinfo.txt" 2>/dev/null || :
  cp -f "$SESSION_DIR/baseline/profiles.txt" "$SRC_DIR/profiles.txt" 2>/dev/null || :
  cp -f "$SESSION_DIR/baseline/profiles.tmp" "$SRC_DIR/profiles.tmp" 2>/dev/null || :
  cp -f "$SESSION_DIR/baseline/profiles.new" "$SRC_DIR/profiles.new" 2>/dev/null || :
}

init_session() {
  rm -rf "$SESSION_DIR"
  mkdir -p "$SESSION_DIR"
  baseline_copy
  cp -f "$SRC_DIR/InCollege-Input.txt" "$MASTER_INPUT"
  : > "$SESSION_DIR/prev_output"
  echo 0 > "$SESSION_DIR/n"
  echo "Initialized step session from src/InCollege-Input.txt"
}

ensure_session() {
  if [[ ! -f "$SESSION_DIR/n" ]]; then
    init_session
  fi
}

compute_delta() {
  local prev="$SESSION_DIR/prev_output"
  local curr="$APP_OUT"
  # Print only lines after the longest common prefix of prev and curr
  awk 'NR==FNR{a[NR]=$0; n=NR; next} {if (FNR<=n && $0==a[FNR]) next; print}' "$prev" "$curr"
}

do_step() {
  ensure_session
  local targetN=""
  if [[ $# -ge 1 ]]; then
    targetN="$1"
  fi
  local n total
  n=$(cat "$SESSION_DIR/n")
  total=$(wc -l < "$MASTER_INPUT" | tr -d ' ')

  if [[ -n "$targetN" ]]; then
    if [[ "$targetN" -lt 0 || "$targetN" -gt "$total" ]]; then
      echo "Target line out of range (0..$total)" >&2
      exit 1
    fi
    n="$targetN"
  else
    n=$((n + 1))
  fi

  if [[ "$n" -le 0 ]]; then
    echo "Nothing to run yet (n=$n)."; echo "$n" > "$SESSION_DIR/n"; exit 0
  fi
  if [[ "$n" -gt "$total" ]]; then
    echo "Reached end of input ($total lines)."; echo "$total" > "$SESSION_DIR/n"; exit 0
  fi

  restore_baseline
  head -n "$n" "$MASTER_INPUT" > "$RUNTIME_INPUT"
  rm -f "$APP_OUT"
  (cd "$ROOT_DIR" && ./InCollege) >/dev/null 2>&1 || true

  compute_delta | tee "$SESSION_DIR/delta.out"
  cp -f "$APP_OUT" "$SESSION_DIR/prev_output" 2>/dev/null || :
  echo "$n" > "$SESSION_DIR/n"
}

show_status() {
  ensure_session
  local n total
  n=$(cat "$SESSION_DIR/n")
  total=$(wc -l < "$MASTER_INPUT" | tr -d ' ')
  echo "Lines executed: $n / $total"
  if [[ "$n" -lt "$total" ]]; then
    echo -n "Next line: "
    sed -n "$((n+1))p" "$MASTER_INPUT" || true
  fi
}

show_output() {
  if [[ -f "$APP_OUT" ]]; then
    cat "$APP_OUT"
  else
    echo "No output yet."
  fi
}

case "${1:-}" in
  init|reset)
    init_session ;;
  step)
    shift || true
    do_step "${1:-}" ;;
  status)
    show_status ;;
  output)
    show_output ;;
  *)
    cat << EOF
Usage: bin/step_incollege.sh <command> [args]

Commands:
  init|reset       Initialize session from src/InCollege-Input.txt
  step [N]         Run up to N lines (or next line if omitted) and print new output
  status           Show current step and next input line
  output           Show full current output file
EOF
    ;;
esac

