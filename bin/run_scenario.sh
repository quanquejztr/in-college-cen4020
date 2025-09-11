#!/usr/bin/env bash
set -euo pipefail

scenario=${1:-}
if [[ -z "${scenario}" ]]; then
  echo "Usage: bin/run_scenario.sh <path-to-scenario>"
  exit 1
fi

if [[ ! -f "${scenario}" ]]; then
  echo "Scenario not found: ${scenario}"
  exit 1
fi

cp "${scenario}" src/InCollege-Test.txt
:
> src/InCollege-Output.txt
./InCollege
echo "--- Output (tail) ---"
tail -n 80 src/InCollege-Output.txt

