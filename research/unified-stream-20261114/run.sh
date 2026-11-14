#!/usr/bin/env bash
set -euo pipefail
cd -- "$(dirname -- "${BASH_SOURCE[0]}")"
RACKET=${RACKET:-racket}
RACO=${RACO:-raco}
timeout 20s "$RACO" make b.rkt c.rkt d.rkt semantics.rkt bench.rkt micro.rkt fusion-scaling.rkt
timeout 35s "$RACO" test semantics.rkt
if [[ $# -eq 0 ]]; then set -- answers pauses mixed; fi
RACKET="$RACKET" timeout 55s python3 run-bench.py "$@"
