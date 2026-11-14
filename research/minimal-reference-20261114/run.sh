#!/usr/bin/env bash
set -euo pipefail
cd -- "$(dirname -- "${BASH_SOURCE[0]}")"
RACO=${RACO:-raco}
RACKET=${RACKET:-racket}
timeout 20s "$RACO" make kernel.rkt tests.rkt examples.rkt
timeout 45s "$RACO" test tests.rkt
timeout 10s "$RACKET" examples.rkt
