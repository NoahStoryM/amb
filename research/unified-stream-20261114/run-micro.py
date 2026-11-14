#!/usr/bin/env python3
import json, os, pathlib, subprocess
root = pathlib.Path(__file__).resolve().parent
racket = os.environ.get('RACKET', 'racket')
for mode in ['pair', 'values', 'closure', 'thunk', 'reused-thunk']:
    p = subprocess.run([racket, str(root/'micro.rkt'), mode],
                       capture_output=True, text=True, timeout=15, check=True)
    result = json.loads(p.stdout)
    (root/'results'/f'micro-{mode}.json').write_text(json.dumps(result, indent=2)+'\n')
    print(mode, result['cpu_ms'], result['allocated_bytes'], flush=True)
