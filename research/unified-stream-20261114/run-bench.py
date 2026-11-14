#!/usr/bin/env python3
"""Run at most a small selected group; checkpoint each isolated process.

RACKET=/path/to/racket python3 run-bench.py answers pauses mixed
Each child: hard 20 second deadline, finite n, 1 warmup + 3 calibration runs + 7 batches.
"""
import json, os, pathlib, platform, subprocess, sys, time
ROOT = pathlib.Path(__file__).resolve().parent
RACKET = os.environ.get('RACKET', 'racket')
SIZES = dict(answers=200000, pauses=200000, mixed=200000,
             left=4000, right=4000, wide=20000, failures=20000,
             fusion=6000, interleave=60000, level=100,
             native=200000, roundtrip=20000, counted=200000)

def main():
    names = sys.argv[1:]
    if not names or any(x not in SIZES for x in names):
        raise SystemExit('Choose workloads: ' + ' '.join(SIZES))
    output = ROOT / 'results'
    output.mkdir(exist_ok=True)
    env = dict(platform=platform.platform(), machine=platform.machine(),
               python=platform.python_version(), racket=RACKET,
               captured_utc=time.strftime('%Y-%m-%dT%H:%M:%SZ', time.gmtime()))
    (output / 'environment.json').write_text(json.dumps(env, indent=2))
    for index, name in enumerate(names):
        expected = None
        # Rotate variant order to reduce systematic order bias.
        order = 'ABCD'[index % 4:] + 'ABCD'[:index % 4]
        for variant in order:
            dest = output / f'{name}-{variant}.json'
            started = time.monotonic()
            try:
                p = subprocess.run([RACKET, str(ROOT/'bench.rkt'), variant, name,
                                    str(SIZES[name])], cwd=ROOT,
                                   text=True, capture_output=True, timeout=20)
                if p.returncode:
                    raise RuntimeError(f'exit {p.returncode}: {p.stderr[:2000]}')
                data = json.loads(p.stdout)
                if any(type(value) is not int for value in data['result'][:2]):
                    raise RuntimeError('answer/pause counters must remain exact integers')
                # All workloads compare answers, pauses AND checksum.
                if expected is None:
                    expected = data['result']
                if data['result'] != expected:
                    raise RuntimeError(f'result {data["result"]} != {expected}')
                dest.write_text(json.dumps(data, indent=2)+'\n')
                print(name, variant, 'CPU median', round(data['cpu_ms']['median'], 3),
                      'ms;', round(time.monotonic()-started, 2), 's process', flush=True)
            except (subprocess.TimeoutExpired, RuntimeError, ValueError) as exc:
                (output/f'{name}-{variant}.error.txt').write_text(str(exc))
                raise SystemExit(str(exc))

if __name__ == '__main__':
    main()
