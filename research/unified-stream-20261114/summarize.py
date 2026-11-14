#!/usr/bin/env python3
"""Generate exact tables from checkpointed JSON; never substitutes missing runs."""
import csv, hashlib, json, pathlib, statistics
root = pathlib.Path(__file__).resolve().parent
out = root/'results'
names = ['answers','pauses','mixed','left','right','wide','failures','fusion',
         'interleave','level','native','roundtrip','counted']
title = dict(answers='连续答案',pauses='连续暂停',mixed='答案/暂停交替',left='左嵌套选择',
             right='右嵌套选择',wide='宽选择',failures='99% 失败分支',fusion='append 反复融合',
             interleave='interleave 公平调度',level='level 同步轮次',native='原生 sequence 消费',
             roundtrip='sequence→amb→sequence',counted='显式暂停块（聚合计数）')
rows = []
table = ['| 工作负载 | A CPU ms | B CPU ms | C CPU ms | D CPU ms | C/A | D/A |',
         '|---|---:|---:|---:|---:|---:|---:|']
details = ['| 工作负载 | 版本 | CPU 中位数 [min,max] ms | Real ms | GC ms | 分配 MiB |',
           '|---|---|---:|---:|---:|---:|']
for name in names:
    data = {v:json.loads((out/f'{name}-{v}.json').read_text()) for v in 'ABCD'}
    a = data['A']
    for v,d in data.items():
        assert d['result']==a['result'], (name,v,'checksum mismatch')
        answers,pauses,checksum=d['result']
        if name=='fusion': answers=d['n']+1
        row = dict(workload=name,variant=v,n=d['n'],answers=answers,pauses=pauses,
                   checksum=checksum,repetitions=d['repetitions'],samples=len(d['samples']))
        for metric in ['cpu_ms','real_ms','gc_ms','allocated_bytes']:
            row[metric]=d[metric]['median']
            row[metric+'_min']=d[metric]['min']
            row[metric+'_max']=d[metric]['max']
            base=a[metric]['median']
            row[metric+'_ratio_A']=d[metric]['median']/base if base else ''
        row['cpu_ns_per_answer']=row['cpu_ms']*1e6/answers if answers else ''
        row['cpu_ns_per_pause']=row['cpu_ms']*1e6/pauses if pauses else ''
        rows.append(row)
        c=d['cpu_ms']
        details.append(f'| {title[name]} | {v} | {c["median"]:.3f} [{c["min"]:.3f},{c["max"]:.3f}] | '
                       f'{d["real_ms"]["median"]:.3f} | {d["gc_ms"]["median"]:.3f} | '
                       f'{d["allocated_bytes"]["median"]/2**20:.3f} |')
    tm=[data[v]['cpu_ms']['median'] for v in 'ABCD']
    table.append('| '+title[name]+' | '+' | '.join(f'{t:.3f}' for t in tm)+
                 f' | {tm[2]/tm[0]:.2f}× | {tm[3]/tm[0]:.2f}× |')
with (out/'summary.csv').open('w',newline='') as f:
    writer=csv.DictWriter(f,fieldnames=list(rows[0]),lineterminator='\n');writer.writeheader();writer.writerows(rows)
(out/'CPU-TABLE.md').write_text('\n'.join(table)+'\n')
(out/'DETAIL-TABLE.md').write_text('\n'.join(details)+'\n')
micro=['| 操作（2,000,000 次） | CPU ms [min,max] | Real ms | GC ms | 总分配 MiB | 字节/次 |',
       '|---|---:|---:|---:|---:|---:|']
for mode in ['pair','values','closure','thunk','reused-thunk']:
    d=json.loads((out/f'micro-{mode}.json').read_text());c=d['cpu_ms']
    micro.append(f'| {mode} | {c["median"]} [{c["min"]},{c["max"]}] | '
                 f'{d["real_ms"]["median"]} | {d["gc_ms"]["median"]} | '
                 f'{d["allocated_bytes"]["median"]/2**20:.3f} | '
                 f'{d["allocated_bytes"]["median"]/d["n"]:.3f} |')
(out/'MICRO-TABLE.md').write_text('\n'.join(micro)+'\n')
manifest={str(p.relative_to(root)):hashlib.sha256(p.read_bytes()).hexdigest()
          for p in sorted(root.rglob('*')) if p.is_file() and 'compiled' not in p.parts
          and p.suffix in ('.rkt','.inc','.py','.sh')}
(out/'source-sha256.json').write_text(json.dumps(manifest,indent=2)+'\n')
print('\n'.join(table))
print('\n'.join(micro))
