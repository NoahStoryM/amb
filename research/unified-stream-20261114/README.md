# amb 统一 Values 流实验

基线提交：`d7edfd5e8b3bcbc5dcd04cf810fff7fed02c609c`，研究分支 `research/choice-as-merge-20261112`。原目录不变；本目录包含可独立运行的 A/B/C/D、自动测试和原始测量。

审阅从 [REVIEW.md](REVIEW.md) 开始；完整分析见 [REPORT.md](REPORT.md)。完整指标在 [results/summary.csv](results/summary.csv)，每次重复的数据在各 JSON。

Noah 已确认 50% 以内的性能开销可以接受。本轮多数所测负载的 CPU 中位数符合这个预算；C 的失败密集项在发布重测中约慢 57%，D 同项约慢 46%；请同时审查语义、初始化时机、单遍所有权与内存分配，不能把测得的范围当成任意程序的上界。

需要 Racket 9.3 CS、Python 3 和 GNU timeout。Racket 安装链接：

https://github.com/NoahStoryM/racket-dist/releases/download/v9.3/racket-9.3-x86_64-linux-buster-cs.sh

下载和安装示例（每一步都有上限）：

```sh
timeout 50s curl -fL --connect-timeout 10 --max-time 45 --retry 0 \
  -o /tmp/racket93.sh \
  https://github.com/NoahStoryM/racket-dist/releases/download/v9.3/racket-9.3-x86_64-linux-buster-cs.sh
timeout 60s sh /tmp/racket93.sh --in-place --dest /tmp/racket93
export RACKET=/tmp/racket93/bin/racket
export RACO=/tmp/racket93/bin/raco
```

有界地复现语义和一小组测量：

```sh
bash run.sh answers pauses mixed
```

`run.sh` 先 `raco make`（特别是 include 文件修改后），再运行全部新语义测试，成功后才运行所选 benchmark。每个 benchmark 子进程有 20 秒超时；输入规模固定；七次测量批次，每次之前 GC；三次预热校准；测量阶段不并发。不要同时运行多个 benchmark 组。

其余测量分组运行，每条命令结束后再运行下一条：

```sh
RACKET="$RACKET" timeout 45s python3 run-bench.py left right wide
RACKET="$RACKET" timeout 45s python3 run-bench.py failures fusion interleave
RACKET="$RACKET" timeout 45s python3 run-bench.py level native roundtrip
RACKET="$RACKET" timeout 20s python3 run-bench.py counted
RACKET="$RACKET" timeout 20s python3 run-micro.py
timeout 15s "$RACKET" fusion-scaling.rkt > results/fusion-scaling.json
timeout 5s python3 summarize.py
```

独立重新运行原版选取的五个测试文件：

```sh
timeout 45s "$RACO" test baseline/tests/kernel.rkt baseline/tests/model.rkt \
  baseline/tests/flat.rkt baseline/tests/catenable.rkt baseline/tests/choices.rkt
```

没有运行原仓库依赖外部 amb/miniKanren/ambKanren 安装的全部测试、book 或旧 benchmark。原版的 59 cases 和本实验的 11 cases 是不同计数，后者另记录 28,939 次显式比较。

直接互操作示例：

```racket
#lang racket/base
(require "c.rkt")
(define s
  (amb-interleave
   (λ () (sequence-generate* (in-range 3)))
   (amb-reify (λ () (amb-pause) 'own))))
(for/list ([e (in-amb-events s)]) e)
;; '((0) (1) (2) pause (own))
;; interleave 在暂停才换边；左边没有暂停，所以先产生 0、1、2。
```

公开 API 尚属原型：C/D 为方便研究导出内部定义。`amb-await` 返回两个值；旧 `nary.rkt` 和任何匹配旧 cons 构造子的调用方需要迁移。`amb-reify` 默认惰性，另有 `amb-reify/eager` 用于恢复旧的首段启动时机。

文件用途：

| 文件 | 用途 |
|---|---|
| baseline/ | 原文件的逐字节副本，A 和独立模型 |
| b.rkt | 双生产者参数，保留旧 Stream 构造子与启动时机 |
| c.rkt / d.rkt | 统一两值协议；D 另支持暂停块 |
| values-core.inc | 两者共享的效应、调度融合、反射、sequence 接口 |
| choice-macros.inc | 原版 amb / for/amb 等宏 |
| semantics.rkt / support.rkt | 确定种子的语义对照和有限 fuel 观察 |
| bench.rkt / run-bench.py | 独立负载、超时、检查点、预热和七次重复 |
| micro*.rkt / run-micro.py | pair、values、closure 和 thunk 成本测量 |
| fusion-scaling.rkt | 结构融合与匿名闭包嵌套的规模对照 |
| summarize.py | 从原始 JSON 生成表格与 CSV |
| STAGE1.md / STAGE2.md | 阶段检查点；最终状态以 REPORT 和结果文件为准 |

底层流采用单遍所有权约定，没有缓存，也没有对所有 thunk 添加重复调用检查。不要重新使用已经交给 merge 的残余状态。普通用户应通过 `in-amb`、`in-answers` 等接口消费。
