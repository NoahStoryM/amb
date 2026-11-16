# 控制原语的候选实现（研究分支，不合并）

讨论在 PR #24：https://github.com/NoahStoryM/amb/pull/24

`pkgs/amb/private/core.rkt` 增加了一个只供实验的切换，用环境变量 `AMB_IMPL` 选择实现：

- `orig`（默认）：`racket/control` 的 `shift-at`/`reset-at`，即 v3 现状；
- `a`：方案 A，只捕获一次加普通 abort，所有 prompt 用默认 handler（body 中 prompt 被重装，即 shift 语义）；
- `s0`：自定义 shift0，prompt 的 handler 为 `(λ (thk) (thk))`，body 在 prompt 之外运行。

`a` 和 `s0` 中的 `amb-fail` 都不捕获续延。

脚本：

- `shift-vs-shift0.rkt`：比较方案 A 与 shift0 两种写法每次效应的耗时和分配。
- `fail-barrier.rkt`：在异常 handler 中（处于续延屏障之下）调用失败。只捕获加 abort 会报错，普通 abort 能正常失败；脚本同时测两者的开销。

- `differential.rkt`：逐项打印数值个数、dynamic-wind 日志、parameterize、续延标记、break、prompt 可用性、merger 中的效应、异常和屏障等情形的结果。在三种实现下各运行一次，输出保存在 `differential-orig.txt`、`differential-a.txt`、`differential-s0.txt`。

这个切换只用于对照测量，正式实现会去掉它。
