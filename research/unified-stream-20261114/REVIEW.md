# 统一 Stream 协议：审阅入口

本目录供 issue [#22](https://github.com/NoahStoryM/amb/issues/22) 的后续独立审阅。分支 `research/unified-stream-20261114`，基于 `research/choice-as-merge-20261112` 的 `d7edfd5e8b3bcbc5dcd04cf810fff7fed02c609c`。只新增本目录，没有替换原 kernel 或修改 pkgs/。

Noah 已确认：**为取得原生协议兼容性和清楚的 API，50% 以内的性能开销可以接受。** 发布重测中，C 的失败密集项约慢 57%，超出该参考范围；D 同项约慢 46%。多数其它项在范围内；这不是任意程序、平台或分配量的保证。请把审阅重点放在语义、协议一致性、实现复杂度和测量可信度，不以“必须比旧版更快”为前提。

## 建议阅读顺序

1. [REPORT.md](REPORT.md) §1–4：A/B/C/D、官方协议、惰性创建与暂停计数。
2. [values-core.inc](values-core.inc)：共享内核；[c.rkt](c.rkt) 与 [d.rkt](d.rkt)：单暂停与计数差异。
3. [semantics.rkt](semantics.rkt) 与 [support.rkt](support.rkt)：测试定义及独立 baseline/model oracle 的用法。
4. REPORT §5、[fusion-scaling.rkt](fusion-scaling.rkt)：appended / leveled 的状态解释和融合。
5. [bench.rkt](bench.rkt)、[run-bench.py](run-bench.py)、[results/summary.csv](results/summary.csv)：测量方法与原始结果；最后看 REPORT §9–10 的 API 建议。

完整复现命令与 Racket 下载链接在 [README.md](README.md)。先编译并运行语义测试，再分组 benchmark；每条命令与子进程均有超时。不要并发跑 benchmark，也不要无界消费无限源。

## 请重点回答的问题

1. **协议是否真正一致？** `#f` 终止后不再调用 next；空列表是零返回值答案；原生 next 不被逐项改包。检查初始化、port 单遍、多值、序列后继和 sequence 输出是否有漏测的边界。
2. **内部直接两值是否正确？** reset 和 continuation 直接交付 `(values result next)`；区分“推进计算的 thunk”与“已经观察到的暂停”，检查 choice、reflect、await、local、once 是否意外多出或吞掉暂停。
3. **惰性边界如何公开？** C/D 创建 amb-reify 时不运行正文，A/B 立即运行到首事件；外部 parameterize、异常时机和 dynamic-wind 因此需要单独审阅。`amb-reify/eager` 是否足够作为迁移入口？默认名称应保留旧时机还是明确改为惰性？目前的事件比较没有证明所有 Racket 控制上下文等价。
4. **融合是否保持源调用时机？** appended 的剩余待办顺序、leveled 的已暂停/当前/未运行三段、局部状态已消费后再合并，以及 catenable 残余的单遍所有权。不要只核对最终答案集合。
5. **D 的暂停块是否安全？** interleave/alternate 每次只用掉一个暂停再换边；level 只对已知纯 paused 记录合并空轮，不调用未知尾部。请找涉及嵌套混合策略、有限 fuel、副作用或反射的最小反例；目前无通用机器证明。
6. **测量是否公允、足以支持结论？** A 是这个研究内核，不是 v2 master。B 是 ready merge 的薄适配，不是最优双 thunk 的下界。native 比的是旧节点转换路径与直接原生路径。counted 的巨大加速来自聚合计数，不能解释为逐次调度同样加速。微测不能证明任意 closure 大小或任意 values 路径无分配；累计分配应独立于 CPU 预算评估；发布前修正过观察器的整数计数问题，详见 results/BENCHMARK-NOTES.md。
7. **公开 API 是否自然？** 是否接受 sequence-generate* 风格 thunk；是否公开 raw 单遍协议；答案序列与完整事件序列如何命名；`amb-await` 改成返回两值是否适合。普通使用者不应手动处理暂停计数。

## 本轮范围外的已知问题

- #22 最新讨论的 `for/amb` 子句生产阶段效应隔离没有在这里解决；choice-macros.inc 仍来自基线。测试覆盖正常宏行为与主体选择，不代表子句中的 amb/pause/fail 已被正确隔离。
- 旧 nary.rkt 没有迁移到 C/D；自定义 amb-strategy 的完整行为没有纳入新测试。reflect 对已知 done 的 singleton 捷径也保留了，不能宣称任意策略下所有事件等价的表示都可互换。
- 尚未运行 TRS 全例集、真实 ambKanren 负载、Typed Racket/contract 或正式包升级检查。
- 性能结果来自单台共享环境；没有统计置信区间、跨平台证明或不超过 50% 的普遍上界。

请区分：实际复现的缺陷、按源码得到的推导、需要追加实验的假设。发现差异时保留最小程序、策略、fuel、实际/期望事件与副作用顺序；不要通过改旧期望来消除差异。
