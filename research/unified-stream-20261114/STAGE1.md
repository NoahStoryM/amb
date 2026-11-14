# 阶段一：阅读与设计（2026-11-14）

基线：`research/choice-as-merge-20261112`，`d7edfd5e8b3bcbc5dcd04cf810fff7fed02c609c`。初始工作区为空；检出后干净；未发现 AGENTS.md。原版文件不修改，baseline/ 是 kernel、model、catenable 的逐字节副本。

已读：kernel.rkt、model.rkt、catenable.rkt，README 的机制、三个命题、公开 merge 约束、验证边界与 API 讨论；kernel/model/flat/choices 测试及现有 benchmark。原版 amb-reify 立即运行到首事件；thunk 在旧表示中既是暂停构造子，又作为答案尾部的延迟计算。两者不能在转换中混淆。

官方依据：https://docs.racket-lang.org/reference/sequences.html 的 sequence-generate*；并核对安装的 Racket 9.3 collects/racket/private/for.rkt:1591。初始化会取得首元素；元素是整组值的列表，空列表是零返回值答案，#f 才是终止。终止时的 next 是会报错的 no-more，消费者不能调用。一般 sequence 不保证重放；单遍是本实验的所有权约定，不等于 Racket 强制检查一次调用。

设计：A 原始实现；B 仅将 merge 参数改成两个生产旧 Stream 的 thunk，保持旧的启动时机；C 全惰性入口，每次推进返回两个值，仅暂停 1；D 在 C 上支持正整数暂停块。C/D 的 reset 与续延直接产生两个值，只有持久的尾部状态需要 thunk/可调用记录。以原模型为独立事件 oracle，不修改期望以迎合实现。

时机差异必须显式呈现：C/D 的 amb-reify 创建不执行正文；第一次推进才执行。因此在流外包 parameterize 再移出消费会连首个选择也改变。事件前缀比较在同一参数作用域内执行；另外专门测试时机区别。考虑额外的 eager 入口用于迁移，不将其成本混入普通 C/D。

保留 appended(s,ts)、leveled(ws,s,ts) 与 catenable 列表。C/D 中记录表示下一事件的计算，不再表示一个已经可见的暂停。leveled 的轮末必须显式返回暂停，等待队列只保存下一轮的计算，不能提前恢复。

计数的意义是 P^n。interleave/alternate 每次只消费一个 P 再换边；level 每轮每支消费一个 P。D 只允许把已知的剩余暂停状态合并；不得提前调用尾部来发现可合并的暂停。

成本假设（待实测）：values 不分配持久 pair，但多返回值接收与调用有成本；unit 返回 thunk 时通常以 closure 替换 pair，并不天然更快。直接让 reset 返回两值可以减少临时 unit 闭包，但返回的 tail、调度记录和答案值列表仍会分配。用 cumulative allocation 与 GC 分别测量，不用存活堆大小冒充总分配量。

安全运行：下载 curl max-time 45s，无重试；安装 timeout 60s；每个测试与 benchmark 使用进程超时和有限输入或事件 fuel。暂不运行依赖其它仓库及安装包的 book/master 比较。
