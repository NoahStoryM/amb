# 阶段二：可运行原型与语义验证

原版五个测试文件：59 cases passed。独立实验：`raco make b.rkt c.rkt d.rkt semantics.rkt bench.rkt` 后，`timeout 35s raco test semantics.rkt`，8 cases passed，21,701 次显式比较。完整输出在 results/semantics.txt。include 修改后必须重新 raco make；仅加载旧 .zo 可能运行旧版内容。

测试包括：固定种子 20261114，3,600 个随机 direct 目标，A/B/C/D 对照事件及每个事件交付时的副作用，A 再对照独立的 explicit-CPS spec；1,800 个随机混合策略流表达式；深/宽/失败/无限答案/永远暂停的有界前缀；零及多返回值；反射、局部搜索、once、ifte；原生 sequence 的终止尾、port 单遍、零值元素；不多强制尾；暂停计数 1..8 的全部两两组合、四策略和七档 fuel。

发现并修复的实现缺陷：B 的最后一个备选原先返回生产者本身而没有调用，错误增加暂停；C/D 的 sequence 观察器最初把推进放进 next-pos，导致 for 预先推进位置时读取尾部，现改成 more? 准备、get 交付的单遍接口；D 的 min 初值 +inf.0 把整数暂停变成浮点数，现用精确整数累积。

已验证的时机区别：A/B 在 reify 时执行首段；C/D 第一次推进才执行；流外 parameterize 的首段读取因此可能不同。dynamic-wind 的创建时机不同，但按两事件前缀消费后的进入、退出顺序相同。C/D 附加 amb-reify/eager 用于兼容旧的创建时机。

API 迁移：amb-await 现在返回两个值 (result,next)，result 为列表或 #f；不再返回旧的 ready cons。in-answers 保留多值序列，in-amb-events 产生一个列表答案或 pause 符号；答案即使是 #f/pause 也装在列表里。序列对象防止重复初始化；底层 thunk 仍以单次消费约定使用，不全面加状态检查。

不覆盖原来的 nary.rkt：它依赖旧构造子，需要独立迁移。C/D 的 amb-strategy 原型接收 next->(Stream 或 #f)；不能直接传入旧版 nary 策略。

最终增补：直接 amb-from 避免 reflect 的临时选择向量；补足宏、shuffler、多值合并、任意零/多值反射、初始化次数，以及 1,800 个压缩暂停的嵌套混合表达式。最终 11 cases、28,939 次显式比较通过，见 results/semantics.txt。阶段旧计数保留作历史。

发布整理阶段：修正 benchmark 中 min 与 +inf.0 引起的计数浮点化，重新测量全部负载；这是观察器修正，不是内核变更。发布数据与原因见 results/BENCHMARK-NOTES.md。
