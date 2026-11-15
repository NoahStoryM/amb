# amb v3 正式版规格（供审核）

本稿汇总 #22 中已确认的决定，是 v3 正式版的规格。它替代 `fc894fe` 版的本文件；较早的 DESIGN.md 与它冲突时，以本稿为准。实现在分支 [`v3`](https://github.com/NoahStoryM/amb/tree/v3)。

依据（详细推导与讨论见各条 comment，本稿不复述）：

- [demo 与本稿初版](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6877709908)
- [Claude 的审阅](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6887205049)
- [Astra：表示等价、合同性、完备性](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6887344679)
- [Fable：融合方案、局部搜索入口](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6887431533)
- [融合版与原语](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6888009968)
- [Astra：融合版逐分支核对](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6888096958)
- [`(amb)` 与 base 导出](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6888132486)
- [Noah 的决定](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6888202723)
- [Fable：正式版第一版与 v2 冲突表](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6888785826)，[handler 改为位置参数](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6888999501)
- [Astra：本稿与 `v3` 代码的审核](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6889515294)
- [Fable：按审核修改](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6890628758)，[`#:length` 的截断](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6890742140)

`demo.rkt`（方程版 merge）和 [`research/fused-merge-20261115`](https://github.com/NoahStoryM/amb/tree/research/fused-merge-20261115)（融合版）是研究阶段的参考，都还带着 0 分支；`v3` 中已去掉，见 §2。

## 1. 模块与导出

| 模块 | 导出 | 职责 |
|---|---|---|
| `amb/base` | `make-amb`、`amb-generate*`、`amb-fail`、`amb-pause`、`current-amb-merger` | 选择、失败、暂停三种效应，策略参数，以及搜索边界 |
| `amb/merge` | `amb-append`、`amb-interleave`、`amb-alternate`、`amb-level` | 标准合并函数；不依赖 amb 的控制效应 |
| `amb/main` | 重导出 `amb/base`；`amb`、`for/amb`、`for*/amb`、`sequence->amb`、`current-amb-shuffler`、`in-amb`、`in-amb/do`、`in-amb*`、`in-amb*/do` | 构造备选的语法和观察入口 |

- `require amb` 对应 `amb/main`。ambKanren 等使用方只依赖 `amb/base` 即可：`run` 用 `amb-generate*` 取答案，`once`、`ifte` 用 `amb-generate*`、`amb-pause`、`amb-fail` 写出。
- main 重导出的参数、prompt 和核心绑定，必须与 base 是同一组，不能各建一份。
- 依赖方向：base 从 merge 取得默认的 `amb-append`；merge 不依赖 base；main 依赖两者。
- `amb-pause` 是原语，不由 `make-amb` 派生：派生写法 `1、(list (void))、#f` 会多出一次 `μ(k(void), empty)`，在缺右单位律的 merger 下行为不同。`amb-fail` 可以精确地派生为"一开始就结束的供应"，但与 `amb-pause` 成对放在 base。
- v2 的 `current-amb-depth-first?`、`current-amb-fair?` 由 `current-amb-merger` 取代，不提供逐项对应的替代。与 v2 的全部行为差别见 `v3` 的 [`tests/v2-conflicts.rkt`](https://github.com/NoahStoryM/amb/blob/v3/pkgs/amb/tests/v2-conflicts.rkt)，文档要逐条写明，尤其是 §6 的那一条。
- 参数检查在入口用 guard，抛 `exn:fail:contract`；不给 merger 套 contract，以免每次合并多一层 chaperone。`in-amb*`、`in-amb*/do` 沿用 v2 的模块边界 contract。
- 在搜索之外调用效应，报 `not in a search`。

## 2. 生成器协议

```racket
;; Generator = () -> (values Result Generator)
;; Result    = list? | #f | exact-positive-integer?
;; Merger    = Generator Generator -> Generator
```

- 列表：一个答案，即一个元素的整组返回值。空列表是零返回值的答案，不表示结束。
- `#f`：结束。之后不得再调用随它返回的 next。
- 正整数 n：n 次逻辑暂停。
- **协议中没有 0。** 返回 0 是值域错误，按 `raise-argument-error` 报错。值域由**读取结果的一方**检查：merger 只检查它自己调用的生成器；一个生成器"独自继续"时（`end ∨ t = t`、融合版的收尾规则），它交出的结果由下一个读者（`make-amb` 或观察者）检查。所以给非法生成器套一层 merge，不等于已经验证过它。
- 单遍所有权：每个生成器及其残余最多被消费一次。融合版展开一条记录，也算一次消费。
- 自然数以外的扩展只有正整数暂停；`#f` 和列表与 `sequence-generate*` 一致，所以 `(λ () (sequence-generate* seq))` 可以直接当作供应者。

## 3. 两种相等

**事件相等 `≈`。** 一次调用解释为：列表是答案 A(vs)，n 是 n 个暂停 P，`#f` 是结束 ✓。一个生成器的逻辑轨迹有三种：

- 有限的正常轨迹 w✓；
- 有限可见前缀之后发散 w⊥，即某次请求的宿主计算不终止；
- 无限的可见轨迹。

两个生成器事件相等，指它们的逻辑轨迹相同；批量的 n 和 n 个 1 被视为相同。比较时必须连同 ✓ 和 ⊥ 一起比，不能只比答案和暂停序列。策略的规律和文档里的等式，都在 `≈` 的意义下陈述。

**源调用轨迹相同。** 事件相等，并且在每个检查点，已经调用过的源生成器及其次序相同。检查点包括每个逻辑事件、结束 ✓，以及运行中途的每次源调用：最后一个可见事件之后、发散之前的调用也要算（反例：G 调用一次源后无限循环，H 直接无限循环，二者事件轨迹都是 ⊥，但副作用不同）。它比 `≈` 更细，是库内优化（融合）必须保持的。

- 比较的两方使用同一组、同样分块的带标号源，只忽略 merger 内部表示层的调用；不能一方把批量 n 改写成 n 次源调用，另一方却要求调用次数相同。
- 它不涉及墙钟时间。
- 异常不在三类轨迹之内；若纳入保证，另记异常观察。

两者都不要求原始返回值相同。例如方程版和融合版对同一输入交出的批量写法可以不同。所以对照测试不能用 `equal?` 比较原始结果。

**适用范围。** 这两种相等是纯事件语义、以及正常事件前缀上的惰性差分标准，不是完整的程序等价判据。测试上另加调用前缀比较、异常注入、单遍检测；有限测试判定不了一般的 ⊥，超时也不能充当发散的证明。

## 4. merger 的协议要求与可选规律

**协议要求**（任何 merger 都必须满足）：

1. 值域：只返回 §2 的 Result。
2. 单遍所有权：每个操作数及其残余最多消费一次；`#f` 之后不调用 next。
3. 合同性：`s ≈ s′ 且 t ≈ t′ ⇒ μ(s,t) ≈ μ(s′,t′)`。

合同性恰好保证 merger 在等价类上有定义。库会改写表示（例如 interleave 用 `pauses` 拆开 n，融合版交出与方程版不同的批量），所以不满足合同性的 merger，结果会随内部写法改变。反例是按原始批次换边的 `chunk-interleave`，见 Astra 的 comment。合同性只约束逻辑结果；merger 若在读取原始批次时另外执行可见副作用，它就能区分融合前后的原始返回，这不在合同性的保证之内。

**可选代数律及活性保证**（标准 merger 各自声明，自定义 merger 不强制）：

- 左单位律 `empty ∨ t ≈ t`，右单位律 `s ∨ empty ≈ s`；
- 结合律：append 和 level 满足，interleave 和 alternate 不满足；
- 公平性（活性），见 §6。

协议要求保证 merger 在等价类上有定义；可选代数律是等价类上的额外等式。依赖这些规律的化简或优化，只能用在已知满足它们的策略上，不能作用于任意 merger。代数律只给出 `≈` 意义下的化简，**不自动保持源调用轨迹**：例如先调用右侧 empty、再原样运行左侧的 merger，满足右单位律，却多调用了一次源。库内优化若承诺 §3 的较强标准，还要另证调用轨迹不变。

**merger 中的效应。** merger 里调用 `amb`、`amb-fail`、`amb-pause` 不在保证之内，也不属于 merger 的数学模型：同一行代码的效应归属取决于它是第几次合并（第一次在选择点、被观察搜索的 prompt 内调用，之后的在观察者那边调用）。文档直接写明：不要这样写。

**原始右折叠。** 对供应 a、b 后结束的选择，固定捕获的 k 和该选择点的 μ，`make-amb` 的结果是 `μ(K_a, μ(K_b, empty))`，其中内层 μ 在右支被请求时才构造和调用。供应中的暂停留在原位：供应 `a, 2, b, #f` 对应 `μ(K_a, P²(μ(K_b, empty)))`。

- 化简为 `μ(K_a, K_b)` 需要右单位律加合同性；单备选等于直接执行，同样依赖右单位律。
- 没有右单位律时，保留原始右折叠，不偷偷删除末尾的空选择，也不按过程身份做 singleton 捷径。
- 例：`tick-append`（每次合并先交出一个暂停）下，`(amb 'a 'b)` 观察到 `P a P b`，而单次 `tick-append(K_a, K_b)` 只给 `P a b`。
- `(amb)` 总是失败；但 merger 缺空搜索单位律时，`(amb (amb) (amb))` 不一定逐事件等于失败。文档需要写明这一点。

文档给出一个正确处理批量 n 的自定义 merger 样板。

## 5. 标准 merger 与融合

方程版（`demo.rkt`，去掉 0 分支）是规格。正式版的 `amb-append` 和 `amb-level` 用融合实现，结构见 [Fable 的方案](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6887431533) 和 [`merge.rkt`](https://github.com/NoahStoryM/amb/blob/7b16156/research/fused-merge-20261115/merge.rkt)：

- 记录 `appended(s, ts)`、`leveled(ws, s, ts)` 带 `prop:procedure`，本身就是合法的生成器；待办列表用可拼接表。
- 每个融合 merger 只展开自己那一种记录。其他生成器，包括另一种标准 merger 的记录，都只通过公共调用接口处理。
- append 不拆批量 n；level 每轮只消费一个暂停，把 `pauses(n−1, s₁)` 登记为等待者。

融合版与方程版之间要保持的是 §3 的两种相等，而且要求在每个逻辑事件处都成立。Astra 已逐分支核对融合版，Fable 的随机差分、异常注入和改错测试都通过。代价按 merge 数和请求数计算，不能从融合推出"每个答案常数时间"。没有嵌套可压平的程序上，融合版慢一到两成。

"把全是纯暂停的空轮合并掉"不在 v3.0 范围内。

## 6. 完备性的前提

> 对规范化后的、有限分支的搜索展开，假设：展开每一步的宿主计算（含供应）终止；每条无限路径都有无限多个显式暂停；参与该完备性保证的所有选择，都使用满足相应公平条件的 merger；局部暂停不被吞掉。则每个未被剪枝、位于有限深度的答案，都在有限个输出事件之后被枚举出来。

- "有限分支"指展开后的树。无穷供应表示为惰性右链时，供应的尾链也算作路径，不能只检查每个主体。无穷供应的备选全都失败、又没有暂停，就违反这个前提，即使每次供应调用都终止。
- 对三种公平 merger（interleave、alternate、level），这组前提是充分条件，不宣称对每一种都必要。混入 append 时，不能自动沿用整棵树的公平结论。
- `once` 等剪枝之后，完备性只针对保留下来的叶。
- 这是按有限前缀、有限调度轮的论证，不是机器证明。

去掉 0 只去掉了一种协议表示，没有消除宿主发散，也没有消除无暂停的无限选择展开。

**这是相对 v2 用户看得见的行为变化**（冲突表第 10 条）：v2 靠全局前沿的公平性，在不改程序的情况下对无穷域完备，例如三层自然数上的选择能找到勾股数；v3 的公平性来自程序里的暂停，同一个程序在四种 merger 下都不终止，因为第一个答案之前既没有答案也没有暂停交给调度器（alternate 在答案处也换边，但这里还没有答案）。在每个备选里加 `(amb-pause)` 后，interleave、alternate 能找到解。文档要写明这一点，并给出改写方法。

无穷多个备选的公平调度（dovetailing）不由四个二元 merger 自动提供。v3.0 不提供 n 路策略：在当前实现中只用得到二路合并。

## 7. 选择与派生语法

- `make-amb next`：在执行时捕获后续 k，读取并固定当前的 `current-amb-merger`。对供应者交出的每组值 v，分支是 `(λ () (apply k v))`，各分支按原始右折叠交给 merger。供应者的暂停作为选择点的暂停原样交出；供应结束则交出 `#f`，不经过 merger。
- `sequence->amb seq` ≡ `(make-amb (λ () (sequence-generate* seq)))`。
- `amb`：把备选包成 thunk 放进向量，调用一次 shuffler，再交给 `sequence->amb`，并在后续中调用选中的 thunk。不提前执行任何备选。
  - `(amb)` 也走这条路径，语义与 `(amb-fail)` 相同：供应一开始就结束，与 merger 是否有单位律无关。
  - 每个向量选择恰好调用一次 shuffler，包括空向量和单元素向量。shuffler 必须接受空向量，这是相对 v2 的行为变化。shuffler 算作供应阶段（§9）。
- `for/amb`、`for*/amb`：通过原生 sequence 供应主体 thunk。
  - 有 `#:length` 时用向量，与 `amb` 一致。没填满的槽按迭代次数截掉，不是备选，也不交给 shuffler；不靠比较哨兵值识别它们。理由：把它们当作失败的备选，`#:length` 给大了每次选择都要多合并几次，而且在没有单位律的 merger 下可见。
  - 保留 v2 的 `#:fill e`：填充的槽是备选。
  - 无长度时，第一版用 `for/stream`，因为最简单。正式版倾向改用 v2 的协程供应：stream 可能带来额外开销，有些边界情况也不好处理。两者在已定语义下比较后再定。
  - `#:break`、`#:final` 要递归到同一个宏，`for*/amb` 递归到 `for*/amb`。

## 8. 局部搜索：`amb-generate*`

```racket
;; (amb-generate* thk) -> (values Result Generator)
```

- 与 `sequence-generate*` 同形：调用时把 `thk` 作为一个独立的搜索，运行到第一个事件为止。它是**急切**的；创建时不执行正文的是 `in-amb/do`，文档要写清这一点。
- `in-amb*/do` 内部的生成器就是 `(λ () (amb-generate* thk))`。
- `(amb-local e)` ≡ `(make-amb (λ () (amb-generate* (λ () e))))`。reflect 就是 `make-amb`，因此旧的"按 done 的身份识别单答案流"捷径不再存在。
- 限定：后续为终结后续时，`make-amb ∘ amb-generate*`（上面带 thunk 的写法，不是普通的函数复合）在四种标准 merger 下恢复原来的逻辑事件；对任意 merger 不普遍成立，例如 `tick-append` 会在每个反射的答案处再添一个暂停。这项重建只针对逻辑事件，不涉及宿主副作用或原始分块的调用次数。
- 对任意后续，不保证局部搜索与直接风格事件相等，需要另验所用策略与后续的分配性质。这不是说两者必然不同：例如只用 append 时，两者对所有后续都相等。
- `amb-local`、`once`、`ifte` 不放进 base，由使用方或 main 之外的模块写出；是否在 main 中提供，审核时再定。

## 9. 供应阶段的效应边界

- 在供应备选期间（子句、枚举器、shuffler），试图影响**当前**搜索的选择、失败、暂停，明确报错；主体中的效应照常工作。实现：供应代码在一个 continuation mark 下运行，三个效应入口查这个 mark，只查到最近的搜索边界为止。
- 完整的独立嵌套搜索允许。**搜索边界保护的是该搜索的主体，以及它捕获的续延**：`amb-generate*` 的初始 reset，和恢复续延时自带的 prompt。不按函数身份特判"这个 next 是 reify 出来的"。
- 这个保证**不**延伸到运行 next 的全部代码。`make-amb` 返回的余生成器、自定义 merger 返回的生成器，本身没有 reset；供应者和自定义 merger 的宿主代码，按调用者的动态环境执行。若要让任意 next 中的效应都属于内层搜索，需要另设计覆盖整个恢复过程的边界，v3.0 不提供。
- 有 `#:length` 的形式在选择点一次性收集全部备选，所以即使只取第一个答案，后面某一行子句有效应也会报错；无长度的形式到那一行才报。这符合两者的供应时机，不要求一致。
- `#:on-pause` 的 handler 在被观察的搜索之外、观察者的环境中运行，不额外检查；观察者本身处在供应阶段时，那里的规则照常适用。
- 不能用迭代器偶然报出的 prompt 错误来定义这部分语义。

## 10. 观察入口

沿用 v2 的分工：

- `in-amb` 返回带缓存的 stream：重复读取已经缓存的元素，不会重新运行搜索。
- `in-amb/do` 返回单遍 sequence：创建时不执行正文；再次迭代时与前一次共享游标；耗尽后保持为空。
- `in-amb*`、`in-amb*/do` 是对应的 thunk 版本，handler 是可选的位置参数，与 v2 的 empty handler 一样：`(in-amb*/do thk handler)`。语法形式 `in-amb`、`in-amb/do` 写 `#:on-pause`，展开为位置参数。
- `#:on-pause`：默认跳过暂停。指定 handler 时，每个逻辑暂停调用一次（批量 n 调用 n 次），其零个或多个返回值成为一个观察元素，不送回搜索。缓存版缓存最终观察到的元素，包括 handler 的结果，重复读取不会再次调用 handler。

**异常之后的状态。** 两种转移不同：

- handler 抛异常：那次暂停已经结束，下一次读取从它之后继续。
- 搜索主体抛异常，或控制用续延跳出：游标失效，之后再读取报 `in-amb: the search cannot go on`，不重跑已经调用过的生成器（§2 的单遍所有权）。不选"重抛记录下来的异常"，因为同一个机制也覆盖了续延跳出的情况，而且不需要给每次读取套 `with-handlers`。`in-amb` 的 stream 由 Racket 自己报 `reentrant or broken delay`。

## 11. 实施状态

已完成（分支 `v3`）：§1 的三个模块；融合版 merge，去掉 0；`amb-generate*`；四个观察入口和 `#:on-pause`；无长度 `for/amb` 用 `for/stream`；§9 的供应检查；v2 测试全部移植并整理出冲突表；§3 两种相等的差分测试。`raco setup --no-docs --pkgs amb` 无错误，`raco test pkgs/amb/tests` 129 个测试通过。

待做：

1. `typed-amb`：它依赖的 `amb/private/amb` 已不存在，还用到 v2 的两个参数和 `#:empty-handler`。
2. scribble 文档和 `README.org` 改为 v3，包括 v2 冲突表、§4 的自定义 merger 样板和"不要在 merger 里用效应"、§6 的行为变化。
3. 在新协议上补 kanren 层，跑书中例子（TRS1 228、TRS2 285）。
4. 测试：运行中途的调用前缀监测、merger 内部抛异常的注入。
5. 之后：比较协程供应；性能优化，包括 v2 的槽位释放、纯暂停空轮的合并。
6. 旧 PR #10、#13–#21、#23 与 issue #11：v3 正式版敲定后关闭，分支保留。
