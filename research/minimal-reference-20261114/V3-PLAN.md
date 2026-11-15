# amb v3 正式版规格（供审核）

本稿汇总 #22 中已确认的决定，是正式实现前的规格。它替代 `fc894fe` 版的本文件；较早的 DESIGN.md 与它冲突时，以本稿为准。

依据（详细推导与讨论见各条 comment，本稿不复述）：

- [demo 与本稿初版](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6877709908)
- [Claude 的审阅](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6887205049)
- [Astra：表示等价、合同性、完备性](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6887344679)
- [Fable：融合方案、局部搜索入口](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6887431533)
- [融合版与原语](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6888009968)
- [Astra：融合版逐分支核对](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6888096958)
- [`(amb)` 与 base 导出](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6888132486)
- [Noah 的决定](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6888202723)

当前可执行的参考是 `demo.rkt`（方程版 merge）和 [`research/fused-merge-20261115`](https://github.com/NoahStoryM/amb/tree/research/fused-merge-20261115)（融合版）。两者都还带着 0 分支，正式实现时去掉，见 §2。

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
- v2 的 `current-amb-depth-first?`、`current-amb-fair?` 由 `current-amb-merger` 取代，不提供逐项对应的替代。

## 2. 生成器协议

```racket
;; Generator = () -> (values Result Generator)
;; Result    = list? | #f | exact-positive-integer?
;; Merger    = Generator Generator -> Generator
```

- 列表：一个答案，即一个元素的整组返回值。空列表是零返回值的答案，不表示结束。
- `#f`：结束。之后不得再调用随它返回的 next。
- 正整数 n：n 次逻辑暂停。
- **协议中没有 0。** 返回 0 是值域错误，按 `raise-argument-error` 报错。
- 单遍所有权：每个生成器及其残余最多被消费一次。融合版展开一条记录，也算一次消费。
- 自然数以外的扩展只有正整数暂停；`#f` 和列表与 `sequence-generate*` 一致，所以 `(λ () (sequence-generate* seq))` 可以直接当作供应者。

## 3. 两种相等

**事件相等 `≈`。** 一次调用解释为：列表是答案 A(vs)，n 是 n 个暂停 P，`#f` 是结束 ✓。一个生成器的逻辑轨迹有三种：

- 有限的正常轨迹 w✓；
- 有限可见前缀之后发散 w⊥，即某次请求的宿主计算不终止；
- 无限的可见轨迹。

两个生成器事件相等，指它们的逻辑轨迹相同；批量的 n 和 n 个 1 被视为相同。比较时必须连同 ✓ 和 ⊥ 一起比，不能只比答案和暂停序列。策略的规律和文档里的等式，都在 `≈` 的意义下陈述。

**源调用轨迹相同。** 在每个逻辑事件之前，已经调用过的源生成器及其次序相同。它比 `≈` 更细，是库内优化（融合）必须保持的。它不涉及墙钟时间，也不要求两种原始表示有相同的调用次数。

两者都不要求原始返回值相同。例如方程版和融合版对同一输入交出的批量写法可以不同。所以对照测试不能用 `equal?` 比较原始结果。

## 4. merger 的协议要求与可选规律

**协议要求**（任何 merger 都必须满足）：

1. 值域：只返回 §2 的 Result。
2. 单遍所有权：每个操作数及其残余最多消费一次；`#f` 之后不调用 next。
3. 合同性：`s ≈ s′ 且 t ≈ t′ ⇒ μ(s,t) ≈ μ(s′,t′)`。

合同性恰好保证 merger 在等价类上有定义。库会改写表示（例如 interleave 用 `pauses` 拆开 n，融合版交出与方程版不同的批量），所以不满足合同性的 merger，结果会随内部写法改变。反例是按原始批次换边的 `chunk-interleave`，见 Astra 的 comment。合同性只约束逻辑结果；merger 若在读取原始批次时另外执行可见副作用，它就能区分融合前后的原始返回，这不在合同性的保证之内。

**可选规律**（标准 merger 各自声明，自定义 merger 不强制）：

- 左单位律 `empty ∨ t ≈ t`，右单位律 `s ∨ empty ≈ s`；
- 结合律：append 和 level 满足，interleave 和 alternate 不满足；
- 公平性，见 §6。

依赖这些规律的化简或优化，只能用在已知满足它们的策略上，不能作用于任意 merger。

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

公平的 merger（interleave、alternate、level）对有限分支的搜索树完备，前提是：

- 每次请求下一个逻辑事件，所需的宿主计算有限，备选的供应也一样；
- 规范化搜索树的每条无限路径都包含无限多个暂停 P，这一条对计算及其后续一起要求；
- 局部搜索的暂停按本模型转发；`once` 等剪枝之后，完备性只针对保留下来的叶。

操作性地说：每个被服务的支路，都在有限时间和有限个答案之后到达暂停或结束。去掉 0 之后，不再需要"连续 0 步数有限"这一条。

无穷多个备选的公平调度（dovetailing）不由四个二元 merger 自动提供。v3.0 不提供 n 路策略：在当前实现中只用得到二路合并。

## 7. 选择与派生语法

- `make-amb next`：在执行时捕获后续 k，读取并固定当前的 `current-amb-merger`。对供应者交出的每组值 v，分支是 `(λ () (apply k v))`，各分支按原始右折叠交给 merger。供应者的暂停作为选择点的暂停原样交出；供应结束则交出 `#f`，不经过 merger。
- `sequence->amb seq` ≡ `(make-amb (λ () (sequence-generate* seq)))`。
- `amb`：把备选包成 thunk 放进向量，调用一次 shuffler，再交给 `sequence->amb`，并在后续中调用选中的 thunk。不提前执行任何备选。
  - `(amb)` 也走这条路径，语义与 `(amb-fail)` 相同：供应一开始就结束，与 merger 是否有单位律无关。
  - 每个向量选择恰好调用一次 shuffler，包括空向量和单元素向量。shuffler 必须接受空向量，这是相对 v2 的行为变化。
- `for/amb`、`for*/amb`：通过原生 sequence 供应主体 thunk。
  - 有 `#:length` 时用向量，与 `amb` 一致。
  - 无长度时，第一版用 `for/stream`，因为最简单。正式版倾向改用 v2 的协程供应：stream 可能带来额外开销，有些边界情况也不好处理。两者在已定语义下比较后再定。
  - `#:break`、`#:final` 要递归到同一个宏，`for*/amb` 递归到 `for*/amb`。

## 8. 局部搜索：`amb-generate*`

```racket
;; (amb-generate* thk) -> (values Result Generator)
```

- 与 `sequence-generate*` 同形：调用时把 `thk` 作为一个独立的搜索，运行到第一个事件为止。它是**急切**的；创建时不执行正文的是 `in-amb/do`，文档要写清这一点。
- `in-amb*/do` 内部的生成器就是 `(λ () (amb-generate* thk))`。
- `(amb-local e)` ≡ `(make-amb (λ () (amb-generate* (λ () e))))`。reflect 就是 `make-amb`，因此旧的"按 done 的身份识别单答案流"捷径不再存在。
- 限定：后续为终结后续时，`make-amb ∘ amb-generate*` 在四种标准 merger 下恢复原来的逻辑事件；对任意 merger 不普遍成立，例如 `tick-append` 会在每个反射的答案处再添一个暂停。后续任意时，局部搜索与直接风格本来就不同。
- `amb-local`、`once`、`ifte` 不放进 base，由使用方或 main 之外的模块写出；是否在 main 中提供，审核时再定。

## 9. 供应阶段的效应边界

- 在供应备选期间（子句、枚举器），试图影响**当前**搜索的选择、失败、暂停，应当明确报错；主体中的效应照常工作。
- 完整的独立嵌套搜索允许。准确的规则是：**恢复一个子搜索时，由它自己建立搜索边界**，包括之后每一次调用 next。由 `amb-generate*` 及其恢复的续延维护这个边界，不按函数身份特判"这个 next 是 reify 出来的"。
- 观察回调（`#:on-pause` 的 handler）影响外围搜索时的边界，在正式的异常设计中一并确定。
- 不能用迭代器偶然报出的 prompt 错误来定义这部分语义。demo 目前的 `no corresponding prompt` 只是现状。

## 10. 观察入口

沿用 v2 的分工：

- `in-amb` 返回带缓存的 stream：重复读取已经缓存的元素，不会重新运行搜索。
- `in-amb/do` 返回单遍 sequence：创建时不执行正文；再次迭代时与前一次共享游标；耗尽后保持为空。
- `in-amb*`、`in-amb*/do` 是对应的 thunk 版本。
- `#:on-pause`：默认跳过暂停。指定 handler 时，每个逻辑暂停调用一次（批量 n 调用 n 次），其零个或多个返回值成为一个观察元素，不送回搜索。缓存版缓存最终观察到的元素，包括 handler 的结果，重复读取不会再次调用 handler。

## 11. 实施顺序

1. 本稿交 Astra 核对。
2. 在新分支上实现正式版，不直接改主分支：
   - 拆出 §1 的三个模块，接入融合版 merge，去掉 0；
   - `amb-generate*`、供应阶段检查、带缓存的 `in-amb`；
   - 移植 v2 的测试（包括 `tests/for-amb.rkt`），加入 §3 两种相等的差分测试；
   - 在新协议上补 kanren 层，跑书中例子（TRS1 228、TRS2 285）。
3. 之后再比较协程供应、做性能优化。
4. 旧 PR #10、#13–#21 与 issue #11：v3 正式版敲定后关闭，分支保留。

审核重点：§3 的两种相等是否足以作为测试标准；§4 协议要求与可选规律的划分；§8 的限定；§9 的边界能否不误伤主体和嵌套搜索；§1 的导出与依赖方向是否一致。
