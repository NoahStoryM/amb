# amb v3：最小参考语义（阶段 A）

本稿先于参考实现提交。基线为原研究 `d7edfd5`、统一协议实验 `62be21d`，以及 [#22](https://github.com/NoahStoryM/amb/issues/22) 中关于单位律、备选供应、for/amb 子句效应与反射捷径的讨论。目标是可执行规格，不做优化或 v2 迁移。

## 1. 载体与公开运算

直接采用原生过程协议，不定义 Stream 类型、结构体或公共转换层：

```racket
;; Generator = () -> (values Result Generator)
;; Result = list? | #f | exact-positive-integer?
;; Merge = Generator Generator -> Generator
```

列表装着一个元素的全部返回值，`'()` 是零值元素，`#f` 才是结束。收到 #f 后不调用 next。正整数 n 是 n 个逻辑暂停；数学上的 P^0 是恒等，不把 0 当事件。生成器及其残余只消费一次，不缓存，不探测过程身份。

`(λ () (sequence-generate* seq))` 可直接输入 make-amb 或 merge。它在首次推进时初始化 seq 并取首元素，以后只调用返回的 next；seq 自身不一定是零参数过程。协议与 [Racket sequence 文档](https://docs.racket-lang.org/reference/sequences.html)一致，暂停是扩展。

| 运算 | 输入/输出与地位 |
|---|---|
| make-amb | Generator → 当前搜索中选择到的一整组值；原始选择效应 |
| amb-fail | 无参数；丢弃当前分支及后续；原始效应 |
| amb-pause | 无参数；交出一次暂停，恢复后返回 void；原始效应 |
| current-amb-merge | 每个实际执行的选择点读取并固定的参数，默认 append |
| amb-append/interleave/alternate/level | 独立的纯生成器组合，不调用搜索效应 |
| in-amb | 建立独立的惰性搜索边界，返回单遍原生 sequence |
| amb、for/amb | 通过 make-amb 派生；不再添加控制原语 |

不公开 reify/reflect、amb-from、事件结构体或 generator 构造器。实现中只保留局部边界和协议所需的小辅助定义。

## 2. merge 的定义方程

用 0 表示终止，A(v,s) 表示一个整组值答案，P(s) 表示一次暂停：

```text
0 ∨ t      = t
A(v,s) ∨ t = A(v, s ∨ t)          ; alternate 改为 A(v, t ∨ s)

append:      P(s) ∨ t = P(s ∨ t)
interleave:  P(s) ∨ t = P(t ∨ s)
alternate:   P(s) ∨ t = P(t ∨ s)

level: P(s) ∨ 0      = P(s)
       P(s) ∨ A(v,t) = A(v, P(s) ∨ t)
       P(s) ∨ P(t)   = P(s ∨ t)
```

level 仅需保留“左侧已暂停，右侧本轮仍在运行”这个局部扫描状态；用一个局部递归函数表达，不用全局队列或优化记录。所有策略消费 n 时都等价于逐个 P；append 可原样转交输入块，interleave/alternate/level 不得一次吃掉 n 个换边机会。参考实现不发现或合并新暂停块。

四个内置 merge 具有双侧零元；只有 append、level 要求/具有结合律，另两者保留括号。自定义 merge 的**语义契约**至少要求按事件观察、单遍、惰性且具有双侧零元，不要求结合。程序无法自动判定这些代数律。

## 3. make-amb：后续先进入每个分支，再由 merge 调度

选择点捕获 k（接受整组返回值、返回两值生成结果）并固定 μ。它的解释 M 是生成过程：

```text
Mμ(g,k)()，先取得 (r,g')：
  r = #f       → 0
  r = 值列表 v → μ(λ().apply(k,v), Mμ(g',k))()
  r = n>0      → P^n(Mμ(g',k))
```

make-amb 丢弃当前控制路径，把 M 的下一结果交给最近的搜索边界。fail 交付终止；pause 交付 P(λ().k(void))。这三个定义可直接用 shift/reset 表达；merge 不知道控制操作。

**值生成器足以派生惰性表达式选择，但必须增加调用这个派生步骤：**

```racket
;; 解释性的展开，真正的宏保持原有词法作用域。
((make-amb
  (λ () (sequence-generate*
         (in-list (list (λ () a) (λ () b)))))))
```

make-amb 选择的是 thunk 这个普通值；外面的函数调用属于捕获的后续 k'，因此选到后才运行 a/b。a/b 内部的选择、失败、暂停都发生在搜索分支内，整段外部后续也属于该分支。不是在供应阶段运行 thunk。

错误展开 `(make-amb (λ () (sequence-generate* (list a b))))` 的最小反例是 a=1、b=(error "early")：首答案本可立即拿到，错误展开却先抛错。只删除最外层调用则会产生两个过程值，也不等于执行备选。

**额外空尾不可隐瞒。** M 会得到 `μ(ka, μ(kb,0))`；用右单位律才化为核心二元方程 `μ(λ().k(a), λ().k(b))`。例如给每次 merge 都额外加 P 的过程缺少右单位律，前者有两个额外 P，后者只有一个。故这里选择明确要求零元律，而不加 lookahead、known-done 身份判断或另一个 binary-choice 原语。若未来要支持任意不守单位律的 merge，需重新决定原语；不能声称现有派生仍满足那条二元方程。

多元 amb 是同一选择点的右折叠，不声称 n 路等份额。零备选直接 amb-fail；单备选仍按 M 解释，内置策略下等价于直接执行。嵌套选择在实际发生时可以另取策略；纯、固定 μ 下才可直接按同一个右梳方程展开。

## 4. 供应与运行的作用域

for/amb 按 Racket 的迭代顺序产生主体 thunk，通过原生 sequence-generate* 供应给 make-amb，再调用所选 thunk。序列表达式、初始化、推进、过滤、break/final 都属于供应；主体及其后续属于分支。迭代可以有普通 I/O 和可变状态，不要求任意意义的纯函数，但不得在供应时执行影响这次搜索的 amb/make-amb、fail、pause。

采用一个私有的动态阶段标记检查所有三个效应入口。每一次 generator pull 都在供应标记内，取到结果后先退出标记，再恢复 k。新的 in-amb 搜索边界遮蔽该标记，所以供应函数内完整消费一个独立搜索合法；间接调用辅助函数越界同样报错。普通源异常原样传播。无需静态检查子句，也不捕获整个枚举器作为可重放搜索。

该规则刻意明确了旧原型未定义/偶然报错的子句行为；不照搬 v2 的重数或“复制余下循环”语义。先在外面选上界，或在主体里选择筛选，是两种明确的替代写法。只取首元素不能触发第二行供应错误。

## 5. sequence 边界与参数时机

创建 in-amb 只建立对象，连 sequence 初始化也不执行搜索正文；Racket 实际请求元素时才推进带最近搜索 prompt 的过程。一个搜索的每次正常返回产生一整组值，后继为终止。原生 make-do-sequence 负责 for 接口；把准备下一元素放在 more?，get 只交付准备好的值，不能在 next-pos 提前执行尾部。序列对象只初始化一次；不是可重放的 stream。

每个 make-amb 实际运行时读取 μ 并保存在该选择点，残余供应不重读。外部 parameterize 仅在创建 in-amb 时生效没有用；消费时的动态环境作用于尚未开始的选择。写进搜索正文的 parameterize 属于捕获的后续。不同选择点可混用策略；不冻结所有参数到创建时，也不把所有选择固定成一个搜索参数。

## 6. 暂停观察的三个选项与本稿选择

| 选项 | 取舍 |
|---|---|
| 仅默认跳过 | API 最少，但难以在公共边界做有界事件观察 |
| 独立事件 sequence | 可明确标记答案/暂停，但增加操作符和事件表示 |
| in-amb 的 #:on-pause | 只增加一个边界选项，handler 的零/多返回值自然成为一个 sequence 元素 |

本参考稿暂选第三项，默认仍跳过。每个逻辑 P 调用 handler 一次，其结果**不**送回搜索后续，不参加 merge，也不是搜索答案。n 按 n 次处理。handler 表达式按普通参数规则在创建时求值，调用在消费时发生；暂停后不能提前恢复搜索。

handler 在观察阶段运行，禁止直接把搜索效应送往外层，独立嵌套 in-amb 仍合法。纯 handler 不改变底层事件轨迹；任意 Racket 回调仍可能通过共享 box/I/O/参数修改影响未来程序，不能承诺带任意副作用的观察完全透明。这里不增加效果系统或 empty-handler 框架。

## 7. 待定事项与验证计划

以上为参考实现采用的明确决策，不等于正式 v3 已定案。仍待作者决定：自定义 merge 是否正式承诺零元律；#:on-pause 是否留在公共 API；单遍重复初始化的错误策略与具体命名。若坚持任意 merge 且要求精确二元构造，最简单替代是私有 binary-choice 控制原语，而不是伪装 M 没有空尾。

阶段 B 用单个短 kernel.rkt，按协议/merge/效应/sequence/派生宏组织；不借用 values-core.inc，不用 callable struct、队列、缓存、压缩、n 路或公开 reify/reflect。阶段 C 以原 model.rkt 的纯 direct 子语言为独立 oracle，按相同有限 fuel 比较；另外测多值、无限前缀、供应隔离、惰性、参数固定与副作用顺序。所有测试进程设超时，不做 benchmark。
