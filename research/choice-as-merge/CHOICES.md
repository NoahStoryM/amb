# 选择的形式：向量、生成器、shuffler 与 n 路策略

接口提案和原型，回应 [#22 的「备选的表示」](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6843669848)，并按 [Astra 的三点回答](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6843723160)定了几处。原型在 [`kernel.rkt`](kernel.rkt) 的 "Choices" 一节和 [`nary.rkt`](nary.rkt)，测试在 [`tests/choices.rkt`](tests/choices.rkt)。

结论先说：v2 的分层可以原样搬过来（生成器是原语，向量附带 shuffle，`for/amb` 按有无 `#:length` 分流）；n 路策略不需要改动二路 merge 的任何东西，是多出来的一种策略值；「不饿死外层兄弟」必须靠一个外层看得见的暂停，它改变事件轨迹，所以应该是一种单独的、点名使用的策略，而不是默认行为。

## 1. 策略的类型

```
Stream   = '() | (cons vs (→ Stream)) | (→ Stream)
Merge    = Stream × (→ Stream) → Stream             二路：amb-append、amb-interleave、amb-alternate、amb-level
Next     = → (U Stream #f)                          下一个备选的流；没有了就是 #f
Strategy = (amb-strategy (Next → Stream))           n 路：看得到全部备选
```

`current-amb-merge` 里放 `(U Merge Strategy)`：普通过程是二路 merge，`amb-strategy` 包起来的是 n 路策略。两者用结构体区分，不靠元数猜。

- `(next)` 被调用的那一刻，下一个备选才开始运行（即 `(k alt)` 求值到它的第一个事件）。所以策略什么时候启动哪个备选、要不要去问生成器，都由它自己调用 `next` 的时机决定；内核不替它预读。
- `next` 是单遍的，和流一样。

**二路怎样提升。** 不需要一个显式的提升函数：选择点拿到 Merge 时自己右折叠，拿到 Strategy 时把 `next` 交给它。按 Astra 第 1 问，多路的 `amb` 由递推律定义，

```
S([])      = 0
S([a])     = a
S(a :: ys) = a ∨ S(ys)        ys 非空
```

对长度归纳唯一得到右梳，对任何二路运算都成立，不要求结合，也不用单位律。分块律 `S(xs ++ ys) = S(xs) ∨ S(ys)` **不**用来定义任何东西（它只对结合的 merge 有解）；n 路轮转是另一种策略，单独提供（第 4 节）。

名字是否改成 `current-amb-strategy`，留给你定；原型里没改，免得牵动别的文件。

## 2. 各个形式怎样展开

| 形式 | 展开 | 备选的来源 |
|---|---|---|
| `(amb)` | `(amb-fail)` | |
| `(amb e ...)` | `(amb* (current-amb-merge) (vector (λ () e) ...))` | 向量 |
| `(amb* m vec)` | 先 `((current-amb-shuffler) vec)`，再 `((shift-at tag k (merge-vector m k vec)))` | 向量（可变，用掉） |
| `(amb-from seq [m])` | `(shift-at tag k …)`，备选是 `(k v)`，`v` 取自 `sequence-generate` | 生成器 |
| `(for/amb #:length n (clause ...) body ...)` | `(amb* (current-amb-merge) (trim (for/vector #:length n #:fill #f (clause ...) (λ () body ...))))` | 向量 |
| `(for/amb (clause ...) body ...)` | `((amb-from (for/stream (clause ...) (λ () body ...))))` | 生成器 |
| `for*/amb` | 同上，换成 `for*/vector`、`for*/stream` | |

两种来源对二路 merge 的折叠：

```racket
;; 向量：长度已知，最后一个备选不再 merge —— 就是递推律
(let loop ([i 0])
  (define s (k (take! alts i)))
  (if (= (add1 i) n) s (m s (λ () (loop (add1 i))))))

;; 生成器：轮到它时才问序列还有没有，所以序列的结束自成一条流
(let loop ()
  (if (more?) (let ([v (get)]) (m (k v) loop)) '()))
```

- **不预读。** 生成器一支不会为了判断「是不是最后一个」而先调用 `more?`。测试 "a sequence is not asked for an element before its turn"：序列交出 0 之后一问就报错，`append` 下第一个答案照样拿得到。向量的备选也是轮到才启动。
- `#:length n` 而实际迭代不足 n 次：`trim` 去掉尾部没填的槽，shuffler 和策略看到的只有真正的备选（v2 是填 `amb-fail` 再压缩）。v2 的 `#:fill` 没有做。
- `#:break` / `#:final` 两种形式都支持，写法同 v2（紧跟在子句之后）。
- 无 `#:length` 的 `for/amb` 用 `for/stream` 把循环变成惰性序列，没有用 v2 的 `call/cc` 协程写法。代价见第 6 节。
- `amb-reflect`（以及 `amb-local`）内部「第一个答案 / 其余答案」的二路选择直接走 `choose`，不经过 shuffler。

## 3. shuffler 和槽位

- `current-amb-shuffler : (parameter/c (-> mutable-vector? void?))`，默认 `void`，与 v2 相同。
- **在哪里读、调用几次。** 在 `amb*` 里、`shift` 之前，也就是选择点上，和 `(current-amb-merge)` 在同一处读。每个向量选择调用一次，作用在**整个**向量上；不分摊到右折叠出来的各个二路节点上（Astra 第 3 问：分摊会改变分布）。只有一个备选时不调用。生成器来源不调用（可能没有尽头）。
- 因为在 `shift` 之前读，写在搜索里面的 `parameterize` 管得到它（测试 "the shuffler is read where the choice is made"）；写在搜索外面的仍有 README 代价 3 的那个注意点。
- **释放。** `take!` 取出一个 thunk 就把槽位置成 `#f`，所以选择点不会一直抓着用过的备选。测试 "a choice lets go of the alternatives it is done with" 用弱引用确认第一个备选用完后它引用的向量被回收，而其余备选还在。
- 顺带的效果：把同一条流强制两次，会碰到已经清空的槽位，现在报错 `an alternative is taken twice; a stream is single-pass`，而不是悄悄重跑。

**随机时比较什么。** 规律表（README 第 3 节）保持确定性的前提：shuffler 是 `void`。带随机 shuffler 时我只做下面两条陈述，比较的对象分别是 Astra 列的第一种和第三种：

1. *同一排列下的轨迹。* 合取的结合律、右分配律、单位律的两边在展开后是同一个表达式，选择点和各自的备选一一对应；给每个选择点固定一个排列，两边的事件轨迹相同。这是论证（两边在 `amb*` 里做的是同一件事），没有测试，也没有请 Astra 看。
2. *有限纯搜索穷举后的答案多重集。* shuffler 只要真的做置换，并且搜索跑到结束，答案的多重集不变，与策略无关。测试 "a permutation keeps the answers of a finite search"（四种 merge 加 `amb-rotate`、`amb-dovetail`）。`once`、有界的 `run`、可能饿死的无穷搜索不在此列。

*每个有限事件前缀的分布*一级我不声称任何律。析取的结合律在分布上是不成立的，测试 "one permutation of a choice is not a permutation at every merge" 是 Astra 的例子：三个各答一次的备选，c 是第一个答案的频率，`(amb a b c)` 约 1/3，`(amb (amb a b) c)` 约 1/2，`(amb a (amb b c))` 约 1/4。

## 4. 两种 n 路策略

[`nary.rkt`](nary.rkt)，各十几行。两者都是一次接纳一个备选，所以备选可以有无穷多个。

**`amb-rotate`：n 路的 interleave。** 已启动的备选轮流运行，一个备选暂停时，这次暂停就是选择点的暂停，然后换下一个；所有人都轮过一遍之后，再启动一个新备选。它自己不产生暂停。

- 空：`'()`；单个：就是那条流；两个：与 `amb-interleave` **逐事件相同**（2000 组随机流，另加经由 `amb` 的同一比较）。
- 三个：与右梳不同，这正是提供它的理由。三个不断产出答案的分支，份额约各 1/3（右梳是 1/2、1/4、1/4）。
- 例：`a P a P a`、`b P b P b`、`c P c` 三条流 → `a P b P a P c P b P a c b`。

**`amb-dovetail`：分阶段。** 第 r 阶段：已启动的备选按启动的先后各运行到自己的下一次暂停，然后再启动一个新备选、运行到它的暂停；阶段结束时选择点暂停**一次**。这是 Astra 第 2 问局部定理里的调度（每轮至多接纳一个新备选，再给每个活着的备选一次服务）。

你问的是：阶段结束、没有任何备选在等、但可能还有备选没启动时，要不要也暂停一次。两种做法都写了，轨迹如下（`P` 是暂停）：

| 备选 | `amb-dovetail`（每阶段都暂停） | `amb-dovetail/unguarded`（有人暂停才暂停） | `amb-rotate` |
|---|---|---|---|
| `a P a P a`，`b P b P b`，`c P c` | `a P a b P a b c P b c` | 相同 | `a P b P a P c P b P a c b` |
| `a`，`b`，`c`（各自立即回答） | `a P b P c P` | `a b c` | `a b c` |
| 三个都立即失败 | `P P P` | 空 | 空 |
| `(amb-from (in-naturals))` | `0 P 1 P 2 P …` | `0 1 2 3 …`，永不暂停 | 同左 |
| 可数个备选全部立即失败 | `P P P …` | **一个事件也交不出来** | 同左 |
| 单个备选 `a` | `a P` | `a` | `a` |
| 单个备选 `a P b` | `a P b` | `a P b` | `a P b` |

它对外层的哪些策略有效（测试 "a choice among infinitely many, and the alternative next to it"：`(amb inner 'x)`，inner 是可数个备选的选择，问 x 找不找得到）：

| inner 的策略 | 外层 append | interleave | alternate | level | rotate | dovetail |
|---|:-:|:-:|:-:|:-:|:-:|:-:|
| 每个备选立即回答：append、rotate、dovetail/unguarded | ✗ | ✗ | ✓ | ✗ | ✗ | ✗ |
| 每个备选立即回答：**dovetail** | ✗ | ✓ | ✓ | ✓ | ✓ | ✓ |
| 每个备选立即失败：append、rotate、dovetail/unguarded | ✗ | ✗ | ✗ | ✗ | ✗ | ✗ |
| 每个备选立即失败：**dovetail** | ✗ | ✓ | ✓ | ✓ | ✓ | ✓ |

读法：

- 外层 `append` 永远无效，它看见暂停也继续左支（Astra 的反例）。所以不能承诺「对任意外层 merge 防饥饿」，只能说：对所有「左边暂停就换边或一起等」的外层策略有效。
- 不带守卫暂停的做法，只在「每个备选立即回答 + 外层 alternate」这一格有效，那是 alternate 自己的功劳（它在答案处也换边）。全部失败时内层连一个事件都没有，任何外层都无从换边。
- 所以**守卫暂停是这件事的全部内容**；轮转本身（`amb-rotate`）不解决它。

代价也在表里：

- `amb-dovetail` 不满足单备选律（`a` 变成 `a P`），也不是任何一种二路 merge：两个备选 `a P c` 和 `b`，它给 `a P c b P`，`level` 给 `a b P c`，`interleave` 给 `a P b c`。它与别的策略只在答案的多重集上一致。多出来的那个尾暂停，是因为它要到下一阶段才去问还有没有备选；想去掉就得预读，而预读正是不该做的。
- 所以按 Astra 说的那个选择，我的建议是：**二路相容不向 `amb-dovetail` 要求**，它是点名使用的策略；`amb`、`for/amb` 的默认行为不变，没有守卫暂停。需要「可数选择不饿死兄弟」的地方写 `(amb-from seq amb-dovetail)`。
- `amb-dovetail/unguarded` 只是为了对照，我不建议保留。

**嵌套。** n 路节点嵌套 n 路节点仍然有分组；`amb-reflect` 的内部选择是二路嵌套的，所以 `amb-local` 之后的答案在 `amb-rotate` 下是右梳的份额，不是等份。原型没有处理这一点。

## 5. 单位律用在哪里

换成新接口之后：

| 位置 | 用到的律 | 说明 |
|---|---|---|
| 向量 + Merge | 无 | 最后一个备选不 merge，就是递推律 |
| 生成器 + Merge | `s ∨ 0 = s` | 序列的结束是一条空流；这是不预读的代价 |
| 向量与生成器给出相同的事件 | `s ∨ 0 = s` | 测试 "the alternatives in a vector, and from a sequence"，四种 merge |
| `amb-reflect` 对单答案流直接返回 | `s ∨ 0 = s`；对 Strategy 是 `S([a]) = a` | |
| `(amb)` 不调用策略 | 对 Strategy 是 `S([]) = 0` | |
| 把失败当作析取的单位元 | `0 ∨ t = t`，`s ∨ 0 = s` | |

对四种内置 merge，用到的只有双侧单位律，Astra 已证。对 Strategy，内核自己不再用任何律（结束由策略自己处理），但上面两处捷径要求 `S([]) = 0`、`S([a]) = a`：`amb-rotate` 满足（测试），`amb-dovetail` 的 `S([a])` 多一个尾暂停，所以在它下面 `amb-reflect` 的捷径只在答案一级成立。

## 6. 基准

`bench/amb.rkt`，深度优先，ms（5 次的中位数；「方程」是同一内核换用照方程写的 merge）：

| | master | 内核 | 方程 |
|---|--:|--:|--:|
| 宽：200000 个备选，`amb-from` | 76 | 53 | 32 |
| 宽：`for/amb`（无 `#:length`） | 75 | 96 | 79 |
| 宽：`for/amb #:length` | 39 | 38 | 28 |
| 8 皇后，`amb-from` | 7 | 6 | 6 |
| 8 皇后，`for/amb #:length` | 7 | 5 | 5 |
| 8 皇后，`(amb 0 … 7)` | 6 | 4 | 6 |
| 10 皇后，`amb-from` | 151 | 149 | 147 |
| 10 皇后，`for/amb #:length` | 153 | 129 | 120 |
| 10 皇后，`for/amb` | 167 | 162 | 195 |

（皇后各行的 master 一列都是 v2 的 `for/amb #:length`，重复测的，可以当噪声的参照。）

- 向量形式与 master 持平或略快；槽位清空和 `#f` 检查看不出代价。
- 无 `#:length` 的 `for/amb` 比 `amb-from` 慢一倍左右（96 对 53），比 master 慢约三成：这是 `for/stream` 每个元素一个流单元的代价，v2 的协程写法正是为了避开它。要追平可以照搬 v2 的写法，原型没做。
- 宽选择上内核比照方程写的慢（53 对 32）：`append` 的记录每个备选要建一条记录和一个表节点，而这里没有嵌套可以压平。这是上一轮就有的常数，与本次改动无关。

## 7. 没有做的、要你定的

- `amb-dovetail` 的完备性前提没有逐条对照 Astra 第 2 问的局部定理（「生成器每次拉取有限时间」「一次服务有限时间」）；`amb-rotate` 两路等于 interleave 只有测试。
- 强制时刻的对照（`tests/flat.rkt` 那种）没有扩展到 n 路策略和生成器来源。
- `Next` 是单个过程。若改成 v2 那样的 `more?` / `get` 一对，已知长度的来源可以让 `amb-dovetail` 去掉尾暂停，但对生成器来源就引入了预读的可能；我选了不给策略这个能力。
- 名字：`current-amb-merge` 还是 `current-amb-strategy`；`amb-rotate`、`amb-dovetail`。
- `#:fill`、contract、Typed 包装没有做。
