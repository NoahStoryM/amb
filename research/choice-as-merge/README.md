# 选择即合并：搜索是从计算到事件流的折叠

研究分支 `research/choice-as-merge-20261112`，基于 amb master `6340911`。只新增本目录，不改动 `pkgs/`。讨论在 [#22](https://github.com/NoahStoryM/amb/issues/22)；Astra 对 `1ac7057` 的数学审阅是其中的[这一条](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6821738392)，下文「Astra 证明了」都指它和她对 `32acf32` 的[复核](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6842391845)（数学证明或逐分支模拟，没有机器验证）。

按约定，这一轮**没有阅读** PR #10–21（`local-frontier`、`split/*` 分支）的实现代码，只看了它们的标题和 diff 行数；ambKanren 的 `research/local-frontier-20261110` 是那套接口的使用方，也没有读。读过的是：amb master、ambKanren master / `lawful-search` / `research/local-search-20261109`、miniKanren-typed master。ambKanren issue #14 的正文我这里打不开，对它的了解来自研究分支里的 `README.md`、`VERSIONS.md` 和各个 `.out` 文件。

## 结论

1. **直接风格的公平搜索只需要一个新效应和一个参数。** 新效应是 `pause`（暂停）；参数是每个选择点上的 `merge`：一个把两条事件流合成一条的二元运算。frontier 不必作为数据结构出现：它就是流表达式里还没求值的那部分。
2. 内核 [`kernel.rkt`](kernel.rkt) 一共 194 行代码（不计注释；其中选择的各种形式——向量、生成器、`for/amb`、shuffler——约 60 行，见 [`CHOICES.md`](CHOICES.md)），其中控制部分是四个定义、十来行：就是用 `shift`/`reset` 写非确定性的教科书写法 `(shift k (append (k a) (k b)))`，把列表换成带暂停的惰性流，把 `append` 换成参数。四种 merge 的定义方程各五六行，写在规格 [`model.rkt`](model.rkt) 里；内核里结合的两种（`append`、`level`）走扁平的快路径，共 55 行，另加 [`catenable.rkt`](catenable.rkt) 44 行（常数时间拼接的表）。
3. `disj = amb`、`conj = 函数复合`、`defrel = 先 pause`，配上 `amb-interleave`（即 `append-inf` / `mplus`），TRS2 书中 285 个例子有 **283 个与原书逐答案相同**，耗时与 miniKanren-typed 的流实现持平（11.1 s 对 12.0 s）；ambKanren master 是 270 个、353 s。
4. 把 `conj` 改成「先局部搜索第一个目标」（一行），就是原书的 `bind`：285 个全同。所以原书与直接风格的差别**只有这一处**，而合取的结合律、分配律也正是在这一处丢掉的。
5. 换成 `amb-level`，得到的就是 ambKanren 现在的全局前沿顺序（285 例中 284 例逐答案相同，另一例的原因见下）。所以「全局前沿」不是另一种机制，而是一种 merge。
6. 复杂性确有不可避免的部分，但要说得比上一版窄（Astra 的审阅第 3 问）：**只看叶子的序列就能算出结果，当且仅当 merge 结合**（命题一）。`append`、`level` 结合，所以它们的调度状态可以是一条不带括号的序列（栈、按轮的队列），master 的 deque 是这种情形。`mplus` 不结合，所以要得到书上的交错，状态里必须以某种形式带着分组的信息；本方案把它留在流的闭包里。这个定理**没有**说状态必须是一棵树（序列的元素可以带路径、权重来编码分组），也**没有**给出每次暂停的代价下界；上一版的「不结合 ⇒ 状态必须是树」「这一步省不掉」说过头了。

## 1. 机制

一个直接风格的非确定性计算是一棵逐步展开的树：它或者返回（叶子），或者失败，或者暂停，或者选择。搜索的观察者按时间顺序只能看到两种事件：一个答案，或者「还没有」。所以搜索的结果类型是

```
Stream = '()                     没有更多事件
       | (cons vs (→ Stream))    一个答案，然后（惰性地）是其余部分
       | (→ Stream)              一次暂停，然后是其余部分
```

这不是从 miniKanren 搬来的预设：它是「一次只运行一个分支、分支能让出控制」这两个条件下观察者所能看到的全部。它恰好是 microKanren / TRS2 的流（元素为 Pause ∪ a，即 `Streamof (Maybe a)`），只是答案之后的尾部也是惰性的（TRS1 的流有这一点，TRS2 没有；amb 的 `in-amb` 需要它：`(amb 1 (loop-forever))` 的第一个答案必须拿得到）。

每个操作说明自己对这条流的贡献。`k` 是到搜索边界为止的续延，`(k v)` 是「操作返回 v 之后」的事件流：

```racket
(define (amb-reify thk)                 ; 搜索：把 (thk) 折叠成流
  (reset-at tag (unit (call-with-values thk list))))

(define (amb-fail)  (shift-at tag k '()))
(define (amb-pause) (shift-at tag k (λ () (k (void)))))
(define (amb* merge alts)               ; (merge (k a) (λ () (merge (k b) (λ () (k c)))))
  ((shift-at tag k                      ; alts 是 thunk 的向量；这里略去 shuffler、槽位的释放和 n 路策略
     (let loop ([i 0])
       (define s (k (vector-ref alts i)))
       (if (= (add1 i) (vector-length alts))
           s
           (merge s (λ () (loop (add1 i)))))))))
```

`(amb e ...)` 是 `(amb* (current-amb-merge) (vector (λ () e) ...))`，`(amb)` 是 `(amb-fail)`。`amb-from`、`for/amb`、`for*/amb`、`current-amb-shuffler` 和看得到全部备选的 n 路策略（`amb-rotate`、`amb-dovetail`）见 [`CHOICES.md`](CHOICES.md)。策略属于选择点（在选择时读取参数），同一次搜索里可以混用（惰性带来的一个注意点见第 6 节代价 3）。

**策略只是一条方程。** 四种 merge 对「已就绪的答案」和「结束的流」处理相同，区别只在 `s` 暂停时怎么办：

```
'()     ∨ t = t
(a : s) ∨ t = a : (s ∨ t)               (amb-alternate: a : (t ∨ s))

amb-append      pause s ∨ t       = pause (s ∨ t)     s 继续占着         深度优先
amb-interleave  pause s ∨ t       = pause (t ∨ s)     让给对方           mplus / append-inf
amb-alternate   同上，且答案之后也让给对方                               TRS1 的 mplusi
amb-level       pause s ∨ pause t = pause (s ∨ t)     两边一起等         按暂停数广度优先
```

暂停和惰性尾部都是 thunk，merge 的第二个参数也是 thunk，所以「让给对方」就是交换两个参数。照方程写，每个 merge 五六行（`model.rkt` 里四个都是这样写的，内核里 `amb-interleave`、`amb-alternate` 也是）。

**结合的 merge 走扁平的快路径**（#22 第 4 点）。照方程写的 merge 是包着 `s` 的一个闭包，`s` 的每个事件都要穿过它上方每一个还开着的选择点：n 层嵌套，每个答案 n 步。结合律 `(s ∨ t) ∨ u = s ∨ (t ∨ u)` 说括号无关紧要，所以没做完的流可以记成一条「还剩什么」的记录：

```racket
(struct appended (s ts) …)      ; 请求 (s)，然后依次是 ts 里各个 thunk 的流          —— 栈
(struct leveled (ws s ts) …)    ; 一轮里：ws 已暂停，(s) 正在跑，ts 还没跑；然后暂停一次，下一轮
```

两个记录都带 `prop:procedure`，仍然是 thunk；别的 merge、`amb-await`、`in-amb` 只通过事件观察流，不需要知道它们。压平发生的条件是：**此刻被请求的 thunk 是同种记录**。这时不调用它，而是读出它的字段并进当前的记录（`appended(appended(s′,ts′),ts)` 变成 `appended(s′, ts′ ++ ts)`）；其余的请求一律按普通的流来调用。这是运行时对「正要请求的那个值」的判断，不是语法条件：`level` 的右参数轮到时同样可以是记录，另一种策略也可能已经把原来的流原样交还（`'() ∨ t = t`），那时语法上的边界已经不在了。上一版说的「沿第一个参数直接嵌套的极大段」只适合描述左深的基准。

两个待办列表 `ts`、`ws` 是 [`catenable.rkt`](catenable.rkt) 的可拼接表（Okasaki），因为并记录时哪一边长都有可能。上一版用 Racket 的 `append`，在 Astra 的单遍例子上是平方的（见命题三）。

**局部搜索是一个值。** `amb-reify` 返回流，流可以交给任何人。三个小函数把它接回直接风格：

```racket
(amb-await s)          ; 把 s 开头的暂停转发给外层搜索，返回 '() 或以答案开头的流
(amb-reflect s merge)  ; 返回 s 的一个答案：在外层搜索里对所有答案做选择；暂停照样转发
(amb-local e)          ; = (amb-reflect (amb-reify (λ () e)))
```

`once`、`ifte` 各三行，天然对暂停透明（`kanren.rkt`）：

```racket
(define ((once g) s) (amb-once (λ () (g s))))
(define ((ifte g1 g2 g3) s)
  (define s* (amb-await (amb-reify (λ () (g1 s)))))
  (if (null? s*) (g3 s) (g2 (amb-reflect s*))))
```

暂停和选择用同一个 prompt，所以暂停不可能越过最近的搜索逃出去：不转发暂停的局部搜索（普通的 `in-amb`）只会让内层成为外层的「一步」，不会丢答案。ambKanren 研究分支里的 `pause-escape*.rkt` 反例在这里得到 `(1)` / `(1 2)`（`tests/kanren.rkt` "a pause does not escape its search"）。amb master 的 `#:empty-handler` 和 `(amb)` / `(amb-fail)` 的区分、ambKanren 的 `suspend` 里靠 `current-amb-depth-first?` 调整入队位置的两个选择点，在这里都不需要：`(amb)` 就是失败，暂停就是暂停。

**规格。** [`model.rkt`](model.rkt) 的 `spec` 是不用控制算子的同一语义：每个目标显式接收续延 `k : State → Stream`，十几行；四种 merge 照方程写。**规格的定义是独立的**：流、四种 merge、`spec` 都不调用 `kernel.rkt` 的任何东西；模块顶部仍然 `require` 它，只是为了同一个文件里的 `direct` 对照。`tests/model.rkt` 检查 `direct = spec`，事件序列（含暂停位置）完全相同：4 种策略各 3000 个随机目标（含 `local`/`once`/`ifte`），再加每个析取各自随机指定策略的 3000 个。

## v3 规格的骨架：三个独立的命题

Astra 建议把「可消去括号」「何时强制」「一次操作的代价」分开陈述。它们确实互不蕴含：`tests/flat.rkt` 末尾的 eager-append 满足一而不满足二；上一版的内核满足一、二而不满足三。规格本身是 `model.rkt` 的流、四条 merge 方程和 `spec`；下面三个命题说的是一个实现（这里是 `kernel.rkt`）相对于它要满足什么。

**流和等式。** 流是 `νX. 1 + (V × X) + X`：字母表 `V ⊎ {P}` 上有限或无限的词，`V` 是一次返回的整组值，`P` 是暂停；`0` 是结束。thunk 在交出下一个构造子之前不终止是另一回事（`⊥`），与 `P^ω` 可以区分。流的相等指：每个有限的事件前缀相同，并且在同样的位置能看到结束。

### 命题一：可消去括号

> 对一个固定的二元运算（及其观察同余），把有限、有序、非空的二叉树按该运算求值。存在只依赖**叶子序列**的函数 F，使每棵树的值都等于 F 作用在它的叶子序列上，当且仅当该运算在可以作叶子的值上结合。若还允许插入、删除空子树，则再需要双侧单位元。

这是自由半群、自由幺半群的标准性质（必要性：三片叶子的两种括号；充分性：用结合律把任何树旋转成同一条右梳）。Astra 证明了四种内置 merge 各自的情况：

| | `0 ∨ t = t`，`s ∨ 0 = s` | 结合（事件级） |
|---|:-:|---|
| `append` | ✓ | ✓　事件词的惰性串接；左边无限时吸收右边 |
| `level` | ✓ | ✓　按轮：每轮的答案串按左、右拼接，轮末的暂停是各输入「暂停」标志的或；末尾的空轮要保留（`P 0 ≠ 0`） |
| `interleave` | ✓ | ✗　`P 0, [b], [c]`：左结合 `P c b`，右结合 `P b c` |
| `alternate` | ✓ | ✗　`[a], [b], [c]`：左结合 `a c b`，右结合 `a b c` |

两个反例和单位律的例子在 `tests/kernel.rkt` "the grouping shows, unless the strategy is associative"、"no alternatives is failure; a pause is not"。

这个命题**不说**的事：

- 它不说不结合的策略的状态必须是一棵树。它只禁止从**不带标注**的叶子序列恢复结果；序列的元素若带着路径、权重或程序计数器，仍然可以编码分组。
- 它不给代价的下界。不能从一个反例推出每次暂停都要 Ω(树高) 的更新。
- 它不说份额。结合只保证括号不影响结果；`append` 也结合，却把全部机会给第一个分支（第 2 节）。
- 它不说惰性（命题二），也不说代价（命题三）。

### 命题二：何时强制

> 对单遍、只按构造子观察流的使用者，内核的 merge 与方程的 merge 在每个有限次请求之后给出相同的事件，并且以相同的次序调用了相同的源 thunk（包括阻塞在哪一次源调用上）。

外延的结合律不够：eager-append 在全流上仍然计算串接、仍然结合，却在交出第一个答案之前强制右支；右支若在第一个构造子之前发散，第一个答案也丢了。所以要模拟的是「到下一个事件之前运行哪些用户 thunk、按什么次序」。两条不变量（按现在的表示写，`ws` 是正序）：

- `appended(s, ts)` 被调用：请求 `s`，然后依次处理 `ts`。当前要请求的若是 `appended(s′, ts′)`，展开为 `s′, ts′ ++ ts`：只读字段，不调用任何 thunk。遇到答案，交还答案并保存尾部；遇到暂停，交还一个可恢复的暂停；遇到结束，才取下一个待办。
- `leveled(ws, s, ts)` 被调用：本轮已经暂停的是 `ws`（按暂停的先后），当前请求 `s`，之后才是 `ts`。当前要请求的若是 `leveled(w′, s′, t′)`，展开为已暂停 `ws ++ w′`、当前 `s′`、待办 `t′ ++ ts`（内层已经跑过本轮的一部分时也是如此）。普通请求只调用当前的 thunk：答案立即交还，结束则丢弃，暂停则登记而不恢复。待办用完时，没有等待者就结束，否则交还**一个**暂停（只有一个等待者时，它自己的 thunk 正好就是这一个暂停）；下一次请求才按原来的次序恢复它们。

展开不产生用户调用，其余每次请求都对应方程解释器的下一次请求；对请求次数归纳即得。混用策略时按这个关系做上下文替换，不需要 `append` 与 `level` 之间的任何混合律。

范围和现状：

- Astra 先对 `1ac7057` 做了逐分支模拟（那一版的 `ws` 是逆序的 Racket 列表，`ts` 用 `append` 拼接），又对 `32acf32` 的新表示（可拼接表，`ws` 正序）[复核](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6842391845)过：把可拼接表读成元素的序列，上面两条不变量成立。她点明了一处：`cat-rest` 会 `force` 表内部延迟链接的 promise，里面装的是表的节点；源流的 thunk 是节点的**元素**，这一步只搬运它，不运行它。测试：`tests/flat.rkt` 在随机的流表达式上（四种策略混用，中途取走若干事件后再合并）比较内核与方程的事件和每个 thunk 被强制的时刻，`tests/model.rkt` 比较 `direct` 与 `spec`。
- 观察器也在范围内。`amb-events` 上一版在收满之后多强制了一步（Astra 复现的 `(amb-events (cons '(a) (λ () (error "tail forced"))) 1)`），已修正；`tests/kernel.rkt` "amb-events stops at the last event it was asked for" 对「最后一个事件是答案」「是暂停」各有一个尾 thunk 一强制就报错的测试，`amb-take` 同样。
- 自定义的 merge 若检查记录的身份、调用栈或内部闭包的身份，就超出了「按构造子观察」这个接口，命题不管它。

### 命题三：一次操作的代价

> 设每份残余状态（一条记录，连同它的两张表）至多被消费一次，并且从空的调度状态开始计费。设 M 是 `amb-append`、`amb-level` 被调用的次数，R 是这两种策略的代码发出请求（调用一个 thunk）的次数；展开不是请求。那么这两种快路径自己做的全部工作（不算被请求的 thunk 内部做的事）是 O(M + R)。

前提是所有权条件，比「每个 thunk 至多调用一次」强（按 Astra 的复核改写）：**被另一条记录展开同样算一次消费**，即使没有调用它的 `prop:procedure`，否则通过别名反复展开同一条记录不受约束；所有输入记录的构造都计入同一段历史，若从一条已经建好的大记录中途开始量，要另计初始势能。只读字段、查看首元素可以重复。

由命题二，这 R 次请求方程也都要发出，所以快路径没有增加请求，自己的开销摊到每次 merge 调用、每次请求上是常数。论证分三步：

1. **一次运行做什么。** 一次运行（强制一条记录，或 `amb-level` 在左流暂停时直接开始）先做若干次展开，再发出至少一次请求，最后至多新建一条记录就返回。每次展开、每次请求只做常数次表操作（`cat-append`、`cat-first`、`cat-rest`、`cat-of`）和常数的其他工作。
2. **数展开。** 记录只在两处新建：一次 merge 调用至多一条（`amb-level` 直接开始的那次运行所建的一条算在这里），一次强制返回时至多一条；设记录被强制 F 次，则新建的记录不超过 M + F 条。由所有权条件，每条记录至多被消费一次：被强制，或被展开。所以展开数 E 满足 E + F ≤ M + F，即 **E ≤ M**。每次运行至少一次请求，所以运行数不超过 R。于是表操作的总数 K 是 O(M + R)。
3. **K 次表操作的代价是 O(K)。** 两层均摊分开算（Astra 的写法，避免把「均摊常数的队列」直接代进定理）：
   - *只数调用。* 先把一次队列原语记作单位成本。`cat-append` / `link-all` 对应 Okasaki 博士论文（CMU-CS-96-177）§7.2.1 的图 7.5（书 §10.2.1），只是直接存已经建好的第二棵子树，省去一个平凡的 suspension。由定理 7.1，K 次表操作发出 O(K) 次队列原语，包括执行延迟的链接时发出的。
   - *再付队列的实际代价。* 队列是一对列表。给每个活着的队列的 rear 里每个格子存常数个信用：入队新建一个格子，出队时若要翻转 rear，按格子花掉信用。要检查的是信用只转移一次：`cat-append` 把旧根的 kids 移给新根并接入右边的表；`link-all` 出队后把孩子的 kids 移给结果，把余下的队列只移进**一个**记忆化的 promise。旧的残余丢弃之后，没有第二条更新历史能再花同一批信用。所以所有翻转的长度之和不超过入队的总数（计步用的 `length` 再扫一遍，只改常数）。

命题之外：

- **R 不以事件数为界。** `append` 的每次请求要么带回一个事件，要么消掉一个待办，所以 R ≤ M + F，F 是记录被强制的次数，也就是从合并的流里取走的事件数。`level` 每轮要请求每条还活着的流一次，这是方程规定的，不是快路径加的。
- **重复强制。** 流不记忆（第 6 节代价 2）。把同一条记录强制两次，结果仍然正确（表是持久的，只有 promise 被赋值），但所有权条件不成立，界也就不成立；`tests/catenable.rkt` 最后一个测试是这种情况。
- **`interleave`、`alternate`** 仍照方程写，一个事件要穿过它上方所有开着的 merge。这是现在这个实现的代价，不是下界（命题一）。

证据和现状：Astra [复核](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6842391845)后同意这个端到端的 O(M + R) 均摊界，前提和第 3 步的写法是按她的意见改的。它是快路径自身的成本，不是每个答案等待时间的上界，单次操作也不是常数：10000 个元素从左到右拼成的表，第一次 `cat-rest` 就是 10000 步（她实测），正是均摊界允许的。`tests/catenable.rkt` 数取尾的步数（每次链接一步，翻转队列时每个元素一步）：随机的操作序列、先全部接在后面再逐个取、交替进行，所测的整段序列**平均**每个操作不超过 3 步。`tests/flat.rkt` "merged again and again: the steps" 在 Astra 的单遍例子 `sᵢ₊₁ = ((cdr (amb-append sᵢ (λ () '()))))` 上，n = 10³、10⁴、10⁵ 总共都不超过 4n 步。时间（`bench/amb.rkt` "merged again and again"，ms）：

| n | 5000 | 10000 | 20000 | 40000 |
|---|--:|--:|--:|--:|
| `append`，`1ac7057`（Racket `append`） | 66 | 274 | 1028 | 4747 |
| `append`，现在 | 0 | 1 | 3 | 10 |
| `append`，照方程写 | 87 | 371 | 1863 | 10480 |
| `level`，`1ac7057` | 46 | 195 | 1110 | 5374 |
| `level`，现在 | 0 | 1 | 4 | 10 |
| `level`，照方程写 | 75 | 325 | 1936 | 10371 |

（`1ac7057` 两行是我在这台机器上测的，与你测的 79/306/1242/6015 同一量级。）

### 公开自定义 merge 的最小约束

Astra 第 7 问的建议。`amb*` 接受任何 `Stream × (→ Stream) → Stream` 的过程，但「能作为参数传入」和「满足内核依赖的定律」是两回事：

- 把后续分发给各分支（第 3 节合取的结合律、右分配律）对 merge **没有任何要求**，不结合、没有单位元也成立。
- 把失败当作析取的单位元，用到 `0 ∨ t = t` 和 `s ∨ 0 = s`。内核里有三处用到右单位律：`amb*` 对最后一个备选不再调用 merge；`amb-from` 则在序列用完时以 `'()` 收尾，两者一致要靠它；`amb-reflect` 对已知只有一个答案的流直接返回（即把 `k a ∨ 0` 化成 `k a`）。
- 命题二涵盖四种内置策略及其混用，命题三估计的是其中 `append`、`level` 两种快路径；两者都不涵盖自定义的 merge，它要自己保证不提前强制第二个参数，并且只按构造子观察它的参数。

## 2. 公平必须保证什么，什么只是策略

**必须的**（任何顺序执行的搜索都绕不开）：

- **让出点。** 驱动器只有在分支交出控制时才能换人，所以必须有某个操作产生「非答案事件」。它可以是独立的 `pause`，也可以并进选择里：master 的 `current-amb-depth-first? #f` 就是「每个选择点都让出一次」（`tests/master-flags.rkt`）。我选独立的效应，因为在哪里让出和在哪里选择是两件事：`defrel` 让出而不选择，`conde` 选择而不让出。
- **轮到。** 每个还活着的分支都要被无限次轮到。
- **局部搜索要转发暂停。** 否则内层搜索对外层是一步，`(conde [(once (nevero))] [(== q 1)])` 就卡住。

**只是策略的**：谁先走、每个分支分到多少、嵌套的括号是否看得出来、答案的顺序。`tests/kernel.rkt` "turns among three alternatives"：三个各自不断产出答案的分支 `(amb a b c)`，答案的份额在 `amb-interleave` 下是 1/2、1/4、1/4，在 `amb-level` 下是各 1/3。两者都公平。

各 1/3 **不是结合律的结果**（Astra 第 5 问；`append` 也结合，份额是 1、0、0）。它来自 `level` 的同步轮次：每一轮里每个还活着的分支都运行到自己的下一次暂停，而这个例子里每个分支每轮恰好产出一个答案。让第一个分支每轮产出两个，`level` 下的份额就是 1/2、1/4、1/4（同一个测试）；每轮仍然只有一个合并后的暂停。`interleave` 的 1/2、1/4、1/4 则来自多路选择是右嵌套的二路选择：在一直可运行、按约定让出的同速分支上，二路 `interleave` 给一片叶子的服务频率是 2^(−叶深)，而三片叶子的有序满二叉树只能是深度 1、2、2，所以每个输入恰好用一次、只靠二路嵌套，得不到三等分。要 n 个同级分支等轮次，得有明确的 n 路接口（第 7 节）。

`tests/fairness.rkt`（✗ = 前 4000 个事件里找不到 x）：

| 程序 | append | interleave | alternate | level |
|---|:-:|:-:|:-:|:-:|
| 左支永远暂停 | ✗ | ✓ | ✓ | ✓ |
| 左支无限个答案，答案之间暂停 | ✗ | ✓ | ✓ | ✓ |
| 左支无限个答案，从不暂停 | ✗ | ✗ | ✓ | ✗ |
| 生成-测试 | ✓ | ✓ | ✓ | ✓ |
| 生成-测试，第一个候选的测试永远暂停（C6） | ✗ | ✓ | ✓ | ✓ |
| `once` 的测试永远暂停 | ✗ | ✓ | ✓ | ✓ |
| x 排在一个永远暂停的计算**之后** | ✗ | ✗ | ✗ | ✗ |

最后一行是合取：`(begin (nevero) 'x)` 里 x 不在任何分支上，调度器无能为力（要解决它得并发地运行合取的两边，那是另一个问题）。第三行说明 `interleave`/`level` 的公平性是有前提的。Astra 第 6 问给出了准确的条件并证明了完备性：

> 树由返回、失败、暂停和**有限分支**的选择组成，**展开单个节点耗时有限**，每条无限路径上有无穷多个暂停。那么用 `interleave`、`alternate` 或 `level`，持续请求结果时，每条有限路径上的答案都会在有限步内出现。

证明用 König 引理（截到各路径的第一个暂停，这一片是有限的）。它给的是「最终出现」，没有统一的延迟上界，也不把 `once` 剪掉的叶子算作答案。前提不能简写成「找不到一条没有暂停的路径」：`(amb (let loop () (loop)) 'x)` 的第一个分支连一个节点都不交出；`(amb-from (in-naturals))` 这种不暂停的无限选择仍会饿死右边的兄弟（它展开成一条没有暂停的无限右梳）；点名使用 `amb-dovetail` 可以解决，代价是多出来的暂停，见 [`CHOICES.md`](CHOICES.md) 第 4 节。`defrel` 保证的是第三个条件。公平是活性性质，不是等式：`append` 和 `level` 都是幺半群，公平性却不同。

**公平不等于及时。** x 前面有 d 个永远暂停的分支时，x 之前的暂停数：`interleave` 是 2^d − 1，`level` 是 0（同一测试文件 "how long x waits"）。这是 `conde` 靠后的子句「理论上公平、实际上等不到」的来源，是 `mplus` 二元嵌套的结果，不是公平性的要求。

**TRS1 的教训**（[`kanren1.rkt`](kanren1.rkt)）：第一版没有暂停，分支只有产出答案时才让出。但一个目标的答案只有对「单独搜索这个目标」的观察者才是事件；在函数复合里，被后一个目标否决的答案根本不产生事件。所以 TRS1 的 `all`/`alli` 必须局部搜索第一个目标（即 `bind`/`bindi`），`condi` 才有机会换人。暂停没有这个问题：它一发生就是事件，穿过复合仍然可见。**这就是直接风格需要暂停、而不能只靠答案级调度的原因。**

## 3. 合取、析取的规律与答案顺序

**直接风格下，合取的规律与策略无关。** 在 `spec` 里按定义展开：

```
run (seq (seq a b) c) s k = run a s (λ (s) (run b s (λ (s) (run c s k)))) = run (seq a (seq b c)) s k
run (seq (alt a b) c) s k = merge (run a s K) (λ () (run b s K))            = run (alt (seq a c) (seq b c)) s k
                            其中 K = (λ (s) (run c s k))
```

两边是同一个表达式，所以结合律、右分配律、单位律对**任何** merge 都成立，而且是事件级的（暂停的位置也相同）。原因是选择点捕获了续延：后面的计算已经在每个分支里面，merge 看得到它的暂停。这也是 `conj = 复合` 仍然公平的原因（C6）。`compare.out` 里 `pause/disj` 方案在 C6 上饿死；按我对 `schemes.rkt` 的阅读，原因是它的 `reflect` 产生的选择点落在扁平的深度优先前沿上，对之后的暂停没有反应。

**析取的规律只取决于 merge。** `fail` 是双侧单位元对四种都成立；结合律对 `append`、`level` 成立，对 `interleave`、`alternate` 不成立（Astra 分别给了证明和反例，见命题一）；逐事件的交换律都不成立。有限输入下四种 merge 都是保留两侧次序的交织，每个事件恰好一次，所以在「多重集」意义下交换、结合都成立。但 **bag 等价不是同余**：外面套一个 `once`，两个排列就成了不同的答案（`tests/kernel.rkt` 里 `(pause c)` 对 `(pause b)`）。下表里的 bag 只在 `laws.rkt` 用到的上下文里成立：洞本身，以及后面接一个合取。

**书上的流把合取的规律也丢了。** `bind` 先把第一个目标的结果变成流（忘掉它是怎么由析取组成的），再把后续接到每个答案上。用内核的话说：

> 书的 `conj2 g1 g2` = `(g2 (amb-local (g1 s)))`

`amb-local` = `reflect ∘ reify` 把一棵子树规范化成右梳 `a | (b | (c | …))`。merge 不结合时，这一步改变顺序。`tests/laws.rkt` 在随机目标上给每条律找出成立的最强等价（`events` ⊂ `answers` ⊂ `bag`），空白处给出最小反例（`LAWS_VERBOSE=1 raco test tests/laws.rkt`）：

| 律 | direct: append | interleave | alternate | level | book: append | interleave | alternate | level |
|---|---|---|---|---|---|---|---|---|
| 合取结合 | events | events | events | events | events | **bag** | **bag** | **bag** |
| 右分配 `(a∨b);c = a;c ∨ b;c` | events | events | events | events | events | **bag** | **bag** | **bag** |
| 合取单位元、`fail;a = fail` | events | events | events | events | events | events | events | events |
| `a;fail = fail` | answers | answers | answers | answers | answers | answers | answers | answers |
| `fail` 是析取单位元 | events | events | events | events | events | events | events | events |
| 析取结合 | events | bag | bag | events | events | bag | bag | events |
| 析取交换、左分配 | bag | bag | bag | bag | bag | bag | bag | bag |
| `(local a) = a` | events | bag | bag | bag | events | events | events | events |

读法：direct 一侧，合取的结合、右分配、单位各行全是 events；book 一侧只有 `append`（列表单子）守得住。`a;fail = fail` 只到 answers：a 的暂停还在，a 不终止时左边也不终止，这对任何顺序执行的搜索都一样。book 的 `level` 也守不住（层内顺序会变），这与 `VERSIONS.md` 里 Astra 的分析一致。`(local a) = a` 一行就是 direct 与 book 的全部差别。

最小的例子（`tests/kernel.rkt` 末尾）：后续计算对 a 多暂停一次时，`(then (amb (amb 'a 'b) 'c))` 的事件是 `(pause C B A)`，`(then (amb-local (amb (amb 'a 'b) 'c)))` 是 `(pause B C A)`，后者与 `(then (amb 'a (amb 'b 'c)))` 相同：局部搜索把括号抹掉了。Astra 的例子见 `tests/kanren.rkt`：direct 下分配律、结合律两侧都是 `(C B A)`；`local` 下重现原书的 `(A B C)` / `(C B A)`。

**平坦的前沿与结合律。** 准确的陈述是命题一：只看叶子的序列就能算出结果，当且仅当 merge 结合（要随意增删空分支，再加双侧单位元）。`append`、`level` 是幺半群，所以一堆嵌套的 merge 可以写成一条序列，内核里它们的记录就是那个栈和那个按轮的队列。`mplus` 不结合，同样的叶子序列在不同括号下结果不同，所以调度状态里必须带着分组的信息；「给选择点一个局部前沿」是带这份信息的一种办法，本方案的办法是不物化它：`(amb-interleave s t)` 这个闭包就是一个两元素的局部前沿。定理不要求这份信息是一棵树，也不给更新代价的下界。

## 4. 与 miniKanren-typed、ambKanren 的比较

[`kanren.rkt`](kanren.rkt) 照着 ambKanren 的 `typed/miniKanren/amb/trs2` 写，去掉了类型；实质的不同只在 "Goals" 一节。`book/` 把 miniKanren-typed 的书中例子（第 1–9 章，285 个 `check-equal?`）逐个运行并记录。Racket 9.3 CS，2 核沙箱，单次运行；两行 ambKanren 是和别的任务同时跑的，时间只能粗看：

| 实现 | same | 仅顺序不同 | 有界 run 取到别的答案 | 超时 | 总耗时 |
|---|--:|--:|--:|--:|--:|
| miniKanren-typed TRS2（原书的流） | 285 | | | | 12.0 s |
| 内核，`interleave`，`conj` = 局部 | 285 | | | | 11.7 s |
| **内核，`interleave`，`conj` = 复合** | **283** | 2 | | | **11.1 s** |
| 内核，`alternate` | 278 | 7 | | | 12.1 s |
| 内核，`level` | 270 | 9 | 6 | | 269.3 s |
| ambKanren master（全局前沿） | 270 | 9 | 6 | | 353.0 s |
| ambKanren `lawful-search` | 270 | 9 | 6 | | 373.5 s |

- `interleave` + 复合与原书不同的两例是 ch8:205（`logo`，`run 9`，九个答案相同、顺序不同）和 ch9:167（`enumerate+o`）。把 `conj` 换成局部后两例都相同，所以差别完全来自 `bind` 的规范化。
- `level` 与 ambKanren master：285 例中 **284 例逐答案相同**（不只是分类相同），耗时最长的 ch8:205 也是同一个结果（244 s 对 298 s）。唯一不同的是 ch9:167（`enumerate+o`，里面有 `onceo`）。`level` 与 `lawful-search` 有两例不同（ch9:167、ch9:186，都含 `onceo`）。三者的差别只在 `once` 怎样把测试的暂停计入外层：master 不计（内层搜索自己消化）；`lawful-search` 是内层每个线程每暂停一次，外层 `suspend` 一次；内核是内层所有线程一起暂停一次，外层暂停一次，于是 `once` 的答案排在「它在测试里的层数」那一层。
- `very-recursiveo` 取 10⁶ 个答案（ch6:104）：流 3.6 s，内核 `interleave` 3.4 s（局部 `conj` 3.1 s），内核 `level` 22 s，ambKanren master 50 s。`VERSIONS.md` 里未融合的 reify/reflect 版本在这一项上是超线性的；这里每次暂停都经过效应，没有做融合。
- TRS1（`kanren1.rkt`，`book/trs1.rkt`）：第一版书中例子（第 1–8、10 章）228 个全同，9.9 s。`conde`/`condi` 是带 `amb-append`/`amb-alternate` 的选择，`all`/`alli` 是 `(amb-reflect (amb-reify …) merge)`，各一行；ambKanren TRS1 里用队列和 promise 写的 `mplusi`/`bindi` 不需要了。
- issue #14 的对照程序（C1–C7、R1–R3）和 `lawful-search` 的 `tests/search.rkt`（`once`/`ifte` 转发暂停、Astra 的两组律）：见 `tests/kanren.rkt`。公平性和 `once`/`ifte` 的各项，三种公平策略在复合与局部两种 `conj` 下结果都与原书相同。
- 普通 amb 程序（`bench/amb.rkt`，5 次的中位数，ms）。「方程」一列是同一个内核换用照方程写的 merge，也就是加快路径之前的样子：

  | 深度优先 | master | 内核 | 方程 |
  |---|--:|--:|--:|
  | 宽：200000 个备选 | 67 | 48 | 32 |
  | 勾股数 < 60 | 16 | 13 | 14 |
  | 8 皇后，全部解 | 10 | 7 | 7 |
  | 深：2^18 个叶子，18 个开着的选择点 | 338 | 204 | 157 |
  | 左嵌套，4000 层 | 4 | 2 | 89 |
  | 左嵌套，8000 层 | 8 | 5 | 483 |
  | 左嵌套，16000 层 | 23 | 18 | 2112 |
  | 8000 个选择点连成的链 | 9 | 8 | 7 |

  | 广度优先（`level`） | master | 内核 | 方程 |
  |---|--:|--:|--:|
  | 深：2^14 个叶子（每个选择点暂停一次） | 27 | 24 | 20 |
  | 250 条流 × 20 轮 | | 5 | 15 |
  | 500 条流 × 20 轮 | | 12 | 49 |
  | 1000 条流 × 20 轮 | | 26 | 137 |

  左嵌套由平方变为线性，与 master 相当。「合并了再合并」（Astra 的单遍例子）的数字在命题三下面。可拼接表有常数代价：「深 2^18」一行上一版是内核 157、方程 175，这一版是 204 对 157，也就是嵌套只有 18 层时，快路径现在比照方程写的慢约三成（仍比 master 快）。个位数、十几毫秒的几行只能看量级。
- 书中例子的 `level` 一行基本不受快路径影响（加快路径之前 303 s，上一版 297 s，换成可拼接表后单独重跑 269 s，与别的任务同时跑的一次是 354 s；285 例结果始终逐项相同）：那里的时间花在这种顺序本身要跑的线程步数上，不在 merge 的嵌套上，所以用扁平 deque 的 ambKanren master 也是同一个量级。这几个数的差别在噪声之内。

master 的两个策略参数（`tests/master-flags.rkt`）：`current-amb-depth-first? #f` 等于「每个选择点先暂停一次」加 `amb-level`，在 master 自己的测试上轨迹相同。`current-amb-fair? #t` 说的是树在全局 deque 里的位置（按时间先后），不是一种 merge：master 测试里的两个例子分别需要两种不同的局部解释。

## 5. 验证了什么，没有证明什么

**Astra 证明了的**（对 `1ac7057`，命题二、三另对 `32acf32`；以 `model.rkt` 的四个 merge 和 `spec` 为准；数学证明或逐分支模拟，没有机器验证；出处是 #22 的[审阅](https://github.com/NoahStoryM/amb/issues/22#issuecomment-6821738392)）：

- 模型：流是 `νX. 1 + (V × X) + X`；选择是按策略标记的代数操作，由 merge 解释，`(merge (k a) (λ () (k b)))` 对不结合的 merge 也成立；`local` 是带作用域的操作，后续不能移进作用域（第 3 节的 `pause C B A` / `pause B C A` 是见证）。没有给 Racket 的可变状态、参数、`dynamic-wind` 建模。
- 四种 merge 的双侧单位律；`append`、`level` 的结合律（含无穷的流）；`interleave`、`alternate` 不结合的反例。
- 命题一（忘掉括号的充要条件）。
- 命题二：对 `1ac7057` 的快路径逐分支模拟，对 `32acf32` 的新表示复核。
- 命题三：端到端的 O(M + R) 均摊界（复核 `32acf32`；论证是我起的，前提和计费口径按她的意见改）。
- 三种公平策略的完备性，前提见第 2 节。
- `(amb)` 是零元、暂停是一元构造子，两者分开时四种策略都守单位律。

**有论证、没有审过的**（我的）：

- 直接风格下合取的结合律、右分配律、单位律（在 `spec` 上按定义展开，两行，第 3 节）。Astra 的第 1 问从自由代数的折叠得到同一个式子。
- `amb-level` 与 ambKanren 全局前沿的顺序相同（书中例子 284/285 逐答案相同，加上论证；没有形式化）。

**只有测试的**：

- 快路径的随机测试和改错测试：`tests/flat.rkt` 4 组各 5000 个随机表达式，比较事件和强制时刻；`tests/model.rkt`。对内核和 `catenable.rkt` 做过 12 处故意改错（两个表的拼接顺序、丢掉内层已暂停的线程、把已暂停的线程排到前面、提前恢复、丢暂停、队列不翻转、链接的顺序、`cat-append` 的两个参数对调、`amb-events` 多强制一步等），每一处都至少被 `tests/catenable.rkt`、`tests/kernel.rkt`、`tests/flat.rkt`、`tests/model.rkt` 中的一个发现。
- `direct = spec`；`direct + 每个合取局部 = book`；`book(interleave)` = `trs2-impl.scm` 的 `append-inf`/`append-map-inf`。各 3000 个随机有限目标，比较完整事件序列。`spec`、`book` 用照方程写的 merge，`direct` 用内核的。
- 第 3 节的规律表：成立的格子是 2400 × 2 次随机试验未见反例（其中析取的单位律、结合律两行现在有 Astra 的证明）；不成立的格子有具体反例，是确定的。bag 一级的格子只对表里用到的上下文成立。
- TRS2 285 例、TRS1 228 例、issue #14 的对照程序（期望值与 miniKanren-typed 的输出核对过）、`pause-escape` 反例、公平性表。换成可拼接表之后，`level` 的 285 例和 TRS1 的 228 例重跑过，结果与之前逐项相同。

**没有的**：

- 任何机器验证。
- `interleave`、`alternate` 有没有比「每个事件穿过所有开着的 merge」更便宜的表示：没有研究。命题一不禁止。
- `dynamic-wind`、异常、`parameterize` 与被多次恢复的续延的交互没有测试（master 也有同样的问题面）。

## 6. 是否更容易理解，代价是什么

我的判断：**是，前提是接受「搜索结果是一条带暂停的流」这个视角。** 概念只有四个（事件流、三个效应、merge、reify/reflect），每个策略是一条方程，规格是一个十几行的解释器，合取的规律可以用展开证明而不是靠测试。没有 frontier、没有树结构体上的策略标志、没有挂在 continuation mark 上的 handler、没有「入队位置」的推理。

与 PR #10 的比较：我没有读它的代码，只看了标题和规模（PR #10 `local-frontier`：15 个文件，+1674 −190，其中 `private/amb.rkt` 改动 536 行）。两者的对应关系见 #22 第 1–3 点（不是我做的对照）：#10 的 `amb/local` 就是这里的 `amb` 配 `amb-interleave`，它物化的 frontier 树就是 merge 闭包组成的树，只有多路选择不同。本目录的内核是 406 行（代码 194 行），加上 `catenable.rkt` 96 行（代码 44 行）。两边功能不对等（见下面的代价 1）。

与 ambKanren 已有研究的关系：`research/local-search-20261109` 的 `trs2-shift` 已经用 shift/reset 把目标 reify 成流，这里不是全新的想法。区别在 merge 放在哪里。`trs2-shift` 的 `disj2` 先把两个分支各自 reify（不带续延），合并之后再用 `append-map-inf` 把续延接回去，所以得到原书的顺序，也得到原书缺的规律，并且每一层析取都多一次 reify/reflect。这里的选择点直接合并带着续延的两个分支，`(merge (k a) (λ () (k b)))`：不需要 reflect，规律成立，Kanren 一侧只剩 `amb` 和函数复合，而 merge 作为 amb 的参数对 Kanren 以外的程序同样可用。

代价：

1. **没有实现 master 的全部 API。** `for/amb`（有无 `#:length`）、`for*/amb`、shuffler 有了原型（[`CHOICES.md`](CHOICES.md)）；没有 `#:fill`、contract、Typed 包装、文档。
2. **流不记忆。** 每个 thunk 只应强制一次（单遍，和 `in-amb/do` 一样）；重复强制会重跑续延（`tests/kernel.rkt` "a stream is single-pass"），命题三的代价界也以单遍为前提。公开接口需要在边界上包一层记忆化的流。
3. **参数在强制流的地方读取。** 搜索是惰性的，所以包在搜索外面的 `(parameterize ([current-amb-merge …]) (amb-reify …))` 只管到第一个事件；之后的选择读到的是强制流那一刻的值。写在搜索里面的 `parameterize` 属于续延，没有这个问题。master 的 `in-amb` 对它的参数也是这样（同一测试文件 "a parameter is read where the stream is forced"）。本目录的测试都在 `parameterize` 之内把流取完。
4. **`interleave`、`alternate` 的事件仍要经过它上方所有开着的 merge。** 它们照方程写，一次暂停要让路径上每一层都换边（流实现也一样）。这是现在这个实现的代价，**不是下界**：命题一只禁止从不带标注的叶子序列恢复结果，带着分组信息的更便宜的表示也许存在，我没有找。`append`、`level` 是扁平的，代价见命题三；为此多了 `catenable.rkt` 的 44 行和 `racket/promise`。
5. **`current-amb-fair?` 没有对应物**，`current-amb-depth-first? #f` 要改写成暂停。若用内核替换驱动器，这两个参数的语义要重新定。
6. **`interleave` 的指数级份额偏置**原样继承自 miniKanren。
7. 与原书仍有 2/285 的顺序差别（要全同就用局部 `conj`，代价是规律）。

## 7. 如果要并入 amb，需要你决定的事

Astra 已经给了建议的两项：

- **`(amb)` 固定为失败，让出用显式的暂停**（第 7 问）。空选择若改成「发一个暂停」，`P 0 ≠ 0`，单位律就没有了；可重解释的 empty-handler 可以是另一套效应接口。本目录已经是这样。
- **`amb` 继续右嵌套**（第 5 问），保证 `(amb a b c)` 这个缩写可以展开，便于解释和复现 TRS2。若要 n 个同级分支等轮次，另给明确的 n 路选择 / 策略接口，它的二元特例与相应的二路策略一致。注意 n 路节点再嵌套 n 路节点仍然有分组，除非接口明确规定合并调度域；n 路的 `interleave`（各自暂停时发出该暂停并轮转）与 `level`（一轮的暂停合成一个）也不是一回事。n 路接口的原型和两种 n 路策略见 [`CHOICES.md`](CHOICES.md)。

还开着的：

- 内核是**替换**现有驱动器，还是作为第二种搜索并存。替换的话，`current-amb-depth-first?`/`current-amb-fair?` 和 `#:empty-handler` 怎么处置。
- 事件流本身是否公开（`amb-reify`），还是只公开 `amb-await`/`amb-once`/`amb-local` 这类组合子和 `in-amb` 序列。
- 自定义 merge 是否公开；公开的话，文档要写明「公开自定义 merge 的最小约束」一节的内容。
- 默认 merge（现在是 `amb-append`，与 master 的默认行为一致）。
- 策略怎样指定：现在是选择时读取的参数（可在一次搜索里混用，但有代价 3 的注意点）；另一种是每次搜索固定一个 merge，由 `in-amb` 接收。

## 文件

| 文件 | 内容 |
|---|---|
| `kernel.rkt` | 内核：效应、四种 merge（结合的两种是扁平的）、reify/reflect、观察流的工具 |
| `nary.rkt` | 两种 n 路策略：`amb-rotate`、`amb-dovetail` |
| `CHOICES.md` | 选择的形式：向量、生成器、shuffler、n 路策略（接口提案） |
| `catenable.rkt` | 常数时间拼接的表（Okasaki），快路径的两个待办列表 |
| `model.rkt` | 规格：流、四种 merge 的定义方程、小目标语言的 `spec`（显式续延）；以及 `direct`（内核）、`book`（流 + bind）、`trs2`（原书代码） |
| `kanren.rkt` / `kanren1.rkt` | TRS2 / TRS1 的语言，`Goal : Substitution → Substitution` |
| `tests/kernel.rkt` | 内核的例子：效应、策略的定义方程、括号的反例、轮次份额、局部搜索、观察器不多强制 |
| `tests/model.rkt` | `direct = spec`，`direct + local = book`，`book = trs2` |
| `tests/choices.rkt` | `amb`/`amb*`/`amb-from`/`for/amb` 的展开、shuffler、槽位释放、n 路策略的轨迹 |
| `tests/flat.rkt` | 内核的 merge = 定义方程：事件与强制时刻；合并了再合并的例子及其步数 |
| `tests/catenable.rkt` | 可拼接表：内容，以及一段操作序列的平均步数 |
| `tests/laws.rkt` | 规律表与最小反例 |
| `tests/fairness.rkt` | 公平性表，等待时间 |
| `tests/kanren.rkt` | issue #14 的对照程序、`once`/`ifte`、Astra 的例子 |
| `tests/master-flags.rkt` | master 的两个策略参数与 merge 的关系 |
| `book/` | 书中例子逐项记录；`results/*.rktd` 是本次运行的记录，`compare.rkt` 汇总并两两比较 |
| `bench/amb.rkt` | master、内核、照方程写的 merge：深度优先、广度优先、合并了再合并 |

## 复现

Racket 9.3 CS；`raco pkg install --batch --deps force --link` 安装 amb、typed-amb、miniKanren-typed（`c197a5c`）、typed-data-queue、ambKanren（master `ad2586d`；`ambkanren-lawful` 的记录来自 `lawful-search` `a8c90bf`）。

```sh
cd research/choice-as-merge
raco test tests                         # 约 18 秒；只需要 amb（master-flags 用）和 rackunit
cd book
VARIANT=interleave racket kernel.rkt    # 也可为 interleave+local、level、alternate
racket reference.rkt                    # miniKanren-typed
racket ambkanren.rkt                    # 已安装的 ambKanren，约 6 分钟
racket trs1.rkt
racket compare.rkt
cd ../bench && racket amb.rkt
```
