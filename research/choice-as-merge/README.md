# 选择即合并：搜索是从计算到事件流的折叠

研究分支 `research/choice-as-merge-20261112`，基于 amb master `6340911`。只新增本目录，不改动 `pkgs/`。讨论在 [#22](https://github.com/NoahStoryM/amb/issues/22)。

按约定，这一轮**没有阅读** PR #10–21（`local-frontier`、`split/*` 分支）的实现代码，只看了它们的标题和 diff 行数；ambKanren 的 `research/local-frontier-20261110` 是那套接口的使用方，也没有读。读过的是：amb master、ambKanren master / `lawful-search` / `research/local-search-20261109`、miniKanren-typed master。ambKanren issue #14 的正文我这里打不开，对它的了解来自研究分支里的 `README.md`、`VERSIONS.md` 和各个 `.out` 文件。

## 结论

1. **直接风格的公平搜索只需要一个新效应和一个参数。** 新效应是 `pause`（暂停）；参数是每个选择点上的 `merge`：一个把两条事件流合成一条的二元运算。frontier 不必作为数据结构出现：它就是流表达式里还没求值的那部分。
2. 内核 [`kernel.rkt`](kernel.rkt) 一共 127 行代码（不计注释），其中控制部分是四个定义、十来行：就是用 `shift`/`reset` 写非确定性的教科书写法 `(shift k (append (k a) (k b)))`，把列表换成带暂停的惰性流，把 `append` 换成参数。四种 merge 的定义方程各五六行，写在规格 [`model.rkt`](model.rkt) 里；内核里结合的两种（`append`、`level`）走扁平的快路径，共约 45 行。
3. `disj = amb`、`conj = 函数复合`、`defrel = 先 pause`，配上 `amb-interleave`（即 `append-inf` / `mplus`），TRS2 书中 285 个例子有 **283 个与原书逐答案相同**，耗时与 miniKanren-typed 的流实现持平（11.1 s 对 12.0 s）；ambKanren master 是 270 个、353 s。
4. 把 `conj` 改成「先局部搜索第一个目标」（一行），就是原书的 `bind`：285 个全同。所以原书与直接风格的差别**只有这一处**，而合取的结合律、分配律也正是在这一处丢掉的。
5. 换成 `amb-level`，得到的就是 ambKanren 现在的全局前沿顺序（285 例中 284 例逐答案相同，另一例的原因见下）。所以「全局前沿」不是另一种机制，而是一种 merge。
6. 复杂性确有不可避免的部分，但比预想的小：**merge 不结合时，调度状态必须是树**（`mplus` 就不结合），所以想要书上的交错就一定要有「局部」结构；这棵树可以不写出来，留在流的闭包里。merge 结合时（`append`、`level`）状态才能压平成栈或队列，master 的 deque 是这种情形。

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
  ((shift-at tag k
     (let loop ([alts alts])
       (cond [(null? alts) '()]
             [(null? (cdr alts)) (k (car alts))]
             [else (merge (k (car alts)) (λ () (loop (cdr alts))))])))))
```

`(amb e ...)` 是 `(amb* (current-amb-merge) (list (λ () e) ...))`。策略属于选择点（在选择时读取参数），同一次搜索里可以混用（惰性带来的一个注意点见第 6 节代价 3）。

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

**结合的 merge 走扁平的快路径**（#22 第 4 点）。照方程写的 merge 是包着 `s` 的一个闭包，`s` 的每个事件都要穿过它上方每一个还开着的选择点：n 层嵌套，每个答案 n 步。结合律 `(s ∨ t) ∨ u = s ∨ (t ∨ u)` 说括号无关紧要，所以没做完的流可以记成一条「还剩什么」的记录，记录套记录时在强制的那一刻并成一条：

```racket
(struct appended (s ts) …)      ; (s)，然后是 thunk 列表 ts 的各条流             —— 栈
(struct leveled (ws s ts) …)    ; 一轮里：ws 已暂停，(s) 正在跑，ts 还没跑；然后暂停一次，下一轮
```

两个记录都带 `prop:procedure`，仍然是 thunk。别的 merge、`amb-await`、`in-amb` 只通过事件观察流，不需要知道它们；一种策略的记录也就自然止于另一种策略的选择点（那里是不透明的闭包）。所以压平的范围是「同一种结合策略、沿第一个参数直接嵌套」的极大段，用到的只有这一条结合律，对混用策略的搜索不需要别的条件。`amb-level` 的结合律有一个简单的论证：把流按暂停切成轮，`level` 就是逐轮拼接（短的一边补空轮），拼接是结合的。

`tests/flat.rkt` 在随机的流表达式上（左深为主，四种策略混用，中途取走若干事件后再合并）检查内核的 merge 与方程逐事件相同，并且各个 thunk 被强制的时刻也相同（不能提前运行还没轮到的流）；`tests/model.rkt` 里 `spec` 用方程、`direct` 用内核，每个析取随机指定策略。

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

**规格。** [`model.rkt`](model.rkt) 的 `spec` 是不用控制算子的同一语义：每个目标显式接收续延 `k : State → Stream`，十几行；四种 merge 照方程写。它不使用 `kernel.rkt` 的任何东西。`tests/model.rkt` 检查 `direct = spec`，事件序列（含暂停位置）完全相同：4 种策略各 3000 个随机目标（含 `local`/`once`/`ifte`），再加每个析取各自随机指定策略的 3000 个。

## 2. 公平必须保证什么，什么只是策略

**必须的**（任何顺序执行的搜索都绕不开）：

- **让出点。** 驱动器只有在分支交出控制时才能换人，所以必须有某个操作产生「非答案事件」。它可以是独立的 `pause`，也可以并进选择里：master 的 `current-amb-depth-first? #f` 就是「每个选择点都让出一次」（`tests/master-flags.rkt`）。我选独立的效应，因为在哪里让出和在哪里选择是两件事：`defrel` 让出而不选择，`conde` 选择而不让出。
- **轮到。** 每个还活着的分支都要被无限次轮到。
- **局部搜索要转发暂停。** 否则内层搜索对外层是一步，`(conde [(once (nevero))] [(== q 1)])` 就卡住。

**只是策略的**：谁先走、每个分支分到多少、嵌套的括号是否看得出来、答案的顺序。`tests/kernel.rkt` "turns among three alternatives"：三个分支的 `(amb a b c)`，`amb-interleave` 的份额是 1/2、1/4、1/4（多路选择是右嵌套的二路选择），`amb-level` 是各 1/3。两者都公平。

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

最后一行是合取：`(begin (nevero) 'x)` 里 x 不在任何分支上，调度器无能为力（要解决它得并发地运行合取的两边，那是另一个问题）。第三行说明 `interleave`/`level` 的公平性以「无限路径上有无限次暂停」为前提，`defrel` 保证了这一点；对 amb 的一般用户，`(amb-from (in-naturals))` 这种不暂停的无限选择仍会饿死右边的兄弟。

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

**析取的规律只取决于 merge。** `fail` 是单位元对四种都成立；结合律对 `append`、`level` 成立，对 `interleave`、`alternate` 不成立；交换律都不成立（只在「答案的多重集」意义下成立）。

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

**平坦的前沿需要结合律。** merge 结合且有单位元时，一堆嵌套的 merge 可以写成一个序列，调度状态就是栈（`append`）或队列（`level`）。merge 不结合时，同样的线程序列在不同括号下行为不同，状态必须保留括号，也就是一棵树。`mplus` 不结合，所以「给选择点一个局部前沿」这个方向对它是必然的；本方案的区别只是不把这棵树物化：`(amb-interleave s t)` 这个闭包就是一个两元素的局部前沿。反过来，内核里 `append`、`level` 的记录就是那个栈和那个队列（第 1 节）。

## 4. 与 miniKanren-typed、ambKanren 的比较

[`kanren.rkt`](kanren.rkt) 照着 ambKanren 的 `typed/miniKanren/amb/trs2` 写，去掉了类型；实质的不同只在 "Goals" 一节。`book/` 把 miniKanren-typed 的书中例子（第 1–9 章，285 个 `check-equal?`）逐个运行并记录。Racket 9.3 CS，2 核沙箱，单次运行；两行 ambKanren 是和别的任务同时跑的，时间只能粗看：

| 实现 | same | 仅顺序不同 | 有界 run 取到别的答案 | 超时 | 总耗时 |
|---|--:|--:|--:|--:|--:|
| miniKanren-typed TRS2（原书的流） | 285 | | | | 12.0 s |
| 内核，`interleave`，`conj` = 局部 | 285 | | | | 11.7 s |
| **内核，`interleave`，`conj` = 复合** | **283** | 2 | | | **11.1 s** |
| 内核，`alternate` | 278 | 7 | | | 12.1 s |
| 内核，`level` | 270 | 9 | 6 | | 296.7 s |
| ambKanren master（全局前沿） | 270 | 9 | 6 | | 353.0 s |
| ambKanren `lawful-search` | 270 | 9 | 6 | | 373.5 s |

- `interleave` + 复合与原书不同的两例是 ch8:205（`logo`，`run 9`，九个答案相同、顺序不同）和 ch9:167（`enumerate+o`）。把 `conj` 换成局部后两例都相同，所以差别完全来自 `bind` 的规范化。
- `level` 与 ambKanren master：285 例中 **284 例逐答案相同**（不只是分类相同），耗时最长的 ch8:205 也是同一个结果（271 s 对 298 s）。唯一不同的是 ch9:167（`enumerate+o`，里面有 `onceo`）。`level` 与 `lawful-search` 有两例不同（ch9:167、ch9:186，都含 `onceo`）。三者的差别只在 `once` 怎样把测试的暂停计入外层：master 不计（内层搜索自己消化）；`lawful-search` 是内层每个线程每暂停一次，外层 `suspend` 一次；内核是内层所有线程一起暂停一次，外层暂停一次，于是 `once` 的答案排在「它在测试里的层数」那一层。
- `very-recursiveo` 取 10⁶ 个答案（ch6:104）：流 3.6 s，内核 `interleave` 3.4 s（局部 `conj` 3.1 s），内核 `level` 22 s，ambKanren master 50 s。`VERSIONS.md` 里未融合的 reify/reflect 版本在这一项上是超线性的；这里每次暂停都经过效应，没有做融合。
- TRS1（`kanren1.rkt`，`book/trs1.rkt`）：第一版书中例子（第 1–8、10 章）228 个全同，9.6 s。`conde`/`condi` 是带 `amb-append`/`amb-alternate` 的选择，`all`/`alli` 是 `(amb-reflect (amb-reify …) merge)`，各一行；ambKanren TRS1 里用队列和 promise 写的 `mplusi`/`bindi` 不需要了。
- issue #14 的对照程序（C1–C7、R1–R3）和 `lawful-search` 的 `tests/search.rkt`（`once`/`ifte` 转发暂停、Astra 的两组律）：见 `tests/kanren.rkt`。公平性和 `once`/`ifte` 的各项，三种公平策略在复合与局部两种 `conj` 下结果都与原书相同。
- 普通 amb 程序（`bench/amb.rkt`，5 次的中位数，ms）。「方程」一列是同一个内核换用照方程写的 merge，也就是加快路径之前的样子：

  | 深度优先 | master | 内核 | 方程 |
  |---|--:|--:|--:|
  | 宽：200000 个备选 | 96 | 40 | 28 |
  | 勾股数 < 60 | 15 | 12 | 16 |
  | 8 皇后，全部解 | 7 | 8 | 8 |
  | 深：2^18 个叶子，18 个开着的选择点 | 392 | 157 | 175 |
  | 左嵌套，4000 层 | 4 | 3 | 105 |
  | 左嵌套，8000 层 | 12 | 9 | 437 |
  | 左嵌套，16000 层 | 26 | 20 | 1854 |

  | 广度优先（`level`） | master | 内核 | 方程 |
  |---|--:|--:|--:|
  | 深：2^14 个叶子（每个选择点暂停一次） | 23 | 19 | 32 |
  | 250 条流 × 20 轮 | | 4 | 13 |
  | 500 条流 × 20 轮 | | 10 | 44 |
  | 1000 条流 × 20 轮 | | 22 | 159 |

  左嵌套由平方变为线性，与 master 相当。前三行比上一版快，是因为 `amb-from` 改成了只捕获一次续延的单个选择点（原来每个元素捕获一次），与快路径无关。个位数毫秒的几行只能看量级。
- 书中例子的 `level` 一行没有因为快路径变快（303 s → 297 s，285 例结果与之前逐项相同）：那里的时间花在这种顺序本身要跑的线程步数上，不在 merge 的嵌套上，所以用扁平 deque 的 ambKanren master 也是同一个量级。

master 的两个策略参数（`tests/master-flags.rkt`）：`current-amb-depth-first? #f` 等于「每个选择点先暂停一次」加 `amb-level`，在 master 自己的测试上轨迹相同。`current-amb-fair? #t` 说的是树在全局 deque 里的位置（按时间先后），不是一种 merge：master 测试里的两个例子分别需要两种不同的局部解释。

## 5. 验证了什么，没有证明什么

验证过（测试，不是证明）：

- `direct = spec`；`direct + 每个合取局部 = book`；`book(interleave)` = `trs2-impl.scm` 的 `append-inf`/`append-map-inf`。各 3000 个随机有限目标，比较完整事件序列。`spec`、`book` 用照方程写的 merge，`direct` 用内核的。
- 内核的四个 merge 与方程逐事件相同、强制时刻相同（`tests/flat.rkt`，4 组各 5000 个随机表达式）。对快路径做过 7 处故意改错（两个列表的拼接顺序、漏掉 `reverse`、丢掉内层已暂停的线程、提前运行、丢暂停），`tests/flat.rkt` 和 `tests/model.rkt` 各自都能发现全部 7 处。
- 上面的规律表：成立的格子是 2400 × 2 次随机试验未见反例；不成立的格子有具体反例，是确定的。
- TRS2 285 例、TRS1 228 例、issue #14 的对照程序（期望值与 miniKanren-typed 的输出核对过）、`pause-escape` 反例、公平性表。

有论证的：直接风格下合取的结合律、右分配律、单位律（在 `spec` 上按定义展开，两行）；`'() ∨ t = t`；`amb-append`、`amb-level` 的结合律（后者按轮分解，见第 1 节）。这些是论证，不是形式化的证明，对无穷的流尤其如此。

没有证明的：

- 各 merge 的右单位律（只有随机测试）。
- 快路径的代价是「每个事件均摊常数步，`level` 另加每轮与活着的线程数成正比」：有基准和论证，没有证明；把一条已经很长的记录当作一等的流反复再合并，会多付拼接列表的代价。
- 三种公平策略对「每条无限路径有无限次暂停」的树是完备的（只有例子和上面的论证）。
- `amb-level` 与全局前沿顺序相同（书中例子和论证；没有形式化）。
- 「平坦前沿 ⇔ merge 结合」是论证，不是定理。
- `dynamic-wind`、异常、`parameterize` 与被多次恢复的续延的交互没有测试（master 也有同样的问题面）。

## 6. 是否更容易理解，代价是什么

我的判断：**是，前提是接受「搜索结果是一条带暂停的流」这个视角。** 概念只有四个（事件流、三个效应、merge、reify/reflect），每个策略是一条方程，规格是一个十几行的解释器，合取的规律可以用展开证明而不是靠测试。没有 frontier、没有树结构体上的策略标志、没有挂在 continuation mark 上的 handler、没有「入队位置」的推理。

与 PR #10 的比较：我没有读它的代码，只看了标题和规模（PR #10 `local-frontier`：15 个文件，+1674 −190，其中 `private/amb.rkt` 改动 536 行）。两者的对应关系见 #22 第 1–3 点（不是我做的对照）：#10 的 `amb/local` 就是这里的 `amb` 配 `amb-interleave`，它物化的 frontier 树就是 merge 闭包组成的树，只有多路选择不同。本目录的内核是 289 行（代码 127 行）。两边功能不对等（见下面的代价 1）。

与 ambKanren 已有研究的关系：`research/local-search-20261109` 的 `trs2-shift` 已经用 shift/reset 把目标 reify 成流，这里不是全新的想法。区别在 merge 放在哪里。`trs2-shift` 的 `disj2` 先把两个分支各自 reify（不带续延），合并之后再用 `append-map-inf` 把续延接回去，所以得到原书的顺序，也得到原书缺的规律，并且每一层析取都多一次 reify/reflect。这里的选择点直接合并带着续延的两个分支，`(merge (k a) (λ () (k b)))`：不需要 reflect，规律成立，Kanren 一侧只剩 `amb` 和函数复合，而 merge 作为 amb 的参数对 Kanren 以外的程序同样可用。

代价：

1. **没有实现 master 的全部 API。** 没有 `for/amb #:length`、shuffler、contract、Typed 包装、文档；`amb-from` 是 `for/amb` 的简化替身。
2. **流不记忆。** 每个 thunk 只应强制一次（单遍，和 `in-amb/do` 一样）；重复强制会重跑续延（`tests/kernel.rkt` "a stream is single-pass"）。公开接口需要在边界上包一层记忆化的流。
3. **参数在强制流的地方读取。** 搜索是惰性的，所以包在搜索外面的 `(parameterize ([current-amb-merge …]) (amb-reify …))` 只管到第一个事件；之后的选择读到的是强制流那一刻的值。写在搜索里面的 `parameterize` 属于续延，没有这个问题。master 的 `in-amb` 对它的参数也是这样（同一测试文件 "a parameter is read where the stream is forced"）。本目录的测试都在 `parameterize` 之内把流取完。
4. **`interleave`、`alternate` 的事件仍要经过它上方所有开着的 merge。** 它们不结合，括号是状态的一部分：一次暂停要让路径上每一层都换边，这一步省不掉（流实现也一样）。`append`、`level` 已经是扁平的（第 1 节）。
5. **`current-amb-fair?` 没有对应物**，`current-amb-depth-first? #f` 要改写成暂停。若用内核替换驱动器，这两个参数的语义要重新定。
6. **`interleave` 的指数级份额偏置**原样继承自 miniKanren。
7. 与原书仍有 2/285 的顺序差别（要全同就用局部 `conj`，代价是规律）。

## 7. 如果要并入 amb，需要你决定的事

- 内核是**替换**现有驱动器，还是作为第二种搜索并存。替换的话，`current-amb-depth-first?`/`current-amb-fair?` 和 `#:empty-handler` 怎么处置。
- 事件流本身是否公开（`amb-reify`），还是只公开 `amb-await`/`amb-once`/`amb-local` 这类组合子和 `in-amb` 序列。
- 默认 merge（现在是 `amb-append`，与 master 的默认行为一致）。
- 策略怎样指定：现在是选择时读取的参数（可在一次搜索里混用，但有代价 3 的注意点）；另一种是每次搜索固定一个 merge，由 `in-amb` 接收。
- 是否要多路的 merge（让 `(amb a b c)` 平分轮次而不经过 `level`）。

## 文件

| 文件 | 内容 |
|---|---|
| `kernel.rkt` | 内核：效应、四种 merge（结合的两种是扁平的）、reify/reflect、观察流的工具 |
| `model.rkt` | 规格：流、四种 merge 的定义方程、小目标语言的 `spec`（显式续延）；以及 `direct`（内核）、`book`（流 + bind）、`trs2`（原书代码） |
| `kanren.rkt` / `kanren1.rkt` | TRS2 / TRS1 的语言，`Goal : Substitution → Substitution` |
| `tests/kernel.rkt` | 内核的例子：效应、策略的定义方程、轮次份额、局部搜索 |
| `tests/model.rkt` | `direct = spec`，`direct + local = book`，`book = trs2` |
| `tests/flat.rkt` | 内核的 merge = 定义方程：事件与强制时刻 |
| `tests/laws.rkt` | 规律表与最小反例 |
| `tests/fairness.rkt` | 公平性表，等待时间 |
| `tests/kanren.rkt` | issue #14 的对照程序、`once`/`ifte`、Astra 的例子 |
| `tests/master-flags.rkt` | master 的两个策略参数与 merge 的关系 |
| `book/` | 书中例子逐项记录；`results/*.rktd` 是本次运行的记录，`compare.rkt` 汇总并两两比较 |
| `bench/amb.rkt` | master、内核、照方程写的 merge：深度优先与广度优先 |

## 复现

Racket 9.3 CS；`raco pkg install --batch --deps force --link` 安装 amb、typed-amb、miniKanren-typed（`c197a5c`）、typed-data-queue、ambKanren（master `ad2586d`；`ambkanren-lawful` 的记录来自 `lawful-search` `a8c90bf`）。

```sh
cd research/choice-as-merge
raco test tests                         # 约 15 秒；只需要 amb（master-flags 用）和 rackunit
cd book
VARIANT=interleave racket kernel.rkt    # 也可为 interleave+local、level、alternate
racket reference.rkt                    # miniKanren-typed
racket ambkanren.rkt                    # 已安装的 ambKanren，约 6 分钟
racket trs1.rkt
racket compare.rkt
cd ../bench && racket amb.rkt
```
