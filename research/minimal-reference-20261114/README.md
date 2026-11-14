# amb v3：可执行的最小参考语义

Noah 后续调整的、与 v2 风格对齐的自然数暂停 demo 见 [DEMO.md](DEMO.md) 和 [demo.rkt](demo.rkt)；以下内容记录前一个参考模型。

先读 [DESIGN.md](DESIGN.md)，再读 [kernel.rkt](kernel.rkt)。阶段 A 的设计在实现前单独提交；本目录是阶段 B/C 的实现与验证，不替换正式包或前两版研究目录。

内核只有一个文件，按协议、merge、choice、sequence 边界、派生语法排列；没有公开的 Stream 抽象、reify/reflect 层、优化记录、catenable 队列或暂停聚合。raw generator 是 Racket 的过程与两值返回，不是另外一种数据结构。

## 读核心定义

选择点只捕获一次后续，读取并固定当前 merge；每个值列表都交给同一个 k：

```racket
;; make-amb 中的答案分支；next 仍是供应过程，不提前拉取。
((merge (λ () (apply k r))
        (λ () (loop next))))
```

amb 供应的是尚未执行的主体 thunk，再在捕获的后续中调用所选 thunk。值生成器与计算备选由这一步接上，不提前计算 a/b，也不把过程当成最终答案。二元方程从 `μ(ka,μ(kb,0))` 化到 `μ(ka,kb)` 用到了右单位律；这是明确的语义条件，不是隐蔽的终止身份优化。

| 操作 | 地位 |
|---|---|
| make-amb、amb-fail、amb-pause | 三个原始控制效应 |
| in-amb | 独立的惰性 sequence 边界 |
| current-amb-merge、四种 merge | 策略参数与独立的生成过程组合 |
| amb、for/amb | 通过 make-amb 派生的语法 |

for/amb 的枚举、过滤、break/final 属于供应，主体属于搜索分支。供应阶段试图影响当前搜索的三个效应会报错；完整的独立嵌套 in-amb 合法。一个私有 continuation mark 记录阶段，既能动态检查间接调用，也不冻结无关参数；新搜索的正常阶段标记遮蔽外层供应/观察标记。

这里刻意不用进入搜索时的私有 parameterize 作为阶段标记：它可能把当时的整个参数环境带入捕获的后续，从而冻结本应在以后读取的策略或其它参数。tests.rkt 对这项区别有明确的回归测试。for/stream 作为宿主的备选来源可能有自身缓存；本内核没有给输入 generator 或搜索结果增加记忆层。

## 原生 sequence 互操作

```racket
(for/list ([x (in-amb (amb 1 2 3))]) x)
;; '(1 2 3)

(for/list ([_ (in-range 5)]
           [x (in-amb (make-amb (λ () (sequence-generate* (in-naturals)))))])
  x)
;; '(0 1 2 3 4)

(for/list ([(x y) (in-amb (amb (values 1 2) (values 3 4)))]) (+ x y))
;; '(3 7)

(for/list ([x (in-amb (begin (amb-pause) 42) #:on-pause (λ () 'paused))]) x)
;; '(paused 42)
```

默认跳过暂停，#:on-pause 仅是本稿暂选的观察接口。暂停回调的结果不送回搜索、不参加 merge；零/多返回值都自然成为一个 sequence 元素。正整数块按每个逻辑暂停观察。带任意共享状态副作用的回调不能承诺完全不影响未来程序，详见 DESIGN §6。

in-amb 创建与 sequence 初始化都不执行正文；实际请求元素才执行。每个选择点在实际执行时读参数并固定，而不是创建 sequence 时冻结整个环境。将 parameterize 写进搜索正文可让它成为后续的一部分。

## 运行与检查

Racket 9.3 CS 已实测；只需要完整 Racket 分发中自带的 racket/control、racket/stream 和 rackunit，不安装本仓库的 amb 包。

```sh
cd research/minimal-reference-20261114
timeout 20s raco make kernel.rkt tests.rkt examples.rkt
timeout 45s raco test tests.rkt
timeout 10s racket examples.rkt
```

也可通过 `RACKET=/path/to/racket RACO=/path/to/raco bash run.sh` 运行。测试从同仓库原来的 `../choice-as-merge/model.rkt` 读取独立 explicit-CPS 规格和四个 reference merge，**不改旧期望**。所有无限例子都有有限事件/答案前缀，进程另有超时；没有 benchmark。

如环境未保留 Racket，安装前先检查 PATH 和已有目录。可用 [Racket 9.3 Linux CS](https://github.com/NoahStoryM/racket-dist/releases/download/v9.3/racket-9.3-x86_64-linux-buster-cs.sh)；下载须设置 connect/max-time 和重试上限，安装也设 timeout。

## 有意限定的范围

本实现直接给出合并方程，不追求长期搜索性能；不提供 for*/amb、#:length、shuffler、n 路公平调度、v2/Typed Racket 兼容层或旧 API 迁移。

与前版有意不同：完全惰性边界；供应阶段的效应统一诊断；不做 known-done singleton 捷径；不公开 reify/reflect；自定义 merge 的零元律明确列为数学前提。DESIGN §3 与 tests.rkt 给出错误的 eager 展开、缺少 thunk 调用、违反右单位律、以及把 sequence 接回后不能作为上下文逆运算的反例。

尚待正式决定：零元律是否成为公开自定义 merge 契约；#:on-pause 是否保留；单遍重复初始化是否固定报错。本原型已选定一致行为以便执行检验，不代表正式 v3 已定案。
