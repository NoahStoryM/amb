# 与 v2 风格对齐的 demo

从 [demo.rkt](demo.rkt) 阅读；它基于 Noah 提供的版本，只补上已讨论的三项修正：

1. 协议允许自然数。0 表示无事件推进，不结束搜索、不换边、不调用暂停 handler；四个 merge、make-amb 与观察器都处理它。level 在扫描右侧时同样跳过 0。
2. 宏工厂用 with-syntax 注入循环形式，并让 for*/amb 的 break/final 分支仍递归到自身。
3. 单遍 sequence 使用独立 done?，耗尽后稳定为空；重新迭代同一对象继续共享游标，不增加重复初始化错误。

保留原作者的名称、curried define、向量备选、sequence->amb 辅助函数、initiate-sequence 和 compose1 写法。#:on-pause 保留。0 次暂停与普通答案 0 不同：后者仍包装在值列表中。

供应阶段与观察回调中的 ambient 搜索效应暂不作为 demo 的已定义用法，也不添加检查；循环体中的选择照常使用。异常完善、性能优化与缓存 stream 接口留给后续版本。没有引入新的结构体或抽象层。只返回 0 而永不产生其它结果的源并不生产事件，不能靠事件 fuel 保证它返回。

小型有界测试见 [demo-tests.rkt](demo-tests.rkt)。原 kernel.rkt、DESIGN.md 和它们的测试保留为前一个参考模型，正整数协议与阶段检查不因本 demo 而回写。

Racket 9.3 CS 实测：8 个 test cases、1,050 次比较通过，记录见 [demo-validation.txt](demo-validation.txt)；包括对照原参考 merge 验证插入 0 后的事件前缀不变。没有运行 benchmark。

```sh
timeout 20s raco make demo.rkt demo-tests.rkt
timeout 30s raco test demo-tests.rkt
```
