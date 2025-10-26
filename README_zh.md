# Ratlang
[English](README.md)

Ratlang 是一个实验性的编程语言与工具链，其全部代码均由 AI 独立完成，没有人工直接修改。项目探索“纯 AI 编写的高级语言”在语法设计、开发体验和工程实践上的可能性，同时保证对现有开发者依旧友好。

## 亮点
- **全栈 AI 生成**：词法、语法、类型检查、解释器、CLI、示例与测试均由 Codex 自动产出。
- **表达力强的语法**：缩进式代码块、管道操作、lambda、模式匹配、Option/Result 字面量与丰富数据结构。
- **树遍解释器**：支持闭包、一等函数、循环、`break/continue`、多分支 `else if` 与小型原生函数集（`print`、`len` 等）。
- **完整工具链**：工作区包含 `rat`（脚本执行 + REPL）、`ratc`、格式化工具、文档生成器、语言服务器与实用示例。
- **TDD 驱动**：`crates/ratlang/tests/api` 中的集成测试直接运行 Ratlang 程序以防回归。

## 实现概览
- **语言前端**：`crates/ratlang/src/{lexer,parser,ast}` 负责词法分析、递归下降解析与 AST 建模。
- **静态分析**：`typeck.rs` 进行轻量推断/检查，在执行前捕获常见类型错误。
- **运行时**：`runtime.rs` 以内存共享的 `Rc` 闭包与环境栈执行 AST，完整传播控制流状态并集成原生函数。
- **编译调度**：`Compiler` 连接源码映射、诊断系统与类型信息，生成交由 VM 执行的 `Compilation`。
- **工具二进制**：`crates/` 下的每个子项目都是面向用户的工具，统一使用同一工作区配置。

## 快速体验
```bash
cargo run -p rat -- examples/production_dashboard.rat
```
脚本参数可通过 `--` 传递给 `main(args)`：
```bash
cargo run -p rat -- examples/feature_tour.rat -- hello world
```
或运行集成测试：
```bash
cargo test -p ratlang --test api -- --nocapture
```

## 开源许可证
GPL-3.0 — 详见 [LICENSE](./LICENSE)。
