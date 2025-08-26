# Sysy 编译器开发进度

> 使用 Rust 语言实现的 Sysy 编译器项目

<div style="background: linear-gradient(135deg, #1a2a6c, #b21f1f, #fdbb2d); padding: 20px; border-radius: 10px; color: white; margin-bottom: 20px;">
  <h2 style="color: white; margin-top: 0;">总体进度</h2>
  <div style="background: rgba(255, 255, 255, 0.2); border-radius: 10px; height: 20px; margin: 10px 0;">
    <div style="background: white; width: 33.3%; height: 100%; border-radius: 10px; display: flex; align-items: center; justify-content: flex-end;">
      <span style="color: #1a2a6c; padding-right: 10px; font-weight: bold;">33.3%</span>
    </div>
  </div>
  <p>已完成 2/6 个阶段</p>
</div>

## 项目阶段

<details open>
<summary style="cursor: pointer; background-color: #4CAF50; color: white; padding: 10px; border-radius: 5px; margin: 5px 0;">
✅ LAB0：环境配置 (已完成)
</summary>
<div style="background-color: #f9f9f9; padding: 15px; border-radius: 0 0 5px 5px; margin-bottom: 10px;">
<p>已完成 Rust 开发环境配置，包括：</p>
<ul>
<li>Rust 工具链安装</li>
<li>项目结构初始化</li>
<li>必要的依赖配置</li>
<li>开发环境测试</li>
</ul>
</div>
</details>

<details open>
<summary style="cursor: pointer; background-color: #4CAF50; color: white; padding: 10px; border-radius: 5px; margin: 5px 0;">
✅ LAB1：词法分析 (已完成)
</summary>
<div style="background-color: #f9f9f9; padding: 15px; border-radius: 0 0 5px 5px; margin-bottom: 10px;">
<p>已完成词法分析器实现，包括：</p>
<ul>
<li>Token 类型定义</li>
<li>词法解析逻辑</li>
<li>错误处理机制</li>
<li>单元测试覆盖</li>
</ul>
</div>
</details>

<details>
<summary style="cursor: pointer; background-color: #f44336; color: white; padding: 10px; border-radius: 5px; margin: 5px 0;">
❌ LAB2：语法分析 (未完成)
</summary>
<div style="background-color: #f9f9f9; padding: 15px; border-radius: 0 0 5px 5px; margin-bottom: 10px;">
<p>计划实现语法分析器，将词法单元转换为抽象语法树(AST)</p>
</div>
</details>

<details>
<summary style="cursor: pointer; background-color: #f44336; color: white; padding: 10px; border-radius: 5px; margin: 5px 0;">
❌ LAB3：类型检查 (未完成)
</summary>
<div style="background-color: #f9f9f9; padding: 15px; border-radius: 0 0 5px 5px; margin-bottom: 10px;">
<p>计划实现类型检查系统，确保程序语义正确性</p>
</div>
</details>

<details>
<summary style="cursor: pointer; background-color: #f44336; color: white; padding: 10px; border-radius: 5px; margin: 5px 0;">
❌ LAB4：中间代码生成 (未完成)
</summary>
<div style="background-color: #f9f9f9; padding: 15px; border-radius: 0 0 5px 5px; margin-bottom: 10px;">
<p>计划实现中间代码生成器，将AST转换为中间表示(IR)</p>
</div>
</details>

<details>
<summary style="cursor: pointer; background-color: #f44336; color: white; padding: 10px; border-radius: 5px; margin: 5px 0;">
❌ LAB5：RISC-V 目标代码生成和寄存器分配 (未完成)
</summary>
<div style="background-color: #f9f9f9; padding: 15px; border-radius: 0 0 5px 5px; margin-bottom: 10px;">
<p>计划实现目标代码生成器，将IR转换为RISC-V汇编代码</p>
</div>
</details>

<details>
<summary style="cursor: pointer; background-color: #f44336; color: white; padding: 10px; border-radius: 5px; margin: 5px 0;">
❌ LAB6：中间代码优化 (未完成)
</summary>
<div style="background-color: #f9f9f9; padding: 15px; border-radius: 0 0 5px 5px; margin-bottom: 10px;">
<p>计划实现中间代码优化器，提高生成代码的性能</p>
</div>
</details>

## 技术栈
- **语言**: Rust
- **构建工具**: Cargo
- **测试框架**: Rust 内置测试框架

## 后续计划
逐步完成剩余阶段，优先实现语法分析和类型检查功能。

<style>
details[open] summary {
  border-radius: 5px 5px 0 0 !important;
}
</style>
