# NES Emulator

一个使用 Rust 编写的 NES 模拟器学习项目。项目实现了 6502 CPU、PPU、总线、卡带解析、画面渲染和单手柄输入，并通过 SDL2 显示游戏画面。

> 该项目主要用于学习 NES 硬件结构与模拟器实现，目前并不追求完整的游戏兼容性。

## 已实现功能

- 6502 CPU 指令执行与多种寻址模式
- 部分非官方 6502 指令
- CPU 内存映射与 RAM 镜像
- PPU 寄存器、显存、调色板和 OAM
- 背景与精灵渲染
- 垂直空白中断（NMI）
- OAM DMA
- iNES 1.0 ROM 解析
- 水平与垂直 Nametable 镜像
- SDL2 窗口输出
- 玩家 1 手柄输入
- CPU、PPU、ROM 解析和指令跟踪相关单元测试

## 当前限制

- 仅适合无需 Mapper 切换的简单 ROM，尚未实现 Mapper 系统
- 不支持 NES 2.0
- APU 和声音输出尚未实现
- 暂不支持玩家 2 手柄
- 部分 PPU 行为和时序仍为简化实现
- 两个示例程序中的 ROM 路径目前为本机绝对路径，运行前需要修改

## 环境要求

- Rust 2021 edition
- Cargo
- SDL2

安装 Rust：

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

macOS 使用 Homebrew 安装 SDL2：

```bash
brew install sdl2
```

Ubuntu / Debian：

```bash
sudo apt install libsdl2-dev
```

## 运行

仓库提供了两个可执行程序：

| 程序 | 源文件 | ROM |
| --- | --- | --- |
| `pacman` | `src/bin/pacman.rs` | `nes/pacman.nes` |
| `play` | `src/bin/play.rs` | `nes/super.nes` |

运行前，请在对应源文件中找到 `std::fs::read(...)`，将其中的 ROM 路径改为当前仓库内 ROM 文件的实际路径。

运行 Pac-Man：

```bash
cargo run --bin pacman
```

运行另一个示例：

```bash
cargo run --bin play
```

如果 macOS 无法找到 SDL2，可以显式指定 Homebrew 的库目录：

```bash
cargo rustc --bin pacman -- -L "$(brew --prefix sdl2)/lib" -l SDL2
./target/debug/pacman
```

## 按键

| 键盘 | NES 按键 |
| --- | --- |
| `↑` / `↓` / `←` / `→` | 方向键 |
| `A` | A |
| `S` | B |
| `Enter` | Start |
| `Space` | Select |
| `Esc` | 退出 |

## 测试

```bash
cargo test
```

## 项目结构

```text
.
├── nes/                 # 示例 ROM
├── src/
│   ├── bin/             # SDL2 程序入口
│   ├── cpu/             # 指令解析、中断与执行跟踪
│   ├── ppu/             # PPU 寄存器
│   ├── render/          # 帧缓冲与调色板
│   ├── bus.rs           # CPU 总线和内存映射
│   ├── cartridges.rs    # iNES ROM 解析
│   ├── cpu.rs           # 6502 CPU
│   ├── joypad.rs        # 手柄输入
│   ├── ppu.rs           # PPU
│   └── render.rs        # 画面渲染
├── Cargo.toml
└── README.md
```

## ROM 说明

ROM 文件可能受版权保护。请仅使用你有权运行和分发的 ROM；如果公开派生项目，请勿提交无授权的游戏 ROM。
