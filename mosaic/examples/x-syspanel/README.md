# System Panel

Terminal-based system monitor displaying real-time CPU, memory, disk, and process metrics. Built with Mosaic TEA architecture and matrix.charts for visualizations.

## Run

```bash
dune exec ./mosaic/examples/x-syspanel/main.exe
```

## Features

- **Per-core CPU usage** – Progress bars for each CPU core with color-coding (red >80%, yellow >50%, green otherwise)
- **Memory metrics** – Total memory, used memory with percentage, and swap usage
- **Disk partitions** – Scrollable list of disk partitions with mount points and usage statistics
- **Top processes** – Running processes sorted by CPU usage with truncated names
- **Sparklines** – Real-time sparkline charts for CPU, memory, and disk usage
- **Process self** – Current process CPU and RSS memory statistics

## Controls

- `q` or `Esc` – quit

## Concepts

- **TEA architecture** – Model-View-Update pattern with subscriptions for periodic updates
- **System metrics collection** – Cross-platform metric gathering using Unix system calls and shell commands
- **Custom metrics library** – `Metrics` module for CPU, memory, disk, and process data collection
- **Progress bars** – Custom canvas drawing for CPU core visualization
- **Scrollable lists** – Using `scroll_box` for partitions and processes
- **Sparklines** – Integration with `matrix.charts` for time-series visualization
- **Cross-platform support** – Works on both Linux and macOS with platform-specific metric collection

## Metrics Collected

- **CPU**: Per-core user/system time, overall CPU usage
- **Memory**: Total, used, swap (total/used) with percentages
- **Disk**: Partition mount points, total/used/available space, usage percentages
- **Processes**: Top processes by CPU usage, PID, process name, CPU percentage

## Platform Support

- **Linux**: Uses `/proc` filesystem and `ps` command
- **macOS**: Uses `sysctl` and `ps` commands

