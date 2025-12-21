(** Sys_metrics: minimal cross-platform system metrics.

    {1 Design Principles}

    - {b Poll-based}: all functions return immediately with current values. No
      threads, no callbacks, no file descriptors. You sample when you want.

    - {b Snapshots are raw counters}: CPU times, network bytes, etc. are
      cumulative. Take two snapshots, diff them yourself or use the provided
      [pct]/[rate] helpers. This gives you control over sampling intervals.

    - {b No hidden state}: unlike designs with internal "samplers," there's
      nothing to initialize or clean up. Functions are pure reads of system
      state.

    - {b Timestamps are external}: call {!val:now_ns} yourself if you need
      timing. Keeps types simple and lets you use your own clock if preferred.

    - {b Fail explicitly}: raises {!exception:Unavailable} rather than returning
      dummy data. You'll know if a metric isn't supported on your platform.

    {1 Platform Notes}

    - {b Linux}: reads procfs directly (/proc/stat, /proc/meminfo,
      /proc/net/dev).
    - {b macOS}: Mach APIs via C stubs (host_processor_info, host_statistics64).
    - {b BSD}: sysctl, getifaddrs.
    - {b Windows}: Win32 APIs (GetSystemTimes, GlobalMemoryStatusEx).

    {1 Quick Start}

    {[
      (* CPU usage over 1 second *)
      let t0 = Sys_metrics.now_ns () in
      let cpu0 = Sys_metrics.Cpu.sample () in
      Unix.sleepf 1.0;
      let t1 = Sys_metrics.now_ns () in
      let cpu1 = Sys_metrics.Cpu.sample () in
      let busy = Sys_metrics.Cpu.busy ~dt_ns:(Int64.sub t1 t0) ~prev:cpu0 ~next:cpu1 in
      Printf.printf "CPU: %.1f%% busy\n" busy

      (* Memory right now *)
      let mem = Sys_metrics.Mem.sample () in
      Printf.printf "Memory: %Ld / %Ld bytes used\n" mem.used mem.total

      (* Network throughput *)
      let net0 = Sys_metrics.Net.sample () in
      Unix.sleepf 1.0;
      let net1 = Sys_metrics.Net.sample () in
      Sys_metrics.Net.rates ~dt_ns:1_000_000_000L ~prev:net0 ~next:net1
      |> Array.iter (fun (name, r) ->
           Printf.printf "%s: %.1f KB/s rx, %.1f KB/s tx\n"
             name (r.rx_bytes_per_s /. 1024.) (r.tx_bytes_per_s /. 1024.))
    ]} *)

exception Unavailable of string
(** Raised when a metric isn't available on the current platform. The string
    describes what's missing (e.g., "NVML not found"). *)

val now_ns : unit -> int64
(** Monotonic timestamp in nanoseconds. Uses [clock_gettime(CLOCK_MONOTONIC)] on
    POSIX, [QueryPerformanceCounter] on Windows. Suitable for measuring
    intervals; not wall-clock time. *)

(** {1 CPU} *)

module Cpu : sig
  (** CPU time counters and utilization.

      Times are cumulative since boot. The units vary by platform (jiffies,
      ticks, 100ns intervals) but cancel out when you diff two samples. *)

  type times = {
    user : int64;  (** Time in user mode. *)
    nice : int64;  (** Time in user mode with low priority (Linux/BSD). *)
    system : int64;  (** Time in kernel mode. *)
    idle : int64;  (** Time idle. *)
    iowait : int64;  (** Time waiting for I/O (Linux only; 0 elsewhere). *)
    irq : int64;  (** Time servicing interrupts (Linux only). *)
    softirq : int64;  (** Time servicing softirqs (Linux only). *)
    steal : int64;  (** Time stolen by hypervisor (Linux only). *)
  }
  (** Raw CPU time counters. On non-Linux platforms, Linux-specific fields are
      0. Use {!val:busy} or {!val:usage} rather than inspecting directly unless
      you need the breakdown. *)

  type t = {
    total : times;  (** Aggregate across all cores. *)
    per_core : times array;  (** Per-core breakdown. May be empty on Windows. *)
  }

  val sample : unit -> t
  (** Read current CPU counters. Cheap (single procfs read on Linux). *)

  val busy : dt_ns:int64 -> prev:times -> next:times -> float
  (** [busy ~dt_ns ~prev ~next] returns CPU busy percentage (0.0–100.0). Busy =
      100 - idle - iowait. The [dt_ns] parameter is currently unused but
      reserved for platforms where counters need time-based normalization. *)

  type pct = {
    user : float;  (** 0.0–100.0 *)
    nice : float;
    system : float;
    idle : float;
    iowait : float;
    irq : float;
    softirq : float;
    steal : float;
    busy : float;  (** Convenience: 100 - idle - iowait *)
  }
  (** Detailed percentage breakdown. Adds to ~100 (may differ slightly due to
      rounding or platform quirks). *)

  val usage : dt_ns:int64 -> prev:times -> next:times -> pct
  (** Full breakdown as percentages. Use {!val:busy} if you just want one
      number. *)
end

(** {1 Memory} *)

module Mem : sig
  (** Physical memory statistics.

      All values are in bytes. A single sample gives current state; no diffing
      needed (unlike CPU). *)

  type t = {
    total : int64;  (** Total physical RAM. *)
    free : int64;  (** Completely unused RAM. *)
    available : int64 option;
        (** RAM available for new allocations without swapping. More useful than
            [free]; includes reclaimable caches. [None] on older kernels or some
            BSDs. *)
    used : int64;  (** [total - available] or [total - free] as fallback. *)
    cached : int64 option;  (** Page cache (Linux). *)
    buffers : int64 option;  (** Buffer cache (Linux). *)
    swap_total : int64 option;  (** Total swap space. *)
    swap_free : int64 option;  (** Free swap space. *)
  }

  val sample : unit -> t
  (** Read current memory statistics. *)
end

(** {1 Network} *)

module Net : sig
  (** Network interface counters and throughput.

      Counters are cumulative since interface came up. They may wrap (32-bit
      counters on some platforms); the rate functions handle this. *)

  type iface = {
    name : string;
    rx_bytes : int64;
    tx_bytes : int64;
    rx_packets : int64;
    tx_packets : int64;
  }
  (** Per-interface cumulative counters. *)

  type t = iface array
  (** Snapshot of all interfaces. Order is not guaranteed stable across samples.
  *)

  val sample : unit -> t
  (** Read current counters for all interfaces. On Linux, reads /proc/net/dev.
      Loopback and virtual interfaces are included; filter by name if unwanted.
  *)

  type rate = {
    rx_bytes_per_s : float;
    tx_bytes_per_s : float;
    rx_packets_per_s : float;
    tx_packets_per_s : float;
  }
  (** Throughput rates computed from counter deltas. *)

  val rate : dt_ns:int64 -> prev:iface -> next:iface -> rate
  (** Compute rates for a single interface. Interfaces must have same [name].
      Handles counter wraparound for 32-bit counters. *)

  val rates : dt_ns:int64 -> prev:t -> next:t -> (string * rate) array
  (** Compute rates for all interfaces present in both snapshots, matched by
      name. Interfaces that appear in only one snapshot are omitted. *)
end

(** {1 Disk} *)

module Disk : sig
  (** Filesystem space usage.

      Reports capacity, not I/O throughput. For I/O stats on Linux, read
      /proc/diskstats yourself—it's too platform-specific to abstract well. *)

  type t = {
    total : int64;  (** Total filesystem capacity in bytes. *)
    free : int64;  (** Free space (includes reserved blocks). *)
    avail : int64;  (** Space available to unprivileged users. *)
    used : int64;
        (** [total - free]. Note: [used + avail] may exceed [total] due to
            reserved blocks. *)
  }

  val stat : string -> t
  (** [stat path] returns usage for the filesystem containing [path]. Raises
      [Unavailable] if [path] doesn't exist or can't be statted. *)
end

(** {1 Process (self)} *)

module Proc : sig
  (** Metrics for the current process.

      CPU times are cumulative; diff two samples for interval usage. Memory
      values are instantaneous. *)

  type t = {
    utime_ns : int64;  (** User CPU time in nanoseconds. *)
    stime_ns : int64;  (** System CPU time in nanoseconds. *)
    rss : int64 option;  (** Resident set size in bytes. *)
    vsize : int64 option;  (** Virtual memory size in bytes. *)
  }

  val sample : unit -> t
  (** Read stats for current process. On Linux, reads /proc/self/stat. *)

  val cpu_pct : dt_ns:int64 -> prev:t -> next:t -> float
  (** Process CPU usage as percentage (0.0–100.0 per core; can exceed 100.0 on
      multi-core if process uses multiple cores). *)
end

(** {1 GPU (optional)} *)

module Gpu : sig
  (** NVIDIA GPU metrics via NVML.

      NVML is loaded dynamically at first call. If the library isn't present (no
      NVIDIA driver, no nvidia-ml), functions return empty arrays rather than
      raising. This lets you safely call them on any system. *)

  type device = {
    index : int;
    name : string;  (** e.g., "NVIDIA GeForce RTX 3080" *)
    util_gpu : int;  (** GPU utilization, 0–100. *)
    util_mem : int;  (** Memory controller utilization, 0–100. *)
    mem_total : int64;  (** Total GPU memory in bytes. *)
    mem_used : int64;  (** Used GPU memory in bytes. *)
    mem_free : int64;  (** Free GPU memory in bytes. *)
    temp_c : int;  (** Temperature in Celsius. *)
    power_mw : int;  (** Power draw in milliwatts. *)
  }

  val sample : unit -> device array
  (** Query all NVIDIA GPUs. Returns [||] if NVML unavailable or no GPUs found.
      Each call queries the driver; cache results if polling frequently. *)

  val available : unit -> bool
  (** [true] if NVML loaded successfully and at least one GPU exists. *)
end
