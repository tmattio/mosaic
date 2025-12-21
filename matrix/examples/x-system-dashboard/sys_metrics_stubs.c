/*
  sys_metrics_stubs.c - C stubs for Sys_metrics

  Exports:
    caml_sys_metrics_cpu_times      : unit -> int64 array array
    caml_sys_metrics_mem_snapshot   : unit -> int64 array
    caml_sys_metrics_net_ifaces     : unit -> (string * int64 * int64 * int64 *
  int64) array caml_sys_metrics_statvfs        : string -> (int64 * int64 *
  int64) caml_sys_metrics_proc_self_mem  : unit -> (int64 * int64)
    caml_sys_metrics_nvml_snapshot  : unit -> (int * string * int * int * int64
  * int64 * int64) array

  Notes:
  - Linux: CPU/mem/net are implemented in OCaml via /proc for best detail.
           Stubs provide disk statvfs and optional NVML.
  - macOS: Mach for CPU+mem; task_info for rss/vsz; getifaddrs for net.
  - BSD: sysctl kern.cp_times for CPU; sysconf for memory fallback; getifaddrs
  for net.
  - Windows: Win32 APIs for all; NVML loaded dynamically.

  NVML: loaded dynamically (no headers required); returns empty array if
  unavailable.
*/

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>
#include <string.h>

static value caml_copy_int64_i(int64_t x) { return caml_copy_int64(x); }

static value alloc_int64_array_from(const int64_t* vals, mlsize_t n) {
  CAMLparam0();
  CAMLlocal1(arr);
  arr = caml_alloc(n, 0);
  for (mlsize_t i = 0; i < n; i++)
    Store_field(arr, i, caml_copy_int64_i(vals[i]));
  CAMLreturn(arr);
}

static value alloc_cpu_row(int64_t user, int64_t nice, int64_t sys,
                           int64_t idle, int64_t iowait, int64_t irq,
                           int64_t softirq, int64_t steal) {
  int64_t vals[8];
  vals[0] = user;
  vals[1] = nice;
  vals[2] = sys;
  vals[3] = idle;
  vals[4] = iowait;
  vals[5] = irq;
  vals[6] = softirq;
  vals[7] = steal;
  return alloc_int64_array_from(vals, 8);
}

#ifdef _WIN32

#define WIN32_LEAN_AND_MEAN
#include <iphlpapi.h>
#include <psapi.h>
#include <windows.h>

#pragma comment(lib, "psapi.lib")
#pragma comment(lib, "iphlpapi.lib")

static uint64_t filetime_to_u64(const FILETIME* ft) {
  ULARGE_INTEGER ui;
  ui.LowPart = ft->dwLowDateTime;
  ui.HighPart = ft->dwHighDateTime;
  return ui.QuadPart;
}

CAMLprim value caml_sys_metrics_cpu_times(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(outer, row, totalrow);

  FILETIME idleTime, kernelTime, userTime;
  if (!GetSystemTimes(&idleTime, &kernelTime, &userTime)) {
    outer = caml_alloc(0, 0);
    CAMLreturn(outer);
  }

  uint64_t idle = filetime_to_u64(&idleTime);
  uint64_t kern = filetime_to_u64(&kernelTime);
  uint64_t user = filetime_to_u64(&userTime);

  /* kernel includes idle; treat "system" as kernel - idle */
  uint64_t sys = (kern > idle) ? (kern - idle) : 0;

  totalrow =
      alloc_cpu_row((int64_t)user, 0, (int64_t)sys, (int64_t)idle, 0, 0, 0, 0);

  outer = caml_alloc(1, 0);
  Store_field(outer, 0, totalrow);
  CAMLreturn(outer);
}

CAMLprim value caml_sys_metrics_mem_snapshot(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(arr);

  MEMORYSTATUSEX st;
  st.dwLength = sizeof(st);
  if (!GlobalMemoryStatusEx(&st)) {
    int64_t vals[10];
    for (int i = 0; i < 10; i++) vals[i] = -1;
    arr = alloc_int64_array_from(vals, 10);
    CAMLreturn(arr);
  }

  /* Best-effort: avail == free for our purposes; swap uses pagefile totals. */
  int64_t vals[10];
  vals[0] = (int64_t)st.ullTotalPhys;
  vals[1] = (int64_t)st.ullAvailPhys;
  vals[2] = (int64_t)st.ullAvailPhys;
  vals[3] = -1; /* active */
  vals[4] = -1; /* inactive */
  vals[5] = -1; /* wired */
  vals[6] = -1; /* cached */
  vals[7] = -1; /* buffers */
  vals[8] = (int64_t)st.ullTotalPageFile;
  vals[9] = (int64_t)st.ullAvailPageFile;

  arr = alloc_int64_array_from(vals, 10);
  CAMLreturn(arr);
}

CAMLprim value caml_sys_metrics_net_ifaces(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(arr, tup, namev);

  PMIB_IF_TABLE2 table = NULL;
  if (GetIfTable2(&table) != NO_ERROR || table == NULL) {
    arr = caml_alloc(0, 0);
    CAMLreturn(arr);
  }

  ULONG n = table->NumEntries;
  arr = caml_alloc(n, 0);

  for (ULONG i = 0; i < n; i++) {
    MIB_IF_ROW2* r = &table->Table[i];

    /* Convert Alias (WCHAR*) -> UTF-8 */
    char name_utf8[256];
    name_utf8[0] = 0;
    int needed = WideCharToMultiByte(CP_UTF8, 0, r->Alias, -1, name_utf8,
                                     (int)sizeof(name_utf8), NULL, NULL);
    if (needed <= 0) {
      /* fallback to index */
      snprintf(name_utf8, sizeof(name_utf8), "if%lu", (unsigned long)i);
    }

    uint64_t rx_b = r->InOctets;
    uint64_t tx_b = r->OutOctets;
    uint64_t rx_p = r->InUcastPkts + r->InNUcastPkts;
    uint64_t tx_p = r->OutUcastPkts + r->OutNUcastPkts;

    namev = caml_copy_string(name_utf8);
    tup = caml_alloc_tuple(5);
    Store_field(tup, 0, namev);
    Store_field(tup, 1, caml_copy_int64_i((int64_t)rx_b));
    Store_field(tup, 2, caml_copy_int64_i((int64_t)tx_b));
    Store_field(tup, 3, caml_copy_int64_i((int64_t)rx_p));
    Store_field(tup, 4, caml_copy_int64_i((int64_t)tx_p));

    Store_field(arr, i, tup);
  }

  FreeMibTable(table);
  CAMLreturn(arr);
}

CAMLprim value caml_sys_metrics_statvfs(value v_path) {
  CAMLparam1(v_path);
  CAMLlocal1(tup);

  const char* path = String_val(v_path);

  /* UTF-8 -> UTF-16 */
  wchar_t wpath[MAX_PATH * 4];
  int wlen = MultiByteToWideChar(CP_UTF8, 0, path, -1, wpath,
                                 (int)(sizeof(wpath) / sizeof(wpath[0])));
  if (wlen <= 0) caml_failwith("GetDiskFreeSpaceEx: path conversion failed");

  ULARGE_INTEGER freeAvail, total, freeTotal;
  if (!GetDiskFreeSpaceExW(wpath, &freeAvail, &total, &freeTotal)) {
    caml_failwith("GetDiskFreeSpaceEx failed");
  }

  tup = caml_alloc_tuple(3);
  Store_field(tup, 0, caml_copy_int64_i((int64_t)total.QuadPart));
  Store_field(tup, 1, caml_copy_int64_i((int64_t)freeTotal.QuadPart));
  Store_field(tup, 2, caml_copy_int64_i((int64_t)freeAvail.QuadPart));
  CAMLreturn(tup);
}

CAMLprim value caml_sys_metrics_proc_self_mem(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(tup);

  PROCESS_MEMORY_COUNTERS_EX pmc;
  memset(&pmc, 0, sizeof(pmc));
  if (!GetProcessMemoryInfo(GetCurrentProcess(), (PROCESS_MEMORY_COUNTERS*)&pmc,
                            sizeof(pmc))) {
    tup = caml_alloc_tuple(2);
    Store_field(tup, 0, caml_copy_int64_i(-1));
    Store_field(tup, 1, caml_copy_int64_i(-1));
    CAMLreturn(tup);
  }

  /* rss = working set; vsize best-effort = private usage */
  tup = caml_alloc_tuple(2);
  Store_field(tup, 0, caml_copy_int64_i((int64_t)pmc.WorkingSetSize));
  Store_field(tup, 1, caml_copy_int64_i((int64_t)pmc.PrivateUsage));
  CAMLreturn(tup);
}

/* ---- NVML dynamic loading (Windows) ---- */

typedef struct nvmlDevice_st* nvmlDevice_t;
typedef unsigned int nvmlReturn_t;
typedef struct {
  unsigned int gpu;
  unsigned int memory;
} nvmlUtilization_t;
typedef struct {
  unsigned long long total;
  unsigned long long free;
  unsigned long long used;
} nvmlMemory_t;

typedef nvmlReturn_t(__cdecl* p_nvmlInit_v2)(void);
typedef nvmlReturn_t(__cdecl* p_nvmlInit)(void);
typedef nvmlReturn_t(__cdecl* p_nvmlShutdown)(void);
typedef nvmlReturn_t(__cdecl* p_nvmlDeviceGetCount_v2)(unsigned int*);
typedef nvmlReturn_t(__cdecl* p_nvmlDeviceGetCount)(unsigned int*);
typedef nvmlReturn_t(__cdecl* p_nvmlDeviceGetHandleByIndex_v2)(unsigned int,
                                                               nvmlDevice_t*);
typedef nvmlReturn_t(__cdecl* p_nvmlDeviceGetHandleByIndex)(unsigned int,
                                                            nvmlDevice_t*);
typedef nvmlReturn_t(__cdecl* p_nvmlDeviceGetName)(nvmlDevice_t, char*,
                                                   unsigned int);
typedef nvmlReturn_t(__cdecl* p_nvmlDeviceGetUtilizationRates)(
    nvmlDevice_t, nvmlUtilization_t*);
typedef nvmlReturn_t(__cdecl* p_nvmlDeviceGetMemoryInfo)(nvmlDevice_t,
                                                         nvmlMemory_t*);
typedef nvmlReturn_t(__cdecl* p_nvmlDeviceGetTemperature)(nvmlDevice_t,
                                                          unsigned int,
                                                          unsigned int*);
typedef nvmlReturn_t(__cdecl* p_nvmlDeviceGetPowerUsage)(nvmlDevice_t,
                                                         unsigned int*);

static FARPROC sym(HMODULE h, const char* name) {
  return GetProcAddress(h, name);
}

CAMLprim value caml_sys_metrics_nvml_snapshot(value unit) {
  CAMLparam1(unit);
  CAMLlocal4(arr, tup, namev, tmp);

  HMODULE h = LoadLibraryA("nvml.dll");
  if (!h) {
    arr = caml_alloc(0, 0);
    CAMLreturn(arr);
  }

  p_nvmlInit_v2 nvmlInit_v2 = (p_nvmlInit_v2)sym(h, "nvmlInit_v2");
  p_nvmlInit nvmlInit = (p_nvmlInit)sym(h, "nvmlInit");
  p_nvmlShutdown nvmlShutdown = (p_nvmlShutdown)sym(h, "nvmlShutdown");
  p_nvmlDeviceGetCount_v2 nvmlDeviceGetCount_v2 =
      (p_nvmlDeviceGetCount_v2)sym(h, "nvmlDeviceGetCount_v2");
  p_nvmlDeviceGetCount nvmlDeviceGetCount =
      (p_nvmlDeviceGetCount)sym(h, "nvmlDeviceGetCount");
  p_nvmlDeviceGetHandleByIndex_v2 nvmlDeviceGetHandleByIndex_v2 =
      (p_nvmlDeviceGetHandleByIndex_v2)sym(h, "nvmlDeviceGetHandleByIndex_v2");
  p_nvmlDeviceGetHandleByIndex nvmlDeviceGetHandleByIndex =
      (p_nvmlDeviceGetHandleByIndex)sym(h, "nvmlDeviceGetHandleByIndex");
  p_nvmlDeviceGetName nvmlDeviceGetName =
      (p_nvmlDeviceGetName)sym(h, "nvmlDeviceGetName");
  p_nvmlDeviceGetUtilizationRates nvmlDeviceGetUtilizationRates =
      (p_nvmlDeviceGetUtilizationRates)sym(h, "nvmlDeviceGetUtilizationRates");
  p_nvmlDeviceGetMemoryInfo nvmlDeviceGetMemoryInfo =
      (p_nvmlDeviceGetMemoryInfo)sym(h, "nvmlDeviceGetMemoryInfo");
  p_nvmlDeviceGetTemperature nvmlDeviceGetTemperature =
      (p_nvmlDeviceGetTemperature)sym(h, "nvmlDeviceGetTemperature");
  p_nvmlDeviceGetPowerUsage nvmlDeviceGetPowerUsage =
      (p_nvmlDeviceGetPowerUsage)sym(h, "nvmlDeviceGetPowerUsage");

  if ((!nvmlInit_v2 && !nvmlInit) || !nvmlShutdown ||
      (!nvmlDeviceGetCount_v2 && !nvmlDeviceGetCount) ||
      (!nvmlDeviceGetHandleByIndex_v2 && !nvmlDeviceGetHandleByIndex) ||
      !nvmlDeviceGetName || !nvmlDeviceGetUtilizationRates ||
      !nvmlDeviceGetMemoryInfo) {
    arr = caml_alloc(0, 0);
    FreeLibrary(h);
    CAMLreturn(arr);
  }

  nvmlReturn_t rc = nvmlInit_v2 ? nvmlInit_v2() : nvmlInit();
  if (rc != 0) {
    arr = caml_alloc(0, 0);
    FreeLibrary(h);
    CAMLreturn(arr);
  }

  unsigned int count = 0;
  rc = nvmlDeviceGetCount_v2 ? nvmlDeviceGetCount_v2(&count)
                             : nvmlDeviceGetCount(&count);
  if (rc != 0 || count == 0) {
    nvmlShutdown();
    FreeLibrary(h);
    arr = caml_alloc(0, 0);
    CAMLreturn(arr);
  }

  arr = caml_alloc(count, 0);

  for (unsigned int i = 0; i < count; i++) {
    nvmlDevice_t dev;
    rc = nvmlDeviceGetHandleByIndex_v2 ? nvmlDeviceGetHandleByIndex_v2(i, &dev)
                                       : nvmlDeviceGetHandleByIndex(i, &dev);
    if (rc != 0) continue;

    char name[96];
    name[0] = 0;
    nvmlUtilization_t util;
    memset(&util, 0, sizeof(util));
    nvmlMemory_t mem;
    memset(&mem, 0, sizeof(mem));

    nvmlDeviceGetName(dev, name, sizeof(name));
    nvmlDeviceGetUtilizationRates(dev, &util);
    nvmlDeviceGetMemoryInfo(dev, &mem);

    unsigned int temp = 0;
    unsigned int power = 0;
    if (nvmlDeviceGetTemperature)
      nvmlDeviceGetTemperature(dev, 0 /* NVML_TEMPERATURE_GPU */, &temp);
    if (nvmlDeviceGetPowerUsage) nvmlDeviceGetPowerUsage(dev, &power);

    namev = caml_copy_string(name);
    tup = caml_alloc_tuple(9);
    Store_field(tup, 0, Val_int((int)i));
    Store_field(tup, 1, namev);
    Store_field(tup, 2, Val_int((int)util.gpu));
    Store_field(tup, 3, Val_int((int)util.memory));
    Store_field(tup, 4, caml_copy_int64_i((int64_t)mem.total));
    Store_field(tup, 5, caml_copy_int64_i((int64_t)mem.used));
    Store_field(tup, 6, caml_copy_int64_i((int64_t)mem.free));
    Store_field(tup, 7, Val_int((int)temp));
    Store_field(tup, 8, Val_int((int)power));

    Store_field(arr, i, tup);
  }

  nvmlShutdown();
  FreeLibrary(h);
  CAMLreturn(arr);
}

/* ---- clock_gettime_ns (Windows) ---- */

CAMLprim value caml_sys_metrics_clock_gettime_ns(value unit) {
  CAMLparam1(unit);
  LARGE_INTEGER freq, counter;
  QueryPerformanceFrequency(&freq);
  QueryPerformanceCounter(&counter);
  int64_t ns =
      (int64_t)((double)counter.QuadPart / (double)freq.QuadPart * 1e9);
  CAMLreturn(caml_copy_int64(ns));
}

CAMLprim value caml_sys_metrics_uname_sysname(value unit) {
  CAMLparam1(unit);
  CAMLreturn(caml_copy_string("Windows"));
}

#else /* POSIX (Linux/macOS/BSD) */

#include <errno.h>
#include <sys/statvfs.h>
#include <sys/types.h>
#include <unistd.h>

#if defined(__APPLE__)
#include <mach/mach.h>
#include <mach/mach_host.h>
#include <mach/processor_info.h>
#include <mach/task_info.h>
#include <sys/sysctl.h>
#endif

#if defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || \
    defined(__DragonFly__) || defined(__APPLE__)
#include <ifaddrs.h>
#include <net/if.h>
#include <net/if_dl.h>
#include <sys/sysctl.h>
#endif

#include <dlfcn.h>

/* ---- CPU times ---- */

CAMLprim value caml_sys_metrics_cpu_times(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(outer, row, totalrow);

#if defined(__APPLE__)
  natural_t ncpu = 0;
  processor_info_array_t cpuInfo;
  mach_msg_type_number_t numCpuInfo;

  kern_return_t kr = host_processor_info(
      mach_host_self(), PROCESSOR_CPU_LOAD_INFO, &ncpu, &cpuInfo, &numCpuInfo);
  if (kr != KERN_SUCCESS || ncpu == 0) {
    outer = caml_alloc(0, 0);
    CAMLreturn(outer);
  }

  /* cpuInfo is integer_t*, groups of CPU_STATE_MAX (=4) per cpu */
  int64_t sum_user = 0, sum_nice = 0, sum_sys = 0, sum_idle = 0;
  outer = caml_alloc((mlsize_t)(ncpu + 1), 0);

  for (natural_t i = 0; i < ncpu; i++) {
    integer_t* base = cpuInfo + (CPU_STATE_MAX * i);
    int64_t user = (int64_t)base[CPU_STATE_USER];
    int64_t nice = (int64_t)base[CPU_STATE_NICE];
    int64_t sys = (int64_t)base[CPU_STATE_SYSTEM];
    int64_t idle = (int64_t)base[CPU_STATE_IDLE];

    sum_user += user;
    sum_nice += nice;
    sum_sys += sys;
    sum_idle += idle;

    row = alloc_cpu_row(user, nice, sys, idle, 0, 0, 0, 0);
    Store_field(outer, (mlsize_t)(i + 1), row);
  }

  totalrow = alloc_cpu_row(sum_user, sum_nice, sum_sys, sum_idle, 0, 0, 0, 0);
  Store_field(outer, 0, totalrow);

  vm_deallocate(mach_task_self(), (vm_address_t)cpuInfo,
                (vm_size_t)(numCpuInfo * sizeof(integer_t)));
  CAMLreturn(outer);

#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || \
    defined(__DragonFly__)
  /* sysctl kern.cp_times: array of longs, 5 states per cpu:
     [user, nice, sys, intr, idle] repeated. */
  size_t sz = 0;
  if (sysctlbyname("kern.cp_times", NULL, &sz, NULL, 0) != 0 || sz == 0) {
    outer = caml_alloc(0, 0);
    CAMLreturn(outer);
  }

  long* buf = (long*)malloc(sz);
  if (!buf) {
    outer = caml_alloc(0, 0);
    CAMLreturn(outer);
  }
  if (sysctlbyname("kern.cp_times", buf, &sz, NULL, 0) != 0) {
    free(buf);
    outer = caml_alloc(0, 0);
    CAMLreturn(outer);
  }

  int states = 5;
  int ncpu = (int)(sz / (sizeof(long) * (size_t)states));
  if (ncpu <= 0) {
    free(buf);
    outer = caml_alloc(0, 0);
    CAMLreturn(outer);
  }

  int64_t sum_user = 0, sum_nice = 0, sum_sys = 0, sum_irq = 0, sum_idle = 0;
  outer = caml_alloc((mlsize_t)(ncpu + 1), 0);

  for (int i = 0; i < ncpu; i++) {
    long* p = buf + (i * states);
    int64_t user = (int64_t)p[0];
    int64_t nice = (int64_t)p[1];
    int64_t sys = (int64_t)p[2];
    int64_t intr = (int64_t)p[3];
    int64_t idle = (int64_t)p[4];

    sum_user += user;
    sum_nice += nice;
    sum_sys += sys;
    sum_irq += intr;
    sum_idle += idle;

    row = alloc_cpu_row(user, nice, sys, idle, 0, intr, 0, 0);
    Store_field(outer, (mlsize_t)(i + 1), row);
  }

  totalrow =
      alloc_cpu_row(sum_user, sum_nice, sum_sys, sum_idle, 0, sum_irq, 0, 0);
  Store_field(outer, 0, totalrow);

  free(buf);
  CAMLreturn(outer);

#else
  /* Linux CPU is handled in OCaml via /proc/stat (more detailed, per-core). */
  outer = caml_alloc(0, 0);
  CAMLreturn(outer);
#endif
}

/* ---- Memory snapshot ---- */

CAMLprim value caml_sys_metrics_mem_snapshot(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(arr);

  int64_t vals[10];
  for (int i = 0; i < 10; i++) vals[i] = -1;

#if defined(__APPLE__)
  /* Total via hw.memsize */
  uint64_t memsize = 0;
  size_t msz = sizeof(memsize);
  if (sysctlbyname("hw.memsize", &memsize, &msz, NULL, 0) == 0) {
    vals[0] = (int64_t)memsize;
  }

  vm_size_t page_size = 0;
  if (host_page_size(mach_host_self(), &page_size) != KERN_SUCCESS)
    page_size = 4096;

  vm_statistics64_data_t vmstat;
  mach_msg_type_number_t count = HOST_VM_INFO64_COUNT;
  if (host_statistics64(mach_host_self(), HOST_VM_INFO64,
                        (host_info64_t)&vmstat, &count) == KERN_SUCCESS) {
    int64_t free_b = (int64_t)vmstat.free_count * (int64_t)page_size;
    int64_t active_b = (int64_t)vmstat.active_count * (int64_t)page_size;
    int64_t inactive_b = (int64_t)vmstat.inactive_count * (int64_t)page_size;
    int64_t wired_b = (int64_t)vmstat.wire_count * (int64_t)page_size;

    vals[1] = free_b;
    vals[2] = free_b + inactive_b; /* "available-ish" */
    vals[3] = active_b;
    vals[4] = inactive_b;
    vals[5] = wired_b;
    /* cached/buffers/swap left unknown here (-1) */
  }

#elif defined(__FreeBSD__) || defined(__OpenBSD__) || defined(__NetBSD__) || \
    defined(__DragonFly__) || defined(__linux__)
  /* Portable-ish fallback: sysconf pages. */
  long pages = sysconf(_SC_PHYS_PAGES);
  long avpg = sysconf(_SC_AVPHYS_PAGES);
  long pgsz = sysconf(_SC_PAGESIZE);
  if (pages > 0 && pgsz > 0) vals[0] = (int64_t)pages * (int64_t)pgsz;
  if (avpg >= 0 && pgsz > 0) {
    vals[1] = (int64_t)avpg * (int64_t)pgsz;
    vals[2] = vals[1];
  }
#endif

  arr = alloc_int64_array_from(vals, 10);
  CAMLreturn(arr);
}

/* ---- Network interfaces ---- */

CAMLprim value caml_sys_metrics_net_ifaces(value unit) {
  CAMLparam1(unit);
  CAMLlocal4(arr, tup, namev, tmp);

#if defined(__APPLE__) || defined(__FreeBSD__) || defined(__OpenBSD__) || \
    defined(__NetBSD__) || defined(__DragonFly__)
  struct ifaddrs* ifap = NULL;
  if (getifaddrs(&ifap) != 0 || !ifap) {
    arr = caml_alloc(0, 0);
    CAMLreturn(arr);
  }

  /* Collect one entry per interface (AF_LINK entries carry stats). */
  typedef struct {
    char name[IFNAMSIZ];
    uint64_t rx_b, tx_b, rx_p, tx_p;
  } iface_acc;

  iface_acc acc[256];
  int acc_n = 0;

  for (struct ifaddrs* ifa = ifap; ifa; ifa = ifa->ifa_next) {
    if (!ifa->ifa_name || !ifa->ifa_addr) continue;
    if (ifa->ifa_addr->sa_family != AF_LINK) continue;
    if (!ifa->ifa_data) continue;

    struct if_data* d = (struct if_data*)ifa->ifa_data;

    /* linear search */
    int idx = -1;
    for (int i = 0; i < acc_n; i++) {
      if (strncmp(acc[i].name, ifa->ifa_name, IFNAMSIZ) == 0) {
        idx = i;
        break;
      }
    }
    if (idx < 0) {
      if (acc_n >= 256) continue;
      idx = acc_n++;
      strncpy(acc[idx].name, ifa->ifa_name, IFNAMSIZ);
      acc[idx].name[IFNAMSIZ - 1] = 0;
    }

    /* if_data counters (bytes/packets). On modern BSD/mac these are 64-bit. */
    acc[idx].rx_b = (uint64_t)d->ifi_ibytes;
    acc[idx].tx_b = (uint64_t)d->ifi_obytes;
    acc[idx].rx_p = (uint64_t)d->ifi_ipackets;
    acc[idx].tx_p = (uint64_t)d->ifi_opackets;
  }

  freeifaddrs(ifap);

  arr = caml_alloc((mlsize_t)acc_n, 0);
  for (int i = 0; i < acc_n; i++) {
    namev = caml_copy_string(acc[i].name);
    tup = caml_alloc_tuple(5);
    Store_field(tup, 0, namev);
    Store_field(tup, 1, caml_copy_int64_i((int64_t)acc[i].rx_b));
    Store_field(tup, 2, caml_copy_int64_i((int64_t)acc[i].tx_b));
    Store_field(tup, 3, caml_copy_int64_i((int64_t)acc[i].rx_p));
    Store_field(tup, 4, caml_copy_int64_i((int64_t)acc[i].tx_p));
    Store_field(arr, (mlsize_t)i, tup);
  }

  CAMLreturn(arr);
#else
  /* Linux uses /proc/net/dev in OCaml (more stable + richer). */
  arr = caml_alloc(0, 0);
  CAMLreturn(arr);
#endif
}

/* ---- Disk statvfs ---- */

CAMLprim value caml_sys_metrics_statvfs(value v_path) {
  CAMLparam1(v_path);
  CAMLlocal1(tup);

  const char* path = String_val(v_path);
  struct statvfs st;
  if (statvfs(path, &st) != 0) {
    caml_failwith("statvfs failed");
  }

  uint64_t fr =
      (st.f_frsize != 0) ? (uint64_t)st.f_frsize : (uint64_t)st.f_bsize;
  uint64_t total = fr * (uint64_t)st.f_blocks;
  uint64_t free = fr * (uint64_t)st.f_bfree;
  uint64_t avail = fr * (uint64_t)st.f_bavail;

  tup = caml_alloc_tuple(3);
  Store_field(tup, 0, caml_copy_int64_i((int64_t)total));
  Store_field(tup, 1, caml_copy_int64_i((int64_t)free));
  Store_field(tup, 2, caml_copy_int64_i((int64_t)avail));
  CAMLreturn(tup);
}

/* ---- Process (self) rss/vsz ---- */

CAMLprim value caml_sys_metrics_proc_self_mem(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(tup);

#if defined(__APPLE__)
  mach_task_basic_info_data_t info;
  mach_msg_type_number_t count = MACH_TASK_BASIC_INFO_COUNT;
  kern_return_t kr = task_info(mach_task_self(), MACH_TASK_BASIC_INFO,
                               (task_info_t)&info, &count);
  if (kr != KERN_SUCCESS) {
    tup = caml_alloc_tuple(2);
    Store_field(tup, 0, caml_copy_int64_i(-1));
    Store_field(tup, 1, caml_copy_int64_i(-1));
    CAMLreturn(tup);
  }
  tup = caml_alloc_tuple(2);
  Store_field(tup, 0, caml_copy_int64_i((int64_t)info.resident_size));
  Store_field(tup, 1, caml_copy_int64_i((int64_t)info.virtual_size));
  CAMLreturn(tup);
#else
  /* Linux handled in OCaml via /proc/self/stat; BSD left unsupported here. */
  tup = caml_alloc_tuple(2);
  Store_field(tup, 0, caml_copy_int64_i(-1));
  Store_field(tup, 1, caml_copy_int64_i(-1));
  CAMLreturn(tup);
#endif
}

/* ---- NVML dynamic loading (POSIX) ---- */

typedef struct nvmlDevice_st* nvmlDevice_t;
typedef unsigned int nvmlReturn_t;
typedef struct {
  unsigned int gpu;
  unsigned int memory;
} nvmlUtilization_t;
typedef struct {
  unsigned long long total;
  unsigned long long free;
  unsigned long long used;
} nvmlMemory_t;

typedef nvmlReturn_t (*p_nvmlInit_v2)(void);
typedef nvmlReturn_t (*p_nvmlInit)(void);
typedef nvmlReturn_t (*p_nvmlShutdown)(void);
typedef nvmlReturn_t (*p_nvmlDeviceGetCount_v2)(unsigned int*);
typedef nvmlReturn_t (*p_nvmlDeviceGetCount)(unsigned int*);
typedef nvmlReturn_t (*p_nvmlDeviceGetHandleByIndex_v2)(unsigned int,
                                                        nvmlDevice_t*);
typedef nvmlReturn_t (*p_nvmlDeviceGetHandleByIndex)(unsigned int,
                                                     nvmlDevice_t*);
typedef nvmlReturn_t (*p_nvmlDeviceGetName)(nvmlDevice_t, char*, unsigned int);
typedef nvmlReturn_t (*p_nvmlDeviceGetUtilizationRates)(nvmlDevice_t,
                                                        nvmlUtilization_t*);
typedef nvmlReturn_t (*p_nvmlDeviceGetMemoryInfo)(nvmlDevice_t, nvmlMemory_t*);
typedef nvmlReturn_t (*p_nvmlDeviceGetTemperature)(nvmlDevice_t, unsigned int,
                                                   unsigned int*);
typedef nvmlReturn_t (*p_nvmlDeviceGetPowerUsage)(nvmlDevice_t, unsigned int*);

static void* dlsym_req(void* h, const char* name) { return dlsym(h, name); }

CAMLprim value caml_sys_metrics_nvml_snapshot(value unit) {
  CAMLparam1(unit);
  CAMLlocal4(arr, tup, namev, tmp);

  void* h = dlopen("libnvidia-ml.so.1", RTLD_LAZY);
  if (!h) h = dlopen("libnvidia-ml.so", RTLD_LAZY);
  if (!h) {
    arr = caml_alloc(0, 0);
    CAMLreturn(arr);
  }

  p_nvmlInit_v2 nvmlInit_v2 = (p_nvmlInit_v2)dlsym_req(h, "nvmlInit_v2");
  p_nvmlInit nvmlInit = (p_nvmlInit)dlsym_req(h, "nvmlInit");
  p_nvmlShutdown nvmlShutdown = (p_nvmlShutdown)dlsym_req(h, "nvmlShutdown");
  p_nvmlDeviceGetCount_v2 nvmlDeviceGetCount_v2 =
      (p_nvmlDeviceGetCount_v2)dlsym_req(h, "nvmlDeviceGetCount_v2");
  p_nvmlDeviceGetCount nvmlDeviceGetCount =
      (p_nvmlDeviceGetCount)dlsym_req(h, "nvmlDeviceGetCount");
  p_nvmlDeviceGetHandleByIndex_v2 nvmlDeviceGetHandleByIndex_v2 =
      (p_nvmlDeviceGetHandleByIndex_v2)dlsym_req(
          h, "nvmlDeviceGetHandleByIndex_v2");
  p_nvmlDeviceGetHandleByIndex nvmlDeviceGetHandleByIndex =
      (p_nvmlDeviceGetHandleByIndex)dlsym_req(h, "nvmlDeviceGetHandleByIndex");
  p_nvmlDeviceGetName nvmlDeviceGetName =
      (p_nvmlDeviceGetName)dlsym_req(h, "nvmlDeviceGetName");
  p_nvmlDeviceGetUtilizationRates nvmlDeviceGetUtilizationRates =
      (p_nvmlDeviceGetUtilizationRates)dlsym_req(
          h, "nvmlDeviceGetUtilizationRates");
  p_nvmlDeviceGetMemoryInfo nvmlDeviceGetMemoryInfo =
      (p_nvmlDeviceGetMemoryInfo)dlsym_req(h, "nvmlDeviceGetMemoryInfo");
  p_nvmlDeviceGetTemperature nvmlDeviceGetTemperature =
      (p_nvmlDeviceGetTemperature)dlsym_req(h, "nvmlDeviceGetTemperature");
  p_nvmlDeviceGetPowerUsage nvmlDeviceGetPowerUsage =
      (p_nvmlDeviceGetPowerUsage)dlsym_req(h, "nvmlDeviceGetPowerUsage");

  if ((!nvmlInit_v2 && !nvmlInit) || !nvmlShutdown ||
      (!nvmlDeviceGetCount_v2 && !nvmlDeviceGetCount) ||
      (!nvmlDeviceGetHandleByIndex_v2 && !nvmlDeviceGetHandleByIndex) ||
      !nvmlDeviceGetName || !nvmlDeviceGetUtilizationRates ||
      !nvmlDeviceGetMemoryInfo) {
    dlclose(h);
    arr = caml_alloc(0, 0);
    CAMLreturn(arr);
  }

  nvmlReturn_t rc = nvmlInit_v2 ? nvmlInit_v2() : nvmlInit();
  if (rc != 0) {
    dlclose(h);
    arr = caml_alloc(0, 0);
    CAMLreturn(arr);
  }

  unsigned int count = 0;
  rc = nvmlDeviceGetCount_v2 ? nvmlDeviceGetCount_v2(&count)
                             : nvmlDeviceGetCount(&count);
  if (rc != 0 || count == 0) {
    nvmlShutdown();
    dlclose(h);
    arr = caml_alloc(0, 0);
    CAMLreturn(arr);
  }

  arr = caml_alloc(count, 0);

  for (unsigned int i = 0; i < count; i++) {
    nvmlDevice_t dev;
    rc = nvmlDeviceGetHandleByIndex_v2 ? nvmlDeviceGetHandleByIndex_v2(i, &dev)
                                       : nvmlDeviceGetHandleByIndex(i, &dev);
    if (rc != 0) continue;

    char name[96];
    name[0] = 0;
    nvmlUtilization_t util;
    memset(&util, 0, sizeof(util));
    nvmlMemory_t mem;
    memset(&mem, 0, sizeof(mem));

    nvmlDeviceGetName(dev, name, sizeof(name));
    nvmlDeviceGetUtilizationRates(dev, &util);
    nvmlDeviceGetMemoryInfo(dev, &mem);

    unsigned int temp = 0;
    unsigned int power = 0;
    if (nvmlDeviceGetTemperature)
      nvmlDeviceGetTemperature(dev, 0 /* NVML_TEMPERATURE_GPU */, &temp);
    if (nvmlDeviceGetPowerUsage) nvmlDeviceGetPowerUsage(dev, &power);

    namev = caml_copy_string(name);
    tup = caml_alloc_tuple(9);
    Store_field(tup, 0, Val_int((int)i));
    Store_field(tup, 1, namev);
    Store_field(tup, 2, Val_int((int)util.gpu));
    Store_field(tup, 3, Val_int((int)util.memory));
    Store_field(tup, 4, caml_copy_int64_i((int64_t)mem.total));
    Store_field(tup, 5, caml_copy_int64_i((int64_t)mem.used));
    Store_field(tup, 6, caml_copy_int64_i((int64_t)mem.free));
    Store_field(tup, 7, Val_int((int)temp));
    Store_field(tup, 8, Val_int((int)power));

    Store_field(arr, i, tup);
  }

  nvmlShutdown();
  dlclose(h);
  CAMLreturn(arr);
}

/* ---- clock_gettime_ns (POSIX) ---- */

#include <sys/utsname.h>
#include <time.h>

CAMLprim value caml_sys_metrics_clock_gettime_ns(value unit) {
  CAMLparam1(unit);
  struct timespec ts;
  if (clock_gettime(CLOCK_MONOTONIC, &ts) != 0) {
    caml_failwith("clock_gettime failed");
  }
  int64_t ns = (int64_t)ts.tv_sec * 1000000000LL + (int64_t)ts.tv_nsec;
  CAMLreturn(caml_copy_int64(ns));
}

CAMLprim value caml_sys_metrics_uname_sysname(value unit) {
  CAMLparam1(unit);
  struct utsname buf;
  if (uname(&buf) != 0) {
    CAMLreturn(caml_copy_string("Unix"));
  }
  CAMLreturn(caml_copy_string(buf.sysname));
}

#endif /* _WIN32 */
