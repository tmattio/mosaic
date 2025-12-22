#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <stdint.h>

#ifdef __APPLE__
#include <mach/mach.h>
#include <mach/host_info.h>
#include <mach/mach_host.h>

static value caml_copy_int64_i(int64_t x) { return caml_copy_int64(x); }

static value alloc_int64_array_from(const int64_t* vals, mlsize_t n) {
  CAMLparam0();
  CAMLlocal1(arr);
  arr = caml_alloc(n, 0);
  for (mlsize_t i = 0; i < n; i++)
    Store_field(arr, i, caml_copy_int64_i(vals[i]));
  CAMLreturn(arr);
}

static value alloc_cpu_row(int64_t user, int64_t nice, int64_t sys, int64_t idle) {
  int64_t vals[4];
  vals[0] = user;
  vals[1] = nice;
  vals[2] = sys;
  vals[3] = idle;
  return alloc_int64_array_from(vals, 4);
}

/* Get CPU load statistics using host_processor_info (returns per-core + total) */
CAMLprim value caml_metrics_get_cpu_load(value unit) {
  CAMLparam1(unit);
  CAMLlocal3(outer, row, totalrow);
  
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
    
    row = alloc_cpu_row(user, nice, sys, idle);
    Store_field(outer, (mlsize_t)(i + 1), row);
  }
  
  totalrow = alloc_cpu_row(sum_user, sum_nice, sum_sys, sum_idle);
  Store_field(outer, 0, totalrow);
  
  vm_deallocate(mach_task_self(), (vm_address_t)cpuInfo,
                (vm_size_t)(numCpuInfo * sizeof(integer_t)));
  CAMLreturn(outer);
}

#else
/* Non-macOS: return empty array (Linux uses /proc/stat in OCaml) */
CAMLprim value caml_metrics_get_cpu_load(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  
  result = caml_alloc(0, 0);
  CAMLreturn(result);
}
#endif

