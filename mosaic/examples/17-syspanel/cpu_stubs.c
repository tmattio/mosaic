#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

#ifdef __APPLE__
#include <mach/mach.h>
#include <mach/host_info.h>
#include <mach/mach_host.h>

/* Get CPU load statistics using host_statistics */
CAMLprim value ocaml_get_cpu_load(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  
  host_cpu_load_info_data_t cpu_load;
  mach_msg_type_number_t count = HOST_CPU_LOAD_INFO_COUNT;
  kern_return_t status;
  
  status = host_statistics(mach_host_self(), HOST_CPU_LOAD_INFO,
                          (host_info_t)&cpu_load, &count);
  
  if (status != KERN_SUCCESS) {
    caml_failwith("host_statistics failed");
  }
  
  /* Return tuple: (user, system, idle, nice) */
  /* natural_t is unsigned int, convert to int64 for OCaml */
  result = caml_alloc(4, 0);
  Store_field(result, 0, caml_copy_int64((int64_t)cpu_load.cpu_ticks[CPU_STATE_USER]));
  Store_field(result, 1, caml_copy_int64((int64_t)cpu_load.cpu_ticks[CPU_STATE_SYSTEM]));
  Store_field(result, 2, caml_copy_int64((int64_t)cpu_load.cpu_ticks[CPU_STATE_IDLE]));
  Store_field(result, 3, caml_copy_int64((int64_t)cpu_load.cpu_ticks[CPU_STATE_NICE]));
  
  CAMLreturn(result);
}

#else
/* Non-macOS: return dummy values */
CAMLprim value ocaml_get_cpu_load(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(result);
  
  result = caml_alloc(4, 0);
  Store_field(result, 0, caml_copy_int64(0));
  Store_field(result, 1, caml_copy_int64(0));
  Store_field(result, 2, caml_copy_int64(0));
  Store_field(result, 3, caml_copy_int64(0));
  
  CAMLreturn(result);
}
#endif

