/* The process-group calls a job-control shell makes, which OCaml's Unix
   library lacks. */

#include <caml/fail.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <unistd.h>

value runtime_job_setpgid(value pid, value pgid) {
  if (setpgid(Int_val(pid), Int_val(pgid)) != 0) uerror("setpgid", Nothing);
  return Val_unit;
}

value runtime_job_tcsetpgrp(value fd, value pgid) {
  if (tcsetpgrp(Int_val(fd), Int_val(pgid)) != 0) uerror("tcsetpgrp", Nothing);
  return Val_unit;
}
