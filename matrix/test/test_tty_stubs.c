/* The call that makes a terminal the controlling terminal of the calling
   session leader, which OCaml's Unix library lacks. */

#include <caml/mlvalues.h>
#include <caml/unixsupport.h>
#include <sys/ioctl.h>
#include <termios.h>

value test_tty_set_controlling(value fd) {
  if (ioctl(Int_val(fd), TIOCSCTTY, 0) != 0) uerror("ioctl", Nothing);
  return Val_unit;
}
