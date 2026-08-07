// POSIX PTY stubs. On platforms without POSIX PTYs — including Windows —
// every entry point raises Unix_error(ENOSYS, ...). A previous ConPTY
// backend stored a custom block where the OCaml side expects a
// Unix.file_descr, which made every I/O call reinterpret unrelated memory
// as a handle; failing loudly is the honest scope until a sound Windows
// implementation exists.

#if defined(__linux__) || defined(__APPLE__) || defined(__FreeBSD__) || \
    defined(__DragonFly__) || defined(__NetBSD__) || defined(__OpenBSD__)
#define MATRIX_HAS_POSIX_PTY 1
#endif

#ifdef MATRIX_HAS_POSIX_PTY
#define _GNU_SOURCE  // For ptsname on Linux
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

// Feature test for ptsname_r availability
#ifdef __GLIBC__
#include <limits.h>
#endif
#endif  // MATRIX_HAS_POSIX_PTY

#include <errno.h>

// OCaml C FFI headers
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

#ifdef MATRIX_HAS_POSIX_PTY

// A C struct to hold the file descriptor pair.
struct pty_fds {
  int master_fd;
  int slave_fd;
};

// This is the modern, portable way to open a PTY.
struct pty_fds c_open_pty_posix() {
  struct pty_fds fds = {-1, -1};

  // Enter blocking section for potentially blocking syscalls
  caml_enter_blocking_section();

  // O_CLOEXEC prevents the file descriptor from being inherited by child
  // processes created by exec functions. This is a good security practice.
  int master_fd = posix_openpt(O_RDWR | O_NOCTTY | O_CLOEXEC);
  if (master_fd == -1) {
    caml_leave_blocking_section();
    return fds;
  }

  // Grant permissions to the slave device
  if (grantpt(master_fd) == -1) {
    close(master_fd);
    caml_leave_blocking_section();
    return fds;
  }

  // Unlock the slave device
  if (unlockpt(master_fd) == -1) {
    close(master_fd);
    caml_leave_blocking_section();
    return fds;
  }

  caml_leave_blocking_section();

  // Get slave name - use thread-safe ptsname_r where available
  char *slave_name = NULL;
#ifdef __GLIBC__
  char slave_name_buf[PATH_MAX];
  if (ptsname_r(master_fd, slave_name_buf, sizeof(slave_name_buf)) != 0) {
    close(master_fd);
    return fds;
  }
  slave_name = slave_name_buf;
#else
  slave_name = ptsname(master_fd);
  if (slave_name == NULL) {
    close(master_fd);
    return fds;
  }
#endif

  caml_enter_blocking_section();
  fds.slave_fd = open(slave_name, O_RDWR | O_NOCTTY);
  caml_leave_blocking_section();

  if (fds.slave_fd == -1) {
    close(master_fd);
    // CRITICAL FIX: Reset master_fd to indicate failure
    fds.master_fd = -1;
    fds.slave_fd = -1;
    return fds;
  }

  fds.master_fd = master_fd;
  return fds;
}

CAMLprim value ocaml_pty_open(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(res);

  struct pty_fds fds = c_open_pty_posix();

  if (fds.master_fd == -1) {
    uerror("open_pty", Nothing);
  }

  res = caml_alloc(2, 0);
  Store_field(res, 0, Val_int(fds.master_fd));
  Store_field(res, 1, Val_int(fds.slave_fd));
  CAMLreturn(res);
}

CAMLprim value ocaml_pty_get_winsize(value fd) {
  CAMLparam1(fd);
  CAMLlocal1(res);
  struct winsize ws;

  caml_enter_blocking_section();
  int ret = ioctl(Int_val(fd), TIOCGWINSZ, &ws);
  caml_leave_blocking_section();

  if (ret == -1) {
    uerror("ioctl(TIOCGWINSZ)", Nothing);
  }

  res = caml_alloc(4, 0);
  Store_field(res, 0, Val_int(ws.ws_row));
  Store_field(res, 1, Val_int(ws.ws_col));
  Store_field(res, 2, Val_int(ws.ws_xpixel));
  Store_field(res, 3, Val_int(ws.ws_ypixel));
  CAMLreturn(res);
}

// Byte version for set_winsize is needed for records with non-float fields
CAMLprim value ocaml_pty_set_winsize_byte(value fd, value ws_val) {
  CAMLparam2(fd, ws_val);
  struct winsize ws;
  ws.ws_row = Int_val(Field(ws_val, 0));
  ws.ws_col = Int_val(Field(ws_val, 1));
  ws.ws_xpixel = Int_val(Field(ws_val, 2));
  ws.ws_ypixel = Int_val(Field(ws_val, 3));

  caml_enter_blocking_section();
  int ret = ioctl(Int_val(fd), TIOCSWINSZ, &ws);
  caml_leave_blocking_section();

  if (ret == -1) {
    uerror("ioctl(TIOCSWINSZ)", Nothing);
  }
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pty_set_winsize(value fd, value ws_val) {
  return ocaml_pty_set_winsize_byte(fd, ws_val);
}

CAMLprim value ocaml_pty_setsid_and_setctty(value slave_fd) {
  CAMLparam1(slave_fd);
  if (setsid() == -1) {
    uerror("setsid", Nothing);
  }
  // The flag `0` for ioctl on TIOCSCTTY is what's typically used.
  if (ioctl(Int_val(slave_fd), TIOCSCTTY, 0) == -1) {
    uerror("ioctl(TIOCSCTTY)", Nothing);
  }
  CAMLreturn(Val_unit);
}

#else  // Unsupported platforms, including Windows: fail loudly.

static value pty_unsupported(const char *cmd) {
  errno = ENOSYS;
  uerror(cmd, caml_copy_string("PTYs are not supported on this platform"));
  return Val_unit;  // Unreachable
}

CAMLprim value ocaml_pty_open(value unit) {
  (void)unit;
  return pty_unsupported("open_pty");
}
CAMLprim value ocaml_pty_get_winsize(value fd) {
  (void)fd;
  return pty_unsupported("get_winsize");
}
CAMLprim value ocaml_pty_set_winsize_byte(value fd, value ws_val) {
  (void)fd;
  (void)ws_val;
  return pty_unsupported("set_winsize");
}
CAMLprim value ocaml_pty_set_winsize(value fd, value ws_val) {
  return ocaml_pty_set_winsize_byte(fd, ws_val);
}
CAMLprim value ocaml_pty_setsid_and_setctty(value slave_fd) {
  (void)slave_fd;
  return pty_unsupported("setsid_and_setctty");
}

#endif  // MATRIX_HAS_POSIX_PTY
