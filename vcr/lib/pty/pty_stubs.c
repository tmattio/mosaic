#define _GNU_SOURCE  // For ptsname on Linux
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

// OCaml C FFI headers
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

// This implementation will work for both Linux and macOS (Darwin)
#if defined(__linux__) || defined(__APPLE__)

// A C struct to hold the file descriptor pair.
struct pty_fds {
  int master_fd;
  int slave_fd;
};

// This is the modern, portable way to open a PTY.
struct pty_fds c_open_pty_posix() {
  struct pty_fds fds = {-1, -1};
  // O_CLOEXEC prevents the file descriptor from being inherited by child
  // processes created by exec functions. This is a good security practice.
  int master_fd = posix_openpt(O_RDWR | O_NOCTTY | O_CLOEXEC);
  if (master_fd == -1) {
    return fds;
  }

  // Grant permissions to the slave device
  if (grantpt(master_fd) == -1) {
    close(master_fd);
    return fds;
  }

  // Unlock the slave device
  if (unlockpt(master_fd) == -1) {
    close(master_fd);
    return fds;
  }

  char *slave_name = ptsname(master_fd);
  if (slave_name == NULL) {
    close(master_fd);
    return fds;
  }

  fds.slave_fd = open(slave_name, O_RDWR | O_NOCTTY);
  if (fds.slave_fd == -1) {
    close(master_fd);
    // Reset slave_fd to indicate failure
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

  if (ioctl(Int_val(fd), TIOCGWINSZ, &ws) == -1) {
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

  if (ioctl(Int_val(fd), TIOCSWINSZ, &ws) == -1) {
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

// Helper to raise Unix error with current errno after fork failure
CAMLprim value ocaml_raise_fork_error(value unit) {
  CAMLparam1(unit);
  uerror("fork", Nothing);  // uerror automatically uses errno
  CAMLreturn(Val_unit);     // Unreachable
}

#else  // For Windows and other unsupported platforms

CAMLprim value ocaml_pty_open(value unit) {
  uerror("open_pty", caml_copy_string("unsupported platform"));
  return Val_unit;  // Unreachable
}
CAMLprim value ocaml_pty_get_winsize(value fd) {
  uerror("get_winsize", caml_copy_string("unsupported platform"));
  return Val_unit;
}
CAMLprim value ocaml_pty_set_winsize_byte(value fd, value ws_val) {
  uerror("set_winsize", caml_copy_string("unsupported platform"));
  return Val_unit;
}
CAMLprim value ocaml_pty_set_winsize(value fd, value ws_val) {
  return ocaml_pty_set_winsize_byte(fd, ws_val);
}
CAMLprim value ocaml_pty_setsid_and_setctty(value slave_fd) {
  uerror("setsid_and_setctty", caml_copy_string("unsupported platform"));
  return Val_unit;
}

#endif
