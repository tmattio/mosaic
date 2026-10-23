/* The terminal device: its window, its line settings, the monotonic clock. */

#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
/* On the Windows ports Unix.file_descr is a custom block wrapping a HANDLE,
   accessed with Handle_val; on Unix it is an immediate int. Both ports ship
   caml/unixsupport.h. */
#include <caml/unixsupport.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/ioctl.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>
#endif

/* The window, [Some (cols, rows)], or [None] where the descriptor reports
   none. */
CAMLprim value matrix_tty_window(value fd) {
  CAMLparam1(fd);
  CAMLlocal2(size, result);
  int cols = 0, rows = 0;
#ifdef _WIN32
  CONSOLE_SCREEN_BUFFER_INFO csbi;
  HANDLE h = Handle_val(fd);
  if (h != INVALID_HANDLE_VALUE && GetConsoleScreenBufferInfo(h, &csbi)) {
    cols = csbi.srWindow.Right - csbi.srWindow.Left + 1;
    rows = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
  }
#else
  struct winsize ws;
  if (ioctl(Int_val(fd), TIOCGWINSZ, &ws) == 0) {
    cols = ws.ws_col;
    rows = ws.ws_row;
  }
#endif
  if (cols <= 0 || rows <= 0) CAMLreturn(Val_none);
  size = caml_alloc_tuple(2);
  Store_field(size, 0, Val_int(cols));
  Store_field(size, 1, Val_int(rows));
  result = caml_alloc_some(size);
  CAMLreturn(result);
}

CAMLprim value matrix_tty_enable_vt(value fd) {
  CAMLparam1(fd);
#ifdef _WIN32
  HANDLE h = Handle_val(fd);
  if (h == INVALID_HANDLE_VALUE) caml_failwith("Invalid handle");
  DWORD mode;
  if (!GetConsoleMode(h, &mode)) caml_failwith("GetConsoleMode failed");
  if (!SetConsoleMode(h, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING))
    caml_failwith("SetConsoleMode failed");
  /* UTF-8 output. This affects the whole process, not only this handle. */
  SetConsoleOutputCP(CP_UTF8);
#else
  (void)fd;
#endif
  CAMLreturn(Val_unit);
}

/* IEXTEN is not representable in OCaml's Unix.terminal_io, so the settings
   reach it through these. With IEXTEN set, the line discipline processes
   VDISCARD (^O) and VLNEXT (^V) even in non-canonical mode, and those bytes
   never reach the program. */

CAMLprim value matrix_tty_get_iexten(value fd) {
  CAMLparam1(fd);
#ifdef _WIN32
  (void)fd;
  CAMLreturn(Val_false);
#else
  struct termios tio;
  if (tcgetattr(Int_val(fd), &tio) != 0) caml_uerror("tcgetattr", Nothing);
  CAMLreturn(Val_bool((tio.c_lflag & IEXTEN) != 0));
#endif
}

CAMLprim value matrix_tty_set_iexten(value fd, value on) {
  CAMLparam2(fd, on);
#ifdef _WIN32
  (void)fd;
  (void)on;
  CAMLreturn(Val_unit);
#else
  struct termios tio;
  if (tcgetattr(Int_val(fd), &tio) != 0) caml_uerror("tcgetattr", Nothing);
  if (Bool_val(on))
    tio.c_lflag |= IEXTEN;
  else
    tio.c_lflag &= ~(tcflag_t)IEXTEN;
  if (tcsetattr(Int_val(fd), TCSANOW, &tio) != 0)
    caml_uerror("tcsetattr", Nothing);
  CAMLreturn(Val_unit);
#endif
}

/* Whether [fd] is the process's controlling terminal: only that terminal
   has a foreground process group the process can read. */
CAMLprim value matrix_tty_is_controlling(value fd) {
#ifdef _WIN32
  (void)fd;
  return Val_false;
#else
  return Val_bool(tcgetpgrp(Int_val(fd)) != -1);
#endif
}

/* OCaml has no monotonic clock in its distribution. */
CAMLprim value matrix_tty_now(value unit) {
  (void)unit;
#ifdef _WIN32
  static LARGE_INTEGER frequency;
  LARGE_INTEGER counter;
  if (frequency.QuadPart == 0) QueryPerformanceFrequency(&frequency);
  QueryPerformanceCounter(&counter);
  return Val_long((intnat)(counter.QuadPart / frequency.QuadPart) * 1000000000 +
                  (intnat)(counter.QuadPart % frequency.QuadPart) * 1000000000 /
                      frequency.QuadPart);
#else
  struct timespec ts;
  clock_gettime(CLOCK_MONOTONIC, &ts);
  return Val_long((intnat)ts.tv_sec * 1000000000 + ts.tv_nsec);
#endif
}
