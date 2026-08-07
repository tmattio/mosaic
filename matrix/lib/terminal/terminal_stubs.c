#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
// On the Windows ports Unix.file_descr is a custom block wrapping a HANDLE,
// accessed with Handle_val; on Unix it is an immediate int. Both ports ship
// caml/unixsupport.h.
#include <caml/unixsupport.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>
#endif

CAMLprim value terminal_get_size(value fd_val) {
  CAMLparam1(fd_val);
  CAMLlocal1(result);
  int width = 80, height = 24;

#ifdef _WIN32
  CONSOLE_SCREEN_BUFFER_INFO csbi;
  HANDLE h = Handle_val(fd_val);
  if (h != INVALID_HANDLE_VALUE && GetConsoleScreenBufferInfo(h, &csbi)) {
    width = csbi.srWindow.Right - csbi.srWindow.Left + 1;
    height = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
  }
#else
  int fd = Int_val(fd_val);
  struct winsize ws;
  if (ioctl(fd, TIOCGWINSZ, &ws) == 0) {
    width = ws.ws_col;
    height = ws.ws_row;
  }
#endif

  if (width <= 0 || height <= 0) {
    width = 80;
    height = 24;
  }

  result = caml_alloc(2, 0);
  Store_field(result, 0, Val_int(width));
  Store_field(result, 1, Val_int(height));
  CAMLreturn(result);
}

CAMLprim value terminal_enable_vt(value fd_val) {
  CAMLparam1(fd_val);
#ifdef _WIN32
  HANDLE h = Handle_val(fd_val);
  if (h == INVALID_HANDLE_VALUE) {
    caml_failwith("Invalid handle");
  }
  DWORD mode;
  if (!GetConsoleMode(h, &mode)) {
    caml_failwith("GetConsoleMode failed");
  }
  // Enable VT processing and UTF-8 support
  if (!SetConsoleMode(h, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING)) {
    caml_failwith("SetConsoleMode failed");
  }
  // Enable UTF-8 output for better Unicode support.
  // Note: This affects the entire process, not just this terminal handle.
  SetConsoleOutputCP(CP_UTF8);
#endif
  CAMLreturn(Val_unit);
}

// IEXTEN is not representable in OCaml's Unix.terminal_io, so raw-mode setup
// reaches it through these stubs. With IEXTEN set, the line discipline
// processes VDISCARD (^O) and VLNEXT (^V) even in non-canonical mode and
// those bytes never reach the application.

CAMLprim value terminal_get_iexten(value fd_val) {
  CAMLparam1(fd_val);
#ifdef _WIN32
  CAMLreturn(Val_bool(0));
#else
  struct termios tio;
  if (tcgetattr(Int_val(fd_val), &tio) != 0) {
    uerror("tcgetattr", Nothing);
  }
  CAMLreturn(Val_bool((tio.c_lflag & IEXTEN) != 0));
#endif
}

CAMLprim value terminal_set_iexten(value fd_val, value on_val) {
  CAMLparam2(fd_val, on_val);
#ifdef _WIN32
  (void)on_val;
  CAMLreturn(Val_unit);
#else
  int fd = Int_val(fd_val);
  struct termios tio;
  if (tcgetattr(fd, &tio) != 0) {
    uerror("tcgetattr", Nothing);
  }
  if (Bool_val(on_val)) {
    tio.c_lflag |= IEXTEN;
  } else {
    tio.c_lflag &= ~(tcflag_t)IEXTEN;
  }
  if (tcsetattr(fd, TCSANOW, &tio) != 0) {
    uerror("tcsetattr", Nothing);
  }
  CAMLreturn(Val_unit);
#endif
}
