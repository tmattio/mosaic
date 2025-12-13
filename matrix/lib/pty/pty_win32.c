#ifdef _WIN32

// Require Windows 10 version 1809 or later for ConPTY API
#ifndef _WIN32_WINNT
#define _WIN32_WINNT 0x0A00
#endif

// Include winsock2.h before windows.h to avoid warning
#include <winsock2.h>
#include <windows.h>
#include <consoleapi.h>  // For HPCON and ConPTY functions
#include <process.h>
#include <stdlib.h>
#include <string.h>

// OCaml C FFI headers
#include <caml/alloc.h>
#include <caml/custom.h>  // For custom_operations
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/unixsupport.h>

// ConPTY API (available in Windows 10 1809+)
typedef HRESULT(WINAPI *CreatePseudoConsole_t)(COORD, HANDLE, HANDLE, DWORD,
                                               HPCON *);
typedef void(WINAPI *ClosePseudoConsole_t)(HPCON);
typedef HRESULT(WINAPI *ResizePseudoConsole_t)(HPCON, COORD);

static CreatePseudoConsole_t pCreatePseudoConsole = NULL;
static ClosePseudoConsole_t pClosePseudoConsole = NULL;
static ResizePseudoConsole_t pResizePseudoConsole = NULL;
static BOOL conpty_initialized = FALSE;

static void init_conpty() {
  if (conpty_initialized) return;

  HMODULE hKernel32 = GetModuleHandleW(L"kernel32.dll");
  if (hKernel32) {
    pCreatePseudoConsole =
        (CreatePseudoConsole_t)GetProcAddress(hKernel32, "CreatePseudoConsole");
    pClosePseudoConsole =
        (ClosePseudoConsole_t)GetProcAddress(hKernel32, "ClosePseudoConsole");
    pResizePseudoConsole =
        (ResizePseudoConsole_t)GetProcAddress(hKernel32, "ResizePseudoConsole");
  }
  conpty_initialized = TRUE;
}

// Custom structure to hold PTY data on Windows
typedef struct {
  HANDLE hReadPipe;
  HANDLE hWritePipe;
  HPCON hPC;
  PROCESS_INFORMATION piClient;
  COORD currentSize;  // Track current size since ConPTY doesn't expose query
} win_pty_t;

// Custom operations for GC-safe handling of win_pty_t
static void win_pty_finalize(value v) {
  win_pty_t *pty = (win_pty_t *)Data_custom_val(v);

  // Close handles if still open
  if (pty->hReadPipe != INVALID_HANDLE_VALUE) {
    CloseHandle(pty->hReadPipe);
    pty->hReadPipe = INVALID_HANDLE_VALUE;
  }
  if (pty->hWritePipe != INVALID_HANDLE_VALUE) {
    CloseHandle(pty->hWritePipe);
    pty->hWritePipe = INVALID_HANDLE_VALUE;
  }

  // Close ConPTY handle
  if (pty->hPC != NULL && pClosePseudoConsole) {
    pClosePseudoConsole(pty->hPC);
    pty->hPC = NULL;
  }

  // Close process handles
  if (pty->piClient.hProcess != NULL) {
    CloseHandle(pty->piClient.hProcess);
    pty->piClient.hProcess = NULL;
  }
  if (pty->piClient.hThread != NULL) {
    CloseHandle(pty->piClient.hThread);
    pty->piClient.hThread = NULL;
  }
}

static struct custom_operations win_pty_ops = {
  "com.matrix.pty.win_pty",
  win_pty_finalize,
  custom_compare_default,
  custom_hash_default,
  custom_serialize_default,
  custom_deserialize_default,
  custom_compare_ext_default,
  custom_fixed_length_default
};

// Helper to allocate a custom block for win_pty_t
static value alloc_win_pty() {
  value v = caml_alloc_custom(&win_pty_ops, sizeof(win_pty_t), 0, 1);
  win_pty_t *pty = (win_pty_t *)Data_custom_val(v);
  memset(pty, 0, sizeof(win_pty_t));
  pty->hReadPipe = INVALID_HANDLE_VALUE;
  pty->hWritePipe = INVALID_HANDLE_VALUE;
  pty->hPC = NULL;
  pty->currentSize.X = 80;
  pty->currentSize.Y = 24;
  return v;
}

// Windows implementation of ocaml_pty_open
CAMLprim value ocaml_pty_open(value unit) {
  CAMLparam1(unit);
  CAMLlocal2(res, master_val);

  init_conpty();
  if (!pCreatePseudoConsole) {
    uerror("CreatePseudoConsole", caml_copy_string("ConPTY not available"));
  }

  // Create pipes for ConPTY
  HANDLE hPipePTYIn, hPipePTYOut;
  HANDLE hPipeIn, hPipeOut;

  if (!CreatePipe(&hPipePTYIn, &hPipeOut, NULL, 0) ||
      !CreatePipe(&hPipeIn, &hPipePTYOut, NULL, 0)) {
    uerror("CreatePipe", Nothing);
  }

  // Create ConPTY
  COORD consoleSize = {80, 24};
  HPCON hPC;
  HRESULT hr =
      pCreatePseudoConsole(consoleSize, hPipePTYIn, hPipePTYOut, 0, &hPC);
  if (FAILED(hr)) {
    CloseHandle(hPipePTYIn);
    CloseHandle(hPipePTYOut);
    CloseHandle(hPipeIn);
    CloseHandle(hPipeOut);
    uerror("CreatePseudoConsole", Nothing);
  }

  // Close the PTY ends of the pipes (ConPTY will use them)
  CloseHandle(hPipePTYIn);
  CloseHandle(hPipePTYOut);

  // Allocate custom block to store handles
  master_val = alloc_win_pty();
  win_pty_t *pty = (win_pty_t *)Data_custom_val(master_val);
  pty->hReadPipe = hPipeIn;
  pty->hWritePipe = hPipeOut;
  pty->hPC = hPC;
  pty->currentSize = consoleSize;

  // Return pair (master, slave) - both pointing to same custom block on Windows
  res = caml_alloc(2, 0);
  Store_field(res, 0, master_val);  // Master (our end)
  Store_field(res, 1, master_val);  // Slave (same on Windows)
  CAMLreturn(res);
}

CAMLprim value ocaml_pty_get_winsize(value fd) {
  CAMLparam1(fd);
  CAMLlocal1(res);

  // Return tracked size (ConPTY doesn't expose a query API)
  win_pty_t *pty = (win_pty_t *)Data_custom_val(fd);

  res = caml_alloc(4, 0);
  Store_field(res, 0, Val_int(pty->currentSize.Y));  // rows
  Store_field(res, 1, Val_int(pty->currentSize.X));  // cols
  Store_field(res, 2, Val_int(0));                   // xpixel
  Store_field(res, 3, Val_int(0));                   // ypixel
  CAMLreturn(res);
}

CAMLprim value ocaml_pty_set_winsize_byte(value fd, value ws_val) {
  CAMLparam2(fd, ws_val);

  init_conpty();
  if (!pResizePseudoConsole) {
    uerror("ResizePseudoConsole", caml_copy_string("ConPTY not available"));
  }

  win_pty_t *pty = (win_pty_t *)Data_custom_val(fd);
  COORD newSize;
  newSize.Y = Int_val(Field(ws_val, 0));  // rows
  newSize.X = Int_val(Field(ws_val, 1));  // cols

  HRESULT hr = pResizePseudoConsole(pty->hPC, newSize);
  if (FAILED(hr)) {
    uerror("ResizePseudoConsole", Nothing);
  }

  // Track the new size
  pty->currentSize = newSize;

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_pty_set_winsize(value fd, value ws_val) {
  return ocaml_pty_set_winsize_byte(fd, ws_val);
}

CAMLprim value ocaml_pty_setsid_and_setctty(value slave_fd) {
  CAMLparam1(slave_fd);
  // No-op on Windows
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_raise_fork_error(value unit) {
  CAMLparam1(unit);
  uerror("CreateProcess", Nothing);
  CAMLreturn(Val_unit);
}

// Windows I/O operations for ConPTY
// Note: On Windows, we need to override Unix I/O since fd is a custom block
CAMLprim value ocaml_pty_read(value fd_val, value buf_val, value ofs_val,
                               value len_val) {
  CAMLparam4(fd_val, buf_val, ofs_val, len_val);
  win_pty_t *pty = (win_pty_t *)Data_custom_val(fd_val);
  int ofs = Int_val(ofs_val);
  int len = Int_val(len_val);
  DWORD bytesRead = 0;

  caml_enter_blocking_section();
  BOOL success =
      ReadFile(pty->hReadPipe, Bytes_val(buf_val) + ofs, len, &bytesRead, NULL);
  caml_leave_blocking_section();

  if (!success && GetLastError() != ERROR_BROKEN_PIPE) {
    uerror("ReadFile", Nothing);
  }

  CAMLreturn(Val_int(bytesRead));
}

CAMLprim value ocaml_pty_write(value fd_val, value buf_val, value ofs_val,
                                value len_val) {
  CAMLparam4(fd_val, buf_val, ofs_val, len_val);
  win_pty_t *pty = (win_pty_t *)Data_custom_val(fd_val);
  int ofs = Int_val(ofs_val);
  int len = Int_val(len_val);
  DWORD bytesWritten = 0;

  caml_enter_blocking_section();
  BOOL success = WriteFile(pty->hWritePipe, Bytes_val(buf_val) + ofs, len,
                           &bytesWritten, NULL);
  caml_leave_blocking_section();

  if (!success) {
    uerror("WriteFile", Nothing);
  }

  CAMLreturn(Val_int(bytesWritten));
}

CAMLprim value ocaml_pty_close(value fd_val) {
  CAMLparam1(fd_val);
  win_pty_t *pty = (win_pty_t *)Data_custom_val(fd_val);

  // Finalize will handle cleanup, but we can trigger early close
  if (pty->hReadPipe != INVALID_HANDLE_VALUE) {
    CloseHandle(pty->hReadPipe);
    pty->hReadPipe = INVALID_HANDLE_VALUE;
  }
  if (pty->hWritePipe != INVALID_HANDLE_VALUE) {
    CloseHandle(pty->hWritePipe);
    pty->hWritePipe = INVALID_HANDLE_VALUE;
  }

  CAMLreturn(Val_unit);
}

#endif  // _WIN32