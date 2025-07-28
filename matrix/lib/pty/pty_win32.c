#ifdef _WIN32

#include <process.h>
#include <stdlib.h>
#include <string.h>
#include <windows.h>

// OCaml C FFI headers
#include <caml/alloc.h>
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
} win_pty_t;

// Windows implementation of ocaml_pty_open
CAMLprim value ocaml_pty_open(value unit) {
  CAMLparam1(unit);
  CAMLlocal1(res);

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
  win_pty_t *pty = (win_pty_t *)malloc(sizeof(win_pty_t));
  pty->hReadPipe = hPipeIn;
  pty->hWritePipe = hPipeOut;
  pty->hPC = hPC;
  memset(&pty->piClient, 0, sizeof(PROCESS_INFORMATION));

  // Return as abstract value
  res = caml_alloc(2, 0);
  Store_field(res, 0, (value)pty);  // Master (our end)
  Store_field(res, 1,
              (value)pty);  // Slave (child's end - same handle in Windows)
  CAMLreturn(res);
}

CAMLprim value ocaml_pty_get_winsize(value fd) {
  CAMLparam1(fd);
  CAMLlocal1(res);

  // Default to 80x24 for now
  res = caml_alloc(4, 0);
  Store_field(res, 0, Val_int(24));  // rows
  Store_field(res, 1, Val_int(80));  // cols
  Store_field(res, 2, Val_int(0));   // xpixel
  Store_field(res, 3, Val_int(0));   // ypixel
  CAMLreturn(res);
}

CAMLprim value ocaml_pty_set_winsize_byte(value fd, value ws_val) {
  CAMLparam2(fd, ws_val);

  init_conpty();
  if (!pResizePseudoConsole) {
    uerror("ResizePseudoConsole", caml_copy_string("ConPTY not available"));
  }

  win_pty_t *pty = (win_pty_t *)fd;
  COORD newSize;
  newSize.Y = Int_val(Field(ws_val, 0));  // rows
  newSize.X = Int_val(Field(ws_val, 1));  // cols

  HRESULT hr = pResizePseudoConsole(pty->hPC, newSize);
  if (FAILED(hr)) {
    uerror("ResizePseudoConsole", Nothing);
  }

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

#endif  // _WIN32