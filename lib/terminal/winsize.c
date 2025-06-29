#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>

#ifdef _WIN32
#include <windows.h>
#else
#include <sys/ioctl.h>
#include <unistd.h>
#endif

CAMLprim value terminal_get_size(value fd_val) {
    CAMLparam1(fd_val);
    CAMLlocal1(result);
    int fd = Int_val(fd_val);
    int width = 80, height = 24; // defaults
    
#ifdef _WIN32
    CONSOLE_SCREEN_BUFFER_INFO csbi;
    HANDLE h = (HANDLE)_get_osfhandle(fd);
    if (h != INVALID_HANDLE_VALUE && GetConsoleScreenBufferInfo(h, &csbi)) {
        width = csbi.srWindow.Right - csbi.srWindow.Left + 1;
        height = csbi.srWindow.Bottom - csbi.srWindow.Top + 1;
    }
#else
    struct winsize ws;
    if (ioctl(fd, TIOCGWINSZ, &ws) == 0) {
        width = ws.ws_col;
        height = ws.ws_row;
    }
#endif
    
    result = caml_alloc(2, 0);
    Store_field(result, 0, Val_int(width));
    Store_field(result, 1, Val_int(height));
    CAMLreturn(result);
}