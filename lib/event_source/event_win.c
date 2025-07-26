#ifdef _WIN32

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <io.h>
#include <windows.h>

// Return raw INPUT_RECORD as OCaml variant (moved parsing to ML)
CAMLprim value event_read_console_input(value handle_val, value count_val) {
  CAMLparam2(handle_val, count_val);
  CAMLlocal2(result, record);

  HANDLE h = (HANDLE)(intptr_t)Int_val(handle_val);
  int max_count = Int_val(count_val);

  INPUT_RECORD ir[128];  // Batch up to 128
  DWORD events_read = 0;

  if (!ReadConsoleInput(h, ir, max_count, &events_read) || events_read == 0) {
    CAMLreturn(Val_int(0));  // None
  }

  // Alloc list of records
  result = Val_emptylist;
  for (DWORD i = events_read; i > 0; i--) {
    INPUT_RECORD *r = &ir[i - 1];

    switch (r->EventType) {
      case KEY_EVENT: {
        KEY_EVENT_RECORD *ker = &r->Event.KeyEvent;
        // Key_event constructor (tag 0) with 5 fields
        record = caml_alloc(5, 0);
        Store_field(record, 0, Val_bool(ker->bKeyDown));
        Store_field(record, 1, Val_int(ker->wRepeatCount));
        Store_field(record, 2, Val_int(ker->wVirtualKeyCode));
        Store_field(record, 3, Val_int(ker->uChar.UnicodeChar));
        Store_field(record, 4, Val_int(ker->dwControlKeyState));
      } break;
      case MOUSE_EVENT: {
        MOUSE_EVENT_RECORD *mer = &r->Event.MouseEvent;
        // Mouse_event constructor (tag 1) with 5 fields
        record = caml_alloc(5, 1);
        Store_field(record, 0, Val_int(mer->dwMousePosition.X));
        Store_field(record, 1, Val_int(mer->dwMousePosition.Y));
        Store_field(record, 2, Val_int(mer->dwButtonState));
        Store_field(record, 3, Val_int(mer->dwControlKeyState));
        Store_field(record, 4, Val_int(mer->dwEventFlags));
      } break;
      case WINDOW_BUFFER_SIZE_EVENT: {
        // Window_buffer_size_event constructor (tag 2) with 2 fields
        record = caml_alloc(2, 2);
        Store_field(record, 0,
                    Val_int(r->Event.WindowBufferSizeEvent.dwSize.X));
        Store_field(record, 1,
                    Val_int(r->Event.WindowBufferSizeEvent.dwSize.Y));
      } break;
      case FOCUS_EVENT:
        // Focus_event constructor (tag 3) with 1 field
        record = caml_alloc(1, 3);
        Store_field(record, 0, Val_bool(r->Event.FocusEvent.bSetFocus));
        break;
      default:
        continue;  // Skip unknown
    }

    value cons = caml_alloc(2, 0);
    Store_field(cons, 0, record);
    Store_field(cons, 1, result);
    result = cons;
  }

  if (result == Val_emptylist) {
    CAMLreturn(Val_int(0));  // None
  }
  value some = caml_alloc(1, 0);
  Store_field(some, 0, result);
  CAMLreturn(some);
}

CAMLprim value event_enable_console_mode(value handle_val,
                                         value enable_mouse_val) {
  // Same as original
  HANDLE h = (HANDLE)(intptr_t)Int_val(handle_val);
  DWORD mode;
  if (!GetConsoleMode(h, &mode)) caml_failwith("Failed to get console mode");
  mode |= ENABLE_WINDOW_INPUT | ENABLE_EXTENDED_FLAGS | ENABLE_INSERT_MODE |
          ENABLE_QUICK_EDIT_MODE;
  mode &= ~(ENABLE_PROCESSED_INPUT | ENABLE_LINE_INPUT | ENABLE_ECHO_INPUT);
  if (Bool_val(enable_mouse_val))
    mode |= ENABLE_MOUSE_INPUT;
  else
    mode &= ~ENABLE_MOUSE_INPUT;
  if (!SetConsoleMode(h, mode)) caml_failwith("Failed to set console mode");
  CAMLreturn(Val_unit);
}

CAMLprim value event_get_console_mode(value handle_val) {
  HANDLE h = (HANDLE)(intptr_t)Int_val(handle_val);
  DWORD mode;
  if (!GetConsoleMode(h, &mode)) caml_failwith("Failed to get console mode");
  CAMLreturn(Val_int(mode));
}

CAMLprim value event_set_console_mode(value handle_val, value mode_val) {
  HANDLE h = (HANDLE)(intptr_t)Int_val(handle_val);
  if (!SetConsoleMode(h, Int_val(mode_val)))
    caml_failwith("Failed to set console mode");
  CAMLreturn(Val_unit);
}

CAMLprim value event_get_number_of_console_input_events(value handle_val) {
  CAMLparam1(handle_val);
  HANDLE h = (HANDLE)(intptr_t)Int_val(handle_val);
  DWORD num;
  if (!GetNumberOfConsoleInputEvents(h, &num)) {
    caml_failwith("Failed to get number of console input events");
  }
  CAMLreturn(Val_int(num));
}

CAMLprim value event_get_console_size(value handle_val) {
  CAMLparam1(handle_val);
  CAMLlocal1(pair);
  HANDLE h = (HANDLE)Int_val(handle_val);
  CONSOLE_SCREEN_BUFFER_INFO info;
  if (!GetConsoleScreenBufferInfo(h, &info))
    caml_failwith("GetConsoleScreenBufferInfo failed");
  pair = caml_alloc(2, 0);
  Store_field(pair, 0, Val_int(info.dwSize.X));
  Store_field(pair, 1, Val_int(info.dwSize.Y));
  CAMLreturn(pair);
}
#else /* Not Windows - provide stubs */

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value event_read_console_input(value handle_val, value count_val) {
  caml_failwith("event_read_console_input: Windows-only function");
}

CAMLprim value event_enable_console_mode(value handle_val,
                                         value enable_mouse_val) {
  caml_failwith("event_enable_console_mode: Windows-only function");
}

CAMLprim value event_get_console_mode(value handle_val) {
  caml_failwith("event_get_console_mode: Windows-only function");
}

CAMLprim value event_set_console_mode(value handle_val, value mode_val) {
  caml_failwith("event_set_console_mode: Windows-only function");
}

CAMLprim value event_get_number_of_console_input_events(value handle_val) {
  caml_failwith(
      "event_get_number_of_console_input_events: Windows-only function");
}

CAMLprim value event_get_console_size(value handle_val) {
  caml_failwith("event_get_console_size: Windows-only function");
}

#endif