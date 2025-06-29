#ifdef _WIN32

#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <io.h>
#include <windows.h>

// Helper to create Input.key_event
static value make_key_event(value key, int ctrl, int alt, int shift) {
  CAMLparam1(key);
  CAMLlocal2(modifier, event);

  // Create modifier record
  modifier = caml_alloc(3, 0);
  Store_field(modifier, 0, Val_bool(ctrl));
  Store_field(modifier, 1, Val_bool(alt));
  Store_field(modifier, 2, Val_bool(shift));

  // Create key_event record
  event = caml_alloc(2, 0);
  Store_field(event, 0, key);
  Store_field(event, 1, modifier);

  // Return Key of key_event
  value result = caml_alloc(1, 0);
  Store_field(result, 0, event);

  CAMLreturn(result);
}

// Convert Windows virtual key to Input.key type
static value convert_key_event(KEY_EVENT_RECORD *ker) {
  CAMLparam0();
  CAMLlocal2(key_val, result);

  if (!ker->bKeyDown) {
    // Ignore key up events
    CAMLreturn(Val_int(0));
  }

  int ctrl = ker->dwControlKeyState & (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED);
  int alt = ker->dwControlKeyState & (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED);
  int shift = ker->dwControlKeyState & SHIFT_PRESSED;

  // Map virtual key codes to Input.key variants
  switch (ker->wVirtualKeyCode) {
    case VK_RETURN:
      key_val = Val_int(1);  // Enter (constant constructor)
      break;
    case VK_TAB:
      key_val = Val_int(2);  // Tab
      break;
    case VK_BACK:
      key_val = Val_int(3);  // Backspace
      break;
    case VK_DELETE:
      key_val = Val_int(4);  // Delete
      break;
    case VK_ESCAPE:
      key_val = Val_int(5);  // Escape
      break;
    case VK_UP:
      key_val = Val_int(6);  // Up
      break;
    case VK_DOWN:
      key_val = Val_int(7);  // Down
      break;
    case VK_LEFT:
      key_val = Val_int(8);  // Left
      break;
    case VK_RIGHT:
      key_val = Val_int(9);  // Right
      break;
    case VK_HOME:
      key_val = Val_int(10);  // Home
      break;
    case VK_END:
      key_val = Val_int(11);  // End
      break;
    case VK_PRIOR:
      key_val = Val_int(12);  // Page_up
      break;
    case VK_NEXT:
      key_val = Val_int(13);  // Page_down
      break;
    case VK_INSERT:
      key_val = Val_int(14);  // Insert
      break;
    case VK_F1:
    case VK_F2:
    case VK_F3:
    case VK_F4:
    case VK_F5:
    case VK_F6:
    case VK_F7:
    case VK_F8:
    case VK_F9:
    case VK_F10:
    case VK_F11:
    case VK_F12:
      key_val = caml_alloc(1, 15);  // F of int
      Store_field(key_val, 0, Val_int(ker->wVirtualKeyCode - VK_F1 + 1));
      break;
    default:
      if (ker->uChar.UnicodeChar != 0) {
        // Regular character - convert to Uchar.t
        key_val = caml_alloc(1, 0);  // Char of Uchar.t
        Store_field(key_val, 0, Val_int(ker->uChar.UnicodeChar));
      } else {
        // Unknown key
        CAMLreturn(Val_int(0));
      }
      break;
  }

  result = make_key_event(key_val, ctrl, alt, shift);
  value some = caml_alloc(1, 0);  // Some
  Store_field(some, 0, result);
  CAMLreturn(some);
}

// Convert Windows mouse event to Input.mouse_event
static value convert_mouse_event(MOUSE_EVENT_RECORD *mer) {
  CAMLparam0();
  CAMLlocal4(event_val, modifier_val, button_val, result);

  // Create modifier record
  modifier_val = caml_alloc(3, 0);
  Store_field(modifier_val, 0,
              Val_bool(mer->dwControlKeyState &
                       (LEFT_CTRL_PRESSED | RIGHT_CTRL_PRESSED)));
  Store_field(modifier_val, 1,
              Val_bool(mer->dwControlKeyState &
                       (LEFT_ALT_PRESSED | RIGHT_ALT_PRESSED)));
  Store_field(modifier_val, 2,
              Val_bool(mer->dwControlKeyState & SHIFT_PRESSED));

  if (mer->dwEventFlags == 0 && mer->dwButtonState != 0) {
    // Button press
    event_val = caml_alloc(4, 0);  // Press
    Store_field(event_val, 0, Val_int(mer->dwMousePosition.X));
    Store_field(event_val, 1, Val_int(mer->dwMousePosition.Y));

    // Determine button
    if (mer->dwButtonState & FROM_LEFT_1ST_BUTTON_PRESSED)
      button_val = Val_int(0);  // Left
    else if (mer->dwButtonState & RIGHTMOST_BUTTON_PRESSED)
      button_val = Val_int(2);  // Right
    else if (mer->dwButtonState & FROM_LEFT_2ND_BUTTON_PRESSED)
      button_val = Val_int(1);  // Middle
    else
      button_val = Val_int(0);  // Default to left

    Store_field(event_val, 2, button_val);
    Store_field(event_val, 3, modifier_val);
  } else if (mer->dwEventFlags & MOUSE_MOVED) {
    // Motion
    event_val = caml_alloc(3, 2);  // Motion
    Store_field(event_val, 0, Val_int(mer->dwMousePosition.X));
    Store_field(event_val, 1, Val_int(mer->dwMousePosition.Y));
    Store_field(event_val, 2, modifier_val);
  } else if (mer->dwEventFlags & MOUSE_WHEELED) {
    // Wheel
    event_val = caml_alloc(4, 0);  // Press (treat wheel as button press)
    Store_field(event_val, 0, Val_int(mer->dwMousePosition.X));
    Store_field(event_val, 1, Val_int(mer->dwMousePosition.Y));

    // Determine wheel direction
    if ((short)HIWORD(mer->dwButtonState) > 0)
      button_val = Val_int(3);  // Wheel_up
    else
      button_val = Val_int(4);  // Wheel_down

    Store_field(event_val, 2, button_val);
    Store_field(event_val, 3, modifier_val);
  } else {
    // Release
    event_val = caml_alloc(3, 1);  // Release
    Store_field(event_val, 0, Val_int(mer->dwMousePosition.X));
    Store_field(event_val, 1, Val_int(mer->dwMousePosition.Y));
    Store_field(event_val, 2, modifier_val);
  }

  // Return Mouse of mouse_event
  result = caml_alloc(1, 1);
  Store_field(result, 0, event_val);

  value some = caml_alloc(1, 0);  // Some
  Store_field(some, 0, result);
  CAMLreturn(some);
}

CAMLprim value event_read_console_input(value handle_val, value timeout_val) {
  CAMLparam2(handle_val, timeout_val);
  CAMLlocal1(result);

  HANDLE h = (HANDLE)(intptr_t)Int_val(handle_val);
  int timeout_ms = Int_val(timeout_val);

  // Wait for input with timeout
  DWORD wait_timeout;
  if (timeout_ms == -1) {
    wait_timeout = INFINITE;  // Block indefinitely
  } else if (timeout_ms == 0) {
    wait_timeout = 0;  // Poll without blocking
  } else {
    wait_timeout = (DWORD)timeout_ms;  // Wait for specified milliseconds
  }
  DWORD wait_result = WaitForSingleObject(h, wait_timeout);

  if (wait_result == WAIT_TIMEOUT) {
    CAMLreturn(Val_int(0));  // None
  }

  INPUT_RECORD ir;
  DWORD events_read;

  if (!ReadConsoleInput(h, &ir, 1, &events_read) || events_read == 0) {
    CAMLreturn(Val_int(0));  // None
  }

  switch (ir.EventType) {
    case KEY_EVENT:
      result = convert_key_event(&ir.Event.KeyEvent);
      break;

    case MOUSE_EVENT:
      result = convert_mouse_event(&ir.Event.MouseEvent);
      break;

    case WINDOW_BUFFER_SIZE_EVENT:
      // Create Resize event
      result = caml_alloc(2, 2);
      Store_field(result, 0, Val_int(ir.Event.WindowBufferSizeEvent.dwSize.X));
      Store_field(result, 1, Val_int(ir.Event.WindowBufferSizeEvent.dwSize.Y));
      value some = caml_alloc(1, 0);  // Some
      Store_field(some, 0, result);
      result = some;
      break;

    case FOCUS_EVENT:
      // Create Focus or Blur event (3=Focus, 4=Blur in Input.event)
      result = caml_alloc(1, 0);  // Some
      Store_field(result, 0, Val_int(ir.Event.FocusEvent.bSetFocus ? 3 : 4));
      break;

    default:
      result = Val_int(0);  // None
      break;
  }

  CAMLreturn(result);
}

CAMLprim value event_enable_console_mode(value handle_val,
                                         value enable_mouse_val) {
  CAMLparam2(handle_val, enable_mouse_val);

  HANDLE h = (HANDLE)(intptr_t)Int_val(handle_val);
  DWORD mode;

  if (!GetConsoleMode(h, &mode)) {
    caml_failwith("Failed to get console mode");
  }

  // Enable required input modes
  mode |= ENABLE_WINDOW_INPUT | ENABLE_EXTENDED_FLAGS;
  mode &= ~ENABLE_PROCESSED_INPUT;  // Disable Ctrl+C processing
  mode &= ~ENABLE_LINE_INPUT;       // Disable line input
  mode &= ~ENABLE_ECHO_INPUT;       // Disable echo

  if (Bool_val(enable_mouse_val)) {
    mode |= ENABLE_MOUSE_INPUT;
  } else {
    mode &= ~ENABLE_MOUSE_INPUT;
  }

  if (!SetConsoleMode(h, mode)) {
    caml_failwith("Failed to set console mode");
  }

  CAMLreturn(Val_unit);
}

CAMLprim value event_get_console_cursor_info(value handle_val) {
  CAMLparam1(handle_val);
  CAMLlocal1(result);

  HANDLE h = (HANDLE)(intptr_t)Int_val(handle_val);
  CONSOLE_CURSOR_INFO cci;

  if (!GetConsoleCursorInfo(h, &cci)) {
    caml_failwith("Failed to get cursor info");
  }

  result = caml_alloc(2, 0);
  Store_field(result, 0, Val_int(cci.dwSize));
  Store_field(result, 1, Val_bool(cci.bVisible));

  CAMLreturn(result);
}

CAMLprim value event_set_console_cursor_info(value handle_val, value size_val,
                                             value visible_val) {
  CAMLparam3(handle_val, size_val, visible_val);

  HANDLE h = (HANDLE)(intptr_t)Int_val(handle_val);
  CONSOLE_CURSOR_INFO cci;

  cci.dwSize = Int_val(size_val);
  cci.bVisible = Bool_val(visible_val);

  if (!SetConsoleCursorInfo(h, &cci)) {
    caml_failwith("Failed to set cursor info");
  }

  CAMLreturn(Val_unit);
}

#else
// Stubs for non-Windows platforms
#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>

CAMLprim value event_open_console_input(value unit) { return Val_unit; }

CAMLprim value event_read_console_input(value handle_val, value timeout_val) {
  return Val_int(0);  // None
}

CAMLprim value event_get_console_screen_buffer_info(value handle) {
  return Val_unit;
}

CAMLprim value event_close_handle(value handle) { return Val_unit; }

CAMLprim value event_enable_console_mode(value handle_val,
                                         value enable_mouse_val) {
  return Val_unit;
}

CAMLprim value event_get_console_cursor_info(value handle_val) {
  value result = caml_alloc(2, 0);
  Store_field(result, 0, Val_int(25));
  Store_field(result, 1, Val_bool(1));
  return result;
}

CAMLprim value event_set_console_cursor_info(value handle_val, value size_val,
                                             value visible_val) {
  return Val_unit;
}

#endif /* _WIN32 */