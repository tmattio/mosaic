open Alcotest
module T = Terminal

(* Helper to create pipes for non-TTY testing *)
let with_pipes f =
  let input_read, input_write = Unix.pipe () in
  let output_read, output_write = Unix.pipe () in
  Fun.protect
    (fun () -> f input_read output_write output_read)
    ~finally:(fun () ->
      Unix.close input_read;
      Unix.close input_write;
      Unix.close output_read;
      Unix.close output_write)

(* Helper to read from output pipe *)
let read_output fd =
  let buffer = Bytes.create 4096 in
  Unix.set_nonblock fd;
  try
    let n = Unix.read fd buffer 0 4096 in
    Bytes.sub_string buffer 0 n
  with Unix.Unix_error (Unix.EAGAIN, _, _) -> ""

(* Test: Non-TTY terminal doesn't send escape sequences *)
let test_non_tty_no_escape_sequences () =
  with_pipes @@ fun input output output_read ->
  let term = T.open_terminal ~input ~output () in

  (* These operations should not send escape sequences on non-TTY *)
  T.set_cursor_visible term false;
  T.set_mouse_mode term `Sgr_any;
  T.enable_bracketed_paste term true;
  T.enable_focus_reporting term true;
  T.enter_alternate_screen term;
  T.set_title term "Test";

  let output_data = read_output output_read in
  check bool "no escape sequences on non-TTY" true (output_data = "");

  T.close term

(* Test: Non-TTY terminal still tracks state *)
let test_non_tty_state_tracking () =
  with_pipes @@ fun input output _output_read ->
  let term = T.open_terminal ~input ~output () in

  T.set_cursor_visible term false;
  check bool "cursor visible state" false (T.cursor_visible term);

  T.move_cursor term ~row:20 ~col:10;
  let pos = T.cursor_position term in
  check int "cursor x" 10 pos.x;
  check int "cursor y" 20 pos.y;
  check bool "cursor visible" true pos.visible;

  T.set_cursor_style term `Line ~blinking:true;
  let style, blinking = T.cursor_style_state term in
  let style_str =
    match style with
    | `Block -> "Block"
    | `Line -> "Line"
    | `Underline -> "Underline"
  in
  check string "cursor style" "Line" style_str;
  check bool "cursor blinking" true blinking;

  T.set_cursor_color term ~r:0.5 ~g:0.25 ~b:0.75 ~a:1.0;
  let r, g, b, a = T.cursor_color term in
  check (float 0.01) "color r" 0.5 r;
  check (float 0.01) "color g" 0.25 g;
  check (float 0.01) "color b" 0.75 b;
  check (float 0.01) "color a" 1.0 a;

  T.set_mouse_mode term `Sgr_button;
  let mouse_mode_str =
    match T.mouse_mode term with
    | `Off -> "Off"
    | `X10 -> "X10"
    | `Normal -> "Normal"
    | `Button -> "Button"
    | `Any -> "Any"
    | `Sgr_normal -> "Sgr_normal"
    | `Sgr_button -> "Sgr_button"
    | `Sgr_any -> "Sgr_any"
  in
  check string "mouse mode" "Sgr_button" mouse_mode_str;

  T.close term

(* Test: Helper function - contains_substring *)
let test_contains_substring () =
  let module Impl = struct
    let contains_substring s sub =
      let len_s = String.length s and len_sub = String.length sub in
      if len_sub = 0 then true
      else
        let rec loop idx =
          if idx + len_sub > len_s then false
          else if String.sub s idx len_sub = sub then true
          else loop (idx + 1)
        in
        loop 0
  end in
  check bool "contains empty" true (Impl.contains_substring "hello" "");
  check bool "contains at start" true (Impl.contains_substring "hello" "hel");
  check bool "contains in middle" true (Impl.contains_substring "hello" "ell");
  check bool "contains at end" true (Impl.contains_substring "hello" "llo");
  check bool "contains full" true (Impl.contains_substring "hello" "hello");
  check bool "not contains" false (Impl.contains_substring "hello" "world");
  check bool "not contains substring too long" false
    (Impl.contains_substring "hi" "hello")

(* Test: Capability normalization *)
let test_capability_normalization () =
  with_pipes @@ fun input output _output_read ->
  let caps =
    {
      T.term = "test";
      rgb = true;
      kitty_keyboard = false;
      kitty_graphics = false;
      bracketed_paste = true;
      focus_tracking = true;
      unicode_width = `Wcwidth;
      sgr_pixels = false;
      color_scheme_updates = false;
      explicit_width = false;
      scaled_text = false;
      sixel = false;
      sync = false;
      hyperlinks = false;
    }
  in

  let term = T.open_terminal ~input ~output ~initial_caps:caps () in
  let normalized = T.capabilities term in

  check bool "rgb preserved" true normalized.rgb;
  check bool "focus_tracking preserved" true normalized.focus_tracking;

  (* no mouse_reporting field anymore *)
  T.close term

(* Test: TERM_PROGRAM fallback populates terminal info when XTVersion missing *)
let test_terminal_info_from_env () =
  let save key = Sys.getenv_opt key in
  let term_prog = save "TERM_PROGRAM" in
  let term_prog_ver = save "TERM_PROGRAM_VERSION" in
  Fun.protect
    (fun () ->
      Unix.putenv "TERM_PROGRAM" "Alacritty";
      Unix.putenv "TERM_PROGRAM_VERSION" "1.99";
      with_pipes @@ fun input output _output_read ->
      let term = T.open_terminal ~input ~output () in
      let info = T.terminal_info term in
      check string "env terminal name" "Alacritty" info.name;
      check string "env terminal version" "1.99" info.version;
      check bool "env info not from xtversion" false info.from_xtversion;
      T.close term)
    ~finally:(fun () ->
      (match term_prog with
      | Some v -> Unix.putenv "TERM_PROGRAM" v
      | None -> Unix.putenv "TERM_PROGRAM" "");
      match term_prog_ver with
      | Some v -> Unix.putenv "TERM_PROGRAM_VERSION" v
      | None -> Unix.putenv "TERM_PROGRAM_VERSION" "")

(* Test: Cursor position clamping *)
let test_cursor_position_clamping () =
  with_pipes @@ fun input output _output_read ->
  let term = T.open_terminal ~input ~output () in

  (* Negative values should be clamped to 1 *)
  T.move_cursor term ~row:(-10) ~col:(-5);
  let pos = T.cursor_position term in
  check int "x clamped to 1" 1 pos.x;
  check int "y clamped to 1" 1 pos.y;

  (* Zero should be clamped to 1 *)
  T.set_cursor_visible term false;
  T.move_cursor term ~row:0 ~col:0;
  let pos = T.cursor_position term in
  check int "x=0 clamped to 1" 1 pos.x;
  check int "y=0 clamped to 1" 1 pos.y;

  T.close term

(* Test: Color component clamping *)
let test_color_clamping () =
  with_pipes @@ fun input output _output_read ->
  let term = T.open_terminal ~input ~output () in

  (* Out of range values *)
  T.set_cursor_color term ~r:2.5 ~g:(-1.0) ~b:0.5 ~a:1.5;
  let r, g, b, a = T.cursor_color term in

  (* Values are stored as-is, clamping happens during rendering *)
  check (float 0.01) "r stored" 2.5 r;
  check (float 0.01) "g stored" (-1.0) g;
  check (float 0.01) "b stored" 0.5 b;
  check (float 0.01) "a stored" 1.5 a;

  T.close term

(* Test: Mode switching *)
let test_mode_switching () =
  with_pipes @@ fun input output _output_read ->
  let term = T.open_terminal ~input ~output () in

  (* Initial mode is Cooked *)
  let mode_str =
    match T.mode term with
    | `Cooked -> "Cooked"
    | `Raw -> "Raw"
    | `Custom _ -> "Custom"
  in
  check string "initial mode" "Cooked" mode_str;

  (* Switch to Raw (no-op on non-TTY) *)
  T.switch_mode term `Raw;
  let mode_str =
    match T.mode term with
    | `Cooked -> "Cooked"
    | `Raw -> "Raw"
    | `Custom _ -> "Custom"
  in
  check string "switched to raw" "Raw" mode_str;

  (* Switch back to Cooked *)
  T.switch_mode term `Cooked;
  let mode_str =
    match T.mode term with
    | `Cooked -> "Cooked"
    | `Raw -> "Raw"
    | `Custom _ -> "Custom"
  in
  check string "switched to cooked" "Cooked" mode_str;

  T.close term

(* Test: with_mode restores previous mode *)
let test_with_mode_restore () =
  with_pipes @@ fun input output _output_read ->
  let term = T.open_terminal ~input ~output () in

  T.switch_mode term `Cooked;
  let result =
    T.with_mode term `Raw (fun () ->
        let mode_str =
          match T.mode term with
          | `Cooked -> "Cooked"
          | `Raw -> "Raw"
          | `Custom _ -> "Custom"
        in
        check string "mode inside with_mode" "Raw" mode_str;
        42)
  in

  check int "with_mode returns value" 42 result;
  let mode_str =
    match T.mode term with
    | `Cooked -> "Cooked"
    | `Raw -> "Raw"
    | `Custom _ -> "Custom"
  in
  check string "mode restored" "Cooked" mode_str;

  T.close term

(* Test: Environment variable capability overrides *)
let test_env_overrides () =
  (* Save original env *)
  let save_env key = Sys.getenv_opt key in
  let kitty_id = save_env "KITTY_WINDOW_ID" in
  let term_prog = save_env "TERM_PROGRAM" in
  let colorterm = save_env "COLORTERM" in

  Fun.protect
    (fun () ->
      with_pipes @@ fun input output _output_read ->
      (* Test KITTY_WINDOW_ID override *)
      Unix.putenv "KITTY_WINDOW_ID" "1";
      (match term_prog with
      | Some v -> Unix.putenv "TERM_PROGRAM" v
      | None -> ());
      (match colorterm with Some v -> Unix.putenv "COLORTERM" v | None -> ());

      let caps =
        {
          T.term = "xterm";
          rgb = false;
          kitty_keyboard = false;
          kitty_graphics = false;
          bracketed_paste = false;
          focus_tracking = false;
          unicode_width = `Wcwidth;
          sgr_pixels = false;
          color_scheme_updates = false;
          explicit_width = false;
          scaled_text = false;
          sixel = false;
          sync = false;
          hyperlinks = false;
        }
      in

      let term = T.open_terminal ~input ~output ~initial_caps:caps () in

      (* KITTY_WINDOW_ID should not override initial_caps when provided *)
      let current_caps = T.capabilities term in
      check bool "explicit caps not overridden" false
        current_caps.kitty_keyboard;

      T.close term)
    ~finally:(fun () ->
      (* Restore env *)
      (match kitty_id with
      | Some v -> Unix.putenv "KITTY_WINDOW_ID" v
      | None -> Unix.putenv "KITTY_WINDOW_ID" "");
      (match term_prog with
      | Some v -> Unix.putenv "TERM_PROGRAM" v
      | None -> Unix.putenv "TERM_PROGRAM" "");
      match colorterm with
      | Some v -> Unix.putenv "COLORTERM" v
      | None -> Unix.putenv "COLORTERM" "")

(* Test: Size returns default for non-TTY *)
let test_size_default () =
  with_pipes @@ fun input output _output_read ->
  let term = T.open_terminal ~input ~output () in
  let cols, rows = T.size term in
  check int "default cols" 80 cols;
  check int "default rows" 24 rows;
  T.close term

(* Test: Write functions *)
let test_write_functions () =
  with_pipes @@ fun input output output_read ->
  let term = T.open_terminal ~input ~output () in

  T.write term "Hello";
  T.write_bytes term (Bytes.of_string " World");
  T.flush term;

  let output_data = read_output output_read in
  check string "write output" "Hello World" output_data;

  T.close term

(* Test: open_terminal with probing *)
let test_open_with_probe () =
  with_pipes @@ fun input output _output_read ->
  (* This is non-TTY, so it won't actually probe, but should still work *)
  let term =
    T.open_terminal ~input ~output ~probe:true ~probe_timeout:0.05 ()
  in
  check bool "terminal opened" true (T.capabilities term |> fun _ -> true);
  T.close term

(* Test: Mouse mode validation - only validates on TTY *)
let test_mouse_validation () =
  with_pipes @@ fun input output _output_read ->
  let caps =
    {
      T.term = "test";
      rgb = false;
      kitty_keyboard = false;
      kitty_graphics = false;
      bracketed_paste = false;
      focus_tracking = false;
      unicode_width = `Wcwidth;
      sgr_pixels = false;
      color_scheme_updates = false;
      explicit_width = false;
      scaled_text = false;
      sixel = false;
      sync = false;
      hyperlinks = false;
    }
  in

  let term = T.open_terminal ~input ~output ~initial_caps:caps () in

  (* On non-TTY, validation doesn't happen, so this won't raise *)
  T.set_mouse_mode term `Sgr_any;
  check string "mouse mode set on non-TTY" "Sgr_any"
    (match T.mouse_mode term with
    | `Off -> "Off"
    | `X10 -> "X10"
    | `Normal -> "Normal"
    | `Button -> "Button"
    | `Any -> "Any"
    | `Sgr_normal -> "Sgr_normal"
    | `Sgr_button -> "Sgr_button"
    | `Sgr_any -> "Sgr_any");

  T.close term

(* Test: poll drains available events *)
let test_poll () =
  with_pipes @@ fun input _output _output_read ->
  let term = T.open_terminal ~input () in

  (* No events available *)
  let count = ref 0 in
  let had_events = T.poll term (fun _ -> incr count) in
  check bool "no events returned false" false had_events;
  check int "no events" 0 !count;

  T.close term

(* Test: set_unicode_width *)
let test_set_unicode_width () =
  with_pipes @@ fun input output _output_read ->
  let term = T.open_terminal ~input ~output () in

  (* Default is Wcwidth *)
  let caps = T.capabilities term in
  let _initial_width =
    match caps.unicode_width with
    | `Unicode -> "Unicode"
    | `Wcwidth -> "Wcwidth"
  in

  (* Override to Unicode *)
  T.set_unicode_width term `Unicode;
  let caps = T.capabilities term in
  let new_width =
    match caps.unicode_width with
    | `Unicode -> "Unicode"
    | `Wcwidth -> "Wcwidth"
  in
  check string "unicode width changed" "Unicode" new_width;

  (* Override to Wcwidth *)
  T.set_unicode_width term `Wcwidth;
  let caps = T.capabilities term in
  let final_width =
    match caps.unicode_width with
    | `Unicode -> "Unicode"
    | `Wcwidth -> "Wcwidth"
  in
  check string "unicode width changed again" "Wcwidth" final_width;

  T.close term

(* Test: modifyOtherKeys toggling on non-TTY updates state *)
let test_modify_other_keys_toggle () =
  with_pipes @@ fun input output _output_read ->
  let term = T.open_terminal ~input ~output () in

  (* Initially disabled *)
  check bool "initial MOK disabled" false (T.modify_other_keys_enabled term);

  (* Enable *)
  T.enable_modify_other_keys term true;
  check bool "MOK enabled" true (T.modify_other_keys_enabled term);

  (* Disable *)
  T.enable_modify_other_keys term false;
  check bool "MOK disabled" false (T.modify_other_keys_enabled term);

  T.close term

let () =
  run "Terminal"
    [
      ( "non-tty",
        [
          test_case "no escape sequences on non-TTY" `Quick
            test_non_tty_no_escape_sequences;
          test_case "state tracking on non-TTY" `Quick
            test_non_tty_state_tracking;
          test_case "size returns default" `Quick test_size_default;
          test_case "write functions" `Quick test_write_functions;
        ] );
      ( "helpers",
        [ test_case "contains_substring" `Quick test_contains_substring ] );
      ( "capabilities",
        [
          test_case "normalization" `Quick test_capability_normalization;
          test_case "environment overrides" `Quick test_env_overrides;
          test_case "terminal info env" `Quick test_terminal_info_from_env;
          test_case "open_terminal_probe" `Quick test_open_with_probe;
        ] );
      ( "cursor",
        [
          test_case "position clamping" `Quick test_cursor_position_clamping;
          test_case "color clamping" `Quick test_color_clamping;
        ] );
      ( "modes",
        [
          test_case "mode switching" `Quick test_mode_switching;
          test_case "with_mode restore" `Quick test_with_mode_restore;
        ] );
      ( "mouse",
        [ test_case "mouse mode on non-TTY" `Quick test_mouse_validation ] );
      ("input", [ test_case "poll" `Quick test_poll ]);
      ( "unicode",
        [ test_case "set_unicode_width" `Quick test_set_unicode_width ] );
      ( "keyboard",
        [
          test_case "modifyOtherKeys toggle" `Quick
            test_modify_other_keys_toggle;
        ] );
    ]
