open Windtrap
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
  is_true ~msg:"no escape sequences on non-TTY" (output_data = "");

  T.close term

(* Test: Non-TTY terminal still tracks state *)
let test_non_tty_state_tracking () =
  with_pipes @@ fun input output _output_read ->
  let term = T.open_terminal ~input ~output () in

  T.set_cursor_visible term false;
  is_false ~msg:"cursor visible state" (T.cursor_visible term);

  T.move_cursor term ~row:20 ~col:10;
  let pos = T.cursor_position term in
  equal ~msg:"cursor x" int 10 pos.x;
  equal ~msg:"cursor y" int 20 pos.y;
  is_true ~msg:"cursor visible" pos.visible;

  T.set_cursor_style term `Line ~blinking:true;
  let style, blinking = T.cursor_style_state term in
  let style_str =
    match style with
    | `Block -> "Block"
    | `Line -> "Line"
    | `Underline -> "Underline"
  in
  equal ~msg:"cursor style" string "Line" style_str;
  is_true ~msg:"cursor blinking" blinking;

  T.set_cursor_color term ~r:0.5 ~g:0.25 ~b:0.75 ~a:1.0;
  let r, g, b, a = T.cursor_color term in
  equal ~msg:"color r" (float 0.01) 0.5 r;
  equal ~msg:"color g" (float 0.01) 0.25 g;
  equal ~msg:"color b" (float 0.01) 0.75 b;
  equal ~msg:"color a" (float 0.01) 1.0 a;

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
  equal ~msg:"mouse mode" string "Sgr_button" mouse_mode_str;

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
  is_true ~msg:"contains empty" (Impl.contains_substring "hello" "");
  is_true ~msg:"contains at start" (Impl.contains_substring "hello" "hel");
  is_true ~msg:"contains in middle" (Impl.contains_substring "hello" "ell");
  is_true ~msg:"contains at end" (Impl.contains_substring "hello" "llo");
  is_true ~msg:"contains full" (Impl.contains_substring "hello" "hello");
  is_false ~msg:"not contains" (Impl.contains_substring "hello" "world");
  is_false ~msg:"not contains substring too long"
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

  is_true ~msg:"rgb preserved" normalized.rgb;
  is_true ~msg:"focus_tracking preserved" normalized.focus_tracking;

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
      equal ~msg:"env terminal name" string "Alacritty" info.name;
      equal ~msg:"env terminal version" string "1.99" info.version;
      is_false ~msg:"env info not from xtversion" info.from_xtversion;
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
  equal ~msg:"x clamped to 1" int 1 pos.x;
  equal ~msg:"y clamped to 1" int 1 pos.y;

  (* Zero should be clamped to 1 *)
  T.set_cursor_visible term false;
  T.move_cursor term ~row:0 ~col:0;
  let pos = T.cursor_position term in
  equal ~msg:"x=0 clamped to 1" int 1 pos.x;
  equal ~msg:"y=0 clamped to 1" int 1 pos.y;

  T.close term

(* Test: Color component clamping *)
let test_color_clamping () =
  with_pipes @@ fun input output _output_read ->
  let term = T.open_terminal ~input ~output () in

  (* Out of range values *)
  T.set_cursor_color term ~r:2.5 ~g:(-1.0) ~b:0.5 ~a:1.5;
  let r, g, b, a = T.cursor_color term in

  (* Values are stored as-is, clamping happens during rendering *)
  equal ~msg:"r stored" (float 0.01) 2.5 r;
  equal ~msg:"g stored" (float 0.01) (-1.0) g;
  equal ~msg:"b stored" (float 0.01) 0.5 b;
  equal ~msg:"a stored" (float 0.01) 1.5 a;

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
  equal ~msg:"initial mode" string "Cooked" mode_str;

  (* Switch to Raw (no-op on non-TTY) *)
  T.switch_mode term `Raw;
  let mode_str =
    match T.mode term with
    | `Cooked -> "Cooked"
    | `Raw -> "Raw"
    | `Custom _ -> "Custom"
  in
  equal ~msg:"switched to raw" string "Raw" mode_str;

  (* Switch back to Cooked *)
  T.switch_mode term `Cooked;
  let mode_str =
    match T.mode term with
    | `Cooked -> "Cooked"
    | `Raw -> "Raw"
    | `Custom _ -> "Custom"
  in
  equal ~msg:"switched to cooked" string "Cooked" mode_str;

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
        equal ~msg:"mode inside with_mode" string "Raw" mode_str;
        42)
  in

  equal ~msg:"with_mode returns value" int 42 result;
  let mode_str =
    match T.mode term with
    | `Cooked -> "Cooked"
    | `Raw -> "Raw"
    | `Custom _ -> "Custom"
  in
  equal ~msg:"mode restored" string "Cooked" mode_str;

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
      is_false ~msg:"explicit caps not overridden" current_caps.kitty_keyboard;

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
  equal ~msg:"default cols" int 80 cols;
  equal ~msg:"default rows" int 24 rows;
  T.close term

(* Test: Write functions *)
let test_write_functions () =
  with_pipes @@ fun input output output_read ->
  let term = T.open_terminal ~input ~output () in

  T.write term "Hello";
  T.write_bytes term (Bytes.of_string " World");
  T.flush term;

  let output_data = read_output output_read in
  equal ~msg:"write output" string "Hello World" output_data;

  T.close term

(* Test: open_terminal with probing *)
let test_open_with_probe () =
  with_pipes @@ fun input output _output_read ->
  (* This is non-TTY, so it won't actually probe, but should still work *)
  let term =
    T.open_terminal ~input ~output ~probe:true ~probe_timeout:0.05 ()
  in
  is_true ~msg:"terminal opened" (T.capabilities term |> fun _ -> true);
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
  equal ~msg:"mouse mode set on non-TTY" string "Sgr_any"
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
  is_false ~msg:"no events returned false" had_events;
  equal ~msg:"no events" int 0 !count;

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
  equal ~msg:"unicode width changed" string "Unicode" new_width;

  (* Override to Wcwidth *)
  T.set_unicode_width term `Wcwidth;
  let caps = T.capabilities term in
  let final_width =
    match caps.unicode_width with
    | `Unicode -> "Unicode"
    | `Wcwidth -> "Wcwidth"
  in
  equal ~msg:"unicode width changed again" string "Wcwidth" final_width;

  T.close term

(* Test: modifyOtherKeys toggling on non-TTY updates state *)
let test_modify_other_keys_toggle () =
  with_pipes @@ fun input output _output_read ->
  let term = T.open_terminal ~input ~output () in

  (* Initially disabled *)
  is_false ~msg:"initial MOK disabled" (T.modify_other_keys_enabled term);

  (* Enable *)
  T.enable_modify_other_keys term true;
  is_true ~msg:"MOK enabled" (T.modify_other_keys_enabled term);

  (* Disable *)
  T.enable_modify_other_keys term false;
  is_false ~msg:"MOK disabled" (T.modify_other_keys_enabled term);

  T.close term

let () =
  run "Terminal"
    [
      group "non-tty"
        [
          test "no escape sequences on non-TTY" test_non_tty_no_escape_sequences;
          test "state tracking on non-TTY" test_non_tty_state_tracking;
          test "size returns default" test_size_default;
          test "write functions" test_write_functions;
        ];
      group "helpers" [ test "contains_substring" test_contains_substring ];
      group "capabilities"
        [
          test "normalization" test_capability_normalization;
          test "environment overrides" test_env_overrides;
          test "terminal info env" test_terminal_info_from_env;
          test "open_terminal_probe" test_open_with_probe;
        ];
      group "cursor"
        [
          test "position clamping" test_cursor_position_clamping;
          test "color clamping" test_color_clamping;
        ];
      group "modes"
        [
          test "mode switching" test_mode_switching;
          test "with_mode restore" test_with_mode_restore;
        ];
      group "mouse" [ test "mouse mode on non-TTY" test_mouse_validation ];
      group "input" [ test "poll" test_poll ];
      group "unicode" [ test "set_unicode_width" test_set_unicode_width ];
      group "keyboard"
        [ test "modifyOtherKeys toggle" test_modify_other_keys_toggle ];
    ]
