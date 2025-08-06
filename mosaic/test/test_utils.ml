(** Test utilities for Mosaic tests *)

open Mosaic

(* Helper functions for tests *)
let screen_to_string screen =
  let width = Screen.cols screen in
  let height = Screen.rows screen in
  let buf = Buffer.create ((width + 1) * height) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      (* After present(), the front buffer contains the rendered content *)
      match Grid.get (Screen.front screen) ~row:y ~col:x with
      | None -> Buffer.add_char buf ' '
      | Some cell ->
          let text = Grid.Cell.get_text cell in
          if text = "" then Buffer.add_char buf ' '
          else Buffer.add_string buf text
    done;
    if y < height - 1 then Buffer.add_char buf '\n'
  done;
  Buffer.contents buf

let screen_to_lines screen =
  let width = Screen.cols screen in
  let height = Screen.rows screen in
  let lines = ref [] in
  for y = height - 1 downto 0 do
    let line = Buffer.create width in
    for x = 0 to width - 1 do
      match Grid.get (Screen.front screen) ~row:y ~col:x with
      | None -> Buffer.add_char line ' '
      | Some cell ->
          let text = Grid.Cell.get_text cell in
          if text = "" then Buffer.add_char line ' '
          else Buffer.add_string line text
    done;
    lines := Buffer.contents line :: !lines
  done;
  !lines

(** Create a test terminal with predefined input for unit testing low-level
    components like the event source or input parser. *)
let make_test_terminal input =
  let term, get_output, _close = Tty.create_from_strings input in
  (term, get_output)

(** Renders a UI element to a raw string representation of the terminal grid.
    This is the foundation for visual snapshot and expect testing. *)
let render_to_string ?(width = 80) ?(height = 24) element =
  let screen = Screen.create ~rows:height ~cols:width () in
  Screen.begin_frame screen;
  Ui.render screen element;
  let _ = Screen.present screen in
  screen_to_string screen

(** Helper to render a UI element to a string and print it for expect testing *)
let print_ui ?(width = 20) ?(height = 5) element =
  (* Wrap the element in a box with border for visual clarity *)
  let width = width + 2 in
  let height = height + 2 in
  let bordered_element =
    Ui.box ~width:(`Cells width) ~height:(`Cells height)
      ~border:Ui.Border.normal ~border_style:Ui.Style.empty [ element ]
  in
  let output = render_to_string ~width ~height bordered_element in
  print_string ("\n" ^ output ^ "\n")

(** A helper for writing concise Alcotest-based layout tests. *)
let assert_renders_to ?width ?height element expected =
  let output = render_to_string ?width ?height element in
  Alcotest.(check string) "Rendered output matches" expected output

(** Assert that output contains a substring *)
let assert_output_contains output expected =
  try
    let _ = Str.search_forward (Str.regexp_string expected) output 0 in
    ()
  with Not_found ->
    Alcotest.failf "Output does not contain '%s':\n%s" expected output

(** Assert that output does not contain a substring *)
let assert_output_not_contains output unexpected =
  try
    let _ = Str.search_forward (Str.regexp_string unexpected) output 0 in
    Alcotest.failf "Output contains unexpected '%s':\n%s" unexpected output
  with Not_found -> ()

(** Compare two events for equality *)
let event_equal e1 e2 =
  match (e1, e2) with
  | Input.Key k1, Input.Key k2 -> k1.key = k2.key && k1.modifier = k2.modifier
  | Input.Mouse m1, Input.Mouse m2 -> m1 = m2
  | Input.Resize (w1, h1), Input.Resize (w2, h2) -> w1 = w2 && h1 = h2
  | Input.Focus, Input.Focus -> true
  | Input.Blur, Input.Blur -> true
  | Input.Paste_start, Input.Paste_start -> true
  | Input.Paste_end, Input.Paste_end -> true
  | Input.Paste s1, Input.Paste s2 -> s1 = s2
  | Input.Cursor_position (r1, c1), Input.Cursor_position (r2, c2) ->
      r1 = r2 && c1 = c2
  | Input.Device_attributes a1, Input.Device_attributes a2 -> a1 = a2
  | _ -> false

(** Show an event as a string *)
let rec show_event = function
  | Input.Key { key; modifier; _ } ->
      Printf.sprintf "Key(%s, ctrl=%b, alt=%b, shift=%b)" (show_key key)
        modifier.ctrl modifier.alt modifier.shift
  | Input.Mouse m -> Printf.sprintf "Mouse(%s)" (show_mouse m)
  | Input.Resize (w, h) -> Printf.sprintf "Resize(%d, %d)" w h
  | Input.Focus -> "Focus"
  | Input.Blur -> "Blur"
  | Input.Paste_start -> "Paste_start"
  | Input.Paste_end -> "Paste_end"
  | Input.Paste s -> Printf.sprintf "Paste(%S)" s
  | Input.Osc (code, data) -> Printf.sprintf "Osc(%d, %S)" code data
  | Input.Clipboard (text, mime) -> Printf.sprintf "Clipboard(%S, %S)" text mime
  | Input.Cursor_position (row, col) ->
      Printf.sprintf "Cursor_position(%d, %d)" row col
  | Input.Device_attributes attrs ->
      Printf.sprintf "Device_attributes([%s])"
        (String.concat ";" (List.map string_of_int attrs))

and show_key = function
  | Char c -> Printf.sprintf "Char(%C)" (Uchar.to_char c)
  | Enter -> "Enter"
  | Tab -> "Tab"
  | Backspace -> "Backspace"
  | Delete -> "Delete"
  | Escape -> "Escape"
  | Up -> "Up"
  | Down -> "Down"
  | Left -> "Left"
  | Right -> "Right"
  | Home -> "Home"
  | End -> "End"
  | Page_up -> "Page_up"
  | Page_down -> "Page_down"
  | Insert -> "Insert"
  | F n -> Printf.sprintf "F%d" n
  | Print_screen -> "Print_screen"
  | Pause -> "Pause"
  | Menu -> "Menu"
  | Media_play -> "Media_play"
  | Media_pause -> "Media_pause"
  | Media_stop -> "Media_stop"
  | Media_next -> "Media_next"
  | Media_prev -> "Media_prev"
  | Volume_up -> "Volume_up"
  | Volume_down -> "Volume_down"
  | Volume_mute -> "Volume_mute"
  | Shift_left -> "Shift_left"
  | Shift_right -> "Shift_right"
  | Ctrl_left -> "Ctrl_left"
  | Ctrl_right -> "Ctrl_right"
  | Alt_left -> "Alt_left"
  | Alt_right -> "Alt_right"
  | Super_left -> "Super_left"
  | Super_right -> "Super_right"
  | Hyper_left -> "Hyper_left"
  | Hyper_right -> "Hyper_right"
  | Meta_left -> "Meta_left"
  | Meta_right -> "Meta_right"
  | Caps_lock -> "Caps_lock"
  | Num_lock -> "Num_lock"
  | KP_0 -> "KP_0"
  | KP_1 -> "KP_1"
  | KP_2 -> "KP_2"
  | KP_3 -> "KP_3"
  | KP_4 -> "KP_4"
  | KP_5 -> "KP_5"
  | KP_6 -> "KP_6"
  | KP_7 -> "KP_7"
  | KP_8 -> "KP_8"
  | KP_9 -> "KP_9"
  | KP_decimal -> "KP_decimal"
  | KP_divide -> "KP_divide"
  | KP_multiply -> "KP_multiply"
  | KP_subtract -> "KP_subtract"
  | KP_add -> "KP_add"
  | KP_enter -> "KP_enter"
  | KP_left -> "KP_left"
  | KP_right -> "KP_right"
  | KP_up -> "KP_up"
  | KP_down -> "KP_down"
  | KP_page_up -> "KP_page_up"
  | KP_page_down -> "KP_page_down"
  | KP_home -> "KP_home"
  | KP_end -> "KP_end"
  | KP_insert -> "KP_insert"
  | KP_delete -> "KP_delete"
  | Scroll_lock -> "Scroll_lock"
  | Media_play_pause -> "Media_play_pause"
  | Media_reverse -> "Media_reverse"
  | Media_fast_forward -> "Media_fast_forward"
  | Media_rewind -> "Media_rewind"
  | Media_record -> "Media_record"
  | Iso_level3_shift -> "Iso_level3_shift"
  | Iso_level5_shift -> "Iso_level5_shift"
  | KP_equal -> "KP_equal"
  | KP_separator -> "KP_separator"
  | KP_begin -> "KP_begin"
  | Unknown n -> Printf.sprintf "Unknown(%d)" n

and show_mouse = function
  | Button_press (x, y, btn, _) ->
      Printf.sprintf "Button_press(%d,%d,%s)" x y (show_button btn)
  | Button_release (x, y, btn, _) ->
      Printf.sprintf "Button_release(%d,%d,%s)" x y (show_button btn)
  | Motion (x, y, _, _) -> Printf.sprintf "Motion(%d,%d)" x y

and show_button = function
  | Left -> "Left"
  | Middle -> "Middle"
  | Right -> "Right"
  | Wheel_up -> "Wheel_up"
  | Wheel_down -> "Wheel_down"
  | Wheel_left -> "Wheel_left"
  | Wheel_right -> "Wheel_right"
  | Button n -> Printf.sprintf "Button%d" n

(** Run a function within Eio context - helper to reduce boilerplate *)
let run_eio f = Eio_main.run (fun env -> Eio.Switch.run (fun sw -> f env sw))

(** A synchronous test harness for running Mosaic applications. This provides a
    controlled way to test an application's `init`, `update`, `view`, and
    `subscriptions` logic without the full async runtime. It's the recommended
    way to write integration tests for applications. *)
module Test_harness = struct
  type ('model, 'msg) t = {
    app : ('model, 'msg) Mosaic.app;
    mutable model : 'model;
    mutable last_cmd : 'msg Cmd.t;
  }

  (** Create a new harness from an app definition. *)
  let create app =
    let model, last_cmd = app.init () in
    { app; model; last_cmd }

  (** Get the current model. *)
  let get_model h = h.model

  (** Get the last command that was issued. *)
  let get_last_cmd h = h.last_cmd

  (** Check if the last command was Cmd.quit. *)
  let is_quit h =
    match h.last_cmd with
    | Cmd.Quit -> true
    | Cmd.Batch cmds ->
        List.exists (function Cmd.Quit -> true | _ -> false) cmds
    | _ -> false

  (** Render the current view to a string. *)
  let view ?width ?height h =
    render_to_string ?width ?height (h.app.view h.model)

  (** Push a message into the update loop. *)
  let push_msg msg h =
    let new_model, cmd = h.app.update msg h.model in
    h.model <- new_model;
    h.last_cmd <- cmd

  (** Simulate a keyboard event by processing it through the app's
      subscriptions. *)
  let push_key_event event h =
    let subs = h.app.subscriptions h.model in
    let handlers = Sub.collect_keyboard [] subs in
    match List.find_map (fun f -> f event) handlers with
    | Some msg -> push_msg msg h
    | None -> ()

  (** Simulate a key press from a raw string (e.g., "q", "\x1b[A").
      This uses the input parser to generate events. *)
  let push_input s h =
    let parser = Input.create () in
    let events = Input.feed parser (Bytes.of_string s) 0 (String.length s) in
    List.iter
      (function
        | Input.Key event ->
            (* The key event types are the same, we can just cast *)
            push_key_event (Obj.magic event) h
        | _ -> ())
      events
end
