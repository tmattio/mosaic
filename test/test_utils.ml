(** Test utilities for Mosaic tests *)

open Mosaic

(* Helper functions for tests *)
let buffer_to_string buffer =
  let width, height = Render.dimensions buffer in
  let buf = Buffer.create ((width + 1) * height) in
  for y = 0 to height - 1 do
    for x = 0 to width - 1 do
      let cell = Render.get buffer x y in
      match cell.Render.chars with
      | [] -> Buffer.add_char buf ' '
      | ch :: _ -> Buffer.add_utf_8_uchar buf ch
    done;
    if y < height - 1 then Buffer.add_char buf '\n'
  done;
  Buffer.contents buf

let buffer_to_lines buffer =
  let width, height = Render.dimensions buffer in
  let lines = ref [] in
  for y = height - 1 downto 0 do
    let line = Buffer.create width in
    for x = 0 to width - 1 do
      let cell = Render.get buffer x y in
      match cell.Render.chars with
      | [] -> Buffer.add_char line ' '
      | ch :: _ -> Buffer.add_utf_8_uchar line ch
    done;
    lines := Buffer.contents line :: !lines
  done;
  !lines

(** Create a test terminal with predefined input for unit testing low-level
    components like the event source or input parser. *)
let make_test_terminal input =
  let term, get_output = Terminal.create_from_strings input in
  (term, get_output)

(** Renders a UI element to a raw string representation of the terminal grid.
    This is the foundation for visual snapshot and expect testing. *)
let render_to_string ?(width = 80) ?(height = 24) element =
  let buffer = Render.create width height in
  Ui.render buffer element;
  let buf = Buffer.create ((width + 1) * height) in
  for y = 0 to height - 1 do
    let cell_line =
      List.init width (fun x ->
          let cell = Render.get buffer x y in
          match cell.Render.chars with
          | [] -> " " (* For continuation of wide chars *)
          | chs ->
              let b = Buffer.create 4 in
              List.iter (Uutf.Buffer.add_utf_8 b) chs;
              Buffer.contents b)
    in
    Buffer.add_string buf (String.concat "" cell_line);
    if y < height - 1 then Buffer.add_char buf '\n'
  done;
  Buffer.contents buf

(** Helper to render a UI element to a string and print it for expect testing *)
let print_ui ?(width = 20) ?(height = 5) element =
  let output = render_to_string ~width ~height element in
  (* Add a visual border to make the output clearer in the test file *)
  let border_line = "+" ^ String.make width '-' ^ "+\n" in
  let lines = String.split_on_char '\n' output in
  let bordered_output =
    List.map
      (fun line ->
        let len = String.length line in
        let padded_line =
          if len < width then line ^ String.make (width - len) ' ' else line
        in
        "|" ^ padded_line ^ "|")
      lines
    |> String.concat "\n"
  in
  print_string ("\n" ^ border_line ^ bordered_output ^ "\n" ^ border_line)

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
  | _ -> false

(** Show an event as a string *)
let rec show_event = function
  | Input.Key { key; modifier } ->
      Printf.sprintf "Key(%s, ctrl=%b, alt=%b, shift=%b)" (show_key key)
        modifier.ctrl modifier.alt modifier.shift
  | Input.Mouse m -> Printf.sprintf "Mouse(%s)" (show_mouse m)
  | Input.Resize (w, h) -> Printf.sprintf "Resize(%d, %d)" w h
  | Input.Focus -> "Focus"
  | Input.Blur -> "Blur"
  | Input.Paste_start -> "Paste_start"
  | Input.Paste_end -> "Paste_end"
  | Input.Paste s -> Printf.sprintf "Paste(%S)" s

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

and show_mouse = function
  | Press (x, y, btn, _) ->
      Printf.sprintf "Press(%d,%d,%s)" x y (show_button btn)
  | Release (x, y, btn, _) ->
      Printf.sprintf "Release(%d,%d,%s)" x y (show_button btn)
  | Motion (x, y, _, _) -> Printf.sprintf "Motion(%d,%d)" x y

and show_button = function
  | Left -> "Left"
  | Middle -> "Middle"
  | Right -> "Right"
  | Wheel_up -> "Wheel_up"
  | Wheel_down -> "Wheel_down"
  | Button n -> Printf.sprintf "Button%d" n

(** Pretty printer for events for use with Alcotest *)
let pp_event fmt event = Fmt.string fmt (show_event event)

(** Alcotest testable for events *)
let event_testable = Alcotest.testable pp_event event_equal

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
