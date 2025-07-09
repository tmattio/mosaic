(** Test utilities for Mosaic tests *)

open Mosaic

(** Create a test terminal with predefined input *)
let make_test_terminal input =
  let term, get_output = Terminal.create_from_strings input in
  (term, get_output)

(** Renders a UI element to a string for snapshot testing. *)
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

(** A helper for writing concise layout tests. *)
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

(** A synchronous test harness for running Mosaic applications. *)
module TestHarness = struct
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
  let view ?width ?height h = render_to_string ?width ?height (h.app.view h.model)

  (** Push a message into the update loop. *)
  let push_msg msg h =
    let new_model, cmd = h.app.update msg h.model in
    h.model <- new_model;
    h.last_cmd <- cmd

  (** Simulate a keyboard event. *)
  let push_key_event event h =
    let subs = h.app.subscriptions h.model in
    let handlers = Sub.collect_keyboard [] subs in
    match List.find_map (fun f -> f event) handlers with
    | Some msg -> push_msg msg h
    | None -> ()

  (** Simulate a key press from a string representation (e.g., "q", "\x1b[A"). *)
  let push_input s h =
    let parser = Input.create () in
    let events = Input.feed parser (Bytes.of_string s) 0 (String.length s) in
    List.iter
      (function Input.Key event -> push_key_event event h | _ -> ())
      events
end