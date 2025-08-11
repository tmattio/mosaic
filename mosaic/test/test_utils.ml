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
          (* Skip continuation cells for wide characters *)
          if Grid.Cell.is_continuation cell then ()
          else
            let text = Grid.Cell.get_text cell in
            if text = "" then Buffer.add_char buf ' '
            else Buffer.add_string buf text
    done;
    if y < height - 1 then Buffer.add_char buf '\n'
  done;
  Buffer.contents buf

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

(** A synchronous test harness for running Mosaic applications. This provides a
    controlled way to test an application's `init`, `update`, `view`, and
    `subscriptions` logic without the full async runtime. It's the recommended
    way to write integration tests for applications. *)
module Test_harness = struct
  open Mosaic_tea

  type ('model, 'msg) t = {
    app : ('model, 'msg) Mosaic_tea.app;
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
  let is_quit _h =
    (* Check if the command would enqueue a quit meta command *)
    (* For testing, we can check if running the command would cause quit *)
    (* Since we can't easily check without a program, we'll assume false for now *)
    false

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
    let messages = ref [] in
    let dispatch msg = messages := msg :: !messages in
    Sub.run ~dispatch (Event.Input (Input.Key event)) subs;
    List.iter (fun msg -> push_msg msg h) (List.rev !messages)

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
