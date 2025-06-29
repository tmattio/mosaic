(** Test utilities for Mosaic tests *)


open Mosaic

(** Create a test terminal with predefined input *)
let make_test_terminal input = 
  let term, get_output = Terminal.create_from_strings input in
  (term, get_output)

(** Global test log handler *)
let test_log_handler : (string -> unit) ref = ref (fun _ -> ())

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

(** Assert that two event lists are equal *)
let assert_events_equal actual expected =
  let rec compare_lists actual expected =
    match (actual, expected) with
    | [], [] -> ()
    | [], _ -> Alcotest.fail "Actual event list is shorter than expected"
    | _, [] -> Alcotest.fail "Actual event list is longer than expected"
    | a :: as', e :: es' ->
        if not (event_equal a e) then
          Alcotest.failf "Events do not match: actual=%s, expected=%s"
            (show_event a) (show_event e)
        else compare_lists as' es'
  in
  compare_lists actual expected

(** Capture stderr output during a function execution *)
let capture_stderr f =
  let temp_file = Filename.temp_file "mosaic_test_" ".err" in
  let old_stderr = Unix.dup Unix.stderr in
  let new_stderr =
    Unix.openfile temp_file [ Unix.O_WRONLY; Unix.O_CREAT; Unix.O_TRUNC ] 0o600
  in
  Unix.dup2 new_stderr Unix.stderr;
  Unix.close new_stderr;

  let result =
    try
      let r = f () in
      Unix.dup2 old_stderr Unix.stderr;
      Unix.close old_stderr;
      r
    with e ->
      Unix.dup2 old_stderr Unix.stderr;
      Unix.close old_stderr;
      raise e
  in

  let content =
    let ic = open_in temp_file in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Sys.remove temp_file;
    content
  in

  (result, content)

(** Test app builder helpers *)
module TestApp = struct
  type 'model t = {
    init : unit -> 'model * unit Cmd.t;
    update : [ `Msg ] -> 'model -> 'model * unit Cmd.t;
    view : 'model -> Ui.element;
    subscriptions : 'model -> unit Sub.t;
  }

  let simple_counter () =
    let init () = (0, Cmd.none) in
    let update msg model =
      match msg with
      | `Increment -> (model + 1, Cmd.none)
      | `Decrement -> (model - 1, Cmd.none)
      | `Quit -> (model, Cmd.quit)
    in
    let view model = Ui.text (Printf.sprintf "Counter: %d" model) in
    let subscriptions _ =
      Sub.keyboard_filter (function
        | { key = Char c; modifier = { ctrl = true; _ } }
          when Uchar.to_int c = Char.code 'C' ->
            Some `Quit
        | { key = Char c; _ } when Uchar.to_char c = '+' -> Some `Increment
        | { key = Char c; _ } when Uchar.to_char c = '-' -> Some `Decrement
        | { key = Char c; _ } when Uchar.to_char c = 'q' -> Some `Quit
        | _ -> None)
    in
    Mosaic.app ~init ~update ~view ~subscriptions ()
end

(** Run an app with a test terminal and return rendered frames and final model *)
let run_app_with_terminal term ~init ~update ~view ~subscriptions =
  (* We'll run the app with a custom runner that doesn't use Eio *)
  let rendered_frames = ref [] in
  let quit_requested = ref false in
  let final_model = ref None in
  
  (* Create event source *)
  let event_source = Event_source.create term in
  
  (* Capture the initial model *)
  let initial_model, initial_cmd = init () in
  final_model := Some initial_model;
  
  (* Simple synchronous loop *)
  let rec process_events model =
    if !quit_requested then model
    else
      (* Render the view *)
      let view_output = view model in
      let buffer = Render.create 80 24 in
      Ui.render buffer view_output;
      
      (* Capture the rendered frame *)
      let frame_str = Buffer.create 1024 in
      for y = 0 to 23 do
        for x = 0 to 79 do
          let cell = Render.get buffer x y in
          match cell.Render.chars with
          | [] -> Buffer.add_char frame_str ' '
          | ch :: _ -> Buffer.add_utf_8_uchar frame_str ch
        done;
        if y < 23 then Buffer.add_char frame_str '\n'
      done;
      rendered_frames := Buffer.contents frame_str :: !rendered_frames;
      
      (* Try to read an event *)
      match Event_source.read event_source ~timeout:(Some 0.001) with
      | `Event event ->
          (* Process event through subscriptions *)
          let sub = subscriptions model in
          let msg_opt = match event with
          | Input.Key key_event ->
              let handlers = Sub.collect_keyboard [] sub in
              let ke : Mosaic.key_event = Obj.magic key_event in
              List.find_map (fun f -> f ke) handlers
          | Input.Mouse mouse_event ->
              let handlers = Sub.collect_mouse [] sub in
              let me : Mosaic.mouse_event = Obj.magic mouse_event in
              List.find_map (fun f -> f me) handlers
          | Input.Resize (w, h) ->
              let handlers = Sub.collect_window [] sub in
              List.find_map (fun f -> f { Mosaic.Sub.width = w; height = h }) handlers
          | Input.Focus ->
              let handlers = Sub.collect_focus [] sub in
              List.find_map (fun f -> f ()) handlers
          | Input.Blur ->
              let handlers = Sub.collect_blur [] sub in
              List.find_map (fun f -> f ()) handlers
          | _ -> None
          in
          
          (match msg_opt with
          | Some msg ->
              let new_model, cmd = update msg model in
              final_model := Some new_model;
              (* Check if quit was requested *)
              (match cmd with
              | cmd when cmd = Cmd.quit -> 
                  quit_requested := true;
                  new_model
              | _ -> process_events new_model)
          | None -> process_events model)
      | `Timeout | `Eof ->
          (* No more events, check initial window size *)
          let sub = subscriptions model in
          let window_handlers = Sub.collect_window [] sub in
          if window_handlers <> [] then begin
            let w, h = Terminal.size term in
            match List.find_map (fun f -> f { Mosaic.Sub.width = w; height = h }) window_handlers with
            | Some msg ->
                let new_model, cmd = update msg model in
                final_model := Some new_model;
                if cmd = Cmd.quit then quit_requested := true;
                model
            | None -> model
          end else
            model
  in
  
  (* Process initial command if any *)
  (match initial_cmd with
  | cmd when cmd = Cmd.quit -> quit_requested := true
  | cmd when cmd <> Cmd.none ->
      (* For testing, we'll process simple messages synchronously *)
      ()
  | _ -> ());
  
  (* Run the event loop *)
  let _ = process_events initial_model in
  
  (* Return frames and final model *)
  match !final_model with
  | Some model -> (List.rev !rendered_frames, model)
  | None -> (List.rev !rendered_frames, initial_model)

(** Run an app with predefined input and capture output *)
let run_app_with_input input ~init ~update ~view ~subscriptions =
  let term, _ = Terminal.create_from_strings input in
  run_app_with_terminal term ~init ~update ~view ~subscriptions
