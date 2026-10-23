open Windtrap

type message = Activate | Focus

type trace = {
  mutable messages : message list;
  mutable focused : bool;
  mutable mapped : int;
}

type step =
  | Bytes of string
  | Key of Matrix.Input.Keystroke.t
  | Release of Matrix.Input.Keystroke.release
  | Check of (trace -> Matrix_test.Terminal.t -> unit)

let button focused disabled =
  Mosaic.button
    ~size:(Mosaic.size ~width:10 ~height:1)
    ~focused ~disabled ~on_focus_request:Focus ~on_activate:Activate "Save"

let drive ?(focused = true) ?(disabled = false) ?(accept_focus = true)
    ?(mapped = false) ?(capture = false) build steps =
  let trace = { messages = []; focused; mapped = 0 } in
  let map_message message =
    trace.mapped <- trace.mapped + 1;
    message
  in
  let application =
    {
      Mosaic.init = (fun () -> (focused, Mosaic.Cmd.none));
      update =
        (fun message focused ->
          trace.messages <- message :: trace.messages;
          let focused =
            match message with Focus when accept_focus -> true | _ -> focused
          in
          trace.focused <- focused;
          (focused, Mosaic.Cmd.none));
      view =
        (fun focused ->
          let view = build focused disabled in
          if mapped then Mosaic.map map_message view else view);
      subscriptions =
        (fun _ ->
          if capture then
            Mosaic.Sub.on_key ~phase:Capture (fun event ->
                Mosaic.Event.Key.prevent_default event;
                None)
          else Mosaic.Sub.none);
    }
  in
  (* Key releases have the program ask the kitty protocol for every
     enhancement, so the terminal sends every fact of a keystroke. *)
  let term = Matrix_test.Terminal.create ~cols:20 ~rows:2 () in
  let s = Matrix_test.create term in
  let config = Matrix_test.config s (Matrix.config ~key_releases:true ()) in
  Mosaic.start ~config ~process_perform:(Matrix_test.perform s) application
    (fun r ->
      Matrix_test.attach s ~step:(Mosaic.step r) ~deadline:(fun () ->
          Mosaic.deadline r);
      List.iter
        (function
          | Bytes bytes -> Matrix_test.input s bytes
          | Key k -> Matrix_test.input s (Matrix_test.Terminal.keystroke term k)
          | Release r ->
              Matrix_test.input s (Matrix_test.Terminal.release term r)
          | Check check -> check trace term)
        steps);
  trace

let activation_count trace =
  List.length
    (List.filter (function Activate -> true | Focus -> false) trace.messages)

let press ?mods key = Key (Matrix.Input.Keystroke.make ?mods key)

let key_presses_only () =
  let keys =
    Matrix.Input.Keystroke.
      [ (Enter, false); (Enter, true); (Char (Uchar.of_char ' '), false) ]
  in
  let steps =
    List.concat_map
      (fun (key, keypad) ->
        [
          Key (Matrix.Input.Keystroke.make ~keypad key);
          Key (Matrix.Input.Keystroke.make ~keypad ~repeat:true key);
          Release
            { key; mods = Matrix.Input.Keystroke.no_mods; base = None; keypad };
        ])
      keys
  in
  let trace = drive button steps in
  equal ~msg:"one activation per press" int 3 (activation_count trace)

let modifiers_and_capture () =
  let none = Matrix.Input.Keystroke.no_mods in
  let modified =
    [
      { none with ctrl = true };
      { none with alt = true };
      { none with shift = true };
      { none with super = true };
      { none with hyper = true };
      { none with meta = true };
    ]
  in
  let trace =
    drive button (List.map (fun mods -> press ~mods Enter) modified)
  in
  equal int 0 (activation_count trace);
  let trace =
    drive button
      [ press ~mods:{ none with caps_lock = true; num_lock = true } Enter ]
  in
  equal ~msg:"locks do not change activation" int 1 (activation_count trace);
  let trace = drive ~capture:true button [ press Enter ] in
  equal ~msg:"global capture prevents activation" int 0 (activation_count trace)

let activation_consumes_the_key () =
  let ancestor = ref 0 in
  let build focused disabled =
    Mosaic.box
      ~on_key:(fun _ ->
        incr ancestor;
        None)
      [ button focused disabled ]
  in
  let trace = drive build [ press Enter; press (Char (Uchar.of_char 'x')) ] in
  equal ~msg:"Enter activates" int 1 (activation_count trace);
  equal ~msg:"only the key the button did not act on reaches its ancestor" int 1
    !ancestor

let pointer_focus_before_activation () =
  let trace =
    drive ~focused:false button
      [ Bytes "\027[<0;2;1M\027[<0;2;1m"; press Enter ]
  in
  equal (list string)
    [ "focus"; "activate"; "activate" ]
    (List.rev_map
       (function Focus -> "focus" | Activate -> "activate")
       trace.messages);
  is_true trace.focused;
  let rejected =
    drive ~focused:false ~accept_focus:false button
      [ Bytes "\027[<0;2;1M\027[<0;2;1m"; press Enter ]
  in
  is_false rejected.focused;
  equal int 0 (activation_count rejected)

let click_requires_complete_gesture () =
  List.iter
    (fun (description, bytes) ->
      let trace = drive button [ Bytes bytes ] in
      equal ~msg:description int 0 (activation_count trace))
    [
      ("release without press", "\027[<0;2;1m");
      ("press outside", "\027[<0;12;1M\027[<0;2;1m");
      ("release outside", "\027[<0;2;1M\027[<0;12;1m");
      ("right button", "\027[<2;2;1M\027[<2;2;1m");
      ("drag inside", "\027[<0;2;1M\027[<32;4;1M\027[<0;4;1m");
      ( "drag out and back",
        "\027[<0;2;1M\027[<32;12;1M\027[<32;2;1M\027[<0;2;1m" );
    ];
  let trace = drive button [ Bytes "\027[<0;2;1M\027[<0;2;1m" ] in
  equal ~msg:"press and release inside" int 1 (activation_count trace)

let disabled_blocks_input () =
  let trace =
    drive ~disabled:true button
      [
        Bytes "\027[<0;2;1M\027[<0;2;1m";
        press Enter;
        press (Char (Uchar.of_char ' '));
      ]
  in
  equal ~msg:"no activation or focus request" int 0 (List.length trace.messages)

let mlx_and_mapping () =
  let build focused disabled =
    Mosaic_mlx.button
      ~size:(Mosaic.size ~width:10 ~height:1)
      ~focused ~disabled ~on_focus_request:Focus ~on_activate:Activate
      ~children:[ "Sa"; "ve" ] ()
  in
  let trace =
    drive ~focused:false ~mapped:true build
      [
        Check
          (fun trace terminal ->
            equal ~msg:"mapping does not dispatch during view construction" int
              0 trace.mapped;
            contains ~sub:"Save"
              (Matrix_test.Screen.text (Matrix_test.Terminal.screen terminal)));
        Bytes "\027[<0;2;1M\027[<0;2;1m";
      ]
  in
  equal int 1 (activation_count trace);
  equal ~msg:"focus and activation mapped at delivery" int 2 trace.mapped

let visible_states_preserve_label_style () =
  let module Style = Mosaic.Ansi.Style in
  let module Attr = Mosaic.Ansi.Attr in
  let base = Style.make ~fg:Mosaic.Ansi.Color.cyan ~bold:true () in
  let build focused disabled =
    Mosaic.button
      ~size:(Mosaic.size ~width:10 ~height:1)
      ~text_style:base ~focused ~disabled ~on_activate:Activate "Save"
  in
  List.iter
    (fun (focused, disabled, inverse, dim) ->
      ignore
        (drive ~focused ~disabled build
           [
             Check
               (fun _ terminal ->
                 let _, style =
                   Matrix_test.Screen.cell
                     (Matrix_test.Terminal.screen terminal)
                     ~row:0 ~col:0
                 in
                 equal ~msg:"focus indication" bool inverse
                   (Attr.mem Inverse style.attrs);
                 equal ~msg:"disabled indication" bool dim
                   (Attr.mem Dim style.attrs);
                 is_true ~msg:"caller attribute preserved"
                   (Attr.mem Bold style.attrs);
                 is_true ~msg:"caller color preserved"
                   (Option.equal Mosaic.Ansi.Color.equal
                      (Some Mosaic.Ansi.Color.cyan) style.fg));
           ]
          : trace))
    [
      (false, false, false, false);
      (true, false, true, false);
      (true, true, false, true);
    ];
  is_false ~msg:"caller style remains immutable" (Attr.mem Inverse base.attrs);
  is_false (Attr.mem Dim base.attrs)

let () =
  run "Button"
    [
      test "keyboard press semantics" key_presses_only;
      test "modifiers and capture" modifiers_and_capture;
      test "activation consumes the key" activation_consumes_the_key;
      test "pointer focus acceptance" pointer_focus_before_activation;
      test "complete primary click gesture" click_requires_complete_gesture;
      test "disabled blocks input" disabled_blocks_input;
      test "MLX labels and deferred mapping" mlx_and_mapping;
      test "visible focus and disabled states preserve label styling"
        visible_states_preserve_label_style;
    ]
