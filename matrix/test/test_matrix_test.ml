(* Tests of matrix.test: sessions on an emulated terminal, the clock and
   settling, input encoded for the program's modes, goldens, and a child on a
   pseudo-terminal. *)

open Windtrap
module T = Matrix_test
module K = Matrix.Input.Keystroke

let ms n = n * 1_000_000

(* A matrix program that waits with [Matrix.next], draws [view] on each
   frame and hands every other step to [on], which says whether the program
   goes on; [body] drives it. *)
let session ?(profile = T.Profile.vte) ?(cols = 20) ?(rows = 2)
    ?(config = Matrix.config ()) ?(view = fun _ _ -> ()) ?(on = fun _ _ -> true)
    body =
  let term = T.Terminal.create ~profile ~cols ~rows () in
  let app = ref None in
  T.run ~config term
    (fun s ->
      Matrix.run ~config:(T.config s) @@ fun a ->
      app := Some a;
      let rec loop () =
        match Matrix.next a with
        | Stopped -> ()
        | Frame ->
            Matrix.draw a (view a);
            loop ()
        | st -> if on a st then loop ()
      in
      loop ())
    (fun s -> body s term (Option.get !app));
  term

(* The same program, taking its steps through the session. *)
let stepped ?(cols = 20) ?(rows = 2) ?(view = fun _ _ -> ())
    ?(on = fun _ _ -> true) body =
  let term = T.Terminal.create ~cols ~rows () in
  let s = T.create term in
  Matrix.run ~config:(T.config s) (fun app ->
      let step ~now =
        match Matrix.step app ~now with
        | None -> `Idle
        | Some Stopped -> `Stopped
        | Some Frame ->
            Matrix.draw app (view app);
            `Stepped
        | Some st -> if on app st then `Stepped else `Stopped
      in
      T.attach s ~step ~deadline:(fun () -> Matrix.deadline app);
      body s term app);
  term

let text term = T.Screen.text (T.Terminal.screen term)
let row term y = T.Screen.row (T.Terminal.screen term) y
let draw text _app grid = Matrix.Grid.draw_text grid ~x:0 ~y:0 ~text:(text ())

(* Steps ------------------------------------------------------------------ *)

let test_settles_at_its_time () =
  let keys = ref 0 in
  let on app = function
    | Matrix.Event (Key _) ->
        incr keys;
        Matrix.request_redraw app;
        true
    | _ -> true
  in
  ignore
  @@ session ~on ~view:(draw (fun () -> Printf.sprintf "%d keys" !keys))
  @@ fun s term _ ->
  equal ~msg:"booted" string "01 |0 keys\n02 |\ncursor hidden\n" (text term);
  T.input s (T.Terminal.text term "ab");
  equal ~msg:"typed" string "2 keys" (row term 0);
  T.input s (T.Terminal.key term Escape);
  equal ~msg:"a lone ESC is Escape at once" string "3 keys" (row term 0);
  equal ~msg:"the clock" int 0 (T.now s);
  equal ~msg:"no deadline" (option int) None (T.deadline s)

(* A program that takes its own steps settles the same way. *)
let test_stepped () =
  let keys = ref 0 in
  let on app = function
    | Matrix.Event (Key _) ->
        incr keys;
        Matrix.request_redraw app;
        true
    | _ -> true
  in
  ignore
  @@ stepped ~on ~view:(draw (fun () -> Printf.sprintf "%d keys" !keys))
  @@ fun s term _ ->
  T.input s (T.Terminal.text term "abc");
  equal ~msg:"typed" string "3 keys" (row term 0);
  equal ~msg:"the clock" int 0 (T.now s)

(* The profile answers the program's probe as it writes it, in no time. *)
let test_probe () =
  ignore @@ session
  @@ fun s term app ->
  let caps = Matrix.capabilities app in
  is_true ~msg:"synchronized output" caps.sync;
  is_true ~msg:"kitty keyboard" (caps.kitty_keyboard <> None);
  is_true ~msg:"focus reports" caps.focus_tracking;
  is_true ~msg:"raw" (T.Terminal.raw term);
  is_true ~msg:"the alternate screen" (T.Terminal.modes term).alternate_screen;
  is_false ~msg:"no update open" (T.Terminal.modes term).synchronized_output;
  equal ~msg:"kitty flags pushed" int 5
    (Vte.keyboard (T.Terminal.vte term)).kitty;
  equal ~msg:"the clock" int 0 (T.now s)

(* A terminal that answers nothing costs the run's wait for the answer. *)
let test_silent () =
  let silent = { T.Profile.vte with answers = false } in
  ignore @@ session ~profile:silent
  @@ fun s _ app ->
  equal ~msg:"the probe's wait" int (ms 500) (T.now s);
  is_true ~msg:"no kitty protocol"
    ((Matrix.capabilities app).kitty_keyboard = None)

let test_pipe () =
  let size app () =
    let c, r = Matrix.size app in
    Printf.sprintf "%dx%d" c r
  in
  let term =
    session ~profile:T.Profile.pipe ~cols:30 ~view:(fun app grid ->
        draw (size app) app grid)
    @@ fun s term _ ->
    equal ~msg:"the size, in band" string "30x2" (row term 0);
    is_false ~msg:"no raw mode" (T.Terminal.raw term);
    T.resize s ~cols:12 ~rows:2;
    equal ~msg:"resized" string "12x2" (row term 0)
  in
  is_false ~msg:"no alternate screen" (T.Terminal.modes term).alternate_screen

(* Input ------------------------------------------------------------------ *)

let events = ref []

let record app = function
  | Matrix.Event (Resize _) as e ->
      events := e :: !events;
      ignore app;
      true
  | Matrix.Event e ->
      events := Matrix.Event e :: !events;
      true
  | _ -> true

let seen () =
  let l =
    List.rev_map
      (function
        | Matrix.Event e -> Format.asprintf "%a" Matrix.pp_event e | _ -> "")
      !events
  in
  events := [];
  l

(* The bytes of a key follow the program's modes: the kitty protocol's flags
   it pushed, or modifyOtherKeys, or neither on a terminal with none. *)
let test_keys_in_the_program's_encoding () =
  let shift_enter term =
    T.Terminal.key term ~mods:{ K.no_mods with shift = true } Enter
  in
  (ignore @@ session ~on:record
  @@ fun s term _ ->
  events := [];
  T.input s (shift_enter term);
  equal ~msg:"kitty" (list string) [ "Key Shift+Enter" ] (seen ()));
  (ignore
  @@ session ~on:record ~config:(Matrix.config ~kitty_keyboard:`Off ())
  @@ fun s term _ ->
  events := [];
  T.input s (shift_enter term);
  equal ~msg:"modifyOtherKeys" string "\027[27;2;13~" (shift_enter term);
  equal ~msg:"modifyOtherKeys, read" (list string) [ "Key Shift+Enter" ]
    (seen ()));
  ignore
  @@ session ~on:record ~profile:{ T.Profile.vte with extensions = [] }
  @@ fun s term _ ->
  events := [];
  T.input s (shift_enter term);
  equal ~msg:"legacy bytes lose Shift" (list string) [ "Key Enter" ] (seen ())

let test_mouse_paste_focus () =
  ignore
  @@ session ~on:record ~config:(Matrix.config ~mouse:`Sgr_any ())
  @@ fun s term _ ->
  events := [];
  T.input s
    (T.Terminal.mouse term
       {
         kind = Press Left;
         x = 3;
         y = 1;
         shift = false;
         alt = false;
         ctrl = true;
       });
  T.input s (T.Terminal.paste term "hi");
  T.input s (T.Terminal.focus term false);
  T.input s (T.Terminal.color_scheme term `Light);
  equal ~msg:"events" (list string)
    [
      "Mouse Press Left at 3,1 ctrl";
      "Paste_start";
      "Paste_data \"hi\"";
      "Paste_end Complete";
      "Focus_out";
      "Color_scheme Light";
    ]
    (seen ())

let test_resize () =
  ignore
  @@ session ~on:record ~cols:20
  @@ fun s term _ ->
  events := [];
  T.resize s ~cols:10 ~rows:3;
  equal ~msg:"the event" (list string) [ "Resize 10x3" ] (seen ());
  equal ~msg:"drawn at once" int 10 (T.Screen.cols (T.Terminal.screen term));
  equal ~msg:"the clock" int 0 (T.now s)

(* Time -------------------------------------------------------------------- *)

(* Every timer on the way has an instant of its own. *)
let test_advance () =
  let fired = ref [] in
  let on app = function
    | Matrix.Timer _ ->
        fired := Matrix.now app :: !fired;
        ignore (Matrix.at app (Matrix.now app + ms 100) : Matrix.timer);
        true
    | _ -> true
  in
  ignore @@ session ~on
  @@ fun s _ app ->
  ignore (Matrix.at app (ms 100) : Matrix.timer);
  T.advance s (ms 1000);
  equal ~msg:"each at its time" (list int)
    (List.init 10 (fun i -> ms (100 * (i + 1))))
    (List.rev !fired);
  equal ~msg:"the clock" int (ms 1000) (T.now s)

(* An animation asks for its next frame after drawing one: a virtual second
   at 30 frames a second draws 30, and a settle draws none. *)
let test_animation () =
  let view app grid =
    Matrix.Grid.draw_text grid ~x:0 ~y:0 ~text:"*";
    ignore grid;
    Matrix.request_redraw app
  in
  let config = Matrix.config ~fps:30. () in
  ignore
  @@ session ~config ~view:(fun app grid -> view app grid)
  @@ fun s _ _ ->
  let before = T.frames s in
  T.settle s;
  equal ~msg:"a settle draws nothing" int before (T.frames s);
  T.advance s (ms 1000);
  equal ~msg:"thirty a second" int 30 (T.frames s - before)

(* A key that comes at the instant a timer is due is read in the turn that
   takes the timer, and before it. *)
let test_set_time () =
  let order = ref [] in
  let on _ = function
    | Matrix.Event (Key _) ->
        order := "key" :: !order;
        true
    | Timer _ ->
        order := "timer" :: !order;
        true
    | _ -> true
  in
  ignore @@ session ~on
  @@ fun s term app ->
  ignore (Matrix.at app (ms 100) : Matrix.timer);
  T.set_time s (ms 100);
  equal ~msg:"no step" (list string) [] !order;
  T.input s (T.Terminal.text term "a");
  equal ~msg:"the key first" (list string) [ "key"; "timer" ] (List.rev !order);
  raises_match ~msg:"back in time" (Exn.invalid_arg ~substring:"set_time")
    (fun () -> T.set_time s 0)

(* The host and the end of a run ---------------------------------------- *)

let test_requests_and_end () =
  let on app = function
    | Matrix.Event (Key { key = Char c; _ }) when Uchar.to_int c = 0x71 -> false
    | Matrix.Event (Key _) ->
        let t = Matrix.terminal app in
        Matrix.Terminal.set_title t "hello";
        Matrix.Terminal.bell t;
        Matrix.Terminal.notify t ~title:"t" ~body:"b";
        Matrix.Terminal.copy_to_clipboard t "xy";
        true
    | _ -> true
  in
  let term =
    session ~on ~view:(draw (fun () -> "running")) @@ fun s term _ ->
    T.input s (T.Terminal.text term "a");
    equal ~msg:"the title" string "hello" (T.Terminal.title term);
    equal ~msg:"the requests: a bell, one notification, a clipboard write" int 3
      (List.length (T.Terminal.requests term));
    T.input s (T.Terminal.text term "q");
    is_true ~msg:"ended" (T.ended s);
    raises_match ~msg:"input after the end" (Exn.invalid_arg ~substring:"ended")
      (fun () -> T.input s "a")
  in
  is_false ~msg:"the alternate screen is left"
    (T.Terminal.modes term).alternate_screen;
  is_false ~msg:"cooked mode" (T.Terminal.raw term);
  equal ~msg:"the title is put back" string "" (T.Terminal.title term)

(* Goldens ---------------------------------------------------------------- *)

let styled_view app grid =
  let red = Matrix.Ansi.Style.make ~fg:Matrix.Ansi.Color.red ~bold:true () in
  Matrix.Grid.draw_text grid ~x:0 ~y:0 ~text:"plain ";
  Matrix.Grid.draw_text grid ~style:red ~x:6 ~y:0 ~text:"red";
  Matrix.Grid.draw_text grid ~x:0 ~y:1 ~text:"wide 日本";
  Matrix.set_cursor app ~visible:true ~style:`Line;
  Matrix.set_cursor_position app ~row:2 ~col:3

let test_goldens () =
  let term = session ~view:styled_view (fun _ _ _ -> ()) in
  ignore term;
  ignore @@ session ~view:styled_view
  @@ fun _ term _ ->
  let screen = T.Terminal.screen term in
  equal ~msg:"text" string "01 |plain red\n02 |wide 日本\ncursor 2,3 bar\n"
    (T.Screen.text screen);
  equal ~msg:"styled" string
    "01 |plain red\n   |      aaa\n02 |wide 日本\ncursor 2,3 bar\na fg=Red bold\n"
    (T.Screen.styled screen);
  equal ~msg:"find"
    (list (pair int int))
    [ (0, 6); (1, 5) ]
    (T.Screen.find screen "red" @ T.Screen.find screen "日");
  equal ~msg:"no difference" (option string) None (T.Screen.diff screen screen)

let test_diff () =
  let text = ref "abcd" and style = ref Matrix.Ansi.Style.default in
  let view _ grid =
    Matrix.Grid.draw_text grid ~style:!style ~x:0 ~y:0 ~text:!text
  in
  let on app = function
    | Matrix.Event (Key _) ->
        text := "abXd";
        style := Matrix.Ansi.Style.make ~bg:Matrix.Ansi.Color.blue ();
        Matrix.request_redraw app;
        true
    | _ -> true
  in
  ignore @@ session ~view ~on
  @@ fun s term _ ->
  let before = T.Terminal.screen term in
  T.input s (T.Terminal.text term "k");
  equal ~msg:"marks" (option string) (Some "01 |abXd\n   |bbXb\n")
    (T.Screen.diff before (T.Terminal.screen term))

(* The oracle ---------------------------------------------------------------- *)

(* The terminal shows the frame the run presented, through input, a resize
   and in the primary screen; and a program that writes behind its frames,
   as a broken renderer does, is caught. *)
let test_presented () =
  let typed = Buffer.create 8 in
  let on app = function
    | Matrix.Event (Key { key = Char c; _ }) ->
        Buffer.add_utf_8_uchar typed c;
        Matrix.request_redraw app;
        true
    | _ -> true
  in
  let view = draw (fun () -> "typed " ^ Buffer.contents typed) in
  List.iter
    (fun mode ->
      Buffer.clear typed;
      ignore
      @@ session ~config:(Matrix.config ~mode ()) ~rows:4 ~on ~view
      @@ fun s term _ ->
      T.check_presented s;
      T.input s (T.Terminal.text term "abc");
      T.check_presented s;
      T.resize s ~cols:12 ~rows:3;
      T.check_presented s)
    [ `Alt; `Primary ];
  let broken app grid =
    view app grid;
    (* Behind the frame's back: the diff does not know of it. *)
    Matrix.Terminal.send (Matrix.terminal app) "\027[1;1HXX"
  in
  ignore @@ session ~on ~view:broken
  @@ fun s term _ ->
  T.input s (T.Terminal.text term "z");
  raises_match ~msg:"caught"
    (function
      | Failure m ->
          String.length m > 0
          && T.Screen.find (T.Terminal.screen term) "XXped" <> []
      | _ -> false)
    (fun () -> T.check_presented s)

(* A child on a pseudo-terminal ------------------------------------------ *)

let child =
  Filename.concat (Filename.dirname Sys.executable_name) "runtime_child.exe"

let test_child () =
  let term = T.Terminal.create ~cols:30 ~rows:3 () in
  let shows text term = T.Screen.find (T.Terminal.screen term) text <> [] in
  let status =
    T.Child.with_spawn term ~prog:child ~args:[ "goodbye" ] @@ fun c ->
    T.Child.wait c ~within:10. (shows "SIZE 30x3");
    is_true ~msg:"answered its probe" (T.Terminal.modes term).alternate_screen;
    T.Child.resize c ~cols:25 ~rows:3;
    T.Child.wait c ~within:10. (shows "SIZE 25x3");
    T.Child.send c (T.Terminal.text term "q");
    T.Child.exit c ~within:10.
  in
  is_true ~msg:"exited" (status = Unix.WEXITED 0);
  is_false ~msg:"the alternate screen is left"
    (T.Terminal.modes term).alternate_screen;
  equal ~msg:"the goodbye, on the primary screen" string
    "01 |GOODBYE\n02 |\n03 |\ncursor 2,1 block blinking\n" (text term)

let test_child_timeout () =
  let term = T.Terminal.create ~cols:30 ~rows:3 () in
  T.Child.with_spawn term ~prog:child ~args:[ "goodbye" ] @@ fun c ->
  raises_match ~msg:"never ready"
    (fun e -> match e with Failure m -> String.length m > 0 | _ -> false)
    (fun () -> T.Child.wait c ~within:0.2 (fun _ -> false));
  T.Child.send c "q";
  ignore (T.Child.exit c ~within:10. : Unix.process_status)

let () =
  run "matrix.test"
    [
      group "Sessions"
        [
          test "a session settles at its time" test_settles_at_its_time;
          test "a program that takes its own steps" test_stepped;
          test "the probe is answered in no time" test_probe;
          test "a silent terminal costs its wait" test_silent;
          test "a pipe" test_pipe;
        ];
      group "Input"
        [
          test "keys in the program's encoding"
            test_keys_in_the_program's_encoding;
          test "mouse, paste, focus and scheme" test_mouse_paste_focus;
          test "a resize" test_resize;
        ];
      group "Time"
        [
          test "advance visits each deadline" test_advance;
          test "an animation keeps its rate" test_animation;
          test "set_time puts a key before a due timer" test_set_time;
        ];
      group "Host and end"
        [ test "requests, and the end" test_requests_and_end ];
      group "Goldens"
        [ test "text, styles, find" test_goldens; test "diff" test_diff ];
      group "Oracle"
        [ test "the terminal shows the presented frame" test_presented ];
      group "Child"
        [
          test "a child, resized, and its goodbye" test_child;
          test "a wait that times out" test_child_timeout;
        ];
    ]
