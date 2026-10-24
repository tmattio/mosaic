(* Two flows of a chat application in mentat's shape (chat.ml), as a test of
   it would write them: a turn streamed in a session, whose goldens hold the
   whole screen and the cursor, with the window title and the notification
   it asks for; and the real program in a pseudo-terminal, whose goodbye is
   a golden of the primary screen it leaves. *)

module T = Matrix_test
module K = Matrix.Input.Keystroke

let print term = print_string (T.Screen.text (T.Terminal.screen term))

let%expect_test "a streamed turn, its clock, its title and its notification" =
  (* The provider gives the test the reply's stream; the session's queue
     runs the program's work, so the run settles at each step. *)
  let stream = ref ignore in
  let provider _prompt send = stream := send in
  let term = T.Terminal.create ~cols:44 ~rows:8 () in
  Mosaic_test.run term (Chat.app provider) @@ fun s ->
  let send d =
    !stream d;
    T.settle s
  in
  T.input s (T.Terminal.text term "say hello");
  T.input s (T.Terminal.key term Enter);
  print term;
  [%expect
    {|
    01 |chat — message the model
    02 |❯ say hello
    03 |⠋ Working… (0s · ctrl+c to quit)
    04 |>
    05 |enter to send · ctrl+c to quit
    06 |
    07 |
    08 |
    cursor hidden
    |}];
  print_endline (T.Terminal.title term);
  [%expect {| ◐ chat |}];
  T.advance s 5_000_000_000;
  print term;
  [%expect
    {|
    01 |chat — message the model
    02 |❯ say hello
    03 |⠋ Working… (5s · ctrl+c to quit)
    04 |>
    05 |enter to send · ctrl+c to quit
    06 |
    07 |
    08 |
    cursor hidden
    |}];
  send (Chat.Text "Hello");
  send (Chat.Text " there.");
  T.input s (T.Terminal.focus term false);
  send Chat.Done;
  print term;
  [%expect
    {|
    01 |chat — message the model
    02 |❯ say hello
    03 |⏺ Hello there.
    04 |  Worked for 5s
    05 |>
    06 |enter to send · ctrl+c to quit
    07 |
    08 |
    cursor hidden
    |}];
  print_endline (T.Terminal.title term);
  List.iter
    (function
      | Vte.Notification { title; body } ->
          Printf.printf "notification %S %S\n" title body
      | Vte.Bell -> print_endline "bell"
      | Vte.Clipboard _ -> print_endline "clipboard")
    (T.Terminal.requests term);
  [%expect {|
    ✳ chat
    notification "" "chat: the turn ended"
    |}];
  (* An idle program draws nothing while the clock moves. *)
  let frames = T.frames s in
  T.advance s 900_000_000;
  Printf.printf "frames while idle: %d\n" (T.frames s - frames);
  [%expect {| frames while idle: 0 |}]

let chat = Filename.concat (Sys.getcwd ()) "chat_main.exe"

let%expect_test "the goodbye follows the alternate screen's end" =
  if Sys.win32 then
    Windtrap.skip ~reason:"a pseudo-terminal, which Windows lacks" ();
  let term = T.Terminal.create ~cols:44 ~rows:6 () in
  let shows text term = T.Screen.find (T.Terminal.screen term) text <> [] in
  let status =
    T.Child.with_spawn term ~prog:chat ~args:[] @@ fun c ->
    T.Child.wait c ~within:10. (shows "message the model");
    (* The keys as the terminal sends them in the modes the program set. *)
    let ctrl_c () =
      T.Terminal.key term
        ~mods:{ K.no_mods with ctrl = true }
        (Char (Uchar.of_char 'c'))
    in
    T.Child.send c (ctrl_c ());
    T.Child.wait c ~within:10. (shows "press ctrl+c again to quit");
    T.Child.send c (ctrl_c ());
    T.Child.exit c ~within:10.
  in
  Printf.printf "exited: %b; alternate screen: %b\n" (status = Unix.WEXITED 0)
    (T.Terminal.modes term).alternate_screen;
  print term;
  [%expect
    {|
    exited: true; alternate screen: false
    01 |Goodbye from chat.
    02 |
    03 |
    04 |
    05 |
    06 |
    cursor 2,1 block blinking
    |}]
