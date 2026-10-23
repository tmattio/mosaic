(* The chat application as a program, with a provider that streams a fixed
   reply, and a goodbye on the primary screen once the terminal is given
   back. *)

let provider prompt send =
  List.iter
    (fun word ->
      Thread.delay 0.02;
      send (Chat.Text word))
    [ "You"; " said: "; prompt ];
  send Chat.Done

let () =
  Mosaic.run (Chat.app provider);
  print_string "Goodbye from chat.\n"
