(** Tests for the Cmd module *)

open Mosaic

let test_none_cmd () =
  (* We can't pattern match on abstract types, just check it compiles *)
  let _ = Cmd.none in
  ()

let test_msg_cmd () =
  (* We can't pattern match on abstract types, just check it compiles *)
  let _ = Cmd.msg `TestMsg in
  ()

let test_quit_cmd () =
  (* We can't pattern match on abstract types, just check it compiles *)
  let _ = Cmd.quit in
  ()

let test_batch_cmd () =
  (* Test batch creation compiles *)
  let _ = Cmd.batch [] in
  let _ = Cmd.batch [ Cmd.msg `Test ] in
  let _ = Cmd.batch [ Cmd.msg `A; Cmd.msg `B ] in
  let _ = Cmd.batch [ Cmd.none; Cmd.msg `A; Cmd.none ] in
  ()

let test_seq_cmd () =
  (* Test sequence creation compiles *)
  let _ = Cmd.seq [] in
  let _ = Cmd.seq [ Cmd.msg `Test ] in
  let _ = Cmd.seq [ Cmd.msg `A; Cmd.msg `B ] in
  ()

let test_after_cmd () =
  (* Test after creation compiles *)
  let _ = Cmd.after 1.0 `DelayedMsg in
  ()

let test_tick_cmd () =
  (* Test tick creation compiles *)
  let _ = Cmd.tick 2.0 (fun elapsed -> `Elapsed elapsed) in
  ()

let test_log_cmd () =
  (* Test log creation compiles *)
  let _ = Cmd.log "test message" in
  ()

let test_window_title_cmd () =
  (* Test set_window_title creation compiles *)
  let _ = Cmd.set_window_title "Test App" in
  ()

let test_perform_cmd () =
  (* Test perform creation compiles *)
  let _ = Cmd.perform (fun () -> Some `Result) in
  ()

let test_exec_cmd () =
  (* Test exec creation compiles *)
  let _ = Cmd.exec (fun () -> ()) `Done in
  ()

let test_map_cmd () =
  (* Test map function compiles *)
  let _ = Cmd.map (fun `A -> `B) (Cmd.msg `A) in
  let _ = Cmd.map (fun x -> x) Cmd.none in
  let _ = Cmd.map (fun `A -> `B) (Cmd.batch [ Cmd.msg `A; Cmd.msg `A ]) in
  let _ = Cmd.map (fun `A -> `B) (Cmd.perform (fun () -> Some `A)) in
  let _ = Cmd.map (fun x -> x) Cmd.quit in
  let _ = Cmd.map (fun x -> x) (Cmd.log "test") in
  ()

let tests =
  [
    ("none cmd", `Quick, test_none_cmd);
    ("msg cmd", `Quick, test_msg_cmd);
    ("quit cmd", `Quick, test_quit_cmd);
    ("batch cmd", `Quick, test_batch_cmd);
    ("seq cmd", `Quick, test_seq_cmd);
    ("after cmd", `Quick, test_after_cmd);
    ("tick cmd", `Quick, test_tick_cmd);
    ("log cmd", `Quick, test_log_cmd);
    ("window title cmd", `Quick, test_window_title_cmd);
    ("perform cmd", `Quick, test_perform_cmd);
    ("exec cmd", `Quick, test_exec_cmd);
    ("map cmd", `Quick, test_map_cmd);
  ]

let () = Alcotest.run "Cmd" [ ("commands", tests) ]
