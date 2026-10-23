(* The examples of matrix.mli and matrix_test.mli, as written there, run. *)

open Windtrap

(* [Matrix.next]'s loop, as its doc writes it. *)
let rec next app =
  match Matrix.step app ~now:(Matrix.clock ()) with
  | Some step -> step
  | None ->
      let timeout =
        match Matrix.deadline app with
        | None -> -1.
        | Some d -> Float.max 0. (Float.of_int (d - Matrix.clock ()) *. 1e-9)
      in
      (try ignore (Unix.select (Matrix.fds app) [] [] timeout)
       with Unix.Unix_error (EINTR, _, _) -> ());
      next app

(* The documented loop draws the first frame, then waits for the input,
   whose end it returns as [Stopped]. *)
let test_next () =
  let input, writer = Unix.pipe ~cloexec:true () in
  let output = Unix.openfile "/dev/null" [ O_WRONLY; O_CLOEXEC ] 0 in
  Fun.protect
    ~finally:(fun () -> List.iter Unix.close [ input; output ])
    (fun () ->
      let frames =
        Matrix.run ~input ~output (fun app ->
            let rec loop frames =
              match next app with
              | Frame ->
                  Matrix.draw app ignore;
                  if frames = 0 then Unix.close writer;
                  loop (frames + 1)
              | Stopped -> frames
              | Event _ | Timer _ | Posted _ -> loop frames
            in
            loop 0)
      in
      equal ~msg:"one frame, then the end of the input" int 1 frames)

(* [Matrix_test]'s preamble example. *)
let test_matrix_test () =
  let t = Matrix_test.create ~width:20 ~height:1 () in
  Fun.protect ~finally:(fun () -> Matrix_test.close t) @@ fun () ->
  Matrix.run ~input:(Matrix_test.input t) ~output:(Matrix_test.output t)
    ~config:(Matrix_test.config t)
  @@ fun app ->
  let keys = ref 0 in
  let rec until_idle () =
    match Matrix.step app ~now:(Matrix_test.now t) with
    | Some Frame ->
        Matrix.draw app (fun grid ->
            Matrix.Grid.draw_text grid ~x:0 ~y:0
              ~text:(Printf.sprintf "%d keys" !keys));
        until_idle ()
    | Some (Event (Key _)) ->
        incr keys;
        Matrix.request_redraw app;
        until_idle ()
    | Some _ -> until_idle ()
    | None -> (
        match Matrix.deadline app with
        | Some deadline ->
            Matrix_test.advance t (deadline - Matrix_test.now t);
            until_idle ()
        | None -> ())
  in
  until_idle ();
  Matrix_test.feed t "a";
  until_idle ();
  assert (String.trim (Matrix_test.screen t) = "1 keys")

(* Steps a run over [t] until it has nothing to do, drawing each frame with
   [on_frame]. *)
let until_idle t app on_frame =
  let rec go () =
    match Matrix.step app ~now:(Matrix_test.now t) with
    | Some Frame ->
        on_frame ();
        go ()
    | Some _ -> go ()
    | None -> (
        match Matrix.deadline app with
        | Some deadline ->
            Matrix_test.advance t (deadline - Matrix_test.now t);
            go ()
        | None -> ())
  in
  go ()

let with_run f =
  let t = Matrix_test.create ~width:20 ~height:3 () in
  Fun.protect ~finally:(fun () -> Matrix_test.close t) @@ fun () ->
  Matrix.run ~input:(Matrix_test.input t) ~output:(Matrix_test.output t)
    ~config:(Matrix_test.config t) (f t)

let hello grid = Matrix.Grid.draw_text grid ~x:0 ~y:0 ~text:"hello"

(* The diagnostics section's dump of every thirtieth frame: the file holds
   the frame drawn. *)
let test_dump_every_thirtieth_frame () =
  let dir = Filename.temp_dir "matrix_dumps" "" in
  with_run @@ fun t app ->
  let frames = ref 0 in
  let on_frame app view =
    incr frames;
    Matrix.draw app (fun grid ->
        view grid;
        if !frames mod 30 = 0 then Matrix.dump_frame ~dir app)
  in
  for _ = 1 to 30 do
    Matrix.request_redraw app;
    until_idle t app (fun () -> on_frame app hello)
  done;
  match Sys.readdir dir with
  | [| file |] ->
      let dump =
        In_channel.with_open_bin (Filename.concat dir file) In_channel.input_all
      in
      is_true ~msg:"the dump holds the frame"
        (String.length dump >= 5
        && Seq.exists
             (fun i -> String.sub dump i 5 = "hello")
             (Seq.init (String.length dump - 4) Fun.id))
  | files -> failf "%d dumps, expected one" (Array.length files)

(* [Debug_overlay]'s preamble example draws the panel over the view. *)
let draw_frame overlay app view =
  let start = Matrix.clock () in
  Matrix.draw app (fun grid ->
      view grid;
      Matrix.Debug_overlay.draw overlay
        ~metrics:(Matrix.Screen.last_metrics (Matrix.screen app))
        ~gc:(Gc.quick_stat ()) grid);
  Matrix.Debug_overlay.record overlay ~start ~ns:(Matrix.clock () - start)

let test_debug_overlay () =
  let t = Matrix_test.create ~width:40 ~height:30 () in
  Fun.protect ~finally:(fun () -> Matrix_test.close t) @@ fun () ->
  Matrix.run ~input:(Matrix_test.input t) ~output:(Matrix_test.output t)
    ~config:(Matrix_test.config t)
  @@ fun app ->
  let overlay = Matrix.Debug_overlay.create () in
  until_idle t app (fun () -> draw_frame overlay app hello);
  let screen = Matrix_test.screen t in
  let shows sub =
    let n = String.length sub in
    Seq.exists
      (fun i -> String.sub screen i n = sub)
      (Seq.init (max 0 (String.length screen - n + 1)) Fun.id)
  in
  is_true ~msg:"the view" (shows "hello");
  is_true ~msg:"the panel" (shows "frames: 0")

(* [set_cursor]'s example: a cursor shown from the first frame. *)
let test_cursor_from_the_first_frame () =
  with_run @@ fun t app ->
  Matrix.set_cursor app ~visible:true;
  until_idle t app (fun () -> Matrix.draw app hello);
  is_true ~msg:"shown" (Matrix.Screen.cursor (Matrix.screen app)).visible

let () =
  run "matrix.examples"
    [
      group "Examples"
        [
          test "Matrix.next's loop" test_next;
          test "Matrix_test's preamble" test_matrix_test;
          test "a dump of every thirtieth frame" test_dump_every_thirtieth_frame;
          test "Debug_overlay's preamble" test_debug_overlay;
          test "a cursor shown from the first frame"
            test_cursor_from_the_first_frame;
        ];
    ]
