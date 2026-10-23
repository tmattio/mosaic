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

let () =
  run "matrix.examples"
    [
      group "Examples"
        [
          test "Matrix.next's loop" test_next;
          test "Matrix_test's preamble" test_matrix_test;
        ];
    ]
