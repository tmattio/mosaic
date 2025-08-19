open Alcotest

let int_eq = (check int) "int equal"
let bool_eq = (check bool) "bool equal"

let test_at () =
  (* Create a fresh incremental instance for this test *)
  let module I = Incremental.Make () in
  let open I in
  
  (* Create clock with start time 0 for this test *)
  Clock.create ~start:0. ();
  
  let now = Clock.now () in
  let target_time = now +. 5. in
  
  stabilize ();
  
  (* Create an 'at' node for future time *)
  let at_node = at target_time in
  let at_obs = observe at_node in
  stabilize ();
  
  (* Should be Before initially *)
  (match Observer.value_exn at_obs with
   | Before -> ()
   | After -> fail "Expected Before");
  
  (* Advance past the target time *)
  Clock.advance_clock ~to_:(now +. 6.) ();
  stabilize ();
  
  (* Should be After now *)
  (match Observer.value_exn at_obs with
   | After -> ()
   | Before -> fail "Expected After")

let test_after () =
  let module I = Incremental.Make () in
  let open I in
  Clock.create ~start:0. ();
  stabilize ();
  
  (* Create an 'after' node for 3 seconds from now *)
  let after_node = after 3. in
  let after_obs = observe after_node in
  stabilize ();
  
  (* Should be Before initially *)
  (match Observer.value_exn after_obs with
   | Before -> ()
   | After -> fail "Expected Before");
  
  (* Advance 4 seconds *)
  Clock.advance_clock ~to_:4. ();
  stabilize ();
  
  (* Debug: print what the value is *)
  let val_desc = match Observer.value_exn after_obs with
    | Before -> "Before"
    | After -> "After"
  in
  Printf.printf "After advance to 4.0: %s, current time: %f\n" val_desc (Clock.now ());
  
  (* Should be After now *)
  (match Observer.value_exn after_obs with
   | After -> ()
   | Before -> fail "Expected After")

let test_at_intervals () =
  let module I = Incremental.Make () in
  let open I in
  Clock.create ~start:0. ();
  
  let base_time = 0. in
  let interval = 2. in
  
  stabilize ();
  
  (* Create an at_intervals node *)
  let interval_node = at_intervals ~base:base_time ~interval in
  let interval_obs = observe interval_node in
  
  (* Track when it fires *)
  let fired_times = ref [] in
  Observer.on_update_exn interval_obs ~f:(function
    | Initialized () -> ()
    | Changed ((), ()) -> 
        (* Just record that it fired - we'll check timing separately *)
        fired_times := !fired_times @ [1]
    | Invalidated -> ());
  
  stabilize ();
  
  (* Advance time and check firing *)
  Clock.advance_clock ~to_:1.9 ();
  stabilize ();
  check (list int) "no firing before interval" [] !fired_times;
  
  Clock.advance_clock ~to_:2.1 ();
  stabilize ();
  check (list int) "fired at first interval" [1] !fired_times;
  
  Clock.advance_clock ~to_:4.1 ();
  stabilize ();
  check (list int) "fired at second interval" [1; 1] !fired_times;
  
  Clock.advance_clock ~to_:7. ();
  stabilize ();
  check (list int) "fired at third interval" [1; 1; 1] !fired_times

let test_snapshot () =
  let module I = Incremental.Make () in
  let open I in
  Clock.create ~start:0. ();
  stabilize ();
  
  (* Create a changing value *)
  let var = Var.create 1 in
  let incr_value = Var.watch var in
  
  (* Observe incr_value to make it necessary *)
  let incr_obs = observe incr_value in
  
  (* Take a snapshot at time 5 with initial value 0 *)
  let snapshot_node = snapshot incr_value ~at:5. ~before:0 in
  let snapshot_obs = observe snapshot_node in
  stabilize ();
  
  (* Should have initial value before snapshot time *)
  int_eq 0 (Observer.value_exn snapshot_obs);
  
  (* Change the variable *)
  Printf.printf "Before Var.set: var value = %d\n" (Var.value var);
  Var.set var 2;
  Printf.printf "After Var.set: var value = %d\n" (Var.value var);
  stabilize ();
  
  (* Debug: check var value *)
  Printf.printf "After stabilize: incr value = %d\n" (Observer.value_exn incr_obs);
  
  (* Snapshot should still be 0 (hasn't fired yet) *)
  int_eq 0 (Observer.value_exn snapshot_obs);
  
  (* Advance to snapshot time *)
  Clock.advance_clock ~to_:5. ();
  stabilize ();
  
  (* Debug: print actual value *)
  Printf.printf "Snapshot value after firing: %d\n" (Observer.value_exn snapshot_obs);
  
  (* Now it should have the snapshot value *)
  int_eq 2 (Observer.value_exn snapshot_obs);
  
  (* Change the variable again *)
  Var.set var 3;
  stabilize ();
  
  (* Snapshot should be frozen at 2 *)
  int_eq 2 (Observer.value_exn snapshot_obs)

let test_snapshot_past () =
  let module I = Incremental.Make () in
  let open I in
  Clock.create ~start:0. ();
  (* Advance to time 10 to test snapshot in the past *)
  Clock.advance_clock ~to_:10. ();
  stabilize ();
  
  (* Try to take a snapshot in the past - should fail *)
  let var = Var.create 1 in
  let incr_value = Var.watch var in
  
  (try
    let _ = snapshot incr_value ~at:5. ~before:0 in
    fail "Expected snapshot in past to fail"
  with Failure msg ->
    check string "error message" "cannot take snapshot in the past" msg)

let test_step_function () =
  let module I = Incremental.Make () in
  let open I in
  Clock.create ~start:0. ();
  stabilize ();
  
  (* Create a step function with multiple steps *)
  let steps = [
    (2., 10);
    (5., 20);
    (8., 30)
  ] in
  let step_node = step_function ~init:0 steps in
  let step_obs = observe step_node in
  stabilize ();
  
  (* Should have initial value *)
  int_eq 0 (Observer.value_exn step_obs);
  
  (* Advance to before first step *)
  Clock.advance_clock ~to_:1. ();
  stabilize ();
  int_eq 0 (Observer.value_exn step_obs);
  
  (* Advance past first step *)
  Clock.advance_clock ~to_:2.5 ();
  stabilize ();
  int_eq 10 (Observer.value_exn step_obs);
  
  (* Advance past second step *)
  Clock.advance_clock ~to_:5.5 ();
  stabilize ();
  int_eq 20 (Observer.value_exn step_obs);
  
  (* Advance past all steps *)
  Clock.advance_clock ~to_:10. ();
  stabilize ();
  int_eq 30 (Observer.value_exn step_obs)

let test_step_function_multiple_at_once () =
  let module I = Incremental.Make () in
  let open I in
  Clock.create ~start:0. ();
  stabilize ();
  
  (* Create a step function with multiple steps *)
  let steps = [
    (2., "a");
    (5., "b");
    (8., "c")
  ] in
  let step_node = step_function ~init:"init" steps in
  let step_obs = observe step_node in
  stabilize ();
  
  (* Should have initial value *)
  check string "initial value" "init" (Observer.value_exn step_obs);
  
  (* Advance past multiple steps at once *)
  Clock.advance_clock ~to_:6. ();
  stabilize ();
  check string "after multiple steps" "b" (Observer.value_exn step_obs)

let test_combined_time_nodes () =
  let module I = Incremental.Make () in
  let open I in
  Clock.create ~start:0. ();
  stabilize ();
  
  (* Create various time-based nodes *)
  let at_node = at 5. in
  let interval_node = at_intervals ~base:0. ~interval:3. in
  
  (* Combine them *)
  let combined = map2 at_node interval_node ~f:(fun at_val _ ->
    match at_val with
    | Before -> "waiting"
    | After -> "triggered"
  ) in
  let combined_obs = observe combined in
  stabilize ();
  
  (* Initially waiting *)
  check string "initially" "waiting" (Observer.value_exn combined_obs);
  
  (* After at fires *)
  Clock.advance_clock ~to_:6. ();
  stabilize ();
  check string "after trigger" "triggered" (Observer.value_exn combined_obs)

let test_advance_clock_backwards () =
  let module I = Incremental.Make () in
  let open I in
  Clock.create ~start:0. ();
  (* Advance to time 10 first *)
  Clock.advance_clock ~to_:10. ();
  stabilize ();
  
  (* Try to advance backwards - should be a no-op *)
  Clock.advance_clock ~to_:5. ();
  stabilize ();
  
  (* Create an at node to verify time hasn't changed *)
  let at_test = at 11. in
  let at_obs = observe at_test in
  stabilize ();
  (match Observer.value_exn at_obs with
   | Before -> () (* Good, we're still before 11 *)
   | After -> fail "Time moved forward unexpectedly")

let test_at_intervals_small_interval () =
  let module I = Incremental.Make () in
  let open I in
  Clock.create ~start:0. ();
  stabilize ();
  
  (* Try to create at_intervals with too small interval *)
  (try
    let _ = at_intervals ~base:0. ~interval:0.0001 in
    fail "Expected too small interval to fail"
  with Failure msg ->
    check string "error message" "at_intervals got too small interval" msg)

let test_multiple_clocks_interaction () =
  let module I = Incremental.Make () in
  let open I in
  Clock.create ~start:0. ();
  stabilize ();
  
  (* Create multiple time-based nodes *)
  let at1 = at 2. in
  let at2 = at 4. in
  let at3 = at 6. in
  
  (* Map over all of them *)
  let all_triggered = map3 at1 at2 at3 ~f:(fun a1 a2 a3 ->
    match a1, a2, a3 with
    | After, After, After -> true
    | _ -> false
  ) in
  let obs = observe all_triggered in
  stabilize ();
  
  (* None triggered initially *)
  bool_eq false (Observer.value_exn obs);
  
  (* Advance past first *)
  Clock.advance_clock ~to_:3. ();
  stabilize ();
  bool_eq false (Observer.value_exn obs);
  
  (* Advance past second *)
  Clock.advance_clock ~to_:5. ();
  stabilize ();
  bool_eq false (Observer.value_exn obs);
  
  (* Advance past all *)
  Clock.advance_clock ~to_:7. ();
  stabilize ();
  bool_eq true (Observer.value_exn obs)

let test_snapshot_with_changing_value () =
  let module I = Incremental.Make () in
  let open I in
  Clock.create ~start:0. ();
  stabilize ();
  
  (* Create a computed value that changes over time *)
  let time_var = Var.create 0. in
  let computed = map (Var.watch time_var) ~f:(fun t -> int_of_float (t *. 10.)) in
  
  (* Take snapshots at different times *)
  let snap1 = snapshot computed ~at:2. ~before:(-1) in
  let snap2 = snapshot computed ~at:4. ~before:(-2) in
  
  let obs1 = observe snap1 in
  let obs2 = observe snap2 in
  stabilize ();
  
  (* Initial values *)
  int_eq (-1) (Observer.value_exn obs1);
  int_eq (-2) (Observer.value_exn obs2);
  
  (* Update time var and advance *)
  Var.set time_var 2.;
  Clock.advance_clock ~to_:2. ();
  stabilize ();
  
  (* First snapshot should have changed *)
  int_eq 20 (Observer.value_exn obs1); (* Frozen at computed value when alarm fired *)
  int_eq (-2) (Observer.value_exn obs2); (* Not yet triggered *)
  
  (* Update time var and advance more *)
  Var.set time_var 4.;
  Clock.advance_clock ~to_:4. ();
  stabilize ();
  
  (* Both should be frozen at their snapshot values *)
  int_eq 20 (Observer.value_exn obs1);  (* Frozen at value when time was 2 *)
  int_eq 40 (Observer.value_exn obs2)   (* Frozen at value when time was 4 *)

let () =
  run "Time-based Incremental" [
    "at", [
      test_case "basic at node" `Quick test_at;
    ];
    "after", [
      test_case "basic after node" `Quick test_after;
    ];
    "at_intervals", [
      test_case "basic at_intervals" `Quick test_at_intervals;
      test_case "small interval error" `Quick test_at_intervals_small_interval;
    ];
    "snapshot", [
      test_case "basic snapshot" `Quick test_snapshot;
      test_case "snapshot in past" `Quick test_snapshot_past;
      test_case "snapshot with changing value" `Quick test_snapshot_with_changing_value;
    ];
    "step_function", [
      test_case "basic step function" `Quick test_step_function;
      test_case "multiple steps at once" `Quick test_step_function_multiple_at_once;
    ];
    "advance_clock", [
      test_case "backward advance" `Quick test_advance_clock_backwards;
    ];
    "combined", [
      test_case "combined time nodes" `Quick test_combined_time_nodes;
      test_case "multiple clocks interaction" `Quick test_multiple_clocks_interaction;
    ];
  ]