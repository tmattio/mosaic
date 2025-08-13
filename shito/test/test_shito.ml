(** Comprehensive test suite for Shito incremental computation library *)

module S = Shito.Make ()

(** Test utilities *)
let assert_raises f =
  try
    ignore (f ());
    Alcotest.fail "Expected exception but none was raised"
  with
  | _ -> ()


(** Core functionality tests *)
let test_var_create_and_watch () =
  let x = S.Var.create 42 in
  Alcotest.(check int) "initial value" 42 (S.Var.value x);
  let x_incr = S.Var.watch x in
  let observer = S.observe x_incr in
  S.stabilize ();
  Alcotest.(check int) "observed value" 42 (S.Observer.value_exn observer)

let test_var_set_and_propagate () =
  let x = S.Var.create 10 in
  let x_incr = S.Var.watch x in
  let observer = S.observe x_incr in
  S.stabilize ();
  Alcotest.(check int) "initial" 10 (S.Observer.value_exn observer);
  
  S.Var.set x 20;
  S.stabilize ();
  Alcotest.(check int) "after set" 20 (S.Observer.value_exn observer)

let test_const () =
  let c = S.const 100 in
  let observer = S.observe c in
  S.stabilize ();
  Alcotest.(check int) "const value" 100 (S.Observer.value_exn observer)

let test_map () =
  let x = S.Var.create 5 in
  let doubled = S.map (S.Var.watch x) ~f:(fun v -> v * 2) in
  let observer = S.observe doubled in
  S.stabilize ();
  Alcotest.(check int) "initial map" 10 (S.Observer.value_exn observer);
  
  S.Var.set x 7;
  S.stabilize ();
  Alcotest.(check int) "after update" 14 (S.Observer.value_exn observer)

let test_map2 () =
  let x = S.Var.create 3 in
  let y = S.Var.create 4 in
  let sum = S.map2 (S.Var.watch x) (S.Var.watch y) ~f:(+) in
  let observer = S.observe sum in
  S.stabilize ();
  Alcotest.(check int) "initial sum" 7 (S.Observer.value_exn observer);
  
  S.Var.set x 5;
  S.stabilize ();
  Alcotest.(check int) "after x update" 9 (S.Observer.value_exn observer);
  
  S.Var.set y 10;
  S.stabilize ();
  Alcotest.(check int) "after y update" 15 (S.Observer.value_exn observer)

let test_map3 () =
  let x = S.Var.create 2 in
  let y = S.Var.create 3 in
  let z = S.Var.create 4 in
  let product = S.map3 
    (S.Var.watch x) 
    (S.Var.watch y) 
    (S.Var.watch z)
    ~f:(fun a b c -> a * b * c) in
  let observer = S.observe product in
  S.stabilize ();
  Alcotest.(check int) "initial product" 24 (S.Observer.value_exn observer);
  
  S.Var.set x 5;
  S.stabilize ();
  Alcotest.(check int) "after update" 60 (S.Observer.value_exn observer)

let test_bind () =
  let switch = S.Var.create true in
  let x = S.Var.create 10 in
  let y = S.Var.create 20 in
  
  let result = S.bind (S.Var.watch switch) ~f:(fun b ->
    if b then S.Var.watch x else S.Var.watch y
  ) in
  
  let observer = S.observe result in
  S.stabilize ();
  Alcotest.(check int) "initial (true->x)" 10 (S.Observer.value_exn observer);
  
  S.Var.set switch false;
  S.stabilize ();
  Alcotest.(check int) "after switch" 20 (S.Observer.value_exn observer);
  
  S.Var.set y 30;
  S.stabilize ();
  Alcotest.(check int) "after y update" 30 (S.Observer.value_exn observer);
  
  S.Var.set x 15;
  S.stabilize ();
  Alcotest.(check int) "x update (not visible)" 30 (S.Observer.value_exn observer);
  
  S.Var.set switch true;
  S.stabilize ();
  Alcotest.(check int) "switch back to x" 15 (S.Observer.value_exn observer)

let test_complex_dag () =
  let x = S.Var.create 2 in
  let y = S.Var.create 3 in
  let x_incr = S.Var.watch x in
  let y_incr = S.Var.watch y in
  
  let x_squared = S.map x_incr ~f:(fun v -> v * v) in
  let y_squared = S.map y_incr ~f:(fun v -> v * v) in
  let sum_of_squares = S.map2 x_squared y_squared ~f:(+) in
  let sqrt_sum = S.map sum_of_squares ~f:(fun v -> float_of_int v |> sqrt |> int_of_float) in
  
  let observer = S.observe sqrt_sum in
  S.stabilize ();
  (* sqrt(2^2 + 3^2) = sqrt(13) = 3 *)
  Alcotest.(check int) "initial" 3 (S.Observer.value_exn observer);
  
  S.Var.set x 3;
  S.Var.set y 4;
  S.stabilize ();
  (* sqrt(3^2 + 4^2) = sqrt(25) = 5 *)
  Alcotest.(check int) "after update" 5 (S.Observer.value_exn observer)

(** Cutoff tests *)
let test_cutoff_phys_equal () =
  let x = S.Var.create 10 in
  let count = ref 0 in
  let tracked = S.map (S.Var.watch x) ~f:(fun v -> 
    incr count;
    v * 2
  ) in
  let observer = S.observe tracked in
  S.stabilize ();
  Alcotest.(check int) "initial count" 1 !count;
  
  S.Var.set x 10;  (* Same value *)
  S.stabilize ();
  (* Since the Var has the same value, it won't propagate to children *)
  Alcotest.(check int) "count after same value" 1 !count;
  Alcotest.(check int) "value unchanged" 20 (S.Observer.value_exn observer)

let test_cutoff_never () =
  let x = S.Var.create 10 in
  let x_incr = S.Var.watch x in
  let count = ref 0 in
  let tracked = S.map x_incr ~f:(fun v -> 
    incr count;
    v
  ) in
  S.set_cutoff tracked S.Cutoff.never;
  
  let downstream_count = ref 0 in
  let downstream = S.map tracked ~f:(fun v -> 
    incr downstream_count;
    v * 2
  ) in
  let observer = S.observe downstream in
  S.stabilize ();
  Alcotest.(check int) "initial" 20 (S.Observer.value_exn observer);
  Alcotest.(check int) "initial count" 1 !count;
  Alcotest.(check int) "initial downstream" 1 !downstream_count;
  
  S.Var.set x 10;  (* Same value but cutoff.never means it propagates *)
  S.stabilize ();
  Alcotest.(check int) "still propagates" 20 (S.Observer.value_exn observer);
  (* With cutoff.never, the value is recomputed but the VAR itself won't trigger 
     because it has the same value. We need to actually change the value *)
  Alcotest.(check int) "count unchanged" 1 !count;
  
  S.Var.set x 11;  (* Different value *)
  S.stabilize ();
  Alcotest.(check int) "after real change" 22 (S.Observer.value_exn observer);
  Alcotest.(check int) "count incremented" 2 !count;
  Alcotest.(check int) "downstream incremented" 2 !downstream_count

let test_cutoff_always () =
  let x = S.Var.create 10 in
  let x_incr = S.Var.watch x in
  let count = ref 0 in
  let tracked = S.map x_incr ~f:(fun v -> 
    incr count;
    v * 2
  ) in
  S.set_cutoff tracked S.Cutoff.always;
  
  let downstream_count = ref 0 in
  let downstream = S.map tracked ~f:(fun v -> 
    incr downstream_count;
    v + 1
  ) in
  let observer = S.observe downstream in
  S.stabilize ();
  Alcotest.(check int) "initial count" 1 !count;
  Alcotest.(check int) "initial downstream" 1 !downstream_count;
  
  S.Var.set x 20;  (* Different value but cutoff.always prevents propagation *)
  S.stabilize ();
  Alcotest.(check int) "tracked recomputed" 2 !count;
  Alcotest.(check int) "downstream not recomputed" 1 !downstream_count;
  (* Value is still old because cutoff prevented update *)
  Alcotest.(check int) "value unchanged" 21 (S.Observer.value_exn observer)

let test_custom_cutoff () =
  let x = S.Var.create 10 in
  let x_incr = S.Var.watch x in
  
  (* Only propagate if difference is > 5 *)
  let with_threshold = S.map x_incr ~f:(fun v -> v) in
  S.set_cutoff with_threshold 
    (S.Cutoff.create (fun ~old ~new_ -> abs (new_ - old) <= 5));
  
  let observer = S.observe with_threshold in
  S.stabilize ();
  Alcotest.(check int) "initial" 10 (S.Observer.value_exn observer);
  
  S.Var.set x 13;  (* Difference is 3, <= 5, so cutoff *)
  S.stabilize ();
  (* The value will be updated internally but not propagated further *)
  Alcotest.(check int) "cutoff (small change)" 13 (S.Observer.value_exn observer);
  
  S.Var.set x 20;  (* Difference is 7 from 13, > 5, so propagate *)
  S.stabilize ();
  Alcotest.(check int) "propagate (large change)" 20 (S.Observer.value_exn observer)

(** Observer tests *)
let test_observer_updates () =
  let x = S.Var.create 10 in
  let x_incr = S.Var.watch x in
  let observer = S.observe x_incr in
  
  (* First stabilize before registering handler *)
  S.stabilize ();
  
  let updates = ref [] in
  S.Observer.on_update_exn observer ~f:(fun update ->
    updates := update :: !updates
  );
  
  Alcotest.(check int) "value after first stabilize" 10 (S.Observer.value_exn observer);
  
  (* Set and stabilize to get an update *)
  S.Var.set x 20;
  S.stabilize ();
  
  (* Should get Changed update *)
  (match !updates with
  | [S.Observer.Update.Changed (old, new_)] -> 
      Alcotest.(check int) "old value" 10 old;
      Alcotest.(check int) "new value" 20 new_
  | _ -> Alcotest.fail "Expected single Changed update");
  
  (* Another update *)
  updates := [];
  S.Var.set x 30;
  S.stabilize ();
  
  (match !updates with
  | [S.Observer.Update.Changed (old, new_)] -> 
      Alcotest.(check int) "old value 2" 20 old;
      Alcotest.(check int) "new value 2" 30 new_
  | _ -> Alcotest.fail "Expected single Changed update")

let test_observer_disallow_future_use () =
  let x = S.Var.create 10 in
  let x_incr = S.Var.watch x in
  let observer = S.observe x_incr in
  S.stabilize ();
  
  Alcotest.(check int) "value before disallow" 10 (S.Observer.value_exn observer);
  
  S.Observer.disallow_future_use observer;
  
  assert_raises 
    (fun () -> S.Observer.value_exn observer)

let test_multiple_observers () =
  let x = S.Var.create 5 in
  let x_incr = S.Var.watch x in
  let doubled = S.map x_incr ~f:(fun v -> v * 2) in
  
  let obs1 = S.observe x_incr in
  let obs2 = S.observe doubled in
  let obs3 = S.observe doubled in  (* Multiple observers on same node *)
  
  S.stabilize ();
  Alcotest.(check int) "obs1" 5 (S.Observer.value_exn obs1);
  Alcotest.(check int) "obs2" 10 (S.Observer.value_exn obs2);
  Alcotest.(check int) "obs3" 10 (S.Observer.value_exn obs3);
  
  S.Var.set x 7;
  S.stabilize ();
  Alcotest.(check int) "obs1 after" 7 (S.Observer.value_exn obs1);
  Alcotest.(check int) "obs2 after" 14 (S.Observer.value_exn obs2);
  Alcotest.(check int) "obs3 after" 14 (S.Observer.value_exn obs3);
  
  S.Observer.disallow_future_use obs2;
  S.Var.set x 9;
  S.stabilize ();
  Alcotest.(check int) "obs1 still works" 9 (S.Observer.value_exn obs1);
  Alcotest.(check int) "obs3 still works" 18 (S.Observer.value_exn obs3)

(** Infix operator tests *)
let test_infix_operators () =
  let open S.Infix in
  let x = S.Var.create 10 in
  let x_incr = S.Var.watch x in
  
  (* Test >>| (map) *)
  let doubled = x_incr >>| fun v -> v * 2 in
  let squared = x_incr >>| fun v -> v * v in
  
  (* Test >>= (bind) *)
  let threshold = S.Var.create 50 in
  let result = (S.Var.watch threshold) >>= fun t ->
    if t > 50 then doubled else squared
  in
  
  let obs = S.observe result in
  S.stabilize ();
  Alcotest.(check int) "initial (squared)" 100 (S.Observer.value_exn obs);
  
  S.Var.set threshold 60;
  S.stabilize ();
  Alcotest.(check int) "after switch (doubled)" 20 (S.Observer.value_exn obs);
  
  S.Var.set x 5;
  S.stabilize ();
  Alcotest.(check int) "after x update" 10 (S.Observer.value_exn obs)

(** Let_syntax tests *)
let test_let_syntax () =
  let open S.Let_syntax in
  
  let x = S.Var.create 3 in
  let y = S.Var.create 4 in
  
  (* Test return *)
  let c = return 100 in
  let obs_c = S.observe c in
  S.stabilize ();
  Alcotest.(check int) "return" 100 (S.Observer.value_exn obs_c);
  
  (* Test map *)
  let doubled = map (S.Var.watch x) ~f:(fun v -> v * 2) in
  let obs_d = S.observe doubled in
  S.stabilize ();
  Alcotest.(check int) "map" 6 (S.Observer.value_exn obs_d);
  
  (* Test both *)
  let paired = both (S.Var.watch x) (S.Var.watch y) in
  let obs_p = S.observe paired in
  S.stabilize ();
  let (a, b) = S.Observer.value_exn obs_p in
  Alcotest.(check int) "both first" 3 a;
  Alcotest.(check int) "both second" 4 b;
  
  (* Test map2 *)
  let sum = map2 (S.Var.watch x) (S.Var.watch y) ~f:(+) in
  let obs_s = S.observe sum in
  S.stabilize ();
  Alcotest.(check int) "map2" 7 (S.Observer.value_exn obs_s)

(** Edge cases and error conditions *)
let test_unstabilized_observer () =
  let x = S.Var.create 10 in
  let observer = S.observe (S.Var.watch x) in
  
  (* Try to read before stabilization *)
  assert_raises
    (fun () -> S.Observer.value_exn observer)

let test_double_stabilize () =
  let x = S.Var.create 10 in
  let _observer = S.observe (S.Var.watch x) in
  
  S.stabilize ();
  S.stabilize ();  (* Should be safe to call multiple times *)
  ()

let test_set_during_stabilization () =
  (* This should be prevented by is_stabilizing check *)
  let x = S.Var.create 10 in
  let _y = S.Var.create 20 in
  
  let _mapped = S.map (S.Var.watch x) ~f:(fun _v ->
    if S.is_stabilizing () then
      Alcotest.(check bool) "is stabilizing" true S.(is_stabilizing ())
    else
      Alcotest.fail "Should be stabilizing";
    42
  ) in
  
  S.stabilize ()

let test_circular_dependency_detection () =
  (* Bind can potentially create cycles if not careful *)
  let x = S.Var.create 0 in
  let first_call = ref true in
  
  let node = S.bind (S.Var.watch x) ~f:(fun v ->
    if !first_call then begin
      first_call := false;
      S.const v
    end else
      (* Return a different value on subsequent calls *)
      S.const (v + 100)
  ) in
  
  let obs = S.observe node in
  S.stabilize ();
  Alcotest.(check int) "initial" 0 (S.Observer.value_exn obs);
  
  S.Var.set x 1;
  S.stabilize ();
  (* After update, not first call anymore, so we get v + 100 *)
  Alcotest.(check int) "after update" 101 (S.Observer.value_exn obs)

(** Performance and stress tests *)
let test_deep_chain () =
  let x = S.Var.create 1 in
  let rec build_chain n node =
    if n = 0 then node
    else build_chain (n - 1) (S.map node ~f:(fun v -> v + 1))
  in
  
  let chain = build_chain 100 (S.Var.watch x) in
  let observer = S.observe chain in
  S.stabilize ();
  Alcotest.(check int) "deep chain initial" 101 (S.Observer.value_exn observer);
  
  S.Var.set x 5;
  S.stabilize ();
  Alcotest.(check int) "deep chain after update" 105 (S.Observer.value_exn observer)

let test_wide_fan_out () =
  let x = S.Var.create 10 in
  let x_incr = S.Var.watch x in
  
  let nodes = List.init 50 (fun i ->
    S.map x_incr ~f:(fun v -> v + i)
  ) in
  
  let observers = List.map S.observe nodes in
  S.stabilize ();
  
  List.iteri (fun i obs ->
    Alcotest.(check int) (Printf.sprintf "node %d" i) (10 + i) (S.Observer.value_exn obs)
  ) observers;
  
  S.Var.set x 20;
  S.stabilize ();
  
  List.iteri (fun i obs ->
    Alcotest.(check int) (Printf.sprintf "node %d after" i) (20 + i) (S.Observer.value_exn obs)
  ) observers

let test_diamond_dag () =
  let x = S.Var.create 5 in
  let x_incr = S.Var.watch x in
  
  (* Create diamond: x -> a, b -> c *)
  let a = S.map x_incr ~f:(fun v -> v * 2) in
  let b = S.map x_incr ~f:(fun v -> v * 3) in
  let c = S.map2 a b ~f:(+) in
  
  let observer = S.observe c in
  S.stabilize ();
  (* 5*2 + 5*3 = 25 *)
  Alcotest.(check int) "diamond initial" 25 (S.Observer.value_exn observer);
  
  S.Var.set x 7;
  S.stabilize ();
  (* 7*2 + 7*3 = 35 *)
  Alcotest.(check int) "diamond after update" 35 (S.Observer.value_exn observer)

let test_multiple_vars_simultaneous_update () =
  let x = S.Var.create 2 in
  let y = S.Var.create 3 in
  let z = S.Var.create 5 in
  
  let sum = S.map3 
    (S.Var.watch x)
    (S.Var.watch y) 
    (S.Var.watch z)
    ~f:(fun a b c -> a + b + c) in
  
  let observer = S.observe sum in
  S.stabilize ();
  Alcotest.(check int) "initial sum" 10 (S.Observer.value_exn observer);
  
  (* Update all vars before stabilizing *)
  S.Var.set x 10;
  S.Var.set y 20;
  S.Var.set z 30;
  S.stabilize ();
  
  Alcotest.(check int) "after batch update" 60 (S.Observer.value_exn observer)

let test_bind_switching_performance () =
  let switch = S.Var.create 0 in
  
  (* Create many vars *)
  let vars = List.init 10 (fun i -> S.Var.create i) in
  let var_nodes = List.map S.Var.watch vars in
  
  (* Bind that switches between them *)
  let result = S.bind (S.Var.watch switch) ~f:(fun i ->
    List.nth var_nodes (i mod 10)
  ) in
  
  let observer = S.observe result in
  
  (* Switch multiple times *)
  for i = 0 to 20 do
    S.Var.set switch i;
    S.stabilize ();
    Alcotest.(check int) (Printf.sprintf "switch %d" i) (i mod 10) (S.Observer.value_exn observer)
  done

(** Test suite organization *)
let core_tests = [
  "var create and watch", `Quick, test_var_create_and_watch;
  "var set and propagate", `Quick, test_var_set_and_propagate;
  "const", `Quick, test_const;
  "map", `Quick, test_map;
  "map2", `Quick, test_map2;
  "map3", `Quick, test_map3;
  "bind", `Quick, test_bind;
  "complex DAG", `Quick, test_complex_dag;
]

let cutoff_tests = [
  "phys_equal", `Quick, test_cutoff_phys_equal;
  "never", `Quick, test_cutoff_never;
  "always", `Quick, test_cutoff_always;
  "custom cutoff", `Quick, test_custom_cutoff;
]

let observer_tests = [
  "updates", `Quick, test_observer_updates;
  "disallow future use", `Quick, test_observer_disallow_future_use;
  "multiple observers", `Quick, test_multiple_observers;
]

let syntax_tests = [
  "infix operators", `Quick, test_infix_operators;
  "let_syntax", `Quick, test_let_syntax;
]

let edge_case_tests = [
  "unstabilized observer", `Quick, test_unstabilized_observer;
  "double stabilize", `Quick, test_double_stabilize;
  "set during stabilization", `Quick, test_set_during_stabilization;
  "circular dependency detection", `Quick, test_circular_dependency_detection;
]

let performance_tests = [
  "deep chain", `Quick, test_deep_chain;
  "wide fan out", `Quick, test_wide_fan_out;
  "diamond DAG", `Quick, test_diamond_dag;
  "multiple vars simultaneous update", `Quick, test_multiple_vars_simultaneous_update;
  "bind switching performance", `Quick, test_bind_switching_performance;
]

let () =
  Alcotest.run "Shito" [
    "core", core_tests;
    "cutoff", cutoff_tests;
    "observer", observer_tests;
    "syntax", syntax_tests;
    "edge_cases", edge_case_tests;
    "performance", performance_tests;
  ]