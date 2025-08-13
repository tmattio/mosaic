(** Test that our implementation matches incremental's semantics exactly *)

module S = Shito.Make ()

let assert_equal msg expected actual =
  if expected <> actual then
    Printf.printf "FAIL: %s - expected %d, got %d\n" msg expected actual
  else
    Printf.printf "PASS: %s\n" msg

let _assert_raises msg f =
  try
    ignore (f ());
    Printf.printf "FAIL: %s - expected exception\n" msg
  with
  | _ -> Printf.printf "PASS: %s - raised exception as expected\n" msg

(** Test 1: Basic bind behavior with changing RHS *)
let test_bind_rhs_change () =
  Printf.printf "\n=== Test 1: Bind RHS change ===\n";
  let x = S.Var.create 0 in
  let call_count = ref 0 in
  let node = S.bind (S.Var.watch x) ~f:(fun v ->
    incr call_count;
    if v = 0 then S.const 100 else S.const 200
  ) in
  let obs = S.observe node in
  S.stabilize ();
  assert_equal "Initial value" 100 (S.Observer.value_exn obs);
  assert_equal "Bind called once" 1 !call_count;
  
  S.Var.set x 1;
  S.stabilize ();
  assert_equal "Changed value" 200 (S.Observer.value_exn obs);
  assert_equal "Bind called twice" 2 !call_count

(** Test 2: Nested binds with scope management *)
let test_nested_binds () =
  Printf.printf "\n=== Test 2: Nested binds ===\n";
  let x = S.Var.create true in
  let y = S.Var.create 10 in
  let z = S.Var.create 20 in
  
  let outer = S.bind (S.Var.watch x) ~f:(fun b ->
    if b then
      S.bind (S.Var.watch y) ~f:(fun v -> S.const (v * 2))
    else
      S.Var.watch z
  ) in
  
  let obs = S.observe outer in
  S.stabilize ();
  assert_equal "Initial nested bind" 20 (S.Observer.value_exn obs);
  
  S.Var.set y 15;
  S.stabilize ();
  assert_equal "Inner bind updated" 30 (S.Observer.value_exn obs);
  
  S.Var.set x false;
  S.stabilize ();
  assert_equal "Switched to z" 20 (S.Observer.value_exn obs);
  
  S.Var.set y 100;
  S.stabilize ();
  assert_equal "y change doesn't affect (disconnected)" 20 (S.Observer.value_exn obs);
  
  S.Var.set z 50;
  S.stabilize ();
  assert_equal "z change affects" 50 (S.Observer.value_exn obs)

(** Test 3: Bind creating nodes that should be invalidated *)
let test_bind_invalidation () =
  Printf.printf "\n=== Test 3: Bind invalidation ===\n";
  let x = S.Var.create 0 in
  let y = S.Var.create 100 in
  
  let node = S.bind (S.Var.watch x) ~f:(fun v ->
    if v = 0 then
      S.map (S.Var.watch y) ~f:(fun y_val -> y_val + 1)
    else
      S.const 999
  ) in
  
  let obs = S.observe node in
  S.stabilize ();
  assert_equal "Initial with map" 101 (S.Observer.value_exn obs);
  
  S.Var.set x 1;
  S.stabilize ();
  assert_equal "Switched to const" 999 (S.Observer.value_exn obs);
  
  (* The map node should be invalidated and y changes shouldn't affect the result *)
  S.Var.set y 200;
  S.stabilize ();
  assert_equal "y change doesn't affect (map invalidated)" 999 (S.Observer.value_exn obs)

(** Test 4: Cutoff behavior *)
let test_cutoff_semantics () =
  Printf.printf "\n=== Test 4: Cutoff semantics ===\n";
  let x = S.Var.create 10 in
  let compute_count = ref 0 in
  
  let intermediate = S.map (S.Var.watch x) ~f:(fun v ->
    incr compute_count;
    v / 10 * 10  (* Round down to nearest 10 *)
  ) in
  
  let downstream_count = ref 0 in
  let downstream = S.map intermediate ~f:(fun v ->
    incr downstream_count;
    v * 2
  ) in
  
  let obs = S.observe downstream in
  S.stabilize ();
  assert_equal "Initial value" 20 (S.Observer.value_exn obs);
  assert_equal "Initial compute" 1 !compute_count;
  assert_equal "Initial downstream" 1 !downstream_count;
  
  (* Change that results in same intermediate value *)
  S.Var.set x 13;
  S.stabilize ();
  assert_equal "Value unchanged" 20 (S.Observer.value_exn obs);
  assert_equal "Intermediate recomputed" 2 !compute_count;
  assert_equal "Downstream not recomputed (cutoff)" 1 !downstream_count;
  
  (* Change that results in different intermediate value *)
  S.Var.set x 25;
  S.stabilize ();
  assert_equal "Value changed" 40 (S.Observer.value_exn obs);
  assert_equal "Intermediate recomputed again" 3 !compute_count;
  assert_equal "Downstream recomputed" 2 !downstream_count

(** Test 5: If-then-else behavior *)
let test_if_then_else () =
  Printf.printf "\n=== Test 5: If-then-else ===\n";
  let cond = S.Var.create true in
  let x = S.Var.create 10 in
  let y = S.Var.create 20 in
  
  let result = S.if_ (S.Var.watch cond) 
    ~then_:(S.Var.watch x)
    ~else_:(S.Var.watch y) in
  
  let obs = S.observe result in
  S.stabilize ();
  assert_equal "Initially then branch" 10 (S.Observer.value_exn obs);
  
  S.Var.set x 15;
  S.stabilize ();
  assert_equal "Then branch updated" 15 (S.Observer.value_exn obs);
  
  S.Var.set cond false;
  S.stabilize ();
  assert_equal "Switched to else branch" 20 (S.Observer.value_exn obs);
  
  S.Var.set x 100;
  S.stabilize ();
  assert_equal "Then branch change ignored" 20 (S.Observer.value_exn obs);
  
  S.Var.set y 30;
  S.stabilize ();
  assert_equal "Else branch updated" 30 (S.Observer.value_exn obs)

(** Test 6: Join behavior *)
let test_join () =
  Printf.printf "\n=== Test 6: Join ===\n";
  let x = S.Var.create (S.const 5) in
  let y = S.const 10 in
  
  let outer = S.join (S.Var.watch x) in
  let obs = S.observe outer in
  S.stabilize ();
  assert_equal "Initially inner const" 5 (S.Observer.value_exn obs);
  
  S.Var.set x y;
  S.stabilize ();
  assert_equal "Switched to different const" 10 (S.Observer.value_exn obs)

(** Test 7: Freeze behavior *)
let test_freeze () =
  Printf.printf "\n=== Test 7: Freeze ===\n";
  let x = S.Var.create 10 in
  let frozen = S.freeze (S.Var.watch x) ~when_:(fun v -> v >= 20) in
  
  let obs = S.observe frozen in
  S.stabilize ();
  assert_equal "Initially not frozen" 10 (S.Observer.value_exn obs);
  
  S.Var.set x 15;
  S.stabilize ();
  assert_equal "Still not frozen" 15 (S.Observer.value_exn obs);
  
  S.Var.set x 20;
  S.stabilize ();
  assert_equal "Now frozen at 20" 20 (S.Observer.value_exn obs);
  
  S.Var.set x 30;
  S.stabilize ();
  assert_equal "Remains frozen at 20" 20 (S.Observer.value_exn obs)

(** Test 8: Stabilization numbers and staleness *)
let test_stabilization_numbers () =
  Printf.printf "\n=== Test 8: Stabilization numbers ===\n";
  let x = S.Var.create 1 in
  let y = S.Var.create 2 in
  
  let sum = S.map2 (S.Var.watch x) (S.Var.watch y) ~f:(+) in
  let doubled = S.map sum ~f:(fun v -> v * 2) in
  
  let obs = S.observe doubled in
  S.stabilize ();
  assert_equal "Initial" 6 (S.Observer.value_exn obs);
  
  (* Change both inputs in same stabilization *)
  S.Var.set x 3;
  S.Var.set y 4;
  S.stabilize ();
  assert_equal "Both changed" 14 (S.Observer.value_exn obs)

(** Test 9: Observer behavior *)
let test_observer_updates () =
  Printf.printf "\n=== Test 9: Observer updates ===\n";
  let x = S.Var.create 10 in
  let obs = S.observe (S.Var.watch x) in
  
  let updates = ref [] in
  S.stabilize ();
  
  S.Observer.on_update_exn obs ~f:(fun update ->
    updates := update :: !updates
  );
  
  S.Var.set x 20;
  S.stabilize ();
  
  match !updates with
  | [S.Observer.Update.Changed (old, new_)] ->
      assert_equal "Old value in update" 10 old;
      assert_equal "New value in update" 20 new_
  | _ -> Printf.printf "FAIL: Expected Changed update\n"

(** Test 10: Complex DAG with multiple paths *)
let test_complex_dag () =
  Printf.printf "\n=== Test 10: Complex DAG ===\n";
  let x = S.Var.create 2 in
  
  (* Multiple paths from x to result *)
  let doubled = S.map (S.Var.watch x) ~f:(fun v -> v * 2) in
  let tripled = S.map (S.Var.watch x) ~f:(fun v -> v * 3) in
  let sum = S.map2 doubled tripled ~f:(+) in
  
  let obs = S.observe sum in
  S.stabilize ();
  assert_equal "Initial (2*2 + 2*3)" 10 (S.Observer.value_exn obs);
  
  S.Var.set x 3;
  S.stabilize ();
  assert_equal "Updated (3*2 + 3*3)" 15 (S.Observer.value_exn obs)

let () =
  Printf.printf "Testing that Shito matches Incremental's semantics...\n";
  test_bind_rhs_change ();
  test_nested_binds ();
  test_bind_invalidation ();
  test_cutoff_semantics ();
  test_if_then_else ();
  test_join ();
  test_freeze ();
  test_stabilization_numbers ();
  test_observer_updates ();
  test_complex_dag ();
  Printf.printf "\n=== All semantic tests completed ===\n"