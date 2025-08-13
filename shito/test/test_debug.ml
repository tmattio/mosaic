module S = Shito.Make ()

let () =
  Printf.printf "\n=== Simple test ===\n";
  let x = S.Var.create 10 in
  let xw = S.Var.watch x in
  
  let mapped = S.map xw ~f:(fun v ->
    Printf.printf "Map recomputing with v = %d\n" v;
    v
  ) in
  
  let observer = S.observe mapped in
  
  Printf.printf "\n=== First stabilization ===\n";
  Printf.printf "Is stabilizing before: %b\n" (S.is_stabilizing ());
  S.stabilize ();
  Printf.printf "Is stabilizing after: %b\n" (S.is_stabilizing ());
  Printf.printf "After first stabilize: value = %d\n" (S.Observer.value_exn observer);
  
  Printf.printf "\n=== Setting var to 20 ===\n";
  S.Var.set x 20;
  Printf.printf "Var.value x = %d\n" (S.Var.value x);
  
  Printf.printf "\n=== Second stabilization ===\n";
  S.stabilize ();
  Printf.printf "After second stabilize: value = %d\n" (S.Observer.value_exn observer);
  Printf.printf "Expected: 20, Got: %d\n" (S.Observer.value_exn observer)