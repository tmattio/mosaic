module S = Shito.Make ()

let () =
  Printf.printf "\n=== Minimal bind test ===\n";
  
  let x = S.Var.create 0 in
  let first_call = ref true in
  
  let node = S.bind (S.Var.watch x) ~f:(fun v ->
    Printf.printf "Bind function called with v=%d, first_call=%b\n" v !first_call;
    if !first_call then begin
      first_call := false;
      S.const v
    end else
      S.const (v + 100)
  ) in
  
  let obs = S.observe node in
  
  Printf.printf "\n=== First stabilize ===\n";
  S.stabilize ();
  Printf.printf "Value after first stabilize: %d\n" (S.Observer.value_exn obs);
  
  Printf.printf "\n=== Setting x to 1 ===\n";
  S.Var.set x 1;
  
  Printf.printf "\n=== Second stabilize ===\n";
  S.stabilize ();
  
  Printf.printf "\n=== Trying to get value ===\n";
  try
    let value = S.Observer.value_exn obs in
    Printf.printf "Value after second stabilize: %d (expected 101)\n" value
  with
  | _ -> Printf.printf "ERROR: Observer has no value!\n"