module S = Shito.Make ()

let () =
  Printf.printf "Creating var\n%!";
  let x = S.Var.create 10 in
  Printf.printf "Creating observer\n%!";
  let obs = S.observe (S.Var.watch x) in
  Printf.printf "About to stabilize\n%!";
  S.stabilize ();
  Printf.printf "After stabilize\n%!";
  Printf.printf "Value: %d\n" (S.Observer.value_exn obs)