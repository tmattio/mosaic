open Incremental

let () =
  let module I = Incremental.Make () in
  let open I in
  Clock.create ~start:0. ();
  stabilize ();
  
  Printf.printf "Initial time: %f\n" (Clock.now ());
  
  (* Create an 'after' node for 3 seconds from now *)
  let after_node = after 3. in
  let after_obs = observe after_node in
  stabilize ();
  
  (* Check initial value *)
  (match Observer.value_exn after_obs with
   | Before t -> Printf.printf "Initial: Before %f\n" t
   | After t -> Printf.printf "Initial: After %f\n" t);
  
  (* Advance 4 seconds *)
  Clock.advance_clock ~to_:4. ();
  stabilize ();
  
  (* Check value after advance *)
  (match Observer.value_exn after_obs with
   | Before t -> Printf.printf "After advance: Before %f\n" t
   | After t -> Printf.printf "After advance: After %f\n" t)