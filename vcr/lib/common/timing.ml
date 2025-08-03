(** Generic timing module for performance tracking *)

type t = {
  mutable phases : (string * float) list;
  mutable active_phases : (string * float) list;
  start_time : float;
}

let create () =
  { phases = []; active_phases = []; start_time = Unix.gettimeofday () }

let start t name =
  let start_time = Unix.gettimeofday () in
  t.active_phases <- (name, start_time) :: t.active_phases

let stop t name =
  let end_time = Unix.gettimeofday () in
  match List.partition (fun (n, _) -> n = name) t.active_phases with
  | [ (_, start_time) ], others ->
      t.active_phases <- others;
      let duration = end_time -. start_time in
      t.phases <- (name, duration) :: t.phases;
      duration
  | [], _ ->
      failwith (Printf.sprintf "Timing.stop: phase '%s' was not started" name)
  | _, _ ->
      failwith
        (Printf.sprintf "Timing.stop: multiple active phases named '%s'" name)

let with_timing t name f =
  start t name;
  Fun.protect f ~finally:(fun () -> ignore (stop t name))

let record t name duration = t.phases <- (name, duration) :: t.phases
let get_total t = Unix.gettimeofday () -. t.start_time

let print ?(title = "TIMING SUMMARY") t =
  let total = get_total t in

  Printf.eprintf "\n[%s]\n" title;
  Printf.eprintf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n";

  (* Print in chronological order *)
  let phases = List.rev t.phases in

  (* Find the longest phase name for alignment *)
  let max_len =
    List.fold_left (fun acc (name, _) -> max acc (String.length name)) 20 phases
  in

  List.iter
    (fun (name, duration) ->
      let padding = String.make (max_len - String.length name) ' ' in
      let percentage =
        Printf.sprintf " (%4.1f%%)" (duration /. total *. 100.0)
      in
      Printf.eprintf "  %s%s: %7.3fs%s\n" name padding duration percentage)
    phases;

  (* Print total *)
  let total_padding = String.make (max_len - 5) ' ' in
  let separator = String.make (max_len - 5) '-' in
  Printf.eprintf "  -----%s---------------\n" separator;
  Printf.eprintf "  Total%s: %7.3fs\n" total_padding total;
  Printf.eprintf "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━\n"
