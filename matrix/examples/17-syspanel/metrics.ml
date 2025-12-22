(** System metrics collection library.
    Stateless, poll-based design - caller manages state and sampling intervals. *)

exception Unavailable of string

(* ---------- C Bindings ---------- *)
external c_get_cpu_load : unit -> int64 * int64 * int64 * int64 = "caml_metrics_get_cpu_load"

(* ---------- Helper Functions ---------- *)
let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let starts_with ~prefix s =
  let lp = String.length prefix and ls = String.length s in
  ls >= lp && String.sub s 0 lp = prefix

(* ---------- CPU Module ---------- *)
module Cpu = struct
  type times = {
    user : int64;
    nice : int64;
    system : int64;
    idle : int64;
  }

  type stats = {
    user : float;
    system : float;
    idle : float;
  }

  (* Sample CPU counters (raw values, cumulative since boot) *)
  let sample () : times option =
    try
      if Sys.file_exists "/proc/stat" then (
        (* Linux: Read /proc/stat directly *)
        let ic = open_in "/proc/stat" in
        let line = try input_line ic with End_of_file -> "" in
        close_in ic;
        if String.length line < 4 || String.sub line 0 4 <> "cpu " then None
        else
          (* Parse: "cpu  1234 567 890 12345 67 89 0 0 0 0" *)
          (* Fields: user, nice, system, idle, iowait, irq, softirq, ... *)
          try
            let parts = String.split_on_char ' ' line in
            let parts = List.filter (fun s -> s <> "") parts in
            match parts with
            | _ :: user :: nice :: system :: idle :: iowait :: _ ->
                let user_val = Int64.of_string user in
                let nice_val = Int64.of_string nice in
                let system_val = Int64.of_string system in
                let idle_val = Int64.of_string idle in
                let iowait_val = Int64.of_string iowait in
                Some
                  {
                    user = Int64.add user_val nice_val;
                    nice = nice_val;
                    system = system_val;
                    idle = Int64.add idle_val iowait_val;
                  }
            | _ :: user :: nice :: system :: idle :: [] ->
                let user_val = Int64.of_string user in
                let nice_val = Int64.of_string nice in
                let system_val = Int64.of_string system in
                let idle_val = Int64.of_string idle in
                Some
                  {
                    user = Int64.add user_val nice_val;
                    nice = nice_val;
                    system = system_val;
                    idle = idle_val;
                  }
            | _ -> None
          with _ -> None)
      else
        (* macOS: Use host_statistics API via C bindings *)
        try
          let user_ticks, system_ticks, idle_ticks, nice_ticks = c_get_cpu_load () in
          Some
            {
              user = Int64.add user_ticks nice_ticks;
              nice = nice_ticks;
              system = system_ticks;
              idle = idle_ticks;
            }
        with _ -> None
    with _ -> None

  (* Calculate CPU usage percentage from two samples *)
  let usage ~(prev : times) ~(next : times) : stats option =
    let delta a b = Int64.sub b a in
    let du = delta prev.user next.user in
    let ds = delta prev.system next.system in
    let di = delta prev.idle next.idle in
    let dt = Int64.add (Int64.add du ds) di in
    if dt > 0L then (
      let dt_f = Int64.to_float dt in
      Some
        {
          user = (Int64.to_float du /. dt_f) *. 100.;
          system = (Int64.to_float ds /. dt_f) *. 100.;
          idle = (Int64.to_float di /. dt_f) *. 100.;
        })
    else None
end

(* ---------- Memory Module ---------- *)
module Mem = struct
  type t = {
    total_gb : float;
    used_gb : float;
    used_percent : float;
  }

  (* Sample memory statistics (instantaneous, no diffing needed) *)
  let sample () : t option =
    try
      if Sys.file_exists "/proc/meminfo" then (
        (* Linux: Read /proc/meminfo directly *)
        let ic = open_in "/proc/meminfo" in
        let mem_total = ref None
        and mem_available = ref None
        and mem_free = ref None in
        let read_kb_value (line : string) : int64 option =
          let len = String.length line in
          let rec find_digit i =
            if i >= len then None
            else
              match line.[i] with
              | '0' .. '9' ->
                  let v_str = ref "" in
                  let j = ref i in
                  while !j < len && (match line.[!j] with '0' .. '9' -> true | _ -> false) do
                    v_str := !v_str ^ String.make 1 line.[!j];
                    incr j
                  done;
                  Some (Int64.of_string !v_str)
              | _ -> find_digit (i + 1)
          in
          match find_digit 0 with
          | Some v -> Some (Int64.mul v 1024L) (* Convert kB to bytes *)
          | None -> None
        in
        let rec loop () =
          match input_line ic with
          | line ->
              let set r =
                match read_kb_value line with
                | None -> ()
                | Some v -> r := Some v
              in
              if starts_with ~prefix:"MemTotal:" line then set mem_total
              else if starts_with ~prefix:"MemAvailable:" line then set mem_available
              else if starts_with ~prefix:"MemFree:" line then set mem_free;
              loop ()
          | exception End_of_file -> ()
        in
        (try loop () with _ -> ());
        close_in_noerr ic;
        match (!mem_total, !mem_available, !mem_free) with
        | Some total_bytes, Some avail_bytes, _ ->
            let used_bytes = Int64.sub total_bytes avail_bytes in
            let total_gb = Int64.to_float total_bytes /. (1024. *. 1024. *. 1024.) in
            let used_gb = Int64.to_float used_bytes /. (1024. *. 1024. *. 1024.) in
            let used_percent =
              if total_bytes > 0L then
                (Int64.to_float used_bytes /. Int64.to_float total_bytes) *. 100.
              else 0.0
            in
            Some { total_gb; used_gb; used_percent }
        | Some total_bytes, None, Some free_bytes ->
            let used_bytes = Int64.sub total_bytes free_bytes in
            let total_gb = Int64.to_float total_bytes /. (1024. *. 1024. *. 1024.) in
            let used_gb = Int64.to_float used_bytes /. (1024. *. 1024. *. 1024.) in
            let used_percent =
              if total_bytes > 0L then
                (Int64.to_float used_bytes /. Int64.to_float total_bytes) *. 100.
              else 0.0
            in
            Some { total_gb; used_gb; used_percent }
        | _ -> None)
      else
        (* macOS: Use vm_stat and sysctl *)
        try
          (* Get total memory *)
          let cmd_total = "sysctl -n hw.memsize 2>/dev/null" in
          let ic_total = Unix.open_process_in cmd_total in
          let total_bytes_str =
            try
              let result = input_line ic_total in
              (try Unix.close_process_in ic_total |> ignore with _ -> ());
              result
            with
            | End_of_file ->
                (try Unix.close_process_in ic_total |> ignore with _ -> ());
                "0"
            | e ->
                (try Unix.close_process_in ic_total |> ignore with _ -> ());
                raise e
          in
          let total_bytes = Int64.of_string total_bytes_str in
          (* Get memory stats from vm_stat *)
          let cmd_vm = "vm_stat 2>/dev/null" in
          let ic_vm = Unix.open_process_in cmd_vm in
          let pages_free = ref 0.0 in
          let pages_inactive = ref 0.0 in
          let page_size = 16384.0 in (* macOS page size *)
          try
            while true do
              let line = input_line ic_vm in
              if String.length line > 0 then (
                if starts_with ~prefix:"Pages free:" line then (
                  let re = Str.regexp "Pages free:[ ]+\\([0-9]+\\)\\." in
                  if Str.string_match re line 0 then
                    pages_free := float_of_string (Str.matched_group 1 line));
                if starts_with ~prefix:"Pages inactive:" line then (
                  let re = Str.regexp "Pages inactive:[ ]+\\([0-9]+\\)\\." in
                  if Str.string_match re line 0 then
                    pages_inactive := float_of_string (Str.matched_group 1 line))
              )
            done;
            assert false
          with End_of_file ->
            (try Unix.close_process_in ic_vm |> ignore with _ -> ());
            let free_bytes = !pages_free *. page_size in
            let inactive_bytes = !pages_inactive *. page_size in
            let used_bytes = Int64.to_float total_bytes -. free_bytes -. inactive_bytes in
            let total_gb = Int64.to_float total_bytes /. (1024. *. 1024. *. 1024.) in
            let used_gb = used_bytes /. (1024. *. 1024. *. 1024.) in
            let used_percent =
              if total_bytes > 0L then
                (used_bytes /. Int64.to_float total_bytes) *. 100.
              else 0.0
            in
            Some { total_gb; used_gb; used_percent }
        with _ -> None
    with _ -> None
end

(* ---------- Convenience type aliases ---------- *)
type cpu_stats = Cpu.stats
type memory_stats = Mem.t

