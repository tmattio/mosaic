(** System metrics collection library.
    Stateless, poll-based design - caller manages state and sampling intervals. *)

exception Unavailable of string

(* ---------- C Bindings ---------- *)
external c_get_cpu_load : unit -> int64 array array = "caml_metrics_get_cpu_load"
external c_statvfs : string -> int64 * int64 * int64 = "caml_metrics_statvfs"
external c_proc_self_mem : unit -> int64 * int64 = "caml_metrics_proc_self_mem"

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

  type sample_result = {
    total : times;
    per_core : times array;
  }

  (* Convert array [user, nice, system, idle] to times *)
  let times_of_array arr =
    if Array.length arr < 4 then None
    else
      Some
        {
          user = arr.(0);
          nice = arr.(1);
          system = arr.(2);
          idle = arr.(3);
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
        (* macOS: Use host_processor_info API via C bindings *)
        try
          let cpu_data = c_get_cpu_load () in
          if Array.length cpu_data = 0 then None
          else
            (* First element (index 0) is the total/aggregate *)
            match times_of_array cpu_data.(0) with
            | Some total -> Some total
            | None -> None
        with _ -> None
    with _ -> None

  (* Sample CPU counters with per-core data *)
  let sample_all () : sample_result option =
    try
      if Sys.file_exists "/proc/stat" then (
        (* Linux: Read /proc/stat for per-core data *)
        let ic = open_in "/proc/stat" in
        let total = ref None in
        let cores = ref [] in
        let rec loop () =
          match input_line ic with
          | line ->
              if String.length line >= 4 && String.sub line 0 4 = "cpu " then (
                (* Total CPU *)
                let parts = String.split_on_char ' ' line in
                let parts = List.filter (fun s -> s <> "") parts in
                (match parts with
                | _ :: user :: nice :: system :: idle :: iowait :: _ ->
                    let user_val = Int64.of_string user in
                    let nice_val = Int64.of_string nice in
                    let system_val = Int64.of_string system in
                    let idle_val = Int64.of_string idle in
                    let iowait_val = Int64.of_string iowait in
                    total :=
                      Some
                        {
                          user = Int64.add user_val nice_val;
                          nice = nice_val;
                          system = system_val;
                          idle = Int64.add idle_val iowait_val;
                        }
                | _ -> ());
                loop ())
              else if String.length line >= 4 && String.sub line 0 3 = "cpu" then (
                (* Per-core CPU (cpu0, cpu1, etc.) *)
                let parts = String.split_on_char ' ' line in
                let parts = List.filter (fun s -> s <> "") parts in
                (match parts with
                | _ :: user :: nice :: system :: idle :: iowait :: _ ->
                    let user_val = Int64.of_string user in
                    let nice_val = Int64.of_string nice in
                    let system_val = Int64.of_string system in
                    let idle_val = Int64.of_string idle in
                    let iowait_val = Int64.of_string iowait in
                    cores :=
                      {
                        user = Int64.add user_val nice_val;
                        nice = nice_val;
                        system = system_val;
                        idle = Int64.add idle_val iowait_val;
                      }
                      :: !cores
                | _ -> ());
                loop ())
              else loop ()
          | exception End_of_file -> ()
        in
        (try loop () with _ -> ());
        close_in_noerr ic;
        match !total with
        | Some t -> Some { total = t; per_core = Array.of_list (List.rev !cores) }
        | None -> None)
      else
        (* macOS: Use host_processor_info API via C bindings *)
        try
          let cpu_data = c_get_cpu_load () in
          if Array.length cpu_data = 0 then None
          else
            match times_of_array cpu_data.(0) with
            | Some total ->
                let per_core =
                  if Array.length cpu_data <= 1 then [||]
                  else
                    Array.init (Array.length cpu_data - 1) (fun i ->
                        match times_of_array cpu_data.(i + 1) with
                        | Some t -> t
                        | None ->
                            {
                              user = 0L;
                              nice = 0L;
                              system = 0L;
                              idle = 0L;
                            })
                in
                Some { total; per_core }
            | None -> None
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

  (* Calculate per-core CPU usage *)
  let usage_per_core ~(prev : sample_result) ~(next : sample_result) : stats array option =
    if Array.length prev.per_core <> Array.length next.per_core then None
    else
      Some
        (Array.mapi
           (fun i prev_core ->
             match usage ~prev:prev_core ~next:next.per_core.(i) with
             | Some stats -> stats
             | None -> { user = 0.0; system = 0.0; idle = 100.0 })
           prev.per_core)
end

(* ---------- Memory Module ---------- *)
module Mem = struct
  type t = {
    total_gb : float;
    used_gb : float;
    used_percent : float;
    swap_total_gb : float;
    swap_used_gb : float;
    swap_used_percent : float;
  }

  (* Sample memory statistics (instantaneous, no diffing needed) *)
  let sample () : t option =
    try
      if Sys.file_exists "/proc/meminfo" then (
        (* Linux: Read /proc/meminfo directly *)
        let ic = open_in "/proc/meminfo" in
        let mem_total = ref None
        and mem_available = ref None
        and mem_free = ref None
        and swap_total = ref None
        and swap_free = ref None in
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
              else if starts_with ~prefix:"MemFree:" line then set mem_free
              else if starts_with ~prefix:"SwapTotal:" line then set swap_total
              else if starts_with ~prefix:"SwapFree:" line then set swap_free;
              loop ()
          | exception End_of_file -> ()
        in
        (try loop () with _ -> ());
        close_in_noerr ic;
        (* Calculate swap memory *)
        let swap_total_gb, swap_used_gb, swap_used_percent =
          match (!swap_total, !swap_free) with
          | Some total_swap, Some free_swap ->
              let used_swap = Int64.sub total_swap free_swap in
              let total_gb = Int64.to_float total_swap /. (1024. *. 1024. *. 1024.) in
              let used_gb = Int64.to_float used_swap /. (1024. *. 1024. *. 1024.) in
              let used_percent =
                if total_swap > 0L then
                  (Int64.to_float used_swap /. Int64.to_float total_swap) *. 100.
                else 0.0
              in
              (total_gb, used_gb, used_percent)
          | _ -> (0.0, 0.0, 0.0)
        in
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
            Some { total_gb; used_gb; used_percent; swap_total_gb; swap_used_gb; swap_used_percent }
        | Some total_bytes, None, Some free_bytes ->
            let used_bytes = Int64.sub total_bytes free_bytes in
            let total_gb = Int64.to_float total_bytes /. (1024. *. 1024. *. 1024.) in
            let used_gb = Int64.to_float used_bytes /. (1024. *. 1024. *. 1024.) in
            let used_percent =
              if total_bytes > 0L then
                (Int64.to_float used_bytes /. Int64.to_float total_bytes) *. 100.
              else 0.0
            in
            Some { total_gb; used_gb; used_percent; swap_total_gb; swap_used_gb; swap_used_percent }
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
            (* Get swap info from sysctl vm.swapusage *)
            let swap_total_gb, swap_used_gb, swap_used_percent =
              try
                let cmd_swap = "sysctl -n vm.swapusage 2>/dev/null" in
                let ic_swap = Unix.open_process_in cmd_swap in
                let swap_line =
                  try
                    let result = input_line ic_swap in
                    (try Unix.close_process_in ic_swap |> ignore with _ -> ());
                    result
                  with
                  | End_of_file ->
                      (try Unix.close_process_in ic_swap |> ignore with _ -> ());
                      ""
                  | e ->
                      (try Unix.close_process_in ic_swap |> ignore with _ -> ());
                      raise e
                in
                (* Parse: "total = 1024.00M  used = 512.00M  free = 512.00M  (encrypted)" *)
                let parse_size s =
                  (* Extract number and unit (M, G, etc.) *)
                  let re = Str.regexp "\\([0-9]+\\.?[0-9]*\\)\\([MG]\\)" in
                  if Str.string_match re s 0 then
                    let value = float_of_string (Str.matched_group 1 s) in
                    let unit = Str.matched_group 2 s in
                    match unit with
                    | "G" -> value
                    | "M" -> value /. 1024.0
                    | _ -> 0.0
                  else 0.0
                in
                let extract_field field_name =
                  (* Match: "field_name = 123.45M" - search anywhere in the string *)
                  let re = Str.regexp (Printf.sprintf "%s = \\([0-9]+\\.?[0-9]*[MG]\\)" (Str.quote field_name)) in
                  try
                    ignore (Str.search_forward re swap_line 0);
                    parse_size (Str.matched_group 1 swap_line)
                  with Not_found -> 0.0
                in
                let total = extract_field "total" in
                let used = extract_field "used" in
                let used_percent = if total > 0.0 then (used /. total) *. 100.0 else 0.0 in
                (total, used, used_percent)
              with _ -> (0.0, 0.0, 0.0)
            in
            Some { total_gb; used_gb; used_percent; swap_total_gb; swap_used_gb; swap_used_percent }
        with _ -> None
    with _ -> None
end

(* ---------- Disk Module ---------- *)
module Disk = struct
  type partition = {
    mount_point : string;
    total_gb : float;
    used_gb : float;
    avail_gb : float;
    used_percent : float;
  }

  type t = {
    total_gb : float;
    used_gb : float;
    avail_gb : float;
    used_percent : float;
    partitions : partition list;
  }

  (* Read partitions using df command *)
  let read_partitions () : partition list =
    try
      let cmd = "df -h 2>/dev/null" in
      let ic = Unix.open_process_in cmd in
      let partitions = ref [] in
      try
        (* Skip header line *)
        ignore (input_line ic);
        while true do
          let line = input_line ic in
          (* Parse df output - format differs between macOS and Linux *)
          (* macOS: Filesystem Size Used Avail Capacity iused ifree %iused Mounted on *)
          (* Linux: Filesystem Size Used Avail Use% Mounted on *)
          (* Split by whitespace and filter empty strings *)
          let parts = String.split_on_char ' ' line in
          let parts = List.filter (fun s -> s <> "") parts in
          (* Detect format by checking if 4th field ends with % (Capacity/Use%) *)
          let is_macos_format =
            List.length parts >= 5
            && (let field4 = List.nth parts 4 in
                String.length field4 > 0 && field4.[String.length field4 - 1] = '%')
          in
          if is_macos_format && List.length parts >= 9 then (
            (* macOS format: filesystem, size, used, avail, capacity%, iused, ifree, %iused, mountpoint... *)
            let filesystem = List.nth parts 0 in
            let size_str = List.nth parts 1 in
            let used_str = List.nth parts 2 in
            let avail_str = List.nth parts 3 in
            let percent_str = List.nth parts 4 in
            (* Mount point is everything after the 8th field (%iused), which is index 8 *)
            let mount_point_parts = List.tl (List.tl (List.tl (List.tl (List.tl (List.tl (List.tl (List.tl parts))))))) in
            let mount_point = String.concat " " mount_point_parts in
            (* Parse size strings like "500Gi", "200Gi", "300Gi", "40%" *)
            let parse_size s =
              let len = String.length s in
              if len < 2 then 0.0
              else
                try
                  let num_str = String.sub s 0 (len - 2) in
                  let unit = String.sub s (len - 2) 2 in
                  let num = float_of_string num_str in
                  match unit with
                  | "Gi" -> num
                  | "Mi" -> num /. 1024.0
                  | "Ki" -> num /. (1024. *. 1024.)
                  | "Ti" -> num *. 1024.0
                  | "Bi" -> 0.0 (* Bytes, too small *)
                  | _ -> 0.0
                with _ -> 0.0
            in
            let parse_percent s =
              try
                let len = String.length s in
                if len > 0 && s.[len - 1] = '%' then
                  let pct_str = String.sub s 0 (len - 1) in
                  float_of_string pct_str
                else 0.0
              with _ -> 0.0
            in
            let total_gb = parse_size size_str in
            let used_gb = parse_size used_str in
            let avail_gb = parse_size avail_str in
            let used_percent = parse_percent percent_str in
            (* Filter out system partitions *)
            if total_gb > 0.1
               && not (String.contains mount_point '\000')
               && not (starts_with ~prefix:"/dev" mount_point)
               && not (starts_with ~prefix:"/proc" mount_point)
               && not (starts_with ~prefix:"/sys" mount_point)
               && not (starts_with ~prefix:"tmpfs" filesystem)
               && not (starts_with ~prefix:"devtmpfs" filesystem)
               && not (starts_with ~prefix:"devfs" filesystem)
            then
              partitions :=
                {
                  mount_point;
                  total_gb;
                  used_gb;
                  avail_gb;
                  used_percent;
                }
                :: !partitions)
          else if List.length parts >= 6 then (
            (* Linux format: filesystem, size, used, avail, use%, mountpoint... *)
            let filesystem = List.nth parts 0 in
            let size_str = List.nth parts 1 in
            let used_str = List.nth parts 2 in
            let avail_str = List.nth parts 3 in
            let percent_str = List.nth parts 4 in
            (* Mount point is everything after use% *)
            let mount_point = String.concat " " (List.tl (List.tl (List.tl (List.tl (List.tl parts))))) in
            (* Parse size strings *)
            let parse_size s =
              let len = String.length s in
              if len < 2 then 0.0
              else
                try
                  let num_str = String.sub s 0 (len - 2) in
                  let unit = String.sub s (len - 2) 2 in
                  let num = float_of_string num_str in
                  match unit with
                  | "Gi" -> num
                  | "Mi" -> num /. 1024.0
                  | "Ki" -> num /. (1024. *. 1024.)
                  | "Ti" -> num *. 1024.0
                  | "Bi" -> 0.0
                  | _ -> 0.0
                with _ -> 0.0
            in
            let parse_percent s =
              try
                let len = String.length s in
                if len > 0 && s.[len - 1] = '%' then
                  let pct_str = String.sub s 0 (len - 1) in
                  float_of_string pct_str
                else 0.0
              with _ -> 0.0
            in
            let total_gb = parse_size size_str in
            let used_gb = parse_size used_str in
            let avail_gb = parse_size avail_str in
            let used_percent = parse_percent percent_str in
            (* Filter out system partitions *)
            if total_gb > 0.1
               && not (String.contains mount_point '\000')
               && not (starts_with ~prefix:"/dev" mount_point)
               && not (starts_with ~prefix:"/proc" mount_point)
               && not (starts_with ~prefix:"/sys" mount_point)
               && not (starts_with ~prefix:"tmpfs" filesystem)
               && not (starts_with ~prefix:"devtmpfs" filesystem)
            then
              partitions :=
                {
                  mount_point;
                  total_gb;
                  used_gb;
                  avail_gb;
                  used_percent;
                }
                :: !partitions)
        done;
        assert false
      with End_of_file ->
        (try Unix.close_process_in ic |> ignore with _ -> ());
        List.rev !partitions
    with _ -> []

  (* Sample disk filesystem statistics *)
  let sample ?(path = "/") () : t option =
    try
      let total, free, avail = c_statvfs path in
      if total < 0L then None
      else
        let used = Int64.max 0L Int64.(sub total free) in
        let total_gb = Int64.to_float total /. (1024. *. 1024. *. 1024.) in
        let used_gb = Int64.to_float used /. (1024. *. 1024. *. 1024.) in
        let avail_gb = Int64.to_float avail /. (1024. *. 1024. *. 1024.) in
        let used_percent =
          if total > 0L then (Int64.to_float used /. Int64.to_float total) *. 100. else 0.0
        in
        let partitions = read_partitions () in
        Some { total_gb; used_gb; avail_gb; used_percent; partitions }
    with _ -> None
end

(* ---------- Process Module ---------- *)
module Proc = struct
  type t = {
    cpu_percent : float;
    rss_mb : float;
    vsize_mb : float;
  }

  (* Helper to skip spaces in string *)
  let skip_spaces s i =
    let len = String.length s in
    let j = ref i in
    while !j < len && is_space s.[!j] do
      incr j
    done;
    !j

  (* Parse /proc/self/stat on Linux to get RSS and VSIZE *)
  let linux_self_rss_vsz () : (int64 option * int64 option) =
    try
      let ic = open_in "/proc/self/stat" in
      let line =
        try
          let s = input_line ic in
          close_in ic;
          s
        with e ->
          close_in_noerr ic;
          raise e
      in
      (* Find the closing paren to skip process name *)
      match String.rindex_opt line ')' with
      | None -> (None, None)
      | Some k ->
          let rest =
            if k + 2 <= String.length line then
              String.sub line (k + 2) (String.length line - (k + 2))
            else ""
          in
          (* Parse space-separated tokens *)
          let rec gather i acc =
            let i = skip_spaces rest i in
            if i >= String.length rest then List.rev acc
            else (
              let j = ref i in
              while !j < String.length rest && not (is_space rest.[!j]) do
                incr j
              done;
              let tok = String.sub rest i (!j - i) in
              gather !j (tok :: acc))
          in
          let toks = gather 0 [] in
          let get_tok idx =
            if idx < List.length toks then Some (List.nth toks idx) else None
          in
          let page_size = 4096L in
          (* Field 22 is VSIZE, field 23 is RSS (in pages) *)
          let vsize =
            match get_tok 20 with
            | Some s -> (try Some (Int64.of_string s) with _ -> None)
            | None -> None
          in
          let rss =
            match get_tok 21 with
            | Some s -> (
                try
                  let pages = Int64.of_string s in
                  Some Int64.(mul pages page_size)
                with _ -> None)
            | None -> None
          in
          (rss, vsize)
    with _ -> (None, None)

  (* Sample process (self) statistics *)
  let sample ~prev_utime ~prev_stime ~dt ~num_cores () : t option =
    try
      (* Get CPU time using Unix.times() *)
      let t = Unix.times () in
      let utime = t.Unix.tms_utime in
      let stime = t.Unix.tms_stime in
      
      (* Calculate CPU percentage if we have previous sample *)
      let cpu_percent =
        match (prev_utime, prev_stime) with
        | Some prev_u, Some prev_s ->
            let cpu_delta = (utime -. prev_u) +. (stime -. prev_s) in
            (* Handle potential time wrapping or negative deltas *)
            let cpu_delta = max 0.0 cpu_delta in
            (* Ensure dt is reasonable (between 0.01 and 10 seconds) *)
            if dt > 0.01 && dt < 10.0 && cpu_delta >= 0.0 then (
              (* Calculate CPU percentage: cpu_time / wall_time * 100 *)
              (* On multi-core systems, this can exceed 100% (e.g., 200% = using 2 cores at 100%) *)
              (* Normalize by number of cores to show as percentage of total system capacity *)
              let raw_percent = (cpu_delta /. dt) *. 100.0 in
              (* Cap at reasonable maximum (e.g., 1000% for safety) *)
              let capped_percent = min raw_percent 1000.0 in
              match num_cores with
              | Some n when n > 0 && n <= 128 ->
                  (* Normalize by cores: divide by number of cores to get system-wide percentage *)
                  let normalized = capped_percent /. float_of_int n in
                  (* Cap normalized value at 100% *)
                  min normalized 100.0
              | _ -> 
                  (* If cores unknown or invalid, show raw percentage but cap at 100% per core *)
                  (* Assume max 8 cores for safety *)
                  min capped_percent 800.0
            ) else 0.0
        | _ -> 0.0
      in
      
      (* Get memory info *)
      let rss, vsize =
        if Sys.file_exists "/proc/self/stat" then
          linux_self_rss_vsz ()
        else (
          let rss_v, vsz_v = c_proc_self_mem () in
          let opt_i64 x = if x < 0L then None else Some x in
          (opt_i64 rss_v, opt_i64 vsz_v))
      in
      
      let rss_mb =
        match rss with
        | Some r -> Int64.to_float r /. (1024. *. 1024.)
        | None -> 0.0
      in
      let vsize_mb =
        match vsize with
        | Some v -> Int64.to_float v /. (1024. *. 1024.)
        | None -> 0.0
      in
      
      Some { cpu_percent; rss_mb; vsize_mb }
    with _ -> None
end

(* ---------- Processes Module ---------- *)
module Processes = struct
  type process = {
    pid : int;
    name : string;
    cpu_percent : float;
    mem_percent : float;
    rss_mb : float;
  }

  (* Read top processes using ps command *)
  let read_top_processes ?(limit = 10) ?(sort_by = `Cpu) () : process list =
    try
      (* Use ps aux to get process list, works on both Linux and macOS *)
      let cmd = "ps aux 2>/dev/null" in
      let ic = Unix.open_process_in cmd in
      let processes = ref [] in
      try
        (* Skip header line *)
        ignore (input_line ic);
        while true do
          let line = input_line ic in
          (* Parse: USER PID %CPU %MEM VSZ RSS TT STAT STARTED TIME COMMAND *)
          (* Split by whitespace, but command can have spaces *)
          let parts = String.split_on_char ' ' line in
          let parts = List.filter (fun s -> s <> "") parts in
          (* Need at least 11 fields: USER, PID, %CPU, %MEM, VSZ, RSS, TT, STAT, STARTED, TIME, COMMAND... *)
          if List.length parts >= 11 then
            try
              let pid = int_of_string (List.nth parts 1) in
              let cpu_str = List.nth parts 2 in
              let mem_str = List.nth parts 3 in
              let rss_str = List.nth parts 5 in
              (* Command is everything from field 10 onwards *)
              let cmd_parts = List.tl (List.tl (List.tl (List.tl (List.tl (List.tl (List.tl (List.tl (List.tl (List.tl parts))))))))) in
              let name = String.concat " " cmd_parts in
              (* Smart truncation: extract basename and truncate intelligently *)
              let truncate_name s max_len =
                if String.length s <= max_len then s
                else (
                  (* Try to extract basename (last component) *)
                  let basename =
                    try
                      let last_slash = String.rindex s '/' in
                      String.sub s (last_slash + 1) (String.length s - last_slash - 1)
                    with Not_found -> s
                  in
                  if String.length basename <= max_len then basename
                  else (
                    (* If basename is still too long, truncate with "..." in the middle *)
                    let prefix_len = (max_len - 3) / 2 in
                    let suffix_len = max_len - 3 - prefix_len in
                    let prefix = String.sub basename 0 prefix_len in
                    let suffix = String.sub basename (String.length basename - suffix_len) suffix_len in
                    prefix ^ "..." ^ suffix
                  )
                )
              in
              let name = truncate_name name 30 in
              (* Parse percentages *)
              let cpu_percent = try float_of_string cpu_str with _ -> 0.0 in
              let mem_percent = try float_of_string mem_str with _ -> 0.0 in
              (* Parse RSS (in KB on Linux, might be different on macOS) *)
              let rss_kb = try float_of_string rss_str with _ -> 0.0 in
              let rss_mb = rss_kb /. 1024.0 in
              (* Filter out kernel processes and very low usage processes *)
              if cpu_percent > 0.0 || mem_percent > 0.0 then
                processes :=
                  {
                    pid;
                    name;
                    cpu_percent;
                    mem_percent;
                    rss_mb;
                  }
                  :: !processes
            with _ -> ()
        done;
        assert false
      with End_of_file ->
        (try Unix.close_process_in ic |> ignore with _ -> ());
        (* Sort by specified criteria *)
        let sorted =
          match sort_by with
          | `Cpu ->
              List.sort (fun a b -> compare b.cpu_percent a.cpu_percent) !processes
          | `Mem ->
              List.sort (fun a b -> compare b.mem_percent a.mem_percent) !processes
          | `Rss ->
              List.sort (fun a b -> compare b.rss_mb a.rss_mb) !processes
        in
        (* Take top N processes *)
        let rec take n = function
          | [] -> []
          | x :: xs -> if n <= 0 then [] else x :: take (n - 1) xs
        in
        take limit sorted
    with _ -> []
end

(* ---------- Convenience type aliases ---------- *)
type cpu_stats = Cpu.stats
type memory_stats = Mem.t
type disk_stats = Disk.t
type disk_partition = Disk.partition
type process_stats = Proc.t
type process_info = Processes.process

