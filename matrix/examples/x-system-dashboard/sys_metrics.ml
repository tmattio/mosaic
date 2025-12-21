exception Unavailable of string

external c_clock_gettime_ns : unit -> int64 = "caml_sys_metrics_clock_gettime_ns"

let now_ns () : int64 =
  try c_clock_gettime_ns ()
  with _ -> Int64.of_float (Unix.gettimeofday () *. 1e9)

let is_space = function ' ' | '\t' | '\r' | '\n' -> true | _ -> false

let starts_with ~prefix s =
  let lp = String.length prefix and ls = String.length s in
  ls >= lp && String.sub s 0 lp = prefix

let trim s =
  let len = String.length s in
  let i = ref 0 in
  while !i < len && is_space s.[!i] do
    incr i
  done;
  let j = ref (len - 1) in
  while !j >= !i && is_space s.[!j] do
    decr j
  done;
  if !j < !i then "" else String.sub s !i (!j - !i + 1)

let skip_spaces s i =
  let len = String.length s in
  let j = ref i in
  while !j < len && is_space s.[!j] do
    incr j
  done;
  !j

let parse_uint64 (s : string) (i0 : int) : int64 * int =
  let len = String.length s in
  let i = ref i0 in
  let acc = ref 0L in
  let digit c = Int64.of_int (Char.code c - Char.code '0') in
  while !i < len do
    match s.[!i] with
    | '0' .. '9' as c ->
      acc := Int64.add (Int64.mul !acc 10L) (digit c);
      incr i
    | _ -> i := len
  done;
  (!acc, !i)

let parse_int64s_from s i0 : int64 list =
  let len = String.length s in
  let rec loop i acc =
    let i = skip_spaces s i in
    if i >= len then List.rev acc
    else
      match s.[i] with
      | '0' .. '9' ->
        let v, j = parse_uint64 s i in
        loop j (v :: acc)
      | _ -> loop (i + 1) acc
  in
  loop i0 []

external c_uname_sysname : unit -> string = "caml_sys_metrics_uname_sysname"

let uname_sysname () =
  if Sys.os_type <> "Unix" then Sys.os_type
  else (
    try c_uname_sysname () with _ -> "Unix"
  )

let is_linux () = uname_sysname () = "Linux"

external c_cpu_times : unit -> int64 array array = "caml_sys_metrics_cpu_times"

external c_mem_snapshot : unit -> int64 array = "caml_sys_metrics_mem_snapshot"

external c_net_ifaces
  :  unit
  -> (string * int64 * int64 * int64 * int64) array
  = "caml_sys_metrics_net_ifaces"

external c_statvfs : string -> int64 * int64 * int64 = "caml_sys_metrics_statvfs"

external c_proc_self_mem : unit -> int64 * int64 = "caml_sys_metrics_proc_self_mem"

external c_nvml_snapshot
  :  unit
  -> (int * string * int * int * int64 * int64 * int64 * int * int) array
  = "caml_sys_metrics_nvml_snapshot"

let opt_i64 (x : int64) : int64 option = if x < 0L then None else Some x

module Cpu = struct
  type times = {
    user : int64;
    nice : int64;
    system : int64;
    idle : int64;
    iowait : int64;
    irq : int64;
    softirq : int64;
    steal : int64;
  }

  type t = {
    total : times;
    per_core : times array;
  }

  type pct = {
    user : float;
    nice : float;
    system : float;
    idle : float;
    iowait : float;
    irq : float;
    softirq : float;
    steal : float;
    busy : float;
  }

  let times_of_fields (a : int64 array) : times =
    let get i = if i < Array.length a then a.(i) else 0L in
    {
      user = get 0;
      nice = get 1;
      system = get 2;
      idle = get 3;
      iowait = get 4;
      irq = get 5;
      softirq = get 6;
      steal = get 7;
    }

  let linux_read_proc_stat () : times * (int * times) list =
    let ic = open_in "/proc/stat" in
    let total = ref None in
    let cores = ref [] in
    let parse_cpu_line (line : string) =
      let len = String.length line in
      if len < 3 || String.sub line 0 3 <> "cpu" then None
      else (
        let j = ref 0 in
        while !j < len && not (is_space line.[!j]) do
          incr j
        done;
        let label = String.sub line 0 !j in
        let nums = parse_int64s_from line !j in
        let fields = Array.of_list nums in
        Some (label, times_of_fields fields)
      )
    in
    let rec loop () =
      match input_line ic with
      | line ->
        (match parse_cpu_line line with
         | None -> ()
         | Some ("cpu", t) -> total := Some t
         | Some (lab, t) ->
           if String.length lab >= 4 then (
             let k = ref 3 in
             while !k < String.length lab && lab.[!k] >= '0' && lab.[!k] <= '9' do
               incr k
             done;
             if !k > 3 then (
               let id = int_of_string (String.sub lab 3 (!k - 3)) in
               cores := (id, t) :: !cores
             )
           ));
        loop ()
      | exception End_of_file -> ()
    in
    (try loop () with _ -> ());
    close_in_noerr ic;
    match !total with
    | None -> raise (Unavailable "Linux: failed to read /proc/stat")
    | Some tot -> (tot, !cores)

  let linux_sample () : t =
    let total, cores_list = linux_read_proc_stat () in
    let max_id = List.fold_left (fun acc (id, _) -> max acc id) (-1) cores_list in
    let per_core =
      if max_id < 0 then [||]
      else (
        let a =
          Array.make (max_id + 1)
            {
              user = 0L;
              nice = 0L;
              system = 0L;
              idle = 0L;
              iowait = 0L;
              irq = 0L;
              softirq = 0L;
              steal = 0L;
            }
        in
        List.iter
          (fun (id, t) -> if id >= 0 && id < Array.length a then a.(id) <- t)
          cores_list;
        a
      )
    in
    { total; per_core }

  let stub_sample () : t =
    let a = c_cpu_times () in
    if Array.length a = 0 then
      raise (Unavailable "CPU metrics not available on this platform/build");
    let total = times_of_fields a.(0) in
    let per_core =
      if Array.length a <= 1 then [||]
      else Array.init (Array.length a - 1) (fun i -> times_of_fields a.(i + 1))
    in
    { total; per_core }

  let sample () : t = if is_linux () then linux_sample () else stub_sample ()

  let delta a b = Int64.sub b a

  let sum_times (t : times) =
    Int64.(
      add t.user
        (add t.nice
           (add t.system
              (add t.idle (add t.iowait (add t.irq (add t.softirq t.steal)))))))

  let diff_times (a : times) (b : times) : times =
    {
      user = delta a.user b.user;
      nice = delta a.nice b.nice;
      system = delta a.system b.system;
      idle = delta a.idle b.idle;
      iowait = delta a.iowait b.iowait;
      irq = delta a.irq b.irq;
      softirq = delta a.softirq b.softirq;
      steal = delta a.steal b.steal;
    }

  let pct_of (d : times) : pct =
    let total = sum_times d in
    let total_f = Int64.to_float total in
    let frac x = if total = 0L then 0.0 else Int64.to_float x /. total_f *. 100.0 in
    let user = frac d.user
    and nice = frac d.nice
    and system = frac d.system
    and idle = frac d.idle
    and iowait = frac d.iowait
    and irq = frac d.irq
    and softirq = frac d.softirq
    and steal = frac d.steal in
    let busy = max 0.0 (100.0 -. idle -. iowait) in
    { user; nice; system; idle; iowait; irq; softirq; steal; busy }

  let busy ~dt_ns:_ ~prev ~next =
    let d = diff_times prev next in
    let p = pct_of d in
    p.busy

  let usage ~dt_ns:_ ~prev ~next =
    let d = diff_times prev next in
    pct_of d
end

module Mem = struct
  type t = {
    total : int64;
    free : int64;
    available : int64 option;
    used : int64;
    cached : int64 option;
    buffers : int64 option;
    swap_total : int64 option;
    swap_free : int64 option;
  }

  let parse_meminfo () =
    let ic = open_in "/proc/meminfo" in
    let total = ref None
    and free = ref None
    and avail = ref None
    and cached = ref None
    and buffers = ref None
    and swap_total = ref None
    and swap_free = ref None in
    let read_kb_value (line : string) : int64 option =
      let len = String.length line in
      let rec find_digit i =
        if i >= len then None
        else
          match line.[i] with
          | '0' .. '9' ->
            let v, _ = parse_uint64 line i in
            Some Int64.(mul v 1024L)
          | _ -> find_digit (i + 1)
      in
      find_digit 0
    in
    let rec loop () =
      match input_line ic with
      | line ->
        let set r =
          match read_kb_value line with
          | None -> ()
          | Some v -> r := Some v
        in
        if starts_with ~prefix:"MemTotal:" line then set total
        else if starts_with ~prefix:"MemFree:" line then set free
        else if starts_with ~prefix:"MemAvailable:" line then set avail
        else if starts_with ~prefix:"Cached:" line then set cached
        else if starts_with ~prefix:"Buffers:" line then set buffers
        else if starts_with ~prefix:"SwapTotal:" line then set swap_total
        else if starts_with ~prefix:"SwapFree:" line then set swap_free;
        loop ()
      | exception End_of_file -> ()
    in
    (try loop () with _ -> ());
    close_in_noerr ic;
    match (!total, !free) with
    | Some t, Some f -> (t, f, !avail, !cached, !buffers, !swap_total, !swap_free)
    | _ -> raise (Unavailable "Linux: failed to read /proc/meminfo")

  let linux_sample () : t =
    let total_v, free_v, avail, cached, buffers, swap_total, swap_free =
      parse_meminfo ()
    in
    let used =
      match avail with
      | Some a -> Int64.max 0L Int64.(sub total_v a)
      | None ->
        let c = Option.value cached ~default:0L
        and b = Option.value buffers ~default:0L in
        Int64.max 0L Int64.(sub (sub (sub total_v free_v) c) b)
    in
    {
      total = total_v;
      free = free_v;
      available = avail;
      used;
      cached;
      buffers;
      swap_total;
      swap_free;
    }

  let stub_sample () : t =
    let a = c_mem_snapshot () in
    if Array.length a < 10 then raise (Unavailable "mem stub returned short array");
    let total = a.(0)
    and free = a.(1)
    and avail = a.(2)
    and cached = a.(6)
    and buffers = a.(7)
    and swap_total = a.(8)
    and swap_free = a.(9) in
    if total < 0L || free < 0L then raise (Unavailable "Memory metrics not available");
    let available_opt = opt_i64 avail in
    let used =
      match available_opt with
      | Some av -> Int64.max 0L Int64.(sub total av)
      | None -> Int64.max 0L Int64.(sub total free)
    in
    {
      total;
      free;
      available = available_opt;
      used;
      cached = opt_i64 cached;
      buffers = opt_i64 buffers;
      swap_total = opt_i64 swap_total;
      swap_free = opt_i64 swap_free;
    }

  let sample () =
    if is_linux () then (
      try linux_sample () with Unavailable _ -> stub_sample ()
    )
    else stub_sample ()
end

module Net = struct
  type iface = {
    name : string;
    rx_bytes : int64;
    tx_bytes : int64;
    rx_packets : int64;
    tx_packets : int64;
  }

  type t = iface array

  type rate = {
    rx_bytes_per_s : float;
    tx_bytes_per_s : float;
    rx_packets_per_s : float;
    tx_packets_per_s : float;
  }

  let linux_read_net_dev () : iface array =
    let ic = open_in "/proc/net/dev" in
    (try ignore (input_line ic) with _ -> ());
    (try ignore (input_line ic) with _ -> ());
    let acc = ref [] in
    let parse_line line =
      let line = trim line in
      match String.index_opt line ':' with
      | None -> None
      | Some k ->
        let name = trim (String.sub line 0 k) in
        let rest = String.sub line (k + 1) (String.length line - k - 1) in
        let nums = parse_int64s_from rest 0 in
        let get n = if n < List.length nums then List.nth nums n else 0L in
        let rx_bytes = get 0
        and rx_packets = get 1
        and tx_bytes = get 8
        and tx_packets = get 9 in
        Some { name; rx_bytes; tx_bytes; rx_packets; tx_packets }
    in
    let rec loop () =
      match input_line ic with
      | line ->
        (match parse_line line with
         | None -> ()
         | Some i -> acc := i :: !acc);
        loop ()
      | exception End_of_file -> ()
    in
    (try loop () with _ -> ());
    close_in_noerr ic;
    Array.of_list (List.rev !acc)

  let stub_sample () : t =
    let a = c_net_ifaces () in
    Array.map
      (fun (name, rx_b, tx_b, rx_p, tx_p) ->
        { name; rx_bytes = rx_b; tx_bytes = tx_b; rx_packets = rx_p; tx_packets = tx_p })
      a

  let sample () : t =
    if is_linux () then (
      try linux_read_net_dev () with _ -> stub_sample ()
    )
    else stub_sample ()

  let rate ~dt_ns ~prev ~next =
    let dt = Int64.to_float dt_ns /. 1e9 in
    if dt <= 0.0 then
      { rx_bytes_per_s = 0.0; tx_bytes_per_s = 0.0; rx_packets_per_s = 0.0; tx_packets_per_s = 0.0 }
    else (
      let delta a b =
        let d = Int64.sub b a in
        if d < 0L then 0L else d
      in
      let rx_b = delta prev.rx_bytes next.rx_bytes
      and tx_b = delta prev.tx_bytes next.tx_bytes
      and rx_p = delta prev.rx_packets next.rx_packets
      and tx_p = delta prev.tx_packets next.tx_packets in
      {
        rx_bytes_per_s = Int64.to_float rx_b /. dt;
        tx_bytes_per_s = Int64.to_float tx_b /. dt;
        rx_packets_per_s = Int64.to_float rx_p /. dt;
        tx_packets_per_s = Int64.to_float tx_p /. dt;
      }
    )

  let rates ~dt_ns ~prev ~next =
    let tbl = Hashtbl.create (Array.length prev * 2 + 1) in
    Array.iter (fun i -> Hashtbl.replace tbl i.name i) prev;
    let acc = ref [] in
    Array.iter
      (fun n ->
        match Hashtbl.find_opt tbl n.name with
        | None -> ()
        | Some p ->
          let r = rate ~dt_ns ~prev:p ~next:n in
          acc := (n.name, r) :: !acc)
      next;
    Array.of_list (List.rev !acc)
end

module Disk = struct
  type t = {
    total : int64;
    free : int64;
    avail : int64;
    used : int64;
  }

  let stat path =
    let total, free, avail =
      try c_statvfs path
      with _ ->
        raise (Unavailable ("statvfs/GetDiskFreeSpaceEx failed for path: " ^ path))
    in
    if total < 0L then raise (Unavailable ("disk usage unavailable for path: " ^ path));
    let used = Int64.max 0L Int64.(sub total free) in
    { total; free; avail; used }
end

module Proc = struct
  type t = {
    utime_ns : int64;
    stime_ns : int64;
    rss : int64 option;
    vsize : int64 option;
  }

  let linux_self_rss_vsz () : int64 option * int64 option =
    let line =
      let ic = open_in "/proc/self/stat" in
      let s =
        try input_line ic
        with e ->
          close_in_noerr ic;
          raise e
      in
      close_in_noerr ic;
      s
    in
    match String.rindex_opt line ')' with
    | None -> (None, None)
    | Some k ->
      let rest =
        if k + 2 <= String.length line then
          String.sub line (k + 2) (String.length line - (k + 2))
        else ""
      in
      let toks =
        let rec gather i acc =
          let i = skip_spaces rest i in
          if i >= String.length rest then List.rev acc
          else (
            let j = ref i in
            while !j < String.length rest && not (is_space rest.[!j]) do
              incr j
            done;
            let tok = String.sub rest i (!j - i) in
            gather !j (tok :: acc)
          )
        in
        gather 0 []
      in
      let get_tok idx =
        if idx < List.length toks then Some (List.nth toks idx) else None
      in
      let page_size = 4096L in
      let vsize =
        match get_tok 20 with
        | Some s -> ( try Some (Int64.of_string s) with _ -> None)
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

  let sample () : t =
    let t = Unix.times () in
    let utime_ns = Int64.of_float (t.Unix.tms_utime *. 1e9) in
    let stime_ns = Int64.of_float (t.Unix.tms_stime *. 1e9) in
    let rss, vsize =
      if is_linux () then (
        try linux_self_rss_vsz () with _ -> (None, None)
      )
      else (
        let rss_v, vsz_v = c_proc_self_mem () in
        (opt_i64 rss_v, opt_i64 vsz_v)
      )
    in
    { utime_ns; stime_ns; rss; vsize }

  let cpu_pct ~dt_ns ~prev ~next =
    let dt = Int64.to_float dt_ns in
    if dt <= 0.0 then 0.0
    else (
      let du = Int64.to_float (Int64.sub next.utime_ns prev.utime_ns) in
      let ds = Int64.to_float (Int64.sub next.stime_ns prev.stime_ns) in
      (du +. ds) /. dt *. 100.0
    )
end

module Gpu = struct
  type device = {
    index : int;
    name : string;
    util_gpu : int;
    util_mem : int;
    mem_total : int64;
    mem_used : int64;
    mem_free : int64;
    temp_c : int;
    power_mw : int;
  }

  let sample () : device array =
    let a = c_nvml_snapshot () in
    Array.map
      (fun (index, name, util_gpu, util_mem, total, used, free, temp_c, power_mw) ->
        {
          index;
          name;
          util_gpu;
          util_mem;
          mem_total = total;
          mem_used = used;
          mem_free = free;
          temp_c;
          power_mw;
        })
      a

  let available () : bool = Array.length (c_nvml_snapshot ()) > 0
end
