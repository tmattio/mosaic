(** System metrics panel - CPU and Memory monitoring example.
    Collects real CPU and memory usage from the system. *)
open Mosaic_tea
module Charts = Matrix_charts
module Cpu_bindings = Cpu_bindings

(* ---------- CPU Collection ---------- *)
type cpu_stats = {
  user : float;
  system : float;
  idle : float;
}

(* ---------- Memory Collection ---------- *)
type memory_stats = {
  total_gb : float;
  used_gb : float;
  (*free_gb : float;*)
  used_percent : float;
}

(* Previous CPU counters for calculating deltas *)
(* For Linux: (user_total, system_total, idle_total, total) *)
(* For macOS: (user_ticks, system_ticks, idle_ticks, nice_ticks) *)
let prev_cpu_counters = ref None

let get_cpu_usage () : cpu_stats option =
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
          
          let parse_values user nice system idle iowait_opt =
            let iowait_val = Option.value ~default:0.0 iowait_opt in
            let user_total = float_of_string user +. float_of_string nice in
            let system_total = float_of_string system in
            let idle_total = float_of_string idle +. iowait_val in
            let total = user_total +. system_total +. idle_total in
            let current = (user_total, system_total, idle_total, total) in
            match !prev_cpu_counters with
            | Some (pu, ps, pi, pt) ->
                let du = user_total -. pu in
                let ds = system_total -. ps in
                let di = idle_total -. pi in
                let dt = total -. pt in
                if dt > 0. then (
                  prev_cpu_counters := Some current;
                  Some
                    {
                      user = (du /. dt) *. 100.;
                      system = (ds /. dt) *. 100.;
                      idle = (di /. dt) *. 100.;
                    })
                else (
                  prev_cpu_counters := Some current;
                  None)
            | None ->
                prev_cpu_counters := Some current;
                Some { user = 0.0; system = 0.0; idle = 100.0 }
          in
          
          match parts with
          | _ :: user :: nice :: system :: idle :: iowait :: _ ->
              parse_values user nice system idle (Some (float_of_string iowait))
          | _ :: user :: nice :: system :: idle :: [] ->
              parse_values user nice system idle None
          | _ -> None
        with _ -> None)
    else
      (* macOS: Use host_statistics API via C bindings *)
      try
        let user_ticks, system_ticks, idle_ticks, nice_ticks =
          Cpu_bindings.get_cpu_load ()
        in
        let user_total = Int64.to_float user_ticks +. Int64.to_float nice_ticks in
        let system_total = Int64.to_float system_ticks in
        let idle_total = Int64.to_float idle_ticks in
        let total = user_total +. system_total +. idle_total in
        let current = (user_total, system_total, idle_total, total) in
        match !prev_cpu_counters with
        | Some (pu, ps, pi, pt) ->
            let du = user_total -. pu in
            let ds = system_total -. ps in
            let di = idle_total -. pi in
            let dt = total -. pt in
            if dt > 0. then (
              prev_cpu_counters := Some current;
              Some
                {
                  user = (du /. dt) *. 100.;
                  system = (ds /. dt) *. 100.;
                  idle = (di /. dt) *. 100.;
                })
            else (
              prev_cpu_counters := Some current;
              None)
        | None ->
            prev_cpu_counters := Some current;
            Some { user = 0.0; system = 0.0; idle = 100.0 }
      with _ -> None
  with _ -> None

let get_memory_usage () : memory_stats option =
  try
    if Sys.file_exists "/proc/meminfo" then (
      (* Linux: Read /proc/meminfo directly *)
      let ic = open_in "/proc/meminfo" in
      let mem_total = ref 0.0 in
      let mem_available = ref 0.0 in
      let mem_free = ref 0.0 in
      try
        while true do
          let line = input_line ic in
          if String.length line > 0 then (
            if String.sub line 0 9 = "MemTotal:" then (
              let parts = String.split_on_char ' ' line in
              let parts = List.filter (fun s -> s <> "") parts in
              match parts with
              | _ :: value :: unit :: _ ->
                  let value_f = float_of_string value in
                  mem_total :=
                    (if unit = "kB" then value_f *. 1024.
                     else if unit = "MB" then value_f *. 1024. *. 1024.
                     else if unit = "GB" then value_f *. 1024. *. 1024. *. 1024.
                     else value_f)
              | _ -> ());
            if String.sub line 0 13 = "MemAvailable:" then (
              let parts = String.split_on_char ' ' line in
              let parts = List.filter (fun s -> s <> "") parts in
              match parts with
              | _ :: value :: unit :: _ ->
                  let value_f = float_of_string value in
                  mem_available :=
                    (if unit = "kB" then value_f *. 1024.
                     else if unit = "MB" then value_f *. 1024. *. 1024.
                     else if unit = "GB" then value_f *. 1024. *. 1024. *. 1024.
                     else value_f)
              | _ -> ());
            if String.sub line 0 8 = "MemFree:" then (
              let parts = String.split_on_char ' ' line in
              let parts = List.filter (fun s -> s <> "") parts in
              match parts with
              | _ :: value :: unit :: _ ->
                  let value_f = float_of_string value in
                  mem_free :=
                    (if unit = "kB" then value_f *. 1024.
                     else if unit = "MB" then value_f *. 1024. *. 1024.
                     else if unit = "GB" then value_f *. 1024. *. 1024. *. 1024.
                     else value_f)
              | _ -> ())
          )
        done;
        assert false (* Should not reach here *)
      with End_of_file ->
        close_in ic;
        let total_bytes = !mem_total in
        let available_bytes = if !mem_available > 0. then !mem_available else !mem_free in
        let used_bytes = total_bytes -. available_bytes in
        let total_gb = total_bytes /. (1024. *. 1024. *. 1024.) in
        let used_gb = used_bytes /. (1024. *. 1024. *. 1024.) in
        let used_percent = if total_bytes > 0. then (used_bytes /. total_bytes) *. 100. else 0.0 in
        Some { total_gb; used_gb; used_percent })
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
        let total_bytes = float_of_string total_bytes_str in
        (* Get memory stats from vm_stat *)
        let cmd_vm = "vm_stat 2>/dev/null" in
        let ic_vm = Unix.open_process_in cmd_vm in
        let pages_free = ref 0.0 in
        let pages_active = ref 0.0 in
        let pages_inactive = ref 0.0 in
        let pages_speculative = ref 0.0 in
        let page_size = 16384.0 in (* macOS page size *)
        try
          while true do
            let line = input_line ic_vm in
            if String.length line > 0 then (
              if String.sub line 0 11 = "Pages free:" then (
                let re = Str.regexp "Pages free:[ ]+\\([0-9]+\\)\\." in
                if Str.string_match re line 0 then
                  pages_free := float_of_string (Str.matched_group 1 line));
              if String.sub line 0 13 = "Pages active:" then (
                let re = Str.regexp "Pages active:[ ]+\\([0-9]+\\)\\." in
                if Str.string_match re line 0 then
                  pages_active := float_of_string (Str.matched_group 1 line));
              if String.sub line 0 15 = "Pages inactive:" then (
                let re = Str.regexp "Pages inactive:[ ]+\\([0-9]+\\)\\." in
                if Str.string_match re line 0 then
                  pages_inactive := float_of_string (Str.matched_group 1 line));
              if String.sub line 0 18 = "Pages speculative:" then (
                let re = Str.regexp "Pages speculative:[ ]+\\([0-9]+\\)\\." in
                if Str.string_match re line 0 then
                  pages_speculative := float_of_string (Str.matched_group 1 line))
            )
          done;
          assert false
        with End_of_file ->
          (try Unix.close_process_in ic_vm |> ignore with _ -> ());
          let used_bytes =
            total_bytes
            -. (!pages_free *. page_size)
            -. (!pages_inactive *. page_size)
          in
          let total_gb = total_bytes /. (1024. *. 1024. *. 1024.) in
          let used_gb = used_bytes /. (1024. *. 1024. *. 1024.) in
          let used_percent = if total_bytes > 0. then (used_bytes /. total_bytes) *. 100. else 0.0 in
          Some { total_gb; used_gb; used_percent }
      with _ -> None
  with _ -> None

(* ---------- Model ---------- *)
type model = {
  cpu : cpu_stats;
  memory : memory_stats;
  sample_acc : float;
  sparkline_cpu : Charts.Sparkline.t;
  sparkline_memory : Charts.Sparkline.t;
}

type msg =
  | Quit
  | Tick of float

(* ---------- Init ---------- *)
let init () =
  let sparkline_cpu =
    Charts.Sparkline.create ~style:(Ansi.Style.make ~fg:Ansi.Color.cyan ())
      ~auto_max:false ~max_value:100. ~capacity:30 ()
  in
  let sparkline_memory =
    Charts.Sparkline.create ~style:(Ansi.Style.make ~fg:Ansi.Color.magenta ())
      ~auto_max:false ~max_value:100. ~capacity:30 ()
  in
  let cpu =
    Option.value
      ~default:{ user = 0.0; system = 0.0; idle = 0.0 }
      (get_cpu_usage ())
  in
  let memory =
    Option.value
      ~default:{ total_gb = 0.0; used_gb = 0.0; used_percent = 0.0 }
      (get_memory_usage ())
  in
  (* Push initial values to sparklines *)
  let total_cpu = cpu.user +. cpu.system in
  Charts.Sparkline.push sparkline_cpu total_cpu;
  Charts.Sparkline.push sparkline_memory memory.used_percent;
  ( {
      cpu;
      memory;
      sample_acc = 0.0;
      sparkline_cpu;
      sparkline_memory;
    },
    Cmd.none )

(* ---------- Update ---------- *)
let update msg m =
  match msg with
  | Quit -> (m, Cmd.quit)
  | Tick dt ->
      (* Sample at ~1Hz (every 1s) - slower to avoid overwhelming system *)
      let sample_acc = m.sample_acc +. dt in
      if sample_acc < 1.0 then
        ( {
            cpu = m.cpu;
            memory = m.memory;
            sample_acc;
            sparkline_cpu = m.sparkline_cpu;
            sparkline_memory = m.sparkline_memory;
          },
          Cmd.none )
      else
        (* Update CPU and Memory *)
        let cpu = Option.value ~default:m.cpu (get_cpu_usage ()) in
        let memory = Option.value ~default:m.memory (get_memory_usage ()) in
        (* Push values to sparklines *)
        let total_cpu = cpu.user +. cpu.system in
        Charts.Sparkline.push m.sparkline_cpu total_cpu;
        Charts.Sparkline.push m.sparkline_memory memory.used_percent;
        ( {
            cpu;
            memory;
            sample_acc = 0.0;
            sparkline_cpu = m.sparkline_cpu;
            sparkline_memory = m.sparkline_memory;
          },
          Cmd.none )

(* ---------- View ---------- *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let view model =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Header *)
      box ~padding:(padding 1) ~background:header_bg
        [
          box ~flex_direction:Row ~justify_content:Space_between
            ~align_items:Center
            ~size:{ width = pct 100; height = auto }
            [
              text ~text_style:(Ansi.Style.make ~bold:true ()) "▸ System Panel";
              text ~text_style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~padding:(padding 1)
        [
          scroll_box ~scroll_y:true ~scroll_x:false
            ~size:{ width = pct 100; height = pct 100 }
            [
              box ~flex_direction:Column ~align_items:Center ~gap:(gap 2)
                ~border:true ~border_color ~padding:(padding 3)
                ~size:{ width = px 70; height = auto }
                [
                  (* CPU Card *)
                  box ~border:true ~padding:(padding 2) ~title:"CPU Usage"
                    ~size:{ width = pct 100; height = auto }
                    [
                      box ~flex_direction:Row ~gap:(gap 2)
                        ~size:{ width = pct 100; height = auto }
                        [
                          (* Left: CPU Metrics *)
                          box ~flex_direction:Column ~gap:(gap 2)
                            ~size:{ width = px 25; height = auto }
                            [
                              (* User CPU *)
                              box ~flex_direction:Row ~justify_content:Space_between
                                ~align_items:Center
                                [
                                  text ~text_style:muted "User:";
                                  text
                                    ~text_style:
                                      (Ansi.Style.make ~bold:true ~fg:Ansi.Color.cyan ())
                                    (Printf.sprintf "%.1f%%" model.cpu.user);
                                ];
                              (* System CPU *)
                              box ~flex_direction:Row ~justify_content:Space_between
                                ~align_items:Center
                                [
                                  text ~text_style:muted "System:";
                                  text
                                    ~text_style:
                                      (Ansi.Style.make ~bold:true ~fg:Ansi.Color.magenta ())
                                    (Printf.sprintf "%.1f%%" model.cpu.system);
                                ];
                              (* Idle CPU *)
                              box ~flex_direction:Row ~justify_content:Space_between
                                ~align_items:Center
                                [
                                  text ~text_style:muted "Idle:";
                                  text
                                    ~text_style:
                                      (Ansi.Style.make ~bold:true ~fg:Ansi.Color.green ())
                                    (Printf.sprintf "%.1f%%" model.cpu.idle);
                                ];
                            ];
                          (* Right: CPU Usage Graph *)
                          box ~flex_direction:Column ~gap:(gap 1)
                            ~flex_grow:1.
                            [
                              text ~text_style:muted "CPU Load:";
                              canvas
                                ~draw:(fun canvas ~width ~height ->
                                  Charts.Sparkline.draw model.sparkline_cpu ~kind:`Bars
                                    canvas ~width ~height)
                                ~size:{ width = pct 100; height = px 8 }
                                ();
                            ];
                        ];
                    ];
                  (* Memory Card *)
                  box ~border:true ~padding:(padding 2) ~title:"Memory Usage"
                    ~size:{ width = pct 100; height = auto }
                    [
                      box ~flex_direction:Row ~gap:(gap 2)
                        ~size:{ width = pct 100; height = auto }
                        [
                          (* Left: Memory Metrics *)
                          box ~flex_direction:Column ~gap:(gap 2)
                            ~size:{ width = px 25; height = auto }
                            [
                              (* Total Memory *)
                              box ~flex_direction:Row ~justify_content:Space_between
                                ~align_items:Center
                                [
                                  text ~text_style:muted "Total:";
                                  text
                                    ~text_style:
                                      (Ansi.Style.make ~bold:true ~fg:Ansi.Color.white ())
                                    (Printf.sprintf "%.1f GB" model.memory.total_gb);
                                ];
                              (* Used Memory *)
                              box ~flex_direction:Row ~justify_content:Space_between
                                ~align_items:Center
                                [
                                  text ~text_style:muted "Used:";
                                  text
                                    ~text_style:
                                      (Ansi.Style.make ~bold:true ~fg:Ansi.Color.magenta ())
                                    (Printf.sprintf "%.1f GB" model.memory.used_gb);
                                ];
                              (* Usage Percentage *)
                              box ~flex_direction:Row ~justify_content:Space_between
                                ~align_items:Center
                                [
                                  text ~text_style:muted "Usage:";
                                  text
                                    ~text_style:
                                      (Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow ())
                                    (Printf.sprintf "%.1f%%" model.memory.used_percent);
                                ];
                            ];
                          (* Right: Memory Usage Graph *)
                          box ~flex_direction:Column ~gap:(gap 1)
                            ~flex_grow:1.
                            [
                              text ~text_style:muted "Memory Load:";
                              canvas
                                ~draw:(fun canvas ~width ~height ->
                                  Charts.Sparkline.draw model.sparkline_memory ~kind:`Bars
                                    canvas ~width ~height)
                                ~size:{ width = pct 100; height = px 8 }
                                ();
                            ];
                        ];
                    ];
                ];
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~text_style:hint "q quit" ];
    ]

(* ---------- Subscriptions ---------- *)
let subscriptions _model =
  Sub.batch
    [
      Sub.on_tick (fun ~dt -> Tick dt);
      Sub.on_key (fun ev ->
          match (Mosaic_ui.Event.Key.data ev).key with
          | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
          | Escape -> Some Quit
          | _ -> None);
    ]

let () = run { init; update; view; subscriptions }