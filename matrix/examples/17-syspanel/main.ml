(** System metrics panel UI.
    Uses Metrics library for data collection. *)
open Mosaic_tea
module Charts = Matrix_charts
module Metrics = Metrics

(* ---------- Model ---------- *)
type model = {
  cpu : Metrics.cpu_stats;
  cpu_per_core : Metrics.cpu_stats array;
  memory : Metrics.memory_stats;
  disk : Metrics.disk_stats;
  process : Metrics.process_stats;
  processes : Metrics.process_info list;
  cpu_prev : Metrics.Cpu.sample_result option;
  proc_prev_utime : float option;
  proc_prev_stime : float option;
  sample_acc : float;
  sparkline_cpu : Charts.Sparkline.t;
  sparkline_memory : Charts.Sparkline.t;
  sparkline_disk : Charts.Sparkline.t;
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
  let sparkline_disk =
    Charts.Sparkline.create ~style:(Ansi.Style.make ~fg:Ansi.Color.yellow ())
      ~auto_max:false ~max_value:100. ~capacity:30 ()
  in
  (* Get initial CPU sample *)
  let cpu_prev = Metrics.Cpu.sample_all () in
  let cpu, cpu_per_core =
    match cpu_prev with
    | Some prev ->
        (* Wait a tiny bit and sample again for initial delta *)
        Unix.sleepf 0.1;
        (match Metrics.Cpu.sample_all () with
        | Some next -> (
            match Metrics.Cpu.usage ~prev:prev.total ~next:next.total with
            | Some stats ->
                ( stats,
    Option.value
                    ~default:[||]
                    (Metrics.Cpu.usage_per_core ~prev ~next) )
            | None ->
                ( ({ user = 0.0; system = 0.0; idle = 100.0 } : Metrics.cpu_stats),
                  Array.make (Array.length prev.per_core)
                    ({ user = 0.0; system = 0.0; idle = 100.0 } : Metrics.cpu_stats) ))
        | None ->
            ( ({ user = 0.0; system = 0.0; idle = 100.0 } : Metrics.cpu_stats),
              (match cpu_prev with
              | Some p -> Array.make (Array.length p.per_core) ({ user = 0.0; system = 0.0; idle = 100.0 } : Metrics.cpu_stats)
              | None -> [||]) ))
    | None -> (({ user = 0.0; system = 0.0; idle = 100.0 } : Metrics.cpu_stats), [||])
  in
  let memory =
    Option.value
      ~default:{ total_gb = 0.0; used_gb = 0.0; used_percent = 0.0; swap_total_gb = 0.0; swap_used_gb = 0.0; swap_used_percent = 0.0 }
      (Metrics.Mem.sample ())
  in
  let disk =
    Option.value
      ~default:{ total_gb = 0.0; used_gb = 0.0; avail_gb = 0.0; used_percent = 0.0; partitions = [] }
      (Metrics.Disk.sample ())
  in
  (* Get initial process sample *)
  let t = Unix.times () in
  let proc_utime = t.Unix.tms_utime in
  let proc_stime = t.Unix.tms_stime in
  (* Get number of CPU cores for normalization *)
  let num_cores = Array.length cpu_per_core in
  let process =
    Option.value
      ~default:{ cpu_percent = 0.0; rss_mb = 0.0; vsize_mb = 0.0 }
      (Metrics.Proc.sample
         ~prev_utime:None ~prev_stime:None ~dt:0.2
         ~num_cores:(if num_cores > 0 then Some num_cores else None)
         ())
  in
  (* Push initial values to sparklines *)
  let total_cpu = cpu.user +. cpu.system in
  Charts.Sparkline.push sparkline_cpu total_cpu;
  Charts.Sparkline.push sparkline_memory memory.used_percent;
  Charts.Sparkline.push sparkline_disk disk.used_percent;
  let processes = Metrics.Processes.read_top_processes ~limit:10 ~sort_by:`Cpu () in
  ( {
      cpu;
      cpu_per_core;
      memory;
      disk;
      process;
      processes;
      cpu_prev;
      proc_prev_utime = Some proc_utime;
      proc_prev_stime = Some proc_stime;
      sample_acc = 0.0;
      sparkline_cpu;
      sparkline_memory;
      sparkline_disk;
    },
    Cmd.none )

(* ---------- Update ---------- *)
let update msg m =
  match msg with
  | Quit -> (m, Cmd.quit)
  | Tick dt ->
      (* Sample at ~5Hz (every 0.2s) *)
      let sample_acc = m.sample_acc +. dt in
      if sample_acc < 0.2 then
        ( {
            cpu = m.cpu;
            cpu_per_core = m.cpu_per_core;
            memory = m.memory;
            disk = m.disk;
            process = m.process;
            processes = m.processes;
            cpu_prev = m.cpu_prev;
            proc_prev_utime = m.proc_prev_utime;
            proc_prev_stime = m.proc_prev_stime;
            sample_acc;
            sparkline_cpu = m.sparkline_cpu;
            sparkline_memory = m.sparkline_memory;
            sparkline_disk = m.sparkline_disk;
          },
          Cmd.none )
      else
        (* Update CPU, Memory, Disk, and Process *)
        let cpu, cpu_per_core, cpu_prev =
          match m.cpu_prev with
          | Some prev -> (
              match Metrics.Cpu.sample_all () with
              | Some next -> (
                  match Metrics.Cpu.usage ~prev:prev.total ~next:next.total with
                  | Some stats ->
                      ( stats,
                        Option.value
                          ~default:m.cpu_per_core
                          (Metrics.Cpu.usage_per_core ~prev ~next),
                        Some next )
                  | None -> (m.cpu, m.cpu_per_core, Some next))
              | None -> (m.cpu, m.cpu_per_core, m.cpu_prev))
          | None -> (
              match Metrics.Cpu.sample_all () with
              | Some next -> (m.cpu, m.cpu_per_core, Some next)
              | None -> (m.cpu, m.cpu_per_core, None))
        in
        let memory =
          Option.value ~default:m.memory (Metrics.Mem.sample ())
        in
        let disk =
          Option.value ~default:m.disk (Metrics.Disk.sample ())
        in
        (* Update process metrics *)
        let t = Unix.times () in
        let proc_utime = t.Unix.tms_utime in
        let proc_stime = t.Unix.tms_stime in
        (* Get number of CPU cores for normalization *)
        let num_cores = Array.length m.cpu_per_core in
        let process =
          Option.value
            ~default:m.process
            (Metrics.Proc.sample
               ~prev_utime:m.proc_prev_utime ~prev_stime:m.proc_prev_stime
               ~dt:sample_acc
               ~num_cores:(if num_cores > 0 then Some num_cores else None)
               ())
        in
        (* Update process list *)
        let processes = Metrics.Processes.read_top_processes ~limit:10 ~sort_by:`Cpu () in
        (* Push values to sparklines *)
        let total_cpu = cpu.user +. cpu.system in
        Charts.Sparkline.push m.sparkline_cpu total_cpu;
        Charts.Sparkline.push m.sparkline_memory memory.used_percent;
        Charts.Sparkline.push m.sparkline_disk disk.used_percent;
        ( {
            cpu;
            cpu_per_core;
            memory;
            disk;
            process;
            processes;
            cpu_prev;
            proc_prev_utime = Some proc_utime;
            proc_prev_stime = Some proc_stime;
            sample_acc = 0.0;
            sparkline_cpu = m.sparkline_cpu;
            sparkline_memory = m.sparkline_memory;
            sparkline_disk = m.sparkline_disk;
          },
          Cmd.none )

(* ---------- View ---------- *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

(* Helper function to draw a horizontal progress bar *)
let draw_progress_bar grid ~width ~height ~value ~max_value ~fill_color =
  let bar_width = width in
  let bar_height = height in
  if bar_width > 0 && bar_height > 0 then (
    (* Draw background *)
    Grid.fill_rect grid ~x:0 ~y:0 ~width:bar_width ~height:bar_height
      ~color:(Ansi.Color.grayscale ~level:3);
    (* Draw filled portion *)
    let filled_width = int_of_float (value /. max_value *. float_of_int bar_width) in
    let filled_width = max 0 (min bar_width filled_width) in
    if filled_width > 0 then
      Grid.fill_rect grid ~x:0 ~y:0 ~width:filled_width ~height:bar_height
        ~color:fill_color)

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
      (* Content - Merged CPU Column Layout *)
      scroll_box ~scroll_y:true ~scroll_x:false ~flex_grow:1.
        ~size:{ width = pct 100; height = pct 100 }
        [
          box ~padding:(padding 1)
            ~size:{ width = pct 100; height = auto }
            [
              box ~flex_direction:Row ~gap:(gap 1)
                ~size:{ width = pct 100; height = auto }
                [
              (* Left: CPU Column (merged - spans full height) *)
              box ~flex_direction:Column ~gap:(gap 1)
                ~size:{ width = pct 50; height = auto }
                [
                  (* CPU Usage Box *)
                  box ~border:true ~padding:(padding 1) ~title:"CPU Usage"
                    ~background:Ansi.Color.default
                    ~size:{ width = pct 100; height = auto }
                    [
                      box ~flex_direction:Column ~gap:(gap 0)
                        ~size:{ width = pct 100; height = auto }
                        [
                          (* Top: Overall CPU Metrics and Graph *)
                          box ~flex_direction:Row ~gap:(gap 3)
                            ~size:{ width = pct 100; height = auto }
                            [
                              (* Left: CPU Metrics *)
                              box ~flex_direction:Column ~gap:(gap 1)
                                ~size:{ width = pct 55; height = auto }
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
                                ~size:{ width = pct 45; height = auto }
                                [
                                  text ~text_style:muted "CPU Load:";
                                  canvas
                                    ~draw:(fun canvas ~width ~height ->
                                      Charts.Sparkline.draw model.sparkline_cpu ~kind:`Braille
                                        canvas ~width ~height)
                                    ~size:{ width = pct 100; height = px 8 }
                                    ();
                                ];
                            ];
                          (* Bottom: Per-Core CPU Usage *)
                          (if Array.length model.cpu_per_core > 0 then
                             box ~flex_direction:Column ~gap:(gap 1)
                               ~size:{ width = pct 100; height = auto }
                               [
                                 text ~text_style:muted "Per-Core Usage:";
                                 scroll_box ~scroll_y:true ~scroll_x:false
                                   ~size:{ width = pct 100; height = px 12 }
                                   (let cores = Array.to_list (Array.mapi (fun i (stats : Metrics.cpu_stats) -> (i, stats)) model.cpu_per_core) in
                                    (* Group cores into pairs for rows *)
                                    let rec chunk_pairs = function
                                      | [] -> []
                                      | [x] -> [[x]]
                                      | x :: y :: rest -> [x; y] :: chunk_pairs rest
                                    in
                                    let rows = chunk_pairs cores in
                                    List.mapi
                                      (fun row_idx row ->
                                        box
                                          ~key:(Printf.sprintf "row-%d" row_idx)
                                          ~flex_direction:Row ~gap:(gap 1)
                                          ~size:{ width = pct 100; height = auto }
                                          (List.mapi
                                             (fun _col_idx (i, (core_stats : Metrics.cpu_stats)) ->
                                               let total_usage = core_stats.user +. core_stats.system in
                                               let bar_color =
                                                 if total_usage > 80. then Ansi.Color.red
                                                 else if total_usage > 50. then Ansi.Color.yellow
                                                 else Ansi.Color.green
                                               in
                                               box
                                                 ~key:(Printf.sprintf "core-%d" i)
                                                 ~padding:(padding 1)
                                                 ~background:
                                                   (if i mod 2 = 0 then Ansi.Color.default
                                                    else Ansi.Color.grayscale ~level:3)
                                                 ~size:{ width = pct 50; height = auto }
                                                 [
                                                   box ~flex_direction:Column ~gap:(gap 0)
                                                     ~size:{ width = pct 100; height = auto }
                                                     [
                                                       (* Core label and percentage *)
                                                       box ~flex_direction:Row ~justify_content:Space_between
                                                         ~align_items:Center
                                                         ~size:{ width = pct 100; height = auto }
                                                         [
                                                           text
                                                             ~text_style:
                                                               (Ansi.Style.make ~fg:bar_color ())
                                                             (string_of_int i);
                                                           text
                                                             ~text_style:
                                                               (Ansi.Style.make ~bold:true ~fg:bar_color ())
                                                             (Printf.sprintf "%.1f%%" total_usage);
                                                         ];
                                                       (* Progress bar *)
                                                       canvas
                                                         ~draw:(fun grid ~width ~height ->
                                                           draw_progress_bar grid ~width ~height
                                                             ~value:total_usage ~max_value:100.0
                                                             ~fill_color:bar_color)
                                                         ~size:{ width = pct 100; height = px 1 }
                                                         ();
                                                     ];
                                                 ])
                                             row))
                                      rows)
                               ]
                           else box ~size:{ width = pct 100; height = px 0 } []);
                        ];
                    ];
                  (* Top Processes Box - separate sibling box *)
                  box ~border:true ~padding:(padding 1) ~title:"Top Processes"
                    ~size:{ width = pct 100; height = auto }
                    [
                      (if List.length model.processes > 0 then
                         scroll_box ~scroll_y:true ~scroll_x:false
                           ~size:{ width = pct 100; height = px 10 }
                           (List.mapi
                              (fun i (proc : Metrics.process_info) ->
                                box
                                  ~key:(Printf.sprintf "process-%d" proc.pid)
                                  ~padding:(padding 1)
                                  ~background:
                                    (if i mod 2 = 0 then Ansi.Color.default
                                     else Ansi.Color.grayscale ~level:3)
                                  [
                                    box ~flex_direction:Row ~justify_content:Space_between
                                      ~align_items:Center
                                      ~size:{ width = pct 100; height = auto }
                                      [
                                        (* Left: Process name and PID *)
                                        box ~flex_direction:Column ~gap:(gap 0)
                                          ~size:{ width = pct 50; height = auto }
                                          [
                                            (* Process name with potential wrapping *)
                                            box ~size:{ width = pct 100; height = auto }
                                              [
                                                text
                                                  ~text_style:
                                                    (Ansi.Style.make ~bold:true
                                                       ~fg:Ansi.Color.white ())
                                                  proc.name;
                                              ];
                                            text
                                              ~text_style:muted
                                              (Printf.sprintf "PID: %d" proc.pid);
                                          ];
                                        (* Right: CPU usage *)
                                        text
                                          ~text_style:
                                            (Ansi.Style.make ~bold:true
                                               ~fg:Ansi.Color.cyan ())
                                          (Printf.sprintf "%.1f%%" proc.cpu_percent);
                                      ];
                                  ])
                              model.processes)
                       else
                         box ~flex_direction:Row ~justify_content:Center
                           ~align_items:Center
                           ~padding:(padding 1)
                           [
                             text ~text_style:muted "no processes found";
                           ]);
                    ];
                ];
              (* Right: Memory, Disk, and Process Column *)
              box ~flex_direction:Column ~gap:(gap 1)
                ~flex_grow:1.
                ~background:Ansi.Color.default
                ~size:{ width = pct 50; height = auto }
                [
                  (* Top: Memory *)
                  box ~border:true ~padding:(padding 1) ~title:"Memory Usage"
                    ~size:{ width = pct 100; height = auto }
                    [
                      box ~flex_direction:Row ~gap:(gap 3)
                        ~size:{ width = pct 100; height = pct 100 }
                        [
                          (* Left: Memory Metrics *)
                          box ~flex_direction:Column ~gap:(gap 1)
                            ~size:{ width = pct 70; height = auto }
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
                              (* Used Memory with inline percentage *)
                              box ~flex_direction:Row ~justify_content:Space_between
                                ~align_items:Center
                                [
                                  text ~text_style:muted "Used:";
                                  box ~flex_direction:Row ~gap:(gap 0) ~align_items:Center
                                    [
                                      text
                                        ~text_style:
                                          (Ansi.Style.make ~bold:true ~fg:Ansi.Color.magenta ())
                                        (Printf.sprintf "%.1f GB" model.memory.used_gb);
                                      text
                                        ~text_style:
                                          (Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow ())
                                        (Printf.sprintf " (%.1f%%)" model.memory.used_percent);
                                    ];
                                ];
                              (* Swap Used with inline percentage *)
                              box ~flex_direction:Row ~justify_content:Space_between
                                ~align_items:Center
                                [
                                  text ~text_style:muted "Swap Used:";
                                  box ~flex_direction:Row ~gap:(gap 0) ~align_items:Center
                                    [
                                      text
                                        ~text_style:
                                          (Ansi.Style.make ~bold:true ~fg:Ansi.Color.magenta ())
                                        (Printf.sprintf "%.1f GB" model.memory.swap_used_gb);
                                      text
                                        ~text_style:
                                          (Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow ())
                                        (Printf.sprintf " (%.1f%%)" model.memory.swap_used_percent);
                                    ];
                                ];
                            ];
                          (* Right: Memory Usage Graph *)
                          box ~flex_direction:Column ~gap:(gap 1)
                            ~size:{ width = pct 30; height = auto }
                            [
                              text ~text_style:muted "Memory Load:";
                              canvas
                                ~draw:(fun canvas ~width ~height ->
                                  Charts.Sparkline.draw model.sparkline_memory ~kind:`Braille
                                    canvas ~width ~height)
                                ~size:{ width = pct 100; height = px 4 }
                                ();
                            ];
                        ];
                    ];
                  (* Middle: Disk *)
                  box ~border:true ~padding:(padding 1) ~title:"Disk Usage"
                    ~size:{ width = pct 100; height = auto }
                    [
                      (* Disk Metrics *)
                      box ~flex_direction:Column ~gap:(gap 1)
                        ~size:{ width = pct 100; height = auto }
                        [
                          (* Total Disk *)
                          box ~flex_direction:Row ~justify_content:Space_between
                            ~align_items:Center
                            [
                              text ~text_style:muted "Total:";
                              text
                                ~text_style:
                                  (Ansi.Style.make ~bold:true ~fg:Ansi.Color.white ())
                                (Printf.sprintf "%.1f GB" model.disk.total_gb);
                            ];
                          (* Used Disk *)
                          box ~flex_direction:Row ~justify_content:Space_between
                            ~align_items:Center
                            [
                              text ~text_style:muted "Used:";
                              text
                                ~text_style:
                                  (Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow ())
                                (Printf.sprintf "%.1f GB" model.disk.used_gb);
                            ];
                          (* Available Disk *)
                          box ~flex_direction:Row ~justify_content:Space_between
                            ~align_items:Center
                            [
                              text ~text_style:muted "Avail:";
                              text
                                ~text_style:
                                  (Ansi.Style.make ~bold:true ~fg:Ansi.Color.green ())
                                (Printf.sprintf "%.1f GB" model.disk.avail_gb);
                            ];
                          (* Usage Percentage *)
                          box ~flex_direction:Row ~justify_content:Space_between
                            ~align_items:Center
                            [
                              text ~text_style:muted "Usage:";
                              text
                                ~text_style:
                                  (Ansi.Style.make ~bold:true ~fg:Ansi.Color.yellow ())
                                (Printf.sprintf "%.1f%%" model.disk.used_percent);
                            ];
                          (* Partitions *)
                          (if List.length model.disk.partitions > 0 then
                             box ~flex_direction:Column ~gap:(gap 1)
                               ~size:{ width = pct 100; height = auto }
                               [
                                 text ~text_style:muted "Partitions:";
                                 scroll_box ~scroll_y:true ~scroll_x:false
                                   ~size:{ width = pct 100; height = px 8 }
                                   (List.mapi
                                      (fun i (part : Metrics.disk_partition) ->
                                        box
                                          ~key:(Printf.sprintf "partition-%d" i)
                                          ~padding:(padding 1)
                                          ~background:
                                            (if i mod 2 = 0 then Ansi.Color.default
                                             else Ansi.Color.grayscale ~level:3)
                                          [
                                            box ~flex_direction:Row ~justify_content:Space_between
                                              ~align_items:Center
                                              ~size:{ width = pct 100; height = auto }
                                              [
                                                (* Left: Mount point *)
                                                text
                                                  ~text_style:
                                                    (Ansi.Style.make ~bold:true
                                                       ~fg:Ansi.Color.white ())
                                                  part.mount_point;
                                                (* Right: Used and percentage *)
                                                box ~flex_direction:Row ~gap:(gap 1)
                                                  ~align_items:Center
                                                  [
                                                    text
                                                      ~text_style:
                                                        (Ansi.Style.make ~bold:true
                                                           ~fg:Ansi.Color.yellow ())
                                                      (Printf.sprintf "%.1f GB" part.used_gb);
                                                    text
                                                      ~text_style:
                                                        (Ansi.Style.make ~bold:true
                                                           ~fg:Ansi.Color.yellow ())
                                                      (Printf.sprintf "(%.1f%%)" part.used_percent);
                                                  ];
                                              ];
                                          ])
                                      model.disk.partitions)
                               ]
                           else
                             box ~flex_direction:Row ~justify_content:Center
                               ~align_items:Center
                               ~padding:(padding 1)
                               [
                                 text ~text_style:muted "no partitions found";
                               ]);
                        ];
                    ];
                  (* Bottom: Process (self) *)
                  box ~border:true ~padding:(padding 1) ~title:"Process (self)"
                    ~size:{ width = pct 100; height = auto }
                    [
                      (* Process Metrics *)
                      box ~flex_direction:Column ~gap:(gap 1)
                        ~size:{ width = pct 100; height = auto }
                        [
                          (* CPU Usage *)
                          box ~flex_direction:Row ~justify_content:Space_between
                            ~align_items:Center
                            [
                              text ~text_style:muted "CPU:";
                              text
                                ~text_style:
                                  (Ansi.Style.make ~bold:true ~fg:Ansi.Color.cyan ())
                                (Printf.sprintf "%.1f%%" model.process.cpu_percent);
                            ];
                          (* RSS Memory *)
                          box ~flex_direction:Row ~justify_content:Space_between
                            ~align_items:Center
                            [
                              text ~text_style:muted "RSS:";
                              text
                                ~text_style:
                                  (Ansi.Style.make ~bold:true ~fg:Ansi.Color.magenta ())
                                (Printf.sprintf "%.1f MB" model.process.rss_mb);
                            ];
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

