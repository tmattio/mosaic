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
  cpu_prev : Metrics.Cpu.sample_result option;
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
      ~default:{ total_gb = 0.0; used_gb = 0.0; used_percent = 0.0 }
      (Metrics.Mem.sample ())
  in
  let disk =
    Option.value
      ~default:{ total_gb = 0.0; used_gb = 0.0; avail_gb = 0.0; used_percent = 0.0 }
      (Metrics.Disk.sample ())
  in
  (* Push initial values to sparklines *)
  let total_cpu = cpu.user +. cpu.system in
  Charts.Sparkline.push sparkline_cpu total_cpu;
  Charts.Sparkline.push sparkline_memory memory.used_percent;
  Charts.Sparkline.push sparkline_disk disk.used_percent;
  ( {
      cpu;
      cpu_per_core;
      memory;
      disk;
      cpu_prev;
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
      (* Sample at ~1Hz (every 1s) *)
      let sample_acc = m.sample_acc +. dt in
      if sample_acc < 1.0 then
        ( {
            cpu = m.cpu;
            cpu_per_core = m.cpu_per_core;
            memory = m.memory;
            disk = m.disk;
            cpu_prev = m.cpu_prev;
            sample_acc;
            sparkline_cpu = m.sparkline_cpu;
            sparkline_memory = m.sparkline_memory;
            sparkline_disk = m.sparkline_disk;
          },
          Cmd.none )
      else
        (* Update CPU, Memory, and Disk *)
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
            cpu_prev;
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
      box ~flex_grow:1. ~padding:(padding 1)
        [
          box ~flex_direction:Row ~gap:(gap 1)
            ~size:{ width = pct 100; height = pct 100 }
            [
              (* Left: CPU Column (merged - spans full height) *)
              box ~border:true ~padding:(padding 1) ~title:"CPU Usage"
                ~flex_grow:1.
                ~size:{ width = pct 50; height = pct 100 }
                [
                  box ~flex_direction:Column ~gap:(gap 1)
                    ~size:{ width = pct 100; height = pct 100 }
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
                           ~flex_grow:1.
                           ~size:{ width = pct 100; height = pct 100 }
                           [
                             text ~text_style:muted "Per-Core Usage:";
                             scroll_box ~scroll_y:true ~scroll_x:false
                               ~size:{ width = pct 100; height = pct 100 }
                               (Array.to_list
                                  (Array.mapi
                                     (fun i (core_stats : Metrics.cpu_stats) ->
                                       let total_usage = core_stats.user +. core_stats.system in
                                       box
                                         ~key:(Printf.sprintf "core-%d" i)
                                         ~padding:(padding 1)
                                         ~background:
                                           (if i mod 2 = 0 then Ansi.Color.default
                                            else Ansi.Color.grayscale ~level:3)
                                         [
                                           box ~flex_direction:Row ~justify_content:Space_between
                                             ~align_items:Center
                                             ~size:{ width = pct 100; height = auto }
                                             [
                                               text
                                                 ~text_style:
                                                   (Ansi.Style.make
                                                      ~fg:
                                                        (if total_usage > 80. then Ansi.Color.red
                                                         else if total_usage > 50. then Ansi.Color.yellow
                                                         else Ansi.Color.green)
                                                      ())
                                                 (Printf.sprintf "Core %d" i);
                                               text
                                                 ~text_style:
                                                   (Ansi.Style.make ~bold:true
                                                      ~fg:
                                                        (if total_usage > 80. then Ansi.Color.red
                                                         else if total_usage > 50. then Ansi.Color.yellow
                                                         else Ansi.Color.green)
                                                      ())
                                                 (Printf.sprintf "%.1f%%" total_usage);
                                             ];
                                         ])
                                     model.cpu_per_core))
                           ]
                       else box ~size:{ width = pct 100; height = px 0 } []);
                    ];
                ];
              (* Right: Memory and Disk Column *)
              box ~flex_direction:Column ~gap:(gap 1)
                ~flex_grow:1.
                ~size:{ width = pct 50; height = pct 100 }
                [
                  (* Top: Memory *)
                  box ~border:true ~padding:(padding 1) ~title:"Memory Usage"
                    ~size:{ width = pct 100; height = pct 50 }
                    [
                      box ~flex_direction:Row ~gap:(gap 3)
                        ~size:{ width = pct 100; height = pct 100 }
                        [
                          (* Left: Memory Metrics *)
                          box ~flex_direction:Column ~gap:(gap 1)
                            ~size:{ width = pct 55; height = auto }
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
                            ~size:{ width = pct 45; height = auto }
                            [
                              text ~text_style:muted "Memory Load:";
                              canvas
                                ~draw:(fun canvas ~width ~height ->
                                  Charts.Sparkline.draw model.sparkline_memory ~kind:`Braille
                                    canvas ~width ~height)
                                ~size:{ width = pct 100; height = px 8 }
                                ();
                            ];
                        ];
                    ];
                  (* Bottom: Disk *)
                  box ~border:true ~padding:(padding 1) ~title:"Disk Usage"
                    ~size:{ width = pct 100; height = pct 50 }
                    [
                      box ~flex_direction:Row ~gap:(gap 3)
                        ~size:{ width = pct 100; height = pct 100 }
                        [
                          (* Left: Disk Metrics *)
                          box ~flex_direction:Column ~gap:(gap 1)
                            ~size:{ width = pct 55; height = auto }
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
                            ];
                          (* Right: Disk Usage Graph *)
                          box ~flex_direction:Column ~gap:(gap 1)
                            ~size:{ width = pct 45; height = auto }
                            [
                              text ~text_style:muted "Disk Load:";
                              canvas
                                ~draw:(fun canvas ~width ~height ->
                                  Charts.Sparkline.draw model.sparkline_disk ~kind:`Braille
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

