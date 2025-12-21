open Matrix
module Charts = Matrix_charts

type history = {
  cpu : Charts.Sparkline.t;
  mem : Charts.Sparkline.t;
  net_rx : Charts.Sparkline.t;
  net_tx : Charts.Sparkline.t;
}

type state = {
  cpu_prev : Sys_metrics.Cpu.t option;
  net_prev : Sys_metrics.Net.t option;
  last_sample : int64;
  history : history;
}

let history_capacity = 120

let create_history () =
  let make color =
    Charts.Sparkline.create
      ~style:(Ansi.Style.make ~fg:color ())
      ~capacity:history_capacity ~auto_max:true ()
  in
  {
    cpu = make Ansi.Color.bright_cyan;
    mem = make Ansi.Color.bright_green;
    net_rx = make Ansi.Color.bright_magenta;
    net_tx = make Ansi.Color.bright_yellow;
  }

let initial_state () =
  {
    cpu_prev = None;
    net_prev = None;
    last_sample = 0L;
    history = create_history ();
  }

let format_bytes bytes =
  let b = Int64.to_float bytes in
  if b >= 1099511627776. then Printf.sprintf "%.1f TB" (b /. 1099511627776.)
  else if b >= 1073741824. then Printf.sprintf "%.1f GB" (b /. 1073741824.)
  else if b >= 1048576. then Printf.sprintf "%.1f MB" (b /. 1048576.)
  else if b >= 1024. then Printf.sprintf "%.1f KB" (b /. 1024.)
  else Printf.sprintf "%.0f B" b

let format_rate rate =
  if rate >= 1073741824. then Printf.sprintf "%.1f GB/s" (rate /. 1073741824.)
  else if rate >= 1048576. then Printf.sprintf "%.1f MB/s" (rate /. 1048576.)
  else if rate >= 1024. then Printf.sprintf "%.1f KB/s" (rate /. 1024.)
  else Printf.sprintf "%.0f B/s" rate

let draw_box grid ~x ~y ~width ~height ~title =
  let border_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:10) () in
  let title_style = Ansi.Style.make ~fg:Ansi.Color.bright_white ~bold:true () in
  Grid.draw_text ~style:border_style grid ~x ~y ~text:"┌";
  for i = 1 to width - 2 do
    Grid.draw_text ~style:border_style grid ~x:(x + i) ~y ~text:"─"
  done;
  Grid.draw_text ~style:border_style grid ~x:(x + width - 1) ~y ~text:"┐";
  if String.length title > 0 then (
    let title_x = x + 2 in
    Grid.draw_text ~style:border_style grid ~x:title_x ~y ~text:" ";
    Grid.draw_text ~style:title_style grid ~x:(title_x + 1) ~y ~text:title;
    Grid.draw_text ~style:border_style grid
      ~x:(title_x + 1 + String.length title)
      ~y ~text:" "
  );
  for row = 1 to height - 2 do
    Grid.draw_text ~style:border_style grid ~x ~y:(y + row) ~text:"│";
    Grid.draw_text ~style:border_style grid ~x:(x + width - 1) ~y:(y + row) ~text:"│"
  done;
  Grid.draw_text ~style:border_style grid ~x ~y:(y + height - 1) ~text:"└";
  for i = 1 to width - 2 do
    Grid.draw_text ~style:border_style grid ~x:(x + i) ~y:(y + height - 1) ~text:"─"
  done;
  Grid.draw_text ~style:border_style grid ~x:(x + width - 1) ~y:(y + height - 1) ~text:"┘"

let draw_progress_bar grid ~x ~y ~width ~value ~max_value ~color ~label =
  let label_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let value_style = Ansi.Style.make ~fg:color ~bold:true () in
  let bar_width = width - 20 in
  let bar_width = max 5 bar_width in
  let pct = if max_value <= 0. then 0. else value /. max_value in
  let filled = int_of_float (pct *. float bar_width) in
  let filled = max 0 (min bar_width filled) in
  Grid.draw_text ~style:label_style grid ~x ~y ~text:(Printf.sprintf "%-10s" label);
  let bg_style = Ansi.Style.make ~bg:(Ansi.Color.grayscale ~level:3) () in
  for i = 0 to bar_width - 1 do
    Grid.draw_text ~style:bg_style grid ~x:(x + 11 + i) ~y ~text:" "
  done;
  let fill_style = Ansi.Style.make ~bg:color () in
  for i = 0 to filled - 1 do
    Grid.draw_text ~style:fill_style grid ~x:(x + 11 + i) ~y ~text:" "
  done;
  Grid.draw_text ~style:value_style grid ~x:(x + 12 + bar_width) ~y
    ~text:(Printf.sprintf "%5.1f%%" (pct *. 100.))

let draw_header grid ~cols =
  let title = "System Dashboard" in
  let title_style = Ansi.Style.make ~fg:Ansi.Color.bright_white ~bold:true () in
  Grid.draw_text ~style:title_style grid ~x:((cols - String.length title) / 2) ~y:0
    ~text:title;
  let help = "Q: quit" in
  let help_style = Ansi.Style.make ~fg:Ansi.Color.bright_black () in
  Grid.draw_text ~style:help_style grid ~x:(cols - String.length help - 2) ~y:0 ~text:help

let draw_cpu_panel grid ~x ~y ~width ~height state cpu_pct =
  draw_box grid ~x ~y ~width ~height ~title:"CPU";
  let cpu = Sys_metrics.Cpu.sample () in
  let num_cores = Array.length cpu.per_core in
  draw_progress_bar grid ~x:(x + 1) ~y:(y + 1) ~width:(width - 2) ~value:cpu_pct
    ~max_value:100. ~color:Ansi.Color.bright_cyan ~label:"Total";
  let label_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let value_style = Ansi.Style.make ~fg:Ansi.Color.bright_cyan () in
  Grid.draw_text ~style:label_style grid ~x:(x + 1) ~y:(y + 2) ~text:"Cores:";
  Grid.draw_text ~style:value_style grid ~x:(x + 12) ~y:(y + 2)
    ~text:(string_of_int num_cores);
  let chart_y = y + 4 in
  let chart_height = height - 5 in
  if chart_height > 0 then
    Charts.Sparkline.draw state.history.cpu ~kind:`Braille ~x:(x + 1) ~y:chart_y grid
      ~width:(width - 2) ~height:chart_height

let draw_memory_panel grid ~x ~y ~width ~height state =
  draw_box grid ~x ~y ~width ~height ~title:"Memory";
  let mem = Sys_metrics.Mem.sample () in
  let used_pct = Int64.to_float mem.used /. Int64.to_float mem.total *. 100. in
  draw_progress_bar grid ~x:(x + 1) ~y:(y + 1) ~width:(width - 2) ~value:used_pct
    ~max_value:100. ~color:Ansi.Color.bright_green ~label:"Used";
  let label_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let value_style = Ansi.Style.make ~fg:Ansi.Color.bright_green () in
  Grid.draw_text ~style:label_style grid ~x:(x + 1) ~y:(y + 2) ~text:"Total:";
  Grid.draw_text ~style:value_style grid ~x:(x + 12) ~y:(y + 2)
    ~text:(format_bytes mem.total);
  Grid.draw_text ~style:label_style grid ~x:(x + 1) ~y:(y + 3) ~text:"Used:";
  Grid.draw_text ~style:value_style grid ~x:(x + 12) ~y:(y + 3)
    ~text:(format_bytes mem.used);
  Grid.draw_text ~style:label_style grid ~x:(x + 1) ~y:(y + 4) ~text:"Free:";
  Grid.draw_text ~style:value_style grid ~x:(x + 12) ~y:(y + 4)
    ~text:(format_bytes mem.free);
  Charts.Sparkline.push state.history.mem used_pct;
  let chart_y = y + 6 in
  let chart_height = height - 7 in
  if chart_height > 0 then
    Charts.Sparkline.draw state.history.mem ~kind:`Braille ~x:(x + 1) ~y:chart_y grid
      ~width:(width - 2) ~height:chart_height

let draw_network_panel grid ~x ~y ~width ~height state =
  draw_box grid ~x ~y ~width ~height ~title:"Network";
  let net = Sys_metrics.Net.sample () in
  let now = Sys_metrics.now_ns () in
  let rx_rate, tx_rate =
    match state.net_prev with
    | None -> (0., 0.)
    | Some prev ->
      let dt_ns = Int64.sub now state.last_sample in
      let rates = Sys_metrics.Net.rates ~dt_ns ~prev ~next:net in
      Array.fold_left
        (fun (rx, tx) (name, (r : Sys_metrics.Net.rate)) ->
          if String.length name > 0 && name.[0] <> 'l' then
            (rx +. r.rx_bytes_per_s, tx +. r.tx_bytes_per_s)
          else (rx, tx))
        (0., 0.) rates
  in
  let label_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let rx_style = Ansi.Style.make ~fg:Ansi.Color.bright_magenta () in
  let tx_style = Ansi.Style.make ~fg:Ansi.Color.bright_yellow () in
  Grid.draw_text ~style:label_style grid ~x:(x + 1) ~y:(y + 1) ~text:"RX:";
  Grid.draw_text ~style:rx_style grid ~x:(x + 12) ~y:(y + 1) ~text:(format_rate rx_rate);
  Grid.draw_text ~style:label_style grid ~x:(x + 1) ~y:(y + 2) ~text:"TX:";
  Grid.draw_text ~style:tx_style grid ~x:(x + 12) ~y:(y + 2) ~text:(format_rate tx_rate);
  Charts.Sparkline.push state.history.net_rx (rx_rate /. 1024.);
  Charts.Sparkline.push state.history.net_tx (tx_rate /. 1024.);
  let chart_y = y + 4 in
  let chart_height = (height - 5) / 2 in
  if chart_height > 0 then (
    Grid.draw_text ~style:rx_style grid ~x:(x + 1) ~y:(chart_y - 1) ~text:"↓ RX";
    Charts.Sparkline.draw state.history.net_rx ~kind:`Braille ~x:(x + 1) ~y:chart_y grid
      ~width:(width - 2) ~height:chart_height;
    let tx_chart_y = chart_y + chart_height + 1 in
    Grid.draw_text ~style:tx_style grid ~x:(x + 1) ~y:(tx_chart_y - 1) ~text:"↑ TX";
    Charts.Sparkline.draw state.history.net_tx ~kind:`Braille ~x:(x + 1) ~y:tx_chart_y grid
      ~width:(width - 2) ~height:chart_height
  )

let draw_disk_panel grid ~x ~y ~width ~height =
  draw_box grid ~x ~y ~width ~height ~title:"Disk";
  let disk = Sys_metrics.Disk.stat "/" in
  let used_pct = Int64.to_float disk.used /. Int64.to_float disk.total *. 100. in
  draw_progress_bar grid ~x:(x + 1) ~y:(y + 1) ~width:(width - 2) ~value:used_pct
    ~max_value:100. ~color:Ansi.Color.bright_yellow ~label:"Used";
  let label_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let value_style = Ansi.Style.make ~fg:Ansi.Color.bright_yellow () in
  Grid.draw_text ~style:label_style grid ~x:(x + 1) ~y:(y + 2) ~text:"Total:";
  Grid.draw_text ~style:value_style grid ~x:(x + 12) ~y:(y + 2)
    ~text:(format_bytes disk.total);
  Grid.draw_text ~style:label_style grid ~x:(x + 1) ~y:(y + 3) ~text:"Used:";
  Grid.draw_text ~style:value_style grid ~x:(x + 12) ~y:(y + 3)
    ~text:(format_bytes disk.used);
  Grid.draw_text ~style:label_style grid ~x:(x + 1) ~y:(y + 4) ~text:"Available:";
  Grid.draw_text ~style:value_style grid ~x:(x + 12) ~y:(y + 4)
    ~text:(format_bytes disk.avail)

let draw_process_panel grid ~x ~y ~width ~height =
  draw_box grid ~x ~y ~width ~height ~title:"Process (self)";
  let proc = Sys_metrics.Proc.sample () in
  let label_style = Ansi.Style.make ~fg:Ansi.Color.white () in
  let value_style = Ansi.Style.make ~fg:Ansi.Color.bright_blue () in
  Grid.draw_text ~style:label_style grid ~x:(x + 1) ~y:(y + 1) ~text:"User time:";
  let utime_s = Int64.to_float proc.utime_ns /. 1e9 in
  Grid.draw_text ~style:value_style grid ~x:(x + 12) ~y:(y + 1)
    ~text:(Printf.sprintf "%.2f s" utime_s);
  Grid.draw_text ~style:label_style grid ~x:(x + 1) ~y:(y + 2) ~text:"Sys time:";
  let stime_s = Int64.to_float proc.stime_ns /. 1e9 in
  Grid.draw_text ~style:value_style grid ~x:(x + 12) ~y:(y + 2)
    ~text:(Printf.sprintf "%.2f s" stime_s);
  (match proc.rss with
   | Some rss ->
     Grid.draw_text ~style:label_style grid ~x:(x + 1) ~y:(y + 3) ~text:"RSS:";
     Grid.draw_text ~style:value_style grid ~x:(x + 12) ~y:(y + 3)
       ~text:(format_bytes rss)
   | None -> ());
  (match proc.vsize with
   | Some vsize ->
     Grid.draw_text ~style:label_style grid ~x:(x + 1) ~y:(y + 4) ~text:"VSize:";
     Grid.draw_text ~style:value_style grid ~x:(x + 12) ~y:(y + 4)
       ~text:(format_bytes vsize)
   | None -> ())

let () =
  let app = Matrix.create ~target_fps:(Some 10.) ~mouse_enabled:false ~debug_overlay:false () in
  let state = ref (initial_state ()) in
  Matrix.run app
    ~on_frame:(fun _ ~dt:_ ->
      let now = Sys_metrics.now_ns () in
      let cpu = Sys_metrics.Cpu.sample () in
      let net = Sys_metrics.Net.sample () in
      let cpu_pct =
        match !state.cpu_prev with
        | None -> 0.
        | Some prev ->
          let dt_ns = Int64.sub now !state.last_sample in
          Sys_metrics.Cpu.busy ~dt_ns ~prev:prev.total ~next:cpu.total
      in
      Charts.Sparkline.push !state.history.cpu cpu_pct;
      state := { !state with cpu_prev = Some cpu; net_prev = Some net; last_sample = now })
    ~on_input:(fun app event ->
      match event with
      | Input.Key { key = Input.Key.Escape; _ } -> Matrix.stop app
      | Input.Key { key = Input.Key.Char u; _ }
        when Uchar.to_int u = Char.code 'q' || Uchar.to_int u = Char.code 'Q' ->
        Matrix.stop app
      | _ -> ())
    ~on_render:(fun app ->
      let grid = Matrix.grid app in
      let cols, rows = Matrix.size app in
      Grid.clear ~color:Ansi.Color.black grid;
      draw_header grid ~cols;
      let cpu_pct =
        match !state.cpu_prev with
        | None -> 0.
        | Some prev ->
          let cpu = Sys_metrics.Cpu.sample () in
          let now = Sys_metrics.now_ns () in
          let dt_ns = Int64.sub now !state.last_sample in
          Sys_metrics.Cpu.busy ~dt_ns ~prev:prev.total ~next:cpu.total
      in
      let half_width = cols / 2 in
      let panel_height = (rows - 1) / 3 in
      draw_cpu_panel grid ~x:0 ~y:1 ~width:half_width ~height:panel_height !state cpu_pct;
      draw_memory_panel grid ~x:half_width ~y:1 ~width:(cols - half_width) ~height:panel_height
        !state;
      draw_network_panel grid ~x:0 ~y:(1 + panel_height) ~width:half_width ~height:panel_height
        !state;
      draw_disk_panel grid ~x:half_width ~y:(1 + panel_height) ~width:(cols - half_width)
        ~height:panel_height;
      draw_process_panel grid ~x:0 ~y:(1 + 2 * panel_height) ~width:cols
        ~height:(rows - 1 - 2 * panel_height))
