open Mosaic
module Ui = Mosaic.Ui

type format =
  | HoursMinutesSeconds
  | MinutesSeconds
  | TotalSeconds
  | Milliseconds
  | Custom of (float -> string)

type theme = {
  time_style : Style.t;
  label_style : Style.t;
  split_style : Style.t;
}

let default_theme =
  {
    time_style = Style.empty;
    label_style = Style.(fg (gray 6));
    split_style = Style.(fg (gray 5));
  }

type model = {
  start_time : float option; (* Unix timestamp when started *)
  accumulated_time : float; (* Time accumulated from previous runs *)
  is_running : bool;
  format : format;
  splits : float list; (* List of split times *)
  theme : theme;
  label : string option;
  show_splits : bool;
  tick_count : int; (* For tracking updates *)
}

type msg = Start | Stop | Tick | Reset | Split

(* Get current time in seconds *)
let get_time () = Unix.gettimeofday ()

(* Formatting functions *)

let format_time_hms seconds =
  let total_seconds = int_of_float seconds in
  let hours = total_seconds / 3600 in
  let minutes = total_seconds mod 3600 / 60 in
  let secs = total_seconds mod 60 in
  Printf.sprintf "%02d:%02d:%02d" hours minutes secs

let format_time_ms seconds =
  let total_seconds = int_of_float seconds in
  let minutes = total_seconds / 60 in
  let secs = total_seconds mod 60 in
  Printf.sprintf "%02d:%02d" minutes secs

let format_time_millis seconds =
  let total_seconds = int_of_float seconds in
  let millis =
    int_of_float ((seconds -. float_of_int total_seconds) *. 1000.0)
  in
  let minutes = total_seconds / 60 in
  let secs = total_seconds mod 60 in
  Printf.sprintf "%02d:%02d.%03d" minutes secs millis

let format_time format seconds =
  match format with
  | HoursMinutesSeconds -> format_time_hms seconds
  | MinutesSeconds -> format_time_ms seconds
  | TotalSeconds -> Printf.sprintf "%.1f" seconds
  | Milliseconds -> format_time_millis seconds
  | Custom f -> f seconds

(* Initialization *)

let init ?(format = MinutesSeconds) ?(auto_start = false) () =
  let model =
    {
      start_time = None;
      accumulated_time = 0.0;
      is_running = false;
      format;
      splits = [];
      theme = default_theme;
      label = None;
      show_splits = false;
      tick_count = 0;
    }
  in
  if auto_start then
    let model =
      { model with is_running = true; start_time = Some (get_time ()) }
    in
    (model, Cmd.tick 0.1 (fun _ -> Tick))
  else (model, Cmd.none)

(* Accessors *)

let elapsed model =
  match model.start_time with
  | Some start when model.is_running ->
      model.accumulated_time +. (get_time () -. start)
  | _ -> model.accumulated_time

let is_running model = model.is_running
let split_times model = List.rev model.splits
let last_split model = match model.splits with [] -> None | h :: _ -> Some h

(* Actions *)

let start model = (model, Cmd.msg Start)
let stop model = (model, Cmd.msg Stop)
let toggle model = if model.is_running then stop model else start model
let reset model = (model, Cmd.msg Reset)
let split model = (model, Cmd.msg Split)
let clear_splits model = { model with splits = [] }
let set_format format model = { model with format }
let with_theme theme model = { model with theme }
let with_label label model = { model with label }
let with_show_splits show model = { model with show_splits = show }

(* Update *)

let update msg model =
  match msg with
  | Start ->
      if model.is_running then (model, Cmd.none)
      else
        ( { model with is_running = true; start_time = Some (get_time ()) },
          Cmd.tick 0.1 (fun _ -> Tick) )
  | Stop ->
      if not model.is_running then (model, Cmd.none)
      else
        let accumulated = elapsed model in
        ( {
            model with
            is_running = false;
            accumulated_time = accumulated;
            start_time = None;
          },
          Cmd.none )
  | Tick ->
      if model.is_running then
        (* Schedule next tick *)
        ( { model with tick_count = model.tick_count + 1 },
          Cmd.tick 0.1 (fun _ -> Tick) )
      else (model, Cmd.none)
  | Reset ->
      ( {
          model with
          start_time = None;
          accumulated_time = 0.0;
          is_running = false;
          splits = [];
        },
        Cmd.none )
  | Split ->
      let current_time = elapsed model in
      ({ model with splits = current_time :: model.splits }, Cmd.none)

(* View *)

let view model =
  let current_time = elapsed model in
  let time_text = format_time model.format current_time in
  let time_elem = Ui.text ~style:model.theme.time_style time_text in

  let main_display =
    match model.label with
    | None -> time_elem
    | Some label ->
        let label_elem = Ui.text ~style:model.theme.label_style (label ^ " ") in
        Ui.hbox [ label_elem; time_elem ]
  in

  if model.show_splits && model.splits <> [] then
    let split_elems =
      List.mapi
        (fun i split_time ->
          let split_text =
            Printf.sprintf "Split %d: %s"
              (List.length model.splits - i)
              (format_time model.format split_time)
          in
          Ui.text ~style:model.theme.split_style split_text)
        (split_times model)
    in
    Ui.vbox (main_display :: split_elems)
  else main_display

(* Subscriptions *)

let subscriptions _model = Sub.none

(* Redefine component with actual functions *)
let component = Mosaic.app ~init ~update ~view ~subscriptions ()
