open Mosaic
module Ui = Mosaic.Ui

type format =
  | Hours_minutes_seconds
  | Minutes_seconds
  | Total_seconds
  | Custom of (float -> string)

type msg = Tick of float | Start | Stop | Reset | Expired
type event_handler = unit -> msg Cmd.t

type theme = {
  time_style : Style.t;
  expired_style : Style.t;
  label_style : Style.t;
}

let default_theme =
  {
    time_style = Style.empty;
    expired_style = Style.(fg Red);
    label_style = Style.(fg (gray 6));
  }

type model = {
  duration : float;
  remaining : float;
  is_running : bool;
  format : format;
  on_expire : event_handler option;
  theme : theme;
  label : string option;
}

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

let format_time format seconds =
  match format with
  | Hours_minutes_seconds -> format_time_hms seconds
  | Minutes_seconds -> format_time_ms seconds
  | Total_seconds -> Printf.sprintf "%.0f" seconds
  | Custom f -> f seconds

(* Initialization *)

let init ?(duration = 60.0) ?(format = Minutes_seconds) ?(auto_start = false) ()
    =
  let model =
    {
      duration = max 0.0 duration;
      remaining = max 0.0 duration;
      is_running = false;
      format;
      on_expire = None;
      theme = default_theme;
      label = None;
    }
  in
  if auto_start then
    let model = { model with is_running = true } in
    (model, Cmd.tick 1.0 (fun elapsed -> Tick elapsed))
  else (model, Cmd.none)

(* Accessors *)

let remaining model = model.remaining
let duration model = model.duration
let is_running model = model.is_running
let is_expired model = model.remaining <= 0.0

let elapsed_percentage model =
  if model.duration <= 0.0 then 1.0
  else
    let elapsed = model.duration -. model.remaining in
    min 1.0 (max 0.0 (elapsed /. model.duration))

(* Actions *)

let start model = (model, Cmd.msg Start)
let stop model = (model, Cmd.msg Stop)
let toggle model = if model.is_running then stop model else start model
let reset model = (model, Cmd.msg Reset)

let set_duration duration model =
  let duration = max 0.0 duration in
  ({ model with duration; remaining = duration; is_running = false }, Cmd.none)

let add_time seconds model =
  { model with remaining = max 0.0 (model.remaining +. seconds) }

let set_format format model = { model with format }
let on_expire handler model = { model with on_expire = Some handler }
let with_theme theme model = { model with theme }
let with_label label model = { model with label }

(* Update *)

let update msg model =
  match msg with
  | Start ->
      if model.is_running || is_expired model then (model, Cmd.none)
      else
        ( { model with is_running = true },
          Cmd.tick 1.0 (fun elapsed -> Tick elapsed) )
  | Stop -> ({ model with is_running = false }, Cmd.none)
  | Tick _elapsed ->
      if not model.is_running then (model, Cmd.none)
      else
        let remaining = max 0.0 (model.remaining -. 1.0) in
        let model = { model with remaining } in
        if remaining <= 0.0 then
          (* Timer expired *)
          let expire_cmd =
            match model.on_expire with
            | Some handler -> handler ()
            | None -> Cmd.none
          in
          ( { model with is_running = false },
            Cmd.batch [ Cmd.msg Expired; expire_cmd ] )
        else
          (* Continue ticking *)
          (model, Cmd.tick 1.0 (fun elapsed -> Tick elapsed))
  | Reset ->
      ({ model with remaining = model.duration; is_running = false }, Cmd.none)
  | Expired -> (model, Cmd.none)

(* View *)

let view model =
  let time_text = format_time model.format model.remaining in
  let style =
    if is_expired model then model.theme.expired_style
    else model.theme.time_style
  in

  let time_elem = Ui.text ~style time_text in

  match model.label with
  | None -> time_elem
  | Some label ->
      let label_elem = Ui.text ~style:model.theme.label_style (label ^ " ") in
      Ui.hbox [ label_elem; time_elem ]

(* Subscriptions *)

let subscriptions _model = Sub.none

(* Redefine component with actual functions *)
let component = Mosaic.app ~init ~update ~view ~subscriptions ()
