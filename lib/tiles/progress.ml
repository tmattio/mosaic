open Mosaic
module Ui = Mosaic.Ui

type style_mode = Solid of Style.color | Gradient of Style.color * Style.color

type theme = {
  full_char : string;
  empty_char : string;
  percentage_style : Style.t;
  bar_style : Style.t;
}

let default_theme =
  {
    full_char = "█";
    empty_char = "░";
    percentage_style = Style.empty;
    bar_style = Style.empty;
  }

type model = {
  percent : float;
  width : int;
  show_percentage : bool;
  style_mode : style_mode;
  theme : theme;
}

type msg = SetPercent of float

let clamp_percent p = max 0.0 (min 1.0 p)

let init ?(percent = 0.0) ?(width = 40) ?(show_percentage = true)
    ?(style_mode = Solid (Style.Index 33)) () =
  let model =
    {
      percent = clamp_percent percent;
      width = max 1 width;
      show_percentage;
      style_mode;
      theme = default_theme;
    }
  in
  (model, Cmd.none)

(* Accessors *)

let percent model = model.percent
let is_complete model = model.percent >= 1.0
let width model = model.width

(* Actions *)

let set_percent percent model = (model, Cmd.msg (SetPercent percent))
let increment amount model = set_percent (model.percent +. amount) model
let decrement amount model = set_percent (model.percent -. amount) model
let complete model = set_percent 1.0 model
let reset model = set_percent 0.0 model
let set_width width model = { model with width = max 1 width }
let show_percentage show model = { model with show_percentage = show }
let set_style_mode style_mode model = { model with style_mode }
let with_theme theme model = { model with theme }

(* Update *)

let update msg model =
  match msg with
  | SetPercent p -> ({ model with percent = clamp_percent p }, Cmd.none)

(* View helpers *)

let interpolate_color c1 c2 t =
  match (c1, c2) with
  | Style.RGB (r1, g1, b1), Style.RGB (r2, g2, b2) ->
      let interp a b =
        int_of_float (float_of_int a +. (t *. float_of_int (b - a)))
      in
      Style.RGB (interp r1 r2, interp g1 g2, interp b1 b2)
  | Style.Index _i1, Style.Index _i2 ->
      (* For indexed colors, just switch at midpoint *)
      if t < 0.5 then c1 else c2
  | _ -> c1 (* Default to first color for mixed types *)

let render_bar model =
  let filled_width = int_of_float (float_of_int model.width *. model.percent) in
  let empty_width = model.width - filled_width in

  match model.style_mode with
  | Solid color ->
      let filled =
        String.concat ""
          (List.init filled_width (fun _ -> model.theme.full_char))
      in
      let empty =
        String.concat ""
          (List.init empty_width (fun _ -> model.theme.empty_char))
      in
      Ui.hbox
        [
          Ui.text ~style:Style.(fg color ++ model.theme.bar_style) filled;
          Ui.text ~style:model.theme.bar_style empty;
        ]
  | Gradient (c1, c2) ->
      (* Render gradient by coloring each character *)
      let chars = ref [] in
      for i = 0 to filled_width - 1 do
        let t =
          if filled_width <= 1 then 0.5
          else float_of_int i /. float_of_int (filled_width - 1)
        in
        let color = interpolate_color c1 c2 t in
        let ch =
          Ui.text
            ~style:Style.(fg color ++ model.theme.bar_style)
            model.theme.full_char
        in
        chars := ch :: !chars
      done;
      let filled_part = Ui.hbox (List.rev !chars) in
      let empty_part =
        Ui.text ~style:model.theme.bar_style
          (String.concat ""
             (List.init empty_width (fun _ -> model.theme.empty_char)))
      in
      Ui.hbox [ filled_part; empty_part ]

let render_percentage model =
  let pct = int_of_float (model.percent *. 100.0) in
  Printf.sprintf " %3d%%" pct

(* View *)

let view model =
  let bar = render_bar model in

  if model.show_percentage then
    let percentage_text = render_percentage model in
    let percentage =
      Ui.text ~style:model.theme.percentage_style percentage_text
    in
    Ui.hbox [ bar; percentage ]
  else bar

(* Subscriptions *)

let subscriptions _ = Sub.none

(* Redefine component with actual functions *)
let component = Mosaic.app ~init ~update ~view ~subscriptions ()
