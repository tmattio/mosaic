(** Procedural drawing with shapes and braille lines. *)

open Mosaic_tea
module Canvas = Mosaic_ui.Canvas

type line_mode = Normal | Braille
type model = { mode : line_mode; angle : float }
type msg = Toggle_mode | Tick of float | Quit

let init () = ({ mode = Normal; angle = 0. }, Cmd.none)

let update msg model =
  match msg with
  | Toggle_mode ->
      let mode =
        match model.mode with Normal -> Braille | Braille -> Normal
      in
      ({ model with mode }, Cmd.none)
  | Tick dt -> ({ model with angle = model.angle +. (dt *. 0.5) }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let mode_name = function Normal -> "Normal" | Braille -> "Braille (sub-cell)"

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let draw_canvas model grid ~width ~height =
  let cx = width / 2 in
  let cy = height / 2 in
  let radius = min cx cy - 2 in

  (* Draw border box *)
  let border_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:8) () in
  Grid.draw_box grid ~x:0 ~y:0 ~width ~height ~border_chars:Grid.Border.single
    ~border_sides:[ `Top; `Right; `Bottom; `Left ]
    ~border_style ~bg_color:Ansi.Color.default ~should_fill:false ();

  (* Draw coordinate axes *)
  let axis_style = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:8) () in
  Grid.draw_line grid ~x1:1 ~y1:cy ~x2:(width - 2) ~y2:cy ~style:axis_style ();
  Grid.draw_line grid ~x1:cx ~y1:1 ~x2:cx ~y2:(height - 2) ~style:axis_style ();

  (* Draw rotating lines from center *)
  let line_kind, x_scale, y_scale =
    match model.mode with Normal -> (`Line, 1, 1) | Braille -> (`Braille, 2, 4)
  in
  let cx_scaled = cx * x_scale in
  let cy_scaled = cy * y_scale in
  let radius_f = Float.of_int radius in
  for i = 0 to 5 do
    let angle = model.angle +. (Float.pi *. Float.of_int i /. 3.) in
    let x2 = (cx + int_of_float (Float.cos angle *. radius_f)) * x_scale in
    let y2 =
      (cy + int_of_float (Float.sin angle *. (radius_f /. 2.))) * y_scale
    in
    let color =
      match i mod 6 with
      | 0 -> Ansi.Color.red
      | 1 -> Ansi.Color.yellow
      | 2 -> Ansi.Color.green
      | 3 -> Ansi.Color.cyan
      | 4 -> Ansi.Color.blue
      | _ -> Ansi.Color.magenta
    in
    Grid.draw_line grid ~x1:cx_scaled ~y1:cy_scaled ~x2 ~y2 ~kind:line_kind
      ~style:(Ansi.Style.make ~fg:color ())
      ()
  done;

  (* Draw some filled rectangles *)
  Grid.fill_rect grid ~x:2 ~y:2 ~width:5 ~height:2 ~color:Ansi.Color.red;
  Grid.fill_rect grid ~x:(width - 7) ~y:2 ~width:5 ~height:2
    ~color:Ansi.Color.green;
  Grid.fill_rect grid ~x:2 ~y:(height - 4) ~width:5 ~height:2
    ~color:Ansi.Color.blue;
  Grid.fill_rect grid ~x:(width - 7) ~y:(height - 4) ~width:5 ~height:2
    ~color:Ansi.Color.yellow;

  (* Draw mode indicator *)
  Grid.draw_text grid ~x:2 ~y:(height - 2)
    ~style:(Ansi.Style.make ~dim:true ())
    ~text:(mode_name model.mode)

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
              text ~text_style:(Ansi.Style.make ~bold:true ()) "▸ Canvas";
              text ~text_style:muted "▄▀ mosaic";
            ];
        ];
      (* Canvas area *)
      box ~flex_grow:1. ~padding:(padding 1)
        [
          canvas
            ~draw:(fun canvas ~width ~height ->
              draw_canvas model canvas ~width ~height)
            ~size:{ width = pct 100; height = pct 100 }
            ();
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~text_style:hint "m toggle mode  •  q quit" ];
    ]

let subscriptions _model =
  Sub.batch
    [
      Sub.on_key (fun ev ->
          match (Mosaic_ui.Event.Key.data ev).key with
          | Char c when Uchar.equal c (Uchar.of_char 'm') -> Some Toggle_mode
          | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
          | Escape -> Some Quit
          | _ -> None);
      Sub.on_tick (fun ~dt -> Tick dt);
    ]

let () = run { init; update; view; subscriptions }
