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

let draw_canvas model canvas ~width ~height =
  let cx = width / 2 in
  let cy = height / 2 in
  let radius = min cx cy - 2 in

  (* Clear canvas *)
  Canvas.clear canvas;

  (* Draw border box *)
  Canvas.draw_box canvas ~x:0 ~y:0 ~width ~height
    ~border_color:(Ansi.Color.grayscale ~level:8) ();

  (* Draw coordinate axes *)
  Canvas.draw_line canvas ~x1:1 ~y1:cy ~x2:(width - 2) ~y2:cy
    ~style:(Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:8) ())
    ();
  Canvas.draw_line canvas ~x1:cx ~y1:1 ~x2:cx ~y2:(height - 2)
    ~style:(Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:8) ())
    ();

  (* Draw rotating lines from center *)
  let line_kind =
    match model.mode with Normal -> `Line | Braille -> `Braille
  in
  let scale = if model.mode = Braille then 2 else 1 in
  for i = 0 to 5 do
    let angle = model.angle +. (Float.pi *. Float.of_int i /. 3.) in
    let x2 =
      cx + int_of_float (Float.cos angle *. Float.of_int (radius * scale))
    in
    let y2 =
      cy + int_of_float (Float.sin angle *. Float.of_int (radius * scale / 2))
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
    Canvas.draw_line canvas ~x1:(cx * scale) ~y1:(cy * scale) ~x2 ~y2
      ~kind:line_kind
      ~style:(Ansi.Style.make ~fg:color ())
      ()
  done;

  (* Draw some filled rectangles *)
  Canvas.fill_rect canvas ~x:2 ~y:2 ~width:5 ~height:2 ~color:Ansi.Color.red;
  Canvas.fill_rect canvas ~x:(width - 7) ~y:2 ~width:5 ~height:2
    ~color:Ansi.Color.green;
  Canvas.fill_rect canvas ~x:2 ~y:(height - 4) ~width:5 ~height:2
    ~color:Ansi.Color.blue;
  Canvas.fill_rect canvas ~x:(width - 7) ~y:(height - 4) ~width:5 ~height:2
    ~color:Ansi.Color.yellow;

  (* Plot mode indicator *)
  Canvas.plot canvas ~x:2 ~y:(height - 2)
    ~style:(Ansi.Style.make ~dim:true ())
    (mode_name model.mode)

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
              text ~content:"▸ Canvas"
                ~text_style:(Ansi.Style.make ~bold:true ())
                ();
              text ~content:"▄▀ mosaic" ~text_style:muted ();
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
        [ text ~content:"m toggle mode  •  q quit" ~text_style:hint () ];
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
