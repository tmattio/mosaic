(** Styled text demonstration with colors and formatting. *)

open Mosaic_tea

type wrap_mode = None | Char | Word
type model = { wrap : wrap_mode }
type msg = Cycle_wrap | Quit

let init () = ({ wrap = Word }, Cmd.none)

let update msg model =
  match msg with
  | Cycle_wrap ->
      let wrap =
        match model.wrap with None -> Char | Char -> Word | Word -> None
      in
      ({ wrap }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let wrap_mode_to_string = function
  | None -> "None"
  | Char -> "Char"
  | Word -> "Word"

let wrap_mode_to_prop = function None -> `None | Char -> `Char | Word -> `Word

(* Palette *)
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
              text ~content:"▸ Text Styles"
                ~text_style:(Ansi.Style.make ~bold:true ())
                ();
              text ~content:"▄▀ mosaic" ~text_style:muted ();
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~flex_direction:Column ~padding:(padding 1) ~gap:(gap 1)
        [
          (* First row: Styles, Foreground Colors, Background Colors *)
          box ~flex_direction:Row ~gap:(gap 1)
            [
              (* Text styles *)
              box ~border:true ~border_color ~padding:(padding 1)
                ~title:"Text Styles" ~flex_direction:Column ~gap:(gap 1)
                [
                  text ~content:"Bold text"
                    ~text_style:(Ansi.Style.make ~bold:true ())
                    ();
                  text ~content:"Italic text"
                    ~text_style:(Ansi.Style.make ~italic:true ())
                    ();
                  text ~content:"Underlined text"
                    ~text_style:(Ansi.Style.make ~underline:true ())
                    ();
                  text ~content:"Strikethrough text"
                    ~text_style:(Ansi.Style.make ~strikethrough:true ())
                    ();
                  text ~content:"Dim text"
                    ~text_style:(Ansi.Style.make ~dim:true ())
                    ();
                  text ~content:"Blink text"
                    ~text_style:(Ansi.Style.make ~blink:true ())
                    ();
                  text ~content:"Bold + Italic"
                    ~text_style:(Ansi.Style.make ~bold:true ~italic:true ())
                    ();
                ];
              (* Foreground Colors *)
              box ~border:true ~border_color ~padding:(padding 1)
                ~title:"Foreground" ~flex_direction:Column ~gap:(gap 1)
                ~size:{ width = px 14; height = auto }
                [
                  text ~content:"Red"
                    ~text_style:(Ansi.Style.make ~fg:Ansi.Color.red ())
                    ();
                  text ~content:"Green"
                    ~text_style:(Ansi.Style.make ~fg:Ansi.Color.green ())
                    ();
                  text ~content:"Blue"
                    ~text_style:(Ansi.Style.make ~fg:Ansi.Color.blue ())
                    ();
                  text ~content:"Yellow"
                    ~text_style:(Ansi.Style.make ~fg:Ansi.Color.yellow ())
                    ();
                  text ~content:"Cyan"
                    ~text_style:(Ansi.Style.make ~fg:Ansi.Color.cyan ())
                    ();
                  text ~content:"Magenta"
                    ~text_style:(Ansi.Style.make ~fg:Ansi.Color.magenta ())
                    ();
                  text ~content:"White"
                    ~text_style:(Ansi.Style.make ~fg:Ansi.Color.white ())
                    ();
                ];
              (* Background Colors *)
              box ~border:true ~border_color ~padding:(padding 1)
                ~title:"Background" ~flex_direction:Column ~gap:(gap 1)
                ~size:{ width = px 14; height = auto }
                [
                  text ~content:" Red "
                    ~text_style:
                      (Ansi.Style.make ~bg:Ansi.Color.red ~fg:Ansi.Color.white
                         ())
                    ();
                  text ~content:" Green "
                    ~text_style:
                      (Ansi.Style.make ~bg:Ansi.Color.green ~fg:Ansi.Color.black
                         ())
                    ();
                  text ~content:" Blue "
                    ~text_style:
                      (Ansi.Style.make ~bg:Ansi.Color.blue ~fg:Ansi.Color.white
                         ())
                    ();
                  text ~content:" Yellow "
                    ~text_style:
                      (Ansi.Style.make ~bg:Ansi.Color.yellow
                         ~fg:Ansi.Color.black ())
                    ();
                  text ~content:" Cyan "
                    ~text_style:
                      (Ansi.Style.make ~bg:Ansi.Color.cyan ~fg:Ansi.Color.black
                         ())
                    ();
                  text ~content:" Magenta "
                    ~text_style:
                      (Ansi.Style.make ~bg:Ansi.Color.magenta
                         ~fg:Ansi.Color.white ())
                    ();
                  text ~content:" White "
                    ~text_style:
                      (Ansi.Style.make ~bg:Ansi.Color.white ~fg:Ansi.Color.black
                         ())
                    ();
                ];
              (* Grayscale *)
              box ~border:true ~border_color ~padding:(padding 1)
                ~title:"Grayscale" ~flex_direction:Row ~gap:(gap 1)
                ~flex_wrap:Wrap
                ~size:{ width = px 14; height = auto }
                (List.init 24 (fun i ->
                     let color = Ansi.Color.grayscale ~level:i in
                     let fg =
                       if i < 12 then Ansi.Color.white else Ansi.Color.black
                     in
                     text ~content:(Printf.sprintf "%02d" i)
                       ~text_style:(Ansi.Style.make ~bg:color ~fg ())
                       ()));
            ];
          (* Second row: Wrap mode demo *)
          box ~border:true ~border_color ~padding:(padding 1)
            ~title:
              (Printf.sprintf "Wrap Mode: %s" (wrap_mode_to_string model.wrap))
            ~size:{ width = px 40; height = auto }
            [
              text
                ~content:
                  "This is a longer text that demonstrates different wrapping \
                   behaviors. Press 'w' to cycle through wrap modes."
                ~size:{ width = pct 100; height = auto }
                ~wrap_mode:(wrap_mode_to_prop model.wrap)
                ();
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~content:"w cycle wrap  •  q quit" ~text_style:hint () ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Mosaic_ui.Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'w') -> Some Cycle_wrap
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
