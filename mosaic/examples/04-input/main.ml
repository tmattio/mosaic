(** Text input with cursor and placeholder. *)

open Mosaic

type cursor_style = Block | Line | Underline
type model = { value : Editor.t; cursor_style : cursor_style }
type msg = Cycle_cursor | Set_value of Editor.t | Quit

let init () = ({ value = Editor.empty; cursor_style = Block }, Cmd.none)

let update msg model =
  match msg with
  | Set_value value -> ({ model with value }, Cmd.none)
  | Cycle_cursor ->
      let cursor_style =
        match model.cursor_style with
        | Block -> Line
        | Line -> Underline
        | Underline -> Block
      in
      ({ model with cursor_style }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let cursor_style_name = function
  | Block -> "Block"
  | Line -> "Line"
  | Underline -> "Underline"

let cursor_style_to_prop = function
  | Block -> `Block
  | Line -> `Line
  | Underline -> `Underline

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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Text Input";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [
          box ~flex_direction:Column ~gap:(gap 2) ~border:true ~border_color
            ~padding:(padding 2)
            [
              (* Input field *)
              box ~flex_direction:Row ~align_items:Center ~gap:(gap 1)
                [
                  text "Name:";
                  input
                    ~size:{ width = px 30; height = px 1 }
                    ~focused:true ~placeholder:"Type your name..."
                    ~cursor_style:(cursor_style_to_prop model.cursor_style)
                    ~cursor_blinking:true ~state:model.value
                    ~on_change:(fun value -> Set_value value)
                    ();
                ];
              (* Cursor style display *)
              text ~style:hint
                (Printf.sprintf "Cursor style: %s"
                   (cursor_style_name model.cursor_style));
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "Tab cycle cursor  •  Esc quit" ];
    ]

let subscriptions _model =
  (* Application shortcuts run before the focused editor; on_keys consumes
     matching shortcuts so Tab changes the cursor style without editing text. *)
  Sub.on_keys [ (Shortcut.tab, Cycle_cursor); (Shortcut.escape, Quit) ]

let () = run { init; update; view; subscriptions }
