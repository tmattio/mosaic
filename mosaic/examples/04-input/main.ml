(** Text input with cursor and placeholder. *)

open Mosaic_tea

type cursor_style = Block | Line | Underline
type model = { value : string; cursor_style : cursor_style }
type msg = Cycle_cursor | Quit

let init () = ({ value = ""; cursor_style = Block }, Cmd.none)

let update msg model =
  match msg with
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

let view model =
  box ~align_items:Center ~justify_content:Center
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~flex_direction:Column ~gap:(gap 2) ~border:true ~padding:(padding 2)
        ~title:"Text Input"
        [
          (* Input field *)
          box ~flex_direction:Row ~align_items:Center ~gap:(gap 1)
            [
              text ~content:"Name:" ();
              text_input ~autofocus:true ~placeholder:"Type your name..."
                ~cursor_style:(cursor_style_to_prop model.cursor_style)
                ~cursor_blinking:true
                ~size:{ width = px 30; height = px 1 }
                ~value:model.value ();
            ];
          (* Cursor style display *)
          text
            ~content:
              (Printf.sprintf "Cursor style: %s"
                 (cursor_style_name model.cursor_style))
            ();
          (* Help *)
          text ~content:"Press 'c' to cycle cursor style, Esc to quit" ();
        ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Mosaic_ui.Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'c') -> Some Cycle_cursor
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
