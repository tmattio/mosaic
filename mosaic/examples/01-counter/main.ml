(** Simple counter demonstrating TEA basics. *)

open Mosaic_tea

type msg = Increment | Decrement | Quit

let init () = (0, Cmd.none)

let update msg model =
  match msg with
  | Increment -> (model + 1, Cmd.none)
  | Decrement -> (model - 1, Cmd.none)
  | Quit -> (model, Cmd.quit)

let view model =
  box ~align_items:Center ~justify_content:Center
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~flex_direction:Column ~align_items:Center ~gap:(gap 1) ~border:true
        ~padding:(padding 2) ~title:"Counter"
        [
          text ~content:(Printf.sprintf "Count: %d" model) ();
          text ~content:"Press + or - to change, q to quit" ();
        ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Mosaic_ui.Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char '+') -> Some Increment
      | Char c when Uchar.equal c (Uchar.of_char '-') -> Some Decrement
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
