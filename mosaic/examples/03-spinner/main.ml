(** Animated spinners with built-in presets. *)

open Mosaic_tea

type model = { preset : Spinner.preset; running : bool }
type msg = Next_preset | Toggle | Quit

let presets = [| Spinner.Dots; Line; Circle; Bounce; Bar; Arrow |]

let preset_name = function
  | Spinner.Dots -> "Dots"
  | Line -> "Line"
  | Circle -> "Circle"
  | Bounce -> "Bounce"
  | Bar -> "Bar"
  | Arrow -> "Arrow"

let init () = ({ preset = Spinner.Dots; running = true }, Cmd.none)

let update msg model =
  match msg with
  | Next_preset ->
      let idx =
        Array.find_index (fun p -> p = model.preset) presets
        |> Option.value ~default:0
      in
      let next_idx = (idx + 1) mod Array.length presets in
      ({ model with preset = presets.(next_idx) }, Cmd.none)
  | Toggle -> ({ model with running = not model.running }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let view model =
  box ~align_items:Center ~justify_content:Center
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~flex_direction:Column ~gap:(gap 2) ~border:true ~padding:(padding 2)
        ~title:"Spinners"
        [
          (* Current spinner with label *)
          box ~flex_direction:Row ~align_items:Center ~gap:(gap 2)
            [
              spinner ~preset:model.preset ~autoplay:model.running
                ~color:Ansi.Color.cyan ();
              text
                ~content:
                  (Printf.sprintf "%s %s" (preset_name model.preset)
                     (if model.running then "(running)" else "(stopped)"))
                ();
            ];
          (* All presets in a row *)
          box ~flex_direction:Row ~gap:(gap 3) ~margin:(margin 1)
            (Array.to_list
               (Array.map
                  (fun preset ->
                    box ~flex_direction:Column ~align_items:Center ~gap:(gap 1)
                      [
                        spinner ~preset ~autoplay:model.running ();
                        text ~content:(preset_name preset)
                          ~text_style:
                            (Ansi.Style.make ~dim:(preset <> model.preset) ())
                          ();
                      ])
                  presets));
          (* Help *)
          text
            ~content:"Press 'n' for next preset, Space to toggle, 'q' to quit"
            ();
        ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Mosaic_ui.Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'n') -> Some Next_preset
      | Char c when Uchar.equal c (Uchar.of_char ' ') -> Some Toggle
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
