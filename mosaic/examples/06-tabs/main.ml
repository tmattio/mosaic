(** Horizontal tab bar with navigation. *)

open Mosaic_tea

type model = { tab : int }
type msg = Quit

let tabs =
  [
    Tab_select.{ label = "Home"; description = Some "Main dashboard" };
    { label = "Files"; description = Some "Browse files" };
    { label = "Settings"; description = Some "Configure options" };
    { label = "Network"; description = Some "Connection status" };
    { label = "Logs"; description = Some "System logs" };
    { label = "Help"; description = Some "Documentation" };
  ]

let init () = ({ tab = 0 }, Cmd.none)
let update msg model = match msg with Quit -> (model, Cmd.quit)

let content_for_tab = function
  | 0 -> "Welcome to the Home tab. This is the main dashboard."
  | 1 -> "File browser content would appear here."
  | 2 -> "Application settings and preferences."
  | 3 -> "Network connection status and diagnostics."
  | 4 -> "System logs and event history."
  | 5 -> "Help documentation and user guide."
  | _ -> "Unknown tab"

let view model =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Tab bar *)
      tab_select ~options:tabs ~show_description:true ~show_underline:true
        ~show_scroll_arrows:true ~wrap_selection:true ~tab_width:12
        ~selected_background:Ansi.Color.blue
        ~selected_text:Ansi.Color.bright_white ();
      (* Content area *)
      box ~flex_grow:1. ~border:true ~padding:(padding 2) ~margin:(margin 1)
        [
          box ~flex_direction:Column ~gap:(gap 1)
            [
              text ~content:(List.nth tabs model.tab).label
                ~text_style:(Ansi.Style.make ~bold:true ())
                ();
              text ~content:(content_for_tab model.tab) ();
            ];
        ];
      (* Help *)
      box ~padding:(padding 1)
        [ text ~content:"Left/Right or [/] to navigate, q to quit" () ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Mosaic_ui.Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
