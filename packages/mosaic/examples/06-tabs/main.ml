(** Horizontal tab bar with navigation.

    Left and Right or [ and ] move; Enter selects. *)

open Mosaic

type tab = Home | Files | Settings | Network | Logs | Help
type model = { tab : tab option }
type msg = Quit | Tab_changed of tab

let tabs =
  [
    Tab_select.item Home ~description:"Main dashboard" "Home";
    Tab_select.item Files ~description:"Browse files" "Files";
    Tab_select.item Settings ~description:"Configure options" "Settings";
    Tab_select.item Network ~description:"Connection status" "Network";
    Tab_select.item Logs ~description:"System logs" "Logs";
    Tab_select.item Help ~description:"Documentation" "Help";
  ]

let init () = ({ tab = Some Home }, Cmd.none)

let update msg model =
  match msg with
  | Quit -> (model, Cmd.quit)
  | Tab_changed tab -> ({ tab = Some tab }, Cmd.none)

let content_for_tab = function
  | Some Home -> "Welcome to the Home tab. This is the main dashboard."
  | Some Files -> "File browser content would appear here."
  | Some Settings -> "Application settings and preferences."
  | Some Network -> "Network connection status and diagnostics."
  | Some Logs -> "System logs and event history."
  | Some Help -> "Help documentation and user guide."
  | None -> "Choose a tab."

let tab_label = function
  | Home -> "Home"
  | Files -> "Files"
  | Settings -> "Settings"
  | Network -> "Network"
  | Logs -> "Logs"
  | Help -> "Help"

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let accent = Ansi.Color.cyan

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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Tabs";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Tab bar *)
      box ~padding:(padding 1)
        [
          tab_select ~equal:( = ) ~selected:model.tab ~focused:true
            ~show_description:true ~show_underline:true ~show_scroll_arrows:true
            ~wrap_selection:true ~tab_width:12 ~selected_background:accent
            ~selected_text_color:Ansi.Color.black
            ~on_change:(fun tab -> Tab_changed tab)
            tabs;
        ];
      (* Content area *)
      box ~flex_grow:1. ~border:true ~border_color ~padding:(padding 2)
        ~margin:(margin 1)
        [
          box ~flex_direction:Column ~gap:(gap 1)
            [
              text
                ~style:(Ansi.Style.make ~bold:true ())
                (Option.fold ~none:"Tabs" ~some:tab_label model.tab);
              text (content_for_tab model.tab);
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "←/→ navigate  •  [/] vim  •  q quit" ];
    ]

let subscriptions _model =
  Sub.on_keys [ (Shortcut.char 'q', Quit); (Shortcut.escape, Quit) ]

let () = run { init; update; view; subscriptions }
