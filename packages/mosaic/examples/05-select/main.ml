(** Vertical list selection with keyboard navigation.

    Up and Down or j and k move; Enter selects. *)

open Mosaic

type msg = Quit | Selected of string

let languages =
  [
    Select.item "OCaml" ~description:"Functional, type-safe" "OCaml";
    Select.item "Rust" ~description:"Safe systems programming" "Rust";
    Select.item "Haskell" ~description:"Pure functional" "Haskell";
    Select.item "TypeScript" ~description:"Typed JavaScript" "TypeScript";
    Select.item "Python" ~description:"Easy to learn" "Python";
    Select.item "Go" ~description:"Fast compilation" "Go";
    Select.item "Zig" ~description:"Low-level control" "Zig";
    Select.item "Elixir" ~description:"Concurrent, fault-tolerant" "Elixir";
  ]

let init () = (Some "OCaml", Cmd.none)

let update msg selected =
  match msg with
  | Quit -> (selected, Cmd.quit)
  | Selected value -> (Some value, Cmd.none)

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let accent = Ansi.Color.cyan

let view selected =
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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Select";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [
          box ~flex_direction:Column ~gap:(gap 2) ~border:true ~border_color
            ~padding:(padding 2)
            [
              text ~style:(Ansi.Style.make ~bold:true ()) "Choose a language:";
              (* Select component *)
              select ~equal:String.equal ~selected
                ~on_change:(fun value -> Selected value)
                ~size:{ width = px 40; height = px 10 }
                ~focused:true ~show_description:true ~show_scroll_indicator:true
                ~wrap_selection:true ~selected_background:accent
                ~selected_text_color:Ansi.Color.black
                ~selected_description_color:(Ansi.Color.of_rgb 0 50 70)
                ~description_color:(Ansi.Color.grayscale ~level:14)
                ~focused_background:(Ansi.Color.of_rgb 20 30 40)
                languages;
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~style:hint "↑/↓ navigate  •  j/k vim  •  q quit" ];
    ]

let subscriptions _selected =
  Sub.on_keys [ (Shortcut.char 'q', Quit); (Shortcut.escape, Quit) ]

let () = run { init; update; view; subscriptions }
