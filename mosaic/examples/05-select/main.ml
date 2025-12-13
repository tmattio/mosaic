(** Vertical list selection with keyboard navigation. *)

open Mosaic_tea

type msg = Quit

let languages : Select.item list =
  [
    { name = "OCaml"; description = Some "Functional, type-safe" };
    { name = "Rust"; description = Some "Safe systems programming" };
    { name = "Haskell"; description = Some "Pure functional" };
    { name = "TypeScript"; description = Some "Typed JavaScript" };
    { name = "Python"; description = Some "Easy to learn" };
    { name = "Go"; description = Some "Fast compilation" };
    { name = "Zig"; description = Some "Low-level control" };
    { name = "Elixir"; description = Some "Concurrent, fault-tolerant" };
  ]

let init () = ((), Cmd.none)
let update msg () = match msg with Quit -> ((), Cmd.quit)

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()
let accent = Ansi.Color.cyan

let view () =
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
              text ~text_style:(Ansi.Style.make ~bold:true ()) "▸ Select";
              text ~text_style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [
          box ~flex_direction:Column ~gap:(gap 2) ~border:true ~border_color
            ~padding:(padding 2)
            [
              text
                ~text_style:(Ansi.Style.make ~bold:true ())
                "Choose a language:";
              (* Select component *)
              select ~show_description:true ~show_scroll_indicator:true
                ~wrap_selection:true ~selected_background:accent
                ~selected_text_color:Ansi.Color.black
                ~size:{ width = px 40; height = px 10 }
                languages;
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [ text ~text_style:hint "↑/↓ navigate  •  j/k vim  •  q quit" ];
    ]

let subscriptions () =
  Sub.on_key (fun ev ->
      match (Mosaic_ui.Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
