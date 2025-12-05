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

let view () =
  box ~align_items:Center ~justify_content:Center
    ~size:{ width = pct 100; height = pct 100 }
    [
      box ~flex_direction:Column ~gap:(gap 2) ~border:true ~padding:(padding 2)
        ~title:"Select Language"
        [
          (* Select component *)
          select ~options:languages ~show_description:true
            ~show_scroll_indicator:true ~wrap_selection:true
            ~selected_background:Ansi.Color.blue
            ~selected_text_color:Ansi.Color.white
            ~size:{ width = px 40; height = px 10 }
            ();
          (* Help *)
          text ~content:"Up/Down or j/k to navigate, q to quit" ();
        ];
    ]

let subscriptions () =
  Sub.on_key (fun ev ->
      match (Mosaic_ui.Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
