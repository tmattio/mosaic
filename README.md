# Mosaic

A modern, declarative terminal UI framework for OCaml with a React-inspired API powered by algebraic effects.

## Overview

Mosaic is a high-performance terminal UI framework that brings React's component model and hooks to the terminal applications. It combines three powerful foundations:

1. **[Matrix](./matrix)** - A highly optimized terminal toolkit with efficient grid representation, damage tracking, and double-buffered rendering
2. **[Toffee](./toffee)** - A pure OCaml port of the Taffy layout engine, providing CSS Grid, Flexbox, and Block layouts
3. **Algebraic Effects** - OCaml 5's effect system enables a direct-style React-like API with hooks

## Quick Start

### React-Style with Hooks (Algebraic Effects)

```ocaml
open Mosaic

let counter () =
  (* Direct-style state management via algebraic effects *)
  let count, set_count = use_state 0 in
  
  (* Direct-style effect with cleanup *)
  use_effect (fun () ->
    Printf.printf "Count changed to %d\n" count;
    Some (fun () -> Printf.printf "Cleanup for %d\n" count)
  ) [Dep.pack count];
  
  (* Direct-style keyboard handler *)
  use_keyboard ~key:(Char '+') (fun () -> 
    set_count (count + 1)
  );
  
  (* Toffee-powered flexbox layout *)
  vbox ~gap:(`Cells 1) [
    text ~style:Style.(fg (rgb 100 200 255) ++ bold) 
      (Printf.sprintf "Count: %d" count);
    hbox ~gap:(`Cells 2) [
      button ~label:"Increment" ~on_click:(fun () -> 
        set_count (count + 1)
      ) ();
      button ~label:"Decrement" ~on_click:(fun () -> 
        set_count (count - 1)
      ) ();
    ];
  ]

let () = run counter
```

### The Elm Architecture

```ocaml
open Mosaic_tea

type model = { count: int }
type msg = Increment | Decrement | Reset

let init () = { count = 0 }, Cmd.none

let update msg model =
  match msg with
  | Increment -> { count = model.count + 1 }, Cmd.none
  | Decrement -> { count = model.count - 1 }, Cmd.none
  | Reset -> { count = 0 }, Cmd.none

let view model =
  Ui.vbox [
    Ui.text (Printf.sprintf "Count: %d" model.count);
    Ui.hbox [
      Ui.text "[+] Increment";
      Ui.text "[-] Decrement";
      Ui.text "[r] Reset";
    ];
  ]

let subscriptions _ =
  Sub.batch [
    Sub.on_key (Char '+') Increment;
    Sub.on_key (Char '-') Decrement;
    Sub.on_key (Char 'r') Reset;
  ]

let () = 
  run (app ~init ~update ~view ~subscriptions ())
```

## License

Mosaic is licensed under the ISC license. See [LICENSE](LICENSE) for details.
