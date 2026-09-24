(** Hierarchical tree view with expand/collapse and guide lines. *)

open Mosaic

type guide_style_kind = Rounded | Single | Heavy | Double

type model = {
  state : Tree.State.t;
  show_guides : bool;
  guide_style_kind : guide_style_kind;
  activated : string option;
}

type msg =
  | Select of Tree.State.t
  | Activate of Tree.item
  | Toggle_guides
  | Toggle_guide_style
  | Quit

let file_tree =
  let open Tree in
  [
    item ~id:"project"
      ~children:
        [
          item ~id:"project/src"
            ~children:
              [
                item ~id:"project/src/main.ml" "main.ml";
                item ~id:"project/src/main.mli" "main.mli";
                item ~id:"project/src/dune" "dune";
              ]
            "src";
          item ~id:"project/test"
            ~children:
              [
                item ~id:"project/test/parser" "test_parser.ml";
                item ~id:"project/test/lexer" "test_lexer.ml";
                item ~id:"project/test/dune" "dune";
              ]
            "test";
          item ~id:"project/doc"
            ~children:
              [
                item ~id:"project/doc/index" "index.mld";
                item ~id:"project/doc/tutorial" "tutorial.mld";
              ]
            "doc";
          item ~id:"project/readme" "README.md";
          item ~id:"project/changes" "CHANGES.md";
          item ~id:"project/license" "LICENSE";
          item ~id:"project/dune-project" "dune-project";
        ]
      "my-project";
    item ~id:"library"
      ~children:
        [
          item ~id:"library/lib"
            ~children:
              [
                item ~id:"library/lib/ml" "lib.ml";
                item ~id:"library/lib/mli" "lib.mli";
              ]
            "lib";
          item ~id:"library/dune-project" "dune-project";
        ]
      "my-lib";
  ]

(* Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let footer_bg = Ansi.Color.grayscale ~level:3
let border_color = Ansi.Color.grayscale ~level:8
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let guide_style_of = function
  | Rounded -> Border.rounded
  | Single -> Border.single
  | Heavy -> Border.heavy
  | Double -> Border.double

let guide_style_name = function
  | Rounded -> "rounded"
  | Single -> "single"
  | Heavy -> "heavy"
  | Double -> "double"

let next_guide_style = function
  | Rounded -> Single
  | Single -> Heavy
  | Heavy -> Double
  | Double -> Rounded

let init () =
  ( {
      state =
        Tree.State.make ~selected:"project"
          ~expanded:
            [
              "project";
              "project/src";
              "project/test";
              "project/doc";
              "library";
              "library/lib";
            ]
          ();
      show_guides = true;
      guide_style_kind = Rounded;
      activated = None;
    },
    Cmd.none )

let update msg model =
  match msg with
  | Select state -> ({ model with state; activated = None }, Cmd.none)
  | Activate item -> ({ model with activated = Some item.label }, Cmd.none)
  | Toggle_guides ->
      ({ model with show_guides = not model.show_guides }, Cmd.none)
  | Toggle_guide_style ->
      ( { model with guide_style_kind = next_guide_style model.guide_style_kind },
        Cmd.none )
  | Quit -> (model, Cmd.quit)

let view model =
  let guide_style = guide_style_of model.guide_style_kind in
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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Tree";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~padding:(padding 1)
        [
          box ~flex_direction:Row ~gap:(gap 2) ~flex_grow:1.
            [
              (* Tree *)
              box ~border:true ~border_color ~flex_grow:1.
                [
                  tree
                    ~size:{ width = pct 100; height = pct 100 }
                    ~focused:true ~items:file_tree ~state:model.state
                    ~show_guides:model.show_guides ~guide_style ~indent_size:2
                    ~guide_color:(Ansi.Color.grayscale ~level:10)
                    ~icon_color:Ansi.Color.cyan ~wrap_selection:true
                    ~on_change:(fun i -> Select i)
                    ~on_activate:(fun i -> Activate i)
                    ();
                ];
              (* Info panel *)
              box ~flex_direction:Column ~gap:(gap 1)
                ~size:{ width = px 28; height = auto }
                [
                  text ~style:(Ansi.Style.make ~bold:true ()) "Info";
                  text ~style:hint
                    (Printf.sprintf "Selected: %s"
                       (Option.value
                          (Tree.State.selected model.state)
                          ~default:"none"));
                  text ~style:hint
                    (Printf.sprintf "Guides: %s"
                       (if model.show_guides then "on" else "off"));
                  text ~style:hint
                    (Printf.sprintf "Style: %s"
                       (guide_style_name model.guide_style_kind));
                  (match model.activated with
                  | Some label ->
                      text
                        ~style:
                          (Ansi.Style.make ~fg:Ansi.Color.green ~bold:true ())
                        (Printf.sprintf "Opened: %s" label)
                  | None -> text ~style:hint "Enter to open");
                ];
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [
          text ~style:hint
            "↑/↓ navigate  •  ←/→ collapse/expand  •  g guides  •  s style  •  \
             q quit";
        ];
    ]

let subscriptions _model =
  Sub.on_keys
    [
      (Shortcut.char 'g', Toggle_guides);
      (Shortcut.char 's', Toggle_guide_style);
      (Shortcut.char 'q', Quit);
      (Shortcut.escape, Quit);
    ]

let () = run { init; update; view; subscriptions }
