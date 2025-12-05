(** Styled text demonstration with colors and formatting. *)

open Mosaic_tea

type wrap_mode = None | Char | Word
type model = { wrap : wrap_mode }
type msg = Cycle_wrap | Quit

let init () = ({ wrap = Word }, Cmd.none)

let update msg model =
  match msg with
  | Cycle_wrap ->
      let wrap =
        match model.wrap with None -> Char | Char -> Word | Word -> None
      in
      ({ wrap }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let wrap_mode_to_string = function
  | None -> "None"
  | Char -> "Char"
  | Word -> "Word"

let wrap_mode_to_prop = function None -> `None | Char -> `Char | Word -> `Word

let view model =
  box ~flex_direction:Column ~padding:(padding 1)
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Section: Basic styles *)
      box ~border:true ~padding:(padding 1) ~title:"Text Styles"
        ~flex_direction:Column ~gap:(gap 1)
        [
          text
            ~content:
              (String.concat ""
                 [ "Fragments allow "; "nested "; "styles with inheritance." ])
            ();
          text ~content:"Bold text"
            ~text_style:(Ansi.Style.make ~bold:true ())
            ();
          text ~content:"Italic text"
            ~text_style:(Ansi.Style.make ~italic:true ())
            ();
          text ~content:"Underlined text"
            ~text_style:(Ansi.Style.make ~underline:true ())
            ();
          text ~content:"Strikethrough text"
            ~text_style:(Ansi.Style.make ~strikethrough:true ())
            ();
          text ~content:"Dim text" ~text_style:(Ansi.Style.make ~dim:true ()) ();
        ];
      (* Section: Colors *)
      box ~border:true ~padding:(padding 1) ~title:"Colors" ~flex_direction:Row
        ~gap:(gap 2) ~margin:(margin 1)
        [
          text ~content:"Red"
            ~text_style:(Ansi.Style.make ~fg:Ansi.Color.red ())
            ();
          text ~content:"Green"
            ~text_style:(Ansi.Style.make ~fg:Ansi.Color.green ())
            ();
          text ~content:"Blue"
            ~text_style:(Ansi.Style.make ~fg:Ansi.Color.blue ())
            ();
          text ~content:"Yellow"
            ~text_style:(Ansi.Style.make ~fg:Ansi.Color.yellow ())
            ();
          text ~content:"Cyan"
            ~text_style:(Ansi.Style.make ~fg:Ansi.Color.cyan ())
            ();
          text ~content:"Magenta"
            ~text_style:(Ansi.Style.make ~fg:Ansi.Color.magenta ())
            ();
          text ~content:" BG "
            ~text_style:
              (Ansi.Style.make ~fg:Ansi.Color.black ~bg:Ansi.Color.white ())
            ();
        ];
      (* Section: Grayscale colors *)
      box ~border:true ~padding:(padding 1) ~title:"Grayscale"
        ~flex_direction:Row ~gap:(gap 1) ~margin:(margin 1)
        (List.init 8 (fun i ->
             let level = i * 3 in
             let color = Ansi.Color.grayscale ~level in
             text
               ~content:(Printf.sprintf " %02d " level)
               ~text_style:(Ansi.Style.make ~bg:color ~fg:Ansi.Color.white ())
               ()));
      (* Section: Wrap modes *)
      box ~border:true ~padding:(padding 1)
        ~title:(Printf.sprintf "Wrap Mode: %s" (wrap_mode_to_string model.wrap))
        ~margin:(margin 1) ~flex_direction:Column
        ~size:{ width = px 50; height = auto }
        [
          text
            ~content:
              "This is a longer text that demonstrates different wrapping \
               behaviors. Press 'w' to cycle through None, Char, and Word wrap \
               modes."
            ~size:{ width = pct 100; height = auto }
            ~wrap_mode:(wrap_mode_to_prop model.wrap)
            ();
        ];
      (* Help *)
      text ~content:"Press 'w' to cycle wrap mode, 'q' to quit" ();
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Mosaic_ui.Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'w') -> Some Cycle_wrap
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
