(** Scrollable content with scroll bars. *)

open Mosaic_tea

type msg = Quit

let init () = ((), Cmd.none)
let update msg () = match msg with Quit -> ((), Cmd.quit)

let items =
  List.init 50 (fun i ->
      Printf.sprintf "%2d. Item number %d - This is a sample list item" (i + 1)
        (i + 1))

let view () =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      (* Header *)
      box ~padding:(padding 1) ~background:Ansi.Color.blue
        [
          text ~content:"Scrollable List"
            ~text_style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.white ())
            ();
        ];
      (* Scrollable content *)
      box ~flex_grow:1. ~padding:(padding 1)
        [
          box ~border:true ~title:"Items" ~flex_grow:1.
            [
              scroll_box ~scroll_y:true ~scroll_x:false
                ~size:{ width = pct 100; height = pct 100 }
                (List.mapi
                   (fun i item ->
                     box ~key:(string_of_int i) ~padding:(padding 1)
                       ~background:
                         (if i mod 2 = 0 then Ansi.Color.default
                          else Ansi.Color.grayscale ~level:3)
                       [ text ~content:item () ])
                   items);
            ];
        ];
      (* Help *)
      box ~padding:(padding 1)
        [ text ~content:"Use mouse wheel to scroll, 'q' to quit" () ];
    ]

let subscriptions () =
  Sub.on_key (fun ev ->
      match (Mosaic_ui.Event.Key.data ev).key with
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
