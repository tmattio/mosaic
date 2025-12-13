(** Multi-component form with focus management. *)

open Mosaic_tea

type model = { name : string; email : string; role : int; submitted : bool }

type msg =
  | Submit
  | Reset
  | Quit
  | Set_role of int
  | Set_name of string
  | Set_email of string

let roles =
  [
    Select.{ name = "Developer"; description = Some "Write code" };
    { name = "Designer"; description = Some "Create designs" };
    { name = "Manager"; description = Some "Lead teams" };
    { name = "Analyst"; description = Some "Analyze data" };
  ]

let init () = ({ name = ""; email = ""; role = 0; submitted = false }, Cmd.none)

let update msg model =
  match msg with
  | Submit -> ({ model with submitted = true }, Cmd.none)
  | Reset -> init ()
  | Set_role role -> ({ model with role }, Cmd.none)
  | Set_name name -> ({ model with name; submitted = false }, Cmd.none)
  | Set_email email -> ({ model with email; submitted = false }, Cmd.none)
  | Quit -> (model, Cmd.quit)

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
              text ~content:"▸ Form"
                ~text_style:(Ansi.Style.make ~bold:true ())
                ();
              text ~content:"▄▀ mosaic" ~text_style:muted ();
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [
          box ~flex_direction:Column ~gap:(gap 2) ~border:true ~border_color
            ~padding:(padding 2)
            [
              text ~content:"Registration"
                ~text_style:(Ansi.Style.make ~bold:true ())
                ();
              (* Name field *)
              box ~flex_direction:Row ~align_items:Center ~gap:(gap 1)
                [
                  box
                    ~size:{ width = px 8; height = px 1 }
                    [ text ~content:"Name:" () ];
                  text_input ~placeholder:"Enter your name..."
                    ~size:{ width = px 25; height = px 1 }
                    ~value:model.name
                    ~on_input:(fun v -> Some (Set_name v))
                    ();
                ];
              (* Email field *)
              box ~flex_direction:Row ~align_items:Center ~gap:(gap 1)
                [
                  box
                    ~size:{ width = px 8; height = px 1 }
                    [ text ~content:"Email:" () ];
                  text_input ~placeholder:"Enter your email..."
                    ~size:{ width = px 25; height = px 1 }
                    ~value:model.email
                    ~on_input:(fun v -> Some (Set_email v))
                    ();
                ];
              (* Role selector *)
              box ~flex_direction:Row ~align_items:Flex_start ~gap:(gap 1)
                [
                  box
                    ~size:{ width = px 8; height = px 1 }
                    [ text ~content:"Role:" () ];
                  select ~options:roles ~show_description:true
                    ~selected_background:accent
                    ~selected_text_color:Ansi.Color.black
                    ~selected_index:model.role
                    ~on_change:(fun idx -> Some (Set_role idx))
                    ~size:{ width = px 25; height = px 5 }
                    ();
                ];
              (* Buttons *)
              box ~flex_direction:Row ~gap:(gap 2)
                [
                  box ~border:true ~border_color ~padding:(padding 1)
                    ~on_mouse:(fun ev ->
                      match Mosaic_ui.Event.Mouse.kind ev with
                      | Down -> Some Submit
                      | _ -> None)
                    [ text ~content:"Submit" () ];
                  box ~border:true ~border_color ~padding:(padding 1)
                    ~on_mouse:(fun ev ->
                      match Mosaic_ui.Event.Mouse.kind ev with
                      | Down -> Some Reset
                      | _ -> None)
                    [ text ~content:"Reset" () ];
                ];
              (* Submission feedback *)
              (if model.submitted then
                 box ~padding:(padding 1)
                   ~background:(Ansi.Color.grayscale ~level:3)
                   [
                     text
                       ~content:
                         (Printf.sprintf "Submitted: %s <%s> as %s" model.name
                            model.email (List.nth roles model.role).name)
                       ~text_style:(Ansi.Style.make ~fg:Ansi.Color.green ())
                       ();
                   ]
               else
                 text ~content:"Fill in the form and click Submit"
                   ~text_style:hint ());
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [
          text ~content:"Tab navigate  •  Enter submit  •  q quit"
            ~text_style:hint ();
        ];
    ]

let subscriptions _model =
  Sub.on_key (fun ev ->
      match (Mosaic_ui.Event.Key.data ev).key with
      | Enter -> Some Submit
      | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
      | Escape -> Some Quit
      | _ -> None)

let () = run { init; update; view; subscriptions }
