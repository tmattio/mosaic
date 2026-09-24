(** Multi-component form with focus management. *)

open Mosaic

type role = Developer | Designer | Manager | Analyst

type model = {
  name : Editor.t;
  email : Editor.t;
  role : role option;
  submitted : bool;
  focus : int; (* index into all_fields *)
}

type msg =
  | Submit
  | Reset
  | Quit
  | Set_role of role
  | Focus of int
  | Set_name of Editor.t
  | Set_email of Editor.t
  | Focus_next
  | Focus_prev

let role_label = function
  | Developer -> "Developer"
  | Designer -> "Designer"
  | Manager -> "Manager"
  | Analyst -> "Analyst"

let roles =
  [
    Select.item Developer ~description:"Write code" "Developer";
    Select.item Designer ~description:"Create designs" "Designer";
    Select.item Manager ~description:"Lead teams" "Manager";
    Select.item Analyst ~description:"Analyze data" "Analyst";
  ]

let init () =
  ( {
      name = Editor.empty;
      email = Editor.empty;
      role = Some Developer;
      submitted = false;
      focus = 0;
    },
    Cmd.none )

let num_fields = 3

let update msg model =
  match msg with
  | Submit -> ({ model with submitted = true }, Cmd.none)
  | Reset ->
      ( {
          name = Editor.empty;
          email = Editor.empty;
          role = Some Developer;
          submitted = false;
          focus = 0;
        },
        Cmd.none )
  | Set_role role -> ({ model with role = Some role }, Cmd.none)
  | Focus focus -> ({ model with focus }, Cmd.none)
  | Set_name name ->
      let submitted =
        model.submitted
        && String.equal (Editor.text model.name) (Editor.text name)
      in
      ({ model with name; submitted }, Cmd.none)
  | Set_email email ->
      let submitted =
        model.submitted
        && String.equal (Editor.text model.email) (Editor.text email)
      in
      ({ model with email; submitted }, Cmd.none)
  | Quit -> (model, Cmd.quit)
  | Focus_next ->
      let next = (model.focus + 1) mod num_fields in
      ({ model with focus = next }, Cmd.none)
  | Focus_prev ->
      let prev = (model.focus - 1 + num_fields) mod num_fields in
      ({ model with focus = prev }, Cmd.none)

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
              text ~style:(Ansi.Style.make ~bold:true ()) "▸ Form";
              text ~style:muted "▄▀ mosaic";
            ];
        ];
      (* Content *)
      box ~flex_grow:1. ~align_items:Center ~justify_content:Center
        [
          box ~flex_direction:Column ~gap:(gap 2) ~border:true ~border_color
            ~padding:(padding 2)
            [
              text ~style:(Ansi.Style.make ~bold:true ()) "Registration";
              (* Name field *)
              box ~flex_direction:Row ~align_items:Center ~gap:(gap 1)
                [
                  box ~size:{ width = px 8; height = px 1 } [ text "Name:" ];
                  input
                    ~size:{ width = px 25; height = px 1 }
                    ~focused:(model.focus = 0) ~on_focus_request:(Focus 0)
                    ~placeholder:"Enter your name..." ~state:model.name
                    ~on_change:(fun v -> Set_name v)
                    ();
                ];
              (* Email field *)
              box ~flex_direction:Row ~align_items:Center ~gap:(gap 1)
                [
                  box ~size:{ width = px 8; height = px 1 } [ text "Email:" ];
                  input
                    ~size:{ width = px 25; height = px 1 }
                    ~focused:(model.focus = 1) ~on_focus_request:(Focus 1)
                    ~placeholder:"Enter your email..." ~state:model.email
                    ~on_change:(fun v -> Set_email v)
                    ();
                ];
              (* Role selector *)
              box ~flex_direction:Row ~align_items:Flex_start ~gap:(gap 1)
                [
                  box ~size:{ width = px 8; height = px 1 } [ text "Role:" ];
                  select
                    ~size:{ width = px 25; height = px 5 }
                    ~focused:(model.focus = 2) ~on_focus_request:(Focus 2)
                    ~show_description:true ~selected_background:accent
                    ~selected_text_color:Ansi.Color.black ~equal:( = )
                    ~selected:model.role
                    ~on_change:(fun role -> Set_role role)
                    roles;
                ];
              (* Buttons *)
              box ~flex_direction:Row ~gap:(gap 2)
                [
                  box ~border:true ~border_color ~padding:(padding 1)
                    ~on_mouse:(fun ev ->
                      match Event.Mouse.kind ev with
                      | Down _ -> Some Submit
                      | _ -> None)
                    [ text "Submit" ];
                  box ~border:true ~border_color ~padding:(padding 1)
                    ~on_mouse:(fun ev ->
                      match Event.Mouse.kind ev with
                      | Down _ -> Some Reset
                      | _ -> None)
                    [ text "Reset" ];
                ];
              (* Submission feedback *)
              (if model.submitted then
                 box ~padding:(padding 1)
                   ~background:(Ansi.Color.grayscale ~level:3)
                   [
                     text
                       ~style:(Ansi.Style.make ~fg:Ansi.Color.green ())
                       (Printf.sprintf "Submitted: %s <%s> as %s"
                          (Editor.text model.name) (Editor.text model.email)
                          (Option.fold ~none:"no role" ~some:role_label
                             model.role));
                   ]
               else text ~style:hint "Fill in the form and click Submit");
            ];
        ];
      (* Footer *)
      box ~padding:(padding 1) ~background:footer_bg
        [
          text ~style:hint
            "Tab/Shift+Tab navigate  •  Enter submit  •  Ctrl+q quit";
        ];
    ]

let subscriptions _model =
  Sub.batch
    [
      Sub.on_key (fun ev ->
          let data = Event.Key.data ev in
          match data.key with
          | Tab ->
              Event.Key.prevent_default ev;
              if data.modifier.shift then Some Focus_prev else Some Focus_next
          | _ -> None);
      Sub.on_keys
        [
          (Shortcut.enter, Submit);
          (Shortcut.ctrl 'q', Quit);
          (Shortcut.escape, Quit);
        ];
    ]

let () = run { init; update; view; subscriptions }
