(** Implementation of the confirmation component *)

open Mosaic

(* Theme configuration *)
type theme = {
  focused_style : Style.t;
  blurred_style : Style.t;
  selected_style : Style.t;
  unselected_style : Style.t;
  submitted_style : Style.t;
}

let default_theme =
  {
    focused_style = Style.(fg (Index 6));
    blurred_style = Style.(fg (Index 8));
    selected_style = Style.(bg (Index 6) ++ fg Black);
    unselected_style = Style.empty;
    submitted_style = Style.(fg Green);
  }

(* Model *)
type model = {
  (* Configuration *)
  affirmative : string;
  negative : string;
  inline : bool;
  (* State *)
  current_selection : bool; (* true = affirmative, false = negative *)
  is_submitted : bool;
  is_focused : bool;
  (* Theme *)
  theme : theme;
}

(* Messages *)
type msg =
  | Move_left
  | Move_right
  | Toggle
  | Submit
  | Quick_select of char
  | Focus
  | Blur
  | Select of bool

(* Initialization *)
let init ?(affirmative = "Yes") ?(negative = "No") ?(default = true)
    ?(inline = false) () =
  let model =
    {
      affirmative;
      negative;
      inline;
      current_selection = default;
      is_submitted = false;
      is_focused = false;
      theme = default_theme;
    }
  in
  (model, Cmd.none)

(* Update *)
let update msg model =
  if model.is_submitted then
    (* Once submitted, ignore most interactions except blur/focus *)
    match msg with
    | Focus -> ({ model with is_focused = true }, Cmd.none)
    | Blur -> ({ model with is_focused = false }, Cmd.none)
    | _ -> (model, Cmd.none)
  else
    match msg with
    | Move_left -> ({ model with current_selection = true }, Cmd.none)
    | Move_right -> ({ model with current_selection = false }, Cmd.none)
    | Toggle ->
        (* Toggle selection *)
        ( { model with current_selection = not model.current_selection },
          Cmd.none )
    | Submit -> ({ model with is_submitted = true }, Cmd.none)
    | Quick_select char ->
        (* Quick select with first letter *)
        if char = Char.lowercase_ascii model.affirmative.[0] then
          ( { model with current_selection = true; is_submitted = true },
            Cmd.none )
        else if char = Char.lowercase_ascii model.negative.[0] then
          ( { model with current_selection = false; is_submitted = true },
            Cmd.none )
        else (model, Cmd.none)
    | Focus -> ({ model with is_focused = true }, Cmd.none)
    | Blur -> ({ model with is_focused = false }, Cmd.none)
    | Select selection ->
        ({ model with current_selection = selection }, Cmd.none)

(* View *)
let view model =
  let open Ui in
  (* Options *)
  let option_style submitted selected focused =
    if submitted && selected then model.theme.submitted_style
    else if selected && focused then model.theme.selected_style
    else if selected then model.theme.focused_style
    else if focused then model.theme.focused_style
    else model.theme.unselected_style
  in

  let affirmative_style =
    option_style model.is_submitted model.current_selection model.is_focused
  in

  let negative_style =
    option_style model.is_submitted
      (not model.current_selection)
      model.is_focused
  in

  let prefix selected = if selected then "▸ " else "  " in

  let affirmative_option =
    text ~style:affirmative_style
      (prefix model.current_selection ^ model.affirmative)
  in

  let negative_option =
    text ~style:negative_style
      (prefix (not model.current_selection) ^ model.negative)
  in

  (* Layout *)
  if model.inline then hbox ~gap:4 [ affirmative_option; negative_option ]
  else vbox [ affirmative_option; negative_option ]

(* Subscriptions *)
let subscriptions model =
  if model.is_focused then
    Sub.batch
      [
        Sub.on_left Move_left;
        Sub.on_up Move_left;
        Sub.on_right Move_right;
        Sub.on_down Move_right;
        Sub.on_tab Toggle;
        Sub.on_enter Submit;
        Sub.keyboard_filter (fun event ->
            match event.key with
            | Char c when Uchar.to_int c >= 32 && Uchar.to_int c < 127 ->
                Some (Quick_select (Char.lowercase_ascii (Uchar.to_char c)))
            | _ -> None);
      ]
  else Sub.none

(* Component export *)
let component = Mosaic.app ~init ~update ~view ~subscriptions ()

(* Accessors *)
let value model =
  if model.is_submitted then Some model.current_selection else None

let is_confirmed model = model.is_submitted && model.current_selection
let is_denied model = model.is_submitted && not model.current_selection
let is_submitted model = model.is_submitted
let is_focused model = model.is_focused
let current_selection model = model.current_selection

(* Actions *)
let focus model = ({ model with is_focused = true }, Cmd.msg Focus)
let blur model = ({ model with is_focused = false }, Cmd.msg Blur)
let reset model = { model with is_submitted = false }

let select_affirmative model =
  if not model.is_submitted then (model, Cmd.msg (Select true))
  else (model, Cmd.none)

let select_negative model =
  if not model.is_submitted then (model, Cmd.msg (Select false))
  else (model, Cmd.none)

let submit model =
  if not model.is_submitted then (model, Cmd.msg Submit) else (model, Cmd.none)

(* Theming *)
let with_theme theme model = { model with theme }

(* Re-export for component interface *)
let update = update
let view = view
let subscriptions = subscriptions
