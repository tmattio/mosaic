(** Implementation of the note/message display component *)

open Mosaic

(* Note types *)
type kind =
  [ `Info | `Success | `Warning | `Error | `Custom of Style.color * string ]

(* Theme configuration *)
type theme = {
  border_style : Ui.border_style;
  title_style : Style.t;
  info_style : Style.t;
  success_style : Style.t;
  warning_style : Style.t;
  error_style : Style.t;
  dismiss_style : Style.t;
}

let default_theme =
  {
    border_style = Ui.Rounded;
    title_style = Style.bold;
    info_style = Style.(fg (Index 6));
    success_style = Style.(fg Green);
    warning_style = Style.(fg Yellow);
    error_style = Style.(fg Red);
    dismiss_style = Style.(fg (Index 8));
  }

(* Model *)
type model = {
  (* Configuration *)
  title : string option;
  text : string;
  kind : kind;
  dismissible : bool;
  width : int option;
  (* State *)
  is_dismissed : bool;
  (* Theme *)
  theme : theme;
}

(* Messages *)
type msg = Dismiss | Show

(* Get icon and color for note kind *)
let kind_info kind theme =
  match kind with
  | `Info -> ("ℹ", theme.info_style, Style.Index 6)
  | `Success -> ("✓", theme.success_style, Style.Green)
  | `Warning -> ("⚠", theme.warning_style, Style.Yellow)
  | `Error -> ("✗", theme.error_style, Style.Red)
  | `Custom (color, icon) -> (icon, Style.(fg color), color)

(* Initialization *)
let init ?title ?(text = "") ?(kind = `Info) ?(dismissible = false) ?width () =
  let model =
    {
      title;
      text;
      kind;
      dismissible;
      width;
      is_dismissed = false;
      theme = default_theme;
    }
  in
  (model, Cmd.none)

(* Update *)
let update msg model =
  match msg with
  | Dismiss ->
      if model.dismissible then ({ model with is_dismissed = true }, Cmd.none)
      else (model, Cmd.none)
  | Show -> ({ model with is_dismissed = false }, Cmd.none)

(* View *)
let view model =
  let open Ui in
  if model.is_dismissed then space 0
  else
    let icon, icon_style, border_color = kind_info model.kind model.theme in

    (* Icon *)
    let icon_elem = text ~style:icon_style icon in

    (* Title *)
    let title_elem =
      match model.title with
      | Some t -> [ text ~style:model.theme.title_style t ]
      | None -> []
    in

    (* Text content *)
    let text_elem = text model.text in

    (* Dismiss button *)
    let dismiss_elem =
      if model.dismissible then
        [ spacer ~flex:1 0; text ~style:model.theme.dismiss_style "[×]" ]
      else []
    in

    (* Content layout *)
    let content =
      if title_elem = [] then
        (* No title: icon and text on same line *)
        hbox ~gap:1 ([ icon_elem; text_elem ] @ dismiss_elem)
      else
        (* With title: icon next to title, text below *)
        vbox
          [
            hbox ~gap:1 ([ icon_elem ] @ title_elem @ dismiss_elem);
            hbox [ space 2; text_elem ];
          ]
    in

    (* Main container *)
    let container =
      vbox ~padding:(padding_all 1)
        ~border:(border ~style:model.theme.border_style ~color:border_color ())
        [ content ]
    in

    match model.width with
    | Some w -> hbox ~width:w [ container ]
    | None -> container

(* Subscriptions *)
let subscriptions _ = Sub.none

(* Component export *)
let component = Mosaic.app ~init ~update ~view ~subscriptions ()

(* Accessors *)
let is_dismissed model = model.is_dismissed
let kind model = model.kind
let title model = model.title
let text model = model.text

(* Actions *)
let dismiss model =
  if model.dismissible then (model, Cmd.msg Dismiss) else (model, Cmd.none)

let show model = (model, Cmd.msg Show)

let set_content ?title ?text model =
  let title = match title with Some t -> Some t | None -> model.title in
  let text = Option.value ~default:model.text text in
  { model with title; text }

let set_kind kind model = { model with kind }

(* Theming *)
let with_theme theme model = { model with theme }
