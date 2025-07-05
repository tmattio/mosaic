open Mosaic
module Ui = Mosaic.Ui

type key_binding = { key : string; description : string; enabled : bool }
type key_group = { title : string option; bindings : key_binding list }

type theme = {
  key_style : Style.t;
  desc_style : Style.t;
  separator_style : Style.t;
  group_title_style : Style.t;
  short_separator : string;
  group_separator : string;
  ellipsis : string;
}

let default_theme =
  {
    key_style = Style.(fg (gray 6));
    desc_style = Style.(fg (gray 4));
    separator_style = Style.(fg (gray 3));
    group_title_style = Style.(bold ++ fg (gray 7));
    short_separator = " • ";
    group_separator = "    ";
    ellipsis = "…";
  }

type model = { show_full : bool; width : int; theme : theme }
type msg = ToggleFull

(* Initialization *)

let init ?(show_full = false) ?(width = 80) () =
  let model = { show_full; width = max 1 width; theme = default_theme } in
  (model, Cmd.none)

(* Accessors *)

let is_showing_full model = model.show_full
let width model = model.width

(* Actions *)

let toggle_full model = { model with show_full = not model.show_full }
let toggle model = (model, Cmd.msg ToggleFull)
let show_full show model = { model with show_full = show }
let set_width width model = { model with width = max 1 width }
let with_theme theme model = { model with theme }

(* Utility functions *)

let make_binding ~key ~description ?(enabled = true) () =
  { key; description; enabled }

let make_group ?title bindings = { title; bindings }

(* View helpers *)

let string_width s =
  String.length s (* Simplified - should use proper unicode width *)

let render_binding theme binding =
  let key_elem = Ui.text ~style:theme.key_style binding.key in
  let desc_elem = Ui.text ~style:theme.desc_style binding.description in
  Ui.hbox ~gap:1 [ key_elem; desc_elem ]

let should_render_binding binding = binding.enabled
let should_render_group group = List.exists should_render_binding group.bindings

(* Short help view *)

let view_short bindings model =
  let theme = model.theme in
  let enabled_bindings = List.filter should_render_binding bindings in

  if enabled_bindings = [] then Ui.text ""
  else
    let separator =
      Ui.text ~style:theme.separator_style theme.short_separator
    in
    let items = ref [] in
    let current_width = ref 0 in

    List.iter
      (fun binding ->
        let item_text = binding.key ^ " " ^ binding.description in
        let item_width = string_width item_text in
        let sep_width = string_width theme.short_separator in

        (* Check if adding this item would exceed width *)
        if
          !current_width > 0
          && !current_width + sep_width + item_width > model.width
        then (
          if
            (* Add ellipsis and stop *)
            !current_width + string_width theme.ellipsis <= model.width
          then
            items :=
              Ui.text ~style:theme.separator_style theme.ellipsis :: !items)
        (* Break the loop by not adding more items *)
          else (
          (* Add separator if not first item *)
          if !current_width > 0 then (
            items := separator :: !items;
            current_width := !current_width + sep_width);
          (* Add the binding *)
          items := render_binding theme binding :: !items;
          current_width := !current_width + item_width))
      enabled_bindings;

    Ui.hbox (List.rev !items)

(* Full help view *)

let render_group theme group =
  let enabled_bindings = List.filter should_render_binding group.bindings in
  if enabled_bindings = [] then []
  else
    let title_elems =
      match group.title with
      | Some title -> [ Ui.text ~style:theme.group_title_style title ]
      | None -> []
    in

    (* Find max key width for alignment *)
    let max_key_width =
      List.fold_left
        (fun acc binding -> max acc (string_width binding.key))
        0 enabled_bindings
    in

    let binding_elems =
      List.map
        (fun binding ->
          let key_padded =
            binding.key
            ^ String.make (max_key_width - string_width binding.key) ' '
          in
          let key_elem = Ui.text ~style:theme.key_style key_padded in
          let desc_elem = Ui.text ~style:theme.desc_style binding.description in
          Ui.hbox ~gap:1 [ key_elem; desc_elem ])
        enabled_bindings
    in

    title_elems @ binding_elems

let view_full groups model =
  let theme = model.theme in
  let renderable_groups = List.filter should_render_group groups in

  if renderable_groups = [] then Ui.text ""
  else
    (* Render each group as a column *)
    let columns =
      List.map
        (fun group ->
          let lines = render_group theme group in
          Ui.vbox lines)
        renderable_groups
    in

    (* Join columns with separator *)
    let separator = Ui.text theme.group_separator in
    let rec join_columns cols =
      match cols with
      | [] -> Ui.text ""
      | [ col ] -> col
      | col :: rest -> Ui.hbox [ col; separator; join_columns rest ]
    in

    join_columns columns

(* Update *)

let update msg model =
  match msg with ToggleFull -> (toggle_full model, Cmd.none)

(* View *)

let view _ = Ui.text "" (* Help component doesn't have its own view *)

(* Subscriptions *)

let subscriptions _ = Sub.none

(* Redefine component with actual functions *)
let component = Mosaic.app ~init ~update ~view ~subscriptions ()
