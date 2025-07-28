open Mosaic
module Ui = Mosaic.Ui

type style = Dots | Numbers | Compact

type theme = {
  active_dot_style : Style.t;
  inactive_dot_style : Style.t;
  number_style : Style.t;
  active_dot : string;
  inactive_dot : string;
}

let default_theme =
  {
    active_dot_style = Style.(fg (Index 33));
    inactive_dot_style = Style.(fg (gray 8));
    number_style = Style.empty;
    active_dot = "●";
    inactive_dot = "○";
  }

type model = {
  current_page : int;
  total_items : int;
  items_per_page : int;
  style : style;
  theme : theme;
}

type msg (* Paginator is controlled externally, no internal messages *)

(* Helper functions *)

let calculate_total_pages total_items items_per_page =
  if total_items <= 0 || items_per_page <= 0 then 0
  else (total_items + items_per_page - 1) / items_per_page

let clamp_page page total_pages =
  if total_pages <= 0 then 0 else max 0 (min (total_pages - 1) page)

(* Initialization *)

let init ?(total_items = 0) ?(items_per_page = 10) ?(current_page = 0)
    ?(style = Dots) () =
  let items_per_page = max 1 items_per_page in
  let total_pages = calculate_total_pages total_items items_per_page in
  let current_page = clamp_page current_page total_pages in
  let model =
    {
      current_page;
      total_items = max 0 total_items;
      items_per_page;
      style;
      theme = default_theme;
    }
  in
  (model, Cmd.none)

(* Accessors *)

let current_page model = model.current_page

let total_pages model =
  calculate_total_pages model.total_items model.items_per_page

let items_per_page model = model.items_per_page
let total_items model = model.total_items
let on_first_page model = model.current_page = 0

let on_last_page model =
  let total = total_pages model in
  total = 0 || model.current_page >= total - 1

let slice_bounds model =
  let start = model.current_page * model.items_per_page in
  let end_ = min model.total_items (start + model.items_per_page) in
  (start, end_)

let items_on_page model =
  let start, end_ = slice_bounds model in
  end_ - start

(* Actions *)

let next_page model =
  let total = total_pages model in
  if model.current_page < total - 1 then
    { model with current_page = model.current_page + 1 }
  else model

let prev_page model =
  if model.current_page > 0 then
    { model with current_page = model.current_page - 1 }
  else model

let go_to_page page model =
  let total = total_pages model in
  { model with current_page = clamp_page page total }

let first_page model = { model with current_page = 0 }

let last_page model =
  let total = total_pages model in
  { model with current_page = max 0 (total - 1) }

let set_total_items items model =
  let total_items = max 0 items in
  let total = calculate_total_pages total_items model.items_per_page in
  let current_page = clamp_page model.current_page total in
  { model with total_items; current_page }

let set_items_per_page items model =
  let items_per_page = max 1 items in
  let total = calculate_total_pages model.total_items items_per_page in
  (* Try to maintain position in the item list *)
  let first_item_on_page = model.current_page * model.items_per_page in
  let new_page = first_item_on_page / items_per_page in
  let current_page = clamp_page new_page total in
  { model with items_per_page; current_page }

let set_style style model = { model with style }
let with_theme theme model = { model with theme }

(* Update *)

let update _msg model = (model, Cmd.none)

(* View *)

let render_dots model =
  let total = total_pages model in
  if total <= 1 then Ui.text ""
  else
    let dots =
      List.init total (fun i ->
          if i = model.current_page then
            Ui.text ~style:model.theme.active_dot_style model.theme.active_dot
          else
            Ui.text ~style:model.theme.inactive_dot_style
              model.theme.inactive_dot)
    in
    Ui.hbox dots

let render_numbers model =
  let total = total_pages model in
  if total <= 1 then Ui.text ""
  else
    let text = Printf.sprintf "Page %d of %d" (model.current_page + 1) total in
    Ui.text ~style:model.theme.number_style text

let render_compact model =
  let total = total_pages model in
  if total <= 1 then Ui.text ""
  else
    let text = Printf.sprintf "%d/%d" (model.current_page + 1) total in
    Ui.text ~style:model.theme.number_style text

let view model =
  match model.style with
  | Dots -> render_dots model
  | Numbers -> render_numbers model
  | Compact -> render_compact model

(* Subscriptions *)

let subscriptions _ = Sub.none

(* Redefine component with actual functions *)
let component = Mosaic.app ~init ~update ~view ~subscriptions ()
