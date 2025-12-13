type point = { x : int; y : int }

type local_bounds = {
  anchor_x : int;
  anchor_y : int;
  focus_x : int;
  focus_y : int;
  is_active : bool;
}

type bounds = { x : int; y : int; width : int; height : int }

type t = {
  get_selected_text : unit -> string;
  mutable anchor_provider : unit -> point;
  mutable normalized_anchor : point;
  mutable normalized_focus : point;
  mutable original_focus : point;
  mutable is_active : bool;
  mutable is_selecting : bool;
}

let update_normalized t =
  let anchor = t.anchor_provider () in
  let focus = t.original_focus in
  let anchor_before_focus =
    anchor.y < focus.y || (anchor.y = focus.y && anchor.x <= focus.x)
  in
  if anchor_before_focus then (
    t.normalized_anchor <- anchor;
    t.normalized_focus <- focus)
  else (
    t.normalized_anchor <- focus;
    t.normalized_focus <- { x = anchor.x + 1; y = anchor.y })

let create ?anchor_provider ~anchor ~focus ~get_selected_text () =
  let anchor_provider =
    Option.value anchor_provider ~default:(fun () -> anchor)
  in
  let t =
    {
      anchor_provider;
      get_selected_text;
      normalized_anchor = anchor;
      normalized_focus = focus;
      original_focus = focus;
      is_active = true;
      is_selecting = true;
    }
  in
  update_normalized t;
  t

let refresh t = update_normalized t

let anchor t =
  refresh t;
  t.normalized_anchor

let focus t =
  refresh t;
  t.normalized_focus

let set_focus t value =
  t.original_focus <- value;
  update_normalized t

let set_anchor t value =
  t.anchor_provider <- (fun () -> value);
  update_normalized t

let bounds t =
  let a = anchor t and f = focus t in
  let x0 = min a.x f.x in
  let y0 = min a.y f.y in
  let x1 = max a.x f.x in
  let y1 = max a.y f.y in
  { x = x0; y = y0; width = x1 - x0; height = y1 - y0 }

let is_active t = t.is_active
let set_is_active t value = t.is_active <- value
let is_selecting t = t.is_selecting
let set_is_selecting t value = t.is_selecting <- value
let get_selected_text t = t.get_selected_text ()

let convert_global_to_local selection_opt ~local_origin_x ~local_origin_y =
  match selection_opt with
  | None -> None
  | Some selection when not (is_active selection) -> None
  | Some selection ->
      let anchor = anchor selection in
      let focus = focus selection in
      Some
        {
          anchor_x = anchor.x - local_origin_x;
          anchor_y = anchor.y - local_origin_y;
          focus_x = focus.x - local_origin_x;
          focus_y = focus.y - local_origin_y;
          is_active = true;
        }
