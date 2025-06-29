type 'msg keyboard_sub = Input.key_event -> 'msg option
type 'msg mouse_sub = Input.mouse_event -> 'msg option
type window_size = { width : int; height : int }
type 'msg window_sub = window_size -> 'msg option
type 'msg focus_sub = unit -> 'msg option
type 'msg blur_sub = unit -> 'msg option

type 'msg t =
  | None
  | Keyboard of 'msg keyboard_sub
  | Mouse of 'msg mouse_sub
  | Window of 'msg window_sub
  | Focus of 'msg focus_sub
  | Blur of 'msg blur_sub
  | Batch of 'msg t list

let none = None
let keyboard f = Keyboard (fun k -> Some (f k))
let keyboard_filter f = Keyboard f
let mouse f = Mouse (fun m -> Some (f m))
let mouse_filter f = Mouse f
let window f = Window (fun w -> Some (f w))
let window_filter f = Window f
let focus f = Focus (fun () -> Some (f ()))
let blur f = Blur (fun () -> Some (f ()))

let mouse_motion f =
  mouse_filter (function
    | Input.Motion (x, y, _, _) -> Some (f x y)
    | _ -> None)

let mouse_click f =
  mouse_filter (function Input.Press (x, y, _, _) -> Some (f x y) | _ -> None)

let window_resize f = window (fun size -> f size.width size.height)

(* Helper functions for specific events *)
let on_key ?ctrl ?alt ?shift key msg =
  let ctrl = Option.value ctrl ~default:false in
  let alt = Option.value alt ~default:false in
  let shift = Option.value shift ~default:false in
  keyboard_filter (fun event ->
      if
        event.key = key && event.modifier.ctrl = ctrl
        && event.modifier.alt = alt
        && event.modifier.shift = shift
      then Some msg
      else None)

let on_char ?ctrl ?alt ?shift c msg =
  let ctrl = Option.value ctrl ~default:false in
  let alt = Option.value alt ~default:false in
  let shift = Option.value shift ~default:false in
  keyboard_filter (fun event ->
      match event.key with
      | Input.Char uchar
        when Uchar.to_char uchar = c
             && event.modifier.ctrl = ctrl && event.modifier.alt = alt
             && event.modifier.shift = shift ->
          Some msg
      | _ -> None)

let on_click f =
  mouse_filter (function
    | Input.Press (x, y, button, _) -> Some (f x y button)
    | _ -> None)

let on_resize f = window_resize f
let on_focus msg = focus (fun () -> msg)
let on_blur msg = blur (fun () -> msg)

let batch subs =
  match List.filter (function None -> false | _ -> true) subs with
  | [] -> None
  | [ sub ] -> sub
  | subs -> Batch subs

let rec map f sub =
  match sub with
  | None -> None
  | Keyboard fn -> Keyboard (fun k -> Option.map f (fn k))
  | Mouse fn -> Mouse (fun m -> Option.map f (fn m))
  | Window fn -> Window (fun w -> Option.map f (fn w))
  | Focus fn -> Focus (fun () -> Option.map f (fn ()))
  | Blur fn -> Blur (fun () -> Option.map f (fn ()))
  | Batch subs -> batch (List.map (map f) subs)

let rec collect_keyboard acc = function
  | None -> acc
  | Keyboard fn -> fn :: acc
  | Mouse _ | Window _ | Focus _ | Blur _ -> acc
  | Batch subs -> List.fold_left collect_keyboard acc subs

let rec collect_mouse acc = function
  | None -> acc
  | Mouse fn -> fn :: acc
  | Keyboard _ | Window _ | Focus _ | Blur _ -> acc
  | Batch subs -> List.fold_left collect_mouse acc subs

let rec collect_window acc = function
  | None -> acc
  | Window fn -> fn :: acc
  | Keyboard _ | Mouse _ | Focus _ | Blur _ -> acc
  | Batch subs -> List.fold_left collect_window acc subs

let rec collect_focus acc = function
  | None -> acc
  | Focus fn -> fn :: acc
  | Keyboard _ | Mouse _ | Window _ | Blur _ -> acc
  | Batch subs -> List.fold_left collect_focus acc subs

let rec collect_blur acc = function
  | None -> acc
  | Blur fn -> fn :: acc
  | Keyboard _ | Mouse _ | Window _ | Focus _ -> acc
  | Batch subs -> List.fold_left collect_blur acc subs
