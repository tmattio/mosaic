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

let on_mouse_motion f =
  mouse_filter (function
    | Input.Motion (x, y, _, _) -> Some (f x y)
    | _ -> None)

let on_mouse_click f =
  mouse_filter (function
    | Input.Button_press (x, y, _, _) -> Some (f x y)
    | _ -> None)

let on_resize f = window (fun size -> f size.width size.height)

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
    | Input.Button_press (x, y, button, _) -> Some (f x y button)
    | _ -> None)

let on_left_click f =
  mouse_filter (function
    | Input.Button_press (x, y, Input.Left, _) -> Some (f x y)
    | _ -> None)

let on_right_click f =
  mouse_filter (function
    | Input.Button_press (x, y, Input.Right, _) -> Some (f x y)
    | _ -> None)

let on_scroll_up f =
  mouse_filter (function
    | Input.Button_press (x, y, Input.Wheel_up, _) -> Some (f x y)
    | _ -> None)

let on_scroll_down f =
  mouse_filter (function
    | Input.Button_press (x, y, Input.Wheel_down, _) -> Some (f x y)
    | _ -> None)

let on_focus msg = focus (fun () -> msg)
let on_blur msg = blur (fun () -> msg)

(* Common key event shorthands *)
let on_enter msg = on_key Input.Enter msg
let on_escape msg = on_key Input.Escape msg
let on_tab msg = on_key Input.Tab msg
let on_backspace msg = on_key Input.Backspace msg
let on_delete msg = on_key Input.Delete msg
let on_up msg = on_key Input.Up msg
let on_down msg = on_key Input.Down msg
let on_left msg = on_key Input.Left msg
let on_right msg = on_key Input.Right msg
let on_page_up msg = on_key Input.Page_up msg
let on_page_down msg = on_key Input.Page_down msg
let on_home msg = on_key Input.Home msg
let on_end msg = on_key Input.End msg

(* Common ctrl key combinations *)
let on_ctrl_c msg = on_char ~ctrl:true 'c' msg
let on_ctrl_x msg = on_char ~ctrl:true 'x' msg
let on_ctrl_v msg = on_char ~ctrl:true 'v' msg
let on_ctrl_z msg = on_char ~ctrl:true 'z' msg
let on_ctrl_a msg = on_char ~ctrl:true 'a' msg
let on_ctrl_s msg = on_char ~ctrl:true 's' msg
let on_ctrl_d msg = on_char ~ctrl:true 'd' msg

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

(* Pretty-printing *)
let pp _pp_msg fmt sub =
  let open Format in
  let rec pp_sub fmt = function
    | None -> fprintf fmt "None"
    | Keyboard _ -> fprintf fmt "Keyboard(<fun>)"
    | Mouse _ -> fprintf fmt "Mouse(<fun>)"
    | Window _ -> fprintf fmt "Window(<fun>)"
    | Focus _ -> fprintf fmt "Focus(<fun>)"
    | Blur _ -> fprintf fmt "Blur(<fun>)"
    | Batch subs ->
        fprintf fmt "Batch[@[<hv>%a@]]"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") pp_sub)
          subs
  in
  pp_sub fmt sub
