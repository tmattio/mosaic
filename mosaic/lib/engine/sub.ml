(** Subscriptions with single closure-based implementation *)

(* The only representation: a closure that takes dispatch and event *)
type 'msg t = { 
  run : dispatch:('msg -> unit) -> Input.event -> unit 
}

(* Window size type *)
type window_size = { width : int; height : int }

(* Constructors *)
let none = { run = (fun ~dispatch:_ _event -> ()) }

let keyboard f = 
  { run = (fun ~dispatch event ->
      match event with
      | Input.Key key_event ->
          dispatch (f key_event)
      | _ -> ()) }

let keyboard_filter f = 
  { run = (fun ~dispatch event ->
      match event with
      | Input.Key key_event ->
          Option.iter dispatch (f key_event)
      | _ -> ()) }

let mouse f = 
  { run = (fun ~dispatch event ->
      match event with
      | Input.Mouse mouse_event ->
          dispatch (f mouse_event)
      | _ -> ()) }

let mouse_filter f = 
  { run = (fun ~dispatch event ->
      match event with
      | Input.Mouse mouse_event ->
          Option.iter dispatch (f mouse_event)
      | _ -> ()) }

let window f = 
  { run = (fun ~dispatch event ->
      match event with
      | Input.Resize (width, height) ->
          dispatch (f { width; height })
      | _ -> ()) }

let window_filter f = 
  { run = (fun ~dispatch event ->
      match event with
      | Input.Resize (width, height) ->
          Option.iter dispatch (f { width; height })
      | _ -> ()) }

let focus f = 
  { run = (fun ~dispatch event ->
      match event with
      | Input.Focus ->
          dispatch (f ())
      | _ -> ()) }

let blur f = 
  { run = (fun ~dispatch event ->
      match event with
      | Input.Blur ->
          dispatch (f ())
      | _ -> ()) }

let paste f = 
  { run = (fun ~dispatch event ->
      match event with
      | Input.Paste text ->
          dispatch (f text)
      | _ -> ()) }

let paste_filter f = 
  { run = (fun ~dispatch event ->
      match event with
      | Input.Paste text ->
          Option.iter dispatch (f text)
      | _ -> ()) }

let batch subs =
  { run = (fun ~dispatch event ->
      List.iter (fun (sub : _ t) -> sub.run ~dispatch event) subs) }

(* Helper functions for specific events *)
let on_mouse_motion f =
  mouse_filter (function
    | Input.Motion (x, y, _, _) -> Some (f x y)
    | _ -> None)

let on_mouse_click f =
  mouse_filter (function
    | Input.Button_press (x, y, _, _) -> Some (f x y)
    | _ -> None)

let on_resize f = window (fun size -> f size.width size.height)

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
let on_paste f = paste f

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

(* Map function for transforming messages *)
let map f (sub : _ t) =
  { run = (fun ~dispatch event ->
      sub.run ~dispatch:(fun msg -> dispatch (f msg)) event) }

(* Runtime interface *)
let run ~dispatch event (sub : _ t) = sub.run ~dispatch event

(* Pretty-printing *)
let pp _pp_msg fmt _sub =
  Format.fprintf fmt "<subscription>"