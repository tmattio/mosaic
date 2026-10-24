module Keystroke = Matrix_input.Keystroke

type 'a binding = Shortcut.t * 'a

let binding_equal equal_action (a, action_a) (b, action_b) =
  Shortcut.equal a b && equal_action action_a action_b

module Keys = Hashtbl.Make (struct
  type t = Keystroke.key

  let equal = ( = )
  let hash = Hashtbl.hash
end)

type 'a actions = {
  chars : (int, 'a option) Hashtbl.t;
  keys : 'a option Keys.t;
}

type 'a t = 'a actions option array

let add t (shortcut, action) =
  let modifier = Shortcut.Private.modifier shortcut in
  let actions =
    match t.(modifier) with
    | Some actions -> actions
    | None ->
        let actions = { chars = Hashtbl.create 16; keys = Keys.create 16 } in
        t.(modifier) <- Some actions;
        actions
  in
  let action = Some action in
  match Shortcut.Private.key shortcut with
  | Char code -> Hashtbl.replace actions.chars (Uchar.to_int code) action
  | key -> Keys.replace actions.keys key action

let make ~defaults ?(custom = []) () =
  let t = Array.make 64 None in
  List.iter (add t) defaults;
  List.iter (add t) custom;
  t

let find_char t modifier code =
  match t.(modifier) with
  | None -> None
  | Some actions -> (
      try Hashtbl.find actions.chars code with Not_found -> None)

let find_key t modifier key =
  match t.(modifier) with
  | None -> None
  | Some actions -> ( try Keys.find actions.keys key with Not_found -> None)

let action t ev = Shortcut.Private.resolve ~find_char ~find_key t ev
