module Key = Matrix_input.Key

type 'a binding = Shortcut.t * 'a

let binding_equal equal_action (a, action_a) (b, action_b) =
  Shortcut.equal a b && equal_action action_a action_b

module Keys = Hashtbl.Make (struct
  type t = Key.t

  let equal = Key.equal
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
  | Key.Char code -> Hashtbl.replace actions.chars (Uchar.to_int code) action
  | key -> Keys.replace actions.keys key action

let make ~defaults ?(custom = []) () =
  let t = Array.make 64 None in
  List.iter (add t) defaults;
  List.iter (add t) custom;
  t

let find_char actions code =
  try Hashtbl.find actions.chars code with Not_found -> None

let action t (ev : Key.event) =
  if ev.event_type = Key.Release then None
  else
    match t.(Shortcut.Private.modifiers ev.modifier) with
    | None -> None
    | Some actions -> (
        let direct =
          match Shortcut.Private.normalize_key ev.key with
          | Key.Char code -> (
              let code = Uchar.to_int code in
              match find_char actions code with
              | Some _ as action -> action
              | None ->
                  let lower = Shortcut.Private.lowercase code in
                  if lower = code then None else find_char actions lower)
          | key -> ( try Keys.find actions.keys key with Not_found -> None)
        in
        match direct with
        | Some _ -> direct
        | None -> (
            match ev.base_key with
            | None -> None
            | Some base ->
                find_char actions
                  (Shortcut.Private.lowercase (Uchar.to_int base))))
