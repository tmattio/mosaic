module Key = Matrix_input.Key
module Modifier = Matrix_input.Modifier

type t = { key : Key.t; modifiers : int }

let modifiers (m : Modifier.t) =
  Bool.to_int m.ctrl
  lor (Bool.to_int m.alt lsl 1)
  lor (Bool.to_int m.shift lsl 2)
  lor (Bool.to_int m.super lsl 3)
  lor (Bool.to_int m.hyper lsl 4)
  lor (Bool.to_int m.meta lsl 5)

let keypad_chars = Array.init 128 (fun code -> Key.Char (Uchar.of_int code))

let normalize_key = function
  | Key.KP_0 -> keypad_chars.(48)
  | KP_1 -> keypad_chars.(49)
  | KP_2 -> keypad_chars.(50)
  | KP_3 -> keypad_chars.(51)
  | KP_4 -> keypad_chars.(52)
  | KP_5 -> keypad_chars.(53)
  | KP_6 -> keypad_chars.(54)
  | KP_7 -> keypad_chars.(55)
  | KP_8 -> keypad_chars.(56)
  | KP_9 -> keypad_chars.(57)
  | KP_decimal -> keypad_chars.(46)
  | KP_divide -> keypad_chars.(47)
  | KP_multiply -> keypad_chars.(42)
  | KP_subtract -> keypad_chars.(45)
  | KP_add -> keypad_chars.(43)
  | KP_equal -> keypad_chars.(61)
  | KP_separator -> keypad_chars.(44)
  | KP_enter -> Enter
  | KP_left -> Left
  | KP_right -> Right
  | KP_up -> Up
  | KP_down -> Down
  | KP_page_up -> Page_up
  | KP_page_down -> Page_down
  | KP_home -> Home
  | KP_end -> End
  | KP_insert -> Insert
  | KP_delete -> Delete
  | key -> key

let key ?(ctrl = false) ?(alt = false) ?(shift = false) ?(super = false)
    ?(hyper = false) ?(meta = false) key =
  {
    key = normalize_key key;
    modifiers =
      modifiers
        {
          Modifier.none with
          ctrl;
          alt;
          shift;
          super;
          hyper;
          meta = meta || alt;
        };
  }

let char ?ctrl ?alt ?shift ?super ?hyper ?meta c =
  key ?ctrl ?alt ?shift ?super ?hyper ?meta (Key.Char (Uchar.of_char c))

let ctrl c = char ~ctrl:true c
let alt c = char ~alt:true c
let shift c = char ~shift:true c
let escape = key Key.Escape
let enter = key Key.Enter
let tab = key Key.Tab
let backspace = key Key.Backspace
let delete = key Key.Delete
let f n = key (Key.F n)
let up = key Key.Up
let down = key Key.Down
let left = key Key.Left
let right = key Key.Right
let space = char ' '
let equal a b = a.modifiers = b.modifiers && Key.equal a.key b.key
let lowercase code = if code >= 65 && code <= 90 then code + 32 else code

let resolve ~find_char ~find_key bindings (ev : Key.event) =
  if ev.event_type = Key.Release then None
  else
    let modifier = modifiers ev.modifier in
    let direct =
      match normalize_key ev.key with
      | Key.Char code -> (
          let code = Uchar.to_int code in
          match find_char bindings modifier code with
          | Some _ as found -> found
          | None ->
              let lower = lowercase code in
              if lower = code then None else find_char bindings modifier lower)
      | key -> find_key bindings modifier key
    in
    match direct with
    | Some _ -> direct
    | None -> (
        match ev.base_key with
        | None -> None
        | Some base ->
            find_char bindings modifier (lowercase (Uchar.to_int base)))

let matches_char t modifier code =
  t.modifiers = modifier
  && match t.key with Key.Char c -> Uchar.to_int c = code | _ -> false

let matches_key t modifier key = t.modifiers = modifier && Key.equal t.key key

let find_char t modifier code =
  if matches_char t modifier code then Some () else None

let find_key t modifier key =
  if matches_key t modifier key then Some () else None

let matches t event =
  Option.is_some (resolve ~find_char ~find_key t (Event.Key.data event))

let rec find_last matches modifier key found = function
  | [] -> ( match found with [] -> None | (_, value) :: _ -> Some value)
  | (shortcut, _) :: rest as bindings ->
      let found = if matches shortcut modifier key then bindings else found in
      find_last matches modifier key found rest

let find_list_char bindings modifier code =
  find_last matches_char modifier code [] bindings

let find_list_key bindings modifier key =
  find_last matches_key modifier key [] bindings

let find bindings event =
  resolve ~find_char:find_list_char ~find_key:find_list_key bindings
    (Event.Key.data event)

module Private = struct
  let resolve = resolve
  let find = find
  let key t = t.key
  let modifier t = t.modifiers
end
