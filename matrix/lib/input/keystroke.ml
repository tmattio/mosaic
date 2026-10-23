type mods = {
  shift : bool;
  alt : bool;
  ctrl : bool;
  super : bool;
  hyper : bool;
  meta : bool;
  caps_lock : bool;
  num_lock : bool;
}

let no_mods =
  {
    shift = false;
    alt = false;
    ctrl = false;
    super = false;
    hyper = false;
    meta = false;
    caps_lock = false;
    num_lock = false;
  }

type media =
  | Play
  | Pause
  | Play_pause
  | Reverse
  | Stop
  | Fast_forward
  | Rewind
  | Track_next
  | Track_previous
  | Record
  | Volume_down
  | Volume_up
  | Mute

type modifier =
  | Left_shift
  | Left_ctrl
  | Left_alt
  | Left_super
  | Left_hyper
  | Left_meta
  | Right_shift
  | Right_ctrl
  | Right_alt
  | Right_super
  | Right_hyper
  | Right_meta
  | Level3_shift
  | Level5_shift

type key =
  | Char of Uchar.t
  | Enter
  | Tab
  | Backspace
  | Escape
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Page_up
  | Page_down
  | Insert
  | Delete
  | Begin
  | F of int
  | Print_screen
  | Pause
  | Menu
  | Scroll_lock
  | Caps_lock
  | Num_lock
  | Media of media
  | Modifier of modifier
  | Unidentified of int

type release = { key : key; mods : mods; base : Uchar.t option; keypad : bool }

type t = {
  key : key;
  mods : mods;
  text : string;
  base : Uchar.t option;
  keypad : bool;
  repeat : bool;
}

(* What a key types, as the decoder reports it: the character of a [Char]
   that is not a C0 or C1 control or DEL, unless a chord modifier is held. *)
let typed mods = function
  | Char u
    when (not (mods.ctrl || mods.alt || mods.super || mods.hyper || mods.meta))
         &&
         let c = Uchar.to_int u in
         c >= 0x20 && (c < 0x7F || c > 0x9F) ->
      let b = Bytes.create (Uchar.utf_8_byte_length u) in
      ignore (Bytes.set_utf_8_uchar b 0 u : int);
      Bytes.unsafe_to_string b
  | _ -> ""

let make ?(mods = no_mods) ?text ?base ?(keypad = false) ?(repeat = false) key =
  let text = match text with Some text -> text | None -> typed mods key in
  { key; mods; text; base; keypad; repeat }

(* Formatting *)

let is_interrupt (k : t) =
  let c = Uchar.of_char 'c' in
  k.mods.ctrl
  && (not (k.mods.alt || k.mods.super || k.mods.hyper || k.mods.meta))
  &&
  match k.key with
  | Char u ->
      Uchar.equal u c || Uchar.equal u (Uchar.of_char 'C') || k.base = Some c
  | _ -> false

let media_name : media -> string = function
  | Play -> "Play"
  | Pause -> "Pause"
  | Play_pause -> "Play_pause"
  | Reverse -> "Reverse"
  | Stop -> "Stop"
  | Fast_forward -> "Fast_forward"
  | Rewind -> "Rewind"
  | Track_next -> "Track_next"
  | Track_previous -> "Track_previous"
  | Record -> "Record"
  | Volume_down -> "Volume_down"
  | Volume_up -> "Volume_up"
  | Mute -> "Mute"

let modifier_name = function
  | Left_shift -> "Left_shift"
  | Left_ctrl -> "Left_ctrl"
  | Left_alt -> "Left_alt"
  | Left_super -> "Left_super"
  | Left_hyper -> "Left_hyper"
  | Left_meta -> "Left_meta"
  | Right_shift -> "Right_shift"
  | Right_ctrl -> "Right_ctrl"
  | Right_alt -> "Right_alt"
  | Right_super -> "Right_super"
  | Right_hyper -> "Right_hyper"
  | Right_meta -> "Right_meta"
  | Level3_shift -> "Level3_shift"
  | Level5_shift -> "Level5_shift"

let pp_uchar ppf u =
  let c = Uchar.to_int u in
  if c > 0x20 && c < 0x7F then Format.fprintf ppf "'%c'" (Char.chr c)
  else if c > 0x9F && c <> 0xAD then (
    let b = Buffer.create 4 in
    Buffer.add_utf_8_uchar b u;
    Format.fprintf ppf "'%s'" (Buffer.contents b))
  else Format.fprintf ppf "U+%04X" c

let pp_key ppf = function
  | Char u -> pp_uchar ppf u
  | F n -> Format.fprintf ppf "F %d" n
  | Media m -> Format.fprintf ppf "Media %s" (media_name m)
  | Modifier m -> Format.pp_print_string ppf (modifier_name m)
  | Unidentified n -> Format.fprintf ppf "Unidentified %d" n
  | Enter -> Format.pp_print_string ppf "Enter"
  | Tab -> Format.pp_print_string ppf "Tab"
  | Backspace -> Format.pp_print_string ppf "Backspace"
  | Escape -> Format.pp_print_string ppf "Escape"
  | Up -> Format.pp_print_string ppf "Up"
  | Down -> Format.pp_print_string ppf "Down"
  | Left -> Format.pp_print_string ppf "Left"
  | Right -> Format.pp_print_string ppf "Right"
  | Home -> Format.pp_print_string ppf "Home"
  | End -> Format.pp_print_string ppf "End"
  | Page_up -> Format.pp_print_string ppf "Page_up"
  | Page_down -> Format.pp_print_string ppf "Page_down"
  | Insert -> Format.pp_print_string ppf "Insert"
  | Delete -> Format.pp_print_string ppf "Delete"
  | Begin -> Format.pp_print_string ppf "Begin"
  | Print_screen -> Format.pp_print_string ppf "Print_screen"
  | Pause -> Format.pp_print_string ppf "Pause"
  | Menu -> Format.pp_print_string ppf "Menu"
  | Scroll_lock -> Format.pp_print_string ppf "Scroll_lock"
  | Caps_lock -> Format.pp_print_string ppf "Caps_lock"
  | Num_lock -> Format.pp_print_string ppf "Num_lock"

let pp_chord ppf mods key =
  let mod_ on name = if on then Format.fprintf ppf "%s+" name in
  mod_ mods.ctrl "Ctrl";
  mod_ mods.alt "Alt";
  mod_ mods.shift "Shift";
  mod_ mods.super "Super";
  mod_ mods.hyper "Hyper";
  mod_ mods.meta "Meta";
  pp_key ppf key;
  if mods.caps_lock then Format.pp_print_string ppf " caps_lock";
  if mods.num_lock then Format.pp_print_string ppf " num_lock"

let pp_base ppf = function
  | None -> ()
  | Some u -> Format.fprintf ppf " base %a" pp_uchar u

let pp ppf (k : t) =
  pp_chord ppf k.mods k.key;
  pp_base ppf k.base;
  if k.keypad then Format.pp_print_string ppf " keypad";
  if k.repeat then Format.pp_print_string ppf " repeat";
  if k.text <> "" then Format.fprintf ppf " text %S" k.text

let pp_release ppf (k : release) =
  pp_chord ppf k.mods k.key;
  pp_base ppf k.base;
  if k.keypad then Format.pp_print_string ppf " keypad"
