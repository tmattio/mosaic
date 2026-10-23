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

(* Encoding *)

type encoding = {
  cursor_keys : bool;
  keypad : bool;
  new_line : bool;
  modify_other_keys : int;
  kitty : int;
}

let legacy =
  {
    cursor_keys = false;
    keypad = false;
    new_line = false;
    modify_other_keys = 0;
    kitty = 0;
  }

(* The modifier bits of the kitty protocol and xterm, 1 plus which is the
   modifier parameter. *)
let bits m =
  let bit b n = if b then n else 0 in
  bit m.shift 1 lor bit m.alt 2 lor bit m.ctrl 4 lor bit m.super 8
  lor bit m.hyper 16 lor bit m.meta 32 lor bit m.caps_lock 64
  lor bit m.num_lock 128

let shift_bit = 1
let alt_bit = 2
let ctrl_bit = 4
let lock_bits = 64 lor 128

(* A chord types nothing: any modifier but Shift and the locks. *)
let chord b = b land 0b111110 <> 0
let is_text c = Uchar.is_valid c && c >= 0x20 && (c < 0x7F || c > 0x9F)

let utf_8 c =
  let b = Buffer.create 4 in
  Buffer.add_utf_8_uchar b (Uchar.of_int c);
  Buffer.contents b

let no_key e k =
  invalid_arg
    (Format.asprintf "Keystroke.encode: no terminal in this encoding%s sends %a"
       (if e.kitty = 0 then "" else " of the kitty protocol")
       pp_key k)

(* The US PC-101 layout's shifted characters, each above the one Shift is
   held for: the kitty protocol's key code is the unshifted one. *)
let us_shifted = "~!@#$%^&*()_+{}|:\"<>?"
let us_unshifted = "`1234567890-=[]\\;',./"

(* The key a character is typed with, and whether Shift is held for it. Caps
   Lock, not Shift, makes an upper-case letter when the lock is on. *)
let typed_with c ~caps_lock =
  if c >= 0x41 && c <= 0x5A then (c + 32, not caps_lock)
  else if c < 0x80 then
    match String.index_opt us_shifted (Char.chr c) with
    | Some i -> (Char.code us_unshifted.[i], true)
    | None -> (c, false)
  else (c, false)

(* Parameters as the kitty protocol and xterm write them: a modifier
   parameter [m] with the event type [e] after a colon when it is not a
   press. *)
let mods_param m e =
  if e = 1 then string_of_int m else Printf.sprintf "%d:%d" m e

(* [CSI 1 ; m X], or [CSI X] and its SS3 form when unmodified. *)
let letter ~ss3 final m e =
  if m = 1 && e = 1 then
    (if ss3 then "\027O" else "\027[") ^ String.make 1 final
  else Printf.sprintf "\027[1;%s%c" (mods_param m e) final

(* [CSI n ; m ~]. *)
let tilde n m e =
  if m = 1 && e = 1 then Printf.sprintf "\027[%d~" n
  else Printf.sprintf "\027[%d;%s~" n (mods_param m e)

(* The kitty protocol's [CSI code:shifted:base ; m:e ; text u]. *)
let csi_u ?shifted ?base ?(text = []) code m e =
  let b = Buffer.create 16 in
  Buffer.add_string b "\027[";
  Buffer.add_string b (string_of_int code);
  (match (shifted, base) with
  | None, None -> ()
  | _ ->
      Buffer.add_char b ':';
      Option.iter (fun c -> Buffer.add_string b (string_of_int c)) shifted;
      Option.iter
        (fun c ->
          Buffer.add_char b ':';
          Buffer.add_string b (string_of_int c))
        base);
  if m <> 1 || e <> 1 || text <> [] then (
    Buffer.add_char b ';';
    if m <> 1 || e <> 1 then Buffer.add_string b (mods_param m e));
  if text <> [] then (
    Buffer.add_char b ';';
    Buffer.add_string b (String.concat ":" (List.map string_of_int text)));
  Buffer.add_char b 'u';
  Buffer.contents b

(* xterm's numbered keys, [CSI n ~], up to F[last]: the kitty protocol has
   codes of its own from F13. *)
let function_codes =
  [| 15; 17; 18; 19; 20; 21; 23; 24; 25; 26; 28; 29; 31; 32; 33; 34 |]

let numbered ~last = function
  | Insert -> Some 2
  | Delete -> Some 3
  | Page_up -> Some 5
  | Page_down -> Some 6
  | F n when n >= 5 && n <= last -> Some function_codes.(n - 5)
  | _ -> None

(* The final byte of the keys xterm sends as [CSI 1 ; m X]. *)
let lettered = function
  | Up -> Some 'A'
  | Down -> Some 'B'
  | Right -> Some 'C'
  | Left -> Some 'D'
  | Begin -> Some 'E'
  | End -> Some 'F'
  | Home -> Some 'H'
  | F 1 -> Some 'P'
  | F 2 -> Some 'Q'
  | F 3 -> Some 'R'
  | F 4 -> Some 'S'
  | _ -> None

let is_cursor_key = function
  | Up | Down | Right | Left | Begin | End | Home -> true
  | _ -> false

(* The kitty protocol's functional key codes (its "Functional key
   definitions"). *)
let kitty_code = function
  | Caps_lock -> Some 57358
  | Scroll_lock -> Some 57359
  | Num_lock -> Some 57360
  | Print_screen -> Some 57361
  | Pause -> Some 57362
  | Menu -> Some 57363
  | F n when n >= 13 && n <= 35 -> Some (57363 + n)
  | Media m ->
      Some
        (57428
        +
        match m with
        | Play -> 0
        | Pause -> 1
        | Play_pause -> 2
        | Reverse -> 3
        | Stop -> 4
        | Fast_forward -> 5
        | Rewind -> 6
        | Track_next -> 7
        | Track_previous -> 8
        | Record -> 9
        | Volume_down -> 10
        | Volume_up -> 11
        | Mute -> 12)
  | Modifier m ->
      Some
        (57441
        +
        match m with
        | Left_shift -> 0
        | Left_ctrl -> 1
        | Left_alt -> 2
        | Left_super -> 3
        | Left_hyper -> 4
        | Left_meta -> 5
        | Right_shift -> 6
        | Right_ctrl -> 7
        | Right_alt -> 8
        | Right_super -> 9
        | Right_hyper -> 10
        | Right_meta -> 11
        | Level3_shift -> 12
        | Level5_shift -> 13)
  | Unidentified c when c >= 57344 && c <= 63743 -> Some c
  | _ -> None

(* The kitty protocol's code of a key on the keypad. *)
let keypad_code = function
  | Char c -> (
      match Uchar.to_int c with
      | c when c >= 0x30 && c <= 0x39 -> Some (57399 + c - 0x30)
      | 0x2E -> Some 57409
      | 0x2F -> Some 57410
      | 0x2A -> Some 57411
      | 0x2D -> Some 57412
      | 0x2B -> Some 57413
      | 0x3D -> Some 57415
      | 0x2C -> Some 57416
      | _ -> None)
  | Enter -> Some 57414
  | Left -> Some 57417
  | Right -> Some 57418
  | Up -> Some 57419
  | Down -> Some 57420
  | Page_up -> Some 57421
  | Page_down -> Some 57422
  | Home -> Some 57423
  | End -> Some 57424
  | Insert -> Some 57425
  | Delete -> Some 57426
  | Begin -> Some 57427
  | _ -> None

(* The final byte of a key of xterm's application keypad, [SS3 X]. *)
let application_keypad = function
  | Char c -> (
      match Uchar.to_int c with
      | 0x3D -> Some 'X'
      | c when c >= 0x2A && c <= 0x39 ->
          (* "*+,-./0123456789" from j. *)
          Some (Char.chr (c - 0x2A + 0x6A))
      | _ -> None)
  | Enter -> Some 'M'
  | Tab -> Some 'I'
  | _ -> None

(* The C0 control that Ctrl with a character sends in legacy bytes (kitty
   keyboard protocol, "Legacy ctrl mapping of ASCII keys"), when it has
   one. *)
let control c =
  if c >= 0x61 && c <= 0x7A then Some (c - 0x60)
  else if c >= 0x41 && c <= 0x5A then Some (c - 0x40)
  else
    match c with
    | 0x20 | 0x32 | 0x40 -> Some 0
    | 0x5B -> Some 0x1B
    | 0x5C -> Some 0x1C
    | 0x5D -> Some 0x1D
    | 0x5E -> Some 0x1E
    | 0x5F -> Some 0x1F
    | 0x3F -> Some 0x7F
    | _ -> None

(* The code xterm's modifyOtherKeys and the kitty protocol give Enter, Tab,
   Backspace and Escape. *)
let other_code = function
  | Enter -> Some 13
  | Tab -> Some 9
  | Backspace -> Some 127
  | Escape -> Some 27
  | _ -> None

(* Legacy bytes and modifyOtherKeys, for a key with no sequence of its own:
   a character, Enter, Tab, Backspace or Escape. [b] holds the modifiers
   without the locks, which legacy bytes never report, and [code] the
   character typed. modifyOtherKeys reports Shift held for the character
   (xterm sends [CSI 27 ; 2 ; 65 ~] for Shift+a). *)
let legacy_other e key code b ~caps_lock =
  let alt = b land alt_bit <> 0 and ctrl = b land ctrl_bit <> 0 in
  let plain =
    match key with
    | Enter -> if e.new_line then "\r\n" else "\r"
    | Tab -> "\t"
    | Backspace -> if ctrl then "\x08" else "\x7f"
    | Escape -> "\x1b"
    | _ -> (
        match if ctrl then control code else None with
        | Some c0 -> String.make 1 (Char.chr c0)
        | None -> utf_8 code)
  in
  let plain = if alt then "\x1b" ^ plain else plain in
  (* Whether the decoder reads [plain] back as the key: Alt is an ESC before
     the key, and Ctrl a C0 control that names no key of its own. *)
  let exact =
    b land lnot alt_bit = 0
    || key <> Enter && key <> Tab && key <> Backspace && key <> Escape
       && b land lnot (alt_bit lor ctrl_bit) = 0
       &&
       match code with
       | 0x68 | 0x69 | 0x6D -> false
       | c -> (c >= 0x61 && c <= 0x7A) || (c >= 0x5C && c <= 0x5F) || c = 0x20
  in
  let b' =
    match key with
    | Char _ when snd (typed_with code ~caps_lock:(caps_lock && not (chord b)))
      ->
        b lor shift_bit
    | _ -> b
  in
  if
    (e.modify_other_keys >= 2 && b' <> 0)
    || (e.modify_other_keys = 1 && not exact)
  then Printf.sprintf "\027[27;%d;%d~" (1 + b') code
  else plain

let check_char k c =
  if not (is_text c) then
    invalid_arg
      (Format.asprintf "Keystroke.encode: %a is a control, not a key" pp_key k)

let encode_legacy e (k : t) =
  let b = bits k.mods land lnot lock_bits in
  let caps_lock = k.mods.caps_lock in
  let m = 1 + b in
  match k.key with
  | key when is_cursor_key key ->
      letter ~ss3:e.cursor_keys (Option.get (lettered key)) m 1
  | F n when n >= 1 && n <= 4 ->
      letter ~ss3:true (Option.get (lettered k.key)) m 1
  | key when numbered ~last:20 key <> None ->
      tilde (Option.get (numbered ~last:20 key)) m 1
  | (Char _ | Enter | Tab) as key
    when k.keypad && e.keypad && application_keypad key <> None ->
      let final = Option.get (application_keypad key) in
      if m = 1 then Printf.sprintf "\027O%c" final
      else Printf.sprintf "\027O1;%d%c" m final
  | Tab when b land shift_bit <> 0 ->
      if b = shift_bit then "\027[Z" else Printf.sprintf "\027[1;%dZ" m
  | (Enter | Tab | Backspace | Escape) as key ->
      legacy_other e key (Option.get (other_code key)) b ~caps_lock
  | Char c ->
      let c = Uchar.to_int c in
      check_char k.key c;
      legacy_other e k.key c b ~caps_lock
  | key -> no_key e key

(* The kitty protocol, once a terminal disambiguates (flag 1) or sends every
   key as a sequence (flag 8). A key that types text, and Enter, Tab and
   Backspace unmodified, keep their legacy bytes unless every key is a
   sequence; their releases are then not reported. *)
let encode_kitty e ~key ~mods ~base ~keypad ~text ~event =
  let all = e.kitty land 8 <> 0 in
  let alternates = e.kitty land 4 <> 0 in
  let b = bits mods in
  let e' = if e.kitty land 2 <> 0 then event else 1 in
  let base = if alternates then Option.map Uchar.to_int base else None in
  let text =
    if all && e.kitty land 16 <> 0 && not (chord b) then
      let rec scalars i acc =
        if i >= String.length text then List.rev acc
        else
          let d = String.get_utf_8_uchar text i in
          scalars
            (i + Uchar.utf_decode_length d)
            (Uchar.to_int (Uchar.utf_decode_uchar d) :: acc)
      in
      scalars 0 []
    else []
  in
  let legacy_bytes bytes = if event = 3 then "" else bytes in
  let keypad_key = if keypad then keypad_code key else None in
  match (key, keypad_key) with
  | Char c, Some code ->
      let c = Uchar.to_int c in
      if (not all) && not (chord b) then legacy_bytes (utf_8 c)
      else csi_u ?base ~text code (1 + b) e'
  | _, Some code -> csi_u ?base code (1 + b) e'
  | Char c, None ->
      let c = Uchar.to_int c in
      check_char key c;
      if (not all) && not (chord b) then legacy_bytes (utf_8 c)
      else
        (* Caps Lock applies to a character typed, never to a chord's. *)
        let code, shifted =
          typed_with c ~caps_lock:(b land 64 <> 0 && not (chord b))
        in
        let b = if shifted then b lor shift_bit else b in
        let shifted = if shifted && alternates then Some c else None in
        csi_u ?shifted ?base ~text code (1 + b) e'
  | (Enter | Tab | Backspace), None when (not all) && b land lnot lock_bits = 0
    ->
      legacy_bytes
        (match key with
        | Enter -> if e.new_line then "\r\n" else "\r"
        | Tab -> "\t"
        | _ -> "\x7f")
  | (Enter | Tab | Backspace | Escape), None ->
      csi_u ?base ~text (Option.get (other_code key)) (1 + b) e'
  | F 3, None -> tilde 13 (1 + b) e'
  | key, None when lettered key <> None ->
      letter
        ~ss3:(e.cursor_keys || not (is_cursor_key key))
        (Option.get (lettered key))
        (1 + b) e'
  | key, None when numbered ~last:12 key <> None ->
      tilde (Option.get (numbered ~last:12 key)) (1 + b) e'
  | Modifier _, None when not all -> no_key e key
  | key, None -> (
      match kitty_code key with
      | Some code -> csi_u ?base code (1 + b) e'
      | None -> no_key e key)

let disambiguates e = e.kitty land 0b1001 <> 0

let encode e (k : t) =
  if disambiguates e then
    encode_kitty e ~key:k.key ~mods:k.mods ~base:k.base ~keypad:k.keypad
      ~text:k.text
      ~event:(if k.repeat then 2 else 1)
  else encode_legacy e k

let encode_release e (r : release) =
  if disambiguates e && e.kitty land 2 <> 0 then
    encode_kitty e ~key:r.key ~mods:r.mods ~base:r.base ~keypad:r.keypad
      ~text:"" ~event:3
  else ""
