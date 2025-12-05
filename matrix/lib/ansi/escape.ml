(* Escape: Low-level ANSI escape sequence builders *)

type writer = { bytes : Bytes.t; cap : int; mutable pos : int }
type t = writer -> unit
type terminator = [ `Bel | `St ]

(* Writer primitives *)

let make bytes = { bytes; cap = Bytes.length bytes; pos = 0 }
let len w = w.pos
let slice w = Bytes.sub w.bytes 0 w.pos

(* Low-level writing *)

let[@inline] write_char w c =
  if w.cap = 0 then
    (* Counting mode, no actual writes *)
    w.pos <- w.pos + 1
  else (
    if w.pos >= w.cap then invalid_arg "Escape.writer: buffer overflow (char)";
    Bytes.unsafe_set w.bytes w.pos c;
    w.pos <- w.pos + 1)

let[@inline] write_string w s =
  let slen = String.length s in
  if slen = 0 then ()
  else if w.cap = 0 then
    (* Counting mode *)
    w.pos <- w.pos + slen
  else (
    if w.pos + slen > w.cap then
      invalid_arg "Escape.writer: buffer overflow (string)";
    Bytes.blit_string s 0 w.bytes w.pos slen;
    w.pos <- w.pos + slen)

let[@inline] write_subbytes w bytes off blen =
  if blen < 0 || off < 0 || off + blen > Bytes.length bytes then
    invalid_arg "Escape.write_subbytes: invalid slice";
  if blen = 0 then ()
  else if w.cap = 0 then
    (* Counting mode *)
    w.pos <- w.pos + blen
  else (
    if w.pos + blen > w.cap then
      invalid_arg "Escape.writer: buffer overflow (bytes)";
    Bytes.blit bytes off w.bytes w.pos blen;
    w.pos <- w.pos + blen)

(* Integer writing *)

let rec add_int_digits w n =
  if n >= 10 then (
    add_int_digits w (n / 10);
    write_char w (Char.chr (48 + (n mod 10))))
  else write_char w (Char.chr (48 + n))

let add_int w n =
  if n < 0 then (
    write_char w '-';
    add_int_digits w (-n))
  else if n = 0 then write_char w '0'
  else add_int_digits w n

(* Write a two-digit lowercase hex value for a byte [0,255] *)
let add_hex2 w v =
  let v = max 0 (min 255 v) in
  let hi = v lsr 4 in
  let lo = v land 0xF in
  let d n = if n < 10 then Char.chr (48 + n) else Char.chr (87 + n) in
  write_char w (d hi);
  write_char w (d lo)

let write_terminator w = function
  | `Bel -> write_char w '\007'
  | `St ->
      write_char w '\027';
      write_char w '\\'

(* Combinators *)

let empty (_ : writer) = ()
let literal s (w : writer) = write_string w s
let char c (w : writer) = write_char w c

let concat a b w =
  a w;
  b w

let seq ts w = List.iter (fun t -> t w) ts
let emit t w = t w
let bytes b ~off ~len w = write_subbytes w b off len

let utf8 cp w =
  let cp =
    (* Clamp invalid unicode scalars or surrogates to replacement char U+FFFD *)
    if cp < 0 || cp > 0x10FFFF || (cp >= 0xD800 && cp <= 0xDFFF) then 0xFFFD
    else cp
  in
  if cp < 0x80 then write_char w (Char.chr cp)
  else if cp < 0x800 then (
    write_char w (Char.chr (0xC0 lor (cp lsr 6)));
    write_char w (Char.chr (0x80 lor (cp land 0x3F))))
  else if cp < 0x10000 then (
    write_char w (Char.chr (0xE0 lor (cp lsr 12)));
    write_char w (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    write_char w (Char.chr (0x80 lor (cp land 0x3F))))
  else (
    write_char w (Char.chr (0xF0 lor (cp lsr 18)));
    write_char w (Char.chr (0x80 lor ((cp lsr 12) land 0x3F)));
    write_char w (Char.chr (0x80 lor ((cp lsr 6) land 0x3F)));
    write_char w (Char.chr (0x80 lor (cp land 0x3F))))

(* IO Utilities *)

let to_string (t : t) =
  (* Pass 1: counting *)
  let counter = { bytes = Bytes.create 0; cap = 0; pos = 0 } in
  t counter;
  let len = counter.pos in
  (* Pass 2: actual write *)
  let bytes = Bytes.create len in
  let w = { bytes; cap = len; pos = 0 } in
  t w;
  Bytes.unsafe_to_string bytes

let to_buffer t buf =
  (* Heuristic allocation: assume 1024 bytes is enough for typical sequences.
     If it overflows, we'll just grow. *)
  let scratch_len = 1024 in
  let bytes = Bytes.create scratch_len in
  let w = { bytes; cap = scratch_len; pos = 0 } in

  (* Try writing to scratch buffer *)
  try
    t w;
    Buffer.add_subbytes buf bytes 0 w.pos
  with Invalid_argument _ ->
    (* Buffer overflowed scratch space. Fallback to counting + exact alloc *)
    let str = to_string t in
    Buffer.add_string buf str

(* CSI / SGR helpers *)

let esc body = concat (literal "\027[") (literal body)

let csi ~params ~command w =
  write_string w "\027[";
  write_string w params;
  write_char w command

let sgr codes w =
  match codes with
  | [] -> ()
  | _ ->
      write_string w "\027[";
      List.iteri
        (fun idx code ->
          if idx > 0 then write_char w ';';
          add_int w code)
        codes;
      write_char w 'm'

let sgr_direct write_code w =
  write_string w "\027[";
  let first = ref true in
  write_code (fun code ->
      if !first then first := false else write_char w ';';
      add_int w code);
  write_char w 'm'

let reset : t = sgr [ 0 ]
let clamp_nonneg v = max 0 v
let clamp_pos v = max 1 v
let clamp_byte v = max 0 (min 255 v)

(* Cursor Control *)

let cursor_up ~n =
  let n = clamp_nonneg n in
  if n = 0 then empty
  else fun w ->
    write_string w "\027[";
    add_int w n;
    write_char w 'A'

let cursor_down ~n =
  let n = clamp_nonneg n in
  if n = 0 then empty
  else fun w ->
    write_string w "\027[";
    add_int w n;
    write_char w 'B'

let cursor_forward ~n =
  let n = clamp_nonneg n in
  if n = 0 then empty
  else fun w ->
    write_string w "\027[";
    add_int w n;
    write_char w 'C'

let cursor_back ~n =
  let n = clamp_nonneg n in
  if n = 0 then empty
  else fun w ->
    write_string w "\027[";
    add_int w n;
    write_char w 'D'

let cursor_next_line ~n =
  let n = clamp_nonneg n in
  if n = 0 then empty
  else fun w ->
    write_string w "\027[";
    add_int w n;
    write_char w 'E'

let cursor_previous_line ~n =
  let n = clamp_nonneg n in
  if n = 0 then empty
  else fun w ->
    write_string w "\027[";
    add_int w n;
    write_char w 'F'

let cursor_horizontal_absolute col =
  let col = clamp_pos col in
  fun w ->
    write_string w "\027[";
    add_int w col;
    write_char w 'G'

let cursor_vertical_absolute row =
  let row = clamp_pos row in
  fun w ->
    write_string w "\027[";
    add_int w row;
    write_char w 'd'

let cursor_position ~row ~col =
  let row = clamp_pos row in
  let col = clamp_pos col in
  fun w ->
    write_string w "\027[";
    add_int w row;
    write_char w ';';
    add_int w col;
    write_char w 'H'

let cursor_save : t = literal "\027[s"
let cursor_restore : t = literal "\027[u"

let move_cursor_and_clear ~row ~col =
  seq [ cursor_position ~row ~col; literal "\027[J" ]

(* Cursor Appearance *)

let show_cursor : t = literal "\027[?25h"
let hide_cursor : t = literal "\027[?25l"

let cursor_style ~shape =
  let n = max 0 (min 6 shape) in
  fun w ->
    write_string w "\027[";
    add_int w n;
    write_string w " q"

let default_cursor_style : t = literal "\027[0 q"
let cursor_block : t = literal "\027[2 q"
let cursor_block_blink : t = literal "\027[1 q"
let cursor_line : t = literal "\027[6 q"
let cursor_line_blink : t = literal "\027[5 q"
let cursor_underline : t = literal "\027[4 q"
let cursor_underline_blink : t = literal "\027[3 q"

let cursor_color ~r ~g ~b w =
  let r = clamp_byte r in
  let g = clamp_byte g in
  let b = clamp_byte b in
  write_char w '\027';
  write_char w ']';
  write_string w "12;#";
  add_hex2 w r;
  add_hex2 w g;
  add_hex2 w b;
  write_terminator w `Bel

let reset_cursor_color : t = literal "\027]112\007"
let reset_cursor_color_fallback : t = literal "\027]12;default\007"

(* Screen Control *)

let clear : t = literal "\027[2J"
let home : t = literal "\027[H"
let clear_and_home : t = literal "\027[H\027[2J"

let erase_display ~mode =
  let mode = if mode < 0 || mode > 3 then 2 else mode in
  fun w ->
    write_string w "\027[";
    add_int w mode;
    write_char w 'J'

let erase_below_cursor : t = literal "\027[J"

let erase_line ~mode =
  let mode = if mode < 0 || mode > 2 then 2 else mode in
  fun w ->
    write_string w "\027[";
    add_int w mode;
    write_char w 'K'

let insert_lines ~n =
  let n = clamp_nonneg n in
  if n = 0 then empty
  else fun w ->
    write_string w "\027[";
    add_int w n;
    write_char w 'L'

let delete_lines ~n =
  let n = clamp_nonneg n in
  if n = 0 then empty
  else fun w ->
    write_string w "\027[";
    add_int w n;
    write_char w 'M'

let scroll_up ~n =
  let n = clamp_nonneg n in
  if n = 0 then empty
  else fun w ->
    write_string w "\027[";
    add_int w n;
    write_char w 'S'

let scroll_down ~n =
  let n = clamp_nonneg n in
  if n = 0 then empty
  else fun w ->
    write_string w "\027[";
    add_int w n;
    write_char w 'T'

let set_scrolling_region ~top ~bottom =
  if top < 1 || bottom <= top then
    invalid_arg "Escape.set_scrolling_region: invalid bounds";
  fun w ->
    write_string w "\027[";
    add_int w top;
    write_char w ';';
    add_int w bottom;
    write_char w 'r'

(* Colors and Attributes *)

let set_foreground ~r ~g ~b =
  let r = clamp_byte r in
  let g = clamp_byte g in
  let b = clamp_byte b in
  fun w ->
    write_string w "\027[";
    write_string w "38;2;";
    add_int w r;
    write_char w ';';
    add_int w g;
    write_char w ';';
    add_int w b;
    write_char w 'm'

let set_background ~r ~g ~b =
  let r = clamp_byte r in
  let g = clamp_byte g in
  let b = clamp_byte b in
  fun w ->
    write_string w "\027[";
    write_string w "48;2;";
    add_int w r;
    write_char w ';';
    add_int w g;
    write_char w ';';
    add_int w b;
    write_char w 'm'

let reset_background : t = literal "\027[49m"
let reset_foreground : t = literal "\027[39m"

(* Screen Buffers *)

let enter_alternate_screen : t = literal "\027[?1049h"
let exit_alternate_screen : t = literal "\027[?1049l"

(* Terminal Properties *)

let set_title ~title w =
  write_char w '\027';
  write_char w ']';
  write_char w '0';
  write_char w ';';
  write_string w title;
  write_terminator w `St

let explicit_width ~width ~text w =
  write_char w '\027';
  write_char w ']';
  write_string w "66;w=";
  add_int w width;
  write_char w ';';
  write_string w text;
  write_terminator w `St

let explicit_width_bytes ~width ~bytes ~off ~len w =
  write_char w '\027';
  write_char w ']';
  write_string w "66;w=";
  add_int w width;
  write_char w ';';
  write_subbytes w bytes off len;
  write_terminator w `St

(* OSC helpers *)

let osc ?(terminator = `St) ~payload w =
  write_char w '\027';
  write_char w ']';
  write_string w payload;
  write_terminator w terminator

(* Hyperlinks (OSC 8) *)

let hyperlink_start ?(params = "") ~url w =
  write_char w '\027';
  write_char w ']';
  write_char w '8';
  write_char w ';';
  if params <> "" then write_string w params;
  write_char w ';';
  write_string w url;
  write_terminator w `St

let hyperlink_end : t = osc ~terminator:`St ~payload:"8;;"

let hyperlink ?(params = "") ~url ~text w =
  hyperlink_start ~params ~url w;
  write_string w text;
  hyperlink_end w

(* Terminal Modes *)

let bracketed_paste_on : t = literal "\027[?2004h"
let bracketed_paste_off : t = literal "\027[?2004l"
let mouse_tracking_on : t = literal "\027[?1000h"
let mouse_tracking_off : t = literal "\027[?1000l"
let mouse_button_tracking_on : t = literal "\027[?1002h"
let mouse_button_tracking_off : t = literal "\027[?1002l"
let mouse_motion_on : t = literal "\027[?1003h"
let mouse_motion_off : t = literal "\027[?1003l"
let mouse_sgr_mode_on : t = literal "\027[?1006h"
let mouse_sgr_mode_off : t = literal "\027[?1006l"
let mouse_pixel_mode_on : t = literal "\027[?1002;1003;1004;1016h"
let mouse_pixel_mode_off : t = literal "\027[?1002;1003;1004;1016l"
let mouse_x10_on : t = literal "\027[?9h"
let mouse_x10_off : t = literal "\027[?9l"
let urxvt_mouse_on : t = literal "\027[?1015h"
let urxvt_mouse_off : t = literal "\027[?1015l"
let focus_tracking_on : t = literal "\027[?1004h"
let focus_tracking_off : t = literal "\027[?1004l"
let sync_output_on : t = literal "\027[?2026h"
let sync_output_off : t = literal "\027[?2026l"
let unicode_mode_on : t = literal "\027[?2027h"
let unicode_mode_off : t = literal "\027[?2027l"
let color_scheme_set : t = literal "\027[?2031h"
let color_scheme_reset : t = literal "\027[?2031l"

(* Key Encoding *)

let csi_u_on : t = literal "\027[>1u"
let csi_u_off : t = literal "\027[<1u"

let csi_u_push ~flags w =
  write_string w "\027[>";
  add_int w flags;
  write_char w 'u'

let csi_u_pop : t = literal "\027[<u"
let modify_other_keys_on : t = literal "\027[>4;1m"
let modify_other_keys_off : t = literal "\027[>4;0m"

(* Device and Capability Queries *)

(* Terminal and Device Information *)

let request_cursor_position : t = literal "\027[6n"
let request_pixel_size : t = literal "\027[14t"
let primary_device_attrs : t = literal "\027[c"
let request_device_attributes = primary_device_attrs
let request_tertiary_device_attributes : t = literal "\027[=c"
let xtversion : t = literal "\027[>0q"
let request_terminal_identity = xtversion
let request_device_status : t = literal "\027[5n"

(* Feature and Protocol Support *)

let request_csi_u_support : t = literal "\027[?u"

let request_kitty_graphics_support : t =
  literal "\027_Gi=31337,s=1,v=1,a=q,t=d,f=24;AAAA\027\\\027[c"

let request_sixel_geometry : t = literal "\027[?2;1;0S"
let request_explicit_width_support : t = literal "\027]66;w=1; \027\\"
let request_scaled_text_support : t = literal "\027]66;s=2; \027\\"
let request_color_scheme : t = literal "\027[?996n"

(* Mode State Queries *)

let request_focus_mode : t = literal "\027[?1004$p"
let request_sgr_pixels_mode : t = literal "\027[?1016$p"
let request_unicode_mode : t = literal "\027[?2027$p"
let request_color_scheme_mode : t = literal "\027[?2031$p"
let request_bracketed_paste_mode : t = literal "\027[?2004$p"
let request_sync_mode : t = literal "\027[?2026$p"

(* Response Markers *)

let bracketed_paste_start : t = literal "\027[200~"
let bracketed_paste_end : t = literal "\027[201~"
