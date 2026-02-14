(* Escape: Low-level ANSI escape sequence builders *)

type writer = { bytes : Bytes.t; cap : int; mutable pos : int }
type t = writer -> unit
type terminator = [ `Bel | `St ]

(* ASCII character code constants *)
let char_0 = Char.code '0' (* 48 *)
let char_a_hex = Char.code 'a' - 10 (* 87: for hex digits a-f *)

(* Writer primitives *)

let make bytes = { bytes; cap = Bytes.length bytes; pos = 0 }
let len w = w.pos
let[@inline] reset_pos w = w.pos <- 0
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

(* Integer writing - optimized for zero stack allocation *)

let[@inline] digit_char n = Char.unsafe_chr (char_0 + n)

(* Powers of 10 lookup table - avoids overflow in magnitude computation *)
let pow10 =
  [|
    1;
    10;
    100;
    1000;
    10000;
    100000;
    1000000;
    10000000;
    100000000;
    1000000000;
    10000000000;
    100000000000;
    1000000000000;
    10000000000000;
    100000000000000;
    1000000000000000;
    10000000000000000;
    100000000000000000;
    1000000000000000000;
  |]

(* Write unsigned integer - fully iterative, no stack allocation *)
let add_uint w n =
  if n = 0 then write_char w '0'
  else
    (* Find number of digits via binary search on pow10 *)
    let digits =
      if n < pow10.(10) then
        if n < pow10.(5) then
          if n < pow10.(3) then
            if n < pow10.(1) then 1 else if n < pow10.(2) then 2 else 3
          else if n < pow10.(4) then 4
          else 5
        else if n < pow10.(7) then if n < pow10.(6) then 6 else 7
        else if n < pow10.(8) then 8
        else if n < pow10.(9) then 9
        else 10
      else if n < pow10.(15) then
        if n < pow10.(12) then if n < pow10.(11) then 11 else 12
        else if n < pow10.(13) then 13
        else if n < pow10.(14) then 14
        else 15
      else if n < pow10.(17) then if n < pow10.(16) then 16 else 17
      else if n < pow10.(18) then 18
      else 19
    in
    for i = digits - 1 downto 0 do
      write_char w (digit_char (n / pow10.(i) mod 10))
    done

let add_int w n =
  if n >= 0 then add_uint w n
  else (
    (* Work in negative space to handle Int.min_int without overflow. For
       negative n: q = n / 10 is negative or zero, r = n - q*10 is in [-9, 0] *)
    write_char w '-';
    let rec emit_negative n =
      let q = n / 10 in
      let r = n - (q * 10) in
      if q <> 0 then emit_negative q;
      write_char w (digit_char (-r))
    in
    emit_negative n)

(* Write a two-digit lowercase hex value for a byte [0,255] *)
let[@inline] hex_digit n =
  Char.unsafe_chr (if n < 10 then char_0 + n else char_a_hex + n)

let add_hex2 w v =
  let v = max 0 (min 255 v) in
  write_char w (hex_digit (v lsr 4));
  write_char w (hex_digit (v land 0xF))

let write_terminator w = function
  | `Bel -> write_char w '\007'
  | `St ->
      write_char w '\027';
      write_char w '\\'

(* Combinators *)

let empty _ = ()
let literal s w = write_string w s
let char c w = write_char w c

let concat a b w =
  a w;
  b w

let seq ts w =
  let rec loop = function
    | [] -> ()
    | t :: rest ->
        t w;
        loop rest
  in
  loop ts

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
  (* Fast path: return empty string literal without allocation *)
  if len = 0 then ""
  else
    (* Pass 2: actual write *)
    let bytes = Bytes.create len in
    let w = { bytes; cap = len; pos = 0 } in
    t w;
    Bytes.unsafe_to_string bytes

(* to_buffer: convenience wrapper via to_string (allocates intermediate
   string) *)
let to_buffer t buf = Buffer.add_string buf (to_string t)

(* CSI / SGR helpers *)

let esc body w =
  write_string w "\027[";
  write_string w body

let csi ~params ~command w =
  write_string w "\027[";
  write_string w params;
  write_char w command

let sgr codes w =
  match codes with
  | [] -> ()
  | first :: rest ->
      write_string w "\027[";
      add_int w first;
      let rec loop = function
        | [] -> ()
        | code :: rest ->
            write_char w ';';
            add_int w code;
            loop rest
      in
      loop rest;
      write_char w 'm'

(* sgr_direct uses writer position to track separator state without ref
   allocation *)
let sgr_direct write_codes w =
  write_string w "\027[";
  let start_pos = w.pos in
  write_codes (fun code ->
      if w.pos > start_pos then write_char w ';';
      add_int w code);
  write_char w 'm'

(* Low-level SGR building primitives for zero-allocation emission *)
let[@inline] sgr_open w = write_string w "\027["
let[@inline] sgr_code w n = add_int w n
let[@inline] sgr_sep w = write_char w ';'
let[@inline] sgr_close w = write_char w 'm'
let reset : t = sgr [ 0 ]
let clamp_nonneg v = max 0 v
let clamp_pos v = max 1 v
let clamp_byte v = max 0 (min 255 v)

(* CSI sequence helpers to reduce code duplication *)

(* CSI with single param, returns empty if n=0 *)
let[@inline] csi_n_opt cmd n =
  let n = clamp_nonneg n in
  if n = 0 then empty
  else fun w ->
    write_string w "\027[";
    add_int w n;
    write_char w cmd

(* CSI with single param, always emits *)
let[@inline] csi_n cmd n w =
  write_string w "\027[";
  add_int w n;
  write_char w cmd

(* CSI with two params *)
let[@inline] csi_nn cmd a b w =
  write_string w "\027[";
  add_int w a;
  write_char w ';';
  add_int w b;
  write_char w cmd

(* Cursor Control *)

let cursor_up ~n = csi_n_opt 'A' n
let cursor_down ~n = csi_n_opt 'B' n
let cursor_forward ~n = csi_n_opt 'C' n
let cursor_back ~n = csi_n_opt 'D' n
let cursor_next_line ~n = csi_n_opt 'E' n
let cursor_previous_line ~n = csi_n_opt 'F' n

let cursor_horizontal_absolute col w =
  let col = clamp_pos col in
  csi_n 'G' col w

let cursor_vertical_absolute row w =
  let row = clamp_pos row in
  csi_n 'd' row w

let cursor_position ~row ~col w =
  let row = clamp_pos row in
  let col = clamp_pos col in
  csi_nn 'H' row col w

let cursor_save : t = literal "\027[s"
let cursor_restore : t = literal "\027[u"

let move_cursor_and_clear ~row ~col =
  seq [ cursor_position ~row ~col; literal "\027[J" ]

(* Cursor Appearance *)

let show_cursor : t = literal "\027[?25h"
let hide_cursor : t = literal "\027[?25l"

type cursor_shape =
  [ `Default
  | `Blinking_block
  | `Block
  | `Blinking_underline
  | `Underline
  | `Blinking_bar
  | `Bar ]

let cursor_shape_to_int = function
  | `Default -> 0
  | `Blinking_block -> 1
  | `Block -> 2
  | `Blinking_underline -> 3
  | `Underline -> 4
  | `Blinking_bar -> 5
  | `Bar -> 6

let cursor_style ~shape w =
  write_string w "\027[";
  add_int w (cursor_shape_to_int shape);
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

type erase_display_mode = [ `Below | `Above | `All | `Scrollback ]

let erase_display_mode_to_int = function
  | `Below -> 0
  | `Above -> 1
  | `All -> 2
  | `Scrollback -> 3

let erase_display ~mode w =
  csi_n 'J' (erase_display_mode_to_int mode) w

let erase_below_cursor : t = literal "\027[J"

type erase_line_mode = [ `Right | `Left | `All ]

let erase_line_mode_to_int = function
  | `Right -> 0
  | `Left -> 1
  | `All -> 2

let erase_line ~mode w =
  csi_n 'K' (erase_line_mode_to_int mode) w

let insert_lines ~n = csi_n_opt 'L' n
let delete_lines ~n = csi_n_opt 'M' n
let scroll_up ~n = csi_n_opt 'S' n
let scroll_down ~n = csi_n_opt 'T' n

let set_scrolling_region ~top ~bottom w =
  if top < 1 || bottom <= top then
    invalid_arg "Escape.set_scrolling_region: invalid bounds";
  csi_nn 'r' top bottom w

let reset_scrolling_region : t = literal "\027[r"

(* Colors and Attributes *)

(* RGB color helper - combines consecutive writes *)
let[@inline] csi_rgb prefix r g b w =
  let r = clamp_byte r in
  let g = clamp_byte g in
  let b = clamp_byte b in
  write_string w prefix;
  add_int w r;
  write_char w ';';
  add_int w g;
  write_char w ';';
  add_int w b;
  write_char w 'm'

let set_foreground ~r ~g ~b w = csi_rgb "\027[38;2;" r g b w
let set_background ~r ~g ~b w = csi_rgb "\027[48;2;" r g b w
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

(* Direct hyperlink emission - zero allocation versions *)
let[@inline] hyperlink_open w url =
  write_char w '\027';
  write_char w ']';
  write_char w '8';
  write_char w ';';
  write_char w ';';
  write_string w url;
  write_terminator w `St

let[@inline] hyperlink_close w =
  write_char w '\027';
  write_char w ']';
  write_string w "8;;";
  write_terminator w `St

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
let request_device_attributes : t = literal "\027[c"
let request_tertiary_device_attributes : t = literal "\027[=c"
let request_terminal_identity : t = literal "\027[>0q"
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
