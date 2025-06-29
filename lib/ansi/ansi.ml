type color =
  | Black
  | Red
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White
  | Default
  | Bright_black
  | Bright_red
  | Bright_green
  | Bright_yellow
  | Bright_blue
  | Bright_magenta
  | Bright_cyan
  | Bright_white
  | Index of int
  | RGB of int * int * int

type style =
  [ `Bold
  | `Dim
  | `Italic
  | `Underline
  | `Double_underline
  | `Blink
  | `Reverse
  | `Strikethrough
  | `Overline ]

type attr = [ `Fg of color | `Bg of color | style | `Reset ]

let esc = "\x1b["
let csi n cmd = Printf.sprintf "%s%d%s" esc n cmd
let csi2 n m cmd = Printf.sprintf "%s%d;%d%s" esc n m cmd
let cursor_up n = if n = 0 then "" else csi n "A"
let cursor_down n = if n = 0 then "" else csi n "B"
let cursor_forward n = if n = 0 then "" else csi n "C"
let cursor_back n = if n = 0 then "" else csi n "D"
let cursor_position row col = csi2 row col "H"
let cursor_save = esc ^ "s"
let cursor_restore = esc ^ "u"
let cursor_show = esc ^ "?25h"
let cursor_hide = esc ^ "?25l"
let set_cursor_style_blinking_block = esc ^ "1 q"
let set_cursor_style_steady_block = esc ^ "2 q"
let set_cursor_style_blinking_underline = esc ^ "3 q"
let set_cursor_style_steady_underline = esc ^ "4 q"
let set_cursor_style_blinking_bar = esc ^ "5 q"
let set_cursor_style_steady_bar = esc ^ "6 q"
let clear_screen = esc ^ "2J"
let clear_screen_above = esc ^ "1J"
let clear_screen_below = esc ^ "0J"
let clear_line = esc ^ "2K"
let clear_line_left = esc ^ "1K"
let clear_line_right = esc ^ "0K"
let scroll_up n = csi n "S"
let scroll_down n = csi n "T"
let alternate_screen_on = esc ^ "?1049h"
let alternate_screen_off = esc ^ "?1049l"
let mouse_on = esc ^ "?1000;1002;1006h"
let mouse_off = esc ^ "?1000;1002;1006l"
let bracketed_paste_on = esc ^ "?2004h"
let bracketed_paste_off = esc ^ "?2004l"

let set_window_title title =
  (* OSC 0 sets both icon and window title *)
  esc ^ "]0;" ^ title ^ "\x07"

let color_to_codes ~bg = function
  | Black -> [ (if bg then 40 else 30) ]
  | Red -> [ (if bg then 41 else 31) ]
  | Green -> [ (if bg then 42 else 32) ]
  | Yellow -> [ (if bg then 43 else 33) ]
  | Blue -> [ (if bg then 44 else 34) ]
  | Magenta -> [ (if bg then 45 else 35) ]
  | Cyan -> [ (if bg then 46 else 36) ]
  | White -> [ (if bg then 47 else 37) ]
  | Default -> [ (if bg then 49 else 39) ]
  | Bright_black -> [ (if bg then 100 else 90) ]
  | Bright_red -> [ (if bg then 101 else 91) ]
  | Bright_green -> [ (if bg then 102 else 92) ]
  | Bright_yellow -> [ (if bg then 103 else 93) ]
  | Bright_blue -> [ (if bg then 104 else 94) ]
  | Bright_magenta -> [ (if bg then 105 else 95) ]
  | Bright_cyan -> [ (if bg then 106 else 96) ]
  | Bright_white -> [ (if bg then 107 else 97) ]
  | Index n when n >= 0 && n <= 255 -> [ (if bg then 48 else 38); 5; n ]
  | RGB (r, g, b)
    when r >= 0 && r <= 255 && g >= 0 && g <= 255 && b >= 0 && b <= 255 ->
      [ (if bg then 48 else 38); 2; r; g; b ]
  | Index n ->
      invalid_arg (Printf.sprintf "Invalid color index: %d (must be 0-255)" n)
  | RGB (r, g, b) ->
      invalid_arg
        (Printf.sprintf
           "Invalid RGB color: (%d,%d,%d) (each component must be 0-255)" r g b)

let style_to_code = function
  | `Bold -> 1
  | `Dim -> 2
  | `Italic -> 3
  | `Underline -> 4
  | `Blink -> 5
  | `Reverse -> 7
  | `Strikethrough -> 9
  | `Double_underline -> 21
  | `Overline -> 53

let attr_to_codes = function
  | `Reset -> [ 0 ]
  | `Fg color -> color_to_codes ~bg:false color
  | `Bg color -> color_to_codes ~bg:true color
  | #style as s -> [ style_to_code s ]

let sgr attrs =
  match attrs with
  | [] -> ""
  | _ ->
      let codes = List.concat_map attr_to_codes attrs in
      let str = String.concat ";" (List.map string_of_int codes) in
      esc ^ str ^ "m"

let reset = sgr [ `Reset ]
let reset_bold_dim = esc ^ "22m"
let reset_underline = esc ^ "24m"
let reset_blink = esc ^ "25m"
let reset_reverse = esc ^ "27m"
let reset_strikethrough = esc ^ "29m"
let reset_overline = esc ^ "55m"

let strip str =
  let b = Buffer.create (String.length str) in
  let len = String.length str in
  let i = ref 0 in
  while !i < len do
    if str.[!i] <> '\x1b' then (
      Buffer.add_char b str.[!i];
      incr i)
    else if
      (* We are at the start of an escape sequence *)
      !i + 1 >= len
    then i := len (* Lone ESC at the end *)
    else
      match str.[!i + 1] with
      | '[' ->
          (* CSI: ESC [ ... (one final char) *)
          let j = ref (!i + 2) in
          while
            !j < len
            &&
            let c = Char.code str.[!j] in
            not (c >= 0x40 && c <= 0x7e)
          do
            incr j
          done;
          if !j < len then i := !j + 1 else i := len
      | ']' -> (
          (* OSC: ESC ] ... ST (ESC \ or BEL) *)
          let terminator = ref None in
          let j = ref (!i + 2) in
          while !j < len && !terminator = None do
            if str.[!j] = '\x07' then terminator := Some (!j + 1)
            else if str.[!j] = '\x1b' && !j + 1 < len && str.[!j + 1] = '\\'
            then terminator := Some (!j + 2)
            else incr j
          done;
          i := match !terminator with Some end_pos -> end_pos | None -> len)
      | 'N' | 'O' ->
          (* SS2, SS3: ESC N/O (one char) *)
          i := if !i + 2 < len then !i + 3 else len
      | 'P' | 'X' | '^' | '_' -> (
          (* DCS, SOS, PM, APC: terminated by ST *)
          let terminator = ref None in
          let j = ref (!i + 2) in
          while !j < len && !terminator = None do
            if str.[!j] = '\x1b' && !j + 1 < len && str.[!j + 1] = '\\' then
              terminator := Some (!j + 2)
            else incr j
          done;
          i := match !terminator with Some end_pos -> end_pos | None -> len)
      | _ ->
          (* Unhandled or simple two-byte sequence *)
          i := !i + 2
  done;
  Buffer.contents b

let hyperlink ~uri text =
  Printf.sprintf "\x1b]8;;%s\x1b\\%s\x1b]8;;\x1b\\" uri text

let style attrs str = sgr attrs ^ str ^ reset
