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
    (** Note: Limited support; may conflict with bold reset in some terminals *)
  | `Blink
  | `Reverse
  | `Conceal
  | `Strikethrough
  | `Overline  (** Note: Limited support *)
  | `Framed  (** Note: Very limited support *)
  | `Encircled  (** Note: Very limited support *) ]

type attr = [ `Fg of color | `Bg of color | style | `Reset ]

let esc = "\x1b["

let csi_params params cmd =
  esc ^ String.concat ";" (List.map string_of_int params) ^ cmd

let csi n cmd = csi_params [ n ] cmd
let csi2 n m cmd = csi_params [ n; m ] cmd
let cursor_up n = if n <= 0 then "" else csi n "A"
let cursor_down n = if n <= 0 then "" else csi n "B"
let cursor_forward n = if n <= 0 then "" else csi n "C"
let cursor_back n = if n <= 0 then "" else csi n "D"
let cursor_next_line n = if n <= 0 then "" else csi n "E"
let cursor_previous_line n = if n <= 0 then "" else csi n "F"

let cursor_horizontal_absolute col =
  if col <= 0 then "" else csi (max 1 col) "G"

let cursor_vertical_absolute row = if row <= 0 then "" else csi (max 1 row) "d"
let cursor_position row col = csi2 (max 1 row) (max 1 col) "H"
let cursor_save = esc ^ "s"
let cursor_restore = esc ^ "u"
let cursor_show = esc ^ "?25h"
let cursor_hide = esc ^ "?25l"
let set_cursor_style_blinking_block = csi 1 " q"
let set_cursor_style_steady_block = csi 2 " q"
let set_cursor_style_blinking_underline = csi 3 " q"
let set_cursor_style_steady_underline = csi 4 " q"
let set_cursor_style_blinking_bar = csi 5 " q"
let set_cursor_style_steady_bar = csi 6 " q"
let set_cursor_style_default = csi 0 " q"

let erase_in_display param =
  match param with
  | 0 -> esc ^ "J"
  | 1 -> esc ^ "1J"
  | 2 -> esc ^ "2J"
  | 3 -> esc ^ "3J"
  | _ -> esc ^ "2J" (* Default to full clear *)

let clear_screen = erase_in_display 2
let clear_screen_above = erase_in_display 1
let clear_screen_below = erase_in_display 0
let clear_terminal = esc ^ "2J" ^ esc ^ "3J" ^ esc ^ "H"

let erase_in_line param =
  match param with
  | 0 -> esc ^ "K"
  | 1 -> esc ^ "1K"
  | 2 -> esc ^ "2K"
  | _ -> esc ^ "2K" (* Default to full line *)

let clear_line = erase_in_line 2
let clear_line_left = erase_in_line 1
let clear_line_right = erase_in_line 0
let scroll_up n = if n <= 0 then "" else csi n "S"
let scroll_down n = if n <= 0 then "" else csi n "T"

let set_scrolling_region ?(top = 1) ?bottom () =
  match bottom with
  | None -> csi_params [] "r" (* Reset to full screen *)
  | Some b ->
      if top < 1 || b <= top then invalid_arg "Invalid scrolling region";
      csi_params [ top; b ] "r"

let insert_lines n = if n <= 0 then "" else csi n "L"
let delete_lines n = if n <= 0 then "" else csi n "M"
let insert_characters n = if n <= 0 then "" else csi n "@"
let delete_characters n = if n <= 0 then "" else csi n "P"
let erase_characters n = if n <= 0 then "" else csi n "X"
let repeat_last_character n = if n <= 0 then "" else csi n "b"
let cursor_tab n = if n <= 0 then "" else csi n "I"
let cursor_back_tab n = if n <= 0 then "" else csi n "Z"
let set_tab_stop = "\x1bH"
let clear_tab_stop_at_cursor = esc ^ "g" (* CSI g = CSI 0 g *)

let clear_tab_stop =
 fun param ->
  match param with
  | 0 -> esc ^ "g"
  | 3 -> esc ^ "3g"
  | _ -> esc ^ "g" (* Default to clear at cursor *)

let clear_all_tab_stops = esc ^ "3g"
let alternate_screen_on = esc ^ "?1049h"
let alternate_screen_off = esc ^ "?1049l"
let mouse_on = esc ^ "?1000;1002;1006h"
let mouse_off = esc ^ "?1000;1002;1006l"

(* Mouse reporting modes *)
let mouse_x10_on = esc ^ "?9h"
let mouse_x10_off = esc ^ "?9l"
let mouse_normal_on = esc ^ "?1000h"
let mouse_normal_off = esc ^ "?1000l"
let mouse_button_tracking_on = esc ^ "?1002h"
let mouse_button_tracking_off = esc ^ "?1002l"
let mouse_any_tracking_on = esc ^ "?1003h"
let mouse_any_tracking_off = esc ^ "?1003l"
let mouse_utf8_on = esc ^ "?1005h"
let mouse_utf8_off = esc ^ "?1005l"
let mouse_sgr_on = esc ^ "?1006h"
let mouse_sgr_off = esc ^ "?1006l"
let mouse_urxvt_on = esc ^ "?1015h"
let mouse_urxvt_off = esc ^ "?1015l"
let mouse_sgr_pixels_on = esc ^ "?1016h"
let mouse_sgr_pixels_off = esc ^ "?1016l"
let bracketed_paste_on = esc ^ "?2004h"
let bracketed_paste_off = esc ^ "?2004l"
let kitty_keyboard_on = esc ^ ">1u"
let kitty_keyboard_off = esc ^ "<u"

(* Kitty keyboard protocol with flags *)
let push_kitty_keyboard_flags flags = Printf.sprintf "%s>%du" esc flags
let pop_kitty_keyboard count = Printf.sprintf "%s<%du" esc count
let query_kitty_keyboard = esc ^ "?u"

(* Kitty keyboard protocol flags *)
module Kitty_keyboard_flags = struct
  let disambiguate_escape_codes = 1
  let report_event_types = 2
  let report_alternate_keys = 4
  let report_all_keys_as_escape_codes = 8
  let report_associated_text = 16
end

let focus_event_on = esc ^ "?1004h"
let focus_event_off = esc ^ "?1004l"

(* Keyboard modes *)
let application_keypad_on = esc ^ "="
let application_keypad_off = esc ^ ">"
let application_cursor_keys_on = esc ^ "?1h"
let application_cursor_keys_off = esc ^ "?1l"

(* modifyOtherKeys modes *)
let modify_other_keys_off = esc ^ ">4;0m"
let modify_other_keys_1 = esc ^ ">4;1m"
let modify_other_keys_2 = esc ^ ">4;2m"
let synchronized_update_on = esc ^ "?2026h"
let synchronized_update_off = esc ^ "?2026l"
let save_screen = esc ^ "?47h"
let restore_screen = esc ^ "?47l"
let set_window_title title = esc ^ "]0;" ^ title ^ "\x07"

(* Window manipulation (XTWINOPS) *)
let window_deiconify = csi 1 "t"
let window_iconify = csi 2 "t"
let window_move x y = csi_params [ 3; x; y ] "t"
let window_resize rows cols = csi_params [ 8; rows; cols ] "t"
let window_maximize = csi_params [ 9; 1 ] "t"
let window_unmaximize = csi_params [ 9; 0 ] "t"
let window_fullscreen = csi_params [ 10; 1 ] "t"
let window_exit_fullscreen = csi_params [ 10; 0 ] "t"
let window_push_title = csi_params [ 22; 0 ] "t"
let window_pop_title = csi_params [ 23; 0 ] "t"

(* DEC cursor save/restore *)
let dec_cursor_save = "\x1b7"
let dec_cursor_restore = "\x1b8"

(* SGR stack (XTPUSHSGR/XTPOPSGR) *)
let push_sgr = esc ^ "#{"
let pop_sgr = esc ^ "#}"

(* Device status requests *)
let request_cursor_position = esc ^ "6n"
let request_device_attributes = csi 0 "c"
let request_secondary_device_attributes = esc ^ ">0c"
let request_terminal_size_chars = esc ^ "18t"
let request_terminal_size_pixels = esc ^ "14t"

(* Uses semicolon separators for sub-parameters (e.g., 38;2;r;g;b), which is
   a widely supported variant compatible with konsole, instead of the colon
   specified in the standard. *)
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
  | Index n ->
      let clamped = max 0 (min 255 n) in
      [ (if bg then 48 else 38); 5; clamped ]
  | RGB (r, g, b) ->
      let cr = max 0 (min 255 r) in
      let cg = max 0 (min 255 g) in
      let cb = max 0 (min 255 b) in
      [ (if bg then 48 else 38); 2; cr; cg; cb ]

let style_to_code = function
  | `Bold -> 1
  | `Dim -> 2
  | `Italic -> 3
  | `Underline -> 4
  | `Double_underline -> 21 (* Note: May conflict with bold reset *)
  | `Blink -> 5
  | `Reverse -> 7
  | `Conceal -> 8
  | `Strikethrough -> 9
  | `Overline -> 53
  | `Framed -> 51
  | `Encircled -> 52

let attr_to_codes = function
  | `Reset -> [ 0 ]
  | `Fg color -> color_to_codes ~bg:false color
  | `Bg color -> color_to_codes ~bg:true color
  | #style as s -> [ style_to_code s ]

let sgr attrs =
  match attrs with
  | [] -> ""
  | _ ->
      let codes = List.concat (List.map attr_to_codes attrs) in
      let str = String.concat ";" (List.map string_of_int codes) in
      esc ^ str ^ "m"

let reset = sgr [ `Reset ]
let reset_bold_dim = esc ^ "22m"
let reset_intensity = esc ^ "22m" (* Alias for reset_bold_dim *)
let reset_italic = esc ^ "23m"
let reset_underline = esc ^ "24m"
let reset_blink = esc ^ "25m"
let reset_reverse = esc ^ "27m"
let reset_conceal = esc ^ "28m"
let reset_strikethrough = esc ^ "29m"
let reset_overline = esc ^ "55m"
let reset_framed_encircled = esc ^ "54m"

let strip str =
  let b = Buffer.create (String.length str) in
  let len = String.length str in
  let i = ref 0 in
  while !i < len do
    if str.[!i] <> '\x1b' then (
      Buffer.add_char b str.[!i];
      incr i)
    else if !i + 1 >= len then i := len
    else
      match str.[!i + 1] with
      | '[' ->
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
          let terminator = ref None in
          let j = ref (!i + 2) in
          while !j < len && !terminator = None do
            if str.[!j] = '\x07' then terminator := Some (!j + 1)
            else if str.[!j] = '\x1b' && !j + 1 < len && str.[!j + 1] = '\\'
            then terminator := Some (!j + 2)
            else incr j
          done;
          i := match !terminator with Some end_pos -> end_pos | None -> len)
      | 'N' | 'O' -> i := if !i + 2 < len then !i + 3 else len
      | 'P' | 'X' | '^' | '_' -> (
          let terminator = ref None in
          let j = ref (!i + 2) in
          while !j < len && !terminator = None do
            if str.[!j] = '\x1b' && !j + 1 < len && str.[!j + 1] = '\\' then
              terminator := Some (!j + 2)
            else incr j
          done;
          i := match !terminator with Some end_pos -> end_pos | None -> len)
      | _ -> i := !i + 2
  done;
  Buffer.contents b

let hyperlink ~uri text =
  Printf.sprintf "\x1b]8;;%s\x1b\\%s\x1b]8;;\x1b\\" uri text

let style attrs str =
  let reset_codes =
    List.concat_map
      (function
        | `Fg _ -> [ 39 ]
        | `Bg _ -> [ 49 ]
        | `Bold | `Dim -> [ 22 ]
        | `Italic -> [ 23 ]
        | `Underline | `Double_underline -> [ 24 ]
        | `Blink -> [ 25 ]
        | `Reverse -> [ 27 ]
        | `Conceal -> [ 28 ]
        | `Strikethrough -> [ 29 ]
        | `Overline -> [ 55 ]
        | `Framed | `Encircled -> [ 54 ]
        | `Reset -> [ 0 ])
      attrs
    |> List.sort_uniq compare
  in
  if attrs = [] then str
  else
    let reset_str =
      if List.mem 0 reset_codes then reset
      else if reset_codes = [] then ""
      else
        let code_str = String.concat ";" (List.map string_of_int reset_codes) in
        esc ^ code_str ^ "m"
    in
    sgr attrs ^ str ^ reset_str
