module Color = Color
module Attr = Attr
module Style = Style
module Parser = Parser
module Escape = Escape
module Sgr_state = Sgr_state

let styled ?(reset = false) ?fg ?bg ?(bold = false) ?(dim = false)
    ?(italic = false) ?(underline = false) ?(blink = false) ?(inverse = false)
    ?(hidden = false) ?(strikethrough = false) ?(overline = false)
    ?(double_underline = false) ?(framed = false) ?(encircled = false) ?link str
    =
  let style =
    Style.make ?fg ?bg ~bold ~dim ~italic ~underline ~blink ~inverse ~hidden
      ~strikethrough ~overline ~double_underline ~framed ~encircled ?link ()
  in
  match link with
  | None -> Style.styled ~reset style str
  | Some url ->
      Escape.to_string (fun w ->
          Escape.hyperlink_open w url;
          Style.emit style w;
          Escape.literal str w;
          Escape.hyperlink_close w;
          if reset then Escape.reset w)

let hyperlink_start ?(params = "") ~url () =
  Escape.to_string (Escape.hyperlink_start ~params ~url)

let hyperlink_end = Escape.to_string Escape.hyperlink_end
let hyperlink ~url ~text = Escape.to_string (Escape.hyperlink ~url ~text)

let render ?hyperlinks_enabled segments =
  Segment.render ?hyperlinks_enabled segments

let parse = Parser.parse

let strip str =
  match String.index_from_opt str 0 '\x1b' with
  | None -> str (* Fast path: no escape sequences, zero allocations *)
  | Some _ ->
      let buf = Buffer.create (String.length str) in
      let len = String.length str in
      let i = ref 0 in
      while !i < len do
        let c = str.[!i] in
        if c = '\x1b' then
          if !i + 1 >= len then i := len
          else
            match str.[!i + 1] with
            | '[' ->
                (* CSI sequences: ESC [ ... <final byte> *)
                let j = ref (!i + 2) in
                while
                  !j < len
                  &&
                  let c2 = Char.code str.[!j] in
                  not (c2 >= 0x40 && c2 <= 0x7e)
                do
                  incr j
                done;
                if !j < len then i := !j + 1 else i := len
            | ']' -> (
                (* OSC sequences: ESC ] ... (BEL | ESC \\) *)
                let terminator = ref None in
                let j = ref (!i + 2) in
                while !j < len && !terminator = None do
                  if str.[!j] = '\x07' then terminator := Some (!j + 1)
                  else if
                    str.[!j] = '\x1b' && !j + 1 < len && str.[!j + 1] = '\\'
                  then terminator := Some (!j + 2)
                  else incr j
                done;
                i :=
                  match !terminator with Some end_pos -> end_pos | None -> len)
            | 'N' | 'O' ->
                (* SS2/SS3: ESC N/O + one char *)
                i := if !i + 2 < len then !i + 3 else len
            | 'P' | 'X' | '^' | '_' -> (
                (* DCS/SOS/PM/APC: ESC P/X/^/_ ... ESC \\ *)
                let terminator = ref None in
                let j = ref (!i + 2) in
                while !j < len && !terminator = None do
                  if str.[!j] = '\x1b' && !j + 1 < len && str.[!j + 1] = '\\'
                  then terminator := Some (!j + 2)
                  else incr j
                done;
                i :=
                  match !terminator with Some end_pos -> end_pos | None -> len)
            | _ ->
                (* Single-char escapes: ESC <char> *)
                i := !i + 2
        else (
          Buffer.add_char buf c;
          incr i)
      done;
      Buffer.contents buf

let cursor_up ~n =
  let n = max 0 n in
  if n = 0 then "" else Escape.to_string (Escape.cursor_up ~n)

let cursor_down ~n =
  let n = max 0 n in
  if n = 0 then "" else Escape.to_string (Escape.cursor_down ~n)

let cursor_forward ~n =
  let n = max 0 n in
  if n = 0 then "" else Escape.to_string (Escape.cursor_forward ~n)

let cursor_back ~n =
  let n = max 0 n in
  if n = 0 then "" else Escape.to_string (Escape.cursor_back ~n)

let cursor_position ~row ~col =
  let row = max 1 row in
  let col = max 1 col in
  Escape.to_string (Escape.cursor_position ~row ~col)

let move_cursor_and_clear ~row ~col =
  Escape.to_string (Escape.move_cursor_and_clear ~row ~col)

let cursor_next_line ~n =
  let n = max 0 n in
  if n = 0 then "" else Escape.to_string (Escape.cursor_next_line ~n)

let cursor_previous_line ~n =
  let n = max 0 n in
  if n = 0 then "" else Escape.to_string (Escape.cursor_previous_line ~n)

let cursor_horizontal_absolute ~col =
  let col = max 1 col in
  Escape.to_string (Escape.cursor_horizontal_absolute col)

let cursor_vertical_absolute ~row =
  let row = max 1 row in
  Escape.to_string (Escape.cursor_vertical_absolute row)

(* Cursor Appearance *)
let show_cursor = Escape.to_string Escape.show_cursor
let hide_cursor = Escape.to_string Escape.hide_cursor
let cursor_style ~shape = Escape.to_string (Escape.cursor_style ~shape)
let default_cursor_style = Escape.to_string Escape.default_cursor_style
let cursor_block = Escape.to_string Escape.cursor_block
let cursor_block_blink = Escape.to_string Escape.cursor_block_blink
let cursor_line = Escape.to_string Escape.cursor_line
let cursor_line_blink = Escape.to_string Escape.cursor_line_blink
let cursor_underline = Escape.to_string Escape.cursor_underline
let cursor_underline_blink = Escape.to_string Escape.cursor_underline_blink
let cursor_color ~r ~g ~b = Escape.to_string (Escape.cursor_color ~r ~g ~b)
let reset_cursor_color = Escape.to_string Escape.reset_cursor_color

let reset_cursor_color_fallback =
  Escape.to_string Escape.reset_cursor_color_fallback

(* Screen Control *)
let erase_display ~mode =
  let mode = if mode < 0 || mode > 3 then 2 else mode in
  Escape.to_string (Escape.erase_display ~mode)

let scroll_up ~n =
  let n = max 0 n in
  if n = 0 then "" else Escape.to_string (Escape.scroll_up ~n)

let scroll_down ~n =
  let n = max 0 n in
  if n = 0 then "" else Escape.to_string (Escape.scroll_down ~n)

let erase_line ~mode =
  let mode = if mode < 0 || mode > 2 then 2 else mode in
  Escape.to_string (Escape.erase_line ~mode)

let clear_and_home = Escape.to_string Escape.clear_and_home
let clear = Escape.to_string Escape.clear
let home = Escape.to_string Escape.home
let erase_below_cursor = Escape.to_string Escape.erase_below_cursor
let cursor_save = Escape.to_string Escape.cursor_save
let cursor_restore = Escape.to_string Escape.cursor_restore

(* Colors and Attributes *)
let reset = "\027[0m"
let set_foreground ~r ~g ~b = Escape.to_string (Escape.set_foreground ~r ~g ~b)
let set_background ~r ~g ~b = Escape.to_string (Escape.set_background ~r ~g ~b)
let reset_background = Escape.to_string Escape.reset_background
let reset_foreground = Escape.to_string Escape.reset_foreground

(* Screen Buffers *)
let enter_alternate_screen = Escape.to_string Escape.enter_alternate_screen
let exit_alternate_screen = Escape.to_string Escape.exit_alternate_screen

(* Terminal Properties *)
let set_title ~title = Escape.to_string (Escape.set_title ~title)

let explicit_width ~width ~text =
  Escape.to_string (Escape.explicit_width ~width ~text)

(* Terminal modes: high-level wrappers for common toggles *)
let mouse_pixel_mode_on = Escape.to_string Escape.mouse_pixel_mode_on
let mouse_pixel_mode_off = Escape.to_string Escape.mouse_pixel_mode_off
let bracketed_paste_on = Escape.to_string Escape.bracketed_paste_on
let bracketed_paste_off = Escape.to_string Escape.bracketed_paste_off
let focus_tracking_on = Escape.to_string Escape.focus_tracking_on
let focus_tracking_off = Escape.to_string Escape.focus_tracking_off
let sync_output_on = Escape.to_string Escape.sync_output_on
let sync_output_off = Escape.to_string Escape.sync_output_off
let unicode_mode_on = Escape.to_string Escape.unicode_mode_on
let unicode_mode_off = Escape.to_string Escape.unicode_mode_off
let mouse_tracking_on = Escape.to_string Escape.mouse_tracking_on
let mouse_tracking_off = Escape.to_string Escape.mouse_tracking_off
let mouse_button_tracking_on = Escape.to_string Escape.mouse_button_tracking_on

let mouse_button_tracking_off =
  Escape.to_string Escape.mouse_button_tracking_off

let mouse_motion_on = Escape.to_string Escape.mouse_motion_on
let mouse_motion_off = Escape.to_string Escape.mouse_motion_off
let mouse_sgr_mode_on = Escape.to_string Escape.mouse_sgr_mode_on
let mouse_sgr_mode_off = Escape.to_string Escape.mouse_sgr_mode_off
let mouse_x10_on = Escape.to_string Escape.mouse_x10_on
let mouse_x10_off = Escape.to_string Escape.mouse_x10_off
let urxvt_mouse_on = Escape.to_string Escape.urxvt_mouse_on
let urxvt_mouse_off = Escape.to_string Escape.urxvt_mouse_off
let color_scheme_set = Escape.to_string Escape.color_scheme_set
let color_scheme_reset = Escape.to_string Escape.color_scheme_reset

(* Key encoding: CSI-u and kitty keyboard protocol *)
let csi_u_on = Escape.to_string Escape.csi_u_on
let csi_u_off = Escape.to_string Escape.csi_u_off
let csi_u_push ~flags = Escape.to_string (Escape.csi_u_push ~flags)
let csi_u_pop = Escape.to_string Escape.csi_u_pop
let modify_other_keys_on = Escape.to_string Escape.modify_other_keys_on
let modify_other_keys_off = Escape.to_string Escape.modify_other_keys_off

(* Device and capability queries *)

(* Terminal and Device Information *)

let request_cursor_position = Escape.to_string Escape.request_cursor_position
let request_pixel_size = Escape.to_string Escape.request_pixel_size

let request_device_attributes =
  Escape.to_string Escape.request_device_attributes

let request_tertiary_device_attributes =
  Escape.to_string Escape.request_tertiary_device_attributes

let request_terminal_identity =
  Escape.to_string Escape.request_terminal_identity

let request_device_status = Escape.to_string Escape.request_device_status

(* Feature and Protocol Support *)

let request_csi_u_support = Escape.to_string Escape.request_csi_u_support

let request_kitty_graphics_support =
  Escape.to_string Escape.request_kitty_graphics_support

let request_sixel_geometry = Escape.to_string Escape.request_sixel_geometry

let request_explicit_width_support =
  Escape.to_string Escape.request_explicit_width_support

let request_scaled_text_support =
  Escape.to_string Escape.request_scaled_text_support

let request_color_scheme = Escape.to_string Escape.request_color_scheme

(* Mode State Queries *)

let request_focus_mode = Escape.to_string Escape.request_focus_mode
let request_sgr_pixels_mode = Escape.to_string Escape.request_sgr_pixels_mode

let request_bracketed_paste_mode =
  Escape.to_string Escape.request_bracketed_paste_mode

let request_sync_mode = Escape.to_string Escape.request_sync_mode
let request_unicode_mode = Escape.to_string Escape.request_unicode_mode

let request_color_scheme_mode =
  Escape.to_string Escape.request_color_scheme_mode

(* Response Markers *)

let bracketed_paste_start = Escape.to_string Escape.bracketed_paste_start
let bracketed_paste_end = Escape.to_string Escape.bracketed_paste_end

let insert_lines ~n =
  let n = max 0 n in
  if n = 0 then "" else Escape.to_string (Escape.insert_lines ~n)

let delete_lines ~n =
  let n = max 0 n in
  if n = 0 then "" else Escape.to_string (Escape.delete_lines ~n)

let set_scrolling_region ~top ~bottom =
  Escape.to_string (Escape.set_scrolling_region ~top ~bottom)
