(** ANSI escape sequences for terminal control *)

(** {1 Colors} *)

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
  | Index of int  (** 256-color palette index *)
  | RGB of int * int * int  (** 24-bit RGB color *)

(** {1 Styles} *)

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

(** {1 Cursor Control} *)

val cursor_up : int -> string
val cursor_down : int -> string
val cursor_forward : int -> string
val cursor_back : int -> string
val cursor_position : int -> int -> string
val cursor_save : string
val cursor_restore : string
val cursor_show : string
val cursor_hide : string

(** {2 Cursor Shape} *)

val set_cursor_style_blinking_block : string
(** Set cursor to blinking block shape *)

val set_cursor_style_steady_block : string
(** Set cursor to steady (non-blinking) block shape *)

val set_cursor_style_blinking_underline : string
(** Set cursor to blinking underline shape *)

val set_cursor_style_steady_underline : string
(** Set cursor to steady (non-blinking) underline shape *)

val set_cursor_style_blinking_bar : string
(** Set cursor to blinking bar (vertical line) shape *)

val set_cursor_style_steady_bar : string
(** Set cursor to steady (non-blinking) bar shape *)

(** {1 Screen Control} *)

val clear_screen : string
val clear_screen_above : string
val clear_screen_below : string
val clear_line : string
val clear_line_left : string
val clear_line_right : string
val scroll_up : int -> string
val scroll_down : int -> string

(** {1 Terminal Modes} *)

val alternate_screen_on : string
val alternate_screen_off : string
val mouse_on : string
val mouse_off : string
val bracketed_paste_on : string
val bracketed_paste_off : string

val set_window_title : string -> string
(** [set_window_title title] returns the escape sequence to set the terminal
    window title. Note: Not all terminals support this feature. *)

(** {1 SGR (Select Graphic Rendition)} *)

val sgr : attr list -> string
(** Generate SGR escape sequence for the given attributes *)

val reset : string
(** Reset all attributes *)

val reset_bold_dim : string
(** Reset bold and dim attributes *)

val reset_italic : string
(** Reset italic attribute *)

val reset_underline : string
(** Reset underline attributes *)

val reset_blink : string
(** Reset blink attribute *)

val reset_reverse : string
(** Reset reverse attribute *)

val reset_strikethrough : string
(** Reset strikethrough attribute *)

val reset_overline : string
(** Reset overline attribute *)

(** {1 Utilities} *)

val strip : string -> string
(** Remove all ANSI escape sequences from a string *)

val hyperlink : uri:string -> string -> string
(** [hyperlink ~uri text] creates a terminal hyperlink. The link will display
    [text] and link to [uri]. Note: Not all terminals support this feature. *)

val style : attr list -> string -> string
(** [style attrs s] wraps string [s] with the SGR sequences for [attrs] and
    appends a reset sequence. This is a convenience function that ensures styles
    are properly reset after the text. *)
