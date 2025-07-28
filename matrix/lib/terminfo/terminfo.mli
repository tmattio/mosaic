(** Type-safe terminfo library using GADTs.

    This library provides a strongly-typed interface to terminal capabilities,
    eliminating string-based lookups and runtime errors from typos.

    {1 Example}

    {[
      match Terminfo.load () with
      | Error `Not_found -> 
          prerr_endline "No terminfo database found"
      | Error (`Parse_error msg) -> 
          prerr_endline ("Parse error: " ^ msg)
      | Ok ti ->
          (* Clear screen - type system knows this returns string option *)
          begin match Terminfo.get ti Clear_screen with
          | None -> print_string "\027[2J\027[H"  (* ANSI fallback *)
          | Some clear -> print_string clear
          end;
          
          (* Move cursor - type system knows this returns (int * int -> string) option *)
          begin match Terminfo.get ti Cursor_position with
          | None -> Printf.printf "\027[%d;%dH" 10 20  (* ANSI fallback *)
          | Some move -> print_string (move (10, 20))
          end;
          
          (* Check capabilities - type system knows this returns bool option *)
          match Terminfo.get ti Has_colors with
          | Some true -> print_endline "Terminal supports colors!"
          | _ -> print_endline "No color support"
    ]}
*)

(** {1 Terminal Capabilities} *)

(** Terminal capability with its return type encoded in the type parameter.
    The type system ensures you get the correct return type for each capability. *)
type _ cap =
  (* Boolean capabilities *)
  | Auto_left_margin : bool cap
      (** Terminal has automatic left margins (bw) *)
  | Auto_right_margin : bool cap
      (** Terminal has automatic margins (am) *)
  | Back_color_erase : bool cap
      (** Screen erased with background color (bce) *)
  | Can_change : bool cap
      (** Terminal can redefine existing colors (ccc) *)
  | Eat_newline_glitch : bool cap
      (** Newline ignored after 80 columns (xenl) *)
  | Has_colors : bool cap
      (** Terminal can display colors (colors) *)
  | Has_meta_key : bool cap
      (** Terminal has a meta key (km) *)
  | Insert_null_glitch : bool cap
      (** Insert mode distinguishes nulls (in) *)
  | Move_insert_mode : bool cap
      (** Safe to move while in insert mode (mir) *)
  | Move_standout_mode : bool cap
      (** Safe to move while in standout mode (msgr) *)
  | Over_strike : bool cap
      (** Terminal can overstrike (os) *)
  | Transparent_underline : bool cap
      (** Underline character overstrikes (ul) *)
  | Xon_xoff : bool cap
      (** Terminal uses xon/xoff handshaking (xon) *)

  (* Numeric capabilities *)
  | Columns : int cap
      (** Number of columns in a line (cols) *)
  | Lines : int cap
      (** Number of lines on screen (lines) *)
  | Max_colors : int cap
      (** Maximum number of colors (colors) *)
  | Max_pairs : int cap
      (** Maximum number of color pairs (pairs) *)
  | Max_attributes : int cap
      (** Maximum combined attributes terminal can handle (ma) *)
  | Init_tabs : int cap
      (** Tabs initially every # spaces (it) *)
  | Virtual_terminal : int cap
      (** Virtual terminal number (vt) *)

  (* String capabilities - non-parameterized *)
  | Bell : string cap
      (** Audible bell character (bel) *)
  | Carriage_return : string cap
      (** Carriage return (cr) *)
  | Clear_screen : string cap
      (** Clear screen and home cursor (clear) *)
  | Clear_to_eol : string cap
      (** Clear to end of line (el) *)
  | Clear_to_eos : string cap
      (** Clear to end of screen (ed) *)
  | Cursor_down : string cap
      (** Down one line (cud1) *)
  | Cursor_home : string cap
      (** Home cursor (upper left corner) (home) *)
  | Cursor_invisible : string cap
      (** Make cursor invisible (civis) *)
  | Cursor_left : string cap
      (** Left one character (cub1) *)
  | Cursor_normal : string cap
      (** Make cursor normal (cnorm) *)
  | Cursor_right : string cap
      (** Right one character (cuf1) *)
  | Cursor_up : string cap
      (** Up one line (cuu1) *)
  | Cursor_visible : string cap
      (** Make cursor very visible (cvvis) *)
  | Delete_character : string cap
      (** Delete character (dch1) *)
  | Delete_line : string cap
      (** Delete line (dl1) *)
  | Enter_alt_charset : string cap
      (** Start alternate character set (smacs) *)
  | Enter_blink_mode : string cap
      (** Turn on blinking (blink) *)
  | Enter_bold_mode : string cap
      (** Turn on bold mode (bold) *)
  | Enter_dim_mode : string cap
      (** Turn on dim mode (dim) *)
  | Enter_insert_mode : string cap
      (** Enter insert mode (smir) *)
  | Enter_reverse_mode : string cap
      (** Turn on reverse video (rev) *)
  | Enter_standout_mode : string cap
      (** Begin standout mode (smso) *)
  | Enter_underline_mode : string cap
      (** Turn on underline mode (smul) *)
  | Exit_alt_charset : string cap
      (** End alternate character set (rmacs) *)
  | Exit_attribute_mode : string cap
      (** Turn off all attributes (sgr0) *)
  | Exit_insert_mode : string cap
      (** End insert mode (rmir) *)
  | Exit_standout_mode : string cap
      (** End standout mode (rmso) *)
  | Exit_underline_mode : string cap
      (** End underline mode (rmul) *)
  | Flash_screen : string cap
      (** Visible bell (flash) *)
  | Insert_character : string cap
      (** Insert character (ich1) *)
  | Insert_line : string cap
      (** Insert line (il1) *)
  | Keypad_local : string cap
      (** Put keypad in local mode (rmkx) *)
  | Keypad_xmit : string cap
      (** Put keypad in transmit mode (smkx) *)
  | Newline : string cap
      (** Newline (behaves like cr followed by lf) (nel) *)
  | Reset_1string : string cap
      (** Reset terminal to sane modes (rs1) *)
  | Reset_2string : string cap
      (** Reset terminal to sane modes (rs2) *)
  | Restore_cursor : string cap
      (** Restore cursor position (rc) *)
  | Save_cursor : string cap
      (** Save cursor position (sc) *)
  | Scroll_forward : string cap
      (** Scroll forward one line (ind) *)
  | Scroll_reverse : string cap
      (** Scroll reverse one line (ri) *)
  | Tab : string cap
      (** Tab character (ht) *)

  (* Parameterized capabilities *)
  | Column_address : (int -> string) cap
      (** Move cursor to column (hpa) *)
  | Cursor_position : (int * int -> string) cap
      (** Move cursor to row, col (cup) - 0-based *)
  | Delete_chars : (int -> string) cap
      (** Delete n characters (dch) *)
  | Delete_lines : (int -> string) cap
      (** Delete n lines (dl) *)
  | Insert_chars : (int -> string) cap
      (** Insert n characters (ich) *)
  | Insert_lines : (int -> string) cap
      (** Insert n lines (il) *)
  | Parm_down_cursor : (int -> string) cap
      (** Move cursor down n lines (cud) *)
  | Parm_left_cursor : (int -> string) cap
      (** Move cursor left n characters (cub) *)
  | Parm_right_cursor : (int -> string) cap
      (** Move cursor right n characters (cuf) *)
  | Parm_up_cursor : (int -> string) cap
      (** Move cursor up n lines (cuu) *)
  | Repeat_char : (char * int -> string) cap
      (** Repeat character n times (rep) *)
  | Row_address : (int -> string) cap
      (** Move cursor to row (vpa) *)
  | Set_background : (int -> string) cap
      (** Set background color (setab) *)
  | Set_foreground : (int -> string) cap
      (** Set foreground color (setaf) *)

(** {1 Terminal Information} *)

type t
(** An opaque type representing a loaded terminfo database for a terminal. *)

val load : ?term:string -> unit -> (t, [`Not_found | `Parse_error of string]) result
(** [load ?term ()] attempts to load the terminfo entry for the specified
    terminal. If [term] is not provided, it uses the TERM environment
    variable. Returns [Error `Not_found] if the entry cannot be found,
    or [Error (`Parse_error msg)] if parsing fails. *)

val get : t -> 'a cap -> 'a option
(** [get ti cap] retrieves a capability from the terminal database.
    The return type is determined by the capability type:
    - Boolean capabilities return [bool option]
    - Numeric capabilities return [int option]
    - String capabilities return [string option]
    - Parameterized capabilities return function options
    
    Returns [None] if the capability is not defined for this terminal. *)

