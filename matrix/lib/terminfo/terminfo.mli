(** Typed access to terminfo capabilities.

    [Terminfo] loads entries from the system terminfo database and exposes each
    capability as a phantom-typed token, so the compiler enforces the return
    type of every lookup while staying close to the original terminfo design.

    {1 Overview}

    - [load] reads a single entry and returns an immutable {!t}.
    - [get] performs an O(1) lookup by supplying a {!cap}.
    - Parameterized capabilities return formatters that emit ready-to-send
      escape sequences using the entry's [%]-expressions.

    {1 Loading entries}

    [load] searches [/usr/share/terminfo], [/lib/terminfo], [/etc/terminfo], and
    [$HOME/.terminfo] (when present) for the entry named by [?term] or by the
    [TERM] environment variable. The parser allocates fresh hashtables for the
    entry's boolean, numeric, and string tables and populates them from the
    file, making each handle independent and safe to use concurrently. Errors
    surface as [`Not_found] when no entry exists or [`Parse_error _] when the
    file is malformed.

    {1 Capability tokens}

    Capabilities are represented by the {!cap} GADT. The phantom type encodes
    whether [get] returns [bool], [int], [string], or higher-order functions
    such as [int -> string] or [(int * int) -> string]. Supplying a mismatched
    constructor is a compile-time error, and each constructor maps directly to a
    documented terminfo capability name.

    {1 Retrieval}

    [get ti cap] never mutates [ti] and returns [None] when the capability is
    missing. Parameterized capabilities close over the underlying formatter: the
    returned function evaluates the terminfo [%] expression on every call and
    produces a fresh string ready to write to the terminal. Arguments are passed
    through the formatter verbatim. If the capability string contains the [%i]
    increment directive, coordinates are adjusted by the formatter; otherwise,
    use the exact values expected by the terminal (typically 0-based or 1-based
    depending on the capability). {!Has_colors} is synthesized from the numeric
    [colors] field and therefore always yields [Some true] or [Some false].

    {1 Example}

    {[
      match Terminfo.load () with
      | Error (`Parse_error msg) ->
          prerr_endline ("terminfo parse error: " ^ msg)
      | Error `Not_found -> prerr_endline "missing terminfo entry"
      | Ok ti -> (
          Option.iter print_string (Terminfo.get ti Terminfo.Clear_screen);
          (match Terminfo.get ti Terminfo.Cursor_position with
          | Some goto -> print_string (goto (5, 10))
          | None -> ());
          match Terminfo.get ti Terminfo.Has_colors with
          | Some true -> print_endline "colors enabled"
          | _ -> print_endline "monochrome fallback")
    ]} *)

(** {1 Terminal Capabilities} *)

(** Terminal capability token whose phantom type encodes the return type. Each
    constructor mirrors a standard terminfo capability and constrains {!get} to
    return the associated OCaml type. *)
type _ cap =
  (* Boolean capabilities *)
  | Auto_left_margin : bool cap  (** Terminal has automatic left margins (bw) *)
  | Auto_right_margin : bool cap  (** Terminal has automatic margins (am) *)
  | Back_color_erase : bool cap
      (** Screen erased with background color (bce) *)
  | Can_change : bool cap  (** Terminal can redefine existing colors (ccc) *)
  | Eat_newline_glitch : bool cap
      (** Newline ignored after 80 columns (xenl) *)
  | Has_colors : bool cap
      (** Terminal can display colors. Synthesized from the numeric [colors]
          capability rather than the boolean table. Returns [Some true] if
          [colors] is present and greater than 0, otherwise [Some false]. *)
  | Has_meta_key : bool cap  (** Terminal has a meta key (km) *)
  | Insert_null_glitch : bool cap  (** Insert mode distinguishes nulls (in) *)
  | Move_insert_mode : bool cap  (** Safe to move while in insert mode (mir) *)
  | Move_standout_mode : bool cap
      (** Safe to move while in standout mode (msgr) *)
  | Over_strike : bool cap  (** Terminal can overstrike (os) *)
  | Transparent_underline : bool cap
      (** Underline character overstrikes (ul) *)
  | Xon_xoff : bool cap  (** Terminal uses xon/xoff handshaking (xon) *)
  (* Numeric capabilities *)
  | Columns : int cap  (** Number of columns in a line (cols) *)
  | Lines : int cap  (** Number of lines on screen (lines) *)
  | Max_colors : int cap  (** Maximum number of colors (colors) *)
  | Max_pairs : int cap  (** Maximum number of color pairs (pairs) *)
  | Max_attributes : int cap
      (** Maximum combined attributes terminal can handle (ma) *)
  | Init_tabs : int cap  (** Tabs initially every # spaces (it) *)
  | Virtual_terminal : int cap  (** Virtual terminal number (vt) *)
  (* String capabilities - non-parameterized *)
  | Bell : string cap  (** Audible bell character (bel) *)
  | Carriage_return : string cap  (** Carriage return (cr) *)
  | Clear_screen : string cap  (** Clear screen and home cursor (clear) *)
  | Clear_to_eol : string cap  (** Clear to end of line (el) *)
  | Clear_to_eos : string cap  (** Clear to end of screen (ed) *)
  | Cursor_down : string cap  (** Down one line (cud1) *)
  | Cursor_home : string cap  (** Home cursor (upper left corner) (home) *)
  | Cursor_invisible : string cap  (** Make cursor invisible (civis) *)
  | Cursor_left : string cap  (** Left one character (cub1) *)
  | Cursor_normal : string cap  (** Make cursor normal (cnorm) *)
  | Cursor_right : string cap  (** Right one character (cuf1) *)
  | Cursor_up : string cap  (** Up one line (cuu1) *)
  | Cursor_visible : string cap  (** Make cursor very visible (cvvis) *)
  | Delete_character : string cap  (** Delete character (dch1) *)
  | Delete_line : string cap  (** Delete line (dl1) *)
  | Enter_alt_charset : string cap  (** Start alternate character set (smacs) *)
  | Enter_blink_mode : string cap  (** Turn on blinking (blink) *)
  | Enter_bold_mode : string cap  (** Turn on bold mode (bold) *)
  | Enter_dim_mode : string cap  (** Turn on dim mode (dim) *)
  | Enter_insert_mode : string cap  (** Enter insert mode (smir) *)
  | Enter_reverse_mode : string cap  (** Turn on reverse video (rev) *)
  | Enter_standout_mode : string cap  (** Begin standout mode (smso) *)
  | Enter_underline_mode : string cap  (** Turn on underline mode (smul) *)
  | Exit_alt_charset : string cap  (** End alternate character set (rmacs) *)
  | Exit_attribute_mode : string cap  (** Turn off all attributes (sgr0) *)
  | Exit_insert_mode : string cap  (** End insert mode (rmir) *)
  | Exit_standout_mode : string cap  (** End standout mode (rmso) *)
  | Exit_underline_mode : string cap  (** End underline mode (rmul) *)
  | Flash_screen : string cap  (** Visible bell (flash) *)
  | Insert_character : string cap  (** Insert character (ich1) *)
  | Insert_line : string cap  (** Insert line (il1) *)
  | Keypad_local : string cap  (** Put keypad in local mode (rmkx) *)
  | Keypad_xmit : string cap  (** Put keypad in transmit mode (smkx) *)
  | Newline : string cap  (** Newline (behaves like cr followed by lf) (nel) *)
  | Reset_1string : string cap  (** Reset terminal to sane modes (rs1) *)
  | Reset_2string : string cap  (** Reset terminal to sane modes (rs2) *)
  | Restore_cursor : string cap  (** Restore cursor position (rc) *)
  | Save_cursor : string cap  (** Save cursor position (sc) *)
  | Scroll_forward : string cap  (** Scroll forward one line (ind) *)
  | Scroll_reverse : string cap  (** Scroll reverse one line (ri) *)
  | Tab : string cap  (** Tab character (ht) *)
  (* Parameterized capabilities *)
  | Column_address : (int -> string) cap  (** Move cursor to column (hpa) *)
  | Cursor_position : (int * int -> string) cap
      (** Move cursor to [(row, col)] (cup). The terminfo formatter applies any
          coordinate transformations specified in the capability string, such as
          [%i] for 1-based conversion. *)
  | Delete_chars : (int -> string) cap  (** Delete n characters (dch) *)
  | Delete_lines : (int -> string) cap  (** Delete n lines (dl) *)
  | Insert_chars : (int -> string) cap  (** Insert n characters (ich) *)
  | Insert_lines : (int -> string) cap  (** Insert n lines (il) *)
  | Parm_down_cursor : (int -> string) cap
      (** Move cursor down n lines (cud) *)
  | Parm_left_cursor : (int -> string) cap
      (** Move cursor left n characters (cub) *)
  | Parm_right_cursor : (int -> string) cap
      (** Move cursor right n characters (cuf) *)
  | Parm_up_cursor : (int -> string) cap  (** Move cursor up n lines (cuu) *)
  | Repeat_char : (char * int -> string) cap
      (** Repeat character n times (rep) *)
  | Row_address : (int -> string) cap  (** Move cursor to row (vpa) *)
  | Set_background : (int -> string) cap  (** Set background color (setab) *)
  | Set_foreground : (int -> string) cap  (** Set foreground color (setaf) *)

(** {1 Terminal Information} *)

type t
(** Immutable handle to a parsed terminfo entry. All lookups reuse in-memory
    tables and the value can be shared freely across threads. *)

val load :
  ?term:string -> unit -> (t, [ `Not_found | `Parse_error of string ]) result
(** [load ?term ()] loads the terminfo entry named by [term] or by the [TERM]
    environment variable. Search order matches the ncurses database layout:
    [/usr/share/terminfo], [/lib/terminfo], [/etc/terminfo], and
    [$HOME/.terminfo] (when present). The entire entry is parsed eagerly and
    loaded into memory.

    Each call creates a new independent handle with fresh hashtables populated
    from the file. No caching is performed across calls.

    Returns [Error `Not_found] if no entry can be located and
    [Error (`Parse_error msg)] if the file cannot be decoded.

    @raise Sys_error if the located entry exists but cannot be opened or read.
*)

val get : t -> 'a cap -> 'a option
(** [get ti cap] returns the capability associated with [cap] from [ti].

    - Boolean capabilities return [bool option].
    - Numeric capabilities return [int option].
    - String capabilities return [string option].
    - Parameterized capabilities return an option-wrapped formatter that
      produces escape sequences using the terminfo [%] expression. Each call to
      the returned function evaluates the [%] expression and allocates a fresh
      string.

    The function never mutates [ti] and returns [None] when the capability is
    undeclared. {!Has_colors} always yields [Some true] or [Some false] because
    it is synthesized from the numeric [colors] field rather than the boolean
    table, ensuring a definite result. *)
