(** The modes of a terminal, set by a program and put back when it is done.

    A terminal has {e modes}, settings that a program changes by writing control
    functions to it, such as mouse tracking, bracketed paste or the alternate
    screen. A {e handle} ({!t}) writes these control functions through an
    {e output} function that the caller gives, and tracks the modes it has set,
    so that it writes each change once and {!reset_state} puts back every mode
    it set. The caller owns all I/O. This library reads no descriptor, waits for
    nothing, starts no thread and handles no signal, so a program with no
    operating system under it can use it. The device itself, its settings and
    its signals belong to [Tty].

    A handle also holds the terminal's {e capabilities} ({!Caps}), which the
    caller derives from what the terminal answers and replaces as answers
    arrive. The handle reads only their multiplexer, to wrap what it writes for
    the terminal outside the multiplexer.

    The life cycle of a handle:
    + {!make} makes a handle with the capabilities known so far.
    + The functions of {!section-protocol} set modes, and {!set_capabilities}
      replaces the capabilities as answers arrive.
    + {!restore_modes} writes the modes again when the terminal may have reset
      them, as after the process was stopped and continued.
    + {!reset_state} puts back every mode the handle set.

    Unless a function's doc says otherwise, a function that sets a mode writes
    its control function only when the mode changes, so a second call with the
    same value writes nothing. A handle made with [~tty:false] writes nothing
    and tracks the modes all the same. An exception that [output] raises passes
    to the caller. For mouse tracking, bracketed paste, focus reporting, the
    kitty keyboard flags, modifyOtherKeys, color scheme reports, resize reports
    and metaSendsEscape, {!reset_state} then still puts back the mode whose
    control function [output] was given. For the other modes, the handle records
    a change only once [output] has returned. A handle must not be used by two
    threads at once.

    This takes the alternate screen and bracketed paste, and puts both back:
    {[
    let () =
      let output s =
        print_string s;
        flush stdout
      in
      let caps =
        Matrix_terminal.Caps.(
          derive (evidence (env Sys.getenv_opt ~win32:Sys.win32)))
      in
      let t = Matrix_terminal.make ~output caps in
      Matrix_terminal.enter_alternate_screen t;
      Matrix_terminal.enable_bracketed_paste t true;
      Matrix_terminal.reset_state t
    ]}

    {1:types Types} *)

type mouse_mode =
  [ `Off
  | `X10
  | `Normal
  | `Button
  | `Any
  | `Sgr_normal
  | `Sgr_button
  | `Sgr_any ]
(** The type for mouse tracking modes.
    - [`Off] reports nothing.
    - [`X10] reports presses (mode 9).
    - [`Normal] reports presses and releases (mode 1000).
    - [`Button] also reports motion while a button is held (mode 1002).
    - [`Any] also reports motion with no button held (mode 1003).
    - [`Sgr_normal], [`Sgr_button] and [`Sgr_any] report as [`Normal], [`Button]
      and [`Any] do, in the SGR encoding (mode 1006). [`Sgr_normal] sets modes
      1006 and 1000, [`Sgr_button] also 1002, and [`Sgr_any] also 1003.

    In the modes without SGR the terminal reports each coordinate in one byte,
    so it reports columns and rows up to 223. *)

type cursor_position = { x : int; y : int; visible : bool }
(** The type for the cursor state a handle tracks. [x] is the column and [y] the
    row of the last {!move_cursor}, both from [1], and [visible] is the
    visibility last set. *)

(** {1:capabilities Capabilities} *)

module Caps = Caps
(** Terminal capabilities, derived from evidence. *)

type capabilities = Caps.t
(** The type for terminal capabilities. *)

(** {1:handle Handles} *)

type t
(** The type for handles. *)

val make : output:(string -> unit) -> ?tty:bool -> capabilities -> t
(** [make ~output ~tty caps] is a handle that writes through [output] and holds
    [caps]. With [~tty:false] (default [true]) it writes nothing, for an output
    that is not a terminal or a test. It starts with every mode off, the primary
    screen, the whole screen as the scrolling region, and the cursor visible at
    [(1, 1)] in the block style with the color [(1., 1., 1., 1.)]. It writes
    nothing and asks the terminal nothing. *)

val close : t -> unit
(** [close t] is [reset_state t]. *)

(** {1:cap_access Capability access} *)

val capabilities : t -> capabilities
(** [capabilities t] is [t]'s capabilities. *)

val set_capabilities : t -> capabilities -> unit
(** [set_capabilities t caps] replaces [t]'s capabilities with [caps]. It writes
    nothing and changes no mode. *)

val pixel_resolution : t -> (int * int) option
(** [pixel_resolution t] is the size of the terminal's text area in pixels,
    [Some (width, height)], as {!set_pixel_resolution} last recorded it, or
    [None] before it has. *)

val set_pixel_resolution : t -> (int * int) option -> unit
(** [set_pixel_resolution t res] records [res] as [t]'s pixel resolution. It
    writes nothing. *)

(** {1:protocol Modes}

    Each function below sets or resets one mode, and {!reset_state} resets it if
    the handle set it. *)

val restore_modes : ?skip_focus:bool -> t -> unit
(** [restore_modes ~skip_focus t] writes again the control functions of the
    modes the handle has set among mouse tracking, focus reporting, bracketed
    paste, the kitty keyboard flags, modifyOtherKeys, color scheme reports,
    resize reports and metaSendsEscape, whether or not they changed. It pops the
    handle's kitty keyboard flags before it pushes them again, so the terminal's
    stack keeps one entry from the handle. With [~skip_focus:true] (default
    [false]) it leaves out focus reporting, since a terminal may answer the
    focus mode with a focus report, and a caller that restores the modes on each
    focus report would loop. *)

(** {2:mouse Mouse tracking} *)

val set_mouse_mode : t -> mouse_mode -> unit
(** [set_mouse_mode t mode] sets mouse tracking to [mode]. When [mode] differs
    from the current mode, it first resets every mouse mode, 9, 1000, 1002,
    1003, 1015 and 1006, then sets those of [mode]. *)

val mouse_mode : t -> mouse_mode
(** [mouse_mode t] is the mouse tracking mode last set, initially [`Off]. *)

(** {2:paste Bracketed paste} *)

val enable_bracketed_paste : t -> bool -> unit
(** [enable_bracketed_paste t b] sets bracketed paste (mode 2004) if [b], and
    resets it otherwise. *)

val bracketed_paste_enabled : t -> bool
(** [bracketed_paste_enabled t] is [true] iff [t] has set bracketed paste. *)

(** {2:focus Focus reporting} *)

val enable_focus_reporting : t -> bool -> unit
(** [enable_focus_reporting t b] sets focus reporting (mode 1004) if [b], and
    resets it otherwise. *)

val focus_reporting_enabled : t -> bool
(** [focus_reporting_enabled t] is [true] iff [t] has set focus reporting. *)

(** {2:kitty_kb Kitty keyboard} *)

val enable_kitty_keyboard : ?flags:int -> t -> bool -> unit
(** [enable_kitty_keyboard ~flags t b] pushes [flags] on the terminal's stack of
    kitty keyboard flags if [b], and pops the handle's entry otherwise. [flags]
    defaults to [0b00101], and its bits are
    - bit 0, disambiguate escape codes;
    - bit 1, report event types (press, repeat and release);
    - bit 2, report alternate keys;
    - bit 3, report all keys as escape codes;
    - bit 4, report associated text.

    A push while the handle has other flags pushed pops them first, so the
    handle has at most one entry on the stack and one pop removes it. *)

val kitty_keyboard_enabled : t -> bool
(** [kitty_keyboard_enabled t] is [true] iff [t] has pushed kitty keyboard
    flags. *)

(** {2:mok modifyOtherKeys} *)

val enable_modify_other_keys : t -> bool -> unit
(** [enable_modify_other_keys t b] sets xterm's modifyOtherKeys to 1
    ([CSI > 4 ; 1 m]) if [b], and to 0 otherwise. *)

val modify_other_keys_enabled : t -> bool
(** [modify_other_keys_enabled t] is [true] iff [t] has set modifyOtherKeys. *)

(** {2:reports Reports and Alt} *)

val enable_color_scheme_reports : t -> bool -> unit
(** [enable_color_scheme_reports t b] sets color scheme reports (mode 2031) if
    [b], and resets them otherwise. While they are set, the terminal sends
    [CSI ? 997 ; 1 n] (dark) or [CSI ? 997 ; 2 n] (light) when its scheme
    changes. *)

val enable_resize_reports : t -> bool -> unit
(** [enable_resize_reports t b] sets in-band resize reports (mode 2048) if [b],
    and resets them otherwise. While they are set, the terminal sends
    [CSI 48 ; rows ; cols ; height ; width t] once when the mode is set and on
    every resize. *)

val enable_meta_sends_escape : t -> bool -> unit
(** [enable_meta_sends_escape t b] sets xterm's metaSendsEscape (mode 1036) if
    [b], and resets it otherwise. While it is set, Alt with a key sends ESC and
    then the key. A caller must set it only where the terminal has it reset
    ({!Caps.t.meta_sends_escape} is [Some false]), since {!reset_state} resets
    it. *)

(** {2:unicode_w Unicode width} *)

val enable_grapheme_mode : t -> bool -> unit
(** [enable_grapheme_mode t b] sets the terminal's grapheme cluster mode (mode
    2027) if [b], and resets it otherwise. A caller must set it only where the
    terminal has it reset ({!Caps.t.grapheme_mode} is [Some false]), since
    {!reset_state} resets it. *)

(** {2:alt_screen Alternate screen} *)

val enter_alternate_screen : t -> unit
(** [enter_alternate_screen t] switches to the alternate screen (DECSET 1049).
*)

val leave_alternate_screen : t -> unit
(** [leave_alternate_screen t] switches back to the primary screen (DECRST
    1049). *)

val alt_screen : t -> bool
(** [alt_screen t] is [true] iff [t] has switched to the alternate screen. *)

(** {2:scroll Scrolling region} *)

val set_scroll_region : t -> top:int -> bottom:int -> unit
(** [set_scroll_region t ~top ~bottom] sets the scrolling region (DECSTBM) to
    the rows [top] to [bottom], from [1].

    Raises [Invalid_argument] if [top < 1] or [bottom <= top], even when [t]
    writes nothing. *)

val clear_scroll_region : t -> unit
(** [clear_scroll_region t] makes the whole screen the scrolling region. *)

val scroll_region : t -> (int * int) option
(** [scroll_region t] is the scrolling region [t] has set, [Some (top, bottom)],
    or [None] for the whole screen. *)

(** {1:cursor Cursor and appearance}

    The [write_] functions append to a writer that the caller writes out later,
    as part of a frame. They append for a handle made with [~tty:false] too, and
    change none of the cursor state the handle tracks. *)

val move_cursor : ?visible:bool -> t -> row:int -> col:int -> unit
(** [move_cursor ~visible t ~row ~col] moves the cursor to [row] and [col], from
    [1], each raised to [1] if smaller, and records the position
    ({!val-cursor_position}). It first shows or hides the cursor as [visible]
    says (default [true]), as {!set_cursor_visible} does. It writes the move
    even when the cursor is there already. *)

val set_cursor_visible : t -> bool -> unit
(** [set_cursor_visible t b] shows the cursor if [b] and hides it otherwise
    (DECTCEM). *)

val cursor_visible : t -> bool
(** [cursor_visible t] is the visibility last set, initially [true]. *)

val cursor_position : t -> cursor_position
(** [cursor_position t] is the position of the last {!move_cursor}, initially
    [(1, 1)], with the visibility last set. *)

val set_cursor_style : t -> Ansi.cursor_style -> blinking:bool -> unit
(** [set_cursor_style t style ~blinking] records the cursor's style and
    blinking. While the cursor is visible, it writes them (DECSCUSR) with the
    color {!cursor_color} records, which is white until {!set_cursor_color}, and
    {!reset_state} then puts back both. While the cursor is hidden, it writes
    the control function that hides it. It writes every time. *)

val cursor_style_state : t -> Ansi.cursor_style * bool
(** [cursor_style_state t] is the cursor's style and blinking last recorded,
    initially [(`Block, false)]. *)

val set_cursor_color : t -> r:float -> g:float -> b:float -> a:float -> unit
(** [set_cursor_color t ~r ~g ~b ~a] records the cursor's color, with components
    from [0.0] to [1.0]. While the cursor is visible, it writes the color (OSC
    12), each component clamped to that range and [nan] taken as [0.0], with the
    recorded style ({!set_cursor_style}), and {!reset_state} then puts back
    both. While the cursor is hidden, it writes the control function that hides
    it. [a] is recorded and never written. It writes every time. *)

val cursor_color : t -> float * float * float * float
(** [cursor_color t] is the cursor's color last recorded, [(r, g, b, a)] as
    given, initially [(1., 1., 1., 1.)]. *)

val reset_cursor_color : t -> unit
(** [reset_cursor_color t] gives the cursor the terminal's default color (OSC 12
    with ["default"], then OSC 112), and records [(1., 1., 1., 1.)]. *)

val write_cursor_style : t -> shape:Ansi.cursor_shape -> Ansi.Writer.t -> unit
(** [write_cursor_style t ~shape w] appends to [w] the control function that
    gives the cursor [shape] (DECSCUSR), and has {!reset_state} put back the
    default shape. *)

val write_cursor_color : t -> (int * int * int) option -> Ansi.Writer.t -> unit
(** [write_cursor_color t color w] appends to [w] the control function that
    gives the cursor [color] (OSC 12), each component clamped to [0] to [255],
    and has {!reset_state} put back the default color. With [None], it appends
    the control functions of {!reset_cursor_color}, and {!reset_state} still
    puts back the default color, since the writer's bytes may never reach the
    terminal. *)

val set_title : t -> string -> unit
(** [set_title t s] sets the window title and the icon name to [s] (OSC 0), and
    has {!reset_state} set them to [""]. [s] must hold no control character,
    since [s] is written as it is and one would end the control function early.
*)

val copy_to_clipboard : t -> string -> unit
(** [copy_to_clipboard t s] asks the terminal to set the clipboard to [s] (OSC
    52, with [s] in base64). Inside tmux ({!Caps.t.multiplexer}) it wraps the
    request for tmux's passthrough. Inside GNU screen it cuts the request into
    DCS strings of at most 252 bytes each, which screen passes to the terminal
    outside. *)

val notify : t -> title:string -> body:string -> unit
(** [notify t ~title ~body] posts a desktop notification as OSC 9, then OSC 777,
    and a terminal ignores the one it does not know. Inside tmux it wraps both
    for tmux's passthrough. [title] and [body] must hold no control character
    and [title] no [;], since they are written as they are. *)

val bell : t -> unit
(** [bell t] rings the terminal's bell (BEL). *)

val query_color_scheme : t -> unit
(** [query_color_scheme t] asks the terminal for its color scheme
    ([CSI ? 996 n]). A terminal that knows the query answers [CSI ? 997 ; 1 n]
    (dark) or [CSI ? 997 ; 2 n] (light), which decodes as
    {!Matrix_input.Reply.Color_scheme}. *)

val query_pixel_resolution : t -> unit
(** [query_pixel_resolution t] asks the terminal for the size of its text area
    in pixels ([CSI 14 t]). The answer decodes as
    {!Matrix_input.Reply.Text_area_pixels}. *)

(** {1:output Output and state} *)

val send : t -> string -> unit
(** [send t s] writes [s] through [output], and nothing for a handle made with
    [~tty:false]. Whoever reads the terminal's input must expect the answer to a
    query sent so ({!Matrix_input.Decoder.expect}), since a reply that is a
    control string, such as XTVERSION's, otherwise decodes as typed keys. *)

val tty : t -> bool
(** [tty t] is [true] iff [t] writes, as {!make}'s [~tty] says. *)

val reset_state : t -> unit
(** [reset_state t] puts back every mode [t] has set. It always shows the cursor
    and resets the SGR attributes. Of the rest, it puts back only what the
    handle changed. It gives the cursor its default color and shape, pops the
    handle's kitty keyboard flags, sets modifyOtherKeys to 0, and resets mouse
    tracking, bracketed paste, focus reporting, color scheme reports, resize
    reports, metaSendsEscape and grapheme cluster mode. It makes the whole
    screen the scrolling region and leaves the alternate screen. On Windows,
    when the primary screen shows, it moves the cursor up by the tracked row
    less one ({!val-cursor_position}) and erases from there to the end of the
    screen. It sets the window title to [""] if the handle set it. The handle
    then tracks the state {!make} gives, but for its capabilities, its pixel
    resolution and its cursor position. *)
