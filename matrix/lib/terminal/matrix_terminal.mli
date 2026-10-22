(** Terminal protocol state machine.

    [Terminal] manages terminal protocol state and writes escape sequences
    through a caller-provided {e output} callback. The caller ({e runtime}) owns
    all I/O resources: no {!Unix.select}, no threads, no wakeup pipe, no signal
    handling.

    {2:caps_detect Capabilities}

    The handle holds the terminal's capabilities, which decide how it wraps
    output for a multiplexer. The runtime derives them ({!Caps}) and sets them
    ({!set_capabilities}) as answers arrive.

    {2:protocol_sym Protocol symmetry}

    Every protocol enabled through the handle is unwound by {!close} or
    {!reset_state}. Protocol operations are idempotent: requesting a mode that
    is already active is a no-op. For non-TTY handles ([tty = false]) escape
    sequences are suppressed but internal state tracking remains active.

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
(** The type for mouse tracking protocols.
    - [`Off] disables mouse tracking.
    - [`X10] is basic press-only tracking (legacy; coordinates limited to
      223x223 cells).
    - [`Normal] reports press and release events with standard xterm encoding
      (mode 1000).
    - [`Button] reports press, release, and drag events (mode 1002).
    - [`Any] reports all motion events, including motion with no buttons pressed
      (mode 1003).
    - [`Sgr_normal] is SGR-extended press and release only (modes 1006 + 1000).
    - [`Sgr_button] is SGR-extended with drag events (modes 1006 + 1000 + 1002).
    - [`Sgr_any] is SGR-extended with all motion (modes 1006 + 1000 + 1002 +
      1003). *)

type cursor_position = { x : int; y : int; visible : bool }
(** The type for tracked cursor positions. [x] and [y] are one-based
    coordinates; [(1, 1)] is the top-left corner. [visible] reflects the last
    requested visibility. *)

(** {1:capabilities Capabilities} *)

module Caps = Caps
(** Capabilities, derived from evidence. See {!Caps}. *)

type capabilities = Caps.t
(** The type for terminal capabilities. *)

(** {1:handle Session handle} *)

type t
(** The type for terminal protocol state. A value of type [t] encapsulates an
    output callback, capability and terminal metadata, and protocol state (mouse
    mode, cursor, alternate screen, etc.).

    Close with {!close} to unwind all active protocols. *)

val make : output:(string -> unit) -> ?tty:bool -> capabilities -> t
(** [make ~output ~tty caps] is a terminal handle with capabilities [caps]:
    - [output] receives escape sequence strings.
    - [tty] controls whether escape sequences are emitted. Defaults to [true].
      Set to [false] for non-TTY outputs or testing.

    The handle starts with no protocols enabled. *)

val close : t -> unit
(** [close t] unwinds all active protocols via [output].

    Disables mouse tracking, bracketed paste, focus reporting, Kitty keyboard,
    modifyOtherKeys, color scheme and resize reports, metaSendsEscape, Unicode
    mode, and alternate screen. Resets cursor visibility and SGR attributes.
    Cursor colour, cursor style, and the window title are reset only when they
    were changed through the handle, so terminal state the application never
    touched is left alone.

    {b Note.} Does not restore termios or close file descriptors; that is the
    runtime's responsibility. *)

(** {1:cap_access Capability access} *)

val capabilities : t -> capabilities
(** [capabilities t] is [t]'s current capability record. *)

val set_capabilities : t -> capabilities -> unit
(** [set_capabilities t caps] replaces [t]'s capability record with [caps]. *)

val pixel_resolution : t -> (int * int) option
(** [pixel_resolution t] is the last known pixel resolution as
    [Some (width, height)], or [None] if unknown. *)

val set_pixel_resolution : t -> (int * int) option -> unit
(** [set_pixel_resolution t res] updates [t]'s cached pixel resolution. *)

(** {1:protocol Protocol control}

    All protocol operations are idempotent. For TTY handles, escape sequences
    are emitted through [output]. For non-TTY handles, only internal state is
    updated. *)

val restore_modes : ?skip_focus:bool -> t -> unit
(** [restore_modes ?skip_focus t] unconditionally re-sends enable sequences for
    every currently-active protocol mode (mouse tracking, focus reporting,
    bracketed paste, Kitty keyboard, modifyOtherKeys, color scheme reports,
    resize reports, metaSendsEscape).

    When [skip_focus] is [true], the focus-tracking enable sequence is not
    re-sent. This avoids a feedback loop when [restore_modes] is called from a
    Focus event handler: some terminals re-report focus state upon receiving the
    enable sequence, creating an infinite cycle.

    Intended for focus-in recovery: some terminal emulators (notably Windows
    Terminal / ConPTY) strip DEC private modes when the window loses focus.

    {b Note.} For the Kitty keyboard protocol, the existing stack entry is
    popped before re-pushing to avoid unbounded stack growth. *)

(** {2:mouse Mouse tracking} *)

val set_mouse_mode : t -> mouse_mode -> unit
(** [set_mouse_mode t mode] configures mouse event tracking to [mode].
    Conflicting modes are disabled first. *)

val mouse_mode : t -> mouse_mode
(** [mouse_mode t] is [t]'s current mouse tracking mode. Initially [`Off]. *)

(** {2:paste Bracketed paste} *)

val enable_bracketed_paste : t -> bool -> unit
(** [enable_bracketed_paste t b] enables or disables bracketed paste mode
    (DECSET/DECRST 2004). Idempotent. *)

val bracketed_paste_enabled : t -> bool
(** [bracketed_paste_enabled t] is [true] iff bracketed paste mode is active. *)

(** {2:focus Focus reporting} *)

val enable_focus_reporting : t -> bool -> unit
(** [enable_focus_reporting t b] enables or disables focus event reporting
    (DECSET/DECRST 1004). Idempotent. *)

val focus_reporting_enabled : t -> bool
(** [focus_reporting_enabled t] is [true] iff focus event reporting is active.
*)

(** {2:kitty_kb Kitty keyboard} *)

val enable_kitty_keyboard : ?flags:int -> t -> bool -> unit
(** [enable_kitty_keyboard ~flags t b] enables or disables the Kitty keyboard
    protocol with:
    {ul
     {- [flags] is a bitmask controlling reported features. Defaults to
        [0b00101] (disambiguate escape codes + report alternate keys). The bits
        are:
        - bit 0: disambiguate escape codes
        - bit 1: report event types (press/repeat/release)
        - bit 2: report alternate keys
        - bit 3: report all keys as escape sequences
        - bit 4: report associated text
     }
    }

    Enabling pushes [flags] onto the terminal's keyboard stack; disabling pops.
    Changing [flags] while enabled pops the previous entry before pushing the
    new value, so the handle contributes at most one stack entry and a single
    pop ({!enable_kitty_keyboard} with [false], {!close}, or {!reset_state})
    fully unwinds it. *)

val kitty_keyboard_enabled : t -> bool
(** [kitty_keyboard_enabled t] is [true] iff the Kitty keyboard protocol is
    active. *)

(** {2:mok modifyOtherKeys} *)

val enable_modify_other_keys : t -> bool -> unit
(** [enable_modify_other_keys t b] enables or disables xterm modifyOtherKeys
    mode. Idempotent. *)

val modify_other_keys_enabled : t -> bool
(** [modify_other_keys_enabled t] is [true] iff modifyOtherKeys mode is active.
*)

(** {2:reports Reports and Alt} *)

val enable_color_scheme_reports : t -> bool -> unit
(** [enable_color_scheme_reports t b] enables or disables reports of color
    scheme changes (DECSET/DECRST 2031): the terminal sends [CSI ? 997 ; 1 n]
    (dark) or [CSI ? 997 ; 2 n] (light) when its scheme changes. Idempotent. *)

val enable_resize_reports : t -> bool -> unit
(** [enable_resize_reports t b] enables or disables in-band resize reports
    (DECSET/DECRST 2048): the terminal sends
    [CSI 48 ; rows ; cols ; height ; width t] on every resize and once when the
    mode is set. Idempotent. *)

val enable_meta_sends_escape : t -> bool -> unit
(** [enable_meta_sends_escape t b] sets or resets xterm's metaSendsEscape (mode
    1036): with it set, Alt with a key sends ESC before the key, rather than the
    key's character with its high bit set. Set it only where it was reset
    ({!Caps.t.meta_sends_escape}), so that {!reset_state} restores what was
    there. Idempotent. *)

(** {2:unicode_w Unicode width} *)

val set_unicode_width : t -> Matrix_text.width_method -> unit
(** [set_unicode_width t w] sets the terminal's grapheme mode (mode 2027) for
    [w]: set for [`Unicode] and [`Unicode_wide], reset for [`Wcwidth].
    Idempotent. *)

(** {2:alt_screen Alternate screen} *)

val enter_alternate_screen : t -> unit
(** [enter_alternate_screen t] switches to the alternate screen buffer (DECSET
    1049). Idempotent. *)

val leave_alternate_screen : t -> unit
(** [leave_alternate_screen t] returns to the primary screen buffer (DECRST
    1049). Idempotent. *)

val alt_screen : t -> bool
(** [alt_screen t] is [true] iff the alternate screen buffer is active. *)

(** {2:scroll Scroll region} *)

val set_scroll_region : t -> top:int -> bottom:int -> unit
(** [set_scroll_region t ~top ~bottom] sets the scrolling region (DECSTBM).
    [top] and [bottom] are one-based, inclusive. Idempotent. *)

val clear_scroll_region : t -> unit
(** [clear_scroll_region t] resets the scrolling region to the full screen.
    Idempotent. *)

val scroll_region : t -> (int * int) option
(** [scroll_region t] is [Some (top, bottom)] if a scrolling region is set, or
    [None] if the full screen is used. *)

(** {1:cursor Cursor and appearance} *)

val move_cursor : ?visible:bool -> t -> row:int -> col:int -> unit
(** [move_cursor ~visible t ~row ~col] moves the cursor to one-based coordinates
    [row], [col]. Coordinates are clamped to [>= 1]. [visible] defaults to
    [true]. *)

val set_cursor_visible : t -> bool -> unit
(** [set_cursor_visible t b] shows or hides the cursor (DECTCEM). Idempotent. *)

val cursor_visible : t -> bool
(** [cursor_visible t] is the last requested cursor visibility. *)

val cursor_position : t -> cursor_position
(** [cursor_position t] is [t]'s tracked cursor position and visibility. *)

val set_cursor_style : t -> Ansi.cursor_style -> blinking:bool -> unit
(** [set_cursor_style t style ~blinking] sets the cursor shape and blinking
    behaviour (DECSCUSR). *)

val cursor_style_state : t -> Ansi.cursor_style * bool
(** [cursor_style_state t] is the current cursor style and blinking flag as
    [(style, blinking)]. *)

val set_cursor_color : t -> r:float -> g:float -> b:float -> a:float -> unit
(** [set_cursor_color t ~r ~g ~b ~a] sets the cursor colour. Components are
    clamped to \[[0.0]; [1.0]\]. [NaN] components are treated as [0.0]. The
    alpha channel is tracked but not sent to the terminal. *)

val cursor_color : t -> float * float * float * float
(** [cursor_color t] is the last set cursor colour as [(r, g, b, a)]. *)

val reset_cursor_color : t -> unit
(** [reset_cursor_color t] restores the terminal's default cursor colour. *)

val write_cursor_style : t -> shape:Ansi.cursor_shape -> Ansi.Writer.t -> unit
(** [write_cursor_style t ~shape writer] appends the cursor shape sequence to
    [writer] and arms its restoration by {!reset_state}. *)

val write_cursor_color : t -> (int * int * int) option -> Ansi.Writer.t -> unit
(** [write_cursor_color t color writer] appends the cursor colour sequence for
    [color], with RGB components clamped to [0..255]. [None] appends the default
    colour reset, including its OSC 12 fallback.

    These writer functions support batched output: they append even for non-TTY
    handles and do not call [output] or change the immediate cursor metadata.
    Restoration is armed before appending a custom appearance. A queued reset
    leaves restoration armed, since writing the batch can fail partway through.
    {!reset_cursor_color} and {!reset_state} clear colour restoration after
    successful output. *)

val set_title : t -> string -> unit
(** [set_title t s] sets the terminal window title to [s] (OSC 0). *)

val copy_to_clipboard : t -> string -> unit
(** [copy_to_clipboard t s] sets the system clipboard to [s] (OSC 52) when the
    terminal supports it. Inside tmux or GNU Screen ({!Caps.t.multiplexer}), the
    sequence is wrapped for DCS passthrough to the outer terminal. *)

val notify : t -> title:string -> body:string -> unit
(** [notify t ~title ~body] posts a desktop notification (OSC 9 followed by OSC
    777; a terminal ignores the sequence it does not understand). Inside tmux
    the payload is wrapped for DCS passthrough so the outer terminal receives
    it. *)

val bell : t -> unit
(** [bell t] rings the terminal bell (BEL). *)

val query_color_scheme : t -> unit
(** [query_color_scheme t] asks whether the terminal uses a light or dark colour
    scheme ([CSI ? 996 n]). Supporting terminals reply [CSI ? 997 ; value n];
    the reply arrives in the input as {!Matrix_input.Reply.Color_scheme}. *)

val query_pixel_resolution : t -> unit
(** [query_pixel_resolution t] sends a pixel resolution query (CSI 14 t) through
    [output]. The reply arrives in the input as
    {!Matrix_input.Reply.Text_area_pixels}. *)

(** {1:output Output and state} *)

val send : t -> string -> unit
(** [send t s] writes [s] through the output callback. For non-TTY handles this
    is a no-op. *)

val tty : t -> bool
(** [tty t] is [true] iff escape sequences are emitted. *)

val reset_state : t -> unit
(** [reset_state t] resets all protocol state to defaults.

    Disables mouse tracking, bracketed paste, focus events, Kitty keyboard,
    modifyOtherKeys, color scheme and resize reports, metaSendsEscape, Unicode
    mode, scroll region, and alternate screen. Resets SGR attributes. Cursor
    colour, cursor style, and the window title are reset only when they were
    changed through the handle. *)

(** {1:tty_helpers TTY helpers}

    Stateless functions for terminal I/O. These do not require a {!type-t}
    session handle. *)

type raw_saved = { termios : Unix.terminal_io; iexten : bool }
(** The terminal state captured by {!set_raw}: the record [Unix.tcgetattr]
    models, plus the IEXTEN local flag, which [Unix.terminal_io] cannot express.
    IEXTEN gates the implementation-defined input specials — VDISCARD (^O) and
    VLNEXT (^V) — that the line discipline processes even in non-canonical mode.
*)

val set_raw : Unix.file_descr -> raw_saved
(** [set_raw fd] puts [fd] into raw mode (no echo, no canonical processing, no
    terminal-generated signals, no extended input specials — a raw terminal that
    leaves IEXTEN set never sees ^O, the kernel eats it) and returns the
    original state for later restoration with {!restore}. File status flags such
    as [O_NONBLOCK] are owned by the I/O backend and are left unchanged. *)

val restore : Unix.file_descr -> raw_saved -> unit
(** [restore fd saved] restores [fd] to the given saved state without changing
    its file status flags. *)

val size : Unix.file_descr -> int * int
(** [size fd] is the terminal dimensions as [(cols, rows)]. Falls back to
    [(80, 24)] on failure. *)

val is_tty : Unix.file_descr -> bool
(** [is_tty fd] is [true] iff [fd] is a terminal. *)

val flush_input : Unix.file_descr -> unit
(** [flush_input fd] discards unread input on [fd] via [tcflush]. *)

val enable_vt : Unix.file_descr -> unit
(** [enable_vt fd] enables VT100 processing on Windows consoles. No-op on Unix.
    Failures are silently ignored. *)
