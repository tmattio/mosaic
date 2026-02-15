(** Internal terminal capability detection.

    Caps implements a functional capability detection pipeline used by
    {!Terminal}. It combines environment heuristics with active probing and
    never keeps hidden mutable state: capabilities are stored in plain records
    and updated by folding events.

    {1 Detection pipeline}

    {2 Stage 1 – Environment heuristics}

    {!initial} constructs a baseline {!t} from:

    - [$TERM], [$COLORTERM], [$KITTY_WINDOW_ID]
    - platform quirks (e.g. Windows ConPTY)
    - explicit overrides such as [MATRIX_FORCE_WCWIDTH] and
      [MATRIX_FORCE_UNICODE]

    This stage emits no escape sequences and is safe for non-interactive runs.

    {2 Stage 2 – Active probing}

    {!probe} sends a single compound payload that includes:

    - cursor save/restore
    - DECRQM queries for private modes (focus, sync, Unicode width, SGR pixels,
      colour-scheme updates, bracketed paste)
    - primary device attributes (DA)
    - explicit-width and scaled-text cursor-position queries
    - XTVersion request
    - CSI u (Kitty keyboard) query
    - Kitty graphics probe
    - iTerm2 proprietary cell-size query when applicable

    Responses are parsed through a temporary {!Input.Parser.t} and converted
    into {!Input.Caps.event} values. {!apply_events} folds those into the
    capability record and {!terminal_info}.

    Kitty detection (via XTVersion containing "kitty" or [$KITTY_WINDOW_ID])
    immediately promotes all Kitty capabilities. Detection of the Kitty keyboard
    protocol alone (via CSI u query) only sets [kitty_keyboard = true], since
    other terminals may support the keyboard protocol without full Kitty
    compatibility.

    {1 Capability model}

    The capability record distinguishes between {b detected} capabilities and
    {b policy overrides}:

    - {b Detected capabilities} from probing are monotonic: once a probe
      confirms a feature, it remains enabled.
    - {b Policy overrides} from the environment may disable features that were
      detected or assumed, to ensure correct behavior in specific contexts:
    - Windows ConPTY: disables Kitty/SIXEL features
    - VS Code terminal: disables Kitty keyboard/graphics
    - tmux/screen: forces [unicode_width = `Wcwidth]
    - [$VHS_RECORD]: disables Kitty features for recording

    When [apply_env_overrides] is [true] in {!probe} or {!apply_events}, policy
    overrides are reapplied after each batch of responses. This keeps forced
    policies in effect even if probes report richer capabilities. *)

type unicode_width = [ `Wcwidth | `Unicode ]
(** How Unicode display width is computed.

    - [`Wcwidth] uses legacy [wcwidth()] behaviour (traditional Unix). This
      matches older terminals but misclassifies some emoji and combining
      characters.
    - [`Unicode] uses modern Unicode width tables (currently 15.0), giving
      correct widths for emoji and combining sequences.

    Terminals such as Kitty, WezTerm, recent iTerm2, and Foot report Unicode
    width support via DECRQM 2027 and are then forced to [`Unicode]. *)

type t = {
  term : string;  (** Raw [$TERM] value used for heuristics. *)
  rgb : bool;
      (** [true] if 24-bit colour is considered safe (COLORTERM=truecolor/24bit
          or Kitty/ConPTY heuristics). *)
  kitty_keyboard : bool;
      (** [true] if the Kitty keyboard protocol is supported. *)
  kitty_graphics : bool;
      (** [true] if the Kitty graphics protocol is supported. *)
  bracketed_paste : bool;
      (** [true] if bracketed paste (DECSET/DECRST 2004) is supported or
          enabled. *)
  focus_tracking : bool;
      (** [true] if focus tracking (DECSET/DECRST 1004) is supported or enabled.
      *)
  unicode_width : unicode_width;
      (** Current Unicode width mode. This reflects both heuristics and any
          DECRQM 2027 responses processed so far. *)
  sgr_pixels : bool;
      (** [true] if SGR pixel-position mouse reports (DECRQM 1016) are
          supported. *)
  color_scheme_updates : bool;
      (** [true] if the terminal supports colour-scheme update notifications
          (Terminal Update protocol, DECRQM 2031). *)
  explicit_width : bool;
      (** [true] if the terminal has reported explicit cell-width information
          via proprietary cursor-position queries (iTerm2/Foot). *)
  explicit_cursor_positioning : bool;
      (** [true] if the terminal may miscalculate grapheme widths but supports
          reliable cursor positioning. When set, the renderer repositions the
          cursor after each wide grapheme to prevent column drift. Terminals
          like tmux, alacritty, and screen benefit from this. Only used when
          {!explicit_width} is [false]. *)
  scaled_text : bool;
      (** [true] if the terminal reports support for scaled text sizing (Foot,
          WezTerm and similar, via proprietary queries). *)
  sixel : bool;
      (** [true] if SIXEL graphics are supported (DA parameter 4 present). *)
  sync : bool;  (** [true] if synchronised output (DECRQM 2026) is supported. *)
  hyperlinks : bool;
      (** [true] if OSC 8 hyperlinks are supported (detected from Kitty/iTerm2
          cues). *)
}
(** Current terminal capability set.

    Boolean fields from probing are monotonic: once a probe confirms a feature,
    subsequent probes will not disable it. However, policy overrides (see
    {!apply_events} with [apply_env_overrides:true]) may disable features to
    ensure correct behavior in specific environments.

    The record is intentionally verbose rather than bit-packed to keep debugging
    and conditional branches readable. *)

type terminal_info = { name : string; version : string; from_xtversion : bool }
(** Best-known terminal emulator identity.

    - [name]: terminal program name, taken from XTVersion payload when
      available, otherwise from [$TERM_PROGRAM].
    - [version]: version string, taken from XTVersion or
      [$TERM_PROGRAM_VERSION].
    - [from_xtversion]: [true] if [name] and [version] came from an XTVersion
      response, [false] if they were inferred from the environment. *)

val initial : ?provided:t -> term:string -> unit -> t * terminal_info
(** [initial ?provided ~term ()] builds the initial capability record and
    terminal metadata.

    - When [provided] is [Some caps], returns [caps] unchanged and derives
      {!terminal_info} from the environment (TERM_PROGRAM/TERM_PROGRAM_VERSION)
      using [caps.term] as the baseline.
    - When [provided] is [None], constructs a baseline from [term] and
      environment heuristics, then applies environment overrides such as
      [MATRIX_FORCE_WCWIDTH], [MATRIX_FORCE_UNICODE], [TERM_PROGRAM],
      tmux/screen fallbacks, and platform-specific quirks.

    In both cases, [terminal_info.from_xtversion] is [false]; XTVersion replies
    processed later via {!apply_events} will take precedence. *)

val apply_event :
  ?apply_env_overrides:bool ->
  caps:t ->
  info:terminal_info ->
  Input.Caps.event ->
  t * terminal_info
(** [apply_event ~caps ~info event] processes a single capability event.

    This is the callback-friendly version of {!apply_events} that avoids list
    allocation. Useful when processing events inline during
    {!Input.Parser.feed}. *)

val apply_events :
  ?apply_env_overrides:bool ->
  caps:t ->
  info:terminal_info ->
  Input.Caps.event list ->
  t * terminal_info
(** [apply_events ~caps ~info events] folds capability events into the records.
    When [apply_env_overrides] is [true], environment heuristics are reapplied
    after folding.

    Each {!Input.Caps.event} may update fields in {!t} and/or {!terminal_info}.
    Events are processed in order; later events win when they provide strictly
    more information (e.g. a Unicode-width DECRQM replacing an environment
    guess).

    This function is used both by {!probe} and by higher-level input loops that
    parse capability replies interleaved with user events. *)

val probe :
  ?timeout:float ->
  ?apply_env_overrides:bool ->
  on_event:(Input.t -> unit) ->
  read_into:(bytes -> int -> int -> int) ->
  wait_readable:(timeout:float -> bool) ->
  send:(string -> unit) ->
  caps:t ->
  info:terminal_info ->
  unit ->
  t * terminal_info
(** [probe ?timeout ~on_event ~read_into ~wait_readable ~send ~caps ~info ()]
    runs the active probing stage. When [?apply_env_overrides] is [true],
    environment overrides are reapplied after each batch of responses to keep
    forced policies (e.g. tmux wcwidth/tmux recording fallbacks) in effect even
    if probes report richer capabilities. Defaults to [false] to preserve caller
    control when custom capability sets are injected.

    - Emits a compound query payload built from [caps.term] using [send].
    - Repeatedly waits for input using [wait_readable], reading into a temporary
      buffer via [read_into].
    - Feeds bytes into an internal {!Input.Parser.t}, collects capability
      events, and applies them with {!apply_events}. User input events (keys,
      mouse, etc.) are forwarded to [on_event] so they are not lost during
      probing.
    - Stops when all three completion conditions are met: (1) explicit-width
      information is available (either [caps.explicit_width = true] or a new
      probe response), (2) scaled-text information is available (either
      [caps.scaled_text = true] or a new probe response), and (3) at least one
      device-attributes (DA) response has been seen. Probing also terminates
      after [timeout] seconds (default 0.2s; non-negative).

    The cursor is saved and restored as part of the payload. Probing is
    idempotent and monotonic: running [probe] multiple times can only add
    capabilities or refine {!terminal_info}, never remove information.

    Callers are responsible for ensuring that [read_into], [wait_readable], and
    [send] interact with the same underlying descriptor and do not interfere
    with normal input processing. *)

val build_probe_payload : string -> string
(** [build_probe_payload term] constructs the compound probe query string for
    the given [$TERM] value. The payload includes cursor save/restore, DECRQM
    queries, DA, cursor-position probes, XTVersion, CSI u, and Kitty graphics
    queries. iTerm2 proprietary queries are appended when [term] contains
    "iterm". *)
