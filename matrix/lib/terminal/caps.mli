(** Terminal capabilities, derived from evidence.

    What a terminal can do is learned from three sources: its answers to
    queries, its identity (the name and version it reports, or that the
    environment names), and guesses from the environment. They disagree, and an
    answer can arrive late, so the capabilities are not accumulated: they are a
    function, {!derive}, of the evidence gathered so far, and a later answer can
    take back what an earlier guess gave.

    {1:precedence Precedence}

    Each fact is taken from the first source that has it:
    + {b user policy}, which the person running the program decides:
      [MATRIX_FORCE_UNICODE] and [MATRIX_FORCE_WCWIDTH] set the width method,
      and [VHS_RECORD] (a recording) asks for the plain widths and no kitty
      protocols;
    + {b corrections} keyed on the terminal's identity, for a terminal that
      answers but is known to do otherwise, each with its reason: Ghostty from
      1.3 widens graphemes past what mode 2027 describes;
    + {b the answer} of the terminal that does the work;
    + {b identity}: what a terminal of that name and version is known to do;
    + {b guesses} from the environment: [TERM], [COLORTERM], the platform, and
      the description the terminfo database has for [TERM] ({!env}), which only
      ever adds a fact: direct colour ([RGB], [Tc], or [setrgbf] with
      [setrgbb]), bracketed paste ([BE]), focus reports ([fe]) and synchronized
      output ([Sync] as mode 2026). A description leaves out direct colour and
      synchronized output for most terminals that have them, and a terminal that
      is not the one [TERM] names (ssh, a multiplexer) may have no description
      here at all, so its silence says nothing;
    + the default, which a caller may supply in place of the environment
      ({!assume}).

    A question is asked of the terminal that does the work. Inside tmux, tmux
    draws the screen and reads the keyboard, so questions about drawing and keys
    go to tmux itself, which answers or stays silent. tmux answers no mode query
    before 3.6, and none for mode 2026 before 3.7, so its facts then come from
    its identity, by version. It brackets the pastes it passes to a pane from
    1.7, and passes focus reports to a pane from 1.8 while its [focus-events]
    option is on. It synchronizes a pane's output from 3.7, and it has no mode
    2027. Only a question about the terminal outside, such as kitty graphics,
    which tmux passes through untouched, is wrapped for tmux's passthrough, and
    only once passthrough is known to be on: tmux drops wrapped queries unless
    its [allow-passthrough] option is set, which it is not by default since 3.3.

    The environment describes the terminal the process started in, which is not
    always the one it talks to: [TERM_PROGRAM] survives into ssh sessions, GNU
    screen, zellij and tmux. The environment gives an identity only when the
    terminal is directly attached, with none of [SSH_CONNECTION], [SSH_CLIENT],
    [SSH_TTY], [TMUX], [STY] or [ZELLIJ] set. An identity the terminal reports
    itself (XTVERSION) is an answer and holds everywhere.

    {1:batches Batches}

    Queries go in batches, each ending with a primary device attributes query
    (DA1), which every terminal answers. A terminal answers in order, so the DA1
    reply says that the batch has been answered: "If an answer for the device
    attributes is received without getting back an answer for the progressive
    enhancement the terminal does not support this protocol" (kitty keyboard
    protocol, "Detection of support"). A query that is silent before its batch's
    DA1 reply is answered "no" where its protocol says so (kitty graphics) and
    stays unknown otherwise: a terminal without DECRQM says nothing of its
    modes. The caller sends the batches, settles them and routes their replies;
    {!ask} gives each batch's bytes, {!next} the batch that follows. *)

(** {1:caps Capabilities} *)

type t = {
  rgb : bool;  (** 24-bit color is safe. *)
  kitty_keyboard : int option;
      (** [Some flags] when the terminal speaks the kitty keyboard protocol,
          with the enhancement flags it last reported in effect; [None] when it
          has not answered the protocol's query. *)
  kitty_graphics : bool;  (** The kitty graphics protocol is supported. *)
  bracketed_paste : bool;  (** Bracketed paste (mode 2004) is supported. *)
  focus_tracking : bool;  (** Focus reports (mode 1004) are supported. *)
  unicode_width : Matrix_text.width_method;
      (** How the terminal measures text, which its answer for mode 2027
          (grapheme cluster mode, "Unicode in Terminals") tells, as a DECRPM
          setting (VT510). Set or permanently set is [`Unicode]. Reset says that
          the terminal has the mode and can set it, which the runtime does
          ({!grapheme_mode}): [`Unicode]. Permanently reset is [`Wcwidth]: with
          the mode off a terminal behaves "as if this specification was not
          implemented at all". The answer "not recognized", or none, says that
          the terminal does not have the mode, and such a terminal measures each
          character by itself, as wcwidth does: [`Wcwidth], unless its identity
          says otherwise. xterm and VTE answer "not recognized" and st does not
          answer, and they measure by character; kitty answers "not recognized"
          and places grapheme clusters, so its identity gives [`Unicode]. *)
  grapheme_mode : bool option;
      (** Mode 2027 as the terminal answered. [Some false] says it has the mode
          and it is reset, and can be set: the runtime then sets it while it
          measures by grapheme clusters, and resets it when it gives the
          terminal back. [Some true] says it is set, and the runtime leaves it
          so. [None] when the terminal does not have the mode, cannot change it,
          or did not answer. *)
  sgr_pixels : bool;  (** SGR pixel mouse reports (mode 1016) are supported. *)
  color_scheme_updates : bool;
      (** Color scheme change reports (mode 2031) are supported. *)
  explicit_width : bool;
      (** The terminal sizes text as OSC 66 says: [w=1] moves the cursor one
          cell. *)
  explicit_cursor_positioning : bool;
      (** The terminal may measure graphemes otherwise than the renderer, so the
          renderer places the cursor after each wide one. *)
  scaled_text : bool;  (** OSC 66 scaled text ([s=2]) is supported. *)
  sixel : bool;  (** SIXEL graphics: DA1 attribute 4. *)
  sync : bool;  (** Synchronized output (mode 2026) is supported. *)
  hyperlinks : bool;  (** OSC 8 hyperlinks are supported. *)
  meta_sends_escape : bool option;
      (** xterm's metaSendsEscape (mode 1036) as the terminal answered.
          [Some false] says it has the mode and it is reset, and can be set: Alt
          then arrives as the character with its high bit set, not as an ESC
          before it. [None] when the terminal does not have the mode, cannot
          change it, or did not answer. *)
  multiplexer : [ `Tmux | `Screen ] option;
      (** The multiplexer between the program and the terminal: tmux when it
          reports itself (XTVERSION) or [TMUX] is set, GNU screen when [STY] is
          set, else as [TERM]'s prefix says. *)
  identity : (string * string) option;
      (** The terminal's name and version, from XTVERSION, or from
          [TERM_PROGRAM] and [TERM_PROGRAM_VERSION] when directly attached. *)
}
(** The type for capabilities. *)

(** {1:env Environment} *)

type env
(** The type for the environment variables that bear on capabilities. *)

val env :
  ?terminfo:Matrix_terminfo.t -> (string -> string option) -> win32:bool -> env
(** [env ~terminfo getenv ~win32] reads the environment through [getenv], such
    as [Sys.getenv_opt]; an empty value counts as unset. [terminfo] is the
    description of [TERM], if there is one ([Matrix_tty.terminfo] finds it).
    [win32] says the terminal is a Windows console. *)

(** {1:evidence Evidence} *)

type evidence
(** The type for what is known of a terminal: its environment and the answers
    received so far. *)

val evidence : env -> evidence
(** [evidence env] is the evidence before any answer. *)

val assume : t -> evidence
(** [assume caps] is evidence with no environment, in which every fact that no
    answer gives is as [caps] says. *)

val add : evidence -> Matrix_input.Reply.t -> evidence
(** [add ev r] is [ev] with the reply [r]: a mode report (DECRPM), the kitty
    keyboard flags, XTVERSION or a kitty graphics reply. Other replies, and the
    cursor reports and DA1 replies whose meaning depends on their batch
    ({!cursor}, {!answered}), leave [ev] as it is. *)

val derive : evidence -> t
(** [derive ev] is the capabilities [ev] gives, by the {!section-precedence}. *)

(** {1:questions Questions} *)

(** The type for batches of queries. *)
type batch =
  | Probe
      (** The startup batch, to the terminal that does the work. It starts with
          S7C1T, so that no reply comes with 8-bit controls, then asks for the
          mode reports, two OSC 66 widths, each written at the start of the
          second row and followed by a cursor position report, XTVERSION, the
          kitty keyboard flags, the kitty graphics query unless a multiplexer is
          in the way, and DA1. *)
  | Passthrough
      (** DA1 alone, wrapped for tmux: answered only when tmux passes it on. *)
  | Outer
      (** Questions about the terminal outside tmux, wrapped for tmux: the kitty
          graphics query, then DA1. *)

val ask : evidence -> batch -> string * Matrix_input.Decoder.owed
(** [ask ev b] is the bytes of [b] and the replies it owes the decoder (see
    {!Matrix_input.Decoder.expect}). *)

val cursor : evidence -> batch -> index:int -> row:int -> col:int -> evidence
(** [cursor ev b ~index ~row ~col] is [ev] with the [index]th cursor position
    report that [b] asked for, from [0], at [row] and [col], from [0]. *)

val answered : evidence -> batch -> attributes:int list -> evidence
(** [answered ev b ~attributes] is [ev] once the DA1 that ends [b] is answered
    with [attributes]. What [b] asked and was not answered is then known not to
    be there, where its protocol says that silence means so. *)

val next : evidence -> batch -> batch option
(** [next ev b] is the batch to send once [b] is answered, if any: inside tmux,
    {!Passthrough} after {!Probe}, then {!Outer} once passthrough is known to be
    on. *)
