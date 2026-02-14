(** Terminal rendering state tracking.

    This module manages the active rendering state (colors, attributes, and
    hyperlinks) of the terminal output stream. It tracks what the terminal
    "thinks" the current style is and only emits codes when the requested style
    differs from that state.

    {1 Overview}

    Writing escape codes for every character cell is inefficient. This module
    memoizes the last emitted style. When the style changes, it emits a full
    reset (SGR 0) followed by the complete new style. This avoids per-cell
    emission while ensuring correctness.

    {1 Performance}

    {!update} is {b zero-allocation}: it writes directly to the [Writer.t]
    using low-level primitives. No closures, lists, or intermediate strings are
    created. This makes it suitable for hot render loops processing thousands of
    cells per frame.

    Typical allocation per frame: only the initial {!create} call (~16 words).

    {1 Usage}

    {[
      let buf = Bytes.create 65536 in
      let writer = Ansi.make buf in
      let state = Sgr_state.create () in

      (* In your render loop - zero allocations per cell: *)
      for row = 0 to height - 1 do
        for col = 0 to width - 1 do
          Sgr_state.update state writer ~fg_r:1.0 ~fg_g:0.0 ~fg_b:0.0
            ~fg_a:1.0 (* Red *)
            ~bg_r:0.0 ~bg_g:0.0 ~bg_b:0.0 ~bg_a:0.0 (* Transparent *)
            ~attrs:(Attr.pack Attr.bold) ~link:"";
          Ansi.emit (Ansi.char 'X') writer
        done;
        (* Reset at row end to prevent style bleed *)
        Sgr_state.reset state
      done
    ]} *)

type t
(** The mutable state of the terminal's rendering settings (colors, attributes,
    hyperlinks). *)

(** {1 Lifecycle} *)

val create : unit -> t
(** [create ()] creates a new state tracker.

    The initial state is "unknown", guaranteeing that the first call to
    {!update} will emit a full reset and style application to ensure
    consistency. *)

val reset : t -> unit
(** [reset t] invalidates the state.

    Forces the next {!update} to emit a reset sequence and full style codes. Use
    this when the output stream might have been modified externally (e.g., by a
    subprocess) or when performing non-contiguous cursor jumps where style bleed
    must be prevented. *)

(** {1 Operations} *)

val update :
  t ->
  Writer.t ->
  fg_r:float ->
  fg_g:float ->
  fg_b:float ->
  fg_a:float ->
  bg_r:float ->
  bg_g:float ->
  bg_b:float ->
  bg_a:float ->
  attrs:int ->
  link:string ->
  unit
(** [update t w ...] synchronizes the terminal with the requested style values.

    If the requested colors, attributes, or hyperlink differ from [t]'s current
    state, this function emits the necessary escape codes to [w] and updates
    [t].

    {b Behavior}:
    - Hyperlink changes are emitted first (OSC 8 sequences).
    - If any SGR component changes, it emits a Reset ([0]) code then reapplies
      the full style (Foreground, Background, Attributes).

    {b Preconditions}:
    - Color components (r, g, b, a) must be normalized floats in [0.0, 1.0].
    - [attrs] must be a valid integer bitmask compatible with {!Attr.pack}.
    - [link] is the hyperlink URL, or [""] for no link. Using empty string as
      the sentinel avoids Option allocation in hot render loops. *)

val close_link : t -> Writer.t -> unit
(** [close_link t w] closes any open hyperlink.

    If a hyperlink is currently open, emits the OSC 8 close sequence and updates
    [t]. Safe to call even if no link is open. *)
