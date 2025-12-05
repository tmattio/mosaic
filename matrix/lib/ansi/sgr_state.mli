(** Terminal SGR (Select Graphic Rendition) state tracking.

    This module manages the active rendering state (colors and attributes) of
    the terminal output stream. It uses delta encoding (diffing) to minimize the
    size of ANSI escape sequences emitted during rendering.

    {1 Overview}

    Writing SGR codes (like [\027\[1;31m]) for every character cell is
    inefficient. This module tracks what the terminal "thinks" the current style
    is, and only emits codes when the requested style differs from that state.

    {1 Usage}

    {[
      let state = Style_state.create () in

      (* In your render loop: *)
      Style_state.update state writer ~fg_r:1.0 ~fg_g:0.0 ~fg_b:0.0
        ~fg_a:1.0 (* Red *)
        ~bg_r:0.0 ~bg_g:0.0 ~bg_b:0.0 ~bg_a:0.0 (* Black *)
        ~attrs:Attr.empty
    ]} *)

type t
(** The mutable state of the terminal's SGR settings. *)

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
  Escape.writer ->
  fg_r:float ->
  fg_g:float ->
  fg_b:float ->
  fg_a:float ->
  bg_r:float ->
  bg_g:float ->
  bg_b:float ->
  bg_a:float ->
  attrs:int ->
  unit
(** [update t w ...] synchronizes the terminal with the requested style values.

    If the requested colors or attributes differ from [t]'s current state, this
    function emits the minimal necessary SGR escape codes to [w] and updates
    [t].

    {b Behavior}:
    - If any component changes, it first emits a Reset ([0]) code to clear
      conflicts.
    - It then emits codes for the new Foreground (Truecolor), Background
      (Truecolor), and Attributes (Bitmask).

    {b Preconditions}:
    - Color components (r, g, b, a) must be normalized floats in [0.0, 1.0].
    - [attrs] must be a valid integer bitmask compatible with {!Attr.pack}. *)
