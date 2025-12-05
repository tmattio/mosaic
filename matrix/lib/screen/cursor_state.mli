(** Terminal hardware cursor state tracking.

    Manages the state of the terminal's hardware cursor (position, style,
    visibility, and color) using delta encoding. *)

type t
(** The mutable cursor state.

    Tracks desired cursor position, style (block/line/underline), blinking
    behavior, color, and visibility. Also maintains the last emitted values for
    delta encoding, ensuring only changed attributes generate escape sequences
    during rendering. *)

val create : unit -> t
(** [create ()] creates a new cursor state tracker. *)

val reset : t -> unit
(** [reset t] marks the cursor state as unknown, forcing full re-emission on the
    next {!emit} call. *)

(** {1 State Management} *)

val set_position : t -> row:int -> col:int -> unit
(** [set_position t ~row ~col] sets the cursor position.

    Coordinates are clamped to minimum 1. The position becomes active during the
    next {!emit} call. *)

val clear_position : t -> unit
(** [clear_position t] clears the cursor position so the cursor remains at its
    current location during emission.

    Use this when you want the terminal to leave the cursor wherever it ended up
    after the last rendering operation, rather than moving it to a specific
    position. *)

val set_style :
  t -> style:[ `Block | `Line | `Underline ] -> blinking:bool -> unit
(** [set_style t ~style ~blinking] configures the cursor's visual style and
    whether it blinks.

    @param style
      Cursor shape: [`Block] for a filled block, [`Line] for a vertical bar, or
      [`Underline] for a horizontal line.
    @param blinking Whether the cursor should blink. *)

val set_color : t -> (int * int * int) option -> unit
(** [set_color t color] sets the cursor color.

    Pass [None] to use the terminal's default cursor color, or [Some (r, g, b)]
    for RGB values in the range 0-255. The color change is emitted during the
    next {!emit} call. *)

val set_visible : t -> bool -> unit
(** [set_visible t visible] sets the logical cursor visibility. *)

val is_visible : t -> bool
(** [is_visible t] returns the current logical cursor visibility. *)

val clamp_to_bounds : t -> max_row:int -> max_col:int -> unit
(** [clamp_to_bounds t ~max_row ~max_col] restricts the stored cursor position
    to the provided limits if a position is set. *)

(** {1 Emission} *)

val hide_temporarily : t -> Ansi.Escape.writer -> unit
(** [hide_temporarily t w] emits a hide sequence if the cursor is currently
    physically visible, and updates the internal tracking state to reflect this.

    Used by render loops to hide the cursor during drawing without altering the
    logical visibility state. *)

val emit : t -> row_offset:int -> Ansi.Escape.writer -> unit
(** [emit t ~row_offset w] synchronizes the terminal cursor with the desired
    state.

    Emits only the escape sequences needed to update position, style, color, and
    visibility based on what has changed since the last emission. This delta
    encoding optimization minimizes output size by tracking the last emitted
    values and comparing them to the current desired state.

    @param row_offset Offset added to the row coordinate during positioning.
    @param w Output writer for escape sequences. *)
