(** Screen.mli

    Double‑buffer screen abstraction built on {!Grid}. A [Screen.t] encapsulates
    *two* grids:

    - The **front buffer** (what is currently shown)
    - The **back buffer** (what the caller is drawing for the next frame)

    Callers draw exclusively to the back buffer through the convenience
    functions below or by grabbing the buffer itself. They commit the frame with
    {!present}, which swaps the buffers, diffs them, and returns the minimal set
    of regions that must be redrawn. *)

type t
(** A double‑buffered terminal screen. *)

(** {1 Construction & sizing} *)

val create :
  rows:int ->
  cols:int ->
  ?east_asian_context:bool ->
  ?style:Ansi.Style.t ->
  unit ->
  t
(** [create ~rows ~cols ?east_asian_context ?style ()] creates a new screen. If
    [style] is provided, both front and back buffers are initialized with empty
    cells having that style (useful for setting a default background color for
    alpha blending). *)

val rows : t -> int
val cols : t -> int

val resize : t -> rows:int -> cols:int -> unit
(** Resizing preserves as much front‑buffer content as possible, then sizes both
    buffers identically. Back‑buffer damage is marked so the next {!present}
    will redraw the whole visible area if needed. *)

(** {1 Viewport} *)

module Viewport : sig
  type t
  (** A rectangular clipping region, inclusive of all its rows and columns. *)

  val make : row:int -> col:int -> width:int -> height:int -> t
  val intersect : t -> t -> t option
  val contains : t -> row:int -> col:int -> bool

  val full : rows:int -> cols:int -> t
  (** Viewport that covers the whole screen size given. *)

  (** {2 Pretty-printing and Equality} *)

  val pp : Format.formatter -> t -> unit
  (** [pp fmt viewport] pretty-prints a viewport for debugging. *)

  val equal : t -> t -> bool
  (** [equal v1 v2] returns true if the two viewports are equal. *)
end

val with_viewport : t -> Viewport.t -> (t -> unit) -> unit
(** Temporarily restrict all drawing operations to viewport bounds. Note: The
    caller must pass the viewport parameter to drawing functions explicitly.
    Example:
    [with_viewport screen clip (fun s -> set_text ~viewport s ~row:0 ~col:0
     ...)] *)

val render_viewport : t -> Viewport.t -> string
(** Render a specific rectangular region as ANSI string *)

val copy_viewport :
  src:t ->
  dst:t ->
  src_viewport:Viewport.t ->
  dst_row:int ->
  dst_col:int ->
  unit
(** Copy a clipped region from src to dst at specified position *)

(** {1 Drawing – all operate on the back buffer} *)

val clear : ?viewport:Viewport.t -> t -> unit

val clear_rect :
  ?viewport:Viewport.t ->
  t ->
  row_start:int ->
  row_end:int ->
  col_start:int ->
  col_end:int ->
  unit

val clear_line : ?viewport:Viewport.t -> t -> row:int -> col:int -> unit

val set_grapheme :
  ?viewport:Viewport.t ->
  t ->
  row:int ->
  col:int ->
  glyph:string ->
  attrs:Ansi.Style.t ->
  unit

val set_text :
  ?viewport:Viewport.t ->
  t ->
  row:int ->
  col:int ->
  text:string ->
  attrs:Ansi.Style.t ->
  int * int
(** Writes a single‑line UTF‑8 string, returns (lines_written,
    columns_advanced). For single-line text, lines_written will be 1 if any text
    was written, 0 otherwise. *)

val set_multiline_text :
  ?viewport:Viewport.t ->
  t ->
  row:int ->
  col:int ->
  text:string ->
  attrs:Ansi.Style.t ->
  int * int
(** Writes text containing newlines; returns (lines_written,
    max_columns_advanced). max_columns_advanced is the maximum width reached
    across all lines. *)

(** {1 Low‑level access} *)

val back : t -> Grid.t
(** Direct back‑buffer access. *)

val front : t -> Grid.t
(** Direct front‑buffer access. *)

(** {1 Frame lifecycle} *)

val begin_frame : t -> unit

val present : t -> Grid.dirty_region list
(** Swap buffers, compute **rectangular** dirty regions, and return them. *)

val batch : t -> (t -> 'a) -> 'a
(** [batch screen f] automatically calls begin_frame, executes f, and returns
    its result. This is a convenience function for typical draw-present cycles.
    Example:
    [batch screen (fun s -> set_text s ~row:0 ~col:0 ~text:"Hello" ~attrs)] *)

val diff_cells : t -> (int * int * Grid.Cell.t) list
(** Compute the list of changed cells that would be produced by [present],
    **without** swapping buffers or mutating state. Returns individual cells. *)

val flush_damage : t -> Grid.rect list
(** Retrieve & clear raw damage rectangles accumulated so far. *)

(** {1 Copy/Clone operations} *)

val clone : t -> t
(** Creates a deep copy of the screen with independent buffers *)

val snapshot : t -> Grid.t
(** Returns an immutable copy of the back buffer for background processing *)

val copy_to : src:t -> dst:t -> unit
(** Copies the back buffer of src to the back buffer of dst *)

(** {1 Rendering} *)

(** Rendering patch for incremental updates *)
type patch =
  | Run of {
      row : int;
      col : int;
      text : string;  (** Concatenated graphemes *)
      style : Ansi.Style.t;
      width : int;  (** Total column width *)
    }  (** Run of cells with same style. Efficient for rendering *)
  | Clear_region of { row : int; col : int; width : int; height : int }
      (** Clear a rectangular region *)
  | Clear_line of { row : int; from_col : int }
      (** Clear from column to end of line *)
  | Clear_screen  (** Clear the entire screen *)

(** {2 Pretty-printing and Equality for patches} *)

val pp_patch : Format.formatter -> patch -> unit
(** [pp_patch fmt patch] pretty-prints a patch for debugging. *)

val patch_equal : patch -> patch -> bool
(** [patch_equal p1 p2] returns true if the two patches are equal. *)

val render : t -> patch list
(** Smart render: compares back buffer with front buffer and returns patches.
    May return [Clear_screen; ...] for full redraw or incremental patches. *)

val render_to_string : t -> string
(** Simple full render of back buffer to ANSI string *)

val patch_to_sgr : patch -> string
(** Convert a single patch to ANSI escape sequences *)

val patches_to_sgr : patch list -> string
(** Convert patches to ANSI escape sequences *)

(** {1 Cursor management} *)

val get_cursor : t -> (int * int) option
(** Get current cursor position from back buffer *)

val set_cursor : t -> row:int -> col:int -> unit
(** Set cursor position in back buffer *)

(** {1 Hash helpers} *)

val hash_front : t -> int
val hash_back : t -> int

(** {1 Performance monitoring} *)

module Perf : sig
  type counter = {
    mutable bytes_allocated : int;
    mutable cells_diffed : int;
    mutable frames_rendered : int;
    mutable patches_generated : int;
    mutable cache_hits : int;
    mutable cache_misses : int;
  }

  val reset : unit -> unit
  (** Reset all performance counters to zero *)

  val get : unit -> counter
  (** Get a snapshot of current performance counters *)
end
