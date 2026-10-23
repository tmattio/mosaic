(** Spatial indexing for mouse hit testing.

    A hit grid maps screen coordinates to integer element IDs. Lookup is [O(1)];
    registration is [O(region area)].

    {1:constants Constants} *)

val empty_id : int
(** [empty_id] is [0]. Represents the absence of any element. *)

type id = int
(** The type for hit-test element identifiers. [0] is reserved for {!empty_id};
    real element identifiers should be positive. *)

(** {1:types Types} *)

(* Rectangle types: this module used to define its own rect and now shares
   Matrix_grid.region. Matrix_charts.Layout.rect and Mosaic_ui.Selection.bounds are
   still structurally identical {x; y; width; height} records in cell
   coordinates; future unification should migrate those to Matrix_grid.region as
   well rather than introduce another copy. *)

type t
(** The type for hit grids. Lookups are [O(1)]; adding a region is
    [O(width * height)] in the clipped region. *)

(** {1:lifecycle Lifecycle} *)

val create : width:int -> height:int -> t
(** [create ~width ~height] is a hit grid of the given dimensions with all cells
    set to {!empty_id}. *)

val resize : t -> width:int -> height:int -> unit
(** [resize t ~width ~height] updates [t]'s dimensions to [width] and [height].
    All cells are reset to {!empty_id}. Internal storage is grown only when
    necessary. *)

val clear : t -> unit
(** [clear t] resets all cells to {!empty_id}. The clip stack is preserved. *)

(** {1:ops Operations} *)

val add : t -> x:int -> y:int -> width:int -> height:int -> id:id -> unit
(** [add t ~x ~y ~width ~height ~id] fills the rectangular region with [id]
    (painter's algorithm: overwrites any existing IDs). The rectangle is clipped
    to the grid bounds and the active clip region. Zero or negative dimensions
    are a no-op. *)

val get : t -> x:int -> y:int -> id
(** [get t ~x ~y] is the element ID at [(x, y)], or {!empty_id} if the
    coordinates are out of bounds. *)

val draw : src:t -> dst:t -> x:int -> y:int -> unit
(** [draw ~src ~dst ~x ~y] paints the regions of [src] onto [dst], with [src]'s
    top-left cell at [(x, y)] in [dst]. A cell of [src] that holds an element
    overwrites the cell of [dst] it lands on, as {!add} does; a cell of [src]
    that holds {!empty_id} leaves [dst] unchanged. The cells that land outside
    [dst] or its active clip are skipped. Identifiers are copied unchanged.

    This embeds a surface's hits in a larger one: draw the surface's elements
    into a hit grid of its own size, then draw that grid at the surface's
    position. The containers already under it keep the cells it left empty, as
    they keep the cells it left undrawn on screen. For a copy that also replaces
    [dst]'s cells with [src]'s empty ones, {!add} the region with {!empty_id}
    first; {!resize} [dst] to [src]'s dimensions, which clears it, then draw at
    [(0, 0)] for a copy of the whole grid.

    [src] may be [dst]: the result is that of drawing a copy of [src]. *)

(** {1:clipping Clipping}

    Hierarchical clipping for hit regions. When a clip is active, {!add}
    operations are constrained to the intersection of the clip rectangle and the
    grid bounds. This prevents elements inside overflow-hidden containers from
    receiving mouse events outside their visible area.

    Push/pop pairs must be balanced. *)

val push_clip : t -> Matrix_grid.region -> unit
(** [push_clip t r] pushes a clipping rectangle. The effective clip is the
    intersection of [r] with the current clip (hierarchical narrowing). *)

val pop_clip : t -> unit
(** [pop_clip t] pops the most recent clip. No-op if the stack is empty. *)

val clear_clip : t -> unit
(** [clear_clip t] removes all clipping regions. *)

val with_clip : t -> Matrix_grid.region -> (unit -> 'a) -> 'a
(** [with_clip t r f] runs [f ()] with [r] as the active clip and pops it on
    return (even on exception). *)
