(** Hit grids, which say which element of a frame lies under each cell.

    A {e hit grid} is a rectangle of cells, [width] wide and [height] high,
    addressed as on screen, so [(x, y)] is column [x] and row [y], and [(0, 0)]
    is the top-left cell. Each cell holds the identifier of one {e element},
    such as a button, or {!empty_id}, which marks the cell {e empty}. A program
    draws its elements into a hit grid as it draws them on screen, back to
    front, and a mouse report's coordinates then give the element under the
    pointer with {!get}. Reading a cell takes constant time, and adding a region
    takes time proportional to the cells it covers.

    Regions are added in {e painter's order}, in the sense that a region
    overwrites what its cells held, so the element added last is the one a cell
    reports. A {e clip} limits where regions land. The grid keeps a stack of
    clips, and the {e active clip} is the intersection of the regions pushed on
    it, so a container that hides its overflow keeps the elements inside it from
    receiving the pointer outside its visible area.

    A grid's life is a loop over frames. {!create} makes it. For each frame,
    {!clear} (or {!resize}, when the screen changed size) empties it; the
    program adds each element's region with {!add}, pushes a clip for each
    container that clips its content and pops it after the content; and {!get}
    answers the mouse reports that arrive until the next frame.

    Unless a value's doc says otherwise, a region that lies partly or wholly
    outside the grid is clipped to it, and no value raises. A grid is mutable
    and must not be used by two domains concurrently.

    This frame gives the whole screen to element 1, then a 20-cell button,
    element 2, on row 5 inside a clip:
    {[
    let module H = Matrix_screen.Hit_grid in
    let hits = H.create ~width:80 ~height:24 in
    H.add hits ~x:0 ~y:0 ~width:80 ~height:24 ~id:1;
    H.with_clip hits { Matrix_grid.x = 10; y = 5; width = 20; height = 3 }
      (fun () -> H.add hits ~x:0 ~y:5 ~width:80 ~height:1 ~id:2);
    assert (H.get hits ~x:12 ~y:5 = 2);
    assert (H.get hits ~x:40 ~y:5 = 1)
    ]} *)

(** {1:ids Identifiers} *)

val empty_id : int
(** [empty_id] is [0], the identifier an empty cell holds. *)

type id = int
(** The type for element identifiers. An element's identifier must be positive,
    since {!empty_id} marks a cell empty. *)

(** {1:grids Grids} *)

type t
(** The type for hit grids. *)

val create : width:int -> height:int -> t
(** [create ~width ~height] is a grid [width] cells wide and [height] high, with
    every cell {!empty_id} and no clip. A negative dimension is taken as [0]. *)

val resize : t -> width:int -> height:int -> unit
(** [resize t ~width ~height] makes [t] [width] cells wide and [height] high, as
    {!create} takes them, and sets every cell to {!empty_id}. The clip stack
    stays as it is. *)

val clear : t -> unit
(** [clear t] sets every cell of [t] to {!empty_id}. The clip stack stays as it
    is. *)

val add : t -> x:int -> y:int -> width:int -> height:int -> id:id -> unit
(** [add t ~x ~y ~width ~height ~id] sets to [id] each cell of the region
    [width] wide and [height] high whose top-left cell is [(x, y)], inside [t]
    and the active clip. A region with a dimension of [0] or less leaves [t] as
    it is. *)

val get : t -> x:int -> y:int -> id
(** [get t ~x ~y] is the identifier the cell [(x, y)] holds, or {!empty_id} when
    [(x, y)] is outside [t]. *)

(** {1:composing Composing grids}

    A surface drawn apart from the screen, such as a scrolled pane, can have a
    hit grid of its own size. {!draw} then places that grid's elements into the
    screen's grid at the surface's position. The containers already under it
    keep the cells the surface left empty, as they keep the cells it left
    undrawn on screen. A copy that also replaces [dst]'s cells with [src]'s
    empty ones first {!add}s the region with {!empty_id}. A copy of a whole grid
    {!resize}s [dst] to [src]'s dimensions, which clears it, then draws at
    [(0, 0)]. *)

val draw : src:t -> dst:t -> x:int -> y:int -> unit
(** [draw ~src ~dst ~x ~y] copies each cell of [src] that is not empty onto the
    cell of [dst] it lands on, with [src]'s top-left cell at [(x, y)] in [dst].
    An empty cell of [src] leaves [dst]'s cell as it is, and a cell that lands
    outside [dst] or its active clip is skipped. [src] may be [dst], and the
    result is then that of drawing a copy of [src]. *)

(** {1:clipping Clipping}

    The active clip limits {!add} and {!draw}. With an empty clip stack, the
    grid alone limits them. A clip stays pushed across {!clear} and {!resize},
    so a program must pop each clip it pushes, or every later region of every
    frame lands inside it. *)

val push_clip : t -> Matrix_grid.region -> unit
(** [push_clip t r] pushes [r] on [t]'s clip stack, so the active clip becomes
    its intersection with [r]. When they do not overlap, the active clip is
    empty and no region lands until the clip is popped. *)

val pop_clip : t -> unit
(** [pop_clip t] pops the clip pushed last, so the active clip is again the one
    before it. It does nothing when the stack is empty. *)

val clear_clip : t -> unit
(** [clear_clip t] empties [t]'s clip stack. *)

val with_clip : t -> Matrix_grid.region -> (unit -> 'a) -> 'a
(** [with_clip t r f] is [f ()] with [r] pushed as by {!push_clip}. It pops the
    clip when [f] returns or raises. *)
