(** Images, which describe what to draw in a rectangle of terminal cells.

    An {e image} is an immutable value with a width and a height in cells, and
    the primitives it draws in that rectangle. A {e primitive} is a text, a
    fill, a box, a hit region or a drawing function. Coordinates are in cells,
    with [(0, 0)] at an image's top-left corner, x growing rightward and y
    downward. Building and composing images draws nothing. {!draw} draws an
    image's primitives into a {!Matrix_grid.t} in order, so a later primitive
    draws over an earlier one.

    An image's size comes from its content. An image is {e empty} when its width
    or its height is [0]. Every value that would return an empty image returns
    {!empty}, of size [(0, 0)], and composition leaves empty images out.

    A {e clip} is a rectangle outside of which an image draws nothing. {!crop}
    sets one, and a composed image keeps the clip of each part, intersected with
    the clips of the images around it.

    A {e hit region} is a rectangle tagged with a {e hit id}, a positive integer
    that the program chooses. When {!draw} is given a
    {!Matrix_screen.Hit_grid.t}, it writes each hit region's id into the cells
    the region covers, so that the program can tell which image a cell belongs
    to.

    Unless a value's doc says otherwise, a negative width, height or padding is
    taken as [0], and no value raises.

    This draws a status line over a rule and a message into a new grid. Each
    image is as wide as its content, and {!vcat} stacks the three.
    {[
    let header =
      Matrix_image.hcat
        [
          Matrix_image.text "Status: ";
          Matrix_image.text
            ~style:(Ansi.Style.make ~fg:Ansi.Color.green ())
            "OK";
        ]
    in
    let panel =
      Matrix_image.vcat
        [
          header;
          Matrix_image.rule_h ~width:20 ();
          Matrix_image.text "System ready";
        ]
    in
    let grid = Matrix_grid.create ~width:80 ~height:24 () in
    Matrix_image.draw panel grid
    ]} *)

module Color = Ansi.Color
module Style = Ansi.Style

(** {1:types Types} *)

type style = Style.t
(** The type for styles, {!Ansi.Style.t}. *)

type h_align = [ `Left | `Center | `Right ]
(** The type for horizontal alignments. *)

type v_align = [ `Top | `Middle | `Bottom ]
(** The type for vertical alignments. *)

type hit_id = int
(** The type for hit ids. A hit id must be positive. {!with_hit} and
    {!with_hit_rect} ignore any other. *)

type t
(** The type for images. *)

(** {1:constructors Primitives}

    Each of these images holds one primitive at its origin, or none. *)

val empty : t
(** [empty] is the image of size [(0, 0)]. *)

val void : int -> int -> t
(** [void w h] is a [w] by [h] image that draws nothing, or {!empty} if [w] or
    [h] is [0]. *)

val fill : ?color:Color.t -> width:int -> height:int -> unit -> t
(** [fill ~width ~height ()] is a [width] by [height] rectangle of [color],
    black by default. {!draw} fills it with {!Matrix_grid.fill_rect}, which
    leaves cells unchanged under {!Color.transparent} and blends a translucent
    color over them. *)

val text :
  ?style:Style.t -> ?width_method:Matrix_text.width_method -> string -> t
(** [text s] is [s] in [style], {!Ansi.Style.default} by default, one row per
    line of [s] split at ['\n'], so a final ['\n'] adds an empty last row. Its
    width is that of its widest line under [width_method], [`Unicode] by
    default, where a tab is 2 cells wide. It is {!empty} when every line is
    empty. *)

val box :
  ?border:Matrix_grid.Border.t ->
  ?border_sides:Matrix_grid.Border.side list ->
  ?border_style:Style.t ->
  ?fill:Color.t ->
  width:int ->
  height:int ->
  unit ->
  t
(** [box ~width ~height ()] is a [width] by [height] box, which {!draw} draws
    with {!Matrix_grid.draw_box}. [border] defaults to
    {!Matrix_grid.Border.single}, [border_sides] to {!Matrix_grid.Border.all},
    and [border_style] to {!Ansi.Style.default}. [fill] is passed on to
    {!Matrix_grid.draw_box}, and is absent by default. *)

val rule_h : ?style:Style.t -> width:int -> unit -> t
(** [rule_h ~width ()] is a horizontal line of [width] cells, drawn as text with
    the horizontal glyph of {!Matrix_grid.Border.single} in [style],
    {!Ansi.Style.default} by default. *)

val rule_v : ?style:Style.t -> height:int -> unit -> t
(** [rule_v ~height ()] is a vertical line of [height] cells, drawn as text with
    the vertical glyph of {!Matrix_grid.Border.single} in [style],
    {!Ansi.Style.default} by default. *)

(** {1:accessors Size} *)

val width : t -> int
(** [width img] is [img]'s width in cells. *)

val height : t -> int
(** [height img] is [img]'s height in cells. *)

val size : t -> int * int
(** [size img] is [(width img, height img)]. *)

(** {1:composition Composition}

    Space that a composed image holds and no part covers draws nothing, and
    shows what the grid had there. *)

val hcat : t list -> t
(** [hcat imgs] places [imgs] side by side from left to right, aligned at their
    tops. Its width is the sum of their widths and its height the greatest of
    their heights. *)

val vcat : t list -> t
(** [vcat imgs] places [imgs] one below the other from top to bottom, aligned at
    their left edges. Its height is the sum of their heights and its width the
    greatest of their widths. *)

val overlay : t list -> t
(** [overlay imgs] places [imgs] at one origin, each drawn after the images
    before it in the list, so later images draw over earlier ones. Its width and
    height are the greatest of theirs. *)

val pad : ?left:int -> ?right:int -> ?top:int -> ?bottom:int -> t -> t
(** [pad img] is [img] with [left], [right], [top] and [bottom] cells of space
    added on those sides, each [0] by default. *)

val hpad : int -> int -> t -> t
(** [hpad l r img] is [pad ~left:l ~right:r img]. *)

val vpad : int -> int -> t -> t
(** [vpad t b img] is [pad ~top:t ~bottom:b img]. *)

val crop : ?l:int -> ?r:int -> ?t:int -> ?b:int -> t -> t
(** [crop img] is [img] without [l], [r], [t] and [b] cells on its left, right,
    top and bottom sides, each [0] by default, with a clip on what remains. It
    is {!empty} when nothing remains. A negative side adds space, as {!pad} does
    with its magnitude. *)

val hcrop : int -> int -> t -> t
(** [hcrop l r img] is [crop ~l ~r img]. *)

val vcrop : int -> int -> t -> t
(** [vcrop t b img] is [crop ~t ~b img]. *)

val hsnap : ?align:h_align -> int -> t -> t
(** [hsnap w img] is [img] made [w] columns wide, by {!hcrop} or {!hpad} on the
    side that [align] gives, [`Center] by default. A centered image puts the odd
    column on its right. It is {!empty} when [w <= 0]. *)

val vsnap : ?align:v_align -> int -> t -> t
(** [vsnap h img] is [img] made [h] rows high, by {!vcrop} or {!vpad} on the
    side that [align] gives, [`Middle] by default. A centered image puts the odd
    row at its bottom. It is {!empty} when [h <= 0]. *)

(** {1:hit_regions Hit regions}

    A hit region added to an image comes after the image's other primitives, and
    {!Matrix_screen.Hit_grid.add} writes it over the ids already in the cells it
    covers. *)

val with_hit : id:hit_id -> t -> t
(** [with_hit ~id img] is [img] with a hit region of [id] that covers it. It is
    [img] when [id <= 0] or [img] is empty. *)

val with_hit_rect :
  id:hit_id -> x:int -> y:int -> width:int -> height:int -> t -> t
(** [with_hit_rect ~id ~x ~y ~width ~height img] is [img] with a hit region of
    [id] on the [width] by [height] rectangle at [(x, y)] in [img]'s
    coordinates. The rectangle may reach past [img]'s edges, and only a clip
    limits it. It is [img] when [id <= 0], [width <= 0] or [height <= 0]. *)

(** {1:drawing Drawing} *)

val draw :
  ?hits:Matrix_screen.Hit_grid.t ->
  ?x:int ->
  ?y:int ->
  t ->
  Matrix_grid.t ->
  unit
(** [draw img grid] draws [img] into [grid] with its top-left corner at
    [(x, y)], [(0, 0)] by default, within [grid]'s own clip. It writes [img]'s
    hit regions into [hits] when [hits] is given, and drops them otherwise. *)

(** {1:low Drawing functions} *)

val custom :
  width:int ->
  height:int ->
  (Matrix_grid.t -> Matrix_screen.Hit_grid.t option -> x:int -> y:int -> unit) ->
  t
(** [custom ~width ~height f] is a [width] by [height] image that {!draw} draws
    by calling [f grid hits ~x ~y], where [grid] and [hits] are those given to
    {!draw} and [(x, y)] is the image's top-left corner in [grid]. [f] must draw
    within those [width] by [height] cells, since only a clip limits what it
    draws. The image's clips apply to what [f] draws into [grid] and [hits]. *)
