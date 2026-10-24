(** Lines of box-drawing characters that join where they meet.

    A {e line set} ({!t}) records horizontal and vertical lines on a rectangle
    of cells, [width] columns by [height] rows from column [x] and row [y],
    named as a grid names them, and holds a value for each of its cells. Each
    line has a {e weight} ({!type-weight}) and a style. A line has an {e arm} in
    each of its cells toward every neighbor it continues into, so its inner
    cells have two arms and its two end cells one. Lines that cross or meet add
    their arms to the cells they share, and a cell's arms say what it is, a
    straight piece, a corner, a tee or a cross. When two lines give a cell an
    arm in the same direction, the arm takes the heavier weight, double being
    the heaviest. A line one cell long has no arm and is a straight piece of its
    direction. A cell takes the style of the first line through it.

    {!Matrix_grid.draw_lines} draws each cell a line passes through with the
    {e glyph} its arms call for in a {e glyph set} ({!glyphs}). A cell whose
    arms all point along one axis is drawn as a straight piece of that axis, so
    a line's end cells are drawn whole. With {!Unicode}, each arm is drawn with
    its weight, as one of Unicode's box-drawing characters (U+2500 to U+257F).
    Unicode has a character for every mix of light and heavy arms, and for
    double arms with single ones when the two arms of each axis have one weight.
    For the other mixes, heavy arms are drawn double and the two arms of an axis
    take the heavier weight. A {!Set} gives one glyph for each shape and draws
    every weight with it.

    The life cycle of a line set:
    + {!create} makes an empty set for a rectangle of cells.
    + {!hline}, {!vline} and {!arm} add lines and arms.
    + {!Matrix_grid.draw_lines} draws it, as often as the program draws the
      frame it belongs to.
    + {!clear} empties it for the lines of another frame.

    This draws a box with a double frame and a light rule across it, whose ends
    join the frame as ╟ and ╢:
    {[
    let l = Matrix_grid.Lines.create ~x:0 ~y:0 ~width:10 ~height:4 in
    Matrix_grid.Lines.hline ~weight:`Double l ~x:0 ~y:0 ~length:10;
    Matrix_grid.Lines.hline ~weight:`Double l ~x:0 ~y:3 ~length:10;
    Matrix_grid.Lines.vline ~weight:`Double l ~x:0 ~y:0 ~length:4;
    Matrix_grid.Lines.vline ~weight:`Double l ~x:9 ~y:0 ~length:4;
    Matrix_grid.Lines.hline l ~x:0 ~y:2 ~length:10;
    let g = Matrix_grid.create ~width:10 ~height:4 () in
    Matrix_grid.draw_lines g l
    ]} *)

(** {1:sets Line sets}

    A line or an arm that leaves the rectangle is cut to it, and a cell outside
    it is left out. The cells inside keep the arms toward the cells left out, so
    a line cut by an edge runs up to it. *)

type weight = [ `Light | `Heavy | `Double ]
(** The type for the weights of lines. *)

type t
(** The type for line sets. *)

val create : x:int -> y:int -> width:int -> height:int -> t
(** [create ~x ~y ~width ~height] is an empty line set for the rectangle of
    [width] columns from column [x] and [height] rows from row [y].

    Raises [Invalid_argument] if [width < 0] or [height < 0]. *)

val x : t -> int
(** [x l] is the first column of [l]'s rectangle. *)

val y : t -> int
(** [y l] is the first row of [l]'s rectangle. *)

val width : t -> int
(** [width l] is the number of columns of [l]'s rectangle. *)

val height : t -> int
(** [height l] is the number of rows of [l]'s rectangle. *)

val clear : t -> unit
(** [clear l] removes every line and arm from [l]. *)

val hline :
  ?weight:weight ->
  ?style:Ansi.Style.t ->
  t ->
  x:int ->
  y:int ->
  length:int ->
  unit
(** [hline ~weight ~style l ~x ~y ~length] adds the horizontal line of the
    [length] cells of row [y] from column [x], of weight [weight] ([`Light] by
    default) in [style] ({!Ansi.Style.default} by default). It adds nothing if
    [length <= 0]. *)

val vline :
  ?weight:weight ->
  ?style:Ansi.Style.t ->
  t ->
  x:int ->
  y:int ->
  length:int ->
  unit
(** [vline ~weight ~style l ~x ~y ~length] adds the vertical line of the
    [length] cells of column [x] from row [y], as {!hline} adds a horizontal
    one. *)

val arm :
  ?weight:weight ->
  ?style:Ansi.Style.t ->
  t ->
  x:int ->
  y:int ->
  [ `Up | `Down | `Left | `Right ] ->
  unit
(** [arm ~weight ~style l ~x ~y dir] adds an arm toward [dir] to the cell of
    column [x] and row [y], as a line into the neighbor in that direction would.
    The neighbor is left as it is, so the arm joins a line to a mark the program
    draws itself, such as a chart's tick mark beside its axis. *)

(** {1:glyph_sets Glyph sets} *)

type set = {
  horizontal : string;  (** A straight piece across. *)
  vertical : string;  (** A straight piece down. *)
  top_left : string;  (** The corner of arms down and right. *)
  top_right : string;  (** The corner of arms down and left. *)
  bottom_left : string;  (** The corner of arms up and right. *)
  bottom_right : string;  (** The corner of arms up and left. *)
  top_t : string;  (** The tee of arms left, right and down. *)
  bottom_t : string;  (** The tee of arms left, right and up. *)
  left_t : string;  (** The tee of arms up, down and right. *)
  right_t : string;  (** The tee of arms up, down and left. *)
  cross : string;  (** The cross of the four arms. *)
}
(** The type for the glyphs of the eleven shapes a cell's arms make, whatever
    their weights. A glyph is drawn as {!Matrix_grid.draw_text} draws a text, so
    a glyph wider than a cell covers the cells after it, and [""] draws nothing.
*)

(** The type for glyph sets. *)
type glyphs =
  | Unicode
      (** Unicode's box-drawing characters, each arm drawn with its weight. *)
  | Rounded
      (** As {!Unicode}, with round corners where both arms are light (╭ ╮ ╰ ╯).
      *)
  | Set of set  (** The glyph of each shape from the set. *)

val ascii : glyphs
(** [ascii] is the set of ASCII characters [-] across, [|] down and [+] for
    every corner, tee and cross, for terminals without box-drawing characters.
*)

val of_border : Border.t -> glyphs
(** [of_border b] is the set of [b]'s characters, so that lines join as the
    boxes of {!Matrix_grid.draw_box} with [b] are drawn. *)

(**/**)

(* For Matrix_grid.draw_lines. The code of a cell is [0] when no line passes
   through it. *)

val code : t -> int -> int
(** [code l idx] is the code of the cell of column [x l + i] and row [y l + j],
    where [idx] is [j * width l + i]. *)

val style : t -> int -> Ansi.Style.t
(** [style l code] is the style of a cell of code [code], not [0]. *)

val scalar : rounded:bool -> int -> Uchar.t
(** [scalar ~rounded code] is the box-drawing character of [code], not [0]. *)

val glyph : set -> int -> string
(** [glyph s code] is the glyph of [code], not [0], in [s]. *)

(**/**)
