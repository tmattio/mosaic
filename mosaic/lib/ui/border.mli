(** [line_style] determines the characters used for drawing borders. *)
type line_style =
  | Solid  (** Single-line box drawing characters (┌─┐). *)
  | Rounded  (** Single-line with rounded corners (╭─╮). *)
  | Double  (** Double-line box drawing characters (╔═╗). *)
  | Thick  (** Heavy-line box drawing characters (┏━┓). *)
  | ASCII  (** Portable characters for maximum compatibility (+-+|). *)

(** Alignment for text within borders *)
type text_align = [ `Left | `Center | `Right ]

(** Text to be embedded in a border line *)
type border_text = {
  text : string;
  align : text_align;
  style : Style.t option;
}

type t = {
  top : bool;
  bottom : bool;
  left : bool;
  right : bool;
  line_style : line_style;
  color : Ansi.color option;
  style : Style.t option;
  top_text : border_text option;
  bottom_text : border_text option;
}
(** Type representing the visual border properties of an element. It
    includes per-side control, style, and optional text overlays. *)

val make :
  ?top:bool ->
  ?bottom:bool ->
  ?left:bool ->
  ?right:bool ->
  ?line_style:line_style ->
  ?color:Ansi.color ->
  ?style:Style.t ->
  ?top_text:border_text ->
  ?bottom_text:border_text ->
  unit ->
  t
(** [make ?top ?bottom ?left ?right ?line_style ?color ?style ?top_text ?bottom_text ()] creates a
    border specification. All sides default to [true] and line_style to [Solid].
    The optional style parameter allows applying additional text attributes to
    the border. The optional top_text and bottom_text allow embedding text in the border lines. *)

val top : t -> bool
(** [top p] returns whether the top border is enabled. *)

val bottom : t -> bool
(** [bottom p] returns whether the bottom border is enabled. *)

val left : t -> bool
(** [left p] returns whether the left border is enabled. *)

val right : t -> bool
(** [right p] returns whether the right border is enabled. *)

val line_style : t -> line_style
(** [line_style p] returns the line style used for drawing. *)

val color : t -> Ansi.color option
(** [color p] returns the optional color used for the border. *)

val style : t -> Style.t option
(** [style p] returns the optional style used for the border. *)

val with_style : t -> Style.t -> t
(** [with_style border style] returns a new border with the given style. *)

val with_top_text : t -> string -> ?align:text_align -> ?style:Style.t -> unit -> t
(** [with_top_text border text ?align ?style ()] returns a new border with text embedded in the top border. *)

val with_bottom_text : t -> string -> ?align:text_align -> ?style:Style.t -> unit -> t
(** [with_bottom_text border text ?align ?style ()] returns a new border with text embedded in the bottom border. *)

val normal : t
(** Pre-defined solid border on all sides. *)

val ascii : t
(** Pre-defined ASCII border on all sides, using portable characters. *)

val rounded : t
(** Pre-defined rounded border on all sides. *)

val double : t
(** Pre-defined double-line border on all sides. *)

val thick : t
(** Pre-defined thick-line border on all sides. *)

val space_h : t -> int
(** [space_h border] returns the horizontal space required by the border. If no
    border is specified, returns 0. *)

val space_v : t -> int
(** [space_v border] returns the vertical space required by the border. If no
    border is specified, returns 0. *)

type border_chars = {
  tl : string;  (** Top-left corner *)
  th : string;  (** Top horizontal *)
  tr : string;  (** Top-right corner *)
  vl : string;  (** Vertical left *)
  bl : string;  (** Bottom-left corner *)
  bh : string;  (** Bottom horizontal *)
  br : string;  (** Bottom-right corner *)
  vr : string;  (** Vertical right *)
  ml : string;  (** Middle left (T-junction) *)
  mr : string;  (** Middle right (T-junction) *)
  mt : string;  (** Middle top (T-junction) *)
  mb : string;  (** Middle bottom (T-junction) *)
  mc : string;  (** Middle cross *)
}

val get_chars : line_style -> border_chars
(** [get_chars line_style] returns the box-drawing characters for the given
    border line style. *)
