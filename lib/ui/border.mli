type t
(** An abstract type representing the visual border properties of an element. It
    includes per-side control and style. *)

(** [border_style] determines the characters used for drawing borders. *)
type style =
  | Solid  (** Single-line box drawing characters (┌─┐). *)
  | Rounded  (** Single-line with rounded corners (╭─╮). *)
  | Double  (** Double-line box drawing characters (╔═╗). *)
  | Thick  (** Heavy-line box drawing characters (┏━┓). *)
  | ASCII  (** Portable characters for maximum compatibility (+-+|). *)

val make :
  ?top:bool ->
  ?bottom:bool ->
  ?left:bool ->
  ?right:bool ->
  ?style:style ->
  ?color:Ansi.color ->
  unit ->
  t
(** [make ?top ?bottom ?left ?right ?style ?color ()] creates a border
    specification. All sides default to [true] and style to [Solid]. *)

val top : t -> bool
(** [top p] returns whether the top border is enabled. *)

val bottom : t -> bool
(** [bottom p] returns whether the bottom border is enabled. *)

val left : t -> bool
(** [left p] returns whether the left border is enabled. *)

val right : t -> bool
(** [right p] returns whether the right border is enabled. *)

val style : t -> style
(** [style p] returns the border style used for drawing. *)

val color : t -> Ansi.color option
(** [color p] returns the optional color used for the border. *)

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
