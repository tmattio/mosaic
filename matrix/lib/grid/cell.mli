(** Cell representation and styling for the VTE module. *)

(* Style module is now Ansi.Style *)

type t
(** Represents a single cell on the terminal grid. A cell can be:
    - Empty: An unoccupied cell
    - Glyph: A cell containing a grapheme cluster with its style and width
    - Continuation: A cell that is part of a wide character (second column) *)

val empty : t
(** An empty (unoccupied) cell. *)

val make_glyph :
  style:Ansi.Style.t -> ?link:string -> east_asian_context:bool -> string -> t
(** [make_glyph text ~style ~link ~east_asian_context] creates a glyph cell with
    the given text and style. The display width is automatically calculated.
    @param text UTF-8 encoded grapheme cluster (must be a single grapheme)
    @param style Visual attributes
    @param link Optional hyperlink
    @param east_asian_context
      If true, ambiguous width characters are treated as width 2
    @return A Glyph cell with computed width, or Empty if width is 0 *)

val make_continuation : style:Ansi.Style.t -> t
(** [make_continuation ~style] creates a continuation cell for wide characters
    with the given style. *)

(** {2 Cell accessors} *)

val width : t -> int
(** [width cell] returns the display width of a cell (0 for Empty/Continuation,
    1-2 for Glyph). *)

val get_style : t -> Ansi.Style.t
(** [get_style cell] returns the style of a cell (default_style for Empty). *)

val get_text : t -> string
(** [get_text cell] returns the text content of a cell (empty string for
    Empty/Continuation). *)

val hash : t -> int
(** [hash cell] computes a hash of the cell for comparison purposes. The hash
    includes all fields that affect rendering (text, style attributes, etc.). *)

(** {2 Cell type checks} *)

val is_empty : t -> bool
(** [is_empty cell] returns true if the cell is empty. *)

val is_glyph : t -> bool
(** [is_glyph cell] returns true if the cell contains a glyph. *)

val is_continuation : t -> bool
(** [is_continuation cell] returns true if the cell is a continuation cell. *)

(** {2 Pretty-printing} *)

val pp : Format.formatter -> t -> unit
(** [pp fmt cell] pretty-prints a cell for debugging. *)

(** {2 Equality} *)

val equal : t -> t -> bool
(** [equal c1 c2] returns true if the two cells are equal. *)
