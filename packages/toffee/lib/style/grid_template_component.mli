(** Grid template component definitions.

    A grid template component represents an element in a [grid-template-columns]
    or [grid-template-rows] definition. It is either a single track sizing
    function or a [repeat()] clause.

    See
    {{:https://developer.mozilla.org/en-US/docs/Web/CSS/grid-template-columns}MDN:
     grid-template-columns} *)

(** {1 Types} *)

type t =
  | Single of Track_sizing_function.t
      (** A single non-repeated track sizing function. *)
  | Repeat of Grid_repetition.t
      (** A [repeat()] clause that generates multiple tracks. *)

(** {1 Constructors} *)

val single : Track_sizing_function.t -> t
(** [single tsf] creates a component with a single track sizing function. *)

val repeat : Grid_repetition.t -> t
(** [repeat rep] creates a component with a [repeat()] clause. *)

(** {1 Common Track Sizing Functions}

    These constructors create [Single] components with commonly used track
    sizing functions. *)

val auto : t
(** [auto] creates a component with automatic track sizing.

    Equivalent to [Single Track_sizing_function.auto]. *)

val min_content : t
(** [min_content] creates a component with min-content track sizing.

    The track size is the minimum size required to fit the content with all soft
    line-wrapping opportunities taken.

    Equivalent to [Single Track_sizing_function.min_content]. *)

val max_content : t
(** [max_content] creates a component with max-content track sizing.

    The track size is the minimum size required to fit the content with no soft
    line-wrapping opportunities taken.

    Equivalent to [Single Track_sizing_function.max_content]. *)

val zero : t
(** [zero] creates a component with zero-sized track.

    Equivalent to [Single Track_sizing_function.zero]. *)

val fr : float -> t
(** [fr value] creates a component with flexible track sizing.

    The track takes a fraction of the available space. The [value] is the
    numerator; the denominator is the sum of all [fr] values in the grid
    dimension.

    See {{:https://www.w3.org/TR/css3-grid-layout/#fr-unit}CSS Grid: fr unit}.

    Equivalent to [Single (Track_sizing_function.fr value)]. *)

val length : float -> t
(** [length value] creates a component with fixed length track sizing.

    Equivalent to [Single (Track_sizing_function.length value)]. *)

val percent : float -> t
(** [percent value] creates a component with percentage-based track sizing.

    The [value] is a fraction in the range [0.0, 1.0], where [0.5] represents
    50%. Equivalent to [Single (Track_sizing_function.percent value)]. *)

val fit_content : Compact_length.t -> t
(** [fit_content lp] creates a component with fit-content track sizing.

    The track size is computed as [max(min_content, min(max_content, lp))],
    clamping the available space by the content's min and max sizes.

    Equivalent to [Single (Track_sizing_function.fit_content lp)]. *)

val minmax : min:Compact_length.t -> max:Compact_length.t -> t
(** [minmax ~min ~max] creates a component with minmax track sizing.

    The track automatically sizes between [min] and [max] based on content and
    available space.

    Equivalent to [Single (Track_sizing_function.minmax ~min ~max)]. *)

(** {1 Predicates} *)

val is_auto_repetition : t -> bool
(** [is_auto_repetition t] returns [true] if [t] is a [Repeat] with automatic
    repetition count ([Auto_fill] or [Auto_fit]).

    Returns [false] for [Single] components or [Repeat] components with a fixed
    [Count]. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal a b] returns [true] if [a] and [b] represent the same component. *)

val compare : t -> t -> int
(** [compare a b] returns a total ordering on components.

    [Single] components sort before [Repeat] components. *)

(** {1 Conversion} *)

val to_string : t -> string
(** [to_string t] returns a CSS-like string representation of the component. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] prints the component to the formatter. *)
