(** CSS dimension values supporting length, percentage, auto, and calc().

    A dimension represents a CSS size value used for properties like [width],
    [height], and [flex_basis]. Dimensions are stored in a compact tagged
    representation backed by {!Compact_length}.

    See {!Compact_length} for conventions on percentage range, abstract units,
    and f32 precision. *)

type t = Compact_length.t
(** A CSS dimension value. *)

(** {1 Constructors} *)

val length : float -> t
(** [length value] creates an absolute length dimension. *)

val percent : float -> t
(** [percent value] creates a percentage dimension.

    The [value] is a fraction in the range [0.0, 1.0], where [0.5] represents
    50%. For 0–100 percentage inputs, prefer {!pct}. *)

val px : float -> t
(** [px value] creates an absolute length dimension in abstract pixel units.

    Equivalent to {!length}. Provided for CSS-style naming. *)

val pct : float -> t
(** [pct percent] creates a percentage dimension from a 0–100 value.

    [pct 50.0] is equivalent to [percent 0.5]. *)

val auto : t
(** [auto] represents automatic sizing. *)

val calc : int -> t
(** [calc index] creates a calc() dimension from an opaque index.

    The index identifies a calc expression to be resolved later. The lower 3
    bits are reserved as a tag and returned as 0. *)

(** {1 Constants} *)

val zero : t
(** [zero] is equivalent to [length 0.0]. *)

(** {1 Inspection} *)

val is_length : t -> bool
(** [is_length t] returns [true] if [t] is a length dimension. *)

val is_percent : t -> bool
(** [is_percent t] returns [true] if [t] is a percentage dimension. *)

val is_auto : t -> bool
(** [is_auto t] returns [true] if [t] is an auto dimension. *)

val is_calc : t -> bool
(** [is_calc t] returns [true] if [t] is a calc() dimension. *)

val is_zero : t -> bool
(** [is_zero t] returns [true] if [t] is a length with value 0.0. *)

val uses_percentage : t -> bool
(** [uses_percentage t] returns [true] if [t] involves percentage computation.

    Returns [true] for percent and calc dimensions. *)

(** {1 Value Extraction} *)

val value : t -> float
(** [value t] extracts the numeric value from [t].

    Raises [Failure] if [t] is auto, min-content, max-content, or calc. *)

val to_option : t -> float option
(** [to_option t] returns [Some value] if [t] is a length, [None] otherwise. *)

(** {1 Resolution} *)

val resolved_percentage_size : t -> float -> float option
(** [resolved_percentage_size t parent_size] resolves percentage dimensions.

    Returns [Some (value * parent_size)] if [t] is a percentage, [None]
    otherwise. The result is rounded to f32 precision to match Taffy behavior.
*)

val resolved_percentage_size_with_calc :
  t -> float -> (int -> float -> float) -> float option
(** [resolved_percentage_size_with_calc t parent_size calc_resolver] resolves
    percentage and calc dimensions.

    Returns:
    - [Some (value * parent_size)] if [t] is a percentage
    - [Some (calc_resolver index parent_size)] if [t] is a calc
    - [None] otherwise

    The calc resolver receives the calc index and parent size, returning the
    computed value. Percentage results are rounded to f32 precision. *)

val maybe_resolve : t -> float option -> (int -> float -> float) -> float option
(** [maybe_resolve t context calc_resolver] resolves [t] using [context].

    Returns:
    - [None] if [t] is auto or [context] is [None] for percent/calc
    - [Some value] if [t] is a length
    - [Some (context * value)] if [t] is a percent and [context] is [Some]
    - [Some (calc_resolver index context)] if [t] is a calc and [context] is
      [Some]

    Percentage and calc results are rounded to f32 precision.

    Raises [Failure] if [t] is not length, percentage, auto, or calc. *)

val resolve_or_zero : t -> float option -> (int -> float -> float) -> float
(** [resolve_or_zero t context calc_resolver] resolves [t] or returns 0.0.

    Equivalent to
    [Option.value ~default:0.0 (maybe_resolve t context calc_resolver)]. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal a b] tests structural equality.

    Calc dimensions are equal if their indices match. Lengths and percentages
    are compared within [Float.epsilon] tolerance. *)

val compare : t -> t -> int
(** [compare a b] provides total ordering.

    Calc dimensions sort before others by index. Non-calc dimensions sort by tag
    then value. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] formats [t] for debugging. *)

val to_string : t -> string
(** [to_string t] converts [t] to a human-readable string.

    Examples: ["10px"], ["50%"], ["auto"], ["calc(#0)"]. *)
