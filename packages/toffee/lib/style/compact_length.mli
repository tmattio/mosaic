(** Compact representation of CSS length values.

    This module provides memory-efficient storage of CSS length values using
    tagged float representation. A single float value encodes both the numeric
    value and its type (length, percentage, auto, etc.) using low-order bits as
    tags.

    On 64-bit platforms, the implementation stores a 32-bit float value in the
    upper 32 bits and an 8-bit tag in the lower 8 bits of a 64-bit float,
    preserving full f32 precision. On 32-bit platforms, a different encoding is
    used with reduced precision.

    This encoding allows toffee's style types to remain compact while supporting
    the full range of CSS dimension values.

    {1 Conventions}

    The following conventions apply throughout toffee's style modules:

    - {b Percentage range}: Percentages are represented in \[0.0, 1.0\], not
      \[0.0, 100.0\]. For example, 50% is [0.5].
    - {b Abstract units}: Length values are in abstract units. The
      interpretation (pixels, logical pixels, mm, etc.) is application-defined.
    - {b f32 precision}: Resolution operations use 32-bit float precision to
      match Rust taffy's arithmetic behavior and ensure consistent layout
      results across implementations. *)

type t = float
(** Compact length value represented as a tagged float.

    The low-order bits encode the value type, while the remaining bits store the
    numeric value. This representation is opaque; use the constructor and
    inspection functions to create and examine values. *)

type calc_resolver = int -> float -> float
(** [calc_resolver index parent_size] resolves a calc expression.

    The [index] identifies the calc expression, and [parent_size] provides
    context for percentage resolution. Returns the computed length value. *)

(** {1 Constructors} *)

val length : float -> t
(** [length value] creates an absolute length.

    The value represents a length in abstract units. The interpretation of these
    units (pixels, logical pixels, millimeters, etc.) is application-defined. *)

val percent : float -> t
(** [percent value] creates a percentage length.

    Percentage values are resolved relative to the size of the containing block.
    See [Conventions] for the percentage range. *)

val auto : t
(** [auto] represents automatic sizing.

    The dimension is computed according to algorithm-specific rules regarding
    the default size of boxes. *)

val fr : float -> t
(** [fr value] creates a fractional unit length.

    Fractional units ([fr]) divide available space proportionally in CSS Grid
    layouts. The [value] is the numerator of the fraction; the denominator is
    the sum of all [fr] values in that grid dimension.

    See
    {{:https://www.w3.org/TR/css3-grid-layout/#fr-unit}CSS Grid Layout
     specification}. *)

val min_content : t
(** [min_content] represents intrinsic minimum size.

    This is the smallest size that can fit the item's contents with all soft
    line-wrapping opportunities taken. *)

val max_content : t
(** [max_content] represents intrinsic maximum size.

    This is the smallest size that can fit the item's contents with no soft
    line-wrapping opportunities taken. *)

val fit_content_px : float -> t
(** [fit_content_px limit] creates a fit-content length with pixel limit.

    The size is computed as [max(min_content, min(max_content, limit))],
    clamping [limit] by the min-content and max-content sizes. *)

val fit_content_percent : float -> t
(** [fit_content_percent limit] creates a fit-content length with percentage
    limit.

    The size is computed as [max(min_content, min(max_content, limit))], where
    [limit] is resolved as a percentage of the containing block, then clamped by
    the min-content and max-content sizes.

    The [limit] value must be in the range [0.0, 1.0]. *)

val calc : int -> t
(** [calc index] creates a calc expression reference.

    The [index] is an opaque handle to the actual calc expression, used with a
    [calc_resolver] to compute the final value. *)

(** {1 Constants} *)

val zero : t
(** [zero] is an absolute length of 0.0. *)

(** {1 Inspection} *)

val is_length : t -> bool
(** [is_length t] returns [true] if [t] is an absolute length. *)

val is_percent : t -> bool
(** [is_percent t] returns [true] if [t] is a percentage length. *)

val is_auto : t -> bool
(** [is_auto t] returns [true] if [t] is automatic sizing. *)

val is_fr : t -> bool
(** [is_fr t] returns [true] if [t] is a fractional unit. *)

val is_min_content : t -> bool
(** [is_min_content t] returns [true] if [t] is min-content. *)

val is_max_content : t -> bool
(** [is_max_content t] returns [true] if [t] is max-content. *)

val is_fit_content_px : t -> bool
(** [is_fit_content_px t] returns [true] if [t] is fit-content with a pixel
    limit. *)

val is_fit_content_percent : t -> bool
(** [is_fit_content_percent t] returns [true] if [t] is fit-content with a
    percentage limit. *)

val is_calc : t -> bool
(** [is_calc t] returns [true] if [t] is a calc expression. *)

val is_length_or_percentage : t -> bool
(** [is_length_or_percentage t] returns [true] if [t] is either an absolute
    length or percentage. *)

val is_intrinsic : t -> bool
(** [is_intrinsic t] returns [true] if [t] is auto, min-content, max-content, or
    fit-content. *)

val is_fit_content : t -> bool
(** [is_fit_content t] returns [true] if [t] is any fit-content variant. *)

val is_zero : t -> bool
(** [is_zero t] returns [true] if [t] is an absolute length with value 0.0. *)

val is_max_or_fit_content : t -> bool
(** [is_max_or_fit_content t] returns [true] if [t] is max-content or any
    fit-content variant. *)

val is_max_content_alike : t -> bool
(** [is_max_content_alike t] returns [true] if [t] behaves like max-content.

    Returns [true] for auto, max-content, and fit-content variants. Per CSS Grid
    specification: "In all cases, treat auto and fit-content() as max-content,
    except where specified otherwise for fit-content()."

    See
    {{:https://www.w3.org/TR/css-grid-1/#algo-terms}CSS Grid Layout
     specification}. *)

val is_min_or_max_content : t -> bool
(** [is_min_or_max_content t] returns [true] if [t] is min-content or
    max-content. *)

val uses_percentage : t -> bool
(** [uses_percentage t] returns [true] if [t] depends on parent size.

    Returns [true] for percentage values, fit-content with percentage limits,
    and calc expressions. *)

(** {1 Value Extraction} *)

val get_tag : t -> int
(** [get_tag t] extracts the internal tag from [t].

    This is a low-level function for internal use by other style modules. *)

val get_value : t -> float
(** [get_value t] extracts the numeric value without validation.

    This is a low-level function for internal use. Unlike [value], this does not
    raise for auto/min-content/max-content, returning an undefined value
    instead. *)

(** {2 Tag constants}

    These are internal constants for low-level tag inspection. *)

val length_tag : int
(** Tag value for absolute lengths. *)

val percent_tag : int
(** Tag value for percentages. *)

val fit_content_px_tag : int
(** Tag value for fit-content with pixel limit. *)

val fit_content_percent_tag : int
(** Tag value for fit-content with percentage limit. *)

val value : t -> float
(** [value t] extracts the numeric value from [t].

    Raises [Failure] if [t] is auto, min-content, max-content, or calc, which
    have no associated numeric value. *)

val get_calc_index : t -> int
(** [get_calc_index t] extracts the calc expression index.

    Behavior is undefined if [t] is not a calc expression. Use [is_calc] to
    check before calling. *)

val resolved_percentage_size : t -> float -> float option
(** [resolved_percentage_size t parent_size] resolves percentage to absolute
    length.

    Returns [Some value] if [t] is a percentage, computing [value * parent_size]
    with f32 precision. Returns [None] otherwise. *)

val resolved_percentage_size_with_calc :
  t -> float -> calc_resolver -> float option
(** [resolved_percentage_size_with_calc t parent_size calc_resolver] resolves
    percentage or calc to absolute length.

    Returns [Some value] if [t] is a percentage or calc expression. For
    percentages, computes [value * parent_size] with f32 precision. For calc
    expressions, applies [calc_resolver]. Returns [None] otherwise. *)

(** {1 Conversion} *)

val to_string : t -> string
(** [to_string t] returns a human-readable representation.

    Examples: ["10px"], ["50%"], ["auto"], ["2fr"], ["min-content"],
    ["max-content"], ["fit-content(200px)"], ["calc(#0)"]. *)

(** {1 Comparison} *)

val equal : t -> t -> bool
(** [equal a b] tests equality of compact length values.

    For numeric values, uses floating-point epsilon comparison. For calc
    expressions, compares indices. *)

val compare : t -> t -> int
(** [compare a b] provides total ordering of compact length values.

    Calc expressions sort before all other values. Within each type, values are
    ordered numerically where applicable. *)

val pp : Format.formatter -> t -> unit
(** [pp fmt t] pretty-prints [t] to [fmt].

    Uses the same format as [to_string]. *)
