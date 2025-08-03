(** Geometric primitives useful for layout *)

(** {1 Axes} *)

type absolute_axis = Horizontal | Vertical
type abstract_axis = Inline | Block

val other_axis : absolute_axis -> absolute_axis
val abstract_other : abstract_axis -> abstract_axis
val abstract_as_abs_naive : abstract_axis -> absolute_axis

(** {1 Generic records} *)

type 'a rect = { left : 'a; right : 'a; top : 'a; bottom : 'a }
type 'a size = { width : 'a; height : 'a }
type 'a point = { x : 'a; y : 'a }
type 'a line = { start : 'a; end_ : 'a }
type ('min, 'max) min_max = { min : 'min; max : 'max }

(** {1 Handy constants (float / bool / option specialisations)} *)

val rect_zero : float rect
val size_zero : float size
val size_none : float option size
val line_true : bool line
val line_false : bool line
val point_zero : float point
val point_none : float option point

(** Line module *)
module Line : sig
  type 'a t = 'a line = { start : 'a; end_ : 'a }

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Map a function over both start and end values *)

  val true_ : bool t
  val false_ : bool t
end

(** {1 Axis-aware accessors} *)

val size_get_abs : 'a size -> absolute_axis -> 'a
val rect_grid_axis_sum : float rect -> absolute_axis -> float
val size_get : 'a size -> abstract_axis -> 'a
val size_set : 'a size -> abstract_axis -> 'a -> 'a size
val point_get : 'a point -> abstract_axis -> 'a
val point_set : 'a point -> abstract_axis -> 'a -> 'a point
val point_transpose : 'a point -> 'a point

(** {1 Arithmetic helpers (float)} *)

val size_add : float size -> float size -> float size
val size_sub : float size -> float size -> float size
val rect_add : float rect -> float rect -> float rect
val rect_horizontal_axis_sum : float rect -> float
val rect_vertical_axis_sum : float rect -> float
val rect_sum_axes : float rect -> float size
val line_sum : float line -> float

(** {1 Mapping helpers (generic)} *)

val size_map : 'a size -> ('a -> 'b) -> 'b size
val size_map_width : 'a size -> ('a -> 'a) -> 'a size
val size_map_height : 'a size -> ('a -> 'a) -> 'a size
val size_zip_map : 'a size -> 'b size -> ('a -> 'b -> 'c) -> 'c size
val rect_map : 'a rect -> ('a -> 'b) -> 'b rect
val rect_zip_size : 'a rect -> 'b size -> ('a -> 'b -> 'c) -> 'c rect
val point_map : 'a point -> ('a -> 'b) -> 'b point
val line_map : 'a line -> ('a -> 'b) -> 'b line

(** {1 Float helpers} *)

val size_f32_max : float size -> float size -> float size
val size_f32_min : float size -> float size -> float size
val size_has_non_zero_area : float size -> bool

(** {1 Option helpers} *)

val size_option_new : float -> float -> float option size
val size_option_both_axis_defined : 'a option size -> bool
val size_option_unwrap_or : 'a option size -> 'a size -> 'a size
val size_option_or : 'a option size -> 'a option size -> 'a option size

val size_maybe_apply_aspect_ratio :
  float option size -> aspect_ratio:float option -> float option size

(** Size module with helper functions *)
module Size : sig
  val zero : float size
  val none : float option size
  val maybe_clamp_value : float -> float option -> float option -> float

  val maybe_clamp :
    float option size ->
    float option size ->
    float option size ->
    float option size

  val add : float size -> float size -> float size
  val map : 'a size -> f:('a -> 'b) -> 'b size
  val maybe_max : float option size -> float option size -> float option size
  val or_ : 'a option size -> 'a option size -> 'a option size
  val maybe_resolve : 'a size -> 'b -> 'c -> 'a size
  val maybe_add : float option size -> float size -> float option size
  val get : 'a size -> abstract_axis -> 'a
  val set : 'a size -> abstract_axis -> 'a -> 'a size

  val maybe_apply_aspect_ratio :
    float option size -> float option -> float option size

  val map2 : 'a size -> 'b size -> f:('a -> 'b -> 'c) -> 'c size
end

(** Rect module with resolve functions *)
module Rect : sig
  val zero : float rect
  val add : float rect -> float rect -> float rect
  val ( + ) : float rect -> float rect -> float rect
  val sum_axes : float rect -> float size
  val horizontal_axis_sum : float rect -> float
  val vertical_axis_sum : float rect -> float
  val sum_axis : float rect -> absolute_axis -> float
end
