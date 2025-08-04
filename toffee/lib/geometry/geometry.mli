(** Geometric primitives useful for layout *)

(** {1 Axis types} *)

(** The simple absolute horizontal and vertical axis *)
module Absolute_axis : sig
  type t = Horizontal | Vertical

  val other : t -> t
  (** Returns the other variant of the enum *)
end

(** The CSS abstract axis *)
module Abstract_axis : sig
  type t = Inline | Block

  val other : t -> t
  (** Returns the other variant of the enum *)

  val to_absolute_naive : t -> Absolute_axis.t
  (** Convert an abstract axis to absolute axis naively assuming Inline is
      Horizontal *)
end

(** {1 Geometric types} *)

(** The width and height of a rectangle *)
module Size : sig
  type 'a t = { width : 'a; height : 'a }

  val zero : float t
  (** A size with zero width and height *)

  val none : 'a option t
  (** A size with None width and height *)

  val make : 'a -> 'a -> 'a t
  (** Create a new size *)

  val square : 'a -> 'a t
  (** Create a square size with equal width and height *)

  val add : float t -> float t -> float t
  (** Add two sizes *)

  val sub : float t -> float t -> float t
  (** Subtract two sizes *)

  val ( + ) : float t -> float t -> float t
  (** Infix operator for add *)

  val ( - ) : float t -> float t -> float t
  (** Infix operator for subtract *)

  val ( * ) : float t -> float -> float t
  (** Scalar multiplication *)

  val ( / ) : float t -> float -> float t
  (** Scalar division *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Apply a function to both width and height *)

  val map_width : ('a -> 'a) -> 'a t -> 'a t
  (** Apply a function to width only *)

  val map_height : ('a -> 'a) -> 'a t -> 'a t
  (** Apply a function to height only *)

  val zip_map : 'a t -> 'b t -> ('a -> 'b -> 'c) -> 'c t
  (** Apply a function to corresponding components of two sizes *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Apply a binary function to corresponding components (map2 f s1 s2) *)

  val get : 'a t -> Abstract_axis.t -> 'a
  (** Get component based on abstract axis *)

  val set : 'a t -> Abstract_axis.t -> 'a -> 'a t
  (** Set component based on abstract axis *)

  val get_absolute : 'a t -> Absolute_axis.t -> 'a
  (** Get component based on absolute axis *)

  val max : 'a t -> 'a t -> 'a t
  (** Component-wise maximum *)

  val min : 'a t -> 'a t -> 'a t
  (** Component-wise minimum *)

  val has_non_zero_area : float t -> bool
  (** Returns true if both width and height are greater than 0 *)

  val equal : 'a t -> 'a t -> bool
  (** Test equality of two sizes *)

  val unwrap_or : 'a option t -> 'a t -> 'a t
  (** Unwrap options with defaults *)

  val choose_first : 'a option t -> 'a option t -> 'a option t
  (** Choose first Some value for each component, fallback to second *)

  val both_axis_defined : 'a option t -> bool
  (** Returns true if both width and height are Some *)

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  (** Compare two sizes using the given comparison function *)

  val equal_with : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** Test equality using the given equality function *)

  val to_string : ('a -> string) -> 'a t -> string
  (** Convert to string using the given function *)

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  (** Pretty printer using the given formatter *)

  val equal_option : ('a -> 'a -> bool) -> 'a option t -> 'a option t -> bool
  (** Test equality of option sizes *)

  (** {2 Operations with optional values} *)

  (** {3 Option → Option operations}
      These preserve None in the first argument *)

  val min_option : float option t -> float option t -> float option t
  (** Component-wise minimum.
      - Some l, Some r → Some (min l r)
      - Some _, None → keep first (None = unbounded)
      - None, _ → None *)

  val max_option : float option t -> float option t -> float option t
  (** Component-wise maximum.
      - Some l, Some r → Some (max l r)
      - Some _, None → keep first (None = unbounded)
      - None, _ → None *)

  val add_option : float option t -> float option t -> float option t
  (** Component-wise addition.
      - Some l, Some r → Some (l + r)
      - Some _, None → keep first (None = zero)
      - None, _ → None *)

  val sub_option : float option t -> float option t -> float option t
  (** Component-wise subtraction.
      - Some l, Some r → Some (l - r)
      - Some _, None → keep first (None = zero)
      - None, _ → None *)

  val clamp_option :
    float option t -> float option t -> float option t -> float option t
  (** Clamp between optional bounds. None means no constraint. *)

  (** {3 Concrete + Option → Option operations}
      These always return Some *)

  val min_with_option : float t -> float option t -> float option t
  (** Always returns Some. None in second arg = unbounded. *)

  val max_with_option : float t -> float option t -> float option t
  (** Always returns Some. None in second arg = unbounded. *)

  val add_with_option : float t -> float option t -> float option t
  (** Always returns Some. None in second arg = zero. *)

  val sub_with_option : float t -> float option t -> float option t
  (** Always returns Some. None in second arg = zero. *)

  (** {3 Concrete operations with optional parameters}
      These return concrete values *)

  val clamp : float t -> float option t -> float option t -> float t
  (** Clamp concrete value between optional bounds. None = no constraint. *)

  val min_or_self : float t -> float option t -> float t
  (** Return minimum if Some, otherwise return self *)

  val max_or_self : float t -> float option t -> float t
  (** Return maximum if Some, otherwise return self *)

  val add_or_zero : float t -> float option t -> float t
  (** Add if Some, otherwise treat as zero *)

  val sub_or_zero : float t -> float option t -> float t
  (** Subtract if Some, otherwise treat as zero *)

  (** {3 Utility operations} *)

  val apply_aspect_ratio : float option t -> float option -> float option t
  (** Apply aspect ratio to compute missing dimension *)
end

(** A 2-dimensional coordinate *)
module Point : sig
  type 'a t = { x : 'a; y : 'a }

  val zero : float t
  (** Origin point (0, 0) *)

  val none : 'a option t
  (** Point with None coordinates *)

  val make : 'a -> 'a -> 'a t
  (** Create a new point *)

  val add : float t -> float t -> float t
  (** Add two points *)

  val sub : float t -> float t -> float t
  (** Subtract two points *)

  val ( + ) : float t -> float t -> float t
  (** Infix operator for add *)

  val ( - ) : float t -> float t -> float t
  (** Infix operator for subtract *)

  val ( * ) : float t -> float -> float t
  (** Scalar multiplication *)

  val ( / ) : float t -> float -> float t
  (** Scalar division *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Apply a function to both x and y *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Apply a binary function to corresponding components *)

  val get : 'a t -> Abstract_axis.t -> 'a
  (** Get component based on abstract axis *)

  val set : 'a t -> Abstract_axis.t -> 'a -> 'a t
  (** Set component based on abstract axis *)

  val transpose : 'a t -> 'a t
  (** Swap x and y components *)

  val to_size : 'a t -> 'a Size.t
  (** Convert point to size *)

  val of_size : 'a Size.t -> 'a t
  (** Create point from size *)

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  (** Compare two points using the given comparison function *)

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** Test equality using the given equality function *)

  val to_string : ('a -> string) -> 'a t -> string
  (** Convert to string using the given function *)

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  (** Pretty printer using the given formatter *)
end

(** An abstract line with start and end *)
module Line : sig
  type 'a t = { start : 'a; end_ : 'a }

  val both_true : bool t
  (** Line with both start and end set to true *)

  val both_false : bool t
  (** Line with both start and end set to false *)

  val make : 'a -> 'a -> 'a t
  (** Create a new line *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Apply a function to both start and end *)

  val sum : float t -> float
  (** Sum of start and end *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Apply a binary function to corresponding components *)

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  (** Compare two lines using the given comparison function *)

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** Test equality using the given equality function *)

  val to_string : ('a -> string) -> 'a t -> string
  (** Convert to string using the given function *)

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  (** Pretty printer using the given formatter *)
end

(** An axis-aligned UI rectangle *)
module Rect : sig
  type 'a t = { left : 'a; right : 'a; top : 'a; bottom : 'a }

  val zero : float t
  (** A rectangle with all values set to 0.0 *)

  val all : 'a -> 'a t
  (** Create a rectangle with all sides set to the same value *)

  val make : left:'a -> right:'a -> top:'a -> bottom:'a -> 'a t
  (** Create a new rectangle *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Apply a function to all four sides *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Apply a binary function to corresponding sides of two rectangles *)

  val zip_size : 'a t -> 'b Size.t -> ('a -> 'b -> 'c) -> 'c t
  (** Apply a function to rect sides with size dimensions *)

  val horizontal_components : 'a t -> 'a Line.t
  (** Extract left/right as a Line *)

  val vertical_components : 'a t -> 'a Line.t
  (** Extract top/bottom as a Line *)

  val add : float t -> float t -> float t
  (** Add two rectangles *)

  val ( + ) : float t -> float t -> float t
  (** Infix operator for add *)

  val horizontal_axis_sum : float t -> float
  (** Sum of left + right *)

  val vertical_axis_sum : float t -> float
  (** Sum of top + bottom *)

  val sum_axes : float t -> float Size.t
  (** Both horizontal and vertical sums as a Size *)

  val grid_axis_sum : float t -> Absolute_axis.t -> float
  (** Sum based on absolute axis *)

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  (** Compare two rectangles using the given comparison function *)

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** Test equality using the given equality function *)

  val to_string : ('a -> string) -> 'a t -> string
  (** Convert to string using the given function *)

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  (** Pretty printer using the given formatter *)
end

(** Generic struct which holds a min and max value *)
module Min_max : sig
  type ('min, 'max) t = { min : 'min; max : 'max }

  val make : 'min -> 'max -> ('min, 'max) t
  (** Create a new min_max *)

  val map : ('a -> 'b) -> ('a, 'a) t -> ('b, 'b) t
  (** Apply a function to both min and max *)

  val map_min : ('min -> 'min) -> ('min, 'max) t -> ('min, 'max) t
  (** Apply a function to min only *)

  val map_max : ('max -> 'max) -> ('min, 'max) t -> ('min, 'max) t
  (** Apply a function to max only *)
end

(** Container that holds an item in each absolute axis *)
module In_both_abs_axis : sig
  type 'a t = { horizontal : 'a; vertical : 'a }

  val make : horizontal:'a -> vertical:'a -> 'a t
  (** Create a new container with values for both axes *)

  val get : 'a t -> Absolute_axis.t -> 'a
  (** Get the value for the specified axis *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** Apply a function to both values *)

  val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  (** Apply a binary function to corresponding values *)

  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  (** Test equality using the given equality function *)

  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  (** Compare using the given comparison function *)

  val to_string : ('a -> string) -> 'a t -> string
  (** Convert to string using the given function *)

  val pp : (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a t -> unit
  (** Pretty printer using the given formatter *)
end

(** {1 Type aliases for convenience} *)

type absolute_axis = Absolute_axis.t
type abstract_axis = Abstract_axis.t
type 'a rect = 'a Rect.t
type 'a size = 'a Size.t
type 'a point = 'a Point.t
type 'a line = 'a Line.t
type ('min, 'max) min_max = ('min, 'max) Min_max.t
type 'a in_both_abs_axis = 'a In_both_abs_axis.t
