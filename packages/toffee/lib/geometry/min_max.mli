(** Generic container for minimum and maximum constraint values.

    A min-max pair represents a constraint range used throughout the layout
    engine for size bounds, such as [min-width]/[max-width] or
    [min-height]/[max-height] in CSS. The type parameters ['min] and ['max]
    allow for different types, enabling flexible constraint representations. *)

type ('min, 'max) t = { min : 'min; max : 'max }
(** [('min, 'max) t] is a container holding a minimum value of type ['min] and a
    maximum value of type ['max].

    The separate type parameters allow for heterogeneous constraints, such as
    [{min = Some 100.0; max = None}] to represent a minimum bound without a
    maximum bound. *)

(** {1 Creation} *)

val make : 'min -> 'max -> ('min, 'max) t
(** [make min max] creates a min-max pair. *)

(** {1 Transformations} *)

val map : ('a -> 'b) -> ('a, 'a) t -> ('b, 'b) t
(** [map f min_max] applies [f] to both the min and max values. *)

val map_min : ('min -> 'min) -> ('min, 'max) t -> ('min, 'max) t
(** [map_min f min_max] applies [f] to the min value only, preserving the max
    value unchanged. *)

val map_max : ('max -> 'max) -> ('min, 'max) t -> ('min, 'max) t
(** [map_max f min_max] applies [f] to the max value only, preserving the min
    value unchanged. *)
