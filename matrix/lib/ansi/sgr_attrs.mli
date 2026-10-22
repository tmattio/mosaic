(** Shared attribute transitions. Masks use {!Attr.pack}; emitted codes omit the
    SGR introducer and terminator. *)

val added : prev:int -> attrs:int -> int
(** [added ~prev ~attrs] contains newly enabled attributes and survivors of
    shared disable codes that must be re-enabled. *)

val emit_enabled : Writer.t -> bool -> int -> bool

val emit_disabled : Writer.t -> bool -> int -> bool
(** [emit_enabled w first attrs] and [emit_disabled w first attrs] append enable
    or disable codes. [first] is true before the first SGR parameter; the result
    is its updated value. *)

val iter_enabled : (int -> unit) -> int -> unit

val iter_disabled : (int -> unit) -> int -> unit
(** [iter_enabled f attrs] and [iter_disabled f attrs] pass the corresponding
    codes to [f], in emission order. *)

val enabled_by : int -> int

val disabled_by : int -> int
(** [enabled_by code] and [disabled_by code] are the attributes SGR [code]
    enables or disables, 0 for none: the inverse of {!iter_enabled} and
    {!iter_disabled}, so reading and writing share one table. [disabled_by 22]
    is bold and dim. *)
