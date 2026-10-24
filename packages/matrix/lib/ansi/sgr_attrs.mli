(** The SGR codes of attribute changes, shared by {!Style} and {!Sgr_state}.

    Sets of attributes are packed by {!Attr.pack}. The codes are written without
    the SGR introducer and terminator. *)

val added : prev:int -> attrs:int -> int
(** [added ~prev ~attrs] is the attributes to set when the terminal changes from
    [prev] to [attrs]. These are the attributes of [attrs] that [prev] lacks,
    and those of [attrs] whose clearing code clears an attribute of [prev] that
    [attrs] lacks. *)

val emit_enabled : Writer.t -> bool -> int -> bool

val emit_disabled : Writer.t -> bool -> int -> bool
(** [emit_enabled w first attrs] and [emit_disabled w first attrs] write the
    codes that set or clear [attrs], with [;] between them. [first] is [true]
    iff no code of the sequence is written yet, and the result is its value
    after the call. *)

val iter_enabled : (int -> unit) -> int -> unit

val iter_disabled : (int -> unit) -> int -> unit
(** [iter_enabled f attrs] and [iter_disabled f attrs] call [f] on the codes
    that set or clear [attrs], in the order {!emit_enabled} and {!emit_disabled}
    write them. *)

val enabled_by : int -> int

val disabled_by : int -> int
(** [enabled_by code] and [disabled_by code] are the attributes SGR [code] sets
    or clears, and 0 for none. They invert {!iter_enabled} and {!iter_disabled},
    so reading and writing share one table. [disabled_by 22] is bold and dim. *)
