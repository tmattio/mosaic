(** The matrix side of the comparison.

    This is the only module that uses matrix's API, so that API changes (the
    return type of [Vte.feed], the parser's tokens, the grid's accessors) are
    absorbed here. *)

type t

val create : rows:int -> cols:int -> t
(** [create ~rows ~cols] is a terminal in its power-up state. Cells without an
    explicit colour must report {!Screen.Default}. *)

val feed : t -> string -> string
(** [feed t bytes] processes [bytes] and is the replies they caused, in order
    ([""] when none). *)

val screen : t -> Screen.t
(** [screen t] is the visible screen. Rows are always ['5'] (single width):
    matrix has no line attributes. *)
