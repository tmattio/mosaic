(** The matrix side of the comparison.

    This is the only module that uses matrix's VTE API, so that API changes (the
    return type of [Vte.feed], the grid's accessors) are absorbed here. *)

type t

val create :
  ?width_method:Matrix_text.width_method -> rows:int -> cols:int -> unit -> t
(** [create ~rows ~cols ()] is a terminal in its power-up state, measuring
    characters with [width_method] (default [`Unicode], matrix's default). Cells
    without an explicit colour must report {!Screen.Default}. *)

val feed : t -> string -> string
(** [feed t bytes] processes [bytes] and is the replies they caused, in order
    ([""] when none). Bytes may be fed in any pieces: the result does not depend
    on the split. *)

val screen : t -> Screen.t
(** [screen t] is the visible screen. Rows are always ['5'] (single width):
    matrix has no line attributes. *)

val cell : t -> row:int -> col:int -> Screen.cell
(** [cell t ~row ~col] is a cell of {!screen}. *)

type cursor = {
  row : int;
  col : int;
  last_column : bool;  (** The Last Column Flag: the next character wraps. *)
  auto_wrap : bool;
  reverse_wrap : bool;
  insert : bool;
  rows : int;
  cols : int;
  alternate : bool;  (** The alternate screen is in use. *)
}

val cursor : t -> cursor
(** [cursor t] is the cursor state that decides where the next character goes.
*)

val protected : t -> row:int -> col:int -> bool
(** [protected t ~row ~col] is [true] iff the cell is protected (DECSCA). *)

val tag : ?protected:bool -> t -> text:string -> (int * int) list -> unit
(** [tag t ~text cells] writes [text], a character of width 1, in each
    [(row, col)] of [cells], protected as the cell was unless [protected] says.
    It breaks the rule that only the terminal writes its cells: use it on a
    terminal kept to follow cells as they move, never on one compared. *)

val tagged : t -> text:string -> (int * int) list
(** [tagged t ~text] are the cells whose text starts with [text]. *)

val modes : t -> Vte.modes
(** [modes t] are the terminal's modes. *)
