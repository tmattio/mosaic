(** The parsing interface of the tree-sitter bindings' [tree-sitter] library,
    without the runtime, so the guide's recipe typechecks without depending on
    it. Each declaration is the bindings' own. *)

type point = { row : int; column : int }

module Language : sig
  type t

  val stub : t
  (** [stub] is what the stubbed grammars return; the bindings have no such
      value. *)
end

module Tree : sig
  type t

  val copy : t -> t

  val edit :
    t ->
    start_byte:int ->
    old_end_byte:int ->
    new_end_byte:int ->
    start_point:point ->
    old_end_point:point ->
    new_end_point:point ->
    unit
end

module Parser : sig
  type t

  val create : Language.t -> t
  val parse_string : ?old:Tree.t -> t -> string -> Tree.t
end

module Query : sig
  type t

  val create : Language.t -> source:string -> t
end

val highlight : Query.t -> Tree.t -> (int * int * string) list
