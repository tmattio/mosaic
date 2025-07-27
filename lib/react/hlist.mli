(** Heterogeneous value lists. Based on https://github.com/dbuenzli/hmap. *)

(** {1:keys Keys} *)

type 'a key
(** The type for keys whose associated value is of type ['a]. *)

(** Keys. *)
module Key : sig
  (** {1:keys Keys} *)

  val create : unit -> 'a key
  (** [create ()] is a new key. *)

  (** {1:exists Existential keys}

      Existential keys allow comparing keys without knowing their type. This can
      be useful for functions like {!filter} or {!mem}. *)

  type t
  (** The type for existential keys. *)

  val hide_type : 'a key -> t
  (** [hide_type k] is an existential key for [k]. *)

  val equal : t -> t -> bool
  (** [equal k k'] is [true] iff [k] and [k'] are the same key. *)

  val compare : t -> t -> int
  (** [compare k k'] is a total order on keys compatible with {!equal}. *)
end

(** {1:lists Lists} *)

type t
(** The type for heterogeneous value lists. *)

val empty : t
(** [empty] is the empty list. *)

val is_empty : t -> bool
(** [is_empty l] is [true] iff [l] is empty. *)

val cons : 'a key -> 'a -> t -> t
(** [cons k v l] is a new list with the binding of [k] to [v] at its head,
    followed by [l]. *)

type binding =
  | B : 'a key * 'a -> binding
      (** The type for bindings, which are the elements of a heterogeneous list.
      *)

val hd : t -> binding option
(** [hd l] is the first binding of the list [l], or [None] if [l] is empty. *)

val tl : t -> t
(** [tl l] is the list [l] without its first binding.
    @raise Invalid_argument if the list is empty. *)

val find : 'a key -> t -> 'a option
(** [find k l] is the value of the first binding with key [k] in [l], if any. *)

val get : 'a key -> t -> 'a
(** [get k l] is the value of the first binding with key [k] in [l].
    @raise Not_found if [k] is not bound in [l]. *)

val mem : 'a key -> t -> bool
(** [mem k l] is [true] iff key [k] is present in [l]. *)

val rem : 'a key -> t -> t
(** [rem k l] returns a list with all bindings for key [k] removed. *)

val length : t -> int
(** [length l] is the number of bindings in [l]. *)

val iter : (binding -> unit) -> t -> unit
(** [iter f l] applies [f] to all bindings of [l], in order. *)

val fold_left : ('a -> binding -> 'a) -> 'a -> t -> 'a
(** [fold_left f acc l] folds over the bindings of [l] with [f], starting with
    [acc]. *)

val for_all : (binding -> bool) -> t -> bool
(** [for_all p l] is [true] iff all bindings of [l] satisfy [p]. *)

val exists : (binding -> bool) -> t -> bool
(** [exists p l] is [true] iff there exists a binding in [l] that satisfies [p].
*)

val filter : (binding -> bool) -> t -> t
(** [filter p l] are the bindings of [l] that satisfy [p]. *)

val rev : t -> t
(** [rev l] reverses the list of bindings [l]. *)

(** {1:func Functorial interface}

    The functorial interface allows associating more information to the keys,
    for example a key name or a value pretty-printer. *)

(** The type for key information. *)
module type KEY_INFO = sig
  type 'a t
  (** The type for key information. *)
end

(** Output signature of the functor {!Make} *)
module type S = sig
  (** {1:keys Keys} *)

  type 'a key
  (** The type for keys whose lookup value is of type ['a]. *)

  module Key : sig
    type 'a info

    val create : 'a info -> 'a key
    val info : 'a key -> 'a info

    type t

    val hide_type : 'a key -> t
    val equal : t -> t -> bool
    val compare : t -> t -> int
  end

  (** {1:lists Lists} *)

  type t

  val empty : t
  val is_empty : t -> bool
  val cons : 'a key -> 'a -> t -> t

  type binding = B : 'a key * 'a -> binding

  val hd : t -> binding option
  val tl : t -> t
  val find : 'a key -> t -> 'a option
  val get : 'a key -> t -> 'a
  val mem : 'a key -> t -> bool
  val rem : 'a key -> t -> t
  val length : t -> int
  val iter : (binding -> unit) -> t -> unit
  val fold_left : ('a -> binding -> 'a) -> 'a -> t -> 'a
  val for_all : (binding -> bool) -> t -> bool
  val exists : (binding -> bool) -> t -> bool
  val filter : (binding -> bool) -> t -> t
  val rev : t -> t
end

(** Functor for heterogeneous lists whose keys hold information of type
    [Key_info.t] *)
module Make : functor (Key_info : KEY_INFO) ->
  S with type 'a Key.info = 'a Key_info.t
