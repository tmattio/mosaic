(** First-class component with statically-known props. *)

module type S = sig
  type props

  val key : props -> string option
  (** Optional diff-key; [None] means “key = position”. *)

  val equal : props -> props -> bool option
  (** If [Some false] → always re-render. If [None] → assume unequal
      (conservative). *)

  val render : props -> Ui.element
end

type 'p t = Pack : (module S with type props = 'p) * 'p -> 'p t

val make : (module S with type props = 'p) -> 'p -> 'p t
