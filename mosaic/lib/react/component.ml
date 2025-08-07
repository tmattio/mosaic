module type S = sig
  type props

  val key : props -> string option
  val equal : props -> props -> bool option
  val render : props -> Ui.element
end

type 'p t = Pack : (module S with type props = 'p) * 'p -> 'p t

let make m p = Pack (m, p)
