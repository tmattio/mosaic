module type S = sig
  type props

  val make : props -> Ui.element
end

type 'props t = (module S with type props = 'props)

let make (type props) (module C : S with type props = props) : props t =
  (module C)

(* For existential wrapping *)
type any_component = Any : 'props t * 'props -> any_component
