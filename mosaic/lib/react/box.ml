type 'a id = 'a Type.Id.t
type t = Pack : 'a id * 'a -> t

let pack id v = Pack (id, v)

let unpack : type b. b id -> t -> b option =
 fun id' (Pack (id, v)) ->
  match Type.Id.provably_equal id id' with
  | Some Type.Equal -> Some v
  | None -> None
