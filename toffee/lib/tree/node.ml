module Node_id = struct
  type t = int64

  let new_ value = value
  let of_int64 id = id
  let to_int64 id = id
  let of_int n = Int64.of_int n
  let to_int id = Int64.to_int id
  let equal = Int64.equal
  let compare = Int64.compare
  let hash = Int64.hash
  let pp fmt id = Format.fprintf fmt "%Ld" id
  let show id = Int64.to_string id
end

type t = Node_id.t
