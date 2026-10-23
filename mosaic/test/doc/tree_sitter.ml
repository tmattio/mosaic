type point = { row : int; column : int }

module Language = struct
  type t = unit

  let stub = ()
end

module Tree = struct
  type t = unit

  let copy () = ()

  let edit () ~start_byte:_ ~old_end_byte:_ ~new_end_byte:_ ~start_point:_
      ~old_end_point:_ ~new_end_point:_ =
    ()
end

module Parser = struct
  type t = unit

  let create () = ()
  let parse_string ?old:_ () _ = ()
end

module Query = struct
  type t = unit

  let create () ~source:_ = ()
end

let highlight () () = []
