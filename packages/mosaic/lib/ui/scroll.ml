type axis = { position : int; edge : bool option; reveal : int option }
type t = { x : axis; y : axis }

let axis position = { position = max 0 position; edge = None; reveal = None }
let at ?(x = 0) y = { x = axis x; y = axis y }
let equal a b = a = b

let follow edge t =
  let pin ending a = { a with edge = Some ending; reveal = None } in
  match edge with
  | `Top -> { t with y = pin false t.y }
  | `Bottom -> { t with y = pin true t.y }
  | `Left -> { t with x = pin false t.x }
  | `Right -> { t with x = pin true t.x }

let reveal ?column ~line t =
  let y = { t.y with reveal = Some (max 0 line) } in
  let x =
    match column with
    | None -> t.x
    | Some x -> { t.x with reveal = Some (max 0 x) }
  in
  { x; y }

let with_position ?x ?y t =
  {
    x = (match x with None -> t.x | Some x -> axis x);
    y = (match y with None -> t.y | Some y -> axis y);
  }

let resolve_axis a viewport content =
  let maximum = max 0 (content - viewport) in
  let position =
    match a.edge with
    | None -> a.position
    | Some false -> 0
    | Some true -> maximum
  in
  let position = max 0 (min maximum position) in
  let position =
    match a.reveal with
    | None -> position
    | Some target ->
        if target < position then target
        else if target >= position + viewport then target - max 1 viewport + 1
        else position
  in
  max 0 (min maximum position)

let resolve t ~width ~height ~content_width ~content_height =
  (resolve_axis t.x width content_width, resolve_axis t.y height content_height)
