type t = {
  mutable width : int;
  mutable height : int;
  mutable capacity : int;
  mutable data :
    (int32, Bigarray.int32_elt, Bigarray.c_layout) Bigarray.Array1.t;
}

let empty_id = 0

let ensure_capacity t cells =
  if cells > t.capacity then (
    (* Geometric growth to reduce allocation churn on frequent resizes *)
    let new_capacity = max cells (t.capacity * 2) in
    t.data <-
      Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout new_capacity;
    t.capacity <- new_capacity)

let linear_fill arr start len value =
  let limit = start + len in
  let rec loop i =
    if i < limit then (
      Bigarray.Array1.unsafe_set arr i value;
      loop (i + 1))
  in
  loop start

let clear t =
  let len = t.width * t.height in
  if len > 0 then linear_fill t.data 0 len 0l

let resize t ~width ~height =
  let width = max 0 width in
  let height = max 0 height in
  let cells = width * height in
  ensure_capacity t cells;
  t.width <- width;
  t.height <- height;
  (* Always clear after resize to ensure deterministic state *)
  clear t

let create ~width ~height =
  let t =
    {
      width = 0;
      height = 0;
      capacity = 0;
      data = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 0;
    }
  in
  resize t ~width ~height;
  t

let clip t ~x ~y ~width ~height =
  let x0 = max 0 x in
  let y0 = max 0 y in
  let x1 = min t.width (x + width) in
  let y1 = min t.height (y + height) in
  (x0, y0, x1, y1)

let add t ~x ~y ~width ~height ~id =
  let x0, y0, x1, y1 = clip t ~x ~y ~width ~height in
  if x0 < x1 && y0 < y1 then
    let stride = t.width in
    let id32 = Int32.of_int id in
    let row_width = x1 - x0 in
    let rec loop_rows row =
      if row < y1 then (
        let start = (row * stride) + x0 in
        linear_fill t.data start row_width id32;
        loop_rows (row + 1))
    in
    loop_rows y0

let get t ~x ~y =
  if x < 0 || y < 0 || x >= t.width || y >= t.height then empty_id
  else
    let idx = (y * t.width) + x in
    Int32.to_int (Bigarray.Array1.unsafe_get t.data idx)

let blit ~src ~dst =
  resize dst ~width:src.width ~height:src.height;
  let len = src.width * src.height in
  (* Manual copy to avoid Bigarray.Array1.sub view allocations *)
  for i = 0 to len - 1 do
    Bigarray.Array1.unsafe_set dst.data i
      (Bigarray.Array1.unsafe_get src.data i)
  done
