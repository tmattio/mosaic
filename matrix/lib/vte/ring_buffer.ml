(** Ring buffer for bounded scrollback *)

type 'a t = {
  buffer : 'a array;
  mutable head : int;
  mutable size : int;
  capacity : int;
}

let create capacity default =
  if capacity <= 0 then
    invalid_arg "Ring_buffer.create: capacity must be positive";
  { buffer = Array.make capacity default; head = 0; size = 0; capacity }

let push t item =
  t.buffer.(t.head) <- item;
  t.head <- (t.head + 1) mod t.capacity;
  if t.size < t.capacity then t.size <- t.size + 1

let size t = t.size
let capacity t = t.capacity
let is_empty t = t.size = 0
let is_full t = t.size = t.capacity

let clear t =
  t.head <- 0;
  t.size <- 0

let get t idx =
  if idx < 0 || idx >= t.size then
    invalid_arg "Ring_buffer.get: index out of bounds";
  let real_idx =
    if t.size < t.capacity then idx (* Buffer not wrapped yet *)
    else (t.head + idx) mod t.capacity (* Wrapped buffer *)
  in
  t.buffer.(real_idx)

let to_list t =
  let result = ref [] in
  let start = if t.size < t.capacity then 0 else t.head in
  for i = 0 to t.size - 1 do
    let idx = (start + i) mod t.capacity in
    result := t.buffer.(idx) :: !result
  done;
  List.rev !result

let iter f t =
  let start = if t.size < t.capacity then 0 else t.head in
  for i = 0 to t.size - 1 do
    let idx = (start + i) mod t.capacity in
    f t.buffer.(idx)
  done

let fold f init t =
  let acc = ref init in
  iter (fun x -> acc := f !acc x) t;
  !acc
