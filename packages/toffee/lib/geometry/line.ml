type 'a t = { start : 'a; end_ : 'a }

(* Constants *)

let both_true = { start = true; end_ = true }
let both_false = { start = false; end_ = false }

(* Creation *)

let make start end_ = { start; end_ }

(* Mapping *)

let map f line = { start = f line.start; end_ = f line.end_ }

(* Arithmetic for numeric lines *)

let sum line = line.start +. line.end_

(* Mapping for two-argument functions *)

let map2 f line1 line2 =
  { start = f line1.start line2.start; end_ = f line1.end_ line2.end_ }

(* Comparison and string functions *)

let compare cmp a b =
  let cmp_start = cmp a.start b.start in
  if cmp_start <> 0 then cmp_start else cmp a.end_ b.end_

let equal eq a b = eq a.start b.start && eq a.end_ b.end_

let to_string f line =
  Printf.sprintf "{ start: %s; end: %s }" (f line.start) (f line.end_)

let pp f fmt line =
  Format.fprintf fmt "{ start: %a; end: %a }" f line.start f line.end_
