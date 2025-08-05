(* A unit of linear measurement, which can also be 'auto'. *)

type t = Compact_length.t

(* Constructors *)

let length value = Compact_length.length value
let percent value = Compact_length.percent value
let auto = Compact_length.auto
let calc index = Compact_length.calc index

(* Constants *)

let zero = Compact_length.zero

(* Inspection *)

let is_length t = Compact_length.is_length t
let is_percent t = Compact_length.is_percent t
let is_auto t = Compact_length.is_auto t
let is_calc t = Compact_length.is_calc t

(* Value extraction *)

let value t = Compact_length.value t

(* Conversion/Resolution *)

let resolve_to_option t context =
  if is_auto t then None
  else if is_length t then Some (value t)
  else if is_percent t then Some (context *. value t)
  else failwith "Invalid length_percentage_auto value (possibly calc)"

let resolve_to_option_with_calc t context calc_resolver =
  if is_auto t then None
  else if is_length t then Some (value t)
  else if is_percent t then Some (context *. value t)
  else if is_calc t then
    Some (calc_resolver (Compact_length.get_calc_index t) context)
  else failwith "Invalid length_percentage_auto value"

(* Pretty printing *)

let to_string = Compact_length.to_string

(* Comparison *)

let equal = Compact_length.equal
let compare = Compact_length.compare
let pp = Compact_length.pp

(* Additional helpers *)

let uses_percentage t = Compact_length.uses_percentage t

let resolved_percentage_size t parent_size =
  Compact_length.resolved_percentage_size t parent_size

let resolved_percentage_size_with_calc t parent_size calc_resolver =
  Compact_length.resolved_percentage_size_with_calc t parent_size calc_resolver

(* Resolve functions from Taffy's resolve.rs *)

let maybe_resolve t context calc_resolver =
  if is_auto t then None
  else if is_length t then Some (value t)
  else if is_percent t then
    match context with None -> None | Some dim -> Some (dim *. value t)
  else if is_calc t then
    match context with
    | None -> None
    | Some dim -> Some (calc_resolver (Compact_length.get_calc_index t) dim)
  else failwith "Invalid length_percentage_auto value"

let resolve_or_zero t context calc_resolver =
  match maybe_resolve t context calc_resolver with Some v -> v | None -> 0.0
