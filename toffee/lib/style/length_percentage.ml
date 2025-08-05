(* A unit of linear measurement, representing either a fixed length or a percentage. *)

type t = Compact_length.t

(* Constructors *)

let length value = Compact_length.length value
let percent value = Compact_length.percent value
let calc index = Compact_length.calc index

(* Constants *)

let zero = Compact_length.zero

(* Inspection *)

let is_length t = Compact_length.is_length t
let is_percent t = Compact_length.is_percent t
let is_calc t = Compact_length.is_calc t

(* Value extraction *)

let value t = Compact_length.value t

(* Conversion/Resolution *)

let resolve t context =
  if is_length t then value t
  else if is_percent t then
    (* Emulate f32 arithmetic to match Taffy's behavior *)
    let result = context *. value t in
    (* Round to f32 precision *)
    Int32.float_of_bits (Int32.bits_of_float result)
  else failwith "Invalid length_percentage value (possibly calc)"

let resolve_with_calc t context calc_resolver =
  if is_length t then value t
  else if is_percent t then
    (* Emulate f32 arithmetic to match Taffy's behavior *)
    let result = context *. value t in
    (* Round to f32 precision *)
    Int32.float_of_bits (Int32.bits_of_float result)
  else if is_calc t then calc_resolver (Compact_length.get_calc_index t) context
  else failwith "Invalid length_percentage value"

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
  if is_length t then Some (value t)
  else if is_percent t then
    match context with
    | None -> None
    | Some dim ->
        (* Emulate f32 arithmetic to match Taffy's behavior *)
        let result = dim *. value t in
        (* Round to f32 precision *)
        Some (Int32.float_of_bits (Int32.bits_of_float result))
  else if is_calc t then
    match context with
    | None -> None
    | Some dim -> Some (calc_resolver (Compact_length.get_calc_index t) dim)
  else failwith "Invalid length_percentage value"

let resolve_or_zero t context calc_resolver =
  match maybe_resolve t context calc_resolver with Some v -> v | None -> 0.0
