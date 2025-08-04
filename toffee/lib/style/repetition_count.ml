(* The first argument to a repeated track definition. 
    This type represents the type of automatic repetition to perform.
    
    See https://www.w3.org/TR/css-grid-1/#auto-repeat for an explanation 
    of how auto-repeated track definitions work and the difference between 
    Auto_fit and Auto_fill. *)

type t =
  | Count of int (* The specified tracks should be repeated exactly N times *)
  | Auto_fill
    (* Auto-repeating tracks should be generated to fill the container *)
  | Auto_fit
(* Auto-repeating tracks should be generated to fit the container *)

let count n = Count n
let auto_fill = Auto_fill
let auto_fit = Auto_fit

let to_string = function
  | Count n -> string_of_int n
  | Auto_fill -> "auto-fill"
  | Auto_fit -> "auto-fit"

let equal a b =
  match (a, b) with
  | Count n1, Count n2 -> n1 = n2
  | Auto_fill, Auto_fill -> true
  | Auto_fit, Auto_fit -> true
  | _ -> false

let compare a b =
  match (a, b) with
  | Count n1, Count n2 -> Int.compare n1 n2
  | Count _, _ -> -1
  | _, Count _ -> 1
  | Auto_fill, Auto_fill -> 0
  | Auto_fill, _ -> -1
  | _, Auto_fill -> 1
  | Auto_fit, Auto_fit -> 0

let pp fmt t = Format.pp_print_string fmt (to_string t)
let is_auto = function Auto_fill | Auto_fit -> true | Count _ -> false

let of_string = function
  | "auto-fit" -> Ok Auto_fit
  | "auto-fill" -> Ok Auto_fill
  | s -> (
      match int_of_string_opt s with
      | Some n when n > 0 -> Ok (Count n)
      | _ -> Error "Invalid repetition count")
