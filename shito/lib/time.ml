(** Simple time implementation for shito without Core dependency *)

type t = float  (* Unix timestamp in seconds *)

module Span = struct
  type t = float  (* Duration in seconds *)

  let of_sec x = x
  let to_sec x = x
  let of_ms x = x /. 1000.
  let to_ms x = x *. 1000.
  let of_us x = x /. 1_000_000.
  let to_us x = x *. 1_000_000.
  let of_ns x = x /. 1_000_000_000.
  let to_ns x = x *. 1_000_000_000.

  let zero = 0.
  let ( + ) = ( +. )
  let ( - ) = ( -. )
  let ( * ) = ( *. )
  let ( / ) = ( /. )
  let ( < ) = ( < )
  let ( > ) = ( > )
  let ( <= ) = ( <= )
  let ( >= ) = ( >= )
  let ( = ) = ( = )
  let abs = abs_float
  let neg x = -. x
  let min = min
  let max = max
  
  let to_string_hum span =
    if span < 0.001 then Printf.sprintf "%.3fus" (span *. 1_000_000.)
    else if span < 1. then Printf.sprintf "%.3fms" (span *. 1000.)
    else if span < 60. then Printf.sprintf "%.3fs" span
    else if span < 3600. then Printf.sprintf "%.1fm" (span /. 60.)
    else Printf.sprintf "%.1fh" (span /. 3600.)
end

let epoch = 0.  (* Unix epoch *)
let now () = Unix.gettimeofday ()

let of_float x = x
let to_float x = x

let add t span = t +. span
let sub t span = t -. span
let diff t1 t2 = t1 -. t2

let ( + ) = add
let ( - ) = sub
let ( < ) = ( < )
let ( > ) = ( > )
let ( <= ) = ( <= )
let ( >= ) = ( >= )
let ( = ) = ( = )

let min = min
let max = max

let to_string_iso8601 t =
  let tm = Unix.gmtime t in
  let year = Stdlib.(tm.Unix.tm_year + 1900) in
  let month = Stdlib.(tm.Unix.tm_mon + 1) in
  let millis = int_of_float ((t -. floor t) *. 1000.) in
  Printf.sprintf "%04d-%02d-%02dT%02d:%02d:%02d.%03dZ"
    year
    month
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec
    millis

let to_string = to_string_iso8601

let next_multiple ~base ~after ~interval =
  if after <= base then
    base +. interval
  else
    let elapsed = after -. base in
    let n = ceil (elapsed /. interval) in
    base +. (n *. interval)