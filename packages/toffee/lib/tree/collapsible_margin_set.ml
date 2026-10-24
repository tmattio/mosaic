type t = { positive : float; negative : float }

let zero = { positive = 0.0; negative = 0.0 }

let from_margin margin =
  if margin >= 0.0 then { positive = margin; negative = 0.0 }
  else { positive = 0.0; negative = margin }

let collapse_with_margin t margin =
  if margin >= 0.0 then { t with positive = Float.max t.positive margin }
  else { t with negative = Float.min t.negative margin }

let collapse_with_set t other =
  {
    positive = Float.max t.positive other.positive;
    negative = Float.min t.negative other.negative;
  }

let resolve t = t.positive +. t.negative

let to_string t =
  Printf.sprintf "CollapsibleMarginSet { positive: %g; negative: %g }"
    t.positive t.negative

let compare a b =
  let cmp_pos = Float.compare a.positive b.positive in
  if cmp_pos <> 0 then cmp_pos else Float.compare a.negative b.negative

let equal a b =
  Float.equal a.positive b.positive && Float.equal a.negative b.negative

let pp fmt t = Format.pp_print_string fmt (to_string t)
