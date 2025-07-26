type t = { top : int; right : int; bottom : int; left : int }

let make ?(top = 0) ?(right = 0) ?(bottom = 0) ?(left = 0) () =
  {
    top = max 0 top;
    right = max 0 right;
    bottom = max 0 bottom;
    left = max 0 left;
  }

let no_padding = make ()
let all n = { top = n; right = n; bottom = n; left = n }
let xy x y = { top = y; right = x; bottom = y; left = x }
let top p = p.top
let right p = p.right
let bottom p = p.bottom
let left p = p.left

let pad ?all ?x ?y ?top ?right ?bottom ?left () =
  let base_val = Option.value all ~default:0 in
  let h_val = Option.value x ~default:base_val in
  let v_val = Option.value y ~default:base_val in

  {
    top = max 0 (Option.value top ~default:v_val);
    right = max 0 (Option.value right ~default:h_val);
    bottom = max 0 (Option.value bottom ~default:v_val);
    left = max 0 (Option.value left ~default:h_val);
  }
