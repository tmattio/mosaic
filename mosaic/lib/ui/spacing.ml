type t = { top : int; right : int; bottom : int; left : int }

let make ?(top = 0) ?(right = 0) ?(bottom = 0) ?(left = 0) () =
  {
    top = max 0 top;
    right = max 0 right;
    bottom = max 0 bottom;
    left = max 0 left;
  }

let no_spacing = make ()
let all n = { top = n; right = n; bottom = n; left = n }
let xy x y = { top = y; right = x; bottom = y; left = x }
let top p = p.top
let right p = p.right
let bottom p = p.bottom
let left p = p.left
