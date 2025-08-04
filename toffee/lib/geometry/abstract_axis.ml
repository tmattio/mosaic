type t = Inline | Block

let other = function Inline -> Block | Block -> Inline

let to_absolute_naive = function
  | Inline -> Absolute_axis.Horizontal
  | Block -> Absolute_axis.Vertical
