type ('a, 'b) t = Left of 'a | Right of 'b

let is_left = function Left _ -> true | Right _ -> false
let is_right = function Left _ -> false | Right _ -> true
let map_left f = function Left a -> Left (f a) | Right b -> Right b
let map_right f = function Left a -> Left a | Right b -> Right (f b)
let bimap fl fr = function Left a -> Left (fl a) | Right b -> Right (fr b)
let fold ~left ~right = function Left a -> left a | Right b -> right b
let swap = function Left a -> Right a | Right b -> Left b
let to_result = function Left a -> Ok a | Right b -> Error b
let of_result = function Ok a -> Left a | Error b -> Right b
let to_option_left = function Left a -> Some a | Right _ -> None
let to_option_right = function Left _ -> None | Right b -> Some b

let pp ~pp_left ~pp_right fmt = function
  | Left a ->
      Format.fprintf fmt "Left(";
      pp_left fmt a;
      Format.fprintf fmt ")"
  | Right b ->
      Format.fprintf fmt "Right(";
      pp_right fmt b;
      Format.fprintf fmt ")"
