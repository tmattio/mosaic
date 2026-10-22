type button = Left | Middle | Right | Extra of int

type kind =
  | Press of button
  | Release of button option
  | Motion of button option
  | Scroll of [ `Up | `Down | `Left | `Right ]

type t = {
  kind : kind;
  x : int;
  y : int;
  shift : bool;
  alt : bool;
  ctrl : bool;
}

let pp_button ppf = function
  | Left -> Format.pp_print_string ppf "Left"
  | Middle -> Format.pp_print_string ppf "Middle"
  | Right -> Format.pp_print_string ppf "Right"
  | Extra n -> Format.fprintf ppf "Extra %d" n

let pp_held ppf = function
  | None -> Format.pp_print_string ppf "None"
  | Some b -> pp_button ppf b

let pp_kind ppf = function
  | Press b -> Format.fprintf ppf "Press %a" pp_button b
  | Release b -> Format.fprintf ppf "Release %a" pp_held b
  | Motion b -> Format.fprintf ppf "Motion %a" pp_held b
  | Scroll d ->
      Format.fprintf ppf "Scroll %s"
        (match d with
        | `Up -> "Up"
        | `Down -> "Down"
        | `Left -> "Left"
        | `Right -> "Right")

let pp ppf m =
  Format.fprintf ppf "%a at %d,%d" pp_kind m.kind m.x m.y;
  if m.shift then Format.pp_print_string ppf " shift";
  if m.alt then Format.pp_print_string ppf " alt";
  if m.ctrl then Format.pp_print_string ppf " ctrl"
