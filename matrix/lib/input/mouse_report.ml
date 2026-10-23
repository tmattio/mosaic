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

(* Encoding *)

type tracking = [ `X10 | `Normal | `Button | `Any ]
type format = [ `X10 | `Sgr | `Urxvt ]
type encoding = { tracking : tracking; format : format }

let reported tracking = function
  | Press (Left | Middle | Right) -> true
  | (Press (Extra _) | Release _ | Scroll _) when tracking <> `X10 -> true
  | Motion (Some _) -> tracking = `Button || tracking = `Any
  | Motion None -> tracking = `Any
  | _ -> false

let button_code = function
  | Left -> 0
  | Middle -> 1
  | Right -> 2
  | Extra n when n >= 8 && n <= 11 -> 128 + n - 8
  | Extra n ->
      invalid_arg
        (Printf.sprintf "Mouse_report.encode: no button %d has a code" n)

let encode e m =
  if m.x < 0 || m.y < 0 then
    invalid_arg "Mouse_report.encode: a negative coordinate";
  if not (reported e.tracking m.kind) then ""
  else
    let sgr = e.format = `Sgr in
    let low =
      match m.kind with
      | Press b -> button_code b
      | Release (Some b) when sgr -> button_code b
      | Release _ -> 3
      | Motion b -> 32 + Option.fold ~none:3 ~some:button_code b
      | Scroll d -> (
          64 + match d with `Up -> 0 | `Down -> 1 | `Left -> 2 | `Right -> 3)
    in
    let mods =
      if e.tracking = `X10 then 0
      else
        (if m.shift then 4 else 0)
        lor (if m.alt then 8 else 0)
        lor if m.ctrl then 16 else 0
    in
    let cb = low lor mods in
    match e.format with
    | `Sgr ->
        Printf.sprintf "\027[<%d;%d;%d%c" cb (m.x + 1) (m.y + 1)
          (match m.kind with Release _ -> 'm' | _ -> 'M')
    | `Urxvt -> Printf.sprintf "\027[%d;%d;%dM" (cb + 32) (m.x + 1) (m.y + 1)
    | `X10 ->
        if m.x + 33 > 255 || m.y + 33 > 255 then ""
        else
          Printf.sprintf "\027[M%c%c%c"
            (Char.chr (cb + 32))
            (Char.chr (m.x + 33))
            (Char.chr (m.y + 33))
