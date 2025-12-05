type flag =
  | Bold
  | Dim
  | Italic
  | Underline
  | Double_underline
  | Blink
  | Inverse
  | Hidden
  | Strikethrough
  | Overline
  | Framed
  | Encircled

let all_flags =
  [
    Bold;
    Dim;
    Italic;
    Underline;
    Double_underline;
    Blink;
    Inverse;
    Hidden;
    Strikethrough;
    Overline;
    Framed;
    Encircled;
  ]

let bit = function
  | Bold -> 0
  | Dim -> 1
  | Italic -> 2
  | Underline -> 3
  | Blink -> 4
  | Inverse -> 5
  | Hidden -> 6
  | Strikethrough -> 7
  | Double_underline -> 8
  | Overline -> 9
  | Framed -> 10
  | Encircled -> 11

let flag_mask flag = 1 lsl bit flag

type t = int

let empty = 0
let bold = flag_mask Bold
let dim = flag_mask Dim
let italic = flag_mask Italic
let underline = flag_mask Underline
let double_underline = flag_mask Double_underline
let blink = flag_mask Blink
let inverse = flag_mask Inverse
let hidden = flag_mask Hidden
let strikethrough = flag_mask Strikethrough
let overline = flag_mask Overline
let framed = flag_mask Framed
let encircled = flag_mask Encircled
let is_empty m = m = 0
let mem flag m = m land flag_mask flag <> 0
let add flag m = m lor flag_mask flag
let remove flag m = m land lnot (flag_mask flag)
let toggle flag m = m lxor flag_mask flag
let union a b = a lor b
let intersect a b = a land b
let diff a b = a land lnot b
let of_list flags = List.fold_left (fun acc flag -> add flag acc) empty flags

let to_list mask =
  List.fold_right
    (fun flag acc -> if mem flag mask then flag :: acc else acc)
    all_flags []

let cardinal mask =
  let rec count n c = if n = 0 then c else count (n lsr 1) (c + (n land 1)) in
  count mask 0

let combine ?(bold = false) ?(dim = false) ?(italic = false)
    ?(underline = false) ?(double_underline = false) ?(blink = false)
    ?(inverse = false) ?(hidden = false) ?(strikethrough = false)
    ?(overline = false) ?(framed = false) ?(encircled = false) () =
  let set cond flag acc = if cond then add flag acc else acc in
  empty |> set bold Bold |> set dim Dim |> set italic Italic
  |> set underline Underline |> set blink Blink |> set inverse Inverse
  |> set hidden Hidden
  |> set strikethrough Strikethrough
  |> set overline Overline |> set framed Framed |> set encircled Encircled
  |> set double_underline Double_underline

let to_sgr_codes mask =
  let rec collect acc = function
    | [] -> List.rev acc
    | flag :: rest ->
        let acc =
          if mem flag mask then
            let code =
              match flag with
              | Bold -> 1
              | Dim -> 2
              | Italic -> 3
              | Underline -> 4
              | Double_underline -> 21
              | Blink -> 5
              | Inverse -> 7
              | Hidden -> 8
              | Strikethrough -> 9
              | Overline -> 53
              | Framed -> 51
              | Encircled -> 52
            in
            code :: acc
          else acc
        in
        collect acc rest
  in
  collect [] all_flags

let pp fmt mask =
  Format.fprintf fmt "[%a]"
    (Format.pp_print_list
       ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
       (fun fmt flag ->
         let s =
           match flag with
           | Bold -> "Bold"
           | Dim -> "Dim"
           | Italic -> "Italic"
           | Underline -> "Underline"
           | Double_underline -> "Double_underline"
           | Blink -> "Blink"
           | Inverse -> "Inverse"
           | Hidden -> "Hidden"
           | Strikethrough -> "Strikethrough"
           | Overline -> "Overline"
           | Framed -> "Framed"
           | Encircled -> "Encircled"
         in
         Format.pp_print_string fmt s))
    (to_list mask)

let pack mask = mask
let unpack mask = mask

let fold f mask init =
  List.fold_left
    (fun acc flag -> if mem flag mask then f flag acc else acc)
    init all_flags

let iter f mask = fold (fun flag () -> f flag) mask () |> ignore

let with_flag flag enabled mask =
  if enabled then add flag mask else remove flag mask

let compare a b = Int.compare a b
