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

(* Ordered by bit index for consistency with [bit] function *)
let all_flags =
  [|
    Bold;
    (* bit 0 *)
    Dim;
    (* bit 1 *)
    Italic;
    (* bit 2 *)
    Underline;
    (* bit 3 *)
    Blink;
    (* bit 4 *)
    Inverse;
    (* bit 5 *)
    Hidden;
    (* bit 6 *)
    Strikethrough;
    (* bit 7 *)
    Double_underline;
    (* bit 8 *)
    Overline;
    (* bit 9 *)
    Framed;
    (* bit 10 *)
    Encircled;
    (* bit 11 *)
  |]

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

let flag_to_sgr_code = function
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

(* SGR codes to disable attributes.
   Note: Bold/Dim share 22, Underline/Double_underline share 24,
   Framed/Encircled share 54. *)
let flag_to_sgr_disable_code = function
  | Bold -> 22 (* normal intensity *)
  | Dim -> 22 (* normal intensity *)
  | Italic -> 23
  | Underline -> 24
  | Double_underline -> 24
  | Blink -> 25
  | Inverse -> 27
  | Hidden -> 28
  | Strikethrough -> 29
  | Overline -> 55
  | Framed -> 54
  | Encircled -> 54

let flag_to_string = function
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
  Array.fold_right
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
  |> set underline Underline
  |> set double_underline Double_underline
  |> set blink Blink |> set inverse Inverse |> set hidden Hidden
  |> set strikethrough Strikethrough
  |> set overline Overline |> set framed Framed |> set encircled Encircled

(* Allocates a list - use iter_sgr_codes for allocation-free alternative *)
let to_sgr_codes mask =
  Array.fold_right
    (fun flag acc ->
      if mem flag mask then flag_to_sgr_code flag :: acc else acc)
    all_flags []

let iter_sgr_codes f mask =
  Array.iter
    (fun flag -> if mem flag mask then f (flag_to_sgr_code flag))
    all_flags

(* Unrolled loop to avoid allocations. Deduplicates shared disable codes:
   Bold/Dim share 22, Underline/Double_underline share 24, Framed/Encircled share 54. *)
let iter_sgr_disable_codes f mask =
  (* Code 22: normal intensity (disables Bold and Dim) *)
  if mask land (bold lor dim) <> 0 then f 22;
  (* Code 23: not italic *)
  if mask land italic <> 0 then f 23;
  (* Code 24: not underlined (disables Underline and Double_underline) *)
  if mask land (underline lor double_underline) <> 0 then f 24;
  (* Code 25: not blinking *)
  if mask land blink <> 0 then f 25;
  (* Code 27: not reversed *)
  if mask land inverse <> 0 then f 27;
  (* Code 28: reveal (disables Hidden) *)
  if mask land hidden <> 0 then f 28;
  (* Code 29: not crossed out *)
  if mask land strikethrough <> 0 then f 29;
  (* Code 54: not framed/encircled (disables Framed and Encircled) *)
  if mask land (framed lor encircled) <> 0 then f 54;
  (* Code 55: not overlined *)
  if mask land overline <> 0 then f 55

let fold_sgr_codes f mask init =
  Array.fold_left
    (fun acc flag ->
      if mem flag mask then f (flag_to_sgr_code flag) acc else acc)
    init all_flags

(* Avoids intermediate list allocation by iterating directly *)
let pp fmt mask =
  Format.pp_print_char fmt '[';
  let (_ : bool) =
    Array.fold_left
      (fun first flag ->
        if mem flag mask then begin
          if not first then Format.fprintf fmt ",@ ";
          Format.pp_print_string fmt (flag_to_string flag);
          false
        end
        else first)
      true all_flags
  in
  Format.pp_print_char fmt ']'

let pack mask = mask
let unpack mask = mask land 0xFFF (* clamp to valid 12-bit range *)

let fold f mask init =
  Array.fold_left
    (fun acc flag -> if mem flag mask then f flag acc else acc)
    init all_flags

let iter f mask = fold (fun flag () -> f flag) mask ()

let with_flag flag enabled mask =
  if enabled then add flag mask else remove flag mask

let equal a b = Int.equal a b
let compare a b = Int.compare a b
