type setting =
  | Not_recognized
  | Set
  | Reset
  | Permanently_set
  | Permanently_reset

type t =
  | Primary_attributes of int list
  | Secondary_attributes of int list
  | Unit_id of string
  | Mode of { dec : bool; mode : int; setting : setting }
  | Cursor of { row : int; col : int }
  | Text_area_pixels of { height : int; width : int }
  | Cell_pixels of { height : int; width : int }
  | Text_area of { rows : int; cols : int }
  | Resize of { rows : int; cols : int; height : int; width : int }
  | Keyboard_flags of int
  | Color_scheme of [ `Dark | `Light ]
  | Version of string
  | Termcap of { valid : bool; entries : (string * string option) list }
  | Color of {
      target : [ `Foreground | `Background | `Cursor | `Palette of int ];
      rgb : int * int * int;
    }
  | Clipboard of { selection : string; data : string }
  | Graphics of { id : int option; message : string }
  | Osc of string
  | Dcs of { header : Ansi.Parser.sequence; data : string }
  | Csi of Ansi.Parser.sequence
  | Ss3 of Ansi.Parser.sequence
  | Cut of [ `Sequence | `String | `Mouse ]
  | Too_long of [ `Dcs | `Osc | `Apc ]

let setting_name = function
  | Not_recognized -> "Not_recognized"
  | Set -> "Set"
  | Reset -> "Reset"
  | Permanently_set -> "Permanently_set"
  | Permanently_reset -> "Permanently_reset"

let pp_ints ppf l =
  Format.pp_print_list
    ~pp_sep:(fun ppf () -> Format.pp_print_char ppf ';')
    Format.pp_print_int ppf l

let pp ppf = function
  | Primary_attributes l -> Format.fprintf ppf "Primary_attributes %a" pp_ints l
  | Secondary_attributes l ->
      Format.fprintf ppf "Secondary_attributes %a" pp_ints l
  | Unit_id s -> Format.fprintf ppf "Unit_id %S" s
  | Mode { dec; mode; setting } ->
      Format.fprintf ppf "Mode %s%d %s"
        (if dec then "?" else "")
        mode (setting_name setting)
  | Cursor { row; col } -> Format.fprintf ppf "Cursor %d,%d" row col
  | Text_area_pixels { height; width } ->
      Format.fprintf ppf "Text_area_pixels %dx%d" width height
  | Cell_pixels { height; width } ->
      Format.fprintf ppf "Cell_pixels %dx%d" width height
  | Text_area { rows; cols } -> Format.fprintf ppf "Text_area %dx%d" cols rows
  | Resize { rows; cols; height; width } ->
      Format.fprintf ppf "Resize %dx%d %dx%d" cols rows width height
  | Keyboard_flags f -> Format.fprintf ppf "Keyboard_flags %d" f
  | Color_scheme `Dark -> Format.pp_print_string ppf "Color_scheme Dark"
  | Color_scheme `Light -> Format.pp_print_string ppf "Color_scheme Light"
  | Version s -> Format.fprintf ppf "Version %S" s
  | Termcap { valid; entries } ->
      Format.fprintf ppf "Termcap%s" (if valid then "" else " invalid");
      List.iter
        (fun (name, value) ->
          match value with
          | None -> Format.fprintf ppf " %S" name
          | Some v -> Format.fprintf ppf " %S=%S" name v)
        entries
  | Color { target; rgb = r, g, b } ->
      let target =
        match target with
        | `Foreground -> "foreground"
        | `Background -> "background"
        | `Cursor -> "cursor"
        | `Palette i -> "palette " ^ string_of_int i
      in
      Format.fprintf ppf "Color %s #%02x%02x%02x" target r g b
  | Clipboard { selection; data } ->
      Format.fprintf ppf "Clipboard %S %S" selection data
  | Graphics { id; message } ->
      Format.fprintf ppf "Graphics%s %S"
        (match id with None -> "" | Some i -> " i=" ^ string_of_int i)
        message
  | Osc s -> Format.fprintf ppf "Osc %S" s
  | Dcs { header; data } ->
      Format.fprintf ppf "Dcs %a %S" Ansi.Parser.pp_token
        (Ansi.Parser.Open (Dcs header)) data
  | Csi s ->
      Format.fprintf ppf "Csi %a" Ansi.Parser.pp_token (Ansi.Parser.Csi s)
  | Ss3 s ->
      Format.fprintf ppf "Ss3 %a" Ansi.Parser.pp_token (Ansi.Parser.Csi s)
  | Cut `Sequence -> Format.pp_print_string ppf "Cut sequence"
  | Cut `String -> Format.pp_print_string ppf "Cut string"
  | Cut `Mouse -> Format.pp_print_string ppf "Cut mouse"
  | Too_long `Dcs -> Format.pp_print_string ppf "Too_long DCS"
  | Too_long `Osc -> Format.pp_print_string ppf "Too_long OSC"
  | Too_long `Apc -> Format.pp_print_string ppf "Too_long APC"

(* Reading control strings *)

module Params = Ansi.Parser.Params

let hex_digit c =
  match c with
  | '0' .. '9' -> Char.code c - 48
  | 'a' .. 'f' -> Char.code c - 87
  | 'A' .. 'F' -> Char.code c - 55
  | _ -> -1

(* [s] written as two hex digits per byte, as XTGETTCAP writes names and
   values. *)
let of_hex s =
  let n = String.length s in
  if n mod 2 <> 0 then None
  else
    let b = Bytes.create (n / 2) in
    let rec go i =
      if i >= n then Some (Bytes.unsafe_to_string b)
      else
        let h = hex_digit s.[i] and l = hex_digit s.[i + 1] in
        if h < 0 || l < 0 then None
        else (
          Bytes.unsafe_set b (i / 2) (Char.unsafe_chr ((h lsl 4) lor l));
          go (i + 2))
    in
    go 0

(* A channel of 1 to 4 hex digits, scaled to 0..255 as XParseColor does. *)
let channel s =
  let n = String.length s in
  if n < 1 || n > 4 then None
  else
    let rec go i v =
      if i >= n then
        let max = (1 lsl (4 * n)) - 1 in
        Some (((v * 255) + (max / 2)) / max)
      else
        let d = hex_digit s.[i] in
        if d < 0 then None else go (i + 1) ((v lsl 4) lor d)
    in
    go 0 0

let rgb spec =
  match String.split_on_char '/' spec with
  | [ r; g; b ] when String.length r > 4 && String.sub r 0 4 = "rgb:" -> (
      match
        (channel (String.sub r 4 (String.length r - 4)), channel g, channel b)
      with
      | Some r, Some g, Some b -> Some (r, g, b)
      | _ -> None)
  | _ -> None

let base64_value c =
  match c with
  | 'A' .. 'Z' -> Char.code c - 65
  | 'a' .. 'z' -> Char.code c - 71
  | '0' .. '9' -> Char.code c + 4
  | '+' -> 62
  | '/' -> 63
  | _ -> -1

(* Base64 (RFC 4648 §4), its padding optional. *)
let of_base64 s =
  let n = String.length s in
  let n = if n > 0 && s.[n - 1] = '=' then n - 1 else n in
  let n = if n > 0 && s.[n - 1] = '=' then n - 1 else n in
  if n mod 4 = 1 || (String.length s > n && String.length s mod 4 <> 0) then
    None
  else
    let out = Buffer.create (n * 3 / 4) in
    let rec go i acc bits =
      if i >= n then Some (Buffer.contents out)
      else
        let v = base64_value s.[i] in
        if v < 0 then None
        else
          let acc = (acc lsl 6) lor v and bits = bits + 6 in
          if bits >= 8 then (
            Buffer.add_char out
              (Char.unsafe_chr ((acc lsr (bits - 8)) land 0xFF));
            go (i + 1) (acc land ((1 lsl (bits - 8)) - 1)) (bits - 8))
          else go (i + 1) acc bits
    in
    go 0 0 0

let split_at c s =
  match String.index_opt s c with
  | None -> (s, None)
  | Some i ->
      (String.sub s 0 i, Some (String.sub s (i + 1) (String.length s - i - 1)))

let is_number s = s <> "" && String.for_all (fun c -> c >= '0' && c <= '9') s

let of_osc content =
  let number, rest = split_at ';' content in
  let color target spec =
    match rgb spec with
    | Some rgb -> Color { target; rgb }
    | None -> Osc content
  in
  match (number, rest) with
  | "10", Some spec -> color `Foreground spec
  | "11", Some spec -> color `Background spec
  | "12", Some spec -> color `Cursor spec
  | "4", Some rest -> (
      match split_at ';' rest with
      | index, Some spec when is_number index && String.length index <= 9 ->
          color (`Palette (int_of_string index)) spec
      | _ -> Osc content)
  | "52", Some rest -> (
      match split_at ';' rest with
      | selection, Some data -> (
          match of_base64 data with
          | Some data -> Clipboard { selection; data }
          | None -> Osc content)
      | _ -> Osc content)
  | _ -> Osc content

let termcap valid data =
  let entry s =
    match split_at '=' s with
    | name, None -> Option.map (fun n -> (n, None)) (of_hex name)
    | name, Some value -> (
        match (of_hex name, of_hex value) with
        | Some n, Some v -> Some (n, Some v)
        | _ -> None)
  in
  let entries = if data = "" then [] else String.split_on_char ';' data in
  let rec go acc = function
    | [] -> Some (Termcap { valid; entries = List.rev acc })
    | e :: rest -> (
        match entry e with Some e -> go (e :: acc) rest | None -> None)
  in
  go [] entries

let of_dcs (h : Ansi.Parser.sequence) data =
  let n = Params.count h.params in
  let raw = Dcs { header = h; data } in
  match (h.marker, h.intermediates, h.final) with
  | Some '>', "", '|' when n = 0 -> Version data
  | None, "!", '|' when n = 0 -> Unit_id data
  | None, "+", 'r' when n = 1 -> (
      match Params.value h.params 0 with
      | Some ((0 | 1) as v) -> Option.value (termcap (v = 1) data) ~default:raw
      | _ -> raw)
  | _ -> raw

let graphics content =
  let keys, message = split_at ';' content in
  let id =
    List.find_map
      (fun kv ->
        match split_at '=' kv with
        | "i", Some v when is_number v && String.length v <= 9 ->
            Some (int_of_string v)
        | _ -> None)
      (String.split_on_char ',' keys)
  in
  Graphics { id; message = Option.value message ~default:"" }
