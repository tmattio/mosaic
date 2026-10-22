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
