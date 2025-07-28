type lang =
  [ `OCaml
  | `OCaml_interface
  | `Dune
  | `Shell
  | `Diff
  | `Custom of TmLanguage.grammar ]

type theme =
  [ `Dracula
  | `Solarized_dark
  | `Solarized_light
  | `Custom of (string * Ui.Style.t) list ]

type error = [ `Unknown_lang of string ]

let default_dark_theme : theme = `Dracula

module String_map = Map.Make (String)

let rec highlight_tokens i spans line = function
  | [] -> List.rev spans
  | tok :: toks ->
      let j = TmLanguage.ending tok in
      if j > String.length line then
        (* Token extends beyond line length, likely includes the newline *)
        List.rev spans
      else if j > i then
        let text = String.sub line i (j - i) in
        let scopes = TmLanguage.scopes tok in
        highlight_tokens j ((scopes, text) :: spans) line toks
      else
        (* Skip zero-length tokens *)
        highlight_tokens j spans line toks

let lang_to_grammars lang =
  match lang with
  | `OCaml -> [ TmLanguage.of_yojson_exn Grammars.ocaml ]
  | `OCaml_interface -> [ TmLanguage.of_yojson_exn Grammars.ocaml_interface ]
  | `Dune -> [ TmLanguage.of_yojson_exn Grammars.dune ]
  | `Shell -> [ TmLanguage.of_yojson_exn Grammars.shell ]
  | `Diff -> [ TmLanguage.of_yojson_exn Grammars.diff ]
  | `Custom grammar -> [ grammar ]

let find_style (theme_map : Ui.Style.t String_map.t) (scopes : string list) :
    Ui.Style.t =
  (* Try to match scopes from most specific to least specific *)
  let rec try_scopes = function
    | [] -> Ui.Style.default
    | scope :: rest ->
        (* Split the scope into parts and try matching from most to least specific *)
        let parts = String.split_on_char '.' scope in
        let rec try_parts parts =
          match parts with
          | [] -> try_scopes rest
          | _ -> (
              let key = String.concat "." parts in
              match String_map.find_opt key theme_map with
              | Some style -> style
              | None -> (
                  (* Try with one less part *)
                  match List.rev parts with
                  | [] -> try_scopes rest
                  | _ :: rev_rest -> try_parts (List.rev rev_rest)))
        in
        try_parts parts
  in
  try_scopes scopes

(* This function is internal and does not produce styled text *)
let tokenize_to_scopes t grammar stack str =
  let lines = String.split_on_char '\n' str in
  let rec loop stack acc = function
    | [] -> List.rev acc
    | line :: lines ->
        (* Add newline for proper pattern matching *)
        let line_with_nl = line ^ "\n" in
        let tokens, stack =
          TmLanguage.tokenize_exn t grammar stack line_with_nl
        in
        (* Use line_with_nl for tokenization to handle newlines correctly *)
        let spans = highlight_tokens 0 [] line_with_nl tokens in
        loop stack (spans :: acc) lines
  in
  loop stack [] lines

(* Parse hex color to RGB *)
let color_from_hex hex =
  let hex =
    if String.starts_with ~prefix:"#" hex then
      String.sub hex 1 (String.length hex - 1)
    else hex
  in
  if String.length hex = 6 then
    try
      let r = int_of_string ("0x" ^ String.sub hex 0 2) in
      let g = int_of_string ("0x" ^ String.sub hex 2 2) in
      let b = int_of_string ("0x" ^ String.sub hex 4 2) in
      Ansi.RGB (r, g, b)
    with _ -> Ansi.Default
  else Ansi.Default

let parse_textmate_theme json =
  let open Yojson.Safe.Util in
  let theme_entries = ref [] in

  try
    let token_colors = json |> member "tokenColors" |> to_list in
    List.iter
      (fun entry ->
        let scopes =
          try
            match member "scope" entry with
            | `String s -> [ s ]
            | `List l -> List.map to_string l
            | _ -> []
          with _ -> []
        in

        let settings = member "settings" entry in
        let foreground =
          try Some (settings |> member "foreground" |> to_string)
          with _ -> None
        in
        let font_style =
          try Some (settings |> member "fontStyle" |> to_string)
          with _ -> None
        in

        match foreground with
        | None -> ()
        | Some fg_color ->
            let color = color_from_hex fg_color in
            let style_attrs = ref [ Ui.Style.Fg color ] in
            (match font_style with
            | Some "italic" -> style_attrs := Ui.Style.Italic :: !style_attrs
            | Some "bold" -> style_attrs := Ui.Style.Bold :: !style_attrs
            | Some "underline" ->
                style_attrs := Ui.Style.Underline :: !style_attrs
            | _ -> ());

            let style = Ui.Style.of_list !style_attrs in
            List.iter
              (fun scope -> theme_entries := (scope, style) :: !theme_entries)
              scopes)
      token_colors;

    !theme_entries
  with _ -> []

let get_theme_def = function
  | `Dracula -> parse_textmate_theme Themes.dracula
  | `Solarized_dark -> parse_textmate_theme Themes.solarized_dark
  | `Solarized_light -> parse_textmate_theme Themes.solarized_light
  | `Custom theme_def -> theme_def

let highlight ?(theme = default_dark_theme) ?tm ~lang src =
  try
    let theme_def = get_theme_def theme in
    let theme_map = String_map.of_list theme_def in

    let t, grammar =
      match (tm, lang) with
      | Some tm, `Custom g -> (tm, g)
      | Some tm, _ -> (
          let grammar_name =
            match lang with
            | `OCaml -> "OCaml"
            | `OCaml_interface -> "OCaml Interface"
            | `Dune -> "Dune"
            | `Shell -> "Shell Script"
            | `Diff -> "Diff"
            | `Custom _ -> assert false (* handled above *)
          in
          ( tm,
            match TmLanguage.find_by_name tm grammar_name with
            | Some g -> g
            | None ->
                failwith
                  (Printf.sprintf "Grammar '%s' not found in TM instance"
                     grammar_name) ))
      | None, _ ->
          let t = TmLanguage.create () in
          let grammars = lang_to_grammars lang in
          List.iter (TmLanguage.add_grammar t) grammars;
          (t, List.hd grammars)
    in

    let lines_of_tokens = tokenize_to_scopes t grammar TmLanguage.empty src in

    let line_elements =
      List.map
        (fun line ->
          let styled_segments =
            List.map
              (fun (scopes, text) ->
                let style = find_style theme_map scopes in
                (* Strip trailing newline from text *)
                let text =
                  if
                    String.length text > 0
                    && text.[String.length text - 1] = '\n'
                  then String.sub text 0 (String.length text - 1)
                  else text
                in
                (text, style))
              line
          in
          Ui.rich_text styled_segments)
        lines_of_tokens
    in

    Ok (Ui.vbox line_elements)
  with Failure msg -> Error (`Unknown_lang msg)
