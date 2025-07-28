type theme_def = (string * Ui.Style.t) list
type theme = [ `Dracula | `Custom of (string * Ui.Style.t) list ]

(* Helper to convert hex color to RGB *)
let hex_to_rgb hex =
  let hex = String.sub hex 1 6 in
  (* Remove # *)
  let r = int_of_string ("0x" ^ String.sub hex 0 2) in
  let g = int_of_string ("0x" ^ String.sub hex 2 2) in
  let b = int_of_string ("0x" ^ String.sub hex 4 2) in
  Ui.Style.rgb r g b

(* Dracula theme colors *)
module Dracula_colors = struct
  let _background = hex_to_rgb "#282a36"
  let foreground = hex_to_rgb "#f8f8f2"
  let comment = hex_to_rgb "#6272a4"
  let yellow = hex_to_rgb "#f1fa8c"
  let purple = hex_to_rgb "#bd93f9"
  let pink = hex_to_rgb "#ff79c6"
  let cyan = hex_to_rgb "#8be9fd"
  let green = hex_to_rgb "#50fa7b"
  let orange = hex_to_rgb "#ffb86c"
  let _red = hex_to_rgb "#ff5555"
  let _bright_red = hex_to_rgb "#ff6e6e"
  let _bright_cyan = hex_to_rgb "#a4ffff"
  let _bright_green = hex_to_rgb "#69ff94"
  let _bright_magenta = hex_to_rgb "#ff92df"
  let _bright_yellow = hex_to_rgb "#ffffa5"
  let _bright_white = hex_to_rgb "#ffffff"
end

let dracula : theme_def =
  let open Dracula_colors in
  [
    (* Comments *)
    ("comment", Ui.Style.fg comment);
    (* Strings *)
    ("string", Ui.Style.fg yellow);
    ("constant.character.escaped", Ui.Style.fg pink);
    ("constant.character.escape", Ui.Style.fg pink);
    (* Numbers and constants *)
    ("constant.numeric", Ui.Style.fg purple);
    ("constant.language", Ui.Style.fg purple);
    ("constant.character", Ui.Style.fg purple);
    ("constant.other", Ui.Style.fg purple);
    (* Variables *)
    ("variable", Ui.Style.fg foreground);
    ("variable.parameter", Ui.Style.(fg orange ++ italic));
    ("variable.other.readwrite.instance", Ui.Style.fg orange);
    (* Keywords *)
    ("keyword", Ui.Style.fg pink);
    ("keyword.control", Ui.Style.fg pink);
    ("keyword.operator", Ui.Style.fg pink);
    ("keyword.other", Ui.Style.fg pink);
    (* Storage *)
    ("storage", Ui.Style.fg pink);
    ("storage.type", Ui.Style.(fg cyan ++ italic));
    ("storage.modifier", Ui.Style.fg pink);
    (* Entity names *)
    ("entity.name.class", Ui.Style.(fg green ++ underline));
    ("entity.other.inherited-class", Ui.Style.(fg green ++ italic ++ underline));
    ("entity.name.function", Ui.Style.fg green);
    ("entity.name.tag", Ui.Style.fg pink);
    ("entity.other.attribute-name", Ui.Style.fg green);
    (* Support *)
    ("support.function", Ui.Style.fg cyan);
    ("support.constant", Ui.Style.fg (hex_to_rgb "#6be5fd"));
    ("support.type", Ui.Style.(fg (hex_to_rgb "#66d9ef") ++ italic));
    ("support.class", Ui.Style.(fg (hex_to_rgb "#66d9ef") ++ italic));
    ("support.other.variable", Ui.Style.fg foreground);
    (* Invalid *)
    ("invalid", Ui.Style.(fg (hex_to_rgb "#F8F8F0") ++ bg pink));
    ("invalid.deprecated", Ui.Style.(fg (hex_to_rgb "#F8F8F0") ++ bg purple));
    (* Diff *)
    ("meta.diff", Ui.Style.fg comment);
    ("meta.diff.header", Ui.Style.fg comment);
    ("markup.deleted", Ui.Style.fg pink);
    ("markup.inserted", Ui.Style.fg green);
    ("markup.changed", Ui.Style.fg (hex_to_rgb "#E6DB74"));
    (* JSON specific *)
    ( "meta.structure.dictionary.json string.quoted.double.json",
      Ui.Style.fg cyan );
    ( "meta.structure.dictionary.value.json string.quoted.double.json",
      Ui.Style.fg yellow );
    (* Punctuation *)
    ("punctuation.definition.comment", Ui.Style.fg comment);
    ("punctuation.definition.string", Ui.Style.fg yellow);
    ("punctuation.definition.variable", Ui.Style.fg foreground);
    ("punctuation.definition.parameters", Ui.Style.fg foreground);
    ("punctuation.definition.array", Ui.Style.fg foreground);
    ("punctuation", Ui.Style.fg foreground);
    (* Other *)
    ("message.error", Ui.Style.fg (hex_to_rgb "#F83333"));
  ]

let get_theme_def = function `Dracula -> dracula | `Custom theme -> theme
