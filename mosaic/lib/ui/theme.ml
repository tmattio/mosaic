type font_styles = {
  heading : Style.t;
  body : Style.t;
  code : Style.t;
  emphasis : Style.t;
}

type t = {
  name : string;
  primary : Style.color;
  secondary : Style.color;
  background : Style.color;
  surface : Style.color;
  error : Style.color;
  warning : Style.color;
  success : Style.color;
  info : Style.color;
  text_primary : Style.color;
  text_secondary : Style.color;
  border : Style.color;
  border_style : Border.style;
  spacing_unit : int;
  font_styles : font_styles;
}

let default_dark =
  {
    name = "dark";
    primary = Style.Bright_blue;
    secondary = Style.Bright_cyan;
    background = Style.Black;
    surface = Style.gray 2;
    error = Style.Bright_red;
    warning = Style.Bright_yellow;
    success = Style.Bright_green;
    info = Style.Bright_blue;
    text_primary = Style.White;
    text_secondary = Style.gray 18;
    border = Style.gray 8;
    border_style = Border.Rounded;
    spacing_unit = 1;
    font_styles =
      {
        heading = Style.(fg White ++ bold);
        body = Style.(fg White);
        code = Style.(fg Bright_cyan);
        emphasis = Style.(fg White ++ italic);
      };
  }

let default_light =
  {
    name = "light";
    primary = Style.Blue;
    secondary = Style.Cyan;
    background = Style.White;
    surface = Style.gray 22;
    error = Style.Red;
    warning = Style.Yellow;
    success = Style.Green;
    info = Style.Blue;
    text_primary = Style.Black;
    text_secondary = Style.gray 8;
    border = Style.gray 18;
    border_style = Border.Rounded;
    spacing_unit = 1;
    font_styles =
      {
        heading = Style.(fg Black ++ bold);
        body = Style.(fg Black);
        code = Style.(fg Blue);
        emphasis = Style.(fg Black ++ italic);
      };
  }

(* Popular themes *)

let dracula =
  {
    name = "dracula";
    primary = Style.rgb_hex 0xBD93F9;
    (* Purple *)
    secondary = Style.rgb_hex 0x6272A4;
    (* Comment *)
    background = Style.rgb_hex 0x282A36;
    (* Background *)
    surface = Style.rgb_hex 0x2B2E3B;
    error = Style.rgb_hex 0xFF5555;
    (* Red *)
    warning = Style.rgb_hex 0xFFB86C;
    (* Orange *)
    success = Style.rgb_hex 0x50FA7B;
    (* Green *)
    info = Style.rgb_hex 0x8BE9FD;
    (* Cyan *)
    text_primary = Style.rgb_hex 0xF8F8F2;
    (* Foreground *)
    text_secondary = Style.rgb_hex 0x6272A4;
    border = Style.rgb_hex 0x44475A;
    (* Current Line *)
    border_style = Border.Rounded;
    spacing_unit = 1;
    font_styles =
      {
        heading = Style.(fg (rgb_hex 0xF8F8F2) ++ bold);
        body = Style.(fg (rgb_hex 0xF8F8F2));
        code = Style.(fg (rgb_hex 0xFF79C6));
        (* Pink *)
        emphasis = Style.(fg (rgb_hex 0xF1FA8C) ++ italic);
        (* Yellow *)
      };
  }

let monokai =
  {
    name = "monokai";
    primary = Style.rgb_hex 0xAE81FF;
    (* Purple *)
    secondary = Style.rgb_hex 0xF92672;
    (* Magenta *)
    background = Style.rgb_hex 0x272822;
    (* Background *)
    surface = Style.rgb_hex 0x2E2E2E;
    error = Style.rgb_hex 0xF92672;
    (* Red *)
    warning = Style.rgb_hex 0xFD971F;
    (* Orange *)
    success = Style.rgb_hex 0xA6E22E;
    (* Green *)
    info = Style.rgb_hex 0x66D9EF;
    (* Blue *)
    text_primary = Style.rgb_hex 0xD6D6D6;
    (* Foreground *)
    text_secondary = Style.rgb_hex 0x797979;
    border = Style.rgb_hex 0x3E3D32;
    border_style = Border.Rounded;
    spacing_unit = 1;
    font_styles =
      {
        heading = Style.(fg (rgb_hex 0xF8F8F2) ++ bold);
        body = Style.(fg (rgb_hex 0xD6D6D6));
        code = Style.(fg (rgb_hex 0x66D9EF));
        emphasis = Style.(fg (rgb_hex 0xE6DB74) ++ italic);
        (* Yellow *)
      };
  }

let nord =
  {
    name = "nord";
    primary = Style.rgb_hex 0x88C0D0;
    (* Nord8 - Frost cyan *)
    secondary = Style.rgb_hex 0x81A1C1;
    (* Nord9 *)
    background = Style.rgb_hex 0x2E3440;
    (* Nord0 *)
    surface = Style.rgb_hex 0x3B4252;
    (* Nord1 *)
    error = Style.rgb_hex 0xBF616A;
    (* Nord11 - Red *)
    warning = Style.rgb_hex 0xEBCB8B;
    (* Nord13 - Yellow *)
    success = Style.rgb_hex 0xA3BE8C;
    (* Nord14 - Green *)
    info = Style.rgb_hex 0x5E81AC;
    (* Nord10 *)
    text_primary = Style.rgb_hex 0xD8DEE9;
    (* Nord4 *)
    text_secondary = Style.rgb_hex 0xE5E9F0;
    (* Nord5 *)
    border = Style.rgb_hex 0x434C5E;
    (* Nord2 *)
    border_style = Border.Rounded;
    spacing_unit = 1;
    font_styles =
      {
        heading = Style.(fg (rgb_hex 0xECEFF4) ++ bold);
        (* Nord6 *)
        body = Style.(fg (rgb_hex 0xD8DEE9));
        code = Style.(fg (rgb_hex 0x88C0D0));
        emphasis = Style.(fg (rgb_hex 0xB48EAD) ++ italic);
        (* Nord15 - Purple *)
      };
  }

let gruvbox =
  {
    name = "gruvbox";
    primary = Style.rgb_hex 0x85A598;
    (* Aqua/Cyan *)
    secondary = Style.rgb_hex 0xA89A85;
    (* fg4 *)
    background = Style.rgb_hex 0x282828;
    (* bg0 *)
    surface = Style.rgb_hex 0x3C3836;
    (* bg1 *)
    error = Style.rgb_hex 0xFB4934;
    (* Bright Red *)
    warning = Style.rgb_hex 0xFE8019;
    (* Bright Orange *)
    success = Style.rgb_hex 0xB8BB26;
    (* Bright Green *)
    info = Style.rgb_hex 0x83A598;
    (* Bright Blue *)
    text_primary = Style.rgb_hex 0xFBF1C7;
    (* fg0 *)
    text_secondary = Style.rgb_hex 0xEBDBB2;
    (* fg1 *)
    border = Style.rgb_hex 0x504945;
    (* bg2 *)
    border_style = Border.Rounded;
    spacing_unit = 1;
    font_styles =
      {
        heading = Style.(fg (rgb_hex 0xFBF1C7) ++ bold);
        body = Style.(fg (rgb_hex 0xFBF1C7));
        code = Style.(fg (rgb_hex 0xFABD2F));
        (* Bright Yellow *)
        emphasis = Style.(fg (rgb_hex 0xD3869B) ++ italic);
        (* Bright Purple *)
      };
  }

let tokyo_night =
  {
    name = "tokyo_night";
    primary = Style.rgb_hex 0xBB9AF7;
    (* Purple *)
    secondary = Style.rgb_hex 0x7AA2F7;
    (* Blue *)
    background = Style.rgb_hex 0x1A1B26;
    (* Background *)
    surface = Style.rgb_hex 0x24283B;
    (* Surface *)
    error = Style.rgb_hex 0xF7768E;
    (* Red *)
    warning = Style.rgb_hex 0xE0AF68;
    (* Yellow *)
    success = Style.rgb_hex 0x9ECE6A;
    (* Green *)
    info = Style.rgb_hex 0x7DCFFF;
    (* Light Blue *)
    text_primary = Style.rgb_hex 0xA9B1D6;
    (* Foreground *)
    text_secondary = Style.rgb_hex 0x9AA5CE;
    border = Style.rgb_hex 0x414868;
    (* Panel *)
    border_style = Border.Rounded;
    spacing_unit = 1;
    font_styles =
      {
        heading = Style.(fg (rgb_hex 0xC0CAF5) ++ bold);
        (* Lighter text *)
        body = Style.(fg (rgb_hex 0xA9B1D6));
        code = Style.(fg (rgb_hex 0x7AA2F7));
        emphasis = Style.(fg (rgb_hex 0xFF9E64) ++ italic);
        (* Orange *)
      };
  }

let catppuccin_mocha =
  {
    name = "catppuccin_mocha";
    primary = Style.rgb_hex 0xF5C2E7;
    (* Pink *)
    secondary = Style.rgb_hex 0xCBA6F7;
    (* Mauve *)
    background = Style.rgb_hex 0x181825;
    (* Base *)
    surface = Style.rgb_hex 0x313244;
    (* Surface0 *)
    error = Style.rgb_hex 0xF28FAD;
    (* Red *)
    warning = Style.rgb_hex 0xFAE3B0;
    (* Yellow *)
    success = Style.rgb_hex 0xABE9B3;
    (* Green *)
    info = Style.rgb_hex 0x89DCEB;
    (* Sky *)
    text_primary = Style.rgb_hex 0xCDD6F4;
    (* Text *)
    text_secondary = Style.rgb_hex 0xBAC2DE;
    (* Subtext1 *)
    border = Style.rgb_hex 0x45475A;
    (* Surface1 *)
    border_style = Border.Rounded;
    spacing_unit = 1;
    font_styles =
      {
        heading = Style.(fg (rgb_hex 0xCDD6F4) ++ bold);
        body = Style.(fg (rgb_hex 0xCDD6F4));
        code = Style.(fg (rgb_hex 0x94E2D5));
        (* Teal *)
        emphasis = Style.(fg (rgb_hex 0xFAB387) ++ italic);
        (* Peach *)
      };
  }

let solarized_dark =
  {
    name = "solarized_dark";
    primary = Style.rgb_hex 0x268BD2;
    (* Blue *)
    secondary = Style.rgb_hex 0x2AA198;
    (* Cyan *)
    background = Style.rgb_hex 0x002B36;
    (* Base03 *)
    surface = Style.rgb_hex 0x073642;
    (* Base02 *)
    error = Style.rgb_hex 0xDC322F;
    (* Red *)
    warning = Style.rgb_hex 0xCB4B16;
    (* Orange *)
    success = Style.rgb_hex 0x859900;
    (* Green *)
    info = Style.rgb_hex 0x268BD2;
    (* Blue *)
    text_primary = Style.rgb_hex 0x839496;
    (* Base0 *)
    text_secondary = Style.rgb_hex 0x586E75;
    (* Base01 *)
    border = Style.rgb_hex 0x073642;
    (* Base02 *)
    border_style = Border.Rounded;
    spacing_unit = 1;
    font_styles =
      {
        heading = Style.(fg (rgb_hex 0x93A1A1) ++ bold);
        (* Base1 *)
        body = Style.(fg (rgb_hex 0x839496));
        code = Style.(fg (rgb_hex 0x2AA198));
        emphasis = Style.(fg (rgb_hex 0x6C71C4) ++ italic);
        (* Violet *)
      };
  }

let solarized_light =
  {
    name = "solarized_light";
    primary = Style.rgb_hex 0x268BD2;
    (* Blue *)
    secondary = Style.rgb_hex 0x2AA198;
    (* Cyan *)
    background = Style.rgb_hex 0xFDF6E3;
    (* Base3 *)
    surface = Style.rgb_hex 0xEEE8D5;
    (* Base2 *)
    error = Style.rgb_hex 0xDC322F;
    (* Red *)
    warning = Style.rgb_hex 0xCB4B16;
    (* Orange *)
    success = Style.rgb_hex 0x859900;
    (* Green *)
    info = Style.rgb_hex 0x268BD2;
    (* Blue *)
    text_primary = Style.rgb_hex 0x586E75;
    (* Base01 *)
    text_secondary = Style.rgb_hex 0x657B83;
    (* Base00 *)
    border = Style.rgb_hex 0xEEE8D5;
    (* Base2 *)
    border_style = Border.Rounded;
    spacing_unit = 1;
    font_styles =
      {
        heading = Style.(fg (rgb_hex 0x073642) ++ bold);
        (* Base02 *)
        body = Style.(fg (rgb_hex 0x586E75));
        code = Style.(fg (rgb_hex 0x2AA198));
        emphasis = Style.(fg (rgb_hex 0x6C71C4) ++ italic);
        (* Violet *)
      };
  }

let make ?(name = "custom") ?primary ?secondary ?background ?surface ?error
    ?warning ?success ?info ?text_primary ?text_secondary ?border ?border_style
    ?spacing_unit ?heading_style ?body_style ?code_style ?emphasis_style () =
  let base = default_dark in
  {
    name;
    primary = Option.value primary ~default:base.primary;
    secondary = Option.value secondary ~default:base.secondary;
    background = Option.value background ~default:base.background;
    surface = Option.value surface ~default:base.surface;
    error = Option.value error ~default:base.error;
    warning = Option.value warning ~default:base.warning;
    success = Option.value success ~default:base.success;
    info = Option.value info ~default:base.info;
    text_primary = Option.value text_primary ~default:base.text_primary;
    text_secondary = Option.value text_secondary ~default:base.text_secondary;
    border = Option.value border ~default:base.border;
    border_style = Option.value border_style ~default:base.border_style;
    spacing_unit = Option.value spacing_unit ~default:base.spacing_unit;
    font_styles =
      {
        heading = Option.value heading_style ~default:base.font_styles.heading;
        body = Option.value body_style ~default:base.font_styles.body;
        code = Option.value code_style ~default:base.font_styles.code;
        emphasis =
          Option.value emphasis_style ~default:base.font_styles.emphasis;
      };
  }

let with_theme theme style =
  (* Apply theme colors by composing with new styles *)
  let new_style =
    match style.Style.fg with
    | Some (Style.Solid Style.Default) -> Style.(style ++ fg theme.text_primary)
    | _ -> style
  in
  let new_style =
    match new_style.Style.bg with
    | Some (Style.Solid Style.Default) ->
        Style.(new_style ++ bg theme.background)
    | _ -> new_style
  in
  new_style

let spacing theme n = n * theme.spacing_unit

let all_themes =
  [
    ("dark", default_dark);
    ("light", default_light);
    ("dracula", dracula);
    ("monokai", monokai);
    ("nord", nord);
    ("gruvbox", gruvbox);
    ("tokyo_night", tokyo_night);
    ("catppuccin-mocha", catppuccin_mocha);
    ("solarized-dark", solarized_dark);
    ("solarized-light", solarized_light);
  ]

let get_theme name =
  List.find_opt (fun (n, _) -> n = name) all_themes |> Option.map snd
