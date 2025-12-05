type unicode_width = [ `Wcwidth | `Unicode ]

type t = {
  term : string;
  rgb : bool;
  kitty_keyboard : bool;
  kitty_graphics : bool;
  bracketed_paste : bool;
  focus_tracking : bool;
  unicode_width : unicode_width;
  sgr_pixels : bool;
  color_scheme_updates : bool;
  explicit_width : bool;
  scaled_text : bool;
  sixel : bool;
  sync : bool;
  hyperlinks : bool;
}

type terminal_info = { name : string; version : string; from_xtversion : bool }

let contains_substring s sub =
  let len_s = String.length s and len_sub = String.length sub in
  if len_sub = 0 then true
  else
    let rec loop idx =
      if idx + len_sub > len_s then false
      else if String.sub s idx len_sub = sub then true
      else loop (idx + 1)
    in
    loop 0

let detect_rgb () =
  let env = Sys.getenv_opt "COLORTERM" |> Option.map String.lowercase_ascii in
  match env with
  | Some v when contains_substring v "truecolor" -> true
  | Some v when contains_substring v "24bit" -> true
  | _ -> false

let detect_kitty ~term =
  Sys.getenv_opt "KITTY_WINDOW_ID" <> None
  || contains_substring (String.lowercase_ascii term) "kitty"

let starts_with ~prefix s =
  let len_p = String.length prefix in
  String.length s >= len_p && String.sub s 0 len_p = prefix

let make_initial_capabilities ~term =
  let rgb = detect_rgb () in
  let kitty = detect_kitty ~term in
  {
    term;
    rgb;
    kitty_keyboard = kitty;
    kitty_graphics = false;
    bracketed_paste = false;
    focus_tracking = false;
    unicode_width = `Unicode;
    sgr_pixels = false;
    color_scheme_updates = false;
    explicit_width = false;
    scaled_text = false;
    sixel = false;
    sync = false;
    hyperlinks = false;
  }

let with_env name f caps =
  match Sys.getenv_opt name with Some value -> f value caps | None -> caps

let with_env_bool name f caps =
  match Sys.getenv_opt name with Some _ -> f caps | None -> caps

let colorterm_truecolor value =
  let v = String.lowercase_ascii value in
  String.equal v "truecolor" || String.equal v "24bit"

let apply_environment_overrides caps =
  let caps = { caps with bracketed_paste = true } in
  (* On Windows, assume modern terminal capabilities (Windows Terminal, ConPTY, etc.) *)
  let caps =
    if Sys.win32 then
      {
        caps with
        rgb = true;
        unicode_width = `Unicode;
        bracketed_paste = true;
        (* ConPTY has limited capabilities compared to Unix terminals *)
        sixel = false;
        kitty_graphics = false;
        kitty_keyboard = false;
      }
    else caps
  in
  let caps =
    if Sys.getenv_opt "TMUX" <> None then { caps with unicode_width = `Wcwidth }
    else
      with_env "TERM"
        (fun term caps ->
          let lower = String.lowercase_ascii term in
          if
            starts_with ~prefix:"tmux" lower
            || starts_with ~prefix:"screen" lower
          then { caps with unicode_width = `Wcwidth }
          else caps)
        caps
  in
  let caps =
    with_env "TERM_PROGRAM"
      (fun prog caps ->
        match prog with
        | "vscode" ->
            {
              caps with
              kitty_keyboard = false;
              kitty_graphics = false;
              unicode_width = `Unicode;
            }
        | "Apple_Terminal" -> { caps with unicode_width = `Wcwidth }
        | _ -> caps)
      caps
  in
  let caps =
    with_env "COLORTERM"
      (fun value caps ->
        if colorterm_truecolor value then { caps with rgb = true } else caps)
      caps
  in
  let caps =
    with_env_bool "TERMUX_VERSION"
      (fun caps -> { caps with unicode_width = `Wcwidth })
      caps
  in
  let caps =
    with_env_bool "VHS_RECORD"
      (fun caps ->
        {
          caps with
          unicode_width = `Wcwidth;
          kitty_keyboard = false;
          kitty_graphics = false;
        })
      caps
  in
  let caps =
    with_env_bool "MATRIX_FORCE_WCWIDTH"
      (fun caps -> { caps with unicode_width = `Wcwidth })
      caps
  in
  let caps =
    with_env_bool "MATRIX_FORCE_UNICODE"
      (fun caps -> { caps with unicode_width = `Unicode })
      caps
  in
  let caps =
    with_env_bool "KITTY_WINDOW_ID"
      (fun caps ->
        {
          caps with
          kitty_keyboard = true;
          kitty_graphics = true;
          unicode_width = `Unicode;
          rgb = true;
          sixel = true;
          hyperlinks = true;
        })
      caps
  in
  caps

let make_terminal_info term =
  { name = term; version = ""; from_xtversion = false }

let update_terminal_info_from_env info =
  if info.from_xtversion then info
  else
    match Sys.getenv_opt "TERM_PROGRAM" with
    | Some prog ->
        let version =
          Option.value ~default:"" (Sys.getenv_opt "TERM_PROGRAM_VERSION")
        in
        { name = prog; version; from_xtversion = false }
    | None -> info

let initial ?provided ~term () =
  match provided with
  | Some caps ->
      let info =
        make_terminal_info caps.term |> update_terminal_info_from_env
      in
      (caps, info)
  | None ->
      let caps =
        make_initial_capabilities ~term |> apply_environment_overrides
      in
      let info =
        make_terminal_info caps.term |> update_terminal_info_from_env
      in
      (caps, info)

let parse_xtversion_payload info payload =
  let payload = String.trim payload in
  if payload = "" then info
  else
    let len = String.length payload in
    let find_char ch start =
      try Some (String.index_from payload start ch) with Not_found -> None
    in
    match find_char '(' 0 with
    | Some paren ->
        let close =
          match find_char ')' (paren + 1) with Some idx -> idx | None -> len
        in
        let name = String.sub payload 0 paren |> String.trim in
        let version =
          if close > paren + 1 then
            String.sub payload (paren + 1) (close - paren - 1) |> String.trim
          else ""
        in
        { name; version; from_xtversion = true }
    | None -> (
        match find_char ' ' 0 with
        | Some space ->
            let name = String.sub payload 0 space |> String.trim in
            let rest_len = len - (space + 1) in
            let version =
              if rest_len > 0 then
                String.sub payload (space + 1) rest_len |> String.trim
              else ""
            in
            { name; version; from_xtversion = true }
        | None -> { name = payload; version = ""; from_xtversion = true })

let mark_kitty_caps caps =
  {
    caps with
    kitty_keyboard = true;
    kitty_graphics = true;
    unicode_width = `Unicode;
    rgb = true;
    sixel = true;
    hyperlinks = true;
    bracketed_paste = true;
  }

let apply_mode_report caps report =
  let is_enabled value = value = 1 || value = 2 in
  let update caps (mode, value) =
    if not (is_enabled value) then caps
    else
      match mode with
      | 1004 -> { caps with focus_tracking = true }
      | 1016 -> { caps with sgr_pixels = true }
      | 2004 -> { caps with bracketed_paste = true }
      | 2026 -> { caps with sync = true }
      | 2027 -> { caps with unicode_width = `Unicode }
      | 2031 -> { caps with color_scheme_updates = true }
      | _ -> caps
  in
  if report.Input.Caps.is_private then List.fold_left update caps report.modes
  else caps

let apply_event (caps, info, seen_da) (event : Input.Caps.event) =
  match event with
  | Input.Caps.Device_attributes attrs ->
      let caps =
        if List.exists (( = ) 4) attrs then { caps with sixel = true } else caps
      in
      (caps, info, true || seen_da)
  | Input.Caps.Mode_report r ->
      let caps = apply_mode_report caps r in
      (caps, info, seen_da)
  | Input.Caps.Pixel_resolution _ -> (caps, info, seen_da)
  | Input.Caps.Cursor_position (row, col) ->
      let caps =
        let caps =
          if row = 1 && col >= 2 then { caps with explicit_width = true }
          else caps
        in
        if row = 1 && col >= 3 then { caps with scaled_text = true } else caps
      in
      (caps, info, seen_da)
  | Input.Caps.Xtversion payload ->
      let info = parse_xtversion_payload info payload in
      let caps =
        if contains_substring (String.lowercase_ascii payload) "kitty" then
          mark_kitty_caps caps
        else caps
      in
      (caps, info, seen_da)
  | Input.Caps.Kitty_graphics_reply payload ->
      let caps =
        if contains_substring payload "i=31337" then
          { caps with kitty_graphics = true }
        else caps
      in
      (caps, info, seen_da)
  | Input.Caps.Kitty_keyboard { level; _ } ->
      let caps = if level > 0 then mark_kitty_caps caps else caps in
      (caps, info, seen_da)

let process_events ?(apply_env_overrides = false) ~caps ~info events =
  let caps, info, _ = List.fold_left apply_event (caps, info, false) events in
  let caps =
    if apply_env_overrides then apply_environment_overrides caps else caps
  in
  (caps, info)

let cursor_save = Ansi.Escape.(to_string cursor_save)
let cursor_restore = Ansi.Escape.(to_string cursor_restore)
let home = Ansi.Escape.(to_string home)

let explicit_width_query =
  Ansi.Escape.(to_string Ansi.Escape.request_explicit_width_support)

let scaled_text_query =
  Ansi.Escape.(to_string Ansi.Escape.request_scaled_text_support)

let xtversion_query =
  Ansi.Escape.(to_string Ansi.Escape.request_terminal_identity)

let csi_u_query = Ansi.Escape.(to_string Ansi.Escape.request_csi_u_support)

let decrqm_sgr_pixels =
  Ansi.Escape.(to_string Ansi.Escape.request_sgr_pixels_mode)

let decrqm_unicode = Ansi.Escape.(to_string Ansi.Escape.request_unicode_mode)

let decrqm_color_scheme =
  Ansi.Escape.(to_string Ansi.Escape.request_color_scheme)

let decrqm_focus = Ansi.Escape.(to_string Ansi.Escape.request_focus_mode)

let decrqm_bracketed_paste =
  Ansi.Escape.(to_string Ansi.Escape.request_bracketed_paste_mode)

let decrqm_sync = Ansi.Escape.(to_string Ansi.Escape.request_sync_mode)
let cursor_position_request = Ansi.Escape.(to_string request_cursor_position)

let kitty_graphics_query =
  Ansi.Escape.(to_string Ansi.Escape.request_kitty_graphics_support)

let primary_device_attrs =
  Ansi.Escape.(to_string Ansi.Escape.request_device_attributes)

let iterm2_proprietary_query = "\027]1337;ReportCellSize\007"

let build_probe_payload term =
  let base_queries =
    [
      cursor_save;
      decrqm_sgr_pixels;
      decrqm_unicode;
      decrqm_color_scheme;
      decrqm_focus;
      decrqm_bracketed_paste;
      decrqm_sync;
      primary_device_attrs;
      home;
      explicit_width_query;
      cursor_position_request;
      home;
      scaled_text_query;
      cursor_position_request;
      xtversion_query;
      csi_u_query;
      kitty_graphics_query;
    ]
  in
  let queries =
    let term_lower = String.lowercase_ascii term in
    if contains_substring term_lower "iterm" then
      base_queries @ [ iterm2_proprietary_query ]
    else base_queries
  in
  String.concat "" (queries @ [ cursor_restore ])

let probe ?(timeout = 0.2) ?(apply_env_overrides = false) ~read_into
    ~wait_readable ~send ~caps ~info () =
  let parser = Input.Parser.create () in
  let payload = build_probe_payload caps.term in
  send payload;
  let buffer = Bytes.create 4096 in
  let deadline = Unix.gettimeofday () +. max 0. timeout in
  let rec loop caps info got_explicit got_scaled got_da =
    let now = Unix.gettimeofday () in
    let complete =
      (got_explicit || caps.explicit_width)
      && (got_scaled || caps.scaled_text)
      && got_da
    in
    if now >= deadline || complete then (caps, info)
    else
      let remaining = deadline -. now in
      let select_timeout = Float.min remaining 0.05 in
      let ready = wait_readable ~timeout:select_timeout in
      if not ready then loop caps info got_explicit got_scaled got_da
      else
        let read = read_into buffer 0 (Bytes.length buffer) in
        if read <= 0 then loop caps info got_explicit got_scaled got_da
        else
          let _, caps_events = Input.Parser.feed parser buffer 0 read in
          let has_da =
            got_da
            || List.exists
                 (function
                   | Input.Caps.Device_attributes _ -> true | _ -> false)
                 caps_events
          in
          let caps, info =
            process_events ~apply_env_overrides ~caps ~info caps_events
          in
          let got_explicit = got_explicit || caps.explicit_width in
          let got_scaled = got_scaled || caps.scaled_text in
          loop caps info got_explicit got_scaled has_da
  in
  let caps, info = loop caps info caps.explicit_width caps.scaled_text false in
  let _, caps_events = Input.Parser.flush parser in
  process_events ~apply_env_overrides ~caps ~info caps_events
