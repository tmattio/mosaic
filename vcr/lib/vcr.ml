open Tape_lang.Ast
module Svg_renderer = Renderer.Vg_renderer
module Gif_renderer = Renderer.Gif_renderer
module Ascii_renderer = Renderer.Ascii_renderer

type config = {
  shell : string;
  width : int; (* Width in pixels for vhs compatibility *)
  height : int; (* Height in pixels for vhs compatibility *)
  typing_speed : float;
  font_size : int;
  font_family : string option; (* Font file path *)
  padding : int; (* Padding in pixels *)
  framerate : int; (* Framerate for GIF capture *)
  playback_speed : float; (* Playback speed multiplier *)
  theme : string option; (* Terminal theme *)
  letter_spacing : float option; (* Letter spacing *)
  line_height : float option; (* Line height *)
  loop_offset : float option; (* GIF loop offset *)
  border_radius : int option; (* Border radius *)
  margin : int option; (* Margin *)
  margin_fill : string option; (* Margin fill color *)
  cursor_blink : bool; (* Cursor blink *)
  window_bar : bool option; (* Window bar *)
  window_bar_size : int option; (* Window bar size *)
}

let default_config =
  {
    shell = "sh";
    width = 640;
    height = 480;
    typing_speed = 0.05;
    font_size = 22;
    font_family = None;
    padding = 60;
    framerate = 50;
    playback_speed = 1.0;
    theme = None;
    letter_spacing = None;
    line_height = None;
    loop_offset = None;
    border_radius = None;
    margin = None;
    margin_fill = None;
    cursor_blink = true;
    window_bar = None;
    window_bar_size = None;
  }

type renderer_type =
  | SVG of {
      renderer :
        (module Renderer.S
           with type t = Svg_renderer.t
            and type config = Svg_renderer.config);
      state : Svg_renderer.t;
    }
  | GIF of {
      renderer :
        (module Renderer.S
           with type t = Gif_renderer.t
            and type config = Gif_renderer.config);
      state : Gif_renderer.t;
    }
  | ASCII of {
      renderer :
        (module Renderer.S
           with type t = Ascii_renderer.t
            and type config = Ascii_renderer.config);
      state : Ascii_renderer.t;
    }

type state = {
  pty_master : Pty.t;
  vte : Vte.t;
  renderer : renderer_type;
  mutable config : config;
  mutable clipboard : string;
}

(* Helper to read from PTY until it's idle for a moment *)
let read_pty_output state =
  let buf = Bytes.create 4096 in
  let rec loop () =
    let ready_fds, _, _ =
      Unix.select [ Pty.to_file_descr state.pty_master ] [] [] 0.05
    in
    if ready_fds <> [] then
      try
        let n = Pty.read state.pty_master buf 0 (Bytes.length buf) in
        if n > 0 then (
          Vte.feed state.vte buf 0 n;
          loop ())
      with Unix.Unix_error (EIO, _, _) ->
        () (* EIO is expected on PTY close *)
    else ()
  in
  loop ();
  match state.renderer with
  | SVG { renderer = (module R); state = renderer_state } ->
      R.capture_frame renderer_state
  | GIF { renderer = (module R); state = renderer_state } ->
      R.capture_frame renderer_state
  | ASCII { renderer = (module R); state = renderer_state } ->
      R.capture_frame renderer_state

let apply_setting_to_config cfg setting value =
  match (setting, value) with
  | Shell, String s -> { cfg with shell = s }
  | Width, Float f -> { cfg with width = int_of_float f }
  | Height, Float f -> { cfg with height = int_of_float f }
  | TypingSpeed, Float f -> { cfg with typing_speed = f }
  | FontSize, Float f -> { cfg with font_size = int_of_float f }
  | FontFamily, String s -> { cfg with font_family = Some s }
  | Padding, Float f -> { cfg with padding = int_of_float f }
  | Framerate, Float f -> { cfg with framerate = int_of_float f }
  | PlaybackSpeed, Float f -> { cfg with playback_speed = f }
  | Theme, String s -> { cfg with theme = Some s }
  | Theme, Json s -> { cfg with theme = Some s }
  | LetterSpacing, Float f -> { cfg with letter_spacing = Some f }
  | LineHeight, Float f -> { cfg with line_height = Some f }
  | LoopOffset, Float f -> { cfg with loop_offset = Some f }
  | BorderRadius, Float f -> { cfg with border_radius = Some (int_of_float f) }
  | Margin, Float f -> { cfg with margin = Some (int_of_float f) }
  | MarginFill, String s -> { cfg with margin_fill = Some s }
  | CursorBlink, Bool b -> { cfg with cursor_blink = b }
  | WindowBar, Bool b -> { cfg with window_bar = Some b }
  | WindowBar, String s -> { cfg with window_bar = Some (s = "true" || s = "1") }
  | WindowBarSize, Float f -> { cfg with window_bar_size = Some (int_of_float f) }
  | _ -> cfg (* Ignore others *)

let handle_set state setting value =
  state.config <- apply_setting_to_config state.config setting value

let rec handle_command state cmd =
  let open Tape_lang.Ast in
  (match cmd with
  | Set (setting, value) -> handle_set state setting value
  | Type { text; speed } ->
      let delay = Option.value speed ~default:state.config.typing_speed in
      String.iter
        (fun char ->
          let s = String.make 1 char in
          ignore (Pty.write_string state.pty_master s 0 1);
          Unix.sleepf delay;
          read_pty_output state)
        text
  | KeyPress { key; count; speed } ->
      let delay = Option.value speed ~default:0.1 in
      let key_sequence =
        match key with
        | Enter -> "\r"
        | Tab -> "\t"
        | Space -> " "
        | Backspace -> "\x7f"
        | Delete -> "\x1b[3~"
        | Escape -> "\x1b"
        | Up -> "\x1b[A"
        | Down -> "\x1b[B"
        | Left -> "\x1b[D"
        | Right -> "\x1b[C"
        | PageUp -> "\x1b[5~"
        | PageDown -> "\x1b[6~"
        | Home -> "\x1b[H"
        | End -> "\x1b[F"
        | Insert -> "\x1b[2~"
      in
      for _ = 1 to count do
        ignore
          (Pty.write_string state.pty_master key_sequence 0
             (String.length key_sequence));
        read_pty_output state;
        Unix.sleepf delay
      done
  | Sleep t ->
      (* Capture frames during sleep for smooth animation *)
      let frame_interval = 1.0 /. float_of_int state.config.framerate in
      let frames = int_of_float (t /. frame_interval) in
      for _ = 1 to frames do
        Unix.sleepf frame_interval;
        read_pty_output state
      done;
      (* Handle any remaining time *)
      let remaining = t -. (float_of_int frames *. frame_interval) in
      if remaining > 0.0 then Unix.sleepf remaining
  | Hide -> Vte.set_cursor_visible state.vte false
  | Show -> Vte.set_cursor_visible state.vte true
  | Copy s -> state.clipboard <- s
  | Paste ->
      ignore
        (Pty.write_string state.pty_master state.clipboard 0
           (String.length state.clipboard))
  | Ctrl keys ->
      (* Parse Ctrl sequences like Ctrl+C, Ctrl+Alt+D, etc. *)
      let ctrl_char =
        match keys with
        | [] -> ""
        | _ :: _ -> (
            (* Extract the base key (last element) *)
            let base_key = List.rev keys |> List.hd |> String.lowercase_ascii in
            if String.length base_key = 1 then
              let c = base_key.[0] in
              if c >= 'a' && c <= 'z' then
                String.make 1 (Char.chr (Char.code c - 96))
              else ""
            else
              (* Handle special cases *)
              match base_key with
              | "c" -> "\x03" (* Ctrl+C *)
              | "d" -> "\x04" (* Ctrl+D *)
              | "z" -> "\x1a" (* Ctrl+Z *)
              | "l" -> "\x0c" (* Ctrl+L *)
              | "r" -> "\x12" (* Ctrl+R *)
              | "a" -> "\x01" (* Ctrl+A *)
              | "e" -> "\x05" (* Ctrl+E *)
              | "k" -> "\x0b" (* Ctrl+K *)
              | "u" -> "\x15" (* Ctrl+U *)
              | "w" -> "\x17" (* Ctrl+W *)
              | _ -> "")
      in
      if ctrl_char <> "" then (
        ignore
          (Pty.write_string state.pty_master ctrl_char 0
             (String.length ctrl_char));
        read_pty_output state)
  | Alt key ->
      (* Handle Alt+key combinations *)
      let alt_sequence = Printf.sprintf "\x1b%s" key in
      ignore
        (Pty.write_string state.pty_master alt_sequence 0
           (String.length alt_sequence));
      read_pty_output state
  | Shift key ->
      (* Handle Shift+key - just send the uppercase version for letters *)
      let shifted_key = 
        if String.length key = 1 then
          String.uppercase_ascii key
        else key
      in
      ignore
        (Pty.write_string state.pty_master shifted_key 0
           (String.length shifted_key));
      read_pty_output state
  | Screenshot path -> 
      (* Capture current terminal content *)
      if Filename.check_suffix path ".txt" then (
        (* Capture terminal buffer as text *)
        let rows = Vte.rows state.vte in
        let buffer = Buffer.create 4096 in
        
        (* Read each line from the terminal buffer *)
        for row = 0 to rows - 1 do
          let line_chars = ref [] in
          let last_non_space = ref (-1) in
          
          (* Get characters for this line *)
          for col = 0 to Vte.cols state.vte - 1 do
            match Vte.get_cell state.vte ~row ~col with
            | None -> line_chars := ' ' :: !line_chars
            | Some cell ->
                let ch = try Uchar.to_char cell.char with _ -> '?' in
                line_chars := ch :: !line_chars;
                if ch <> ' ' then last_non_space := col
          done;
          
          (* Convert to string and trim trailing spaces *)
          let full_line =
            String.init (List.length !line_chars) (fun i ->
                List.nth (List.rev !line_chars) i)
          in
          let trimmed_line =
            if !last_non_space >= 0 then String.sub full_line 0 (!last_non_space + 1)
            else ""
          in
          Buffer.add_string buffer trimmed_line;
          Buffer.add_char buffer '\n'
        done;
        
        (* Write to file *)
        let oc = open_out path in
        output_string oc (Buffer.contents buffer);
        close_out oc
      ) else (
        (* For now, skip non-.txt screenshots *)
        (* TODO: Implement PNG export when PNG library is available *)
        ())
  | Wait { pattern; timeout; _ } ->
      (* Wait for pattern to appear on screen *)
      let timeout = Option.value timeout ~default:15.0 in
      let pattern = Option.value pattern ~default:">$" in
      let regex = Str.regexp pattern in
      let start_time = Unix.gettimeofday () in
      let rec wait_loop () =
        if Unix.gettimeofday () -. start_time > timeout then ()
          (* Timeout reached *)
        else
          let current_line =
            (* Get the current line from VTE *)
            let row, _ = Vte.cursor_pos state.vte in
            let line_buffer = Buffer.create 80 in
            for col = 0 to Vte.cols state.vte - 1 do
              match Vte.get_cell state.vte ~row ~col with
              | None -> Buffer.add_char line_buffer ' '
              | Some cell ->
                  let ch = try Uchar.to_char cell.char with _ -> '?' in
                  Buffer.add_char line_buffer ch
            done;
            Buffer.contents line_buffer
          in
          try
            let _ = Str.search_forward regex current_line 0 in
            () (* Pattern found *)
          with Not_found ->
            Unix.sleepf 0.01;
            (* Wait 10ms before checking again *)
            read_pty_output state;
            wait_loop ()
      in
      wait_loop ()
  | Env (key, value) ->
      (* Set environment variable *)
      Unix.putenv key value
  | Source tape_file ->
      (* Load and execute commands from another tape file *)
      let ic = open_in tape_file in
      Fun.protect
        ~finally:(fun () -> close_in_noerr ic)
        (fun () ->
          match Tape_lang.from_channel ic with
          | Ok commands -> List.iter (handle_command state) commands
          | Error _ -> ())
  | Require _ ->
      (* Require commands are handled during initialization *)
      ()
  | Output _ ->
      (* Output commands are handled separately *)
      ());
  read_pty_output state (* Capture a frame after every command *)

let run tape output_path =
  (* output_path can be:
     - Some path: generate output to the specified path (or "-" for stdout)
     - None: run tape without generating output (for side effects like screenshots)
  *)
  (* First, check all required programs *)
  let check_requirements () =
    List.iter
      (function
        | Require prog ->
            (* Check if program exists in PATH *)
            let check_program prog =
              let paths =
                try String.split_on_char ':' (Sys.getenv "PATH")
                with Not_found -> [ "/usr/bin"; "/bin"; "/usr/local/bin" ]
              in
              List.exists
                (fun path -> Sys.file_exists (Filename.concat path prog))
                paths
            in
            if not (check_program prog) then
              failwith
                (Printf.sprintf "Required program '%s' not found in PATH" prog)
        | _ -> ())
      tape
  in
  check_requirements ();

  let initial_config =
    List.fold_left
      (fun cfg cmd ->
        match cmd with
        | Set (setting, value) -> apply_setting_to_config cfg setting value
        | _ -> cfg)
      default_config tape
  in

  (* Calculate character dimensions based on font size *)
  let char_width = max 6 (initial_config.font_size * 6 / 10) in
  let char_height = max 8 (initial_config.font_size * 12 / 10) in

  (* Calculate terminal dimensions from pixel dimensions *)
  let term_cols = initial_config.width / char_width in
  let term_rows = initial_config.height / char_height in

  let winsize = { Pty.rows = term_rows; cols = term_cols; x = 0; y = 0 } in
  let prog = initial_config.shell in
  let argv = [| prog |] in

  (* Set custom prompt like vhs *)
  let env =
    if Filename.basename prog = "bash" || Filename.basename prog = "sh" then
      Array.append (Unix.environment ())
        [|
          "PS1=\\[\\e[38;2;90;86;224m\\]> \\[\\e[0m\\]";
          "BASH_SILENCE_DEPRECATION_WARNING=1";
        |]
    else if Filename.basename prog = "zsh" then
      Array.append (Unix.environment ())
        [| "PROMPT=%F{#5B56E0}> %F{reset_color}" |]
    else Unix.environment ()
  in

  let pty_master, pid = Pty.spawn ~prog ~argv ~winsize ~env () in
  let vte = Vte.create ~rows:term_rows ~cols:term_cols () in

  (* Determine renderer based on file extension *)
  let renderer =
    match output_path with
    | None ->
        (* No output requested, use a dummy GIF renderer that won't be used for output *)
        let renderer_config =
          {
            Gif_renderer.char_width;
            char_height;
            frame_delay = 100 / initial_config.framerate;
            theme = Gif_renderer.default_theme;
            font_path = initial_config.font_family;
            font_size = initial_config.font_size;
            target_width = Some initial_config.width;
            target_height = Some initial_config.height;
            padding = initial_config.padding;
          }
        in
        let renderer_state = Gif_renderer.create vte renderer_config in
        GIF
          {
            renderer =
              (module Gif_renderer : Renderer.S
                with type t = Gif_renderer.t
                 and type config = Gif_renderer.config);
            state = renderer_state;
          }
    | Some path when path = "-" || Filename.check_suffix path ".gif" ->
      let renderer_config =
        {
          Gif_renderer.char_width;
          char_height;
          frame_delay = 100 / initial_config.framerate;
          (* Convert fps to centiseconds *)
          theme = Gif_renderer.default_theme;
          font_path = initial_config.font_family;
          font_size = initial_config.font_size;
          target_width = Some initial_config.width;
          target_height = Some initial_config.height;
          padding = initial_config.padding;
        }
      in
      let renderer_state = Gif_renderer.create vte renderer_config in
      GIF
        {
          renderer =
            (module Gif_renderer : Renderer.S
              with type t = Gif_renderer.t
               and type config = Gif_renderer.config);
          state = renderer_state;
        }
    | Some path
      when Filename.check_suffix path ".ascii" || Filename.check_suffix path ".txt"
      ->
      let renderer_config = Ascii_renderer.default_config in
      let renderer_state = Ascii_renderer.create vte renderer_config in
      ASCII
        {
          renderer =
            (module Ascii_renderer : Renderer.S
              with type t = Ascii_renderer.t
               and type config = Ascii_renderer.config);
          state = renderer_state;
        }
    | Some path when Filename.check_suffix path ".svg" ->
      let renderer_config =
        {
          Svg_renderer.font_family = "monospace";
          font_size = 16.0;
          line_height = 1.4;
          theme = Svg_renderer.default_theme;
        }
      in
      let renderer_state = Svg_renderer.create vte renderer_config in
      SVG
        {
          renderer =
            (module Svg_renderer : Renderer.S
              with type t = Svg_renderer.t
               and type config = Svg_renderer.config);
          state = renderer_state;
        }
    | Some path ->
      (* Unknown extension *)
      failwith
        (Printf.sprintf
           "Unsupported output format for '%s'. Supported formats: .gif, .svg, \
            .txt, .ascii"
           path)
  in

  let state =
    { pty_master; vte; renderer; config = initial_config; clipboard = "" }
  in

  Fun.protect
    ~finally:(fun () ->
      (* Render the output *)
      let output_data =
        match state.renderer with
        | SVG { renderer = (module R); state = renderer_state } ->
            R.render renderer_state
        | GIF { renderer = (module R); state = renderer_state } ->
            R.render renderer_state
        | ASCII { renderer = (module R); state = renderer_state } ->
            R.render renderer_state
      in

      (* Write output only if requested *)
      (match output_path with
       | None -> () (* No output requested *)
       | Some "-" ->
           (* Write to stdout *)
           output_string stdout output_data;
           flush stdout
       | Some path ->
           (* Write to file *)
           let oc = open_out_bin path in
           output_string oc output_data;
           close_out oc);

      Pty.close pty_master;
      try Unix.kill pid Sys.sigkill with _ -> ())
    (fun () ->
      Unix.sleepf 0.5;
      (* Give shell time to start *)
      read_pty_output state;

      (* Capture initial state *)
      List.iter (handle_command state) tape;
      (* Give shell a moment to finish *)
      Unix.sleepf 0.5;
      read_pty_output state)
