open Tape_lang.Ast
module Svg_renderer = Renderer.Vg_renderer
module Gif_renderer = Renderer.Gif_renderer
module Ascii_renderer = Renderer.Ascii_renderer
module Png_renderer = Renderer.Png_renderer

let src = Logs.Src.create "vcr" ~doc:"VCR recording"

module Log = (val Logs.src_log src : Logs.LOG)

(* Central virtual clock for deterministic timing *)
module Clock = struct
  type t = { mutable now : float; frame_interval : float }

  let create ~fps = { now = 0.0; frame_interval = 1.0 /. float_of_int fps }
  let tick t dt = t.now <- t.now +. dt
  let now t = t.now
  let frame_interval t = t.frame_interval

  let should_capture_frame t last_frame_time =
    t.now >= last_frame_time +. t.frame_interval
end

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
  mutable recording : bool;
  clock : Clock.t; (* Virtual clock for deterministic timing *)
  mutable last_frame_time : float; (* Time of last captured frame *)
}

(* Helper to read from PTY until it's idle for a moment - REMOVED as polling defeats event-driven architecture *)

(* Capture frames based on recording timeline *)
let capture_frames_for_time state =
  if not state.recording then (
    Log.debug (fun m -> m "Not recording, skipping frame capture");
    ())
  else
    (* Capture all frames between last_frame_time and current clock time *)
    let frames_captured = ref 0 in
    while Clock.should_capture_frame state.clock state.last_frame_time do
      (* Calculate elapsed time since last frame *)
      let old_last_frame_time = state.last_frame_time in
      state.last_frame_time <-
        state.last_frame_time +. Clock.frame_interval state.clock;
      let elapsed = state.last_frame_time -. old_last_frame_time in

      (* Always add the elapsed time to the renderer *)
      (match state.renderer with
      | SVG { renderer = (module R); state = renderer_state } ->
          R.add_pending_delay renderer_state elapsed
      | GIF { renderer = (module R); state = renderer_state } ->
          R.add_pending_delay renderer_state elapsed
      | ASCII { renderer = (module R); state = renderer_state } ->
          R.add_pending_delay renderer_state elapsed);

      (* Take a snapshot **only** when something changed *)
      if Vte.is_dirty state.vte then (
        Log.debug (fun m ->
            m "Capturing frame at time %.3fs (dirty: true)"
              state.last_frame_time);
        (match state.renderer with
        | SVG { renderer = (module R); state = renderer_state } ->
            R.capture_frame renderer_state
        | GIF { renderer = (module R); state = renderer_state } ->
            R.capture_frame renderer_state
        | ASCII { renderer = (module R); state = renderer_state } ->
            R.capture_frame renderer_state);
        Vte.clear_dirty state.vte;
        incr frames_captured)
    done;
    if !frames_captured > 0 then
      Log.debug (fun m -> m "Captured %d frames" !frames_captured)

(* Write to PTY with proper handling of EAGAIN/EWOULDBLOCK *)
let write_to_pty state data =
  let rec write_with_retry offset remaining =
    if remaining = 0 then ()
    else
      try
        let written = Pty.write_string state.pty_master data offset remaining in
        write_with_retry (offset + written) (remaining - written)
      with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
        (* PTY buffer full, yield briefly instead of blocking *)
        Eio.Fiber.yield ();
        write_with_retry offset remaining
  in
  write_with_retry 0 (String.length data)

(* Advance virtual time and capture frames *)
let advance_time state duration =
  let start_time = Clock.now state.clock in
  let end_time = start_time +. duration in
  Log.debug (fun m ->
      m "Advancing time by %.3fs (from %.3fs to %.3fs)" duration start_time
        end_time);

  (* Advance time and capture frames *)
  Clock.tick state.clock duration;
  capture_frames_for_time state

(* Sleep while still processing PTY output *)
let sleep_with_output state duration =
  (* First, drain any pending PTY output instantly *)
  let buf = Bytes.create 4096 in
  let rec drain_all () =
    let had_data = ref false in
    let rec drain () =
      try
        let n = Pty.read state.pty_master buf 0 (Bytes.length buf) in
        if n > 0 then (
          had_data := true;
          Log.debug (fun m -> m "Draining during sleep: %d bytes" n);
          Vte.feed state.vte buf 0 n;
          drain ())
      with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> ()
    in
    drain ();
    (* If we got data, yield to let more arrive and drain again *)
    if !had_data then (
      Eio.Fiber.yield ();
      drain_all ())
  in
  drain_all ();

  (* Capture frame if terminal changed during drain *)
  if Vte.is_dirty state.vte then (
    capture_frames_for_time state;
    Vte.clear_dirty state.vte);

  (* Now advance virtual time for the sleep duration *)
  advance_time state duration

(* Async version using Eio *)
let read_pty_output_async ~sw ~clock:_ state =
  let pty_fd = Pty.in_fd state.pty_master in
  let buf = Bytes.create 4096 in

  let rec drain () =
    (* read until EAGAIN / EOF *)
    match
      try Some (Pty.read state.pty_master buf 0 (Bytes.length buf)) with
      | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) -> None
      | Unix.Unix_error (Unix.EIO, _, _) -> raise Exit
    with
    | None -> () (* nothing left right now *)
    | Some 0 -> raise Exit (* slave closed; exit fiber *)
    | Some n ->
        Log.debug (fun m ->
            m "Read %d bytes from PTY: %S" n (Bytes.sub_string buf 0 (min n 50)));
        Vte.feed state.vte buf 0 n;
        drain () (* keep draining *)
  in

  let rec pump () =
    drain ();
    (* 1. handle everything queued *)
    Eio_unix.await_readable pty_fd;
    (* 2. park until next edge *)
    pump ()
  in

  Eio.Fiber.fork_daemon ~sw (fun () ->
      Log.debug (fun m -> m "PTY reader started");
      try pump ()
      with Exit ->
        Log.debug (fun m -> m "PTY reader exiting");
        `Stop_daemon)

(* Helper function to convert key to escape sequence *)
and key_to_sequence = function
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
  | WindowBar, String s ->
      { cfg with window_bar = Some (s = "true" || s = "1") }
  | WindowBarSize, Float f ->
      { cfg with window_bar_size = Some (int_of_float f) }
  | _ -> cfg (* Ignore others *)

let handle_set state setting value =
  state.config <- apply_setting_to_config state.config setting value

let process_tape_config tape =
  List.fold_left
    (fun cfg cmd ->
      match cmd with
      | Set (setting, value) -> apply_setting_to_config cfg setting value
      | _ -> cfg)
    default_config tape

(* Calculate character dimensions from font size *)
let calculate_char_dimensions font_size =
  let char_width = max 6 (font_size * 6 / 10) in
  let char_height = max 8 (font_size * 12 / 10) in
  (char_width, char_height)

(* Calculate terminal dimensions from pixel dimensions *)
let calculate_terminal_dimensions width height char_width char_height =
  let term_cols = width / char_width in
  let term_rows = height / char_height in
  (term_cols, term_rows)

let create_renderer config vte output_path =
  let char_width, char_height = calculate_char_dimensions config.font_size in

  match Filename.extension output_path with
  | ".gif" ->
      let renderer_config =
        {
          Gif_renderer.char_width;
          char_height;
          frame_delay = 100 / config.framerate;
          theme = Gif_renderer.default_theme;
          font_path = config.font_family;
          font_size = config.font_size;
          target_width = Some config.width;
          target_height = Some config.height;
          padding = config.padding;
        }
      in
      let module R = Gif_renderer in
      let state = R.create vte renderer_config in
      GIF { renderer = (module R); state }
  | ".svg" ->
      let renderer_config =
        {
          Svg_renderer.font_family = "monospace";
          font_size = float_of_int config.font_size;
          line_height = 1.2;
          theme = Svg_renderer.default_theme;
        }
      in
      let module R = Svg_renderer in
      let state = R.create vte renderer_config in
      SVG { renderer = (module R); state }
  | ".txt" | ".ascii" ->
      let module R = Ascii_renderer in
      let state = R.create vte { R.separator = "" } in
      ASCII { renderer = (module R); state }
  | ext -> failwith (Printf.sprintf "Unsupported output format: %s" ext)

let generate_output renderer output_path =
  let output_data =
    match renderer with
    | SVG { renderer = (module R); state } -> R.render state
    | GIF { renderer = (module R); state } -> R.render state
    | ASCII { renderer = (module R); state } -> R.render state
  in

  if output_path = "-" then (
    output_string stdout output_data;
    flush stdout)
  else
    (* Create parent directory if it doesn't exist *)
    let oc = open_out_bin output_path in
    output_string oc output_data;
    close_out oc

(* Async command execution *)
let rec handle_command_async state cmd =
  let open Tape_lang.Ast in
  match cmd with
  | Set (setting, value) -> handle_set state setting value
  | Type { text; speed } ->
      let delay = Option.value speed ~default:state.config.typing_speed in
      (* For typing animation, we need to interleave writing and time advancement *)
      String.iteri
        (fun i char ->
          (* Mark terminal state before this character *)
          let was_dirty = Vte.is_dirty state.vte in

          (* Write the character *)
          write_to_pty state (String.make 1 char);

          (* Drain output to get the echo *)
          let buf = Bytes.create 4096 in
          let rec drain_with_timeout attempts =
            if attempts <= 0 then ()
            else
              try
                let n = Pty.read state.pty_master buf 0 (Bytes.length buf) in
                if n > 0 then (
                  Vte.feed state.vte buf 0 n;
                  (* Continue draining if more data available *)
                  drain_with_timeout attempts)
              with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
                (* No data available, maybe wait a tiny bit for echo *)
                if i = 0 || not (Vte.is_dirty state.vte || was_dirty) then (
                  (* First char or no echo yet - give it a microsleep *)
                  Eio.Fiber.yield ();
                  drain_with_timeout (attempts - 1))
          in
          drain_with_timeout 3;

          (* Debug: check cursor position *)
          let row, col = Vte.cursor_pos state.vte in
          Log.debug (fun m ->
              m "Cursor after char %d: row=%d, col=%d" i row col);

          (* Advance virtual time for this character *)
          advance_time state delay)
        text
  | KeyPress { key; count; speed } ->
      let delay = Option.value speed ~default:0.1 in
      let key_sequence = key_to_sequence key in
      for _ = 1 to count do
        write_to_pty state key_sequence;
        (* For Enter key, wait for command output *)
        if key = Enter then (
          Log.debug (fun m -> m "Enter pressed, waiting for command output");
          (* Give shell time to process the command *)
          let rec wait_for_output attempts =
            if attempts > 0 then (
              (* Drain PTY output *)
              let buf = Bytes.create 4096 in
              let had_data = ref false in
              let rec drain () =
                try
                  let n = Pty.read state.pty_master buf 0 (Bytes.length buf) in
                  if n > 0 then (
                    had_data := true;
                    Log.debug (fun m ->
                        m "Read %d bytes after Enter: %S" n
                          (Bytes.sub_string buf 0 (min n 50)));
                    Vte.feed state.vte buf 0 n;
                    drain ())
                with
                | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
                  ()
              in
              drain ();
              (* If we didn't get data, yield and try again *)
              if not !had_data then (
                Eio.Fiber.yield ();
                wait_for_output (attempts - 1))
              else Log.debug (fun m -> m "Got output after Enter"))
          in
          wait_for_output 20);
        (* Advance time and capture frames *)
        advance_time state delay
      done
  | Sleep t ->
      (* Sleep while still processing PTY output *)
      sleep_with_output state t
  | Hide -> state.recording <- false
  | Show -> state.recording <- true
  | Copy s -> state.clipboard <- s
  | Paste -> write_to_pty state state.clipboard
  | Ctrl keys ->
      (* Handle Ctrl key combinations *)
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
      if ctrl_char <> "" then write_to_pty state ctrl_char
  | Alt key ->
      (* Handle Alt+key combinations *)
      let alt_sequence = Printf.sprintf "\x1b%s" key in
      write_to_pty state alt_sequence
  | Shift key ->
      (* Handle Shift+key - just send the uppercase version for letters *)
      let shifted_key =
        if String.length key = 1 then String.uppercase_ascii key else key
      in
      write_to_pty state shifted_key
  | Screenshot path ->
      (* Debug cursor position *)
      let row, col = Vte.cursor_pos state.vte in
      Log.debug (fun m -> m "Screenshot: cursor at row=%d, col=%d" row col);

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
                let ch =
                  if String.length cell.glyph > 0 then cell.glyph.[0] else ' '
                in
                line_chars := ch :: !line_chars;
                if ch <> ' ' then last_non_space := col
          done;

          let full_line =
            String.init (List.length !line_chars) (fun i ->
                List.nth (List.rev !line_chars) i)
          in

          (* Trim trailing spaces *)
          let trimmed_line =
            if !last_non_space >= 0 then
              String.sub full_line 0 (!last_non_space + 1)
            else ""
          in
          Buffer.add_string buffer trimmed_line;
          Buffer.add_char buffer '\n'
        done;

        (* Write to file *)
        let oc = open_out path in
        output_string oc (Buffer.contents buffer);
        close_out oc)
      else
        (* For images, we'd need to implement PNG support *)
        Log.err (fun m ->
            m
              "Screenshot format not supported: %s. Supported formats: .txt \
               (PNG support not yet implemented)"
              (Filename.extension path))
  | Wait { target; pattern; timeout } ->
      (* Wait for pattern to appear on screen *)
      let _ = target in
      (* target is unused in vcr *)
      let timeout = Option.value timeout ~default:15.0 in
      let pattern = Option.value pattern ~default:">$" in
      let regex = Str.regexp pattern in

      (* Helper to get all visible text from VTE *)
      let get_visible_text () =
        let buffer = Buffer.create 1024 in
        let rows = Vte.rows state.vte in
        for row = 0 to rows - 1 do
          for col = 0 to Vte.cols state.vte - 1 do
            match Vte.get_cell state.vte ~row ~col with
            | None -> Buffer.add_char buffer ' '
            | Some cell ->
                let ch =
                  if String.length cell.glyph > 0 then cell.glyph.[0] else ' '
                in
                Buffer.add_char buffer ch
          done;
          Buffer.add_char buffer '\n'
        done;
        Buffer.contents buffer
      in

      let rec wait_loop elapsed =
        if elapsed > timeout then
          Log.warn (fun m ->
              m "Wait timed out after %.1fs looking for pattern: %s" timeout
                pattern)
        else
          let visible_text = get_visible_text () in
          try
            let _ = Str.search_forward regex visible_text 0 in
            () (* Pattern found *)
          with Not_found ->
            (* Advance virtual time instead of real sleep *)
            advance_time state 0.1;
            (* Yield to let PTY reader run *)
            Eio.Fiber.yield ();
            wait_loop (elapsed +. 0.1)
      in
      wait_loop 0.0
  | Source filename ->
      (* Source a file of commands *)
      if Sys.file_exists filename then
        let ic = open_in filename in
        Fun.protect
          ~finally:(fun () -> close_in_noerr ic)
          (fun () ->
            match Tape_lang.from_channel ic with
            | Ok commands -> List.iter (handle_command_async state) commands
            | Error msg ->
                Log.err (fun m ->
                    m "Failed to parse sourced file %s: %s" filename msg))
  | Env (_key, _value) ->
      (* Set environment variable - this would need to be done before spawning shell *)
      Log.warn (fun m -> m "Env command should be used before shell spawn")
  | Require _ ->
      (* Already handled during initialization *)
      ()
  | Output _ ->
      (* Output commands are handled separately *)
      ()

let run tape output_path =
  let total_start = Unix.gettimeofday () in
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let eio_start = Unix.gettimeofday () in
  Printf.eprintf "[TIMING] Eio startup: %.3fs\n" (eio_start -. total_start);
  (* Process initial config and setup - same as sync *)
  let initial_config =
    List.fold_left
      (fun cfg cmd ->
        match cmd with
        | Set (setting, value) -> apply_setting_to_config cfg setting value
        | _ -> cfg)
      default_config tape
  in

  (* All the setup code from run_sync *)
  let check_requirements () =
    let required_programs =
      List.filter_map (function Require prog -> Some prog | _ -> None) tape
    in
    List.iter
      (fun prog ->
        match Sys.command (Printf.sprintf "which %s > /dev/null 2>&1" prog) with
        | 0 -> ()
        | _ ->
            Log.err (fun m -> m "Required program '%s' not found" prog);
            exit 1)
      required_programs
  in
  check_requirements ();
  let setup_start = Unix.gettimeofday () in

  let char_width, char_height =
    calculate_char_dimensions initial_config.font_size
  in
  let term_cols, term_rows =
    calculate_terminal_dimensions initial_config.width initial_config.height
      char_width char_height
  in

  (* Create VTE first *)
  let vte = Vte.create ~rows:term_rows ~cols:term_cols () in
  Printf.eprintf "[TIMING] VTE setup: %.3fs\n"
    (Unix.gettimeofday () -. setup_start);

  (* Create renderer *)
  let renderer =
    match output_path with
    | None ->
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
        let module R = Gif_renderer in
        let state = R.create vte renderer_config in
        GIF { renderer = (module R); state }
    | Some path -> create_renderer initial_config vte path
  in

  (* Spawn shell with VHS-compatible settings *)
  let shell_basename = Filename.basename initial_config.shell in

  (* Get shell command and environment like VHS *)
  let prog, argv, unix_env =
    match shell_basename with
    | "bash" | "sh" ->
        let env =
          Array.append (Unix.environment ())
            [|
              "PS1=\\[\\e[38;2;90;86;224m\\]> \\[\\e[0m\\]";
              "BASH_SILENCE_DEPRECATION_WARNING=1";
              "HISTFILE=/dev/null";
              (* Disable history *)
            |]
        in
        ( initial_config.shell,
          [| initial_config.shell; "--noprofile"; "--norc"; "-i" |],
          env )
    | "zsh" ->
        let env =
          Array.append (Unix.environment ())
            [| "PROMPT=%F{#5B56E0}> %F{reset_color}" |]
        in
        ( initial_config.shell,
          [| initial_config.shell; "--histnostore"; "--no-rcs"; "-i" |],
          env )
    | "fish" ->
        ( initial_config.shell,
          [|
            initial_config.shell;
            "--login";
            "--no-config";
            "--private";
            "-C";
            "function fish_greeting; end";
            "-C";
            "function fish_prompt; set_color 5B56E0; echo -n \"> \"; set_color \
             normal; end";
          |],
          Unix.environment () )
    | "nu" ->
        ( initial_config.shell,
          [|
            initial_config.shell;
            "--execute";
            "$env.PROMPT_COMMAND = {'\\033[;38;2;91;86;224m>\\033[m '}; \
             $env.PROMPT_COMMAND_RIGHT = {''}";
          |],
          Unix.environment () )
    | "osh" ->
        let env =
          Array.append (Unix.environment ())
            [| "PS1=\\[\\e[38;2;90;86;224m\\]> \\[\\e[0m\\]" |]
        in
        (initial_config.shell, [| initial_config.shell; "--norc" |], env)
    | "xonsh" ->
        ( initial_config.shell,
          [|
            initial_config.shell;
            "--no-rc";
            "-D";
            "PROMPT=\\033[;38;2;91;86;224m>\\033[m ";
          |],
          Unix.environment () )
    | _ ->
        (* Default: run shell as-is *)
        Log.warn (fun m ->
            m "Unknown shell '%s', using default settings" shell_basename);
        (initial_config.shell, [| initial_config.shell |], Unix.environment ())
  in

  let winsize = { Pty.rows = term_rows; cols = term_cols; x = 0; y = 0 } in

  Log.debug (fun m ->
      m "Spawning shell: %s with args: %s" prog
        (String.concat " " (Array.to_list argv)));
  let spawn_start = Unix.gettimeofday () in
  let pty_master =
    Pty.spawn ~prog ~args:(Array.to_list argv) ~winsize ~env:unix_env ()
  in
  Printf.eprintf "[TIMING] Shell spawn: %.3fs\n"
    (Unix.gettimeofday () -. spawn_start);
  Log.debug (fun m -> m "Shell spawned");

  (* Set PTY to non-blocking mode for better async I/O handling *)
  Pty.set_nonblock pty_master;

  let clock = Clock.create ~fps:initial_config.framerate in
  let state =
    {
      pty_master;
      vte;
      renderer;
      config = initial_config;
      clipboard = "";
      recording = true;
      clock;
      last_frame_time = 0.0;
    }
  in

  (* Cleanup on exit *)
  Eio.Switch.on_release sw (fun () ->
      Pty.close pty_master
      (* Note: Without the PID, we can't kill the process. The shell should
         exit when the PTY is closed. *));

  (* Start async PTY reader and frame timer *)
  let clock = Eio.Stdenv.clock env in
  Log.debug (fun m -> m "Starting PTY reader...");
  read_pty_output_async ~sw ~clock state;
  Log.debug (fun m -> m "PTY reader forked");

  (* Wait for shell to output its prompt before proceeding *)
  let prompt_start = Unix.gettimeofday () in
  let rec wait_for_shell_prompt ~attempts =
    if attempts <= 0 then
      Log.warn (fun m -> m "Shell prompt never appeared – continuing anyway")
    else (
      (* Give the PTY reader fiber a chance to run *)
      Eio.Fiber.yield ();

      (* Small real delay to let shell actually start *)
      if attempts = 100 then Eio.Time.sleep clock 0.1;

      if
        Vte.is_dirty state.vte
        || String.contains (Vte.to_string_grid state.vte) '>'
      then (
        Log.debug (fun m -> m "Shell prompt ready");
        (* Give it a tiny bit more time to fully settle *)
        Eio.Fiber.yield ();
        Eio.Time.sleep clock 0.05;

        (* Now advance virtual time and capture the initial frame *)
        advance_time state 0.1;
        capture_frames_for_time state)
      else wait_for_shell_prompt ~attempts:(attempts - 1))
  in
  wait_for_shell_prompt ~attempts:100;
  Printf.eprintf "[TIMING] Wait for prompt: %.3fs\n"
    (Unix.gettimeofday () -. prompt_start);

  (* Execute commands *)
  Log.debug (fun m -> m "Executing %d commands..." (List.length tape));
  let exec_start = Unix.gettimeofday () in
  List.iter
    (fun cmd ->
      let cmd_start = Unix.gettimeofday () in
      let cmd_name =
        match cmd with
        | Sleep t -> Printf.sprintf "Sleep %.1f" t
        | Type { text; _ } -> Printf.sprintf "Type '%s'" text
        | KeyPress { key; _ } ->
            Printf.sprintf "KeyPress %s"
              (match key with Enter -> "Enter" | _ -> "Other")
        | Set (setting, _) ->
            Printf.sprintf "Set %s"
              (match setting with
              | Shell -> "Shell"
              | Width -> "Width"
              | Height -> "Height"
              | _ -> "Other")
        | _ -> "Other"
      in
      Log.debug (fun m -> m "Executing command: %s" cmd_name);
      handle_command_async state cmd;
      let cmd_end = Unix.gettimeofday () in
      let cmd_desc =
        match cmd with
        | Sleep t -> Printf.sprintf "Sleep %.1f" t
        | Type { text; _ } ->
            Printf.sprintf "Type '%s'"
              (String.sub text 0 (min 20 (String.length text)))
        | KeyPress { key; _ } ->
            Printf.sprintf "KeyPress %s"
              (match key with Enter -> "Enter" | _ -> "Other")
        | _ -> "Other"
      in
      if cmd_end -. cmd_start > 0.1 then
        Printf.eprintf "[TIMING] Command '%s': %.3fs\\n" cmd_desc
          (cmd_end -. cmd_start))
    tape;
  Printf.eprintf "[TIMING] Command execution: %.3fs\n"
    (Unix.gettimeofday () -. exec_start);

  Log.debug (fun m ->
      m "Recording complete. Total time: %.1fs" (Clock.now state.clock));

  (* Ensure any remaining time is captured *)
  capture_frames_for_time state;

  (* Render output *)
  let render_start = Unix.gettimeofday () in
  let output_data =
    match renderer with
    | SVG { renderer = (module R); state = renderer_state } ->
        R.render renderer_state
    | GIF { renderer = (module R); state = renderer_state } ->
        R.render renderer_state
    | ASCII { renderer = (module R); state = renderer_state } ->
        R.render renderer_state
  in
  Printf.eprintf "[TIMING] Rendering: %.3fs\n"
    (Unix.gettimeofday () -. render_start);

  (* Write output *)
  match output_path with
  | None -> ()
  | Some "-" ->
      output_string stdout output_data;
      flush stdout
  | Some path ->
      let oc = open_out_bin path in
      output_string oc output_data;
      close_out oc
