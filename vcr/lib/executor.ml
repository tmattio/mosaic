(** Executor - runs tape commands and produces event log *)

open Tape_lang.Ast

let src = Logs.Src.create "vcr.executor" ~doc:"VCR executor"

module Log = (val Logs.src_log src : Logs.LOG)

(** Get shell spawn information based on configuration *)
let shell_spawn_info config =
  let shell_basename = Filename.basename config.Config.shell in
  match shell_basename with
  | "bash" | "sh" ->
      let env =
        Array.append (Unix.environment ())
          [|
            "PS1=\\[\\e[38;2;90;86;224m\\]> \\[\\e[0m\\]";
            "BASH_SILENCE_DEPRECATION_WARNING=1";
            "HISTFILE=/dev/null";
          |]
      in
      (config.shell, [| config.shell; "--noprofile"; "--norc"; "-i" |], env)
  | "zsh" ->
      let env =
        Array.append (Unix.environment ())
          [| "PROMPT=%F{#5B56E0}> %F{reset_color}" |]
      in
      (config.shell, [| config.shell; "--histnostore"; "--no-rcs"; "-i" |], env)
  | "fish" ->
      ( config.shell,
        [|
          config.shell;
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
      ( config.shell,
        [|
          config.shell;
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
      (config.shell, [| config.shell; "--norc" |], env)
  | "xonsh" ->
      ( config.shell,
        [|
          config.shell;
          "--no-rc";
          "-D";
          "PROMPT=\\033[;38;2;91;86;224m>\\033[m ";
        |],
        Unix.environment () )
  | _ ->
      (* Default: run shell as-is *)
      (config.shell, [| config.shell |], Unix.environment ())

type state = {
  pty_master : Pty.t;
  vte : Vte.t;
  virtual_time : float;
  events : Event.log; (* Immutable list of events *)
  clipboard : string;
  config : Config.t;
  prev_grid : Grid.t option;
  prev_cursor : (int * int) option;
  pty_mutex : Eio.Mutex.t; (* Mutex for thread-safe PTY access *)
}

(** Add an event to the log - returns new state *)
let add_event state event = { state with events = event :: state.events }

(** Write to PTY with proper error handling *)
let write_to_pty state data =
  Eio.Mutex.use_rw ~protect:true state.pty_mutex (fun () ->
      let rec write_with_retry offset remaining =
        if remaining = 0 then Ok ()
        else
          try
            let written =
              Pty.write_string state.pty_master data offset remaining
            in
            write_with_retry (offset + written) (remaining - written)
          with
          | Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
              Eio.Fiber.yield ();
              write_with_retry offset remaining
          | Unix.Unix_error (Unix.EIO, _, _) ->
              Log.err (fun m ->
                  m "PTY write failed with EIO: shell process has likely exited");
              Error
                (Error.Process_error "Shell process terminated unexpectedly")
          | exn -> Error.io_error "write to PTY" exn
      in
      write_with_retry 0 (String.length data))

(** Read and process PTY output *)
let drain_pty_output state =
  let buf = Bytes.create 4096 in
  let rec drain state had_changes =
    try
      let n =
        Eio.Mutex.use_ro state.pty_mutex (fun () ->
            Pty.read state.pty_master buf 0 (Bytes.length buf))
      in
      if n > 0 then (
        Log.debug (fun m -> m "Read %d bytes from PTY" n);
        Vte.feed state.vte buf 0 n;

        (* Check for changes *)
        if Vte.is_dirty state.vte then
          let grid = Vte.grid state.vte in
          let cursor_row, cursor_col = Vte.cursor_pos state.vte in

          (* Process screen changes *)
          let state' =
            match state.prev_grid with
            | None ->
                (* First time - all rows are new *)
                let grid_rows = Grid.rows grid in
                let changed_rows =
                  List.init grid_rows (fun row ->
                      let row_data = Grid.copy_row grid row in
                      (row, Array.map (fun cell -> Some cell) row_data))
                in
                add_event state
                  (Screen_change { at = state.virtual_time; changed_rows })
            | Some prev ->
                (* Compute diff to find changed cells *)
                let changed_cells = Grid.diff_cells prev grid in
                if changed_cells <> [] then (
                  (* Group changed cells by row *)
                  let rows_map = Hashtbl.create 10 in
                  List.iter
                    (fun (row, col) ->
                      let cols =
                        match Hashtbl.find_opt rows_map row with
                        | None -> []
                        | Some cols -> cols
                      in
                      Hashtbl.replace rows_map row (col :: cols))
                    changed_cells;

                  (* Build changed_rows with only changed cells *)
                  let changed_rows =
                    Hashtbl.fold
                      (fun row cols acc ->
                        (* Get max column to size the array *)
                        let max_col = List.fold_left max 0 cols in
                        let row_changes = Array.make (max_col + 1) None in
                        (* Fill in only the changed cells *)
                        List.iter
                          (fun col ->
                            row_changes.(col) <- Grid.get grid ~row ~col)
                          cols;
                        (row, row_changes) :: acc)
                      rows_map []
                    |> List.sort (fun (r1, _) (r2, _) -> compare r1 r2)
                  in

                  add_event state
                    (Screen_change { at = state.virtual_time; changed_rows }))
                else state
          in

          (* Process cursor changes *)
          let state'' =
            match state'.prev_cursor with
            | None ->
                add_event state'
                  (Cursor_move
                     {
                       at = state'.virtual_time;
                       row = cursor_row;
                       col = cursor_col;
                     })
            | Some (prev_row, prev_col)
              when prev_row <> cursor_row || prev_col <> cursor_col ->
                add_event state'
                  (Cursor_move
                     {
                       at = state'.virtual_time;
                       row = cursor_row;
                       col = cursor_col;
                     })
            | _ -> state'
          in

          (* Update previous state *)
          let state_final =
            {
              state'' with
              prev_grid = Some (Grid.copy grid);
              prev_cursor = Some (cursor_row, cursor_col);
            }
          in

          drain state_final true
        else drain state had_changes)
      else (state, had_changes)
    with Unix.Unix_error ((Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
      (state, had_changes)
  in
  drain state false

(** Sleep while processing output *)
let sleep_with_output state duration =
  (* Only add sleep event if duration is significant *)
  let state' =
    if duration > 0.001 then
      add_event state (Sleep { at = state.virtual_time; duration })
    else state
  in

  (* Drain any immediate output *)
  let state'', _ = drain_pty_output state' in

  (* Advance virtual time *)
  { state'' with virtual_time = state''.virtual_time +. duration }

(** Async PTY reader *)
let read_pty_async ~sw state_ref =
  let pty_fd = Pty.in_fd !state_ref.pty_master in

  let rec pump () =
    (* Drain all available data *)
    let new_state, _ = drain_pty_output !state_ref in
    state_ref := new_state;
    (* Wait for more data *)
    Eio_unix.await_readable pty_fd;
    pump ()
  in

  Eio.Fiber.fork_daemon ~sw (fun () ->
      Log.debug (fun m -> m "PTY reader started");
      try pump ()
      with _ ->
        Log.debug (fun m -> m "PTY reader exiting");
        `Stop_daemon)

(** Convert key to escape sequence *)
let key_to_sequence = function
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

(** Format command for display *)
let format_command = function
  | Set (setting, value) ->
      let setting_str =
        match setting with
        | Shell -> "Shell"
        | FontFamily -> "Font Family"
        | FontSize -> "Font Size"
        | Framerate -> "Framerate"
        | Height -> "Height"
        | LetterSpacing -> "Letter Spacing"
        | LineHeight -> "Line Height"
        | LoopOffset -> "Loop Offset"
        | Padding -> "Padding"
        | PlaybackSpeed -> "Playback Speed"
        | Theme -> "Theme"
        | TypingSpeed -> "Typing Speed"
        | Width -> "Width"
        | WindowBar -> "Window Bar"
        | WindowBarSize -> "Window Bar Size"
        | BorderRadius -> "Border Radius"
        | Margin -> "Margin"
        | MarginFill -> "Margin Fill"
        | WaitTimeout -> "Wait Timeout"
        | WaitPattern -> "Wait Pattern"
        | CursorBlink -> "Cursor Blink"
      in
      let value_str =
        match value with
        | String s -> s
        | Float f -> string_of_float f
        | Int i -> string_of_int i
        | Bool b -> string_of_bool b
        | Json s -> s
      in
      Printf.sprintf "Set %s %s" setting_str value_str
  | Type { text; speed = _ } -> Printf.sprintf "Type %s" text
  | KeyPress { key; count; speed = _ } ->
      let key_str =
        match key with
        | Enter -> "Enter"
        | Tab -> "Tab"
        | Space -> "Space"
        | Backspace -> "Backspace"
        | Delete -> "Delete"
        | Escape -> "Escape"
        | Up -> "Up"
        | Down -> "Down"
        | Left -> "Left"
        | Right -> "Right"
        | PageUp -> "PageUp"
        | PageDown -> "PageDown"
        | Home -> "Home"
        | End -> "End"
        | Insert -> "Insert"
      in
      if count > 1 then Printf.sprintf "%s %d" key_str count else key_str
  | Sleep duration ->
      if duration < 1.0 then
        Printf.sprintf "Sleep %dms" (int_of_float (duration *. 1000.0))
      else Printf.sprintf "Sleep %gs" duration
  | Hide -> "Hide"
  | Show -> "Show"
  | Copy s -> Printf.sprintf "Copy %S" s
  | Paste -> "Paste"
  | Output s -> Printf.sprintf "Output %s" s
  | _ -> ""

(** Handle a single command *)
let handle_command state = function
  | Set (setting, value) -> (
      match Config.apply_setting state.config setting value with
      | Ok config -> Ok { state with config }
      | Error msg -> Error (Error.Invalid_config msg))
  | Type { text; speed } ->
      let delay = Option.value speed ~default:state.config.typing_speed in
      let open Error.Syntax in
      let rec type_chars state = function
        | [] -> Ok state
        | char :: rest ->
            (* Get current cursor position *)
            let start_row, start_col = Vte.cursor_pos state.vte in
            let expected_col = start_col + 1 in

            (* Advance virtual time BEFORE writing *)
            let state =
              { state with virtual_time = state.virtual_time +. delay }
            in
            let* () = write_to_pty state (String.make 1 char) in

            (* Wait for cursor to advance or screen to update *)
            let rec wait_for_echo state attempts =
              if attempts > 100 then
                (* Timeout after ~100ms *)
                Ok state
              else
                let state', _ = drain_pty_output state in
                let new_row, new_col = Vte.cursor_pos state'.vte in
                if new_col = expected_col || new_row > start_row then
                  (* Character was processed *)
                  Ok state'
                else (
                  (* Wait a bit more *)
                  Eio.Fiber.yield ();
                  wait_for_echo state' (attempts + 1))
            in
            let* state' = wait_for_echo state 0 in
            type_chars state' rest
      in
      type_chars state (String.to_seq text |> List.of_seq)
  | KeyPress { key; count; speed } ->
      let delay = Option.value speed ~default:0.1 in
      let key_sequence = key_to_sequence key in
      let open Error.Syntax in
      let rec press_key state n =
        if n = 0 then Ok state
        else
          (* Advance virtual time BEFORE writing *)
          let state =
            { state with virtual_time = state.virtual_time +. delay }
          in
          let* () = write_to_pty state key_sequence in
          (* Drain output after key press *)
          let state', _ = drain_pty_output state in
          press_key state' (n - 1)
      in
      press_key state count
  | Sleep duration -> Ok (sleep_with_output state duration)
  | Hide | Show ->
      (* Recording state doesn't affect event log *)
      Ok state
  | Copy s -> Ok { state with clipboard = s }
  | Paste ->
      let open Error.Syntax in
      let* () = write_to_pty state state.clipboard in
      Ok state
  | _ ->
      (* Other commands not relevant for event log *)
      Ok state

(** Wait for shell prompt *)
let wait_for_prompt state =
  (* Just drain any immediate output *)
  let state', _ = drain_pty_output state in
  (* The prompt should appear quickly, no need for complex waiting *)
  state'

(** Run tape and produce event log *)
let run ~sw ~env:_ tape =
  let open Error.Syntax in
  (* Process initial config *)
  let* initial_config =
    Config.from_tape tape
    |> Result.map_error (fun msg -> Error.Invalid_config msg)
  in

  (* Calculate dimensions *)
  let char_width = max 6 (initial_config.font_size * 6 / 10) in
  let char_height = max 8 (initial_config.font_size * 12 / 10) in
  let term_cols = initial_config.width / char_width in
  let term_rows = initial_config.height / char_height in

  (* Validate dimensions *)
  let* () =
    if term_cols > 0 && term_rows > 0 then Ok ()
    else
      Error
        (Error.Invalid_config
           (Printf.sprintf "Invalid terminal dimensions: %dx%d" term_cols
              term_rows))
  in

  (* Create VTE *)
  let vte = Vte.create ~rows:term_rows ~cols:term_cols () in

  (* Get shell spawn info *)
  let prog, argv, unix_env = shell_spawn_info initial_config in
  let winsize = { Pty.rows = term_rows; cols = term_cols; x = 0; y = 0 } in

  (* Spawn shell *)
  Log.debug (fun m -> m "Spawning shell: %s" prog);
  Printf.eprintf "[DEBUG] About to spawn shell %s\n" prog;
  flush stderr;
  let* pty_master =
    try
      let args_without_prog = Array.to_list argv |> List.tl in
      let pty =
        Pty.spawn ~prog ~args:args_without_prog ~winsize ~env:unix_env ()
      in
      Printf.eprintf "[DEBUG] Shell spawned successfully\n";
      flush stderr;
      Ok pty
    with
    | Unix.Unix_error (err, fn, arg) ->
        Error
          (Error.Process_error
             (Printf.sprintf "Failed to spawn shell %s: %s(%s): %s" prog fn arg
                (Unix.error_message err)))
    | exn ->
        Error
          (Error.Process_error
             (Printf.sprintf "Failed to spawn shell %s: %s" prog
                (Printexc.to_string exn)))
  in
  Pty.set_nonblock pty_master;

  (* Create initial state *)
  let initial_state =
    {
      pty_master;
      vte;
      virtual_time = 0.0;
      events = [];
      clipboard = "";
      config = initial_config;
      prev_grid = None;
      prev_cursor = None;
      pty_mutex = Eio.Mutex.create ();
    }
  in

  (* Cleanup on exit *)
  Eio.Switch.on_release sw (fun () -> Pty.close pty_master);

  (* Use a mutable reference for async PTY reader *)
  let state_ref = ref initial_state in

  (* Start async PTY reader *)
  read_pty_async ~sw state_ref;

  (* Wait for prompt *)
  let state = wait_for_prompt !state_ref in
  state_ref := state;

  (* Record initial screen state *)
  let grid = Vte.grid state.vte in
  let cursor_row, cursor_col = Vte.cursor_pos state.vte in
  let state =
    {
      state with
      prev_grid = Some (Grid.copy grid);
      prev_cursor = Some (cursor_row, cursor_col);
    }
  in
  state_ref := state;

  (* Execute commands *)
  let* final_state =
    List.fold_left
      (fun state_res cmd ->
        let* state = state_res in

        (* Print command if it should be displayed *)
        (match cmd with
        | Output _ -> () (* Output is printed in main.ml *)
        | _ ->
            let cmd_str = format_command cmd in
            if cmd_str <> "" then (
              Printf.printf "%s\n" cmd_str;
              flush stdout));

        let* new_state = handle_command state cmd in

        (* Update the reference for async reader *)
        state_ref := new_state;

        (* After each command, yield to let PTY reader process any output *)
        Eio.Fiber.yield ();

        (* Drain any output that arrived *)
        let drained_state, _ = drain_pty_output !state_ref in
        state_ref := drained_state;
        Ok drained_state)
      (Ok state) tape
  in

  (* Final drain *)
  let final_state, _ = drain_pty_output final_state in

  (* Return sorted event log and final VTE state *)
  Ok (Event.sort_log final_state.events, vte, initial_config)
