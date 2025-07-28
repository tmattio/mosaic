(* program.ml - Core Mosaic runtime toolkit *)

type t = {
  mutable model : Obj.t; (* Generic model storage *)
  mutable running : bool;
  mutable quit_pending : bool;
  term : Tty.t;
  event_source : Event_source.t;
  mutable alt_screen : bool;
  mouse : bool;
  fps : int;
  mutable previous_buffer : Render.buffer option;
  clock : float Eio.Time.clock_ty Eio.Std.r;
  sw : Eio.Switch.t;
  env : Eio_unix.Stdenv.base;
  debug_log : out_channel option;
  mutable static_elements : Ui.element list;
  terminal_mutex : Eio.Mutex.t;
  state_mutex : Eio.Mutex.t;
  mutable last_static_height : int;
  mutable last_printed_static : int;
  mutable previous_dynamic_buffer : Render.buffer option;
  mutable last_width : int;
  resize_cond : Eio.Condition.t;
  mutable pending_updates : bool;
  mutable sigint_prev : Sys.signal_behavior option;
  mutable sigterm_prev : Sys.signal_behavior option;
  mutable sighup_prev : Sys.signal_behavior option;
}

type config = {
  terminal : Tty.t option;
  alt_screen : bool;
  mouse : bool;
  fps : int;
  debug_log : out_channel option;
}

let log_debug (t : t) s =
  match t.debug_log with
  | Some chan ->
      let time = Unix.gettimeofday () in
      Printf.fprintf chan "[%f] %s\n%!" time s
  | None -> ()

let create ~sw ~env config =
  let term =
    match config.terminal with
    | Some term -> term
    | None -> Tty.create ~tty:true Unix.stdin Unix.stdout
  in
  let event_source = Event_source.create ~sw ~env ~mouse:config.mouse term in
  {
    model = Obj.repr ();
    (* Will be set by the API layer *)
    running = true;
    quit_pending = false;
    term;
    event_source;
    alt_screen = config.alt_screen;
    mouse = config.mouse;
    fps = config.fps;
    previous_buffer = None;
    clock = Eio.Stdenv.clock env;
    sw;
    env;
    debug_log = config.debug_log;
    static_elements = [];
    terminal_mutex = Eio.Mutex.create ();
    state_mutex = Eio.Mutex.create ();
    last_static_height = 0;
    last_printed_static = 0;
    previous_dynamic_buffer = None;
    last_width = fst (Tty.size term);
    resize_cond = Eio.Condition.create ();
    pending_updates = false;
    sigint_prev = None;
    sigterm_prev = None;
    sighup_prev = None;
  }

(*
  Command Processing Logic

  The core of the command processing is the `go` function, which takes an
  execution `mode` (`Parallel` or `Sequential`).

  - `Parallel` mode is the default for top-level commands. It forks a new fiber
    for each asynchronous command (`Perform`, `Exec`, `Tick`), allowing them to
    run concurrently without blocking the message loop.

  - `Sequential` mode is used for commands inside `Cmd.Sequence`. It executes
    asynchronous commands in the *current* fiber, effectively blocking until
    they complete before proceeding. This ensures that commands in a sequence
    run one after another, as intended.

  - `Cmd.Batch` always runs its children in parallel, but if it's inside a
    `Cmd.Sequence`, the sequence will wait for the entire batch to complete
    before moving to the next command.
*)
type exec_mode = Parallel | Sequential

let process_cmd t dispatch cmd =
  let rec go mode cmd =
    let run_in_mode work =
      match mode with
      | Parallel -> Eio.Fiber.fork ~sw:t.sw work
      | Sequential -> work ()
    in
    match cmd with
    | Cmd.None -> ()
    | Cmd.Msg m -> dispatch m
    | Cmd.Batch cmds ->
        (* A batch runs all its commands in parallel, regardless of the current mode. *)
        List.iter (go Parallel) cmds
    | Cmd.Sequence cmds ->
        (* A sequence runs all its commands sequentially. If it's part of a batch,
           the parent fiber will wait for the whole sequence to finish. *)
        List.iter (go Sequential) cmds
    | Cmd.Perform f ->
        run_in_mode (fun () ->
            match f () with Some msg -> dispatch msg | None -> ())
    | Cmd.Perform_eio f ->
        run_in_mode (fun () ->
            match f ~sw:t.sw ~env:t.env with
            | Some msg -> dispatch msg
            | None -> ())
    | Cmd.Exec exec_cmd ->
        run_in_mode (fun () ->
            Eio.Mutex.use_rw ~protect:true t.terminal_mutex (fun () ->
                let cleanup () =
                  (* Do nothing if the program is already quitting *)
                  if t.running then (
                    Tty.restore_state t.term;
                    Tty.hide_cursor t.term;
                    Tty.set_mode t.term `Raw;
                    if t.mouse then Tty.set_mouse_mode t.term `Normal;
                    Tty.enable_kitty_keyboard t.term;
                    if t.alt_screen then Tty.enable_alternate_screen t.term;
                    t.previous_buffer <- None)
                in
                Tty.save_state t.term;
                Tty.show_cursor t.term;
                Tty.set_mouse_mode t.term `None;
                Tty.disable_kitty_keyboard t.term;
                if t.alt_screen then Tty.disable_alternate_screen t.term;
                Tty.set_mode t.term `Cooked;
                Tty.release t.term;
                let clear_and_home =
                  Ansi.clear_screen ^ Ansi.cursor_position 1 1
                in
                Tty.write t.term
                  (Bytes.of_string clear_and_home)
                  0
                  (String.length clear_and_home);
                Tty.flush t.term;
                Fun.protect ~finally:cleanup (fun () ->
                    Eio_unix.run_in_systhread exec_cmd.run));
            dispatch exec_cmd.on_complete)
    | Cmd.Tick (duration, f) ->
        run_in_mode (fun () ->
            let start_time = Eio.Time.now t.clock in
            Eio.Time.sleep t.clock duration;
            let elapsed = Eio.Time.now t.clock -. start_time in
            if t.running then dispatch (f elapsed))
    | Cmd.Quit -> t.quit_pending <- true
    | Cmd.Log message -> Printf.eprintf "%s\n%!" message
    | Cmd.Print element ->
        t.static_elements <- t.static_elements @ [ element ];
        t.previous_buffer <- None;
        if not t.alt_screen then
          let width, _ = Tty.size t.term in
          let _, el_h = Ui.measure ~width element in
          t.last_static_height <- t.last_static_height + el_h
    | Cmd.Set_window_title title ->
        Eio.Mutex.use_rw ~protect:true t.terminal_mutex (fun () ->
            Tty.write t.term
              (Bytes.of_string (Ansi.set_window_title title))
              0
              (String.length (Ansi.set_window_title title));
            Tty.flush t.term)
    | Cmd.Enter_alt_screen ->
        if not t.alt_screen then (
          log_debug t "Entering alternate screen";
          Eio.Mutex.use_rw ~protect:true t.terminal_mutex (fun () ->
              Tty.enable_alternate_screen t.term);
          t.alt_screen <- true;
          t.previous_buffer <- None)
    | Cmd.Exit_alt_screen ->
        if t.alt_screen then (
          log_debug t "Exiting alternate screen";
          Eio.Mutex.use_rw ~protect:true t.terminal_mutex (fun () ->
              Tty.disable_alternate_screen t.term);
          t.alt_screen <- false;
          t.previous_buffer <- None;
          t.previous_dynamic_buffer <- None;
          t.last_static_height <- 0;
          t.last_printed_static <- 0)
    | Cmd.Repaint ->
        log_debug t "Forcing repaint";
        t.previous_buffer <- None
    | Cmd.Clear_screen ->
        log_debug t "Clearing screen";
        Eio.Mutex.use_rw ~protect:true t.terminal_mutex (fun () ->
            let reset_seq = Ansi.clear_screen ^ Ansi.esc ^ "H" in
            Tty.write t.term
              (Bytes.of_string reset_seq)
              0 (String.length reset_seq);
            Tty.flush t.term);
        t.static_elements <- [];
        t.last_static_height <- 0;
        t.last_printed_static <- 0
    | Cmd.Clear_terminal ->
        log_debug t "Clearing terminal";
        Eio.Mutex.use_rw ~protect:true t.terminal_mutex (fun () ->
            let reset_seq = Ansi.clear_terminal in
            Tty.write t.term
              (Bytes.of_string reset_seq)
              0 (String.length reset_seq);
            Tty.flush t.term);
        t.static_elements <- [];
        t.last_static_height <- 0;
        t.last_printed_static <- 0;
        t.previous_dynamic_buffer <- None;
        t.previous_buffer <- None
  in
  (* All top-level commands are executed in parallel by default *)
  go Parallel cmd

let render_element t dynamic_element =
  log_debug t "Render: Starting render pass";
  let width, height = Tty.size t.term in
  let buffer, non_alt_output =
    Eio.Mutex.use_ro t.state_mutex (fun () ->
        let static_elements_snapshot = t.static_elements in

        let buffer = Render.create width height in

        if t.alt_screen then (
          (* Alt-screen: Keep combined vbox for full buffer render *)
          let combined_element =
            if static_elements_snapshot = [] then dynamic_element
            else
              Ui.vbox
                (static_elements_snapshot
                @ [ Ui.vbox ~flex_grow:1 [ dynamic_element ] ])
          in
          Ui.render buffer combined_element;
          (buffer, ""))
        else
          (* Non-alt-screen: Append new static relatively, clear/redraw dynamic *)
          let output_buf = Buffer.create 1024 in

          (* Append only new static elements (delta since last render) *)
          let new_static_start = t.last_printed_static in
          let new_statics =
            let rec drop n lst =
              if n <= 0 then lst
              else match lst with [] -> [] | _ :: tl -> drop (n - 1) tl
            in
            drop new_static_start static_elements_snapshot
          in

          (* Render dynamic to buffer *)
          let _, dynamic_height = Ui.measure ~width dynamic_element in
          let dyn_buffer = Render.create width dynamic_height in
          Ui.render dyn_buffer dynamic_element;

          (* Get previous height *)
          let previous_height =
            match t.previous_dynamic_buffer with
            | None -> 0
            | Some b -> snd (Render.dimensions b)
          in

          (* Detect if full redraw needed *)
          let force_full =
            new_statics <> []
            || t.previous_dynamic_buffer = None
            || previous_height <> dynamic_height
            || width <> t.last_width
          in
          t.last_width <- width;

          (* Position to old dynamic start if previous exists *)
          if previous_height > 0 then (
            Buffer.add_string output_buf (Ansi.cursor_up previous_height);
            Buffer.add_string output_buf "\r");

          if force_full && previous_height > 0 then (
            (* Replace with explicit per-line erase to clear the old dynamic area *)
            let erase_line = "\r" ^ Ansi.clear_line ^ "\n" in
            (* \r \x1b[2K \n *)
            for _ = 0 to previous_height - 1 do
              Buffer.add_string output_buf erase_line
            done;
            (* Reposition cursor back to the start of the cleared area *)
            Buffer.add_string output_buf (Ansi.cursor_up previous_height);
            Buffer.add_string output_buf "\r");

          (* Append new static elements *)
          let added_height = ref 0 in
          List.iter
            (fun el ->
              let _, el_h = Ui.measure ~width el in
              let el_buffer = Render.create width el_h in
              Ui.render el_buffer el;
              let rendered =
                Render.render_full ~mode:Render.Relative el_buffer
              in
              Buffer.add_string output_buf rendered;
              Buffer.add_string output_buf "\r\n";
              added_height := !added_height + el_h)
            new_statics;
          t.last_printed_static <- List.length static_elements_snapshot;
          t.last_static_height <- t.last_static_height + !added_height;

          (* Prepare and output dynamic *)
          let dynamic_height = snd (Render.dimensions dyn_buffer) in
          let dyn_output =
            if force_full then
              Render.render_full ~mode:Render.Relative dyn_buffer ^ "\r\n"
            else
              (* previous_dynamic_buffer should always be set here,
           so we can safely use it for diffing *)
              let prev_buf = Option.get t.previous_dynamic_buffer in
              let patches = Render.diff prev_buf dyn_buffer in
              if patches = [] then Ansi.cursor_down dynamic_height ^ "\r"
              else
                let sorted_patches =
                  List.sort
                    (fun (p1 : Render.patch) (p2 : Render.patch) ->
                      match (p1, p2) with
                      | ( Change { row = r1; col = c1; _ },
                          Change { row = r2; col = c2; _ } ) ->
                          if r1 = r2 then Int.compare c1 c2
                          else Int.compare r1 r2
                      | Clear _, Change _ -> -1
                      | Change _, Clear _ -> 1
                      | ( Clear { row = r1; col = c1; _ },
                          Clear { row = r2; col = c2; _ } ) ->
                          if r1 = r2 then Int.compare c1 c2
                          else Int.compare r1 r2)
                    patches
                in
                let patch_buf = Buffer.create 1024 in
                let current_row = ref 0 in
                let current_col = ref 0 in
                List.iter
                  (fun (p : Render.patch) ->
                    match p with
                    | Change { row; col; new_cell } ->
                        (* Move down if needed *)
                        let row_diff = row - !current_row in
                        if row_diff > 0 then (
                          Buffer.add_string patch_buf
                            (Ansi.cursor_down row_diff);
                          Buffer.add_string patch_buf "\r";
                          current_col := 0);
                        current_row := row;

                        (* Move forward on line *)
                        let col_diff = col - !current_col in
                        if col_diff > 0 then
                          Buffer.add_string patch_buf
                            (Ansi.cursor_forward col_diff);
                        current_col := col + new_cell.width;

                        (* Apply style and content *)
                        let sgr_codes =
                          let attrs = [] in
                          let attrs =
                            match new_cell.attr.fg with
                            | Some c -> `Fg c :: attrs
                            | None -> attrs
                          in
                          let attrs =
                            match new_cell.attr.bg with
                            | Some c -> `Bg c :: attrs
                            | None -> attrs
                          in
                          let attrs =
                            if new_cell.attr.bold then `Bold :: attrs else attrs
                          in
                          let attrs =
                            if new_cell.attr.dim then `Dim :: attrs else attrs
                          in
                          let attrs =
                            if new_cell.attr.italic then `Italic :: attrs
                            else attrs
                          in
                          let attrs =
                            if new_cell.attr.underline then `Underline :: attrs
                            else attrs
                          in
                          let attrs =
                            if new_cell.attr.blink then `Blink :: attrs
                            else attrs
                          in
                          let attrs =
                            if new_cell.attr.reverse then `Reverse :: attrs
                            else attrs
                          in
                          let attrs =
                            if new_cell.attr.strikethrough then
                              `Strikethrough :: attrs
                            else attrs
                          in
                          Ansi.sgr attrs
                        in
                        Buffer.add_string patch_buf sgr_codes;
                        let content =
                          match new_cell.chars with
                          | [] -> " "
                          | chars ->
                              let b = Buffer.create 8 in
                              List.iter (Uutf.Buffer.add_utf_8 b) chars;
                              Buffer.contents b
                        in
                        let styled_content =
                          match new_cell.attr.uri with
                          | Some uri -> Ansi.hyperlink ~uri content
                          | None -> content
                        in
                        Buffer.add_string patch_buf styled_content;
                        Buffer.add_string patch_buf Ansi.reset
                    | Clear _ -> () (* TODO: Handle clear patches *))
                  sorted_patches;
                Buffer.add_string patch_buf
                  (Ansi.cursor_down (dynamic_height - !current_row) ^ "\r");
                Buffer.contents patch_buf
          in
          Buffer.add_string output_buf dyn_output;

          (* Clear excess if height decreased - no extra cursor_down needed *)
          if dynamic_height < previous_height then
            Buffer.add_string output_buf Ansi.clear_screen_below;

          (* Update state *)
          t.previous_dynamic_buffer <- Some dyn_buffer;
          t.previous_buffer <- None;

          (* Return buffer and output string *)
          (buffer, Buffer.contents output_buf))
  in

  (* Calculate a simple hash of buffer content for debugging *)
  let buffer_hash buffer =
    let w, h = Render.dimensions buffer in
    let hash = ref 0 in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        let cell = Render.get buffer x y in
        match cell.Render.chars with
        | [] -> ()
        | c :: _ -> hash := !hash + Uchar.to_int c + x + (y * 1000)
      done
    done;
    !hash
  in
  log_debug t (Printf.sprintf "Current buffer hash: %d" (buffer_hash buffer));

  (* Prepare output based on mode *)
  let output =
    if not t.alt_screen then
      (* Non-alt-screen output was already prepared *)
      non_alt_output
    else
      (* Alt-screen mode uses buffer-based rendering *)
      match t.previous_buffer with
      | None ->
          (* First render *)
          log_debug t "Render: Full redraw (no previous buffer)";
          Render.render_full buffer
      | Some prev_buf ->
          (* Subsequent renders - use diff *)
          log_debug t
            (Printf.sprintf "Previous buffer hash: %d" (buffer_hash prev_buf));
          let patches = Render.diff prev_buf buffer in
          if patches = [] then (
            log_debug t "Render: No changes detected (0 patches)";
            "" (* Return empty string when no changes *))
          else (
            log_debug t
              (Printf.sprintf "Render: %d patches detected"
                 (List.length patches));
            Render.render_patches patches)
  in

  (* Log output with preview for debugging *)
  if String.length output > 0 then
    let preview =
      if String.length output > 50 then
        String.escaped (String.sub output 0 50) ^ "..."
      else String.escaped output
    in
    log_debug t
      (Printf.sprintf "Writing %d bytes: %s" (String.length output) preview)
  else log_debug t "Writing 0 bytes (no output)";

  (* Only write to terminal if t is still running AND there's output *)
  if t.running && String.length output > 0 then
    Eio.Mutex.use_rw ~protect:true t.terminal_mutex (fun () ->
        Tty.write t.term (Bytes.of_string output) 0 (String.length output);
        Tty.flush t.term);

  (* Only cache buffer for alt-screen mode *)
  if t.alt_screen then t.previous_buffer <- Some buffer

let render t element = render_element t element

let run_input_loop t handle_event =
  while t.running do
    let timeout = Some (1.0 /. float_of_int t.fps) in
    match Event_source.read t.event_source ~clock:t.clock ~timeout with
    | `Event event -> handle_event event
    | `Timeout -> ()
  done

let run_render_loop t get_element =
  log_debug t "Starting render loop";
  let frame_duration = 1.0 /. float_of_int t.fps in
  while t.running do
    (* Protect the render call from cancellation. If a cancel happens during
       render, it will be deferred until the render is complete, preventing
       the terminal mutex from being poisoned. The cancellation will then be
       processed during the subsequent sleep. *)
    Eio.Cancel.protect (fun () -> render t (get_element ()));
    Eio.Time.sleep t.clock frame_duration
  done

let full_cleanup term alt_screen_was_on =
  (* A comprehensive, single-string reset for the terminal state Mosaic modifies.
     This can be used for both graceful shutdown and signal handling. *)
  let common_seq =
    Ansi.cursor_show ^ Ansi.mouse_off ^ Ansi.bracketed_paste_off
  in
  let cleanup_seq =
    if alt_screen_was_on then
      common_seq ^ Ansi.kitty_keyboard_off ^ Ansi.alternate_screen_off
    else common_seq ^ Ansi.kitty_keyboard_off
  in
  Tty.write term (Bytes.of_string cleanup_seq) 0 (String.length cleanup_seq);
  Tty.set_mode term `Cooked;
  Tty.flush term

let setup_terminal t =
  log_debug t "Setting up terminal";
  Eio.Mutex.use_rw ~protect:true t.terminal_mutex (fun () ->
      (* Force-reset terminal features to a known-good state before enabling ours. *)
      let reset_seq = Ansi.mouse_off ^ Ansi.bracketed_paste_off in
      Tty.write t.term (Bytes.of_string reset_seq) 0 (String.length reset_seq);

      Tty.set_mode t.term `Raw;
      Tty.hide_cursor t.term;
      if t.alt_screen then (
        log_debug t "Enabling alternate screen";
        Tty.enable_alternate_screen t.term);
      if t.mouse then Tty.set_mouse_mode t.term `Normal;
      Tty.enable_kitty_keyboard t.term;
      Tty.enable_bracketed_paste t.term)

let run_resize_loop t handle_resize =
  while t.running do
    Eio.Condition.await_no_mutex t.resize_cond;
    if t.running then handle_resize (Tty.size t.term)
  done

let setup_signal_handlers (t : t) =
  (* The state of alt_screen can change, so we capture its initial value for the signal handler. *)
  let initial_alt_screen = t.alt_screen in
  let handler _signum =
    (* THIS IS A SIGNAL HANDLER. DO NOT ALLOCATE, USE EIO, OR DO ANYTHING COMPLEX. *)
    (* We perform a direct, low-level cleanup and then exit. *)
    full_cleanup t.term initial_alt_screen;
    exit 130 (* Standard exit code for termination by Ctrl+C *)
  in
  t.sigint_prev <- Some (Sys.signal Sys.sigint (Sys.Signal_handle handler));
  t.sigterm_prev <- Some (Sys.signal Sys.sigterm (Sys.Signal_handle handler));
  t.sighup_prev <- Some (Sys.signal Sys.sighup (Sys.Signal_handle handler));

  (* SIGWINCH handler to notify the app of terminal resizes. *)
  Tty.set_resize_handler t.term (fun _ -> Eio.Condition.broadcast t.resize_cond)

let cleanup t =
  Tty.remove_resize_handlers t.term;
  try
    Eio.Mutex.use_rw ~protect:true t.terminal_mutex (fun () ->
        full_cleanup t.term t.alt_screen)
  with Eio.Mutex.Poisoned _ ->
    (* The mutex was poisoned, likely by a fiber (e.g. from Cmd.Exec) that
       was cancelled while holding the lock. The terminal state is unknown,
       but we must try to restore it. We run the cleanup without the lock,
       which is safe because all other fibers are terminated at this point. *)
    full_cleanup t.term t.alt_screen;

    (* Restore signal handlers *)
    let restore_signal sig_no saved =
      Sys.set_signal sig_no (Option.value saved ~default:Sys.Signal_default)
    in
    restore_signal Sys.sigint t.sigint_prev;
    restore_signal Sys.sigterm t.sigterm_prev;
    restore_signal Sys.sighup t.sighup_prev

(* Accessors and utility functions *)
let is_running t = t.running
let set_running t running = t.running <- running
let is_quit_pending t = t.quit_pending
let set_quit_pending t pending = t.quit_pending <- pending
let set_model t model = t.model <- Obj.repr model
let get_model t = Obj.obj t.model
let with_state_mutex t ~protect f = Eio.Mutex.use_rw ~protect t.state_mutex f

let with_terminal_mutex t ~protect f =
  Eio.Mutex.use_rw ~protect t.terminal_mutex f

let invalidate_buffer t = t.previous_buffer <- None

let add_static_element t element =
  t.static_elements <- t.static_elements @ [ element ]

let clear_static_elements t = t.static_elements <- []
let clock t = t.clock
let switch t = t.sw
let env t = t.env
let resize_condition t = t.resize_cond
let get_pending_updates t = t.pending_updates
let set_pending_updates t pending = t.pending_updates <- pending
let terminal t = t.term
