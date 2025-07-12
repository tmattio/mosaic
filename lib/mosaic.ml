(* Mosaic - A delightful OCaml TUI framework inspired by The Elm Architecture *)

(* Internal modules *)
module Ansi = Ansi
module Render = Render

(* The Elm Architecture: Model-View-Update with effects and subscriptions *)
type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> Ui.element;
  subscriptions : 'model -> 'msg Sub.t;
}

(* Public modules *)
module Style = Render.Style
module Ui = Ui
module Cmd = Cmd
module Sub = Sub
module Terminal = Terminal
module Input = Input
module Event_source = Event_source

module Program = struct
  type ('model, 'msg) t = {
    mutable model : 'model;
    mutable running : bool;
    app : ('model, 'msg) app;
    term : Terminal.t;
    event_source : Event_source.t;
    alt_screen : bool;
    mouse : bool;
    fps : int;
    mutable previous_buffer : Render.buffer option;
    msg_stream : 'msg Eio.Stream.t;
    clock : float Eio.Time.clock_ty Eio.Std.r;
    sw : Eio.Switch.t;
    cmd_queue : 'msg Cmd.t Queue.t; (* Command queue for sequential execution *)
    debug_log : out_channel option; (* Debug logging *)
    mutable lines_rendered : int;
        (* Track lines rendered in non-alt-screen mode *)
    mutable print_queue : string list;
        (* Queue for print messages in non-alt-screen mode *)
  }

  let log_debug program s =
    match program.debug_log with
    | Some chan ->
        let time = Unix.gettimeofday () in
        Printf.fprintf chan "[%f] %s\n%!" time s
    | None -> ()

  let create_program ~env ~sw ?terminal ?(alt_screen = true) ?(mouse = false)
      ?(fps = 60) ?debug_log app =
    let term =
      match terminal with
      | Some t -> t
      | None -> Terminal.create ~tty:true Unix.stdin Unix.stdout
    in
    let event_source = Event_source.create term in
    let model, _init_cmd = app.init () in
    let msg_stream = Eio.Stream.create 100 in
    {
      model;
      running = true;
      app;
      term;
      event_source;
      alt_screen;
      mouse;
      fps;
      previous_buffer = None;
      msg_stream;
      clock = Eio.Stdenv.clock env;
      sw;
      cmd_queue = Queue.create ();
      debug_log;
      lines_rendered = 0;
      print_queue = [];
    }

  let send_msg program msg =
    let model, cmd = program.app.update msg program.model in
    program.model <- model;
    cmd

  let rec process_cmd program cmd =
    let open Eio.Std in
    match cmd with
    | Cmd.None -> ()
    | Cmd.Msg m -> Eio.Stream.add program.msg_stream m
    | Cmd.Batch cmds -> List.iter (process_cmd program) cmds
    | Cmd.Perform f ->
        Fiber.fork ~sw:program.sw (fun () ->
            match f () with
            | Some msg -> Eio.Stream.add program.msg_stream msg
            | None -> ())
    | Cmd.Exec exec_cmd ->
        Fiber.fork ~sw:program.sw (fun () ->
            (* Define cleanup function *)
            let cleanup () =
              Terminal.restore_state program.term;
              Terminal.hide_cursor program.term;
              Terminal.set_mode program.term `Raw;
              if program.mouse then Terminal.enable_mouse program.term;
              if program.alt_screen then
                Terminal.enable_alternate_screen program.term;
              program.previous_buffer <- None
            in

            (* Release terminal to normal state *)
            Terminal.save_state program.term;
            Terminal.show_cursor program.term;
            Terminal.disable_mouse program.term;
            Terminal.disable_alternate_screen program.term;
            Terminal.set_mode program.term `Cooked;
            Terminal.release program.term;

            (* Clear screen and move cursor to top *)
            Terminal.write program.term (Bytes.of_string "\x1b[2J\x1b[H") 0 6;
            Terminal.flush program.term;

            (* Execute with protection *)
            Fun.protect ~finally:cleanup (fun () ->
                Eio_unix.run_in_systhread exec_cmd.run);

            (* Send completion message *)
            Eio.Stream.add program.msg_stream exec_cmd.on_complete)
    | Cmd.Tick (duration, f) ->
        (* Use fork_daemon so the timer doesn't block program shutdown *)
        Fiber.fork_daemon ~sw:program.sw (fun () ->
            let start_time = Eio.Time.now program.clock in
            Eio.Time.sleep program.clock duration;
            let elapsed = Eio.Time.now program.clock -. start_time in
            (* Only send message if program is still running *)
            if program.running then
              Eio.Stream.add program.msg_stream (f elapsed);
            `Stop_daemon)
    | Cmd.Sequence cmds -> (
        (* Process commands sequentially - only the first command goes to the queue,
           the rest are wrapped in a new Sequence command that will be processed after *)
        match cmds with
        | [] -> ()
        | [ cmd ] -> Queue.add cmd program.cmd_queue
        | h :: t ->
            Queue.add h program.cmd_queue;
            Queue.add (Cmd.Sequence t) program.cmd_queue)
    | Cmd.Quit ->
        program.running <- false;
        (* Give other loops a chance to exit *)
        Eio.Fiber.yield ();
        ()
    | Cmd.Log message ->
        (* Write to stderr to avoid corrupting the UI *)
        Printf.eprintf "%s\n%!" message
    | Cmd.Print message ->
        if program.alt_screen then
          (* In alt-screen mode, print to stderr like Log *)
          Printf.eprintf "%s\n%!" message
        else
          (* In non-alt-screen mode, queue for later printing *)
          program.print_queue <- program.print_queue @ [ message ]
    | Cmd.Set_window_title title ->
        (* Write the escape sequence to set window title *)
        Terminal.write program.term
          (Bytes.of_string (Ansi.set_window_title title))
          0
          (String.length (Ansi.set_window_title title));
        Terminal.flush program.term

  let handle_input_event program event =
    log_debug program (Format.asprintf "Input event: %a" Input.pp_event event);
    let subs = program.app.subscriptions program.model in
    let msgs =
      match event with
      | Input.Key key_event ->
          let keyboard_handlers = Sub.collect_keyboard [] subs in
          List.filter_map (fun f -> f key_event) keyboard_handlers
      | Input.Focus ->
          let focus_handlers = Sub.collect_focus [] subs in
          List.filter_map (fun f -> f ()) focus_handlers
      | Input.Blur ->
          let blur_handlers = Sub.collect_blur [] subs in
          List.filter_map (fun f -> f ()) blur_handlers
      | Input.Mouse mouse_event ->
          let mouse_handlers = Sub.collect_mouse [] subs in
          List.filter_map (fun f -> f mouse_event) mouse_handlers
      | Input.Resize (w, h) ->
          (* Invalidate the previous buffer on resize.
             This prevents the diffing logic in the render loop from comparing
             buffers of two different sizes, which would cause a crash.
             By setting it to None, we force a full redraw on the next frame. *)
          program.previous_buffer <- None;

          let window_handlers = Sub.collect_window [] subs in
          let size = { Sub.width = w; Sub.height = h } in
          List.filter_map (fun f -> f size) window_handlers
      | _ -> []
    in
    log_debug program
      (Format.asprintf "Generated %d messages from input event"
         (List.length msgs));
    List.iter (Eio.Stream.add program.msg_stream) msgs

  let render program =
    (* Flush any queued print messages before rendering the main view *)
    if program.print_queue <> [] && not program.alt_screen then (
      let output = String.concat "\r\n" program.print_queue ^ "\r\n" in
      (* Write directly, this will scroll the terminal *)
      Terminal.write program.term (Bytes.of_string output) 0
        (String.length output);
      Terminal.flush program.term;
      program.print_queue <- [];
      (* After printing, the old view is gone, so we must force a full repaint
         of the TUI below the new printed lines. *)
      program.previous_buffer <- None);

    log_debug program "Render: Starting render pass";
    let element = program.app.view program.model in

    (* Clear all caches before rendering *)
    Ui.clear_cache element;

    (* Create a new buffer for this frame *)
    let width, height = Terminal.size program.term in
    (* For non-alt-screen mode, use a reasonable height *)
    let actual_height =
      if not program.alt_screen then
        (* Use enough height for the UI but not too much *)
        min height 20 (* Limit to 20 lines for inline mode *)
      else height
    in
    let buffer = Render.create width actual_height in

    (* Render the UI element tree into the buffer using the layout engine *)
    Ui.render buffer element;

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
    log_debug program
      (Printf.sprintf "Current buffer hash: %d" (buffer_hash buffer));

    (* Prepare output based on mode *)
    let output =
      match program.previous_buffer with
      | None ->
          (* First render *)
          log_debug program "Render: Full redraw (no previous buffer)";
          if not program.alt_screen then (
            (* Non-alt-screen mode - first render *)
            let buf = Buffer.create 1024 in
            (* Clear from cursor down *)
            Buffer.add_string buf Ansi.clear_screen_below;

            (* Use render_full with Relative mode for non-alt-screen *)
            let rendered = Render.render_full ~mode:Render.Relative buffer in
            Buffer.add_string buf rendered;

            (* After a full relative render, move to the next line and save position *)
            let _, height = Render.dimensions buffer in
            program.lines_rendered <- height;
            Buffer.add_string buf "\r\n";

            Buffer.contents buf)
          else Render.render_full buffer
      | Some prev_buf ->
          (* Subsequent renders - use diff *)
          log_debug program
            (Printf.sprintf "Previous buffer hash: %d" (buffer_hash prev_buf));
          let patches = Render.diff prev_buf buffer in
          if patches = [] then (
            log_debug program "Render: No changes detected (0 patches)";
            "" (* Return empty string when no changes *))
          else (
            log_debug program
              (Printf.sprintf "Render: %d patches detected"
                 (List.length patches));
            if not program.alt_screen then (
              (* START of REPLACEMENT LOGIC from the plan *)
              let buf = Buffer.create 1024 in
              let prev_height = program.lines_rendered in
              let new_height = snd (Render.dimensions buffer) in

              (* 1. Reposition cursor to the start of the old view area. *)
              if prev_height > 0 then (
                Buffer.add_string buf (Ansi.cursor_up prev_height);
                Buffer.add_string buf "\r");

              (* Save cursor position so we can return here later *)
              Buffer.add_string buf Ansi.cursor_save;

              (* 2. Render the patches, which will move the cursor around. *)
              let patch_str =
                Render.render_patches ~mode:Render.Relative patches
              in
              Buffer.add_string buf patch_str;

              (* 3. If the new view is shorter, clear any leftover lines. *)
              if new_height < prev_height then (
                (* To clear correctly, we must reposition to the end of the new content *)
                Buffer.add_string buf (Ansi.cursor_up prev_height);
                Buffer.add_string buf "\r";
                if new_height > 0 then
                  Buffer.add_string buf (Ansi.cursor_down new_height);
                Buffer.add_string buf Ansi.clear_screen_below);

              (* 4. After all drawing and clearing, explicitly move the cursor
                 to a known, consistent position for the *next* frame. This position
                 should be the beginning of the line AFTER our rendered content. *)

              (* Restore to the saved position (start of our render area) *)
              Buffer.add_string buf Ansi.cursor_restore;

              (* Now, move down to the line just after the new content. *)
              if new_height > 0 then
                Buffer.add_string buf (Ansi.cursor_down new_height);
              Buffer.add_string buf "\r";

              (* 5. Update state for the next frame. *)
              program.lines_rendered <- new_height;

              Buffer.contents buf)
            else Render.render_patches patches)
    in

    (* Log output with preview for debugging *)
    if String.length output > 0 then
      let preview =
        if String.length output > 50 then
          String.escaped (String.sub output 0 50) ^ "..."
        else String.escaped output
      in
      log_debug program
        (Printf.sprintf "Writing %d bytes: %s" (String.length output) preview)
    else log_debug program "Writing 0 bytes (no output)";

    (* Only write to terminal if program is still running AND there's output *)
    if program.running && String.length output > 0 then (
      Terminal.write program.term (Bytes.of_string output) 0
        (String.length output);
      Terminal.flush program.term);

    program.previous_buffer <- Some buffer

  let input_loop program =
    while program.running do
      let timeout = Some (1.0 /. float_of_int program.fps) in
      match
        Event_source.read program.event_source ~sw:program.sw
          ~clock:program.clock ~timeout
      with
      | `Event event -> handle_input_event program event
      | `Timeout -> ()
      | `Eof ->
          program.running <- false;
          ()
    done

  let render_loop program =
    log_debug program "Starting render loop";
    let frame_duration = 1.0 /. float_of_int program.fps in
    while program.running do
      render program;
      Eio.Time.sleep program.clock frame_duration
    done

  let message_loop program =
    while program.running do
      (* Process queued commands first *)
      while not (Queue.is_empty program.cmd_queue) do
        let cmd = Queue.take program.cmd_queue in
        process_cmd program cmd
      done;

      (* Block until a new message arrives *)
      if program.running then (* Check again before blocking *)
        let msg = Eio.Stream.take program.msg_stream in
        if program.running then (
          (* Check again after waking up *)
          log_debug program "Processing message";
          let cmd = send_msg program msg in
          log_debug program
            (Format.asprintf "Command generated: %a"
               (Cmd.pp (fun fmt _ -> Format.fprintf fmt "<msg>"))
               cmd);
          (* If it's a sequence, it will be added to the queue.
             Otherwise, process it immediately *)
          process_cmd program cmd)
    done

  let setup_terminal program =
    log_debug program "Setting up terminal";
    Terminal.set_mode program.term `Raw;
    Terminal.hide_cursor program.term;
    if program.alt_screen then (
      log_debug program "Enabling alternate screen";
      Terminal.enable_alternate_screen program.term);
    if program.mouse then Terminal.enable_mouse program.term;

    (* Setup SIGWINCH handler *)
    let sigwinch_handler (w, h) =
      let resize_event = Input.Resize (w, h) in
      handle_input_event program resize_event
    in
    Terminal.set_sigwinch_handler (Some sigwinch_handler);

    (* Setup termination signal handlers to ensure terminal cleanup *)
    let termination_handler _ =
      (* Ensure terminal is cleaned up before exit *)
      Terminal.set_sigwinch_handler None;
      Terminal.show_cursor program.term;
      Terminal.disable_mouse program.term;
      Terminal.disable_alternate_screen program.term;
      Terminal.set_mode program.term `Cooked;
      Terminal.release program.term;
      exit 0
    in
    (* Install handlers for common termination signals *)
    (try Sys.set_signal Sys.sigterm (Sys.Signal_handle termination_handler)
     with _ -> ());
    (try Sys.set_signal Sys.sigint (Sys.Signal_handle termination_handler)
     with _ -> ());
    try Sys.set_signal Sys.sighup (Sys.Signal_handle termination_handler)
    with _ -> ()

  let cleanup_terminal program =
    Terminal.set_sigwinch_handler None;
    Terminal.flush program.term;
    Terminal.show_cursor program.term;
    Terminal.disable_mouse program.term;
    Terminal.disable_alternate_screen program.term;
    Terminal.set_mode program.term `Cooked;
    Terminal.release program.term

  let run ~sw ~env ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
      ?debug_log app =
    let open Eio.Std in
    let program =
      create_program ~env ~sw ?terminal ~alt_screen ~mouse ~fps ?debug_log app
    in

    log_debug program "===== Program Start =====";

    (* Initialize with init command *)
    let init_cmd = snd (app.init ()) in
    log_debug program
      (Format.asprintf "Initial command: %a"
         (Cmd.pp (fun fmt _ -> Format.fprintf fmt "<msg>"))
         init_cmd);
    Queue.add init_cmd program.cmd_queue;

    setup_terminal program;

    Fun.protect ~finally:(fun () -> cleanup_terminal program) @@ fun () ->
    (* Run all loops concurrently *)
    Fiber.all
      [
        (fun () -> input_loop program);
        (fun () -> render_loop program);
        (fun () -> message_loop program);
      ]
end

(* API functions *)
let app ~init ~update ~view ?(subscriptions = fun _ -> Sub.none) () =
  { init; update; view; subscriptions }

let run ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
    ?(debug = false) app =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  let debug_log = if debug then Some (open_out "mosaic-debug.log") else None in
  Fun.protect
    ~finally:(fun () -> Option.iter close_out debug_log)
    (fun () ->
      Program.run ~sw ~env ?terminal ~alt_screen ~mouse ~fps ?debug_log app)

let run_eio ~sw ~env ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
    ?(debug = false) app =
  let debug_log = if debug then Some (open_out "mosaic-debug.log") else None in
  Fun.protect
    ~finally:(fun () -> Option.iter close_out debug_log)
    (fun () ->
      Program.run ~sw ~env ?terminal ~alt_screen ~mouse ~fps ?debug_log app)
