(* Mosaic - A delightful OCaml TUI framework inspired by The Elm Architecture *)

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
module Component = Component

module Program = struct
  type ('model, 'msg) t = {
    mutable model : 'model;
    mutable running : bool;
    app : ('model, 'msg) app;
    term : Terminal.t;
    event_source : Event_source.t;
    mutable alt_screen : bool;
    mouse : bool;
    fps : int;
    mutable previous_buffer : Render.buffer option;
    msg_stream : 'msg Eio.Stream.t;
    clock : float Eio.Time.clock_ty Eio.Std.r;
    sw : Eio.Switch.t;
    env : Eio_unix.Stdenv.base; (* Eio environment for perform_eio *)
    cmd_queue : 'msg Cmd.t Queue.t; (* Command queue for sequential execution *)
    debug_log : out_channel option; (* Debug logging *)
    mutable static_elements : Ui.element list;
        (* Accumulated static elements for both alt-screen and non-alt-screen modes *)
    terminal_mutex : Eio.Mutex.t; (* Mutex to protect terminal state changes *)
    state_mutex : Eio.Mutex.t; (* Mutex to protect model and static state *)
    mutable last_static_height : int; (* Total height of all static elements *)
    mutable last_printed_static : int;
        (* Number of static elements already printed *)
    mutable previous_dynamic_buffer : Render.buffer option;
        (* Previous dynamic buffer for non-alt-screen diffing *)
    mutable last_width : int; (* Last terminal width to detect resizes *)
    resize_cond : Eio.Condition.t;
        (* Condition for signal handler to wake resize fiber *)
    last_resize_w : int ref; (* Last resize width from signal handler *)
    last_resize_h : int ref; (* Last resize height from signal handler *)
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
    (* Detect terminal background and configure adaptive colors *)
    let is_dark = Terminal.has_dark_background term in
    Render.set_terminal_background ~dark:is_dark;
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
      env;
      cmd_queue = Queue.create ();
      debug_log;
      static_elements = [];
      terminal_mutex = Eio.Mutex.create ();
      state_mutex = Eio.Mutex.create ();
      last_static_height = 0;
      last_printed_static = 0;
      previous_dynamic_buffer = None;
      last_width = fst (Terminal.size term);
      resize_cond = Eio.Condition.create ();
      last_resize_w = ref 0;
      last_resize_h = ref 0;
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
    | Cmd.Perform_eio f ->
        Fiber.fork ~sw:program.sw (fun () ->
            match f ~sw:program.sw ~env:program.env with
            | Some msg -> Eio.Stream.add program.msg_stream msg
            | None -> ())
    | Cmd.Exec exec_cmd ->
        Fiber.fork ~sw:program.sw (fun () ->
            (* Protect all terminal state changes with mutex *)
            Eio.Mutex.use_rw ~protect:true program.terminal_mutex (fun () ->
                (* Define cleanup function *)
                let cleanup () =
                  Terminal.restore_state program.term;
                  Terminal.hide_cursor program.term;
                  Terminal.set_mode program.term `Raw;
                  if program.mouse then Terminal.enable_mouse program.term;
                  Terminal.enable_kitty_keyboard program.term;
                  if program.alt_screen then
                    Terminal.enable_alternate_screen program.term;
                  program.previous_buffer <- None
                in

                (* Release terminal to normal state *)
                Terminal.save_state program.term;
                Terminal.show_cursor program.term;
                Terminal.disable_mouse program.term;
                Terminal.disable_kitty_keyboard program.term;
                Terminal.disable_alternate_screen program.term;
                Terminal.set_mode program.term `Cooked;
                Terminal.release program.term;

                (* Clear screen and move cursor to top *)
                let clear_and_home =
                  Ansi.clear_screen ^ Ansi.cursor_position 1 1
                in
                Terminal.write program.term
                  (Bytes.of_string clear_and_home)
                  0
                  (String.length clear_and_home);
                Terminal.flush program.term;

                (* Execute with protection *)
                Fun.protect ~finally:cleanup (fun () ->
                    Eio_unix.run_in_systhread exec_cmd.run));

            (* Send completion message *)
            Eio.Stream.add program.msg_stream exec_cmd.on_complete)
    | Cmd.Tick (duration, f) ->
        (* Use regular fork to ensure proper cleanup on shutdown *)
        Fiber.fork ~sw:program.sw (fun () ->
            let start_time = Eio.Time.now program.clock in
            Eio.Time.sleep program.clock duration;
            let elapsed = Eio.Time.now program.clock -. start_time in
            (* Only send message if program is still running *)
            if program.running then
              Eio.Stream.add program.msg_stream (f elapsed))
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
        (* Wake up the resize fiber so it can exit *)
        Eio.Condition.broadcast program.resize_cond;
        (* Give other loops a chance to exit *)
        Eio.Fiber.yield ();
        ()
    | Cmd.Log message ->
        (* Write to stderr to avoid corrupting the UI *)
        Printf.eprintf "%s\n%!" message
    | Cmd.Print element ->
        program.static_elements <- program.static_elements @ [ element ];
        program.previous_buffer <- None;
        (* Track element height for non-alt-screen positioning *)
        if not program.alt_screen then
          let width, _ = Terminal.size program.term in
          let _, el_h = Ui.measure ~width element in
          program.last_static_height <- program.last_static_height + el_h
    | Cmd.Set_window_title title ->
        (* Write the escape sequence to set window title *)
        Eio.Mutex.use_rw ~protect:true program.terminal_mutex (fun () ->
            Terminal.write program.term
              (Bytes.of_string (Ansi.set_window_title title))
              0
              (String.length (Ansi.set_window_title title));
            Terminal.flush program.term)
    | Cmd.Enter_alt_screen ->
        if not program.alt_screen then (
          log_debug program "Entering alternate screen";
          Eio.Mutex.use_rw ~protect:true program.terminal_mutex (fun () ->
              Terminal.enable_alternate_screen program.term);
          program.alt_screen <- true;
          (* Force a full repaint since the screen is now blank *)
          program.previous_buffer <- None)
    | Cmd.Exit_alt_screen ->
        if program.alt_screen then (
          log_debug program "Exiting alternate screen";
          Eio.Mutex.use_rw ~protect:true program.terminal_mutex (fun () ->
              Terminal.disable_alternate_screen program.term);
          program.alt_screen <- false;
          (* Force a full repaint to redraw the inline UI *)
          program.previous_buffer <- None;
          program.previous_dynamic_buffer <- None;
          (* Reset tracking for the inline view *)
          program.last_static_height <- 0;
          program.last_printed_static <- 0)
    | Cmd.Repaint ->
        log_debug program "Forcing repaint";
        program.previous_buffer <- None
    | Cmd.Clear_screen ->
        log_debug program "Clearing screen";
        Eio.Mutex.use_rw ~protect:true program.terminal_mutex (fun () ->
            (* Clear screen, scrollback, and move cursor home *)
            let reset_seq = Ansi.clear_terminal in
            Terminal.write program.term
              (Bytes.of_string reset_seq)
              0 (String.length reset_seq);
            Terminal.flush program.term);
        (* Reset static elements *)
        program.static_elements <- [];
        program.last_static_height <- 0;
        program.last_printed_static <- 0;
        program.previous_dynamic_buffer <- None;
        (* Force a full repaint after clearing *)
        program.previous_buffer <- None

  let handle_input_event program event =
    log_debug program (Format.asprintf "Input event: %a" Input.pp_event event);
    let msgs =
      Eio.Mutex.use_ro program.state_mutex (fun () ->
          let subs = program.app.subscriptions program.model in
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

              (* Clear UI element caches on resize since dimensions have changed *)
              let element = program.app.view program.model in
              Ui.clear_cache element;

              (* Also clear caches on all static elements to force reflow at new width *)
              Eio.Mutex.use_rw ~protect:false program.state_mutex (fun () ->
                  List.iter Ui.clear_cache program.static_elements);

              (* For non-alt-screen, clear terminal and reset tracking *)
              if not program.alt_screen then (
                Eio.Mutex.use_rw ~protect:true program.terminal_mutex (fun () ->
                    let clear_seq = Ansi.clear_terminal in
                    Terminal.write program.term
                      (Bytes.of_string clear_seq)
                      0 (String.length clear_seq);
                    Terminal.flush program.term);
                program.last_static_height <- 0;
                program.last_printed_static <- 0;
                program.previous_dynamic_buffer <- None);

              let window_handlers = Sub.collect_window [] subs in
              let size = { Sub.width = w; Sub.height = h } in
              List.filter_map (fun f -> f size) window_handlers
          | _ -> [])
    in
    log_debug program
      (Format.asprintf "Generated %d messages from input event"
         (List.length msgs));
    List.iter (Eio.Stream.add program.msg_stream) msgs

  let render program =
    log_debug program "Render: Starting render pass";
    let width, height = Terminal.size program.term in

    (* Protect reads of model and static elements *)
    let buffer, non_alt_output =
      Eio.Mutex.use_ro program.state_mutex (fun () ->
          let dynamic_element = program.app.view program.model in
          let static_elements_snapshot = program.static_elements in

          let buffer = Render.create width height in

          if program.alt_screen then (
            (* Alt-screen: Keep combined vbox for full buffer render *)
            let combined_element =
              if static_elements_snapshot = [] then dynamic_element
              else
                Ui.vbox
                  (static_elements_snapshot @ [ Ui.expand dynamic_element ])
            in
            Ui.render buffer combined_element;
            (buffer, ""))
          else
            (* Non-alt-screen: Append new static relatively, clear/redraw dynamic *)
            let output_buf = Buffer.create 1024 in

            (* Append only new static elements (delta since last render) *)
            let new_static_start = program.last_printed_static in
            let new_statics =
              let rec drop n lst =
                if n <= 0 then lst
                else match lst with [] -> [] | _ :: t -> drop (n - 1) t
              in
              drop new_static_start static_elements_snapshot
            in

            (* Render dynamic to buffer *)
            let _, dynamic_height = Ui.measure ~width dynamic_element in
            let dyn_buffer = Render.create width dynamic_height in
            Ui.render dyn_buffer dynamic_element;

            (* Get previous height *)
            let previous_height =
              match program.previous_dynamic_buffer with
              | None -> 0
              | Some b -> snd (Render.dimensions b)
            in

            (* Detect if full redraw needed *)
            let force_full =
              new_statics <> []
              || program.previous_dynamic_buffer = None
              || previous_height <> dynamic_height
              || width <> program.last_width
            in
            program.last_width <- width;

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
            program.last_printed_static <- List.length static_elements_snapshot;
            program.last_static_height <-
              program.last_static_height + !added_height;

            (* Prepare and output dynamic *)
            let dynamic_height = snd (Render.dimensions dyn_buffer) in
            let dyn_output =
              if force_full then
                Render.render_full ~mode:Render.Relative dyn_buffer ^ "\r\n"
              else
                (* previous_dynamic_buffer should always be set here,
             so we can safely use it for diffing *)
                let prev_buf = Option.get program.previous_dynamic_buffer in
                let patches = Render.diff prev_buf dyn_buffer in
                if patches = [] then Ansi.cursor_down dynamic_height ^ "\r"
                else
                  let sorted_patches =
                    List.sort
                      (fun (p1 : Render.patch) p2 ->
                        if p1.row = p2.row then Int.compare p1.col p2.col
                        else Int.compare p1.row p2.row)
                      patches
                  in
                  let patch_buf = Buffer.create 1024 in
                  let current_row = ref 0 in
                  let current_col = ref 0 in
                  List.iter
                    (fun (p : Render.patch) ->
                      (* Move down if needed *)
                      let row_diff = p.row - !current_row in
                      if row_diff > 0 then (
                        Buffer.add_string patch_buf (Ansi.cursor_down row_diff);
                        Buffer.add_string patch_buf "\r";
                        current_col := 0);
                      current_row := p.row;

                      (* Move forward on line *)
                      let col_diff = p.col - !current_col in
                      if col_diff > 0 then
                        Buffer.add_string patch_buf
                          (Ansi.cursor_forward col_diff);
                      current_col := p.col + p.new_cell.width;

                      (* Apply style and content *)
                      Buffer.add_string patch_buf
                        (Style.to_sgr p.new_cell.style);
                      let content =
                        match p.new_cell.chars with
                        | [] -> " "
                        | chars ->
                            let b = Buffer.create 8 in
                            List.iter (Uutf.Buffer.add_utf_8 b) chars;
                            Buffer.contents b
                      in
                      let styled_content =
                        match p.new_cell.style.uri with
                        | Some uri -> Ansi.hyperlink ~uri content
                        | None -> content
                      in
                      Buffer.add_string patch_buf styled_content;
                      Buffer.add_string patch_buf Ansi.reset)
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
            program.previous_dynamic_buffer <- Some dyn_buffer;
            program.previous_buffer <- None;

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
    log_debug program
      (Printf.sprintf "Current buffer hash: %d" (buffer_hash buffer));

    (* Prepare output based on mode *)
    let output =
      if not program.alt_screen then
        (* Non-alt-screen output was already prepared *)
        non_alt_output
      else
        (* Alt-screen mode uses buffer-based rendering *)
        match program.previous_buffer with
        | None ->
            (* First render *)
            log_debug program "Render: Full redraw (no previous buffer)";
            Render.render_full buffer
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
              Render.render_patches patches)
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
    if program.running && String.length output > 0 then
      Eio.Mutex.use_rw ~protect:true program.terminal_mutex (fun () ->
          Terminal.write program.term (Bytes.of_string output) 0
            (String.length output);
          Terminal.flush program.term);

    (* Only cache buffer for alt-screen mode *)
    if program.alt_screen then program.previous_buffer <- Some buffer

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
        Eio.Mutex.use_rw ~protect:false program.state_mutex (fun () ->
            process_cmd program cmd)
      done;

      (* Block until a new message arrives *)
      if program.running then (* Check again before blocking *)
        let msg = Eio.Stream.take program.msg_stream in
        if program.running then (
          (* Check again after waking up *)
          log_debug program "Processing message";
          Eio.Mutex.use_rw ~protect:false program.state_mutex (fun () ->
              let cmd = send_msg program msg in
              log_debug program
                (Format.asprintf "Command generated: %a"
                   (Cmd.pp (fun fmt _ -> Format.fprintf fmt "<msg>"))
                   cmd);
              (* If it's a sequence, it will be added to the queue.
               Otherwise, process it immediately *)
              process_cmd program cmd))
    done

  let setup_terminal program =
    log_debug program "Setting up terminal";
    Eio.Mutex.use_rw ~protect:true program.terminal_mutex (fun () ->
        Terminal.set_mode program.term `Raw;
        Terminal.hide_cursor program.term;
        if program.alt_screen then (
          log_debug program "Enabling alternate screen";
          Terminal.enable_alternate_screen program.term);
        if program.mouse then Terminal.enable_mouse program.term;
        Terminal.enable_kitty_keyboard program.term);

    (* Setup SIGWINCH handler *)
    let sigwinch_handler (w, h) =
      program.last_resize_w := w;
      program.last_resize_h := h;
      Eio.Condition.broadcast program.resize_cond
    in
    Terminal.set_sigwinch_handler (Some sigwinch_handler);

    (* Setup termination signal handlers to ensure terminal cleanup *)
    let termination_handler _ =
      (* Ensure terminal is cleaned up before exit *)
      Terminal.set_sigwinch_handler None;
      Eio.Mutex.use_rw ~protect:true program.terminal_mutex (fun () ->
          Terminal.show_cursor program.term;
          Terminal.disable_mouse program.term;
          Terminal.disable_kitty_keyboard program.term;
          Terminal.disable_alternate_screen program.term;
          Terminal.set_mode program.term `Cooked;
          Terminal.release program.term)
    in
    (* Install handlers for common termination signals *)
    Sys.set_signal Sys.sigterm (Sys.Signal_handle termination_handler);
    Sys.set_signal Sys.sigint (Sys.Signal_handle termination_handler);
    Sys.set_signal Sys.sighup (Sys.Signal_handle termination_handler)

  let cleanup_terminal program =
    Terminal.set_sigwinch_handler None;
    Eio.Mutex.use_rw ~protect:true program.terminal_mutex (fun () ->
        Terminal.flush program.term;
        Terminal.show_cursor program.term;
        Terminal.disable_mouse program.term;
        Terminal.disable_kitty_keyboard program.term;
        Terminal.disable_alternate_screen program.term;
        Terminal.set_mode program.term `Cooked;
        Terminal.release program.term)

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
        (fun () ->
          (* Resize handler fiber *)
          while program.running do
            Eio.Condition.await_no_mutex program.resize_cond;
            if program.running then
              let w = !(program.last_resize_w) in
              let h = !(program.last_resize_h) in
              handle_input_event program (Input.Resize (w, h))
          done);
      ]
end

(* API functions *)
let app ~init ~update ~view ?(subscriptions = fun _ -> Sub.none) () =
  { init; update; view; subscriptions }

let run_eio ~sw ~env ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
    ?(debug = false) app =
  let debug_log = if debug then Some (open_out "mosaic-debug.log") else None in
  Fun.protect
    ~finally:(fun () -> Option.iter close_out debug_log)
    (fun () ->
      Program.run ~sw ~env ?terminal ~alt_screen ~mouse ~fps ?debug_log app)

let run ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
    ?(debug = false) app =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  run_eio ~sw ~env ?terminal ~alt_screen ~mouse ~fps ~debug app
