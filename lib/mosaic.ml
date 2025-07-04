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

(* Input event types - re-export from Input module *)
type key = Input.key =
  | Char of Uchar.t
  | Enter
  | Tab
  | Backspace
  | Delete
  | Escape
  | Up
  | Down
  | Left
  | Right
  | Home
  | End
  | Page_up
  | Page_down
  | Insert
  | F of int (* F1-F20 *)

type modifier = Input.modifier = { ctrl : bool; alt : bool; shift : bool }
type key_event = Input.key_event = { key : key; modifier : modifier }

type mouse_button = Input.mouse_button =
  | Left
  | Middle
  | Right
  | Wheel_up
  | Wheel_down
  | Button of int (* Extended buttons 4-11 *)

type mouse_event = Input.mouse_event =
  | Press of int * int * mouse_button * modifier
  | Release of int * int * mouse_button * modifier
  | Motion of int * int * Input.mouse_button_state * modifier

(* Public modules *)
module Ui = Ui
module Style = Ui.Style
(* Style is exported as top-level, but implemented in Ui *)

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
  }

  let create_program ~env ~sw ?terminal ?(alt_screen = true) ?(mouse = false)
      ?(fps = 60) app =
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
        Fiber.fork ~sw:program.sw (fun () ->
            let start_time = Eio.Time.now program.clock in
            Eio.Time.sleep program.clock duration;
            let elapsed = Eio.Time.now program.clock -. start_time in
            Eio.Stream.add program.msg_stream (f elapsed))
    | Cmd.Sequence cmds ->
        (* Add commands to the front of the queue to execute in order *)
        List.iter (fun cmd -> Queue.add cmd program.cmd_queue) cmds
    | Cmd.Quit ->
        program.running <- false;
        ()
    | Cmd.Log message ->
        (* Write to stderr to avoid corrupting the UI *)
        Printf.eprintf "%s\n%!" message
    | Cmd.SetWindowTitle title ->
        (* Write the escape sequence to set window title *)
        Terminal.write program.term
          (Bytes.of_string (Ansi.set_window_title title))
          0
          (String.length (Ansi.set_window_title title));
        Terminal.flush program.term

  let handle_input_event program event =
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
          let window_handlers = Sub.collect_window [] subs in
          let size = { Sub.width = w; Sub.height = h } in
          List.filter_map (fun f -> f size) window_handlers
      | _ -> []
    in
    List.iter (Eio.Stream.add program.msg_stream) msgs

  let render program =
    let element = program.app.view program.model in

    (* Create a new buffer for this frame *)
    let width, height = Terminal.size program.term in
    let buffer = Render.create width height in

    (* Render the UI element tree into the buffer using the layout engine *)
    Ui.render buffer element;

    (* Now diff and patch as intended *)
    let output =
      match program.previous_buffer with
      | None ->
          Terminal.write program.term
            (Bytes.of_string (Ansi.cursor_position 1 1 ^ Ansi.clear_screen))
            0
            (String.length (Ansi.cursor_position 1 1 ^ Ansi.clear_screen));
          Render.render_full buffer
      | Some prev_buf ->
          let patches = Render.diff prev_buf buffer in
          Render.render_patches patches
    in

    Terminal.write program.term (Bytes.of_string output) 0
      (String.length output);
    Terminal.flush program.term;

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

      (* Wait for a new message *)
      match Eio.Stream.take_nonblocking program.msg_stream with
      | Some msg ->
          let cmd = send_msg program msg in
          (* If it's a sequence, it will be added to the queue.
             Otherwise, process it immediately *)
          process_cmd program cmd
      | None ->
          (* No message available, wait a bit *)
          Eio.Time.sleep program.clock 0.001
    done

  let setup_terminal program =
    Terminal.set_mode program.term `Raw;
    Terminal.hide_cursor program.term;
    if program.alt_screen then Terminal.enable_alternate_screen program.term;
    if program.mouse then Terminal.enable_mouse program.term;

    (* Setup SIGWINCH handler *)
    let sigwinch_handler (w, h) =
      let resize_event = Input.Resize (w, h) in
      handle_input_event program resize_event
    in
    Terminal.set_sigwinch_handler (Some sigwinch_handler)

  let cleanup_terminal program =
    Terminal.set_sigwinch_handler None;
    Terminal.show_cursor program.term;
    Terminal.disable_mouse program.term;
    Terminal.disable_alternate_screen program.term;
    Terminal.set_mode program.term `Cooked;
    Terminal.release program.term

  let run ~sw ~env ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
      app =
    let open Eio.Std in
    let program =
      create_program ~env ~sw ?terminal ~alt_screen ~mouse ~fps app
    in

    (* Initialize with init command *)
    let init_cmd = snd (app.init ()) in
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

let run ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60) app =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  Program.run ~sw ~env ?terminal ~alt_screen ~mouse ~fps app

let run_eio ~sw ~env ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
    app =
  Program.run ~sw ~env ?terminal ~alt_screen ~mouse ~fps app

(* Helper functions *)
let key ?(ctrl = false) ?(alt = false) ?(shift = false) k =
  { key = k; modifier = { ctrl; alt; shift } }

let char ?(ctrl = false) ?(alt = false) ?(shift = false) c =
  { key = Char (Uchar.of_char c); modifier = { ctrl; alt; shift } }
