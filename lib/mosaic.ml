(* mosaic_tea.ml *)
(* Mosaic - A delightful OCaml TUI framework inspired by The Elm Architecture *)

open Engine

(* Public modules *)
module Style = Ui.Style
module Ui = Ui
module Cmd = Cmd
module Sub = Sub
module Input = Input
module Component = Component

(* The Elm Architecture: Model-View-Update with effects and subscriptions *)
type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> Ui.element;
  subscriptions : 'model -> 'msg Sub.t;
}

(* API functions *)
let app ~init ~update ~view ?(subscriptions = fun _ -> Sub.none) () =
  { init; update; view; subscriptions }

let run_eio (type model msg) ~sw ~env ?terminal ?(alt_screen = true)
    ?(mouse = false) ?(fps = 60) ?(debug = false) (app : (model, msg) app) =
  (* Configuration *)
  let debug_log = if debug then Some (open_out "mosaic-debug.log") else None in
  let config = Program.{ terminal; alt_screen; mouse; fps; debug_log } in

  (* Create the program handle *)
  let p = Program.create ~sw ~env config in

  (* Initialize the model and get initial command *)
  let model, initial_cmd = app.init () in
  let model_ref = ref model in
  Program.set_model p model;

  (* Message stream for TEA *)
  let msg_stream = Eio.Stream.create 100 in

  (* Command queue for processing *)
  let cmd_queue = Queue.create () in
  Queue.add initial_cmd cmd_queue;

  (* Dispatch function *)
  let dispatch msg = Eio.Stream.add msg_stream msg in

  (* Input event handler *)
  let handle_input_event event =
    Program.log_debug p (Format.asprintf "Input event: %a" Input.pp_event event);
    let msgs =
      Program.with_state_mutex p ~protect:true (fun () ->
          let subs = app.subscriptions !model_ref in
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
          | Input.Paste s ->
              let paste_handlers = Sub.collect_paste [] subs in
              List.filter_map (fun f -> f s) paste_handlers
          | Input.Resize (w, h) ->
              (* Invalidate the previous buffer on resize *)
              Program.invalidate_buffer p;

              (* For non-alt-screen, clear terminal and reset tracking *)
              if not alt_screen then (
                Program.with_terminal_mutex p ~protect:true (fun () ->
                    let term =
                      Terminal.create ~tty:true Unix.stdin Unix.stdout
                    in
                    let clear_seq = Ansi.clear_terminal in
                    Terminal.write term
                      (Bytes.of_string clear_seq)
                      0 (String.length clear_seq);
                    Terminal.flush term);
                Program.clear_static_elements p);

              let window_handlers = Sub.collect_window [] subs in
              let size = { Sub.width = w; Sub.height = h } in
              List.filter_map (fun f -> f size) window_handlers
          | _ -> [])
    in
    Program.log_debug p
      (Format.asprintf "Generated %d messages from input event"
         (List.length msgs));
    List.iter dispatch msgs
  in

  (* Handle resize events *)
  let handle_resize (w, h) = handle_input_event (Input.Resize (w, h)) in

  (* Message processing loop *)
  let message_loop () =
    while Program.is_running p do
      (* Process queued commands first *)
      while not (Queue.is_empty cmd_queue) do
        let cmd = Queue.take cmd_queue in
        Program.with_state_mutex p ~protect:false (fun () ->
            Program.process_cmd p dispatch cmd)
      done;

      if Program.is_quit_pending p then (
        Program.render p (app.view !model_ref);
        Program.set_running p false;
        (* Wake up the resize fiber so it can exit *)
        Eio.Condition.broadcast (Program.resize_condition p);
        (* Give other loops a chance to exit *)
        Eio.Fiber.yield ())
      else if Program.is_running p then
        (* Block until a new message arrives *)
        let msg = Eio.Stream.take msg_stream in
        if Program.is_running p then
          Program.with_state_mutex p ~protect:false (fun () ->
              let new_model, cmd = app.update msg !model_ref in
              model_ref := new_model;
              Program.set_model p new_model;
              Program.process_cmd p dispatch cmd)
    done
  in

  Fun.protect
    ~finally:(fun () ->
      Option.iter close_out debug_log;
      Program.cleanup p)
    (fun () ->
      Program.setup_terminal p;
      Program.setup_signal_handlers p;

      Eio.Fiber.all
        [
          (fun () -> Program.run_input_loop p handle_input_event);
          (fun () -> Program.run_render_loop p (fun () -> app.view !model_ref));
          message_loop;
          (fun () -> Program.run_resize_loop p handle_resize);
        ])

let run ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
    ?(debug = false) app =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  run_eio ~sw ~env ?terminal ~alt_screen ~mouse ~fps ~debug app
