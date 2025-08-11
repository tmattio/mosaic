open Engine
module Style = Ui.Style
module Ui = Ui
module Cmd = Cmd
module Sub = Sub
module Event = Event
module Input = Input
module Component = Component

(*  The Elm Architecture types *)

type ('model, 'msg) app = {
  init : unit -> 'model * 'msg Cmd.t;
  update : 'msg -> 'model -> 'model * 'msg Cmd.t;
  view : 'model -> Ui.element;
  subscriptions : 'model -> 'msg Sub.t;
}

let app ~init ~update ~view ?(subscriptions = fun _ -> Sub.none) () =
  { init; update; view; subscriptions }

(*  Runtime *)

let run_eio (type model msg) ~(sw : Eio.Switch.t) ~(env : Eio_unix.Stdenv.base)
    ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60) ?(debug = false)
    (app : (model, msg) app) =
  (* 1) Build Program config and start the runtime *)
  let debug_log = if debug then Some (open_out "mosaic-debug.log") else None in
  let cfg = Program.config ?terminal ~alt_screen ~mouse ~fps ?debug_log () in

  (* We need to track if prog has been created for cleanup *)
  let prog_ref = ref None in

  Fun.protect
    ~finally:(fun () ->
      (* Always stop the program if it was created *)
      Option.iter Program.stop !prog_ref;
      Option.iter close_out debug_log)
    (fun () ->
      (* A mutable model lives only in this layer *)
      let init_model, init_cmd = app.init () in
      let model_ref : model ref = ref init_model in
      let model_mutex = Eio.Mutex.create () in

      (* Place to queue commands that still need to be executed *)
      let cmd_queue : msg Cmd.t Queue.t = Queue.create () in
      (* initial command from [init *)
      Queue.add init_cmd cmd_queue;

      (* Message stream (unbounded) for the TEA message pum *)
      let msg_stream : msg Eio.Stream.t = Eio.Stream.create Int.max_int in
      let dispatch (m : msg) = Eio.Stream.add msg_stream m in

      (* Call‑backs expected by Program *)
      let render () =
        Eio.Mutex.use_ro model_mutex (fun () -> app.view !model_ref)
      in

      let handle_input_event (event : Input.event) =
        let subs =
          Eio.Mutex.use_ro model_mutex (fun () -> app.subscriptions !model_ref)
        in
        (* Collect messages from subscriptions *)
        let messages = ref [] in
        let collect_msg msg = messages := msg :: !messages in
        Sub.run ~dispatch:collect_msg (Event.Input event) subs;

        (* Handle resize event to trigger render *)
        (match event with
        | Input.Resize _ -> Option.iter Program.schedule_render !prog_ref
        | _ -> ());

        let generated = List.rev !messages in
        List.iter dispatch generated
      in

      let handle_resize ~w ~h = handle_input_event (Input.Resize (w, h)) in

      (* Start the Program runtim *)
      let prog, process_cmd =
        Program.start ~sw ~env cfg ~render ~on_input:handle_input_event
          ~on_resize:handle_resize ()
      in
      prog_ref := Some prog;

      (* 2) TEA message / command pump *)
      let rec message_loop () =
        (* First, execute any queued commands *)
        while not (Queue.is_empty cmd_queue) do
          let cmd = Queue.take cmd_queue in
          process_cmd dispatch cmd;
          (* Check if quit was requested *)
          if not (Program.is_running prog) then raise Exit
        done;

        (* Then wait for the next messag *)
        match Eio.Stream.take msg_stream with
        | msg ->
            let _new_model, cmd =
              Eio.Mutex.use_rw model_mutex ~protect:true (fun () ->
                  let current_model = !model_ref in
                  let new_model, cmd = app.update msg current_model in
                  model_ref := new_model;
                  (new_model, cmd))
            in
            Queue.add cmd cmd_queue;
            message_loop ()
        | exception End_of_file -> ()
      in

      (* Run everything concurrently - wait for message loop to complete *)
      try message_loop () with Exit -> ())

(* Convenience wrapper for the common case – sets up its own Eio event loo *)
let run ?terminal ?(alt_screen = true) ?(mouse = false) ?(fps = 60)
    ?(debug = false) app =
  Eio_main.run @@ fun env ->
  Eio.Switch.run @@ fun sw ->
  run_eio ~sw ~env ?terminal ~alt_screen ~mouse ~fps ~debug app
