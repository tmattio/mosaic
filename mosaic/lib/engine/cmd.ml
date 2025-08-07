(** Commands with single closure-based implementation *)

(* The only representation: a closure that takes a dispatch function *)
type 'msg t = { run : dispatch:('msg -> unit) -> unit }

(* Meta commands for runtime operations *)
type meta =
  | Quit
  | Print of Ui.element
  | Set_window_title of string
  | Repaint
  | Clear_screen
  | Clear_terminal
  | Enter_alt_screen
  | Exit_alt_screen
  | Log of string
  | Tick of float * (float -> unit)
  | Perform_eio of (sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> unit)

(* Local queue for collecting meta commands during execution *)
let local_meta_queue : meta Queue.t option ref = ref None

(* Enqueue a meta command *)
let enqueue_meta meta =
  match !local_meta_queue with Some q -> Queue.add meta q | None -> ()

(* User command constructors *)
let none = { run = (fun ~dispatch:_ -> ()) }
let msg m = { run = (fun ~dispatch -> dispatch m) }

let batch cmds =
  {
    run =
      (fun ~dispatch -> List.iter (fun (cmd : _ t) -> cmd.run ~dispatch) cmds);
  }

let perform f = { run = (fun ~dispatch -> Option.iter dispatch (f ())) }

let perform_eio f =
  {
    run =
      (fun ~dispatch ->
        enqueue_meta
          (Perform_eio (fun ~sw ~env -> Option.iter dispatch (f ~sw ~env))));
  }

let exec f msg =
  {
    run =
      (fun ~dispatch ->
        f ();
        dispatch msg);
  }

(* Meta command constructors *)
let quit = { run = (fun ~dispatch:_ -> enqueue_meta Quit) }

let tick duration f =
  {
    run =
      (fun ~dispatch ->
        enqueue_meta (Tick (duration, fun elapsed -> dispatch (f elapsed))));
  }

let log message = { run = (fun ~dispatch:_ -> enqueue_meta (Log message)) }
let print element = { run = (fun ~dispatch:_ -> enqueue_meta (Print element)) }

let set_window_title title =
  { run = (fun ~dispatch:_ -> enqueue_meta (Set_window_title title)) }

let enter_alt_screen =
  { run = (fun ~dispatch:_ -> enqueue_meta Enter_alt_screen) }

let exit_alt_screen =
  { run = (fun ~dispatch:_ -> enqueue_meta Exit_alt_screen) }

let repaint = { run = (fun ~dispatch:_ -> enqueue_meta Repaint) }
let clear_screen = { run = (fun ~dispatch:_ -> enqueue_meta Clear_screen) }
let clear_terminal = { run = (fun ~dispatch:_ -> enqueue_meta Clear_terminal) }

let seq cmds =
  {
    run =
      (fun ~dispatch ->
        (* Sequential execution *)
        List.iter (fun (cmd : _ t) -> cmd.run ~dispatch) cmds);
  }

let after delay msg = tick delay (fun _ -> msg)

(* Map function for transforming messages *)
let map f (cmd : _ t) =
  { run = (fun ~dispatch -> cmd.run ~dispatch:(fun msg -> dispatch (f msg))) }

(* Runtime interface - returns list of meta commands *)
let run ~dispatch (cmd : _ t) =
  let queue = Queue.create () in
  let old = !local_meta_queue in
  local_meta_queue := Some queue;
  Fun.protect
    (fun () ->
      cmd.run ~dispatch;
      (* Return the collected meta commands *)
      let metas = Queue.fold (fun acc m -> m :: acc) [] queue in
      List.rev metas)
    ~finally:(fun () -> local_meta_queue := old)
