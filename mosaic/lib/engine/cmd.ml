(** Commands with single closure-based implementation *)

let src = Logs.Src.create "cmd" ~doc:"Command execution"

module Log = (val Logs.src_log src : Logs.LOG)

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
  | Perform of (unit -> unit)
  | Perform_eio of (sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> unit)

let pp_meta ppf = function
  | Quit -> Format.fprintf ppf "Quit"
  | Print _ -> Format.fprintf ppf "Print(<element>)"
  | Set_window_title title -> Format.fprintf ppf "Set_window_title(%S)" title
  | Repaint -> Format.fprintf ppf "Repaint"
  | Clear_screen -> Format.fprintf ppf "Clear_screen"
  | Clear_terminal -> Format.fprintf ppf "Clear_terminal"
  | Enter_alt_screen -> Format.fprintf ppf "Enter_alt_screen"
  | Exit_alt_screen -> Format.fprintf ppf "Exit_alt_screen"
  | Log msg -> Format.fprintf ppf "Log(%S)" msg
  | Tick (interval, _) -> Format.fprintf ppf "Tick(%.3f)" interval
  | Perform _ -> Format.fprintf ppf "Perform(<fn>)"
  | Perform_eio _ -> Format.fprintf ppf "Perform_eio(<fn>)"

(* Domain-local queue for collecting meta commands during execution *)
let local_meta_queue_key : meta Queue.t option Domain.DLS.key =
  Domain.DLS.new_key (fun () -> None)

(* Enqueue a meta command *)
let enqueue_meta meta =
  Log.debug (fun m -> m "Enqueuing meta command: %a" pp_meta meta);
  match Domain.DLS.get local_meta_queue_key with
  | Some q -> Queue.add meta q
  | None -> ()

(* User command constructors *)
let none = { run = (fun ~dispatch:_ -> ()) }
let msg m = { run = (fun ~dispatch -> dispatch m) }

let batch cmds =
  {
    run =
      (fun ~dispatch -> List.iter (fun (cmd : _ t) -> cmd.run ~dispatch) cmds);
  }

let perform f =
  {
    run =
      (fun ~dispatch ->
        enqueue_meta
          (Perform (fun () -> try Option.iter dispatch (f ()) with _ -> ())));
  }

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
        (* enqueue in the order they must happen *)
        enqueue_meta Exit_alt_screen;
        enqueue_meta (Perform (fun () -> f ()));
        enqueue_meta Enter_alt_screen;
        enqueue_meta Repaint;
        (* dispatch *after* we're back in our UI mode *)
        enqueue_meta (Perform (fun () -> dispatch msg)));
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
  Log.debug (fun m -> m "Running command");
  let queue = Queue.create () in
  let old = Domain.DLS.get local_meta_queue_key in
  Domain.DLS.set local_meta_queue_key (Some queue);
  Fun.protect
    (fun () ->
      cmd.run ~dispatch;
      (* Return the collected meta commands *)
      let metas = Queue.fold (fun acc m -> m :: acc) [] queue in
      List.rev metas)
    ~finally:(fun () -> Domain.DLS.set local_meta_queue_key old)
