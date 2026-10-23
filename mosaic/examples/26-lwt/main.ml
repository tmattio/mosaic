(** Timers that are Lwt promises, in a program that is a promise itself. *)

open Mosaic

type timer = { id : int; seconds : float; rang : bool }
type model = { timers : timer list; next : int }
type msg = Add | Rang of int | Quit

let init () = ({ timers = []; next = 1 }, Cmd.none)

(* A timer is a promise of Lwt's: the work runs on Lwt's thread, so it
   starts the promise there. *)
let update msg model =
  match msg with
  | Add ->
      let id = model.next and seconds = 1. +. Random.float 3. in
      let timer = { id; seconds; rang = false } in
      let ring { dispatch; _ } =
        Lwt.async (fun () ->
            Lwt.map (fun () -> dispatch (Rang id)) (Lwt_unix.sleep seconds))
      in
      ({ timers = model.timers @ [ timer ]; next = id + 1 }, Cmd.perform ring)
  | Rang id ->
      let ring t = if t.id = id then { t with rang = true } else t in
      ({ model with timers = List.map ring model.timers }, Cmd.none)
  | Quit -> (model, Cmd.quit)

let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

let view model =
  let line t =
    text
      (Printf.sprintf "#%d  %.1f s  %s" t.id t.seconds
         (if t.rang then "rang" else "waiting"))
  in
  box ~flex_direction:Column ~padding:(padding 1) ~gap:(gap 1)
    [
      text ~style:(Ansi.Style.make ~bold:true ()) "Lwt timers";
      box ~flex_direction:Column
        (if model.timers = [] then [ text ~style:muted "No timer yet." ]
         else List.map line model.timers);
      text ~style:muted "a add a timer  •  q quit";
    ]

let subscriptions _ =
  Sub.on_keys
    [
      (Shortcut.char 'a', Add);
      (Shortcut.char 'q', Quit);
      (Shortcut.escape, Quit);
    ]

(* The Lwt glue. Lwt runs its promises on one thread, which a program must
   not hold up, so the program runs on a domain of its own, as Mosaic.run
   runs it, and the work of [Cmd.perform] runs on Lwt's thread. *)

type tui = {
  stop : Matrix.Stop.t;
  finished : unit Lwt.t;
      (** Resolves once the terminal is given back, with what the program
          raised. A program cancelled while it takes the terminal gives it back
          once that is done: on a terminal that does not answer, after the wait
          for its answers (up to max(500 ms, four round trips)). *)
}

(* Starts [app] on a domain of its own. The domain closes [ended] once the
   terminal is given back, which makes its other end readable. *)
let start app =
  let stop = Matrix.Stop.create () in
  let ended, closed = Unix.pipe ~cloexec:true () in
  let process_perform work =
    Lwt_preemptive.run_in_main_dont_wait
      (fun () ->
        work ();
        Lwt.return_unit)
      ignore
  in
  let domain =
    Domain.spawn (fun () ->
        Fun.protect
          ~finally:(fun () -> Unix.close closed)
          (fun () -> Mosaic.run ~stop ~process_perform app))
  in
  let ended = Lwt_unix.of_unix_file_descr ended in
  let finished =
    Lwt.finalize
      (fun () ->
        Lwt.map (fun () -> Domain.join domain) (Lwt_unix.wait_read ended))
      (fun () -> Lwt_unix.close ended)
  in
  { stop; finished }

(* [run tui] resolves once [tui] has ended. Cancelling it requests the
   program's stop and resolves at once, without waiting for the terminal:
   the program gives it back on its own domain, within the wait for an
   answer the terminal owes, and the process's exit restores it if the
   process ends first. Code that must wait for it waits for [finished]. *)
let run tui =
  Lwt.finalize
    (fun () -> Lwt.protected tui.finished)
    (fun () ->
      Matrix.Stop.request tui.stop;
      Lwt.return_unit)

(* With [--for SECONDS], the program is cancelled after that long. *)
let () =
  Random.self_init ();
  let limit =
    match Sys.argv with
    | [| _; "--for"; seconds |] -> Some (float_of_string seconds)
    | _ -> None
  in
  Lwt_main.run
    (let open Lwt.Syntax in
     let tui = start { init; update; view; subscriptions } in
     let* () =
       match limit with
       | None -> run tui
       | Some seconds -> Lwt.pick [ run tui; Lwt_unix.sleep seconds ]
     in
     (* Writing before the terminal is given back would write on the
        program's screen. *)
     let* () = tui.finished in
     Lwt_io.printl "The terminal is the shell's again.")
