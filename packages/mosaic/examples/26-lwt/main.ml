(** Timers that are Lwt promises, in a program that is a promise itself.

    It is built only when lwt is installed. [Mosaic_lwt.run] is the whole glue:
    the program runs on a domain of its own, so it never holds up Lwt's thread,
    and the work of [Cmd.perform] runs on Lwt's thread.

    a adds a timer; q or Esc quits. *)

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
      let ring dispatch =
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

(* With [--for SECONDS], [Lwt.pick] cancels the program after that long. *)
let () =
  Random.self_init ();
  let limit =
    match Sys.argv with
    | [| _; "--for"; seconds |] -> Some (float_of_string seconds)
    | _ -> None
  in
  let program = Mosaic_lwt.run { init; update; view; subscriptions } in
  Lwt_main.run
    (match limit with
    | None -> program
    | Some seconds -> Lwt.pick [ program; Lwt_unix.sleep seconds ])
