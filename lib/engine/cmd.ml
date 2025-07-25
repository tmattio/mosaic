type 'msg exec_cmd = { run : unit -> unit; on_complete : 'msg }

type 'msg t =
  | None
  | Msg of 'msg
  | Batch of 'msg t list
  | Perform of (unit -> 'msg option)
  | Perform_eio of (sw:Eio.Switch.t -> env:Eio_unix.Stdenv.base -> 'msg option)
  | Exec of 'msg exec_cmd
  | Tick of float * (float -> 'msg)
  | Sequence of 'msg t list
  | Quit
  | Log of string
  | Print of Ui.element
  | Set_window_title of string
  | Enter_alt_screen
  | Exit_alt_screen
  | Repaint
  | Clear_screen
  | Clear_terminal

let none = None
let msg m = Msg m

let batch cmds =
  match List.filter (function None -> false | _ -> true) cmds with
  | [] -> None
  | [ cmd ] -> cmd
  | cmds -> Batch cmds

let perform f = Perform f
let perform_eio f = Perform_eio f
let exec f msg = Exec { run = f; on_complete = msg }
let quit = Quit
let tick duration f = Tick (duration, f)
let log message = Log message
let print element = Print element
let set_window_title title = Set_window_title title
let enter_alt_screen = Enter_alt_screen
let exit_alt_screen = Exit_alt_screen
let repaint = Repaint
let clear_screen = Clear_screen
let clear_terminal = Clear_terminal

let seq cmds =
  match List.filter (function None -> false | _ -> true) cmds with
  | [] -> None
  | [ cmd ] -> cmd
  | cmds -> Sequence cmds

let after delay msg = tick delay (fun _ -> msg)

(* Pretty-printing *)
let pp pp_msg fmt cmd =
  let open Format in
  let rec pp_cmd fmt = function
    | None -> fprintf fmt "None"
    | Msg m -> fprintf fmt "Msg(%a)" pp_msg m
    | Batch cmds ->
        fprintf fmt "Batch[@[<hv>%a@]]"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") pp_cmd)
          cmds
    | Perform _ -> fprintf fmt "Perform(<fun>)"
    | Perform_eio _ -> fprintf fmt "Perform_eio(<fun>)"
    | Exec { on_complete; _ } ->
        fprintf fmt "Exec{on_complete=%a}" pp_msg on_complete
    | Tick (duration, _) -> fprintf fmt "Tick(%.3f, <fun>)" duration
    | Sequence cmds ->
        fprintf fmt "Sequence[@[<hv>%a@]]"
          (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ";@ ") pp_cmd)
          cmds
    | Quit -> fprintf fmt "Quit"
    | Log message -> fprintf fmt "Log(%S)" message
    | Print _ -> fprintf fmt "Print(<element>)"
    | Set_window_title title -> fprintf fmt "Set_window_title(%S)" title
    | Enter_alt_screen -> fprintf fmt "Enter_alt_screen"
    | Exit_alt_screen -> fprintf fmt "Exit_alt_screen"
    | Repaint -> fprintf fmt "Repaint"
    | Clear_screen -> fprintf fmt "Clear_screen"
    | Clear_terminal -> fprintf fmt "Clear_terminal"
  in
  pp_cmd fmt cmd

let rec to_list = function
  | None -> []
  | Msg m -> [ Msg m ]
  | Batch cmds -> List.concat_map to_list cmds
  | Perform _ as p -> [ p ]
  | Perform_eio _ as p -> [ p ]
  | Exec _ as e -> [ e ]
  | Tick _ as t -> [ t ]
  | Sequence cmds ->
      [ Sequence cmds ] (* Keep as atomic for sequential execution *)
  | Quit -> [ Quit ]
  | Log _ as l -> [ l ]
  | Print _ as p -> [ p ]
  | Set_window_title _ as s -> [ s ]
  | Enter_alt_screen -> [ Enter_alt_screen ]
  | Exit_alt_screen -> [ Exit_alt_screen ]
  | Repaint -> [ Repaint ]
  | Clear_screen -> [ Clear_screen ]
  | Clear_terminal -> [ Clear_terminal ]

let map f cmd =
  let rec go = function
    | None -> None
    | Msg m -> Msg (f m)
    | Batch cmds -> batch (List.map go cmds)
    | Perform fn -> Perform (fun () -> Option.map f (fn ()))
    | Perform_eio fn -> Perform_eio (fun ~sw ~env -> Option.map f (fn ~sw ~env))
    | Exec exec_cmd ->
        Exec { exec_cmd with on_complete = f exec_cmd.on_complete }
    | Tick (duration, g) -> Tick (duration, fun t -> f (g t))
    | Sequence cmds -> seq (List.map go cmds)
    | Quit -> Quit
    | Log _ as l -> l (* Log commands are not affected by map *)
    | Print _ as p -> p (* Print commands are not affected by map *)
    | Set_window_title _ as s ->
        s (* Set_window_title commands are not affected by map *)
    | Enter_alt_screen -> Enter_alt_screen
    | Exit_alt_screen -> Exit_alt_screen
    | Repaint -> Repaint
    | Clear_screen -> Clear_screen
    | Clear_terminal -> Clear_terminal
  in
  go cmd
