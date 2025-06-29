type 'msg exec_cmd = { run : unit -> unit; on_complete : 'msg }

type 'msg t =
  | None
  | Msg of 'msg
  | Batch of 'msg t list
  | Perform of (unit -> 'msg option)
  | Exec of 'msg exec_cmd
  | Tick of float * (float -> 'msg)
  | Sequence of 'msg t list
  | Quit
  | Log of string
  | SetWindowTitle of string

let none = None
let msg m = Msg m

let batch cmds =
  match List.filter (function None -> false | _ -> true) cmds with
  | [] -> None
  | [ cmd ] -> cmd
  | cmds -> Batch cmds

let perform f = Perform f
let exec f msg = Exec { run = f; on_complete = msg }
let quit = Quit
let tick duration f = Tick (duration, f)
let log message = Log message
let set_window_title title = SetWindowTitle title

let sequence cmds =
  match List.filter (function None -> false | _ -> true) cmds with
  | [] -> None
  | [ cmd ] -> cmd
  | cmds -> Sequence cmds

let seq = sequence (* Alias for API compatibility *)
let after delay msg = tick delay (fun _ -> msg)

let rec to_list = function
  | None -> []
  | Msg m -> [ Msg m ]
  | Batch cmds -> List.concat_map to_list cmds
  | Perform _ as p -> [ p ]
  | Exec _ as e -> [ e ]
  | Tick _ as t -> [ t ]
  | Sequence cmds ->
      [ Sequence cmds ] (* Keep as atomic for sequential execution *)
  | Quit -> [ Quit ]
  | Log _ as l -> [ l ]
  | SetWindowTitle _ as s -> [ s ]

let map f cmd =
  let rec go = function
    | None -> None
    | Msg m -> Msg (f m)
    | Batch cmds -> batch (List.map go cmds)
    | Perform fn -> Perform (fun () -> Option.map f (fn ()))
    | Exec exec_cmd ->
        Exec { exec_cmd with on_complete = f exec_cmd.on_complete }
    | Tick (duration, g) -> Tick (duration, fun t -> f (g t))
    | Sequence cmds -> sequence (List.map go cmds)
    | Quit -> Quit
    | Log _ as l -> l (* Log commands are not affected by map *)
    | SetWindowTitle _ as s ->
        s (* SetWindowTitle commands are not affected by map *)
  in
  go cmd
