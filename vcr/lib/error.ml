(** Error types and handling for VCR library *)

type t =
  | Io_error of string * exn
  | Process_error of string
  | Render_error of string
  | Invalid_tape of string * int option * int option
  | Invalid_config of string

let to_string = function
  | Io_error (context, exn) ->
      Printf.sprintf "I/O error in %s: %s" context (Printexc.to_string exn)
  | Process_error msg -> Printf.sprintf "Process error: %s" msg
  | Invalid_tape (msg, line, col) -> (
      match (line, col) with
      | Some l, Some c ->
          Printf.sprintf "Invalid tape at line %d, column %d: %s" l c msg
      | Some l, None -> Printf.sprintf "Invalid tape at line %d: %s" l msg
      | None, Some c -> Printf.sprintf "Invalid tape at column %d: %s" c msg
      | None, None -> Printf.sprintf "Invalid tape: %s" msg)
  | Render_error msg -> Printf.sprintf "Render error: %s" msg
  | Invalid_config msg -> Printf.sprintf "Invalid configuration: %s" msg

let catch f =
  try Ok (f ()) with
  | Unix.Unix_error (err, fn, arg) ->
      Error
        (Io_error
           (Printf.sprintf "%s(%s)" fn arg, Unix.Unix_error (err, fn, arg)))
  | exn -> Error (Io_error ("unknown operation", exn))

let io_error context exn = Error (Io_error (context, exn))

module Syntax = struct
  let ( let* ) = Result.bind
  let ( let+ ) r f = Result.map f r
end
