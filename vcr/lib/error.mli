(** Error types and handling for VCR library *)

(** Error types that can occur during VCR operations *)
type t =
  | Io_error of string * exn
      (** I/O error with context and original exception *)
  | Process_error of string  (** Process-related errors (PTY, shell, etc.) *)
  | Render_error of string
      (** Rendering errors (GIF, PNG, SVG, quantization, etc.) *)
  | Invalid_tape of string * int option * int option
      (** Parse error with message and optional line/column *)
  | Invalid_config of string  (** Configuration validation errors *)

val to_string : t -> string
(** Convert error to human-readable string *)

val catch : (unit -> 'a) -> ('a, t) result
(** Create a result from an exception-throwing function *)

val io_error : string -> exn -> ('a, t) result
(** Create an I/O error result from an exception *)

(** Operators for result handling *)
module Syntax : sig
  val ( let* ) : ('a, 'e) result -> ('a -> ('b, 'e) result) -> ('b, 'e) result
  val ( let+ ) : ('a, 'e) result -> ('a -> 'b) -> ('b, 'e) result
end
