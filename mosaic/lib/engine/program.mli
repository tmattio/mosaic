(** Program – Mosaic’s event / render runtime This interface is intentionally
    minimal and stable. *)

open Eio

(** {1 Configuration} *)

type config

val config :
  ?terminal:Tty.t ->
  ?alt_screen:bool ->
  (* default = true  *)
  ?mouse:bool ->
  (* default = false *)
  ?fps:int ->
  (* default = 60    *)
  ?debug_log:out_channel ->
  (* default = none  *)
  unit ->
  config
(** Build a configuration record. All fields have sensible defaults. *)

(** {1 Handle} *)

type t
(** Opaque handle to a running program. *)

(** {1 Running} *)

val start :
  sw:Switch.t ->
  env:Eio_unix.Stdenv.base ->
  config ->
  render:(unit -> Ui.element) ->
  on_input:(Input.event -> unit) ->
  on_resize:(w:int -> h:int -> unit) ->
  ?tick:(elapsed:float -> unit) ->
  unit ->
  t * (('msg -> unit) -> 'msg Cmd.t -> unit)
(** [start ~sw ~env cfg ~render ~on_input ~on_resize ?tick ()]
    - Prepares the terminal according to [cfg].
    - Spawns the internal render, input and resize fibres on [sw].
    - Returns 1. a handle [t] that can later be stopped, and 2. a
      function [process_cmd] — use it to execute a ['msg Cmd.t], supplying your
      own ['msg -> unit] dispatcher.

    The call returns immediately; the program keeps running in fibres. *)

val stop : t -> unit
(** Gracefully shut the program down (restores terminal state, joins fibres). *)

(** {1 Utility} *)

val request_render : t -> unit
(** Ask the render loop to redraw as soon as possible (useful after an
    asynchronous change). No‑op if the program is already scheduled to paint. *)
