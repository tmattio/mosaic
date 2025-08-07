(** Mosaic React - A React-style declarative UI library for terminal
    applications *)

(** {1 Context API} *)

type 'a context
(** The type of a context that can hold values of type ['a] *)

val create_context : ?default:'a -> unit -> 'a context
(** Create a new context with an optional default value *)

val provide : 'a context -> 'a -> (unit -> 'b) -> 'b
(** [provide ctx value thunk] runs [thunk ()] with [value] available in context
    [ctx] *)

(** {1 Hooks} *)

val use_state : 'a -> 'a * ('a -> unit)
(** [use_state initial] returns the current state value and a setter function *)

val use_effect : ?deps:Obj.t array -> (unit -> (unit -> unit) option) -> unit
(** [use_effect ?deps setup] runs the setup function and manages cleanup. If
    [deps] is provided, the effect only re-runs when dependencies change. *)

val use_context : 'a context -> 'a
(** [use_context ctx] retrieves the current value from the context *)

val use_subscription : 'msg Engine.Sub.t -> unit
(** Subscribe to external events *)

val dispatch_cmd : 'msg Engine.Cmd.t -> unit
(** Dispatch a command *)

(** {1 Component Creation} *)

module type Component = sig
  type props

  val render : props -> Ui.element
end

val component : (module Component with type props = 'p) -> 'p -> Ui.element
(** Create a component from a module and props *)

(** {1 Re-exported Modules} *)

module Ui = Ui
module Cmd = Engine.Cmd
module Sub = Engine.Sub
module Input = Input

(** {1 Runtime} *)

val run_eio :
  sw:Eio.Switch.t ->
  env:Eio_unix.Stdenv.base ->
  ?terminal:Tty.t ->
  ?alt_screen:bool ->
  ?mouse:bool ->
  ?fps:int ->
  ?debug:out_channel ->
  (unit -> Ui.element) ->
  unit
(** Run the application with explicit Eio environment *)

val run :
  ?terminal:Tty.t ->
  ?alt_screen:bool ->
  ?mouse:bool ->
  ?fps:int ->
  ?debug:out_channel ->
  (unit -> Ui.element) ->
  unit
(** Run the application - main entry point *)
