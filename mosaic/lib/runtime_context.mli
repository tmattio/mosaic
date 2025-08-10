(** Runtime context for sharing system services

    This module provides access to the input router, focus manager, and layout
    snapshot throughout the component tree. *)

type t = {
  input_router : Engine.Input_router.t;
  focus_manager : Engine.Focus_manager.t;
  mutable snapshot : Ui.Layout_snapshot.t option;
}
(** The runtime context type *)

val create : unit -> t
(** Create a new runtime context *)

val current : unit -> t option
(** Get the current runtime context (if available) *)

val set_current : t option -> unit
(** Set the current runtime context *)

val update_snapshot : t -> Ui.Layout_snapshot.t -> unit
(** Update the layout snapshot *)

val get_snapshot : t -> Ui.Layout_snapshot.t option
(** Get the current snapshot *)

val context : t Context.t
(** Context for use with React-style context API *)
