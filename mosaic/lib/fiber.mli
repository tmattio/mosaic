(** Mounted component instance – completely independent of Context. *)

module Sub = Engine.Sub
module Cmd = Engine.Cmd

type t
(** Module type for polymorphic command runner *)

val current_key : t option Domain.DLS.key
val create_root : (unit -> Ui.element) -> t
val mark_dirty : ?reason:string -> t -> unit
val is_dirty : t -> bool
val reconcile : t -> Ui.element
val destroy : t -> unit
val with_handler : t -> (unit -> 'a) -> 'a

(** Type-erased subscription for React *)
type erased_sub = Erased_sub : 'msg Sub.t -> erased_sub

(** Type-erased command for React *)
type erased_cmd = Erased_cmd : unit Cmd.t -> erased_cmd

val collect_subscriptions : t -> erased_sub list
(** Collect all subscriptions from the fiber tree *)

val has_pending_commands : t -> bool
(** Check if there are pending commands and collect them *)

val collect_pending_commands : t -> erased_cmd list

val request_render_ref : (unit -> unit) ref
(** Global ref for requesting renders when bubbling to root *)
