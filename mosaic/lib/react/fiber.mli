(** Mounted component instance – completely independent of Context. *)

module Sub = Engine.Sub
module Cmd = Engine.Cmd

type t

val current_key : t option Domain.DLS.key
val create_root : (unit -> Ui.element) -> t
val mark_dirty : t -> unit
val is_dirty : t -> bool
val reconcile : t -> Ui.element
val destroy : t -> unit
val with_handler : t -> (unit -> 'a) -> 'a

(** Type-erased subscription for React *)
type erased_sub = ErasedSub : 'msg Sub.t -> erased_sub

(** Collect all subscriptions from the fiber tree *)
val collect_subscriptions : t -> erased_sub list

(** Check if there are pending commands and collect them *)
val has_pending_commands : t -> bool
val collect_pending_commands : t -> 'msg Cmd.t
