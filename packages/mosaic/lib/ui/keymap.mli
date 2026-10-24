(** Compiled typed keyboard bindings. *)

type 'a binding = Shortcut.t * 'a
(** A shortcut and its semantic action. *)

val binding_equal : ('a -> 'a -> bool) -> 'a binding -> 'a binding -> bool
(** [binding_equal equal_action a b] compares shortcuts and their actions. *)

type 'a t
(** An immutable action map. *)

val make : defaults:'a binding list -> ?custom:'a binding list -> unit -> 'a t
(** [make ~defaults ?custom ()] compiles bindings. Later bindings override
    earlier equivalent shortcuts; [custom] follows [defaults]. *)

val action : 'a t -> Matrix_input.Keystroke.t -> 'a option
(** [action t ev] is the bound action, if any. Lookup uses {!Shortcut} matching
    rules: the key exactly, then the base-layout key. *)
