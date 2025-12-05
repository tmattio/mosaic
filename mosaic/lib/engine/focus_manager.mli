(** Focus manager for keyboard navigation

    This module manages focus state and tab order for UI elements, enabling
    keyboard navigation through focusable components. *)

type t
(** The focus manager type *)

type focusable = {
  key : Ui.Attr.key;
  tab_index : int option; (* None = natural order, Some n = specific order *)
  auto_focus : bool; (* Should this element get focus initially? *)
  order : int; (* insertion order for stable sorting *)
}
(** Focusable element registration *)

val create : unit -> t
(** Create a new focus manager *)

val set_router : t -> Input_router.t -> unit
(** Connect the focus manager to an input router for focus state sync *)

val register : t -> focusable -> unit
(** Register a focusable element. If already registered, updates its properties.
*)

val unregister : t -> Ui.Attr.key -> unit
(** Unregister a focusable element *)

val focus : t -> Ui.Attr.key -> unit
(** Set focus to a specific element *)

val blur : t -> unit
(** Remove focus from the current element *)

val get_focused : t -> Ui.Attr.key option
(** Get the currently focused element, if any *)

val focus_first : t -> unit
(** Focus the first element in tab order *)

val focus_last : t -> unit
(** Focus the last element in tab order *)

val focus_next : t -> unit
(** Move focus to the next element (wraps around) *)

val focus_prev : t -> unit
(** Move focus to the previous element (wraps around) *)

val handle_tab : t -> forward:bool -> bool
(** Handle Tab key press. [forward] = true for Tab, false for Shift+Tab
    [handle_tab t ~forward] moves focus accordingly and returns [true] if the
    tab event was handled (i.e. focus changed) and [false] otherwise. *)

val clear : t -> unit
(** Clear all focusable elements and remove focus *)
