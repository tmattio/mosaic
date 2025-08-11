(** Key management for Tile components

    This module provides key generation and management utilities for interactive
    components. Keys are used to maintain component identity across re-renders
    and to attach event handlers. *)

type key = Ui.Attr.key
(** Type alias for UI element keys *)

val use_key : prefix:string -> key
(** [use_key ~prefix] generates a unique, stable key for a component. The key
    persists across re-renders at the same call site.

    @param prefix A string prefix for the generated key (e.g., "btn", "input")
    @return A unique key that can be used with UI elements

    Example:
    {[
      let button_key = use_key ~prefix:"btn" in
      with_key button_key (Ui.text "Click me")
    ]} *)

val with_key : key -> Ui.element -> Ui.element
(** [with_key key element] attaches a key to a UI element. This is essential for
    event handling and maintaining element identity.

    @param key The key to attach
    @param element The UI element to attach the key to
    @return The element with the key attached

    Example:
    {[
      let my_key = use_key ~prefix:"my_component" in
      with_key my_key (Ui.text "Hello")
    ]} *)
