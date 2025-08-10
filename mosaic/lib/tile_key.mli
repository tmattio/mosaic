(** Key generation helper for Tile components

    This module provides a hook for generating stable, unique keys for UI
    elements within components. *)

val use_key : prefix:string -> Ui.Attr.key
(** Generate a unique key with the given prefix. The key is stable across
    re-renders of the same component instance.

    Example:
    {[
      let button ~label ~on_click () =
        let key = Tile_key.use_key ~prefix:"btn" in
        Ui.with_key key (Ui.panel ~border:Rounded (Ui.text label))
    ]} *)
