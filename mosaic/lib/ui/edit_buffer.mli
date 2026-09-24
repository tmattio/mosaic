(** Grapheme-aware text editing buffer.

    A mutable buffer that tracks cursor position, selection, and undo history in
    terms of Unicode grapheme clusters. Content may contain newlines for
    multi-line editing. *)

include module type of Editor.Private.Buffer
