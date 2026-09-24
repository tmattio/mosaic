(** Persistent text editing state. *)

type t
(** Text, caret, selection and bounded undo history. Operations leave their
    argument intact. Cursor-only changes share the indexed source and history;
    text edits currently splice and index a flat string. *)

val empty : t
(** [empty] has empty text and no history. *)

val of_string : ?max_length:int -> string -> t
(** [of_string ?max_length text] places the caret at the end of [text].
    [max_length] defaults to [max_int] and limits grapheme clusters, clamped to
    zero. Initial text exceeding it is truncated. *)

val text : t -> string
(** [text t] is the editor's text, without copying. *)

val length : t -> int
(** [length t] is its number of grapheme clusters. *)

val cursor : t -> int
(** [cursor t] is the caret's grapheme boundary. *)

val caret : t -> Matrix.Text.Layout.caret
(** [caret t] includes the boundary's soft-wrap affinity. *)

val selection : t -> (int * int) option
(** [selection t] is the selected half-open grapheme range, if nonempty. *)

val selected_text : t -> string
(** [selected_text t] is the selected text, or the empty string. *)

val source : t -> Matrix.Text.Layout.Source.t
(** [source t] is the immutable indexed text shared with layout. *)

val clear : t -> t
(** [clear t] deletes all text, saving an undo point. *)

val insert : string -> t -> t
(** [insert text t] replaces the selection, or inserts at the caret. The result
    is resegmented and the caret rebased after the insertion. The length limit
    admits a fitting prefix while preserving unselected text. If the resulting
    text is unchanged, the caret, selection and history are preserved. *)

val set_cursor : ?select:bool -> int -> t -> t
(** [set_cursor ?select pos t] moves to a clamped grapheme boundary. [select],
    default [false], extends selection from its existing anchor or old caret. *)

val set_caret : ?select:bool -> Matrix.Text.Layout.caret -> t -> t
(** [set_caret ?select caret t] also sets soft-wrap affinity. *)

val select : anchor:int -> cursor:int -> t -> t
(** [select ~anchor ~cursor t] selects between clamped grapheme boundaries. *)

val clear_selection : t -> t
(** [clear_selection t] clears selection without moving the caret. *)

val select_all : t -> t
(** [select_all t] selects all text with the caret at its end. *)

(** {1 Editing}
    Deletions replace an active selection first, except line deletion which
    deletes the caret's logical line. Each text change saves an undo point. *)

val delete_backward : t -> t
(** [delete_backward t] deletes the preceding grapheme. *)

val delete_forward : t -> t
(** [delete_forward t] deletes the following grapheme. *)

val delete_word_backward : t -> t
(** [delete_word_backward t] deletes to the previous word boundary. *)

val delete_word_forward : t -> t
(** [delete_word_forward t] deletes to the next word boundary. *)

val delete_to_line_start : t -> t
(** [delete_to_line_start t] deletes to the logical line start. *)

val delete_to_line_end : t -> t
(** [delete_to_line_end t] deletes to the logical line end. *)

val delete_line : t -> t
(** [delete_line t] deletes the current logical line including its newline. *)

val undo : t -> t
(** [undo t] restores the previous undo point. *)

val redo : t -> t
(** [redo t] restores the most recently undone edit. *)

(** {1 Movement}
    Movement clears selection by default. [select:true] extends it. *)

val move_left : ?select:bool -> t -> t
(** [move_left ?select t] moves to the preceding grapheme. *)

val move_right : ?select:bool -> t -> t
(** [move_right ?select t] moves to the following grapheme. *)

val move_word_backward : ?select:bool -> t -> t
(** [move_word_backward ?select t] moves to the previous word boundary. *)

val move_word_forward : ?select:bool -> t -> t
(** [move_word_forward ?select t] moves to the next word boundary. *)

val move_home : ?select:bool -> t -> t
(** [move_home ?select t] moves to the start of the text. *)

val move_end : ?select:bool -> t -> t
(** [move_end ?select t] moves to the end of the text. *)

val move_line_start : ?select:bool -> t -> t
(** [move_line_start ?select t] moves to the logical line start. *)

val move_line_end : ?select:bool -> t -> t
(** [move_line_end ?select t] moves to the logical line end. *)

val accept : current:t -> t -> t
(** [accept ~current proposal] accepts a directly derived proposal only while
    [current] is its original state, including caret and selection. Otherwise it
    returns [current]. Use this when deliberately deferring proposals; ordinary
    widget callbacks are delivered synchronously. Values from {!of_string} are
    unconditional replacements. *)

(**/**)

module Private : sig
  module Buffer : sig
    type t
    (** The type for mutable editing buffers. *)

    (** {1:helpers Helpers} *)

    val strip_newlines : string -> string
    (** [strip_newlines s] is [s] with all CR, LF, and CRLF sequences removed.

        {b Note.} When [s] contains no newline characters the original string is
        returned without allocation. *)

    (** {1:constructors Constructors} *)

    val create :
      ?max_length:int ->
      ?width_method:Matrix.Text.width_method ->
      ?tab_width:int ->
      string ->
      t
    (** [create ?max_length ?width_method ?tab_width initial] is a buffer whose
        initial content is [initial], with cursor at the end.

        [max_length] is the maximum number of grapheme clusters the buffer may
        hold. [initial] is truncated to [max_length] grapheme clusters if it
        exceeds that limit. [max_length] defaults to [1000] and is clamped to
        [>= 0].

        [width_method] controls how grapheme display widths are computed.
        Defaults to [`Unicode].

        [tab_width] is the display width of tab characters. Defaults to [2] and
        is clamped to [>= 1].

        See also {!set_max_length}. *)

    (** {1:content Content} *)

    val text : t -> string
    (** [text t] is the current content of [t]. *)

    val set_text : t -> string -> unit
    (** [set_text t s] replaces the content of [t] with [s].

        [s] is truncated to {!max_length} [t] grapheme clusters if it exceeds
        that limit. The cursor moves to the end and any active selection is
        cleared. No undo point is saved. *)

    val length : t -> int
    (** [length t] is the number of grapheme clusters in [t]. *)

    val display_width : t -> int
    (** [display_width t] is the total display width of [t] in terminal columns.
    *)

    val is_empty : t -> bool
    (** [is_empty t] is [true] iff {!length} [t] is [0]. *)

    (** {1:lines Line information} *)

    val line_count : t -> int
    (** [line_count t] is the number of logical lines in [t]. Always [>= 1]. *)

    val cursor_line : t -> int
    (** [cursor_line t] is the 0-based logical line index of the cursor. *)

    val cursor_col : t -> int
    (** [cursor_col t] is the grapheme column of the cursor within its logical
        line. *)

    (** {1:cursor Cursor} *)

    val cursor : t -> int
    (** [cursor t] is the cursor position as a 0-based grapheme cluster offset.
        The value is in \[[0];[length t]\]. *)

    val set_cursor : t -> int -> unit
    (** [set_cursor t pos] moves the cursor to [pos], clamped to
        \[[0];[length t]\]. Clears any active selection. *)

    val set_cursor_offset : ?select:bool -> t -> int -> unit
    (** [set_cursor_offset ?select t pos] moves the cursor to [pos], clamped to
        \[[0];[length t]\].

        When [select] is [true], extends or creates a selection anchored at the
        previous cursor position. When [select] is [false] (default), any active
        selection is cleared.

        See also {!set_cursor}. *)

    val cursor_display_offset : t -> int
    (** [cursor_display_offset t] is the display column of the cursor in
        terminal columns. *)

    (** {1:selection Selection} *)

    val selection : t -> (int * int) option
    (** [selection t] is [Some (lo, hi)] when a selection is active, where
        [lo < hi] are grapheme cluster offsets. [None] when no selection is
        active. *)

    val has_selection : t -> bool
    (** [has_selection t] is [true] iff {!selection} [t] is [Some _]. *)

    val selected_text : t -> string
    (** [selected_text t] is the text of the current selection, or [""] if no
        selection is active. *)

    val clear_selection : t -> unit
    (** [clear_selection t] clears the active selection without moving the
        cursor. *)

    val select_all : t -> unit
    (** [select_all t] selects all text in [t] and moves the cursor to the end.
    *)

    (** {1:editing Editing} *)

    val clear : t -> bool
    (** [clear t] deletes all text, saving an undo point if nonempty. *)

    val insert : t -> string -> bool
    (** [insert t s] is [true] iff inserting [s] changed the text of [t].

        [s] replaces the active selection or is inserted at the cursor. If the
        resulting text is unchanged, the caret, selection and history are
        preserved. The result is resegmented, allowing combining characters to
        join adjacent clusters even at {!max_length} [t]. If the full insertion
        exceeds the limit, only an insertion prefix fitting the available
        grapheme capacity is admitted; existing unselected text is preserved. An
        undo point is saved before any modification. *)

    val delete_backward : t -> bool
    (** [delete_backward t] is [true] iff a deletion changed the text of [t].

        If a selection is active the selected text is deleted. Otherwise the
        grapheme cluster immediately before the cursor (Backspace behaviour) is
        deleted. An undo point is saved before any modification. *)

    val delete_forward : t -> bool
    (** [delete_forward t] is [true] iff a deletion changed the text of [t].

        If a selection is active the selected text is deleted. Otherwise the
        grapheme cluster immediately after the cursor (Delete behaviour) is
        deleted. An undo point is saved before any modification. *)

    val delete_word_backward : t -> bool
    (** [delete_word_backward t] is [true] iff a deletion changed the text of
        [t].

        Deletes from the cursor to the previous word boundary. An undo point is
        saved before any modification. *)

    val delete_word_forward : t -> bool
    (** [delete_word_forward t] is [true] iff a deletion changed the text of
        [t].

        Deletes from the cursor to the next word boundary. An undo point is
        saved before any modification. *)

    val delete_to_line_start : t -> bool
    (** [delete_to_line_start t] is [true] iff a deletion changed the text of
        [t].

        Deletes from the cursor to the start of the current logical line. An
        undo point is saved before any modification. *)

    val delete_to_line_end : t -> bool
    (** [delete_to_line_end t] is [true] iff a deletion changed the text of [t].

        Deletes from the cursor to the end of the current logical line (before
        any newline). Returns [false] when the cursor is already at the line
        end. An undo point is saved before any modification. *)

    val delete_to_start : t -> bool
    (** [delete_to_start t] is [true] iff a deletion changed the text of [t].

        Deletes from the cursor to the start of the buffer. An undo point is
        saved before any modification. *)

    val delete_to_end : t -> bool
    (** [delete_to_end t] is [true] iff a deletion changed the text of [t].

        Deletes from the cursor to the end of the buffer. An undo point is saved
        before any modification. *)

    val delete_line : t -> bool
    (** [delete_line t] is [true] iff a deletion changed the text of [t].

        Deletes the current logical line including its trailing line separator.
        An undo point is saved before any modification. *)

    (** {1:movement Cursor movement}

        Movement operations return [true] iff the cursor position changed.

        When [select] is [true] the operation extends or creates a selection.
        When [select] is [false] (default) any active selection is cleared. If a
        selection was active and [select] is [false], the cursor moves to the
        selection edge in the direction of movement. *)

    val move_left : ?select:bool -> t -> bool
    (** [move_left ?select t] is [true] iff the cursor moved.

        Moves the cursor one grapheme cluster to the left. [select] defaults to
        [false]. *)

    val move_right : ?select:bool -> t -> bool
    (** [move_right ?select t] is [true] iff the cursor moved.

        Moves the cursor one grapheme cluster to the right. [select] defaults to
        [false]. *)

    val move_word_forward : ?select:bool -> t -> bool
    (** [move_word_forward ?select t] is [true] iff the cursor moved.

        Moves the cursor to the next word boundary. [select] defaults to
        [false]. *)

    val move_word_backward : ?select:bool -> t -> bool
    (** [move_word_backward ?select t] is [true] iff the cursor moved.

        Moves the cursor to the previous word boundary. [select] defaults to
        [false]. *)

    val move_home : ?select:bool -> t -> bool
    (** [move_home ?select t] is [true] iff the cursor moved.

        Moves the cursor to the start of the buffer. [select] defaults to
        [false]. *)

    val move_end : ?select:bool -> t -> bool
    (** [move_end ?select t] is [true] iff the cursor moved.

        Moves the cursor to the end of the buffer. [select] defaults to [false].
    *)

    val move_line_start : ?select:bool -> t -> bool
    (** [move_line_start ?select t] is [true] iff the cursor moved.

        Moves the cursor to the start of the current logical line. [select]
        defaults to [false]. *)

    val move_line_end : ?select:bool -> t -> bool
    (** [move_line_end ?select t] is [true] iff the cursor moved.

        Moves the cursor to the end of the current logical line, before any
        trailing newline. [select] defaults to [false]. *)

    (** {1:undo Undo and redo} *)

    val undo : t -> bool
    (** [undo t] is [true] iff undoing restored a previous state.

        Restores the most recent undo point. The history is bounded: once it is
        full, saving a new undo point drops the oldest one. Returns [false] when
        no undo history is available. *)

    val redo : t -> bool
    (** [redo t] is [true] iff redoing re-applied a change.

        Re-applies the most recently undone change. Returns [false] when no redo
        history is available. *)

    (** {1:limits Limits} *)

    val max_length : t -> int
    (** [max_length t] is the maximum number of grapheme clusters [t] may hold.
    *)

    val set_max_length : t -> int -> unit
    (** [set_max_length t n] sets the maximum grapheme cluster count of [t] to
        [n], clamped to [>= 0]. If the current content exceeds [n] grapheme
        clusters it is truncated to fit.

        See also {!create}. *)

    (** {1:offsets Offset conversions} *)

    val line_of_offset : t -> int -> int
    (** [line_of_offset t pos] is the 0-based logical line containing grapheme
        offset [pos]. [pos] is clamped to \[[0];[length t]\]. *)

    val col_of_offset : t -> int -> int
    (** [col_of_offset t pos] is the grapheme column of [pos] within its logical
        line. [pos] is clamped to \[[0];[length t]\]. *)

    val line_start : t -> int -> int
    (** [line_start t line] is the grapheme offset of the first character on
        [line]. [line] is clamped to \[[0];[line_count t - 1]\]. *)

    val line_end : t -> int -> int
    (** [line_end t line] is the grapheme offset past the last character on
        [line], before the newline (or at buffer end for the last line). [line]
        is clamped to \[[0];[line_count t - 1]\]. *)

    val byte_offset : t -> grapheme:int -> int
    (** [byte_offset t ~grapheme] is the byte offset of grapheme cluster
        [grapheme] in the buffer's content string. Returns
        [String.length (text t)] when [grapheme >= length t]. *)

    val source : t -> Matrix.Text.Layout.Source.t
    (** [source t] is the indexed current content. It is shared with display
        geometry; edits install a new source while existing source values remain
        valid. *)

    val caret : t -> Matrix.Text.Layout.caret
    (** [caret t] is the current grapheme boundary and soft-wrap affinity. *)

    val set_caret : ?select:bool -> t -> Matrix.Text.Layout.caret -> unit
    (** [set_caret t caret] sets the clamped cursor and its affinity. [select],
        default [false], extends selection as in {!set_cursor_offset}. *)

    val copy : t -> t
    (** [copy t] is an independent buffer sharing immutable text, source indexes
        and history with [t]. Its cost is independent of the text length. *)

    val same : t -> t -> bool
    (** [same a b] tests whether copies retain the same content, interaction
        state and history. *)
  end

  val select_carets :
    anchor:Matrix.Text.Layout.caret -> cursor:Matrix.Text.Layout.caret -> t -> t

  val preferred_col : t -> int option
  val propose : base:t -> preferred_col:int option -> t -> t
end
