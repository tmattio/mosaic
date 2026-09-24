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
  val buffer : t -> Edit_buffer.t
  val preferred_col : t -> int option
  val propose : base:t -> preferred_col:int option -> Edit_buffer.t -> t
end
