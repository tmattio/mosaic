[@@@ocamlformat "disable"]
(*---------------------------------------------------------------------------
   Copyright (c) 2014 The uuseg programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** Grapheme cluster segmenter.

    Vendored from uuseg v17.0.0 with the following modifications:
    - Added {!reset} function for segmenter reuse (zero allocation)
    - Added [ignore_zwj] option to disable GB11 (emoji ZWJ sequences)
    - Added {!set_ignore_zwj} to change the option after creation
    - Added {!check_boundary} for zero-allocation direct boundary checks *)

(** {1 Segmenter} *)

type t

val create : ?ignore_zwj:bool -> unit -> t
(** [create ?ignore_zwj ()] returns a new grapheme cluster segmenter.

    @param ignore_zwj When [true], GB11 is disabled: ZWJ never joins emoji
    sequences, forcing a break after ZWJ. Defaults to [false]. *)

val copy : t -> t
val equal : t -> t -> bool

val reset : t -> unit
(** [reset s] resets [s] to its initial state, ready to segment a new string.
    The [ignore_zwj] setting is preserved across reset. *)

val set_ignore_zwj : t -> bool -> unit
(** [set_ignore_zwj s v] sets the [ignore_zwj] option for [s]. *)

val check_boundary : t -> Uchar.t -> bool
(** [check_boundary s u] returns [true] if there is a grapheme cluster
    boundary BEFORE [u], and updates the segmenter state.

    This is a zero-allocation alternative to {!add} for performance-critical
    code. The first character always returns [true] (GB1). *)

val check_boundary_with_width : t -> Uchar.t -> int
(** [check_boundary_with_width s u] performs a boundary check and extracts the
    display width from a single property table lookup. Returns a packed int:
    bit 2 = is_boundary, bits 0-1 = width encoding (0→-1, 1→0, 2→1, 3→2). *)

val add : t -> [ `Await | `End | `Uchar of Uchar.t ] -> Uuseg_base.ret
